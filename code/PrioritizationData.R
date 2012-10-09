library(RODBC)
library(ggplot2)
library(plyr)
library(xtable)
library(reshape2)
library(car)

#### Database setup ####

# Note, this obnoxiously only works from Windows when we're authenticated to the Active Domain
# Kerberos authentication was a bridge too far

# Production remote DB
dbhandle <- odbcDriverConnect('driver={SQL Server};server=sql2008\\psrcsql;database=shrp2c18final;trusted_connection=true')

# Sometimes it's handy to get a list of database tables
# tabs <- sqlQuery(dbhandle, 'select * from information_schema.tables')


#### Global variables ####
## Except for results, these should all really live in the DB

# Kimberly S. sent along this list of projects that are significantly funded
# I believe these are all MTPIDs
sig_funded <- c(4172, 4171, 4170, 4169, 2834, 1885, 2883, 3418,
                2313,2844, 3610, 2502, 2502, 3429, 547, 1879,
                3364, 4127, 2892, 2851, 2894, 2899, 2900, 2689,
                3609, 4263, 1956, 4160, 500, 511, 1432, 2847,
                122, 3612, 2902, 958, 610, 5538, 4025, 4609,
                2668, 5146, 5166, 5168, 5279, 5448, 4121, 1299,
                5537, 4272, 4002, 1239, 2890, 2812, 792, 1200,
                265, 3643, 3601, 2880, 116, 113, 1224, 1308,
                5509, 2905, 4277, 3569, 2823, 2805, 1950, 4276,
                4050, 4047, 2493, 2492, 3550, 5543, 5343, 4355,
                4354, 4353, 4352, 4341, 4340, 4311, 4281, 4280,
                4267, 4252, 4251, 4189, 4159, 4099, 3527, 2910,
                2882, 2567, 1714, 1661, 1658, 1652, 1644, 1627)

# Question IDs associated with our scorecards as opposed to something else.
# Question IDs are defined in project DB
scorecard_qs <- c(96, 97, 98, 110, 111, 113,
                  69, 70, 71, 72, 73, 74, 114,
                  66, 67, 68, 106, 107,
                  90, 91, 92, 117, 118, 120,
                  99, 101, 102, 121, 122, 134, 136,
                  89, 141,
                  93, 94, 95, 148, 150, 151, 152,
                  59, 60, 61, 62, 63, 64, 65, 159,
                  75, 76, 77, 78)


#### Summary functions ####
# Some summary functions

# Simply return the pre-existing "DashView" DB view
# Used for some quick summaries
dashView <- function() {
  dash.view <- sqlQuery(dbhandle,
                        'SELECT D.*, A.strName AS "Name"
                          FROM DashView D
                        
                        LEFT JOIN agencies A
                        ON D.AgencyID = A.ID')
  dash.view$status[is.na(dash.view$status)] <- "In Progress"
  
  # Subset out the significantly funded projects
  dash.view <- dash.view[! dash.view$mtpid %in% sig_funded, ]
  
  # Only grab a few of the fields that we need
  dash.view <- dash.view[, c("Name", "mtpid", "Basics 2", "status")]
  names(dash.view) <- c("Jurisdiction", "MTPID", "Project", "Status")
  
  # Convert NA to "Agency not Identified"
  dash.view$Jurisdiction[is.na(dash.view$Jurisdiction)] <- "Agency not Identified"
  
  return(dash.view)
}



# Read in the dataset for real
readAndMerge <- function () {
  m <- sqlQuery(dbhandle,
                'SELECT D.MTPID,
                        R.ProjectID, R.QuestionID,
                        Q.Text AS "Question",
                        A.ID AS AnswerID, A.Text AS "Answer",
                        D.status as "Status",
                        C.category as "Category"

                      FROM Responses R
  
                      LEFT JOIN Answers A
                        ON R.AnswerID = A.ID
  
                      LEFT JOIN Questions Q
                        ON Q.ID = A.QuestionID

                      LEFT JOIN DashView D
                        ON D.id = R.ProjectID

                      LEFT JOIN project_category C
                        ON C.mtpid = D.MTPID
  
                      ORDER BY C.Category, R.ProjectID, R.QuestionID;'
  )
  
  #Filter NA projects
  m <- m[! is.na(m$ProjectID),]
  q <- questionList()
  m <- merge(m, q, by=intersect(names(m), names(q)))
  
  # Filter out significantly funded projects
  m <- m[! m$MTPID %in% sig_funded, ]
  
  return(m)
}


questionList <- function() {
  q <- sqlQuery(dbhandle,
                'SELECT Q.Text AS "Question",
                  Q.ID as "QuestionID",
                A.ID AS AnswerID,
                A.Text AS "Answer"
                FROM Questions Q
                INNER JOIN Answers A
                ON Q.ID=A.QuestionID')
  
  # Use our hardcoded scorecard questions list to filter out non-scorecard questions.
  q <- q[q$QuestionID %in% scorecard_qs, ]
  
  # merge answer values; should be done in SQL at DB level, but we don't have Qs in db
  q <- merge(x=q, y=answerValues(), by="AnswerID")
  q <- merge(x=q, y=scorecardMembership(), by="QuestionID")
  
  return(q)
}

# Sponsor form app doesn't know which questions belong to which scorecard,
# so we had to manually figure these out.
scorecardMembership <- function() {
  scorecards <- read.csv(file="./data/Scorecards.csv")
  return(scorecards)
}

# Sponsor form app doesn't know the points value for any given answer,
# so we had to also manually figure these out (FYI, a royal pain)
answerValues <- function() {
  answer.values <- read.csv(file="./data/AnswerValues.csv")
  return(answer.values)
}


# The makers of our sponsor form also failed to make questions mutually exclusive
# So, we have to do this manually as well (FYI an extra pain).
mutexResponses <- function() {
  m <- readAndMerge()
  m.names <- names(m)
  
  # Ordinarily, questions are independent
  m$MutexGroup <- m$QuestionID
  
  # But some should be grouped
  m$MutexGroup[m$QuestionID %in% c(96, 97)] <- "m1"
  m$MutexGroup[m$QuestionID %in% c(98, 110)] <- "m2"
  m$MutexGroup[m$QuestionID %in% c(72, 73)] <- "m3"
  m$MutexGroup[m$QuestionID %in% c(99, 101, 102, 121)] <- "m4"
  m$MutexGroup[m$QuestionID %in% c(134, 136)] <- "m5"
  m$MutexGroup[m$QuestionID %in% c(94, 95, 148)] <- "m6"
  m$MutexGroup[m$QuestionID %in% c(150, 151, 152)] <- "m7"
  m$MutexGroup[m$QuestionID %in% c(59, 60, 61)] <- "m8"
  m$MutexGroup[m$QuestionID %in% c(62, 63)] <- "m9"
  m$MutexGroup[m$QuestionID %in% c(66, 67)] <- "m9"
  
  # Get the highest value answer for each group of questions, by project
  d <- ddply(m, .(ProjectID, MutexGroup), summarize,
             Max=max(Values, na.rm=TRUE))
  

  # Merge the data frames
  m <- merge(x=d, y=m, by=c("ProjectID", "MutexGroup"))
  
  
  # If a response was not zero or was less than the max, drop the response
  # This condition means that someone didn't choose between mutually exclusive options
  m <- m[m$Values == 0 | m$Values == m$Max, ]
  
  # Only select the original set of names
  m <- subset(m, select=m.names)
  
  return(m)
  
}


tallyScores <- function() {
  m <- mutexResponses()
  # Generate totals by card
  m.by.card <- ddply(m, .(MTPID,  Scorecard), summarize,
                     Score = sum(Values, na.rm=TRUE))
  
  # In how many scorecards did each project receive points
  m.num.card <- ddply(m.by.card, .(MTPID), summarize,
                      NumCats=length(na.exclude(Score[Score > 0])))
  
  # Cast the data into a wide format
  m.by.card <- dcast(m.by.card, MTPID ~ Scorecard, value = "Score")
  
  # Generate total score by project
  m.by.proj <- ddply(m, .(MTPID), summarize,
                     TotalScore=sum(Values, na.rm=TRUE),
                     Status=unique(Status),
                     Category=unique(Category))
  # Homogenize the status field
  m.by.proj$Status[is.na(m.by.proj$Status)] <- "In Progress"
  
  
  # merge the summaries and return
  m.merge <- merge(x=m.by.card, y=m.by.proj, by="MTPID")
  
  # And now merge the number of scorecards data
  m.merge <- merge(x=m.merge, y=m.num.card, by="MTPID")
  
  # Only return submitted or reviewed projects
  m.merge <- m.merge[m.merge$Status != "In Progress", ]
  m.merge$Status <- NULL
  
  return(m.merge)
}


# Function creates a long-formatted table of tallies
talliesLong <- function(t) {
  t <- melt(t[, 2:10])
  names(t) <- c("Scorecard", "Score")
  return(t)
}

# Function to z-score and quantile cut tallies
talliesZScore <- function(t) {
  labels <- c("\\HVLow", "\\HLow", "\\HMed", "\\HHi", "\\HVHi")
  for(x in 2:11) {
    t[,x] <- cut(scale(t[,x]), 5, labels=labels)
  }

  return(t)
}

#odbcCloseAll()

