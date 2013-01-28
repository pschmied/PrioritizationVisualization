library(RODBC)
library(ggplot2)
library(plyr)
library(xtable)
library(reshape2)
library(car)
library(gtools)
library(scales)

#### Database setup ####

# Note, this obnoxiously only works from Windows when we're authenticated to the Active Domain
# Kerberos authentication was a bridge too far

# Production remote DB
dbhandle <- odbcDriverConnect('driver={SQL Server};server=sql2008\\psrcsql;database=shrp2c18final;trusted_connection=true')

# Sometimes it's handy to get a list of database tables
# tabs <- sqlQuery(dbhandle, 'select * from information_schema.tables')


#### Global variables ####
## Except for results, these should all really live in the DB


allResponses <- function () {
  m <- sqlQuery(dbhandle,
                'SELECT * FROM all_responses
                WHERE MTPID IS NOT NULL
                AND Prioritize = \'TRUE\'')
  
  # Recode Category labels
  m$Category <- as.character(m$Category)
  m$Category[m$Category == "Transit & Ferry Related"] <- "Transit"
  m$Category[m$Category == "Roadway Related - State Route"] <- "Highways"
  m$Category[m$Category == "Roadway Related - Arterial"] <- "Arterials"
  m$Category[m$Category == "Bicycle/Pedestrian"] <- "Bike/Ped"
  m$Category <- as.factor(m$Category)
  
  # Recode NA submission statuses; results in warning if there are none
  #m$Submission[is.na(m$Submission)] <- "In Progress"
  
  # Pull year as an added field
  m1 <- m[!is.na(m$QuestionID) & m$QuestionID == 29, ]
  m1 <- subset(m1, select=c("MTPID", "AnswerFreeText"))
  names(m1) <- c("MTPID", "Completed")
  m <- merge(m, m1, by=intersect(names(m), names(m1)))
  
  # Clean description of one nasty unicode character
  m$Description <- gsub(" ", "", m$Description)
  
  m <- dedupe(m)
  
  m <- mutexPoints(m)
  
  return(m)
}


# Deduplication function for edge case where we have multiple year responses
# Naively take last (maybe latest) response
dedupe <- function(responses) {
  m1 <- unique(responses[ ,1:16])
  m2 <- subset(responses, select=c("MTPID", "QuestionID", "Completed"))
  m2 <- ddply(.data=m2, .(MTPID, QuestionID), summarize, Completed=tail(Completed, n=1))
  m <- merge(m1, m2, by=intersect(names(m1), names(m2)))
  return(m)
}


# The makers of our sponsor form also failed to make questions mutually exclusive
# So, we have to do this manually as well (FYI an extra pain).
mutexPoints <- function(responses) {
  m <- responses
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
  
  # Responses including questions that are in a mutex group (speeds thing up)
  mq <- m[grep("^m[0-9]", m$MutexGroup), ]
  mq <- mq[! is.na(mq$Points), ]

  
  # Get the highest value answer for each group of questions, by project
  d <- ddply(mq, .(MTPID, MutexGroup), summarize,
             Max=max(Points, na.rm=TRUE))
  

  # Merge the data frames
  m <- merge(x=d, y=m, by=c("MTPID", "MutexGroup"), all.y=TRUE)
  
  
  # If a response was not zero or was less than the max, drop the response
  # This condition means that someone didn't choose between mutually exclusive options
  m$Points[! is.na(m$Max) & m$Points < m$Max ] <- 0
  
  # Only select the original set of names
  m <- subset(m, select=m.names)
  
  return(m)
  
}


tallyScores <- function() {
  m <- allResponses()
  m <- m[! is.na(m$Scorecard), ] # Remove NA scorecards
  
  # Generate totals by card
  m.by.card <- ddply(m, .(MTPID,  Scorecard), summarize,
                     Score = sum(Points, na.rm=TRUE))
  
  # In how many scorecards did each project receive points
  m.num.card <- ddply(m.by.card, .(MTPID), summarize,
                      NumCats=length(na.exclude(Score[Score > 0])))
  
  # Cast the data into a wide format
  m.by.card <- dcast(m.by.card, MTPID ~ Scorecard, value = "Score")
  
  # Generate total score by project
  m.by.proj <- ddply(m, .(MTPID), summarize,
                     TotalScore=sum(Points, na.rm=TRUE),
                     Description=unique(Description),
                     Submission=unique(Submission),
                     Category=unique(Category),
                     Cost=unique(Cost),
                     Section=unique(Section),
                     Completed=Completed[1],
                     Jurisdiction=Jurisdiction[1])
  
  # merge the summaries and return
  m.merge <- merge(x=m.by.card, y=m.by.proj, by="MTPID")
  
  # And now merge the number of scorecards data
  m.merge <- merge(x=m.merge, y=m.num.card, by="MTPID")
  
  # Sort by total score
  m.merge <- m.merge[with(m.merge, order(-TotalScore)), ]
  
  return(m.merge)
}


# Function creates a long-formatted table of tallies
talliesLong <- function(t) {
  t <- melt(t, measure.vars=c("Air Quality", "Freight", "Jobs", "Multi-Modal",
                              "Puget Sound Land and Water", "Safety and System Security",
                              "Social Equity and Access to Opportunity",
                              "Support for Centers", "Travel"))
  names(t)[names(t)=="variable"] <- "Scorecard"
  names(t)[names(t)=="value"] <- "Score"
  return(t)
}

# Function to z-score and quantile cut tallies
talliesZScore <- function(t) {
  # Take a table of tallies and turn it into a ZScored table with LaTeX control sequences
  labels <- c("\\HVLow", "\\HLow", "\\HMed", "\\HHi", "\\HVHi")
  for(x in 2:10) {
    t[,x] <- cut(scale(t[,x]), 5, labels=labels)
  }
  
  t <- subset(t, select = c("MTPID", "Jurisdiction", "Description", "Air Quality", "Freight", "Jobs", "Multi-Modal",
                   "Puget Sound Land and Water", "Safety and System Security",
                   "Social Equity and Access to Opportunity",
                   "Support for Centers", "Travel", "TotalScore", "Cost", "Category", "Section"))
  
  return(t)
}

tallies5Level <- function(t) {
  # Take a table of tallies and turn it into a 5-leveled table with LaTeX control sequences
  labels <- c("\\HVLow", "\\HLow", "\\HMed", "\\HHi", "\\HVHi")
  
  for(x in 2:10) {
    t[,x] <- recode(t[,x], "0:2='\\\\HVLow'; 3:4='\\\\HLow'; 5:6='\\\\HMed'; 7:8='\\\\HHi'; 9:10='\\\\HVHi'")
  }
  
  # re-sort by totalscore
  #t <- t[with(t, order(-TotalScore)), ]
  
  t$TotalScore <- recode(t$TotalScore, "0:18='\\\\HVLow'; 19:37='\\\\HLow'; 38:55='\\\\HMed'; 56:73='\\\\HHi'; 74:100='\\\\HVHi'")
  
  t <- subset(t, select = c("MTPID", "Jurisdiction", "Description", "Air Quality", "Freight", "Jobs", "Multi-Modal",
                            "Puget Sound Land and Water", "Safety and System Security",
                            "Social Equity and Access to Opportunity",
                            "Support for Centers", "Travel", "TotalScore", "Cost", "Category", "Section"))
  
  return(t)
}

# Write a table of all responses

#odbcCloseAll()

