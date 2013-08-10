# Read in the R functions that query and digest our data
source("./code/PrioritizationData.R")

# Make outputs directories
dir.create(file.path("./report", "tables"), showWarnings = FALSE)
dir.create(file.path("./report", "figures"), showWarnings = FALSE)

# Get our tallies
tallies <- tallyScores()
# Make a long-formatted version for easier summarization / visualization
tallies_long <- talliesLong(tallies)

# Dump the full results table to CSV
write.csv(allResponses(), "./report/tables/all_responses.csv")

# Summarize the projects by status, writing files to disk as .csv and .tex
projects_by_status <- ddply(.data=tallies, .(Submission), summarize, Total=length(Submission))
print(xtable(projects_by_status),
      file="./report/tables/projects_by_status.tex",
      include.rownames=FALSE,
      booktabs=TRUE,
      only.contents=TRUE)
write.csv(projects_by_status, "./report/tables/projects_by_status.csv")



# Summarize projects by jurisdiction, ditto csv and tex
projects_by_jurisdiction <- ddply(.data=tallies,
                                  .(Jurisdiction),
                                  summarize,
                                  Completed=length(Submission[Submission == "Reviewed" | Submission == "Submitted"]),
                                  Projects=length(Submission))
# sort
projects_by_jurisdiction <- projects_by_jurisdiction[with(projects_by_jurisdiction,
                                                          order(-Completed, -Projects, Jurisdiction)), ]
# export
print(xtable(projects_by_jurisdiction),
      file="./report/tables/projects_by_jurisdiction.tex",
      include.rownames=FALSE,
      booktabs=TRUE,
      only.contents=TRUE)
write.csv(projects_by_jurisdiction, "./report/tables/projects_by_jurisdiction.csv")



### Plots
# Graphical summary of the distribution by score
distribution_by_score <- ggplot(tallies[! is.na(tallies$Category),],
                                aes(x=TotalScore)) +
  geom_histogram(aes(y=..density..), binwidth=5, fill="darkgray") +
  geom_density(fill="red", alpha=0.3) +
  facet_wrap( ~ Category) +
  labs(list(x='Score', y='Projects (density)'))

ggsave(filename="./report/figures/distribution_by_score.pdf",
       plot=distribution_by_score)


# Graphical summary of the distribution, faceted by score AND cat
# First, recode the completion date, because it's not very clean
tallies_long$first_period <- tallies_long$Completed %in% c(seq(2000, 2020))
tallies_long$second_period <- tallies_long$Completed %in% c(seq(2021, 2040))

distScoreCat <- function(tallies_long) {
  t.cor <- ddply(tallies_long, .(Category, Scorecard), summarize, n=paste("n =", length(unique(MTPID))))
  ggplot(tallies_long[! is.na(tallies_long$Category),],
                                  aes(x=Score)) +
                                    geom_histogram(aes(y=..density..), binwidth=1, fill="darkgray") +
                                    geom_density(fill="red", alpha=0.3) +
                                    facet_grid(Category ~ Scorecard) +
                                    labs(list(x='Score', y='Projects (density)')) +
                                    geom_text(data=t.cor, aes(x=9, y=1.1, label=n), size = 2, colour="black", inherit.aes=FALSE, parse=FALSE) +
                                    theme(text=element_text(size=8))
  
}

distribution_by_score_and_cat <- distScoreCat(tallies_long)
ggsave(filename="./report/figures/distribution_by_score_and_cat.pdf", plot=distribution_by_score_and_cat, width=10, height=7.5, units="in")

distribution_by_score_and_cat_2020 <- distScoreCat(tallies_long[tallies_long$first_period == TRUE, ])
ggsave(filename="./report/figures/distribution_by_score_and_cat_2020.pdf", plot=distribution_by_score_and_cat_2020, width=10, height=7.5, units="in")

distribution_by_score_and_cat_2040 <- distScoreCat(tallies_long[tallies_long$second_period == TRUE, ])
ggsave(filename="./report/figures/distribution_by_score_and_cat_2040.pdf", plot=distribution_by_score_and_cat_2040, width=10, height=7.5, units="in")



# Projects by score and number of categories in which points were won
# Note: this is a silly plot and should not be used
tallies$Cost_08_Millions <- quantcut((tallies$Cost/1000000), q=seq(0,1,by=0.2))
projects_by_cats_and_score <- ggplot(tallies) +
  aes(x=TotalScore, y=NumCats, color=Category, size = Cost_08_Millions) +
  geom_jitter(position = position_jitter(height = 0.3), alpha=0.5) +
  labs(list(x='Total Score',
                   y='Number of Categories'))

ggsave(filename="./report/figures/projects_by_cats_and_score.pdf",
       plot=projects_by_cats_and_score)

# Proportion of projects by score, by scorecard
projectScoreScorecard <- function(category) {
  if(missing(category)) {
    df <- tallies_long # the full set
  } else {
    df <- tallies_long[tallies_long$Category == category, ]
  }
  ggplot(df, aes(x=Score)) +
    geom_bar(aes(y=..density..), binwidth=1, fill="darkgray") +
    geom_density(fill="red", alpha=0.3) +
    facet_wrap(~ Scorecard) +
    labs(list(x='Score',
                     y='Projects (density)'))
}

# Proportion of all projects by score, by scorecard
projects_by_score_by_scorecard <- projectScoreScorecard()
ggsave(filename="./report/figures/projects_by_score_by_scorecard.pdf",
       plot=projects_by_score_by_scorecard)

# Proportion of Bike/Ped projects by score, by scorecard
projects_by_score_by_scorecard_bikeped <- projectScoreScorecard("Bike/Ped")
ggsave(filename="./report/figures/projects_by_score_by_scorecard_bikeped.pdf",
       plot=projects_by_score_by_scorecard_bikeped)


# Proportion of Arterial projects by score, by scorecard
projects_by_score_by_scorecard_arterials <- projectScoreScorecard("Arterials")
ggsave(filename="./report/figures/projects_by_score_by_scorecard_arterials.pdf",
       plot=projects_by_score_by_scorecard_arterials)


# Proportion of Highways projects by score, by scorecard
projects_by_score_by_scorecard_highways <- projectScoreScorecard("Highways")
ggsave(filename="./report/figures/projects_by_score_by_scorecard_highways.pdf",
       plot=projects_by_score_by_scorecard_highways)

# Proportion of Transit projects by score, by scorecard
projects_by_score_by_scorecard_transit <- projectScoreScorecard("Transit")
ggsave(filename="./report/figures/projects_by_score_by_scorecard_transit.pdf",
       plot=projects_by_score_by_scorecard_transit)

# Disaggregate tables
write.csv(tallies, "./report/tables/raw_tallies.csv")


# Set a column order for formatted tables
column_order <-   c("Category", "MTPID", "Jurisdiction", "Description", "Air Quality", "Freight", "Jobs", "Multi-Modal", "Puget Sound Land and Water", "Safety and System Security", "Social Equity and Access to Opportunity", "Support for Centers", "Travel", "TotalScore", "Cost", "Section")

# Consumer reports tables
tallies_5 <- tallies5Level(tallies)
tallies_5 <- subset(tallies_5, select=column_order) #reorder columns and subset
write.csv(tallies_5, "./report/tables/tallies_5.csv")

# Reformat cost column for pretty printing
tallies_5$Cost <- round(tallies_5$Cost / 1000000, 2)

# Reformat score column to drop decimals / avoid Terminator human casualty count problem
# Problem is in xtable, so being lazy and converting to character
tallies_5$TotalScore <- as.character(tallies_5$TotalScore)

# Reformat description to enable dot-filling to end of the line
# tallies_5$Description <- paste(tallies_5$Description, "\\dotfill", sep="")

# Bad index subsetting, but I'm lazy and it works. 2:12 == MTPID through scorecard
tallies_5_nonmotor <- tallies_5[tallies_5$Category == "Bike/Ped", 2:16]
tallies_5_arterial <- tallies_5[tallies_5$Category == "Arterials", 2:16]
tallies_5_stateroute <- tallies_5[tallies_5$Category == "Highways", 2:16]
tallies_5_transit <- tallies_5[tallies_5$Category == "Transit", 2:16]
tallies_5_unknown <- tallies_5[is.na(tallies_5$Category), 2:16]

table_names <- c("tallies_5_nonmotor", "tallies_5_arterial",
                 "tallies_5_stateroute", "tallies_5_transit",
                 "tallies_5_unknown")

for(t in table_names) {
print(xtable(get(t)),
      file=paste("./report/tables/", t, ".tex", sep=""),
      include.rownames=FALSE,
      include.colnames=FALSE,
      booktabs=TRUE,
      hline.after=NULL,
      only.contents=TRUE,
      sanitize.text.function= function(x){gsub("&", "and", x)})
}

