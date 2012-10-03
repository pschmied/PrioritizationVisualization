# Read in the R functions that query and digest our data
source("./code/PrioritizationData.R")

# Get our tallies
tallies <- tallyScores()
# Make a long-formatted version for easier summarization / visualization
tallies_long <- talliesLong(tallies)


# Summarize the projects by status, writing files to disk as .csv and .tex
projects_by_status <- responseSummary()
print(xtable(projects_by_status),
      file="./report/tables/projects_by_status.tex",
      include.rownames=FALSE,
      booktabs=TRUE,
      only.contents=TRUE)
write.csv(projects_by_status, "./report/tables/projects_by_status.csv")

# Summarize projects by jurisdiction, ditto csv and tex
projects_by_jurisdiction <- jurisdictionSummary()
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
  facet_wrap(~ Category) +
  opts(labels=list(x='Score', y='Projects (density)'))

ggsave(filename="./report/figures/distribution_by_score.pdf",
       plot=distribution_by_score)

# Projects by score and number of categories in which points were won
# Note: this is a silly plot and should not be used
projects_by_cats_and_score <- ggplot(tallies) +
  aes(x=TotalScore, y=NumCats, color=Category) +
  geom_smooth(method=lm, se=FALSE) +
  geom_point() +
  opts(labels=list(x='Total Score',
                   y='Number of Categories'))

ggsave(filename="./report/figures/projects_by_cats_and_score.pdf",
       plot=projects_by_cats_and_score)

# Proportion of projects by score, by scorecard
projects_by_score_by_scorecard <- ggplot(tallies_long, aes(x=Score)) +
  geom_bar(aes(y=..density..), binwidth=1, fill="darkgray") +
  geom_density(fill="red", alpha=0.3) +
  facet_wrap(~ Scorecard) +
  opts(labels=list(x='Score',
                   y='Projects (density)'))

ggsave(filename="./report/figures/projects_by_score_by_scorecard.pdf",
       plot=projects_by_score_by_scorecard)


# Consumer reports tables
tallies_z <- talliesZScore(tallies)
tallies_z <- cbind(Category = tallies_z$Category, tallies_z[, 1:11]) #reorder and subset
write.csv(tallies_z, "./report/tables/tallies_z.csv")

tallies_z_nonmotor <- tallies_z[tallies_z$Category == "Bicycle/Pedestrian", 2:12]
tallies_z_arterial <- tallies_z[tallies_z$Category == "Roadway Related - Arterial", 2:12]
tallies_z_stateroute <- tallies_z[tallies_z$Category == "Roadway Related - State Route", 2:12]
tallies_z_transit <- tallies_z[tallies_z$Category == "Transit & Ferry Related", 2:12]
tallies_z_unknown <- tallies_z[is.na(tallies_z$Category), 2:12]

table_names <- c("tallies_z_nonmotor", "tallies_z_arterial",
                 "tallies_z_stateroute", "tallies_z_transit",
                 "tallies_z_unknown")

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