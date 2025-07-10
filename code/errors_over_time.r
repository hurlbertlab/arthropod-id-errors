# Read in data files
library(dplyr)
library(rvest)
library(tidyr)
library(stringr)
library(ggplot2)
library(lme4)
library(jtools) #for effect_plot()


## Get most recent data files from caterpillars-count-data repo
data_repo <- "https://github.com/hurlbertlab/caterpillars-count-data"
webpage <- read_html(data_repo)
repo_links <- html_attr(html_nodes(webpage, "a"), "href")
data_links <- tibble(link = repo_links[grepl(".csv", repo_links)]) %>%
  mutate(file_name = word(link, 6, 6, sep = "/")) %>%
  distinct()


## Read data files from data repo links
github_raw <- "https://raw.githubusercontent.com/hurlbertlab/caterpillars-count-data/master/"

surveys = read.csv(paste(github_raw, filter(data_links, grepl("Survey.csv", file_name))$file_name, sep = ''), header = TRUE, stringsAsFactors = FALSE)

expertIDs = read.csv(paste(github_raw, filter(data_links, grepl("ExpertIdentification.csv", file_name))$file_name, sep = ''), header = TRUE, stringsAsFactors = FALSE)


arths = read.csv(paste(github_raw, filter(data_links, grepl("ArthropodSighting.csv", file_name))$file_name, sep = ''), header = TRUE, stringsAsFactors = FALSE) %>%
  rename(Group = "UpdatedGroup", BeetleLarva = "UpdatedBeetleLarva", Sawfly = "UpdatedSawfly") 


# Create dataframe that has the cumulative number of surveys and photos, and 
# cumulative error rate by user.
# Here we exclude observations submitted as caterpillars, spiders, or ants which
# are known to have extremely low error rates.
# Error rate is the percent of observations that were incorrectly submitted 
# (i.e., the percent of observations submitted as Group X that were not actually Group X).

df = left_join(surveys, arths, by = c('ID' = 'SurveyFK')) %>%
  rename(ArthropodSightingFK = ID.y) %>%
  left_join(expertIDs, by = c("ArthropodSightingFK", "OriginalGroup")) %>%
  rename(SurveyID = ID.x) %>%            
  dplyr::select(SurveyID, UserFKOfObserver, ArthropodSightingFK, OriginalGroup, Length, StandardGroup) %>%
  arrange(SurveyID) %>%
  group_by(UserFKOfObserver) %>%
  mutate(userSurveyNumber = row_number(),
         agreement = StandardGroup == OriginalGroup,
         incorrect = ifelse(agreement, 0, 1)) %>%
  filter(!OriginalGroup %in% c("unidentified", "other", "caterpillar", "spider", "ant"), 
         !is.na(StandardGroup)) %>%
  mutate(photoObsNum = row_number(), 
         cumNumCorrect = cumsum(agreement),
         cumErrorRate = 100*(photoObsNum - cumNumCorrect)/photoObsNum) %>%
  arrange(UserFKOfObserver, SurveyID)

userTotals = df %>%
  group_by(UserFKOfObserver) %>%
  summarize(totalSurveys = max(userSurveyNumber),
            totalPhotos = max(photoObsNum)) %>%
  arrange(desc(totalPhotos))


# Function for making an error over time (vs. number of surveys) plot
errorsOverTimePlot = function(UserID, dataframe = df, new = TRUE, ...) {
  
  tmp = filter(dataframe, UserFKOfObserver == UserID)
  
  if(new) {
    plot(tmp$photoObsNum, tmp$cumErrorRate, type = 'l', xlab = "Number of surveys", 
         ylab = "Cumulative error rate", ...)
  } else {
    points(tmp$photoObsNum, tmp$cumErrorRate, type = 'l', ...)
  }
  
}



## EXAMPLES:

errorsOverTimePlot(UserID = 3654, dataframe = df, col = 'salmon', new = TRUE, ylim = c(0, 40))
errorsOverTimePlot(UserID = 2020, dataframe = df, col = 'dodgerblue', new = FALSE)


## Multi-panel figure:

# Visualize the top 30 users by totalPhotos
par(mfrow = c(5, 6), mar = c(5, 3, 1, 1))

# Loop over several different user IDs to create a plot for each one
for (u in userTotals$UserFKOfObserver[2:31]) { # exclude userID 26, the 1st one
  
  errorsOverTimePlot(u, dataframe = df, new = T, main = paste("UserID", u)) 
  
}

# GLM modelling incorrect id as a function of num cumulative photos submitted at the time of the id

# Add any userIDs to be excluded from analysis. For now just excluding AHH (#26).
df.glm = df %>%
  filter(UserFKOfObserver != 26, 
         UserFKOfObserver %in% userTotals$UserFKOfObserver[userTotals$totalPhotos >= 20])

# Random intercepts for UserID
error.glm = glmer(incorrect ~ scale(photoObsNum) + (1 | UserFKOfObserver), 
                                 data = df.glm, family = "binomial",
                                 glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

# p = 0.0004 for photoObsNum
error.plot = effect_plot(error.glm, pred = photoObsNum, interval = TRUE, int.type = "confidence", 
            x.label = "Number of photo observations", y.label = "Error rate")

error.plot + theme(axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16))

