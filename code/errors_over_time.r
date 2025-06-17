# Read in data files
library(dplyr)
library(rvest)
library(tidyr)
library(stringr)
library(ggplot2)

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


# Create dataframe that has the cumulative number of surveys and errors by user

df = left_join(surveys, arths, by = c('ID' = 'SurveyFK')) %>%
  rename(ArthropodSightingFK = ID.y) %>%
  left_join(expertIDs, by = c("ArthropodSightingFK", "OriginalGroup")) %>%
  rename(SurveyID = ID.x) %>%            
  dplyr::select(SurveyID, UserFKOfObserver, ArthropodSightingFK, OriginalGroup, Length, StandardGroup) %>%
  arrange(SurveyID) %>%
  group_by(UserFKOfObserver) %>%
  mutate(userSurveyNumber = row_number(),
         agreement = StandardGroup == OriginalGroup) %>%
  filter(!OriginalGroup %in% c("unidentified", "other"), 
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

dfSelect = df %>%
  filter(UserFKOfObserver %in% c(2763, 3654, 2020, 2024, 2809, 3165, 3204, 3625, 3654, 3661))

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

# Do plots for some example users, e.g. UserFKOfObserver 2763, 3654, 2020, 2023, 2024, 2809, 3158, 3165, 3204, 3625, 3654, 3661

dfSelect = df %>%
  filter(UserFKOfObserver %in% c(3654, 2020, 2024, 2809, 3165, 3204, 3625, 3654, 3661))


## EXAMPLES:

errorsOverTimePlot(UserID = 3654, dataframe = df, col = 'salmon', new = TRUE, ylim = c(0, 40))
errorsOverTimePlot(UserID = 2020, dataframe = df, col = 'dodgerblue', new = FALSE)


## Multi-panel figure:

# first, create a set of panels with 3 rows and 4 columns; specify margins around each panel
par(mfrow = c(3, 4), mar = c(5, 3, 1, 1))

# Loop over several different user IDs to create a plot for each one
for (u in c(2763, 3654, 2020, 2024, 2809, 3165, 3204, 3625, 3654, 3661)) { 
  
  errorsOverTimePlot(u, dataframe = df, new = T, main = paste("UserID", u))
  
}

# (incomplete) Each plot in the multi-panel figure combined...
#par(mfrow = c(1, 1), mar = c(5, 3, 1, 1))

# Loop over several different user IDs to create a plot for each one
for (u in c(2763, 3654, 2020, 2023, 2024, 2809, 3158, 3165, 3204, 3625, 3654, 3661)) 
#   

# Figure out how to create a dataframe that just has the cumulative number of photos FOR THAT GROUP, and error rates specific to THAT GROUP.

aphid_errors_and_photos = df %>%
  select(UserFKOfObserver, OriginalGroup, agreement) %>%
  filter(OriginalGroup == "aphid") %>%
  group_by(UserFKOfObserver) %>%
  mutate(photoObsNumAphid = row_number(), 
         aphidNumCorrect = cumsum(agreement),
         aphidErrorRate = 100*(photoObsNumAphid - aphidNumCorrect)/photoObsNumAphid) %>% arrange(UserFKOfObserver)

TB_errors_and_photos = df %>%
  select(UserFKOfObserver, OriginalGroup, agreement) %>%
  filter(OriginalGroup == "truebugs") %>%
  group_by(UserFKOfObserver) %>%
  mutate(photoObsNum = row_number(), 
         cumNumCorrect = cumsum(agreement),
         cumErrorRate = 100*(photoObsNum - cumNumCorrect)/photoObsNum) %>% arrange(UserFKOfObserver)


# count of TB records by user
TBusers = count(TB_errors_and_photos, UserFKOfObserver) %>% 
  arrange(desc(n))

par(mfrow = c(2, 2), mar = c(5, 5, 2, 1))

# Loop over several different user IDs to create a plot for each one
for (u in TBusers$UserFKOfObserver[TBusers$n >= 20]) { 
  
  errorsOverTimePlot(u, dataframe = TB_errors_and_photos, new = F, 
                     main = paste("UserID", u))
  
}

# current workspace

par(mfrow = c(2, 2), mar = c(5, 5, 2, 1))

for (u in TBusers$UserFKOfObserver[TBusers$n >= 20]) { 
  
  errorsOverTimePlot(u, dataframe = df, new = T, main = paste("UserID", u))
  
}


par(mfrow = c(1, 1), mar = c(5, 3, 1, 1))

errorsOverTimePlot(26, dataframe = TB_errors_and_photos, new = T, ylim = c(0, 20))

# Loop over several different user IDs to create a plot for each one
for (u in TBusers$UserFKOfObserver[TBusers$n >= 20]) { 
  
errorsOverTimePlot(u, dataframe = TB_errors_and_photos, new = T, col = u, lwd = 3)
  }
# error rates by arthropod (1 user)

# You should still be able to use the errorsOverTimePlot() function, but you will just put in 

#     dataframe = <new dataframe name>

# linear logistic regression - 12 panel figure histograms of lengths


