# Script for Identifying the types of errors made by citizen scientists in Caterpillars Count!

# Load libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)
library(popbio)
library(ggpubr)
library(lubridate) #built under R version 4.3.2

# Read in raw data
expert_ID = read.csv("2023-09-12_ExpertIdentification.csv")
arthro_sight = read.csv("2023-09-12_ArthropodSighting.csv") #%>%
  # filter(OriginalGroup == "daddylonglegs" & Length <= 20 |
  #          OriginalGroup == "fly" & Length <= 40 |
  #          OriginalGroup == "ant" & Length <= 25|
  #          OriginalGroup == "bee" & Length <= 35|
  #          OriginalGroup =="grasshopper" & Length <= 40|
  #          OriginalGroup == "caterpillar" & Length <= 80|
  #          OriginalGroup == "spider" & Length <= 40|
  #          OriginalGroup == "grasshopper" & Length <= 50 |
  #          OriginalGroup == "moths" & Length <= 80|
  #          OriginalGroup == "leafhopper" & Length <= 30|
  #          OriginalGroup == "ant" & Length <= 25|
  #          OriginalGroup == "truebugs" & Length <= 40)
surveys = read.csv("2023-09-12_Survey.csv")
game = read.csv("2023-09-26_VirtualSurveyScore.csv")

# true_counts displays OriginalGroup:StandardGroup:number of ID's with that pair 
true_counts = expert_ID %>%
   group_by(OriginalGroup, StandardGroup) %>%
   summarize(number = n())

# total_counts shows total amount of OriginalGroup IDs 
total_counts = expert_ID %>%
   group_by(OriginalGroup) %>%
   summarize(total_ID = n())
  
# use left_join() to compare total with proportion from true_counts
error_num = true_counts %>%
  group_by(OriginalGroup) %>% 
  left_join(total_counts, true_counts, by = c("OriginalGroup" = "OriginalGroup")) %>%
  mutate(rate = round((number / total_ID) * 100, 1)) %>%
  arrange(OriginalGroup, desc(rate)) 


# pivot table 
pivoted = error_num %>%
  select(OriginalGroup, StandardGroup, rate) %>%
  pivot_wider(names_from = StandardGroup, values_from = rate)

arthGroupsWeWant = c("ant", "aphid", "bee", "beetle", "caterpillar", 
                     "daddylonglegs", "fly", "grasshopper", "leafhopper",
                     "moths", "spider", "truebugs")

pivot2 = pivoted[pivoted$OriginalGroup %in% arthGroupsWeWant, 
                 arthGroupsWeWant] %>%
  mutate(OriginalGroup = arthGroupsWeWant) %>%
  select(OriginalGroup, ant:truebugs)

# image() plotting
par(mar = c(6, 6, 1, 1))
image(as.matrix(pivot2[, 2:ncol(pivot2)]), xaxt="n", yaxt="n", col = hcl.colors(100, "viridis")) 

mtext(arthGroupsWeWant, 1, at = (1:12)/11, las = 2, line = 0.5, padj = -2) +
mtext(arthGroupsWeWant, 2, at = (1:12)/11, las = 1, line = 1, padj = 3) 

# can add numbers to middle cells?

?seq


# Stacked bar graph: "What Arthropods are Mistaken For"

only_error_num = error_num %>%
  filter(OriginalGroup != StandardGroup,
         StandardGroup %in% arthGroupsWeWant, 
         OriginalGroup %in% arthGroupsWeWant)

d2 = only_error_num
d3 = aggregate(d2$rate, by=list(d2$OriginalGroup), FUN = sum)
d3 = d3[order(-d3$x),]
str = d3$Group.1
d2$StandardGroup = factor(d2$OriginalGroup, levels=str)

stacked = ggplot(d2, aes(fill=StandardGroup, y=rate, x=OriginalGroup)) +
  geom_bar(position='stack', 
           stat = 'identity') + 
  labs(x = "Originally Submitted As...", 
       y = "Error Rate", 
       title = "What Arthropods are Mistaken For", 
       fill = "Arthropod Group") +
  theme(plot.title = element_text(hjust=0.5, size=10), 
        legend.text = element_text(size = 5), 
        legend.key.size = unit(2, 'mm'), 
        legend.title = element_text("testing"), 
        axis.text.x = element_text(size = 8, angle = 270, hjust = 0, vjust = 0))

# reverse denominator stacked bar graph: "what are certain arthropods typically suspected as?"
d2 = only_error_num
d3 = aggregate(d2$rate, by=list(d2$StandardGroup), FUN = sum)
d3 = d3[order(-d3$x),]
str = d3$Group.1
d2$StandardGroup = factor(d2$StandardGroup, levels=str)

rev_stacked = ggplot(d2, aes(fill=OriginalGroup, y=rate, x=StandardGroup)) +
  geom_bar(position='stack', 
           stat = 'identity') + 
  labs(x = "Arthropod Species", 
       y = "Error Rate", 
       title = "What Arthropods are Typically Suspected As", 
       fill = "Suspected As...") +
  theme(plot.title = element_text(hjust=0.5, size=10), 
        legend.text = element_text(size = 5), 
        legend.key.size = unit(2, 'mm'), 
        axis.text.x = element_text(size = 8, angle = 270, hjust = 0, vjust = 0)) 


###
rev_stacked = ggplot(only_error_num, aes(fill=OriginalGroup, y=rate, x=fct_infreq(StandardGroup))) +
  geom_bar(position='stack', 
           stat = 'identity') + 
  labs(x = "Arthropod Species", 
       y = "Error Rate", 
       title = "What Arthropods are Typically Suspected As", 
       fill = "Suspected As...") +
  theme(plot.title = element_text(hjust=0.5, size=10), 
        legend.text = element_text(size = 5), 
        legend.key.size = unit(2, 'mm'), 
        axis.text.x = element_text(size = 8, angle = 270, hjust = 0, vjust = 0)) 

# graph: commonness of arthropod (with photos vs without photos?) 'how often are certain species of arthropod spotted'

standard_total_id = expert_ID %>%
  group_by(StandardGroup) %>%
  summarize(total_ID = n()) %>%
  filter(StandardGroup %in% arthGroupsWeWant)

commonness = ggplot(standard_total_id, aes(y=total_ID, x=StandardGroup)) + 
  geom_bar(position='stack', 
           stat = 'identity') + 
  labs(x = "Arthropod Group", 
       y = "Amount of Observations", 
       title = "How Often are Arthropods Spotted") +
  theme(axis.text.x = element_text(size = 6))

# LENGTH ANALYSIS

# lengthdf = left_join(expert_ID, arthro_sight, by = c("ArthropodSightingFK" = "ID", "OriginalGroup")) %>%
#   select(OriginalGroup, StandardGroup, Length) %>%
#   filter(OriginalGroup %in% arthGroupsWeWant) %>%
#   mutate(agreement = OriginalGroup == StandardGroup) %>%
#   group_by(OriginalGroup) %>%
#   summarize(n0.5 = sum(Length <= 5, na.rm = T),
#             n5.15 = sum(Length > 5 & Length <= 15, na.rm = T),
#             n15plus = sum(Length > 15, na.rm = T)), 
#             error0.5 = sum(Length <= 5 & !agreement, na.rm = T), 
#             error5.15 = sum(Length > 5 & Length <= 15 & !agreement, na.rm = T), 
#             errorn15plus = sum(Length > 15 & !agreement, na.rm = T)
#          
  
lengthdf = left_join(expert_ID, arthro_sight, by = c("ArthropodSightingFK" = "ID", "OriginalGroup")) %>% 
  select(OriginalGroup, StandardGroup, Length) %>% 
  filter(OriginalGroup %in% arthGroupsWeWant) %>% 
  mutate(agreement = OriginalGroup==StandardGroup) %>% 
  group_by(OriginalGroup) %>% 
  summarize(n0.5 = sum(Length <= 5, na.rm = T),
            n5.15 = sum(Length > 5 & Length <= 15, na.rm = T),
            n15plus = sum(Length > 15, na.rm = T),
            error0.5 = sum(Length <= 5 & !agreement, na.rm = T),
            error5.15 = sum(Length > 5 & Length <= 15 & !agreement, na.rm = T),
            error15plus = sum(Length > 15 & !agreement, na.rm = T),
            rate0.5 = 100*error0.5/n0.5,
            rate5.15 = 100*error5.15/n5.15,
            rate15plus = 100*error15plus/n15plus) +
  las = 1, ylim = c(0,  1.2*max(lengthdf[lengthdf$OriginalGroup == arth, c("rate0.5", "rate5.15", "rate15plus")])) 

par(mfrow = c(4,3), mar=c(2.5,5,1,1))

for (arth in lengthdf$OriginalGroup) { plot(1:3, lengthdf[lengthdf$OriginalGroup == arth, c("rate0.5", "rate5.15", "rate15plus")], type = 'b', main = arth, ylab = "% error", xaxt = "n", xlab = "", xlim = c(0.5, 3.5), mtext(c("2-5 mm", "5-15 mm", "15+ mm"), 1, at = 1:3, line = 0.3, cex = 0.45))}
# fly disparity? truebugs? 

# 12 panel figure of accuracy versus length  ------------------------

correctness_table = left_join(expert_ID, arthro_sight, by = c("ArthropodSightingFK" = "ID", "OriginalGroup")) %>% 
  select(OriginalGroup, StandardGroup, Length) %>% 
  filter(OriginalGroup %in% arthGroupsWeWant) %>% 
  mutate(agreement = OriginalGroup==StandardGroup, binary = as.integer(agreement)) %>% 
  group_by(OriginalGroup, Length)
  

par(mfrow = c(4,3), mar=c(2.5,5,2,1), oma = c(4, 1, 1, 1))

for (arth in arthGroupsWeWant) { 
  
  arthSubset = filter(correctness_table, OriginalGroup == arth)
  
  # Logistic regression curve:
  
   arthGLM = glm(binary ~ Length, data = arthSubset, family = "binomial")

   predicted_data = data.frame(Length = seq(min(arthSubset$Length, na.rm = TRUE), max(arthSubset$Length, na.rm=TRUE)))
   
   predicted_data$binary = predict(arthGLM, predicted_data, type="response")
   
   slope = round(coef(arthGLM)[2], 3)
   p = summary(arthGLM)$coefficients[2, 4]
   
   pstar = case_when(p < 0.001 ~ "***",
                     p < 0.01 & p > 0.001 ~ "**",
                     p < 0.05 & p > 0.01 ~ "*",
                     .default = "")
   
   plot(jitter(arthSubset$Length, .6), arthSubset$binary,
        main = arth, xlab = "", las = 1, yaxt = "n", ylab = "")
   
   mtext("Incorrect <------> Correct", 2, line = .5, cex = .5)
        
   lines(binary ~ Length, predicted_data, lwd=2, col="green")
   
   abline(h = 0.9, col = 'red', lty = 'dotted')
   
   minLength.9 = min(predicted_data$Length[predicted_data$binary >= 0.9])
   
   abline(v = minLength.9, col = 'blue', lty = 'dotted')

   text(x = .8*max(arthSubset$Length, na.rm = T), y = .2, labels = paste0("slope = ", slope, pstar))

}
mtext("Length (mm)", 1, cex = 1.5, outer = TRUE, line = 1)


  # Plot glm predicted response curve using example code here:
  # https://www.geeksforgeeks.org/how-to-plot-a-logistic-regression-curve-in-r/


#for (arth in correctness_table$OriginalGroup) { glm(correctness_table$binary ~ correctness_table$Length)}

correctness_plot = correctness_table %>%
  filter(correctness_table$OriginalGroup %in% 'ant') %>%
  logi.hist.plot(Length, agreement)

############## Beat sheet / Visual Survey Accuracy Comparison #############

# 1
# join expert_ID to arthro_sight to get SurveyFK column, then join to surveys to get ObservationMethod column

errorsByMethod = expert_ID %>%
  left_join(arthro_sight[, c("ID", "SurveyFK")], c("ArthropodSightingFK" = "ID")) %>%
  left_join(surveys[, c("ID", "ObservationMethod")], c("SurveyFK" = "ID")) %>% 
  filter(!OriginalGroup %in% c("other", "unidentified")) %>%
  group_by(ObservationMethod, OriginalGroup) %>%
  summarize(nTot = n(),
            numIncorrect = sum(OriginalGroup != StandardGroup),
            errorRate = 100*numIncorrect/nTot)

bsvplot = plot(errorsByMethod$errorRate[errorsByMethod$ObservationMethod == "Beat sheet"], xlab = "Beat Sheet", ylab = "Visual", main = "Observation Method Accuracy", errorsByMethod$errorRate[errorsByMethod$ObservationMethod == "Visual"], cex = log10 ((errorsByMethod$nTot[errorsByMethod$ObservationMethod == "Beat sheet"] + errorsByMethod$nTot[errorsByMethod$ObservationMethod == "Visual"])/2), pch = 16, col = 'salmon')

text(errorsByMethod$errorRate[errorsByMethod$ObservationMethod == "Beat sheet"], errorsByMethod$errorRate[errorsByMethod$ObservationMethod == "Visual"], errorsByMethod$OriginalGroup[errorsByMethod$ObservationMethod == "Visual"], cex = 0.7)

abline(a=0, b = 1)

#Game Data Analysis - how good are people at estimating length?
# filtered to difficult arth groups/out 'easy' groups rather than by individual, because error rate by individual might be skewed due to 'easy' arths

# for each user, we want the 1st score, avg score, max/"best" score, for all 4 scores (lengths, percentfound, IDaccuracy)

# first quiz score to their survey error rate (use 1st score or best score here)

gameplaydf =  game %>%
  select(UserFK, Score, LengthAccuracy, IdentificationAccuracy, PercentFound) %>%
  group_by(UserFK) %>%
  summarize(userplays = n(), 
            maxscore = max(Score, na.rm = TRUE),
            first = Score[1], 
            max = max(Score, na.rm = TRUE),
            best_length_accuracy = max(LengthAccuracy[LengthAccuracy != -1], na.rm = TRUE),
            best_ID_accuracy = max(IdentificationAccuracy[IdentificationAccuracy != -1], na.rm = TRUE),
            best_pct_found = max(PercentFound[PercentFound != -1], na.rm = TRUE),
            first_length_accuracy = LengthAccuracy[LengthAccuracy != -1][1],
            first_ID_accuracy = IdentificationAccuracy[IdentificationAccuracy != -1][1],
            first_pct_found = PercentFound[PercentFound != -1][1])

# Change -Inf values to NA
gameplaydf[gameplaydf == -Inf | gameplaydf == Inf] = NA


# Calculating survey identification error rates based on the non-easy (caterpillar, ant, spider) bugs
#  (also excluding other and unidentified)
surveyusererrors = expert_ID %>%
  left_join(arthro_sight[, c("ID", "SurveyFK")], c("ArthropodSightingFK" = "ID")) %>%
  left_join(surveys[, c("ID", "UserFKOfObserver")], c("SurveyFK" = "ID")) %>%
  dplyr::select(UserFKOfObserver, ArthropodSightingFK, OriginalGroup, StandardGroup) %>%
  group_by(UserFKOfObserver) %>%
  filter(!OriginalGroup %in% c("unidentified", "other", "caterpillar", "ant", "spider"), 
         !is.na(StandardGroup)) %>%
  summarize(UserObsNum = n(), 
         UserNumCorrect = sum(StandardGroup == OriginalGroup), 
         UserErrorRate = 100*(UserObsNum - UserNumCorrect)/UserObsNum) %>%
  arrange(desc(UserObsNum)) %>%
  filter(UserObsNum > 5) #%>%
  # select(UserFKOfObserver, UserErrorRate)

gameplayandusererrors = gameplaydf %>%
  inner_join(surveyusererrors[, c("UserErrorRate", "UserFKOfObserver", "UserObsNum")], by = c("UserFK" = "UserFKOfObserver")) %>%
  arrange(desc(UserObsNum))

######## Plot showing AVERAGE SCORE vs. SURVEY ERROR RATE#########

par(mfrow = c(2, 2), mar = c(4, 4, 1, 1))

# Plot showing BEST SCORE vs. SURVEY ERROR RATE

plot(gameplayandusererrors$maxscore, gameplayandusererrors$UserErrorRate, xlab = "Best Score", ylab = "Survey Error Rate (%)", main = "", cex = 2, col = 'dark green')

abline(lm(gameplayandusererrors$UserErrorRate ~ gameplayandusererrors$maxscore), col = 'green')

# each user's average LengthAccuracy predicts survey error rate?
# put LengthAccuracy vs. ID accuracy vs. percent found to survey error rate

######## PLOT: does LengthAccuracy predict Survey Error Rate? #######

plot(gameplayandusererrors$best_length_accuracy, gameplayandusererrors$UserErrorRate, xlab = "Best Length Accuracy", ylab = "Survey Error Rate (%)", main = "", cex = 2, ylim = c(0, 30))

bestlength = lm(gameplayandusererrors$UserErrorRate ~ gameplayandusererrors$best_length_accuracy)

lengthR2 = summary(bestlength)$r.squared                
                
abline(bestlength, col = 'green')


########## PLOT: does IDAccuracy predict Survey Error Rate? ##########
  
plot(gameplayandusererrors$best_ID_accuracy, gameplayandusererrors$UserErrorRate, xlab = "Best ID Accuracy", ylab = "Survey Error Rate (%)", main = "", cex = 2, ylim = c(0, 30))

abline(lm(gameplayandusererrors$UserErrorRate~gameplayandusererrors$best_ID_accuracy), col = 'green')


#########PLOT: Does PercentFound predict Survey Error Rate? ##########

plot(gameplayandusererrors$best_pct_found, gameplayandusererrors$UserErrorRate, xlab = "Best Percent Found", ylab = "Survey Error Rate (%)", main = "", cex = 2, ylim = c(0, 30))

abline(lm(gameplayandusererrors$UserErrorRate~gameplayandusererrors$best_pct_found), col = 'green')



########################################################################
#
# Change over time in game scores
#
#######################################################################


################## PLOT: IMPROVEMENT OVER TIME (GAME)################

# gameplays without subscores
userCounts = game %>%
  filter(Score > 500) %>% #filtering because these scores are likely incomplete gameplays
  dplyr::count(UserFK) %>%
  arrange(desc(n))

# gameplays with subscores
userCounts_filtered = game %>%
  filter(PercentFound != -1,
         Score > 500) %>%
  count(UserFK) %>%
  arrange(desc(n))

# filters out scores before subscores were measured
subscores_overtime = game %>%
  filter(UserFK %in% userCounts$UserFK[userCounts$n >= 4],
         !UserFK %in% c(25, 26), # filter to multi-play users
         PercentFound != -1, 
         Score > 500) %>% #filtering because these scores are likely incomplete gameplays
  group_by(UserFK) %>% 
  mutate(playnumber = row_number())

# keeps scores from before subscores were measured
scores_overtime = game %>% #scores with no subscores are included here
  filter(UserFK %in% userCounts$UserFK[userCounts$n >= 4],
         !UserFK %in% c(25, 26), # filter to multi-play users
         Score > 500) %>%  #filtering because these scores are likely incomplete gameplays
  group_by(UserFK) %>% 
  mutate(playnumber = row_number())

############### PLOTS: improvement over time by game sub score #################

par(mfrow = c(2,2), mar=c(2.5,5,2,1), oma = c(4, 1, 1, 1))
element_text(family = "serif", face = "bold")

#Total game score plot
plot(scores_overtime$playnumber, scores_overtime$Score, pch = 16, type = 'n', las = 1, ylab ="Score", xlab = "")

element_text(family = "serif", face = "bold")

userList_score = unique(scores_overtime$UserFK)

i = 0
for (user in userList_score) { 
  i = i + 1
  tmp = scores_overtime %>%
    filter(UserFK == user)
  
  points(tmp$playnumber, tmp$Score, pch = 16, type = 'l', col = rainbow(length(userList_score))[i], lwd = 3)
  
}

scoretest = cor.test(scores_overtime$playnumber, scores_overtime$Score, method = "spearman", exact = FALSE)

text(14, 1550, paste("r =", round(scoretest$estimate,2)))

#PercentFound plot
plot(subscores_overtime$playnumber, subscores_overtime$PercentFound, pch = 16, type = 'n', las = 1, ylab ="Percent Found", xlab = "")

userList = unique(subscores_overtime$UserFK)

i = 0
for (user in userList[c(1:5, 7:8)]) { #one user is weird (2803)
  i = i + 1
  tmp = subscores_overtime%>%
    filter(UserFK == user)
  
  points(tmp$playnumber, tmp$PercentFound, pch = 16, type = 'l', col = rainbow(8)[i], lwd = 3)
  
  
}

scoretest = cor.test(subscores_overtime$playnumber, subscores_overtime$PercentFound, method = "spearman", exact = FALSE)
text(8, 50, paste("r =",round(scoretest$estimate,2)))

#LengthAccuracy plot
plot(subscores_overtime$playnumber, subscores_overtime$LengthAccuracy, pch = 16, type = 'n', las = 1, ylab ="Length Accuracy", xlab = "")

userList = unique(subscores_overtime$UserFK)

i = 0
for (user in userList) {
  i = i + 1
  tmp = subscores_overtime%>%
    filter(UserFK == user)
  
  points(tmp$playnumber, tmp$LengthAccuracy, pch = 16, type = 'l', col = rainbow(8)[i], lwd = 3)
  
  
}

scoretest = cor.test(subscores_overtime$playnumber, subscores_overtime$LengthAccuracy, method = "spearman", exact = FALSE)
text(8, 50, paste("r =",round(scoretest$estimate,2)))

#ID Accuracy Plot
plot(subscores_overtime$playnumber, subscores_overtime$IdentificationAccuracy, pch = 16, type = 'n', las = 1, ylab ="ID Accuracy", xlab = "")

userList = unique(subscores_overtime$UserFK)

i = 0
for (user in userList) {
  i = i + 1
  tmp = subscores_overtime%>%
    filter(UserFK == user)
  
  points(tmp$playnumber, tmp$IdentificationAccuracy, pch = 16, type = 'l', col = rainbow(8)[i], lwd = 3)
  
}

scoretest = cor.test(subscores_overtime$playnumber, subscores_overtime$IdentificationAccuracy, method = "spearman", exact = FALSE)
text(8, 50, paste("r =",round(scoretest$estimate,2)))

mtext("Number of Game Plays", 1, outer = TRUE, cex = 1.5, line = 1.5)


################### PLOTS: improvement over time for EACH USER #################

# Total Score over time for each user
scores_userList = unique(scores_overtime$UserFK)

par(mfrow = c(3, 3), mar=c(2.5,3.5,1,1))

for (user in scores_userList) {
  tmp = scores_overtime %>%
    filter(UserFK == user)
  
  plot(tmp$playnumber, tmp$Score, xlab = "", ylab = "", main = user, type = "l")
  
}
 mtext("Number of GamePlays", 1, outer = TRUE, cex = 1.5, line = 1.5)
 
 mtext("Total Score", 2, outer = TRUE, cex = 1.5, line = -0.5)
 
 
# Percent found over time for each user
subscores_userList = unique(subscores_overtime$UserFK)
 
par(mfrow = c(3, 3), mar=c(2.5,3.5,1,1))
 
for (user in subscores_userList) {
  tmp = subscores_overtime %>%
  filter(UserFK == user)
  
  plot(tmp$playnumber, tmp$PercentFound, xlab = "", ylab = "", main = user, type = "l")
   
 }

mtext("Number of GamePlays", 1, outer = TRUE, cex = 1.5, line = 1.5)
mtext("Percent Found", 2, outer = TRUE, cex = 1.5, line = -0.5)

# these have less than 5 gameplays! error in filter somewhere (applies to all subscores throughout this part)

# Length Accuracy over time - each user
par(mfrow = c(3, 3), mar=c(2.5,3.5,1,1))

for (user in subscores_userList) {
  tmp = subscores_overtime %>%
    filter(UserFK == user)
  
  plot(tmp$playnumber, tmp$LengthAccuracy, xlab = "", ylab = "", main = user, type = "l")
  
}
mtext("Number of GamePlays", 1, outer = TRUE, cex = 1.5, line = 1.5)
mtext("Length Accuracy", 2, outer = TRUE, cex = 1.5, line = -0.5)


# ID Accuracy over time - each user 
par(mfrow = c(3, 3), mar=c(2.5,3.5,1,1))

for (user in subscores_userList) {
  tmp = subscores_overtime %>%
    filter(UserFK == user)
  
  plot(tmp$playnumber, tmp$IdentificationAccuracy, xlab = "", ylab = "", main = user, type = "l")
  
}
mtext("Number of GamePlays", 1, outer = TRUE, cex = 1.5, line = 1.5)
mtext("ID Accuracy", 2, outer = TRUE, cex = 1.5, line = -0.5)

# does practicing with the game improve survey score? analyze time stamps
# relative to survey submission timestamps (doing the survey before/after the game)
# compare timestamps of users in expert ID vs those users and their gamescores

############################# TIMESTAMP ANALYSIS ###############################

timestampdf = expert_ID %>%
  left_join(surveys[, c("ID", "UserFKOfObserver", "LocalDate", "LocalTime")], c("ArthropodSightingFK" = "ID")) %>%
  select("OriginalGroup", "StandardGroup", "UserFKOfObserver", "LocalDate", "LocalTime") %>%
  group_by(UserFKOfObserver) %>%
  mutate(correct = OriginalGroup == StandardGroup,
         doy = yday(LocalDate), 
         Year = as.numeric(substr(LocalDate, 1, 4)),
         yearday = Year + doy)

gamescoresdf = game %>%
  select("UserFK", "Score", "Timestamp") %>%
  mutate(doy = yday(Timestamp), 
         Year = as.numeric(substr(Timestamp, 1, 4)),
         yearday = Year + doy/365)

# 1a) Figure out how to extract Year from the date field and create a new column for it
# 1b) Then create yearday = Year + (doy/365) for both timestampdf and gamescoresdf, and use as x-axis below

# 2) need to create a new vector of users for people that have BOTH played game AND done surveys and use that in the for loop

userCounts_filtered_surveys = surveys %>%
  count(UserFKOfObserver) %>%
  arrange(desc(n))

game_and_survey = left_join(userCounts_filtered, userCounts_filtered_surveys, by = c("UserFK" = "UserFKOfObserver")) %>%
  filter(n.x > 2, n.y > 2)
  
par(mfrow = c(3, 3), mar=c(2.5,3.5,1,1))
for (user in game_and_survey$UserFK) {
  
  df1 = timestampdf %>%
    filter(UserFKOfObserver %in% user) #2066
  df2 = gamescoresdf %>%
    filter(UserFK %in% user)
  
  plot(df1$yearday, rep(1, nrow(df1)), pch = 17, col = 'red', xlab = 'Day', ylab = '', yaxt = 'n')
  
  points(df2$yearday, rep(1, nrow(df2)), col = 'blue')
  
  #tmp1 = filter(timestampdf, UserFKOfObserver %in% user)
  #tmp2 = filter(gamescoresdf, UserFK %in% user)
  
  # plot surveys
#   plot(tmp$yearday, rep(1, nrow(tmp1)), pch = 17, col = 'red', xlim = c(2000,2204), xlab = "Day", ylab = "", ylim = c(0,1), yaxt = "n", main = user)
#   
#   # plot game play dates
#   points(tmp2$yearday, rep(1, nrow(tmp2)), pch = 16, col = 'blue')
 }

