# Script for Identifying the types of errors made by citizen scientists in Caterpillars Count!

# Add a new comment

# Load libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(vioplot)
library(ggpubr)

# Read in raw data

setwd("C:/Users/ellen27/Documents/")

expert_ID = read.csv("2025-04-14_ExpertIdentification.csv")
expert_ID$OriginalGroup[expert_ID$SawflyUpdated == 1 & expert_ID$OriginalGroup == 'bee'] = 'sawfly larvae'
expert_ID$StandardGroup[expert_ID$SawflyUpdated == 1] = 'sawfly larvae'

# Fix two records manually that the user assumed originally were sawfly larvae (but in one case forgot to check the box)
expert_ID$OriginalGroup[expert_ID$ArthropodSightingFK %in% c(116543,129308)] = 'sawfly larvae'

surveys = read.csv("2025-04-14_Survey.csv")
game = read.csv("2025-04-14_VirtualSurveyScore.csv")
arthro_sight = read.csv("2025-04-14_ArthropodSighting.csv")

# true_counts displays OriginalGroup:StandardGroup:SawflyUpdated:number of ID's with that pair 

true_counts = expert_ID %>%
  select(OriginalGroup, StandardGroup, SawflyUpdated) %>%
   group_by(OriginalGroup, StandardGroup, SawflyUpdated) %>%
   summarize(number = n())

# total_counts shows total amount of OriginalGroup IDs 
total_counts = expert_ID %>%
   group_by(OriginalGroup) %>%
   summarize(total_OGID = n())
  
# use left_join() to compare total with proportion from true_counts
error_num = true_counts %>%
  group_by(OriginalGroup, SawflyUpdated) %>% 
  left_join(total_counts, true_counts, by = c("OriginalGroup" = "OriginalGroup")) %>%
  mutate(rate = round((number / total_OGID) * 100, 1)) %>%
  arrange(OriginalGroup, desc(rate)) 


#######################################################################
#
#    Arthropod Mis-identification Analysis: On-Site / Field Data
#
######################################################################

arthGroupsWeWant = c("ant", "aphid", "bee", "beetle", "caterpillar", 
                     "daddylonglegs", "fly", "grasshopper", "leafhopper",
                     "moths", "spider", "truebugs", "sawfly larvae")

arthGroupNames = data.frame(originalName = arthGroupsWeWant,
                            revisedName = c("ants", "aphids", "bees, wasps", "beetles",
                                            "caterpillars", "daddy longlegs", "flies",
                                            "grasshoppers", "leafhoppers", "moths",
                                            "spiders", "true bugs", "sawfly larvae"),
                            maxLength = c(15, 10, 22, 30, 60, 15, 25, 40, 25, 25, 22, 35, 50))


####### Plot: Stacked bar graph: "What Arthropods are Mistaken For" #######

only_error_num = error_num %>%
  filter(OriginalGroup != StandardGroup,
         StandardGroup %in% arthGroupsWeWant, 
         OriginalGroup %in% arthGroupsWeWant) %>%
  left_join(arthGroupNames, by = c('StandardGroup' = 'originalName')) %>%
  rename(StandardGroupRevised = revisedName) %>%
  left_join(arthGroupNames, by = c('OriginalGroup' = 'originalName')) %>%
  rename(OriginalGroupRevised = revisedName) 

only_error_num$StandardGroupRevised[only_error_num$SawflyUpdated == 1] = "sawfly larvae"

d2 = only_error_num
d3 = aggregate(d2$rate, by=list(d2$OriginalGroupRevised), FUN = sum)
d3 = d3[order(-d3$x),]
str = d3$Group.1
d2$OriginalGroupRevised = factor(d2$OriginalGroupRevised, levels=str)

color_values = c(
  "ants" = "#8da0cb",
  "aphids" = "#1b9e77",
  "bees, wasps" = "#d95f02",
  "beetles" = "#a6761d",
  "caterpillars" = "#666666",
  "daddy longlegs" = "#66c2a5",
  "flies" = "#e7298a",
  "grasshoppers" = "#66a61e",
  "leafhoppers" = "#fc8d62",
  "moths" = "#e6ab02",
  "sawfly larvae" = "#e78ac3",
  "spiders" = "#a6d854",
  "true bugs" = "#7570b3")

stacked = ggplot(d2, aes(fill = StandardGroupRevised, y = rate, 
  x = OriginalGroupRevised)) +   geom_bar(position = 'stack', stat = 'identity') +
  scale_fill_manual(
    values = color_values,
    breaks = sort(names(color_values)),  
    name = "Actual Group") +
   scale_y_continuous(breaks = seq(0, 30, by = 5)) +
   labs(
     x = "Originally Reported As...",
     y = "Error Rate",
     fill = "Actual Group"
   ) +
   theme_bw() +
   theme(
     legend.text = element_text(size = 11),
     legend.title = element_text(size = 14),
     axis.title = element_text(size = 16),
     axis.text.x = element_text(size = 14, angle = 45, hjust = 1, vjust = 1),
     axis.text.y = element_text(size = 14),
     plot.margin = unit(c(1, .5, .2, .5), "cm")
   )

#pdf('figures/misidentified1.pdf', height = 5, width = 7)
print(stacked)
#dev.off()


######## Plot:"What are certain arthropods typically suspected as?" ##########

d4 = aggregate(d2$rate, by=list(d2$StandardGroupRevised), FUN = sum)
d4 = d4[order(-d4$x),]
str2 = d4$Group.1
d2$StandardGroupRevised = factor(d2$StandardGroupRevised, levels=str2)

rev_stacked = ggplot(d2, aes(fill=OriginalGroupRevised, y=rate, x=StandardGroupRevised)) +
  geom_bar(position='stack', 
           stat = 'identity') + 
  scale_fill_manual(
    values = color_values,
    breaks = sort(names(color_values)),  
    name = "Actual Group") +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  labs(x = "Actual Group", 
       y = "Error Rate", 
       #title = "Most Common Misidentifications", 
       fill = "Reported As") +
  theme_bw() + 
  theme(#plot.title = element_text(hjust=0.5, size=18), 
        legend.text = element_text(size = 11), 
        legend.key.size = unit(6, 'mm'), 
        legend.title = element_text(size = 14), 
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 14)) +
  theme(plot.margin = unit(c(1,.5,.2,.5), "cm"))

#pdf('figures/misidentified2.pdf', height = 5, width = 7)
print(rev_stacked)
#dev.off()

#pdf('figures/2-panel_misidentifications.pdf', height = 11, width = 7)
grid.arrange(stacked, rev_stacked, nrow=2)
#dev.off()



########## TWO PANEL PLOT: "Field Identification Accuracy" ##################



#only_error_num = error_num %>%
#filter(OriginalGroup != StandardGroup,
       #StandardGroup %in% arthGroupsWeWant, 
       #OriginalGroup %in% arthGroupsWeWant) %>%
  #left_join(arthGroupNames, by = c('StandardGroup' = 'originalName')) %>%
  #rename(StandardGroupRevised = revisedName) %>%
  #left_join(arthGroupNames, by = c('OriginalGroup' = 'originalName')) %>%
  #rename(OriginalGroupRevised = revisedName)


########## Plot: "How often are certain species of arthropods spotted? ##############

standard_total_id = expert_ID %>%
  group_by(StandardGroup) %>%
  summarize(total_ID = n()) %>%
  filter(StandardGroup %in% arthGroupsWeWant)

commonness = ggplot(standard_total_id, aes(y=total_ID, x=StandardGroup)) + 
  geom_bar(position='stack', 
           stat = 'identity') + 
  labs(x = "Arthropod Group", 
       y = "Total Amount of Observations", 
       title = "How Often are Arthropods Spotted") +
  theme(axis.text.x = element_text(size = 6))

print(commonness)

#############################################################
#
#      LENGTH ANALYSIS
#
##############################################################

################## Plot: Length vs % Error per arthropod #############

correctness_table = left_join(expert_ID, arthro_sight, by = c("ArthropodSightingFK" = "ID", "OriginalGroup")) %>% 
  select(OriginalGroup, StandardGroup, Length) %>% 
  filter(OriginalGroup %in% arthGroupsWeWant) %>% 
  mutate(agreement = OriginalGroup==StandardGroup, binary = as.integer(agreement)) %>% 
  group_by(OriginalGroup, Length)
  
correct_by_length = correctness_table %>%  #counts # of incorrect length observations
  group_by(StandardGroup, Length) %>%
  summarize(nObs = n(),
            nWrong = sum(!agreement),
            errorRate = 100*nWrong/nObs)

# New version plotting error rates vs length instead of correct/incorrect

pdf('figures/error_rates_vs_length.pdf', height = 8, width = 10)

par(mfrow = c(4,3), mar=c(2.5,4,1,1), oma = c(4, 4, 0, 2), tck = -.03, mgp = c(2, .8, 0), 
    cex.axis = 1.5, cex.main = 1.8)

# Panels in order of error trends:
# --the first 6 panels have uniformly low error rates regardless of length
# --the next 5 panels show reduced error rates for larger specimens
# --aphids just have high error rates

for (arth in c("caterpillar", "ant", "spider", "beetle", "leafhopper", "fly",
               "grasshopper", "bee", "daddylonglegs", "moths", "truebugs", "aphid")) { 
  
  arthSubset = filter(correct_by_length, StandardGroup == arth)
  
  plot(arthSubset$Length, arthSubset$errorRate, xlab = "", las = 1, 
       ylab = "", cex = log10(arthSubset$nObs)+.2, pch = 16, col = 'gray40',
       xlim = c(0, arthGroupNames$maxLength[arthGroupNames$originalName == arth]), ylim = c(0, 80))
  title(arthGroupNames$revisedName[arthGroupNames$originalName == arth], line = -1.5)
  abline(h = 10, col = 'red', lty = 'dashed', lwd = 2)
  
  text(15, 40, paste("r =",round(scoretest$estimate,2)))
  
  #!!! sometimes this plot shows up, sometimes it doesn't
  
}

#Tile plots:
par(mfrow = c(3, 4))  # 3 rows, 4 columns — adjust as needed

for (arth in c("caterpillar", "ant", "spider", "beetle", "leafhopper", "fly",
               "grasshopper", "bee", "daddylonglegs", "moths", "truebugs", "aphid")) { 
  
  arthSubset = filter(correct_by_length, StandardGroup == arth)
  
  plot(arthSubset$Length, arthSubset$errorRate, xlab = "", las = 1, 
       ylab = "", cex = log10(arthSubset$nObs)+.2, pch = 16, col = 'gray40',
       xlim = c(0, arthGroupNames$maxLength[arthGroupNames$originalName == arth]), ylim = c(0, 80))
  title(arthGroupNames$revisedName[arthGroupNames$originalName == arth], line = -1.5)
  abline(h = 10, col = 'red', lty = 'dashed', lwd = 2)
}

####### ggplot2 length analysis with r^2 / p-value / regression lines

pdf('figures/error_rates_vs_length.pdf', height = 8, width = 10)
par(mfrow = c(4,3), mar=c(2.5,4,1,1), oma = c(4, 4, 0, 2), tck = -.03, mgp = c(2, .8, 0), 
    cex.axis = 1.5, cex.main = 1.8)

# Merge group names and cleaning data (deleting NA values)
correct_plot_data <- correct_by_length %>%
  left_join(arthGroupNames, by = c("StandardGroup" = "originalName")) %>%
  filter(!is.na(Length), !is.na(errorRate), !is.na(nObs), nObs > 0)

# Plot with regression and correlation stats
ggplot(correct_plot_data, aes(x = Length, y = errorRate)) +
  geom_point(aes(size = log10(nObs) + 0.2), color = "gray40", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid", linewidth = 0.8) +
  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = "left", label.y.npc = "top", size = 3.5) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red", linewidth = 1) +
  facet_wrap(~ revisedName, scales = "free_x") +
  labs(x = "Length", y = "Error Rate") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.position = "none")


# Calculate p-values based on generalized linear models (glms) that don't assume a linear relationship
bug.glm = glm(1 - binary ~ Length, data = correctness_table[correctness_table$OriginalGroup == 'truebugs'], family = 'binomial')
summary(bug.glm)

dad.glm = glm(1 - binary ~ Length, data = correctness_table[correctness_table$OriginalGroup == 'daddy longlegs' & correctness_table$Length < 20], family = 'binomial')
summary(dad.glm)

################

# Ensure folder exists
if (!dir.exists("figures")) dir.create("figures")

pdf('figures/error_rates_vs_length.pdf', height = 8, width = 10)

# Merge group names and clean data
correct_plot_data <- correct_by_length %>%
  left_join(arthGroupNames, by = c("StandardGroup" = "originalName")) %>%
  filter(!is.na(Length), !is.na(errorRate), !is.na(nObs), nObs > 0)

# Create the plot object
p <- ggplot(correct_plot_data, aes(x = Length, y = errorRate)) +
  geom_point(aes(size = log10(nObs) + 0.2), color = "gray40", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.8) +
  stat_cor(method = "pearson",
           aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")),
           label.x.npc = "left", label.y.npc = "top", size = 3.5) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red", linewidth = 1) +
  facet_wrap(~ revisedName, scales = "free_x") +
  labs(x = "Length", y = "Error Rate") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.position = "none")

# Print it to the PDF device
print(p)

# Close the device
dev.off()



######################################################################
#
#      Beat sheet / Visual Survey Accuracy Comparison 
#
#################################################################

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

par(mfrow = c(1,1), mar = c(4, 4, 1, 1))

bsvplot = plot(errorsByMethod$errorRate[errorsByMethod$ObservationMethod == "Beat sheet"], xlab = "Beat Sheet", ylab = "Visual", main = "", errorsByMethod$errorRate[errorsByMethod$ObservationMethod == "Visual"], cex = log10 ((errorsByMethod$nTot[errorsByMethod$ObservationMethod == "Beat sheet"] + errorsByMethod$nTot[errorsByMethod$ObservationMethod == "Visual"])/2), pch = 16, col = 'salmon')

text(errorsByMethod$errorRate[errorsByMethod$ObservationMethod == "Beat sheet"], errorsByMethod$errorRate[errorsByMethod$ObservationMethod == "Visual"], errorsByMethod$OriginalGroup[errorsByMethod$ObservationMethod == "Visual"], cex = 0.7)

abline(a=0, b = 1)


#########################################################################
#
#       Game Data Analysis 
#
##########################################################################

# 1) How good are people at estimating length?
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
            first_pct_found = PercentFound[PercentFound != -1][1]) %>%
  filter(!UserFK %in% c(25, 26)) #remove records from Allen and Aaron

# Change -Inf values to NA
gameplaydf[gameplaydf == -Inf | gameplaydf == Inf] = NA

#################################################
# Figure of distribution of 3 sub game scores 
#################################################

# Compare first vs best for each subscore category
wilcox.test(gameplaydf$best_length_accuracy, gameplaydf$first_length_accuracy) # p = 9.8e-6
wilcox.test(gameplaydf$best_ID_accuracy, gameplaydf$first_ID_accuracy) # p = 1.7e-5
wilcox.test(gameplaydf$best_pct_found, gameplaydf$first_pct_found)     # p = 8.8e=6

#pdf('figures/game_scores.pdf', height = 5, width = 7)
par(mar = c(7, 5, 1, 1), cex.lab = 1.8)
vioplot(gameplaydf[gameplaydf$userplays >= 2, c('first_pct_found', 'best_pct_found', 
                       'first_ID_accuracy', 'best_ID_accuracy', 
                       'first_length_accuracy', 'best_length_accuracy')],
        col = c('goldenrod', 'goldenrod4', 'firebrick1', 'firebrick', 'turquoise', 'turquoise4'), 
        xaxt = 'n', las = 1, ylab = "Accuracy", cex.axis = 1.2, at = c(1:2, 4:5, 7:8))
axis(1, at = c(1:2, 4:5, 7:8), tck = -0.01, labels = F)
mtext(rep(c("First", "Best"), times = 3), 1, at = c(1:2, 4:5, 7:8), cex = 1.25, line = .5)
mtext(c("% Found", "Identification", "Length\nestimation"), 1, at = c(1.5, 4.5, 7.5), , cex = 1.8, padj = .5, line = 3, col = c('goldenrod4', 'firebrick', 'turquoise4'))
#print(game_scores)

#dev.off()

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
  arrange(desc(UserObsNum)) %>%
  mutate(correctrate = 100 - UserErrorRate)

######## Plots showing FIRST SUBSCORE vs SURVEY ERROR RATE ###########

# filter down to just the game scores that include subscores for a person's first time playing the game
# show 3 panels with frequency histograms of finding accuracy, id accuracy, and length accuracy
# this lets us say which things people are better at on average and which tasks people have more problems with. 

par(mfrow = c(2, 2), mar = c(4, 4, 1, 1))

# x axis is score, y-axis is count of how many users got that score

# PLOT: does the first Length Accuracy predict Survey Error Rate? 

subscores_gameplaydf = gameplaydf %>%
  select(UserFK, first_length_accuracy, first_ID_accuracy, first_pct_found) %>%
  filter(first_length_accuracy >= 0, 
         UserFK != 25,
         UserFK != 26) %>%
  mutate(n = row_number(UserFK)) %>%
  arrange(n)

par(mfrow = c(2, 2), mar = c(4, 4, 1, 1)) 

hist(subscores_gameplaydf$first_length_accuracy, xlab = "Length Score", ylab = "Score Frequency", las = 1, main = "", col = 'salmon') 

hist(subscores_gameplaydf$first_ID_accuracy, xlab = "ID Score", ylab = "Score Frequency", las = 1, main = "", col = 'salmon') 

hist(subscores_gameplaydf$first_pct_found, xlab = "Percent Found", ylab = "Score Frequency", las = 1, main = "", col = 'salmon')
  
# barplot(subscores_gameplaydf$first_length_accuracy, names.arg = subscores_gameplaydf$n, width = 0.2, xlab = "Users", ylab = "First Length Score", ylim = c(0,100), col = 'salmon', space = 0.2)
# 
# lengthavgline = mean(subscores_gameplaydf$first_length_accuracy)
#  
# abline(a = lengthavgline, b = 0, col = 'blue', )

# PLOT: does the first IDAccuracy predict Survey Error Rate?

barplot(subscores_gameplaydf$first_ID_accuracy, names.arg = subscores_gameplaydf$n, width = 0.2, xlab = "Users", ylab = "First ID Score", ylim = c(0,100), col = 'salmon', space = 0.2)

IDavgline = mean(subscores_gameplaydf$first_ID_accuracy)

abline(a = IDavgline, b = 0, col = 'blue', )


# PLOT: Does First PercentFound predict Survey Error Rate? 

barplot(subscores_gameplaydf$first_pct_found, names.arg = subscores_gameplaydf$n, width = 0.2, xlab = "Users", ylab = "First Percent Found Score", ylim = c(0,100), col = 'salmon', space = 0.2)

percentavgline = mean(subscores_gameplaydf$first_pct_found)

abline(a = percentavgline, b = 0, col = 'blue')

text(paste(round(percentavgline), 2))

############## Plot showing BEST SCORE vs. SURVEY ERROR RATE #############

par(mfrow = c(2, 2), mar = c(4, 4, 1, 1))

plot(gameplayandusererrors$maxscore, gameplayandusererrors$UserErrorRate, xlab = "Best Score", ylab = "Survey Error Rate (%)", main = "", cex = 2, col = 'dark green')

abline(lm(gameplayandusererrors$UserErrorRate ~ gameplayandusererrors$maxscore), col = 'green')

# each user's average LengthAccuracy predicts survey error rate?
# put LengthAccuracy vs. ID accuracy vs. percent found to survey error rate

######## PLOT: does LengthAccuracy predict Survey Error Rate? #######

plot(gameplayandusererrors$best_length_accuracy, gameplayandusererrors$UserErrorRate, xlab = "Best Length Accuracy", ylab = "Survey Error Rate (%)", main = "", cex = 2, ylim = c(0, 30))

bestlength = lm(gameplayandusererrors$UserErrorRate ~ gameplayandusererrors$best_length_accuracy)

lengthR2 = summary(bestlength)$r.squared                
                
abline(bestlength, col = 'green')


########## PLOT: does online IDAccuracy predict Survey Error Rate? ##########

par(mfrow = c(1,1))
  
plot(gameplayandusererrors$best_ID_accuracy, gameplayandusererrors$correctrate, xlab = "Virtual Surveys ID Scores", ylab = "On-Site Surveys ID Accuracy", cex = 2, ylim = c(70, 100), main = "", pch = 16, col = 'salmon')

abline(lm(gameplayandusererrors$correctrate~gameplayandusererrors$best_ID_accuracy), col = 'blue')


#########PLOT: Does PercentFound predict Survey Error Rate? ##########

plot(gameplayandusererrors$best_pct_found, gameplayandusererrors$UserErrorRate, xlab = "Best Percent Found", ylab = "Survey Error Rate (%)", main = "", cex = 2, ylim = c(0, 30))

abline(lm(gameplayandusererrors$UserErrorRate~gameplayandusererrors$best_pct_found), col = 'green')

########PLOT: Best ID Accuracy Scores for Hard-to-ID Arthros  #######
## we are excluding caterpillars, ants, spiders...
## we are including only leafhoppers, beetles, true bugs, grasshoppers, flies, bees, aphids

plot(gameplayandusererrors$best_ID_accuracy, gameplayandusererrors$UserErrorRate, xlab = "Best ID Accuracy", 
     ylab = "Survey Error Rate (%)", 
     main = "", 
     cex = 2, 
     ylim = c(0, 30))

abline(lm(gameplayandusererrors$UserErrorRate~gameplayandusererrors$best_ID_accuracy), col = 'green')




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

text(30, 1200, paste("r =", round(scoretest$estimate,2)))

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
text(20, 50, paste("r =",round(scoretest$estimate,2)))

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
text(20, 50, paste("r =",round(scoretest$estimate,2)))

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
text(20, 50, paste("r =",round(scoretest$estimate,2)))

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
  left_join(arthro_sight[, c("ID", "SurveyFK")], by = c("ArthropodSightingFK" = "ID")) %>%
  left_join(surveys[, c("ID", "UserFKOfObserver", "LocalDate", "LocalTime")], c("SurveyFK" = "ID")) %>%
  select("OriginalGroup", "StandardGroup", "UserFKOfObserver", "LocalDate", "LocalTime") %>%
  group_by(UserFKOfObserver) %>%
  mutate(correct = OriginalGroup == StandardGroup,
         doy = yday(LocalDate), 
         Year = as.numeric(substr(LocalDate, 1, 4)),
         yearday = Year + doy/365)

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

game_and_survey = inner_join(userCounts_filtered, surveyusererrors, by = c("UserFK" = "UserFKOfObserver")) %>%
  filter(n >= 2, UserObsNum >= 8, UserFK != 26)
  
par(mfrow = c(2, 3), mar=c(2.5,3.5,3,1))
for (user in game_and_survey$UserFK) {
  
  df1 = timestampdf %>%
    filter(UserFKOfObserver %in% user) #2066
  df2 = gamescoresdf %>%
    filter(UserFK %in% user)
  
  plot(df1$yearday, df1$correct, pch = 17, col = 'red', xlab = 'Day', ylab = '', 
       yaxt = 'n', main = user, xlim = c(min(df1$yearday, df2$yearday), max(df1$yearday, df2$yearday)))
  
  abline(v = df2$yearday, col = 'blue')
#  points(df2$yearday, rep(1, nrow(df2)), pch = 16, cex = 2, col = 'blue')
  
  #tmp1 = filter(timestampdf, UserFKOfObserver %in% user)
  #tmp2 = filter(gamescoresdf, UserFK %in% user)
  
  # plot surveys
#   plot(tmp$yearday, rep(1, nrow(tmp1)), pch = 17, col = 'red', xlim = c(2000,2204), xlab = "Day", ylab = "", ylim = c(0,1), yaxt = "n", main = user)
#   
#   # plot game play dates
#   points(tmp2$yearday, rep(1, nrow(tmp2)), pch = 16, col = 'blue')
}


#################################################################################
############   OLD STUFF ########################################################

# UNFINISHED: not a super useful plot. 
# modify: make points increase or decrease to represent sample size

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
            rate15plus = 100*error15plus/n15plus)

#this was attached to the above code: idk what it was for - 
#las = 1, ylim = c(0,  1.2*max(lengthdf[lengthdf$OriginalGroup == arth, c("rate0.5", "rate5.15", "rate15plus")])) 

par(mfrow = c(4,3), mar=c(2.5,5,1,1))

for (arth in lengthdf$OriginalGroup) { plot(1:3, lengthdf[lengthdf$OriginalGroup == arth, c("rate0.5", "rate5.15", "rate15plus")], type = 'b', main = arth, ylab = "% error", xaxt = "n", xlab = "", xlim = c(0.5, 3.5), mtext(c("2-5 mm", "5-15 mm", "15+ mm"), 1, at = 1:3, line = 0.3, cex = 0.45))}
# fly disparity? truebugs? 

############## Plot: Incorrect/Correct ID vs Length of Arthropod #############

#####################  OLD ##################################

par(mfrow = c(4,3), mar=c(2.5,4,0,1), oma = c(4, 1, 1, 2))

for (arth in arthGroupsWeWant) { 
  
  arthSubset = filter(correctness_table, StandardGroup == arth)
  
  # Logistic regression curve:
  
  arthGLM = glm(binary ~ Length, data = arthSubset, family = "binomial")
  
  predicted_data = data.frame(Length = seq(min(arthSubset$Length, na.rm = TRUE), max(arthSubset$Length, na.rm=TRUE)))
  
  predicted_data$binary = predict(arthGLM, predicted_data, type="response")
  
  slope = round(coef(arthGLM)[2], 3)
  p = summary(arthGLM)$coefficients[2, 4]
  
  pstar = case_when(p < 0.001 ~ "p<0.001",
                    p < 0.01 & p > 0.001 ~ "p < 0.01 & p > 0.001",
                    p < 0.05 & p > 0.01 ~ "p < 0.05 & p > 0.01",
                    p > 0.05 ~ "p>0.05",
                    .default = "")
  
  plot(jitter(arthSubset$Length, .5), jitter(arthSubset$binary, 0.3),
       xlab = "", las = 1, yaxt = "n", ylab = "")
  
  title(arth, line = -2.5)
  
  #mtext("Incorrect <------> Correct", 2, line = .5, cex = .5)
  
  lines(binary ~ Length, predicted_data, lwd=2, col="green")
  
  abline(h = 0.9, col = 'red', lty = 'dotted')
  
  minLength.9 = min(predicted_data$Length[predicted_data$binary >= 0.9])
  
  abline(v = minLength.9, col = 'blue', lty = 'dotted')
  
  #p_value = t.test(arthSubset$Length, arthSubset$binary)$p.value
  
  text(x = .8*max(arthSubset$Length, na.rm = T), y = .2, labels = paste0(pstar), cex = 0.68)
  
}
mtext("Length (mm)", 1.3, cex = 1, outer = TRUE, line = 1)


