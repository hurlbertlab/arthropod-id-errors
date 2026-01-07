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
library(RColorBrewer)
library(lme4)
library(jtools) #for effect_plot()

# Read in raw data

expert_ID = read.csv("data/2025-12-12_ExpertIdentification.csv", quote = '\"', fill = TRUE)
expert_ID$OriginalGroup[expert_ID$SawflyUpdated == 1 & expert_ID$OriginalGroup == 'bee'] = 'sawfly larvae'
expert_ID$StandardGroup[expert_ID$SawflyUpdated == 1] = 'sawfly larvae'

# Fix two records manually that the user assumed originally were sawfly larvae (but in one case forgot to check the box)
expert_ID$OriginalGroup[expert_ID$ArthropodSightingFK %in% c(116543,129308)] = 'sawfly larvae'

surveys = read.csv("data/2025-12-12_Survey.csv", quote = '\"', fill = T)
game = read.csv("data/2025-12-12_VirtualSurveyScore.csv")
arthro_sight = read.csv("data/2025-12-12_ArthropodSighting.csv")


# Arthropod groups and their revised labels
arthGroupsWeWant = c("ant", "aphid", "bee", "beetle", "caterpillar", 
                     "daddylonglegs", "fly", "grasshopper", "leafhopper",
                     "moths", "spider", "truebugs", "sawfly larvae")

arthGroupNames = data.frame(originalName = arthGroupsWeWant,
                            revisedName = c("ants", "aphids", "bees, wasps", "beetles",
                                            "caterpillars", "daddy longlegs", "flies",
                                            "grasshoppers", "leafhoppers", "moths",
                                            "spiders", "true bugs", "sawfly larvae"),
                            maxLength = c(15, 10, 22, 30, 60, 15, 25, 40, 25, 25, 22, 35, 50))



# total_OG_obs is the total number of observations submitted as a given group,
# and will be used as the denominator for calculating error rates for the
# first stacked bar chart ("Originally submitted as...")
total_OG_counts = expert_ID %>%
  group_by(OriginalGroup) %>%
  summarize(total_OG_obs = n())

# total_SG_obs is the total number of observations of each StandardGroup,
# and will be used as the denominator for calculating error rates for the
# second stacked bar chart ("Actual Group")
total_SG_counts = expert_ID %>%
  group_by(StandardGroup) %>%
  summarize(total_SG_obs = n())

# Lumping all StandardGroup id's that are not in arthGroupsWeWant in a category called "other"
# for which we will calculate a single lumped error rate that should be included in bar stack
error_num = expert_ID %>%
  select(OriginalGroup, StandardGroup, SawflyUpdated) %>%
  mutate(StandardGroup2 = ifelse(StandardGroup %in% arthGroupsWeWant, StandardGroup, "other")) %>%
  group_by(OriginalGroup, StandardGroup2, SawflyUpdated) %>%
  summarize(number = n()) %>% 
  left_join(total_OG_counts, by = "OriginalGroup") %>%
  left_join(total_SG_counts, by = c("StandardGroup2" = "StandardGroup")) %>%
  mutate(errorRate1 = round((number / total_OG_obs) * 100, 2),
         errorRate2 = round((number / total_SG_obs) * 100, 2)) %>%
  arrange(OriginalGroup, desc(errorRate1)) 


#######################################################################
#
#    Arthropod Mis-identification Analysis: On-Site / Field Data
#
######################################################################

####### Plot: Stacked bar graph: "What Arthropods are Mistaken For" #######

only_error_num = error_num %>%
  filter(OriginalGroup != StandardGroup2,
         #StandardGroup %in% arthGroupsWeWant, 
         OriginalGroup %in% arthGroupsWeWant) %>%
  left_join(arthGroupNames[, c('originalName', 'revisedName')], by = c('StandardGroup2' = 'originalName')) %>%
  rename(StandardGroupRevised = revisedName) %>%
  left_join(arthGroupNames[, c('originalName', 'revisedName')], by = c('OriginalGroup' = 'originalName')) %>%
  rename(OriginalGroupRevised = revisedName) %>%
  mutate(StandardGroupRevised = ifelse(is.na(StandardGroupRevised), "other", StandardGroupRevised))

# Order groups according to descending summed errorRate1
order1 = only_error_num %>%
  group_by(OriginalGroupRevised) %>%
  summarize(totalError1 = sum(errorRate1, na.rm = T)) %>%
  arrange(desc(totalError1))

order2 = only_error_num %>%
  group_by(StandardGroupRevised) %>%
  summarize(totalError2 = sum(errorRate2, na.rm = T)) %>%
  arrange(desc(totalError2))


only_error_num$OriginalGroupRevised = factor(only_error_num$OriginalGroupRevised, 
                                             levels = order1$OriginalGroupRevised)

only_error_num$StandardGroupRevised = factor(only_error_num$StandardGroupRevised, 
                                             levels = order2$StandardGroupRevised)

# Revise colors?
colors = brewer.pal(12, "Paired")

color_values = c(
  "ants" = colors[1],
  "aphids" = colors[2],
  "bees, wasps" = colors[3],
  "beetles" = colors[4],
  "caterpillars" = colors[5],
  "daddy longlegs" = colors[6],
  "flies" = colors[7],
  "grasshoppers" = colors[8],
  "leafhoppers" = colors[9],
  "moths" = colors[10],
  "sawfly larvae" = "gray90",
  "spiders" = colors[11],
  "true bugs" = colors[12],
  "other" = "gray50")

stacked = ggplot(only_error_num, aes(fill = StandardGroupRevised, y = errorRate1, 
  x = OriginalGroupRevised)) +   geom_bar(position = 'stack', stat = 'identity') +
  scale_fill_manual(
    values = color_values,
    breaks = sort(names(color_values)),  
    name = "Actual Group") +
   scale_y_continuous(breaks = seq(0, 40, by = 5)) +
   labs(
     x = "Originally Reported As...",
     y = "% Error Rate",
     fill = "Actual Group"
   ) +
   theme_bw() +
   theme(
     legend.text = element_text(size = 11),
     legend.title = element_text(size = 14),
     axis.title = element_text(size = 16),
     axis.text.x = element_text(size = 14, angle = 45, hjust = 1, vjust = 1),
     axis.text.y = element_text(size = 14),
     plot.margin = unit(c(0.1, .5, .2, .5), "cm")
   )

#pdf('figures/misidentified1.pdf', height = 5, width = 7)
print(stacked)
#dev.off()


######## Plot:"What are certain arthropods typically suspected as?" ##########
only_error_num$StandardGroupRevised = factor(only_error_num$StandardGroupRevised, 
                                             levels = order2$StandardGroupRevised)

rev_stacked = ggplot(only_error_num[only_error_num$StandardGroup2 != "other",], 
                     aes(fill=OriginalGroupRevised, y=errorRate2, x=StandardGroupRevised)) +
  geom_bar(position='stack', 
           stat = 'identity') + 
  scale_fill_manual(
    values = color_values,
    breaks = sort(names(color_values)),  
    name = "Arthropod Group") +
  scale_y_continuous(breaks = seq(0, 60, by = 10)) +
  labs(x = "Actual Group", 
       y = "% Error Rate", 
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
  theme(plot.margin = unit(c(0,.5,.2,.5), "cm"))

#pdf('figures/misidentified2.pdf', height = 5, width = 7)
print(rev_stacked)
#dev.off()

#pdf('figures/2-panel_misidentifications.pdf', height = 11, width = 7)
grid.arrange(stacked, rev_stacked, nrow=2)
#dev.off()


# TOTAL MISID RATES
# a) at what rate were incorrect submissions entered?
# incorrect submissions / total (original + standard) submissions 
# total error per original group
# average rate ?? summary metric or precise rates:
# (total number of misidentified samples / total samples)


# b) at what rate were arthropods incorrectly identified? 
# total error per standard group 

# do we want to find the SUM of all the error rates per originalgroup / standardgroup?
# then average that?


#### Single rates 

# OriginalGroup misidentification rate 
global_error_rate1 = mean(only_error_num$errorRate1, na.rm = TRUE)
print(global_error_rate1)

# StandardGroup misidentification rate
global_error_rate2 = mean(only_error_num$errorRate2, na.rm = TRUE)
print(global_error_rate2)



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
  filter(StandardGroup %in% arthGroupsWeWant) %>% 
  #filter(!(OriginalGroup == 'daddylonglegs' & Length > 10)) %>%
  mutate(agreement = OriginalGroup==StandardGroup, binary = as.integer(agreement)) %>% 
  group_by(OriginalGroup, Length)
  
correct_by_length = correctness_table %>%  #counts # of incorrect length observations
  group_by(StandardGroup, Length) %>%
  summarize(nObs = n(),
            nWrong = sum(!agreement),
            errorRate = 100*nWrong/nObs)

######### Plotting error rates vs length and running glms

# Store parameter estimate and p-values from GLMs in this dataframe
length_estimates = data.frame(StandardGroup = NULL,
                              length_estimate = NULL,
                              p = NULL)

#pdf('figures/error_rates_vs_length.pdf', height = 8, width = 10)

par(mfrow = c(3,3), mar=c(2.5,4,1,1), oma = c(4, 4, 1, 1), tck = -.03, mgp = c(2, .8, 0), 
    cex.axis = 1.5, cex.main = 1.8)


# Panels in order of error trends:
# --not showing caterpillars, ants and spiders which have uniformly low error rates
# --
for (arth in c("truebugs", "leafhopper", "bee", "moths", "beetle", 
               "grasshopper", "fly", "daddylonglegs", "aphid")) { 
  
  # GLM
  tmp.glm = glm(1 - binary ~ Length, 
                data = correctness_table[correctness_table$StandardGroup == arth,], 
                family = 'binomial')
  
  tmp.df = data.frame(StandardGroup = arth,
                      length_estimate = summary(tmp.glm)$coefficients[2, 1],
                      p = summary(tmp.glm)$coefficients[2, 4])
  
  #tmp.plot = effect_plot(tmp.glm, pred = Length, interval = TRUE, int.type = 'confidence', 
   #                      y.label = 'Error rate', x.label = 'Length (mm)',
   #                      main.title = arth)
  
  #assign(paste0(arth, '.plot'), tmp.plot)
  
  length_estimates = rbind(length_estimates, tmp.df)



  p_display = case_when(tmp.df$p < .0001 ~ '***',
                        tmp.df$p < .001 ~ '**',
                        round(tmp.df$p,2) <= .01 ~ '*',
                        .default = '')
  #p_display = if_else(round(tmp.df$p,2) <= .01, paste("p =", signif(tmp.df$p, 2)), '')
  

  # Plot panel
  
  arthSubset = filter(correct_by_length, StandardGroup == arth)
  
  plot(arthSubset$Length, arthSubset$errorRate, xlab = "", las = 1, 
       ylab = "", cex = log10(arthSubset$nObs)+.2, pch = 16, col = 'gray60',
       xlim = c(0, arthGroupNames$maxLength[arthGroupNames$originalName == arth]), ylim = c(0, 80))
  
  # Arthropod group title
  title(paste(arthGroupNames$revisedName[arthGroupNames$originalName == arth], p_display), 
        line = -1.3, cex.main = 1.7)
  # p-value
  #title(p_display, line = -3, cex.main = 1.3)
  
  abline(h = 10, col = 'red', lty = 'dashed', lwd = 2)

}

mtext("Length (mm)", 1, cex = 2, outer = TRUE, line = 2)
mtext("Error rate (%)", 2, cex = 2, outer = TRUE, line = 1.5)


# Alternate ggplot2 plotting of responses using effect_plot()
# ggarrange(truebugs.plot, leafhopper.plot, bee.plot, moths.plot, beetle.plot,
#           grasshopper.plot, fly.plot, daddylonglegs.plot, aphid.plot,
#           ncol = 3, nrow = 3)






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
  filter(PercentFound > 25,   # Filter records that likely reflect a user that bailed out of the game early
         IdentificationAccuracy != -1) %>%  # Filter out records from before subscores were kept (they are stored as -1)
  select(UserFK, Score, LengthAccuracy, IdentificationAccuracy, PercentFound) %>%
  group_by(UserFK) %>%
  summarize(userplays = n(), 
            maxscore = max(Score, na.rm = TRUE),
            first = Score[1], 
            max = max(Score, na.rm = TRUE),
            best_length_accuracy = max(LengthAccuracy, na.rm = TRUE),
            best_ID_accuracy = max(IdentificationAccuracy, na.rm = TRUE),
            best_pct_found = max(PercentFound, na.rm = TRUE),
            first_length_accuracy = LengthAccuracy[1],
            first_ID_accuracy = IdentificationAccuracy[1],
            first_pct_found = PercentFound[1]) %>%
  filter(!UserFK %in% c(25, 26),   #remove records from Allen and Aaron
         userplays >= 2)           #users with at least 2 plays 


#################################################
# Figure of distribution of 3 sub game scores 
#################################################

## Compare first vs best for each subscore category
wilcox.test(gameplaydf$best_pct_found, gameplaydf$first_pct_found, paired = TRUE)     # p = 2.46e-12
wilcox.test(gameplaydf$best_ID_accuracy, gameplaydf$first_ID_accuracy, paired = TRUE) # p = 1.13e-11
wilcox.test(gameplaydf$best_length_accuracy, gameplaydf$first_length_accuracy, paired = TRUE) # p = 5.29e-12

## Compare best subscores across categories
# ID better than % found, p = 0.0004
wilcox.test(gameplaydf$best_pct_found, gameplaydf$best_ID_accuracy, paired = TRUE)     
# ID better than length, p = 9.67e-11
wilcox.test(gameplaydf$best_ID_accuracy, gameplaydf$best_length_accuracy, paired = TRUE)
# % found better than length, p = 0.0004
wilcox.test(gameplaydf$best_length_accuracy, gameplaydf$best_pct_found, paired = TRUE)  


#pdf('figures/game_scores.pdf', height = 5, width = 7)
par(mar = c(7, 5, 1, 1), cex.lab = 1.8)
vioplot(gameplaydf[gameplaydf$userplays >= 2, c('first_pct_found', 'best_pct_found', 
                       'first_ID_accuracy', 'best_ID_accuracy', 
                       'first_length_accuracy', 'best_length_accuracy')],
        col = c('goldenrod', 'goldenrod4', 'firebrick1', 'firebrick', 'turquoise', 'turquoise4'),
        xaxt = 'n', las = 1, cex.axis = 1.2, at = c(1:2, 4:5, 7:8), ylim = c(-4, 113))
axis(1, at = c(1:2, 4:5, 7:8), tck = -0.01, labels = F)
mtext("Accuracy", side = 2, line = 3, cex = 2)
mtext(rep(c("First", "Best"), times = 3), 1, at = c(1:2, 4:5, 7:8), cex = 1.25, line = .5)
mtext(c("% Found", "% Identified", "Length\naccuracy"), 1, at = c(1.5, 4.5, 7.5), , cex = 1.8, padj = .5, line = 3, col = c('goldenrod4', 'firebrick', 'turquoise4'))
text(1.5, -3, labels = "***", cex = 2) 
text(4.5, -3, labels = "***", cex = 2) 
text(7.5, -3, labels = "***", cex = 2) 

segments(x0= c(2, 2, 4.9), y0 = c(101, 103, 103), x1 = c(2, 4.9, 4.9), y1 = c(103, 103, 101))
text(3.5, 106, "**", cex = 2)
segments(x0= c(5.1, 5.1, 8), y0 = c(101, 103, 103), x1 = c(5.1, 8, 8), y1 = c(103, 103, 101))
text(6.5, 106, "***", cex = 2)
segments(x0= c(2, 2, 8), y0 = c(108, 110, 110), x1 = c(2, 8, 8), y1 = c(110, 110, 108))
text(4.7, 113, "**", cex = 2)

#dev.off()



####################################################
# Identification error rate over time
####################################################

# Create dataframe that has the cumulative number of surveys and photos, and 
# cumulative error rate by user.
# Here we exclude observations submitted as caterpillars, spiders, or ants which
# are known to have extremely low error rates.
# Error rate is the percent of observations that were incorrectly submitted 
# (i.e., the percent of observations submitted as Group X that were not actually Group X).

df = left_join(surveys, arthro_sight, by = c('ID' = 'SurveyFK')) %>%
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
                         x.label = "Cumulative number of photo observations", y.label = "Error rate")

error.plot + theme(axis.text.x = element_text(size = 18),
                   axis.text.y = element_text(size = 18),
                   axis.title.x = element_text(size = 20),
                   axis.title.y = element_text(size = 20))


# Visualizing individual user examples
# Function for making an error over time (vs. number of surveys) plot
errorsOverTimePlot = function(UserID, dataframe = df, new = TRUE, ...) {
  
  tmp = filter(dataframe, UserFKOfObserver == UserID)
  
  if(new) {
    plot(tmp$photoObsNum, tmp$cumErrorRate/100, type = 'l', xlab = "Number of surveys", 
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



############################
# Length accuracy
############################
# Students were asked to measure the length of 6 arthropod specimens.

# Students were assigned to one of 3 treatments:
#  A - control, students were asked to estimate the length 
#      in millimeters without any tools or prompting of length reference points.
#  B - students were allowed to measure the width of their thumbnail and/or 
#      fingernail in millimeters immediately prior to being asked to estimate length of specimens
#      (i.e. they could use their thumb to aid in estimation)
#  C - students were provided a ruler to aid in estimating arthropod length


# Survey results with length estimations
results = read.csv('data/raw_length_estimates.csv', header = T)

names(results)[2:8] = c('Group', 'Specimen1_8', 'Specimen2_21', 'Specimen3_6', 
                        'Specimen4_31', 'Specimen5_12', 'Specimen6_19')

results$color = case_when(results$Group == 'A' ~ 'dodgerblue',
                          results$Group == 'B' ~ 'salmon',
                          results$Group == 'C' ~ 'limegreen')

results$symbol = case_when(results$Group == 'A' ~ 17,
                           results$Group == 'B' ~ 15,
                           results$Group == 'C' ~ 16)

# One student (row 10) measured in cm, while one student (row 4) measured in tenths of mm.

# Plotting revised data
results2 = results
results2[4, 3:8] = results[4, 3:8]/10
results2[10, 3:8] = results[10, 3:8]*10


long = pivot_longer(results2, cols = Specimen1_8:Specimen6_19, names_to = "Specimen")
long$trueLength = as.numeric(word(long$Specimen, 2, sep = "_"))
long$deviation = long$value - long$trueLength
long$pctdev = 100*long$deviation/long$trueLength

long %>% group_by(Group) %>% summarize(medDev = median(deviation), medPct = median(pctdev))

lengthdata = data.frame(Control_deviation = long$deviation[long$Group == 'A'],
                        Control_pctdev = long$pctdev[long$Group == 'A'],
                        Thumb_deviation = c(long$deviation[long$Group == 'B'], rep(NA, 108 - sum(long$Group == 'B'))),
                        Thumb_pctdev = c(long$pctdev[long$Group == 'B'], rep(NA, 108 - sum(long$Group == 'B'))),
                        Ruler_deviation = c(long$deviation[long$Group == 'C'], rep(NA, 108 - sum(long$Group == 'C'))),
                        Ruler_pctdev = c(long$pctdev[long$Group == 'C'], rep(NA, 108 - sum(long$Group == 'C'))))


# Wilcoxon tests
wilcox.test(lengthdata$Control_deviation, lengthdata$Thumb_deviation, paired = FALSE)
wilcox.test(lengthdata$Ruler_deviation, lengthdata$Thumb_deviation, paired = FALSE)
wilcox.test(lengthdata$Control_deviation, lengthdata$Ruler_deviation, paired = FALSE)

wilcox.test(lengthdata$Control_pctdev, lengthdata$Thumb_pctdev, paired = FALSE)
wilcox.test(lengthdata$Ruler_pctdev, lengthdata$Thumb_pctdev, paired = FALSE)
wilcox.test(lengthdata$Control_pctdev, lengthdata$Ruler_pctdev, paired = FALSE)

# Plot
pdf('figures/Figure6_length_accuracy.pdf', height = 5, width = 8)
par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2.2, 0))
boxplot(lengthdata[, c(1, 3, 5)], xaxt = "n", xlab = "Treatment", ylab = "Deviation (mm)", cex.lab = 1.5, 
        col = c('dodgerblue', 'salmon', 'limegreen'), las = 1, ylim = c(-22, 42))
abline(h = 0, lty = 'dashed')
mtext("(a)", side = 3, line = 0.5, adj = 0, cex = 2, outer = TRUE)
mtext(c("Control", "Thumb", "Ruler"), 1, at = 1:3, line = 1, cex = 1.3)
segments(x0= c(1, 1, 1.9), y0 = c(34, 36, 36), x1 = c(1, 1.9, 1.9), y1 = c(36, 36, 34))
text(1.5, 38, "**", cex = 2)
segments(x0= c(1, 1, 3), y0 = c(38, 40, 40), x1 = c(1, 3, 3), y1 = c(40, 40, 38))
text(2, 42, "**", cex = 2)
segments(x0= c(2, 2, 3), y0 = c(18, 20, 20), x1 = c(2, 3, 3), y1 = c(20, 20, 18))
text(2.5, 23, expression(italic(p) == 0.51), cex = 1.2)


boxplot(lengthdata[, c(2, 4, 6)], xaxt = "n", xlab = "Treatment", ylab = "Deviation (%)", cex.lab = 1.5, 
        col = c('dodgerblue', 'salmon', 'limegreen'), las = 1, ylim = c(-80, 137))
abline(h = 0, lty = 'dashed')
mtext("(b)", side = 3, line = 0.5, adj = 0.53, cex = 2, outer = TRUE)
mtext(c("Control", "Thumb", "Ruler"), 1, at = 1:3, line = 1, cex = 1.3)
segments(x0= c(1, 1, 2), y0 = c(110, 117, 117), x1 = c(1, 2, 2), y1 = c(117, 117, 110))
text(1.5, 122, "**", cex = 2)
segments(x0= c(1, 1, 3), y0 = c(122, 129, 129), x1 = c(1, 3, 3), y1 = c(129, 129, 122))
text(2, 135, "**", cex = 2)
segments(x0= c(2, 2, 3), y0 = c(50, 57, 57), x1 = c(2, 3, 3), y1 = c(57, 57, 50))
text(2.5, 67, expression(italic(p) == "0.40"), cex = 1.2)
dev.off()









############################
############################
###
###   OLD STUFF
###
############################
############################



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

for (arth in arthGroupsWeWant[1:12]) { 
  
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



# Old length multipanel plot that can be modified / prettified
par(mfrow = c(4,3), mar=c(2.5,4,0,1), oma = c(4, 1, 1, 2))

for (arth in arthGroupsWeWant[1:12]) { 
  
  
  tmp = correct_by_length[correct_by_length$StandardGroup == arth,]
  
  plot(tmp$Length[tmp$StandardGroup == arth],
       tmp$errorRate[tmp$StandardGroup == arth], pch = 16,
       cex = log10(tmp$nObs[tmp$StandardGroup == arth])+.2,
       xlab = "", las = 1, ylab = "")
  
  title(arth, line = -2.5)
  
}
mtext("Length (mm)", 1.3, cex = 1, outer = TRUE, line = 1)
