## Script for Identifying the types of errors made by citizen scientists in Caterpillars Count!

# Add a new comment

## Load libraries
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
library(scales)

## Read in raw data
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


#########################################################################
# Virtual Survey Game Data Analysis 
###########################################################################

# For each user that has played the game at least twice, we want the 1st score and max/"best" score 
# for all 3 subscores (lengths, percentfound, IDaccuracy)

gameplaydf =  game %>%
  filter(PercentFound > 25,   # Filter records that likely reflect a user that bailed out of the game early
         IdentificationAccuracy != -1) %>%  # Filter out records from before subscores were kept (which are stored as -1)
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
  filter(!UserFK %in% c(25, 26),   #remove records from Allen and Aaron (admin testers)
         userplays >= 2)           #users with at least 2 plays 


################################################################
# Figure 2 - Comparisons of distributions of 3 sub game scores 
################################################################

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


pdf('figures/Figure2_game_scores.pdf', height = 5, width = 7)
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

segments(x0= c(1.1, 1.1, 2), y0 = c(2, 0, 0), x1 = c(1.1, 2, 2), y1 = c(0, 0, 2))
segments(x0= c(4.1, 4.1, 5), y0 = c(2, 0, 0), x1 = c(4.1, 5, 5), y1 = c(0, 0, 2))
segments(x0= c(7.1, 7.1, 8), y0 = c(2, 0, 0), x1 = c(7.1, 8, 8), y1 = c(0, 0, 2))
text(1.5, -4, labels = "***", cex = 2) 
text(4.5, -4, labels = "***", cex = 2) 
text(7.5, -4, labels = "***", cex = 2) 

segments(x0= c(2, 2, 4.9), y0 = c(101, 103, 103), x1 = c(2, 4.9, 4.9), y1 = c(103, 103, 101))
text(3.5, 106, "**", cex = 2)
segments(x0= c(5.1, 5.1, 8), y0 = c(101, 103, 103), x1 = c(5.1, 8, 8), y1 = c(103, 103, 101))
text(6.5, 106, "***", cex = 2)
segments(x0= c(2, 2, 8), y0 = c(108, 110, 110), x1 = c(2, 8, 8), y1 = c(110, 110, 108))
text(4.7, 113, "**", cex = 2)

dev.off()


















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
     y = "False Positive %",
     fill = "Actual Group"
   ) +
   theme_bw() +
   theme(
     legend.text = element_text(size = 11),
     legend.title = element_text(size = 14),
     axis.title = element_text(size = 16),
     axis.text.x = element_text(size = 14, angle = 45, hjust = 1, vjust = 1),
     axis.text.y = element_text(size = 14),
     plot.margin = unit(c(.1, .5, .2, .5), "cm"),
     legend.box.margin = margin(t = 30),
     legend.margin = margin(t = 35)
   ) +
  labs(tag = "(a)") +
  theme(plot.tag = element_text(size = 20))

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
    name = "Reported As") +
  scale_y_continuous(breaks = seq(0, 60, by = 10)) +
  labs(x = "Arthropod Group", 
       y = "False Negative %", 
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
  theme(plot.margin = unit(c(.1,.5,.2,.5), "cm"),
        legend.box.margin = margin(t = 20),
        legend.margin = margin(t = 20)) +
  labs(tag = "(b)") +
  theme(plot.tag = element_text(size = 20))


pdf('figures/Figure3_misidentifications.pdf', height = 9, width = 7)
grid.arrange(stacked, rev_stacked, nrow=2)
dev.off()



##############################################################
## Length vs % Error per arthropod #############
##############################################################

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

pdf('figures/Figure4_error_rates_vs_length.pdf', height = 6, width = 8)

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
  
  length_estimates = rbind(length_estimates, tmp.df)

  p_display = case_when(tmp.df$p < .0001 ~ '***',
                        tmp.df$p < .001 ~ '**',
                        round(tmp.df$p,2) <= .01 ~ '*',
                        .default = '')
  # Plot panel
  
  arthSubset = filter(correct_by_length, StandardGroup == arth)
  
  plot(arthSubset$Length, arthSubset$errorRate, xlab = "", las = 1, 
       ylab = "", cex = log10(arthSubset$nObs)+.2, pch = 16, col = 'gray60',
       xlim = c(0, arthGroupNames$maxLength[arthGroupNames$originalName == arth]), ylim = c(0, 80))
  
  # Arthropod group title
  title(paste(arthGroupNames$revisedName[arthGroupNames$originalName == arth], p_display), 
        line = -1.3, cex.main = 1.7)
  # p-value

  abline(h = 10, col = 'red', lty = 'dashed', lwd = 2)

}

mtext("Length (mm)", 1, cex = 2, outer = TRUE, line = 2)
mtext("False negative %", 2, cex = 2, outer = TRUE, line = 1.5)

dev.off()





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
  left_join(expert_ID, by = c("ArthropodSightingFK", "OriginalGroup")) %>%
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
                         x.label = "Cumulative number of photos", y.label = "False negative %") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  theme_classic(base_size = 18) +
  theme(axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))

pdf('figures/Figure5_errors_over_time.pdf', height = 5, width = 7)
error.plot 
dev.off()

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
pdf('figures/Figure6_length_accuracy.pdf', height = 4.5, width = 8)
par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 1.3, 0))
boxplot(lengthdata[, c(1, 3, 5)], xaxt = "n", xlab = "Treatment", ylab = "Deviation (mm)", cex.lab = 1.5, 
        col = c('dodgerblue', 'salmon', 'limegreen'), las = 1, ylim = c(-22, 42))
abline(h = 0, lty = 'dashed')
mtext("(a)", side = 3, line = -0.5, adj = 0, cex = 2, outer = TRUE)
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
mtext("(b)", side = 3, line = -0.5, adj = 0.53, cex = 2, outer = TRUE)
mtext(c("Control", "Thumb", "Ruler"), 1, at = 1:3, line = 1, cex = 1.3)
segments(x0= c(1, 1, 2), y0 = c(110, 117, 117), x1 = c(1, 2, 2), y1 = c(117, 117, 110))
text(1.5, 122, "**", cex = 2)
segments(x0= c(1, 1, 3), y0 = c(122, 129, 129), x1 = c(1, 3, 3), y1 = c(129, 129, 122))
text(2, 135, "**", cex = 2)
segments(x0= c(2, 2, 3), y0 = c(50, 57, 57), x1 = c(2, 3, 3), y1 = c(57, 57, 50))
text(2.5, 67, expression(italic(p) == "0.40"), cex = 1.2)
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




