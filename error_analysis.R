# Script for Identifying the types of errors made by citizen scientists in Caterpillars Count!

# Load libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)
library(popbio)
library(ggpubr)

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


# table: surveys, observations, photos
# graph: user error rates over time
# graph: calculate arthropod group-specific error rates


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
  
  # Creating a logistic regression curve:
  
   arthGLM = glm(binary ~ Length, data = arthSubset, family = "binomial")
  # 
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
   
   mtext("Incorrect <------> Correct", 2, line = .5, cex = .8)
        
   lines(binary ~ Length, predicted_data, lwd=2, col="green")
   
   abline(h = 0.9, col = 'red', lty = 'dotted')
   
   minLength.9 = min(predicted_data$Length[predicted_data$binary >= 0.9])
   
   abline(v = minLength.9, col = 'blue', lty = 'dotted')

   text(x = .8*max(arthSubset$Length, na.rm = T), y = .2, labels = paste0("slope = ", slope, pstar))

}
mtext("Length (mm)", 1, cex = 2, outer = TRUE, line = 2)


  # Plot glm predicted response curve using example code here:
  # https://www.geeksforgeeks.org/how-to-plot-a-logistic-regression-curve-in-r/


#for (arth in correctness_table$OriginalGroup) { glm(correctness_table$binary ~ correctness_table$Length)}

correctness_plot = correctness_table %>%
  filter(correctness_table$OriginalGroup %in% 'ant') %>%
  logi.hist.plot(Length, agreement)

# Beat sheet / Visual Survey Accuracy Comparison

# 1
# need to join expert_ID to arthro_sight to get SurveyFK column, then join to surveys to get ObservationMethod column
# group_by needs to include ObservationMethod

errorsByMethod = expert_ID %>%
  left_join(arthro_sight[, c("ID", "SurveyFK")], c("ArthropodSightingFK" = "ID")) %>%
  left_join(surveys[, c("ID", "ObservationMethod")], c("SurveyFK" = "ID")) %>% 
  filter(!OriginalGroup %in% c("other", "unidentified")) %>%
  group_by(ObservationMethod, OriginalGroup) %>%
  summarize(nTot = n(),
            numIncorrect = sum(OriginalGroup != StandardGroup),
            errorRate = 100*numIncorrect/nTot)



plot(errorsByMethod$errorRate[errorsByMethod$ObservationMethod == "Beat sheet"], errorsByMethod$errorRate[errorsByMethod$ObservationMethod == "Visual"], cex = log10((errorsByMethod$nTot[errorsByMethod$ObservationMethod == "Beat sheet"] + errorsByMethod$nTot[errorsByMethod$ObservationMethod == "Visual"])/2), pch = 16, col = 'salmon')
text(errorsByMethod$errorRate[errorsByMethod$ObservationMethod == "Beat sheet"], errorsByMethod$errorRate[errorsByMethod$ObservationMethod == "Visual"], errorsByMethod$OriginalGroup[errorsByMethod$ObservationMethod == "Visual"])
abline(a=0, b = 1)



# 2
# ultimately only need 100 - correct assignments for error rate
# separate out into separate beat sheet or visual survey dataframes
# we need a value for the error rate that occurs for BEAT SHEETS,
# rather that just a general, total error rate...

beatsheet_df = left_join(surveyFK_obs, error_num, c("OriginalGroup.x" = "OriginalGroup")) %>%
  filter(ObservationMethod == 'Beat sheet') %>%
  select(OriginalGroup.x, UpdatedGroup, error_rate, ID, ObservationMethod) #did not include ReviewedAndApproved because I assumed the StandardGroup accounted for this?
#why are the notes repeated?

visual_df = left_join(error_num, surveyFK_obs, c("OriginalGroup" = "OriginalGroup.x")) %>%
  filter(ObservationMethod == 'Visual')

# 3
# then join those together by OriginalGroup
# then you can plot error rates for one method against the other, do a linear regression

# copy and pasted for observation method analysis...

true_counts = expert_ID %>%
  group_by(OriginalGroup, StandardGroup) %>%
  summarize(occurrences = n())

# total_counts shows total amount of OriginalGroup IDs 
total_counts = expert_ID %>%
  group_by(OriginalGroup) %>%
  summarize(total_ID = n())

# use left_join() to compare total with proportion from true_counts
error_num = true_counts %>%
  group_by(OriginalGroup) %>% 
  left_join(total_counts, true_counts, by = c("OriginalGroup" = "OriginalGroup")) %>%
  filter(OriginalGroup != StandardGroup) %>%
  mutate(rate = round((occurrences / total_ID) * 100, 1), error_rate = (100 - sum(rate))) %>%
  arrange(OriginalGroup, desc(rate)) 

 # end goal is a linear regression of BS error % versus Visual error %, with EACH POINT as an "OriginalGroup". Each originalgroup should have x = bs error rate (one value), and y = visual error rate (one value)


