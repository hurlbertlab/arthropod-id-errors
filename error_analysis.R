# Script for Identifying the types of errors made by citizen scientists in Caterpillars Count!

# Load libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)

# Read in raw data
expert_ID = read.csv("2023-07-13_ExpertIdentification.csv")
arthro_sight = read.csv("2023-07-17_ArthropodSighting.csv")

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

# 12 panel figure histogram of lengths ------------------------

correctness_table = left_join(expert_ID, arthro_sight, by = c("ArthropodSightingFK" = "ID", "OriginalGroup")) %>% 
  select(OriginalGroup, StandardGroup, Length) %>% 
  filter(OriginalGroup %in% arthGroupsWeWant) %>% 
  mutate(agreement = OriginalGroup==StandardGroup, binary = as.integer(agreement)) %>% 
  group_by(OriginalGroup, Length)

par(mfrow = c(4,3), mar=c(2.5,5,1,1))

for (arth in correctness_table$OriginalGroup) { plot(1:3, correctness_table[correctness_table$OriginalGroup == arth, c("Length")], type = 'b', main = arth, xaxt = "n", xlab = "", xlim = c(0.5, 3.5), mtext(c("Length"), 1, at = 1:3, line = 0.3, cex = 0.45))}

#for (arth in correctness_table$OriginalGroup) { glm(correctness_table$binary ~ correctness_table$Length)}


