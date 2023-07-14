# Script for Identifying the types of errors made by citizen scientists in Caterpillars Count!

# Load libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
# TESTINGGGGGGGGGGGG
# Read in raw data
arthro_sight = read.csv("2023-07-13_ExpertIdentification.csv")

# true_counts displays OriginalGroup:UpdatedGroup/StandardGroup:number of ID's with that pair according to those with a photo
 true_counts = arthro_sight %>%
   # filter(PhotoURL != "") %>%
   group_by(OriginalGroup, StandardGroup) %>%
   summarize(number = n())
# true_counts = arthro_sight() %>%
#   group_by(OriginalGroup) %>%
#   summarise(number = n())

# total_counts shows total amount of OriginalGroup IDs 
 total_counts = arthro_sight %>%
   # filter(PhotoURL != "") %>%
   group_by(OriginalGroup) %>%
   summarize(total_ID = n())
  
# use left_join() to compare total with proportion from true_counts
error_num = true_counts %>%
  group_by(OriginalGroup) %>% 
  left_join(total_counts, true_counts, by = c("OriginalGroup" = "OriginalGroup")) %>%
  mutate(rate = round((number / total_ID) * 100, 1)) %>%
  arrange(OriginalGroup, desc(rate)) #to see total desc rate (ie daddylonglegs are the least erroneously-ID'd arthropods) delete 'OriginalGroup' from arrange()


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

image(as.matrix(pivot2[, 2:ncol(pivot2)]), xaxt="n", yaxt="n")
mtext(arthGroupsWeWant, 1, at = (1:12)/12, las = 2, line = 1)
mtext(arthGroupsWeWant, 2, at = (1:12)/12, las = 1, line = 1)


?seq




# Stacked bar graphs

only_error_num = error_num %>%
  filter(OriginalGroup != StandardGroup)



stacked = ggplot(only_error_num, aes(fill=OriginalGroup, y=rate, x=StandardGroup)) + 
  geom_bar(position='stack', 
           stat = 'identity') + 
  labs(x = "Arthropod Group", 
       y = "Error Rate", 
       title = "Error Rate in Arthropod ID from Caterpillars Count!") +
  scale_fill_manual('Position', values=c('coral2', 'steelblue', 'pink', 'green', 'darkblue', 'turquoise', 'orange','green4', 'orange4','yellow', 'yellow3', 'pink4', 'darkturquoise', 'red')) + 
  theme(plot.title = element_text(hjust=0.5, size=10), 
        legend.text = element_text(size = 5), 
        legend.key.size = unit(2, 'mm'), 
        legend.title = element_text("testing"), 
        axis.text.x = element_text(size = 2.5))

grid = only_error_num %>%
  image(matrix('OriginalGroup', 'UpdatedGroup', 'rate'))
# 'z' must be a matrix ???


