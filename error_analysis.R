# Script for Identifying the types of errors made by citizen scientists in Caterpillars Count!

# Load libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# Read in raw data
arthro_sight = read.csv("2023-07-06_ArthropodSighting.csv")

# true_counts displays OriginalGroup:UpdatedGroup:number of ID's with that pair
true_counts = arthro_sight %>%
  filter(PhotoURL != "") %>%
  group_by(OriginalGroup, UpdatedGroup) %>%
  summarize(number = n())

# total shows total amount of OriginalGroup IDs 
total_counts = arthro_sight %>%
  filter(PhotoURL != "") %>%
  group_by(OriginalGroup) %>%
  summarize(total_ID = n())
  
# use left_join() to compare total with proportion from true_counts
error_num = true_counts %>%
  group_by(OriginalGroup) %>% 
  left_join(total_counts, true_counts, by = c("OriginalGroup" = "OriginalGroup")) %>%
  mutate(rate = (number / total_ID) * 100) %>%
  arrange(OriginalGroup, desc(rate)) #to see total desc rate (ie daddylonglegs are the least erroneously-ID'd arthropods) delete 'OriginalGroup' from arrange()

only_error_num = error_num %>%
  filter(OriginalGroup != UpdatedGroup)

pivoted = only_error_num %>%
  pivot_wider(names_from = UpdatedGroup, values_from = rate)

stacked = ggplot(only_error_num, aes(fill=OriginalGroup, y=rate, x=UpdatedGroup)) + 
  geom_bar(position='stack', 
           stat = 'identity') + 
  labs(x = "Arthropod Group", 
       y = "Error Rate", 
       title = "Error Rate in Arthropod ID from Caterpillars Count!") +
  scale_fill_manual('Position', values=c('coral2', 'steelblue', 'pink', 'green', 'darkblue', 'turquoise', 'orange','green4', 'orange4','yellow', 'yellow3', 'pink4', 'darkturquoise', 'red')) + 
  theme(plot.title = element_text(hjust=0.5, size=10), 
        legend.text = element_text(size = 5), 
        legend.key.size = unit(2, 'mm'), 
        axis.text.x = element_text(size = 5))

grid = only_error_num %>%
  image(matrix('OriginalGroup', 'UpdatedGroup', 'rate'))

# create plots to visualize data -> image(matrix), pivot_wider(), etc


