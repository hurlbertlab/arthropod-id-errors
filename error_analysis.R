# Script for ...

# Load libraries
library(dplyr)
library(tidyr)
library(stringr)


# Read in raw data
arthro_sight = read.csv("2023-07-06_ArthropodSighting.csv")


true_counts = arthro_sight %>%
  filter(PhotoURL != "") %>%
  group_by(OriginalGroup, UpdatedGroup) %>%
  summarize(number = n())

# make table that singles out OriginalGroup (includes total ID of OriginalGroup)
# use left_join() to compare total with proportion from true_counts
# divide / error rate ... use mutate()

# part 2: use range()


# true_counts = arthro_sight %>% group_by(OriginalGroup) %>% summarise(trueAnts = ??, trueBeetle = ??, etc)

# ATTEMPTS:

# true_counts = summarise(trueAnts = sum((arthro_sight$UpdatedGroup == "ant"))) 
#cant sum a conditional... 

# arthro_sight$UpdatedGroup %>% true_counts = summarise(trueAnts = count(("ant"))) 
#"no applicable method for 'count' applied to class "character"
# str_count() doesn't work either

# arthro_sight$UpdatedGroup %>% true_counts = summarise(trueAnts = count((if_else(arthro_sight$UpdatedGroup == 'ant', 1, 0))))
# no applicable method for 'count' applied to an object of class 'c('double', 'numeric')

# if_else(arthro_sight$UpdatedGroup == 'ant', 1, 0)
# every entry is TRUE when 'ant', FALSE when anything other than 'ant'. How to make the code seek through each point?

********************************************************
  
  # use 'summarize' to create new variables that are the *counts* of the different values in the UpdatedGroup column
  
  # ie for every record that a CC user said was a 'beetle' we want a "trueBeetles" column which is the number of those records that are actually beetles based on UpdatedGroup, as well as trueAnts, trueTrueBugs, etc. columns.
  
  # dont fully understand the "summarize" function