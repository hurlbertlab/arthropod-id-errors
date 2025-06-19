library(gsheet)
library(dplyr)
library(tidyr)
library(stringr)

# Students were asked to measure the length of 6 arthropod specimens.

# Students were assigned to one of 3 treatments:
#  A - control, students were asked to estimate the length 
#      in millimeters without any tools or prompting of length reference points.
#  B - students were allowed to measure the width of their thumbnail and/or 
#      fingernail in millimeters immediately prior to being asked to estimate length of specimens
#      (i.e. they could use their thumb to aid in estimation)
#  C - students were provided a ruler to aid in estimating arthropod length


# Survey results with length estimations
url = "https://docs.google.com/spreadsheets/d/1j3ZC6ED_yPXXu9ESuRMzKWprlg7U-0xCPn36Fc7wZ_0/edit?resourcekey=&gid=1690406713#gid=1690406713"

results = gsheet2tbl(url)

names(results)[3:9] = c('Group', 'Specimen1_8', 'Specimen2_21', 'Specimen3_6', 
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
results2[4, 4:9] = results[4, 4:9]/10
results2[10, 4:9] = results[10, 4:9]*10


long = pivot_longer(results2, cols = Specimen1_8:Specimen6_19, names_to = "Specimen")
long$trueLength = as.numeric(word(long$Specimen, 2, sep = "_"))
long$deviation = long$value - long$trueLength
long$pctdev = 100*long$deviation/long$trueLength

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1))
boxplot(long$deviation ~ long$Group, xaxt = "n", xlab = "Treatment", ylab = "Deviation (mm)", cex.lab = 1.5, col = c('dodgerblue', 'salmon', 'limegreen'))
abline(h = 0, lty = 'dashed')
mtext(c("Control", "Thumb", "Ruler"), 1, at = 1:3, line = 1, cex = 1.3)
text(2, 32, "Medians:")
text(1:3, rep(27, 3), c("-4", "-2", "-1"))


boxplot(long$pctdev ~ long$Group, xaxt = "n", xlab = "Treatment", ylab = "Deviation (%)", cex.lab = 1.5, col = c('dodgerblue', 'salmon', 'limegreen'))
abline(h = 0, lty = 'dashed')
mtext(c("Control", "Thumb", "Ruler"), 1, at = 1:3, line = 1, cex = 1.3)
text(2, 68, "Medians:")
text(1:3, rep(50, 3), c("-37.5%", "-11.5%", "-3.2%"))


# Reorganized file for conducting Wilcoxon tests between treatment groups

lengthdata = read.csv('data/length_estimation_errors.csv')