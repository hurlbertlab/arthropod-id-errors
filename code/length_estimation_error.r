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
par(mfrow = c(1, 2), mar = c(5, 5, 1, 1))
boxplot(lengthdata[, c(1, 3, 5)], xaxt = "n", xlab = "Treatment", ylab = "Deviation (mm)", cex.lab = 1.5, 
        col = c('dodgerblue', 'salmon', 'limegreen'), las = 1, ylim = c(-22, 42))
abline(h = 0, lty = 'dashed')
mtext(c("Control", "Thumb", "Ruler"), 1, at = 1:3, line = 1, cex = 1.3)
segments(x0= c(1, 1, 1.9), y0 = c(34, 36, 36), x1 = c(1, 1.9, 1.9), y1 = c(36, 36, 34))
text(1.5, 38, "**", cex = 2)
segments(x0= c(1, 1, 3), y0 = c(38, 40, 40), x1 = c(1, 3, 3), y1 = c(40, 40, 38))
text(2, 42, "**", cex = 2)
segments(x0= c(2, 2, 3), y0 = c(18, 20, 20), x1 = c(2, 3, 3), y1 = c(20, 20, 18))
text(2.55, 22, "p = 0.51", cex = 1.2)

boxplot(lengthdata[, c(2, 4, 6)], xaxt = "n", xlab = "Treatment", ylab = "Deviation (%)", cex.lab = 1.5, 
        col = c('dodgerblue', 'salmon', 'limegreen'), las = 1, ylim = c(-80, 137))
abline(h = 0, lty = 'dashed')
mtext(c("Control", "Thumb", "Ruler"), 1, at = 1:3, line = 1, cex = 1.3)
segments(x0= c(1, 1, 2), y0 = c(110, 117, 117), x1 = c(1, 2, 2), y1 = c(117, 117, 110))
text(1.5, 122, "**", cex = 2)
segments(x0= c(1, 1, 3), y0 = c(122, 129, 129), x1 = c(1, 3, 3), y1 = c(129, 129, 122))
text(2, 135, "**", cex = 2)
segments(x0= c(2, 2, 3), y0 = c(50, 57, 57), x1 = c(2, 3, 3), y1 = c(57, 57, 50))
text(2.5, 64, "p = 0.40", cex = 1.2)


