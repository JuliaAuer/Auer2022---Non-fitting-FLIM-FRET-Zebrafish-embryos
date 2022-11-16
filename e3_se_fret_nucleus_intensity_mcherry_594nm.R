"C:/Users/s1611128/Documents/Julia/Nanog-Cit-mCh-Pou5f3 FRET-FLIM/WIM/e3_594_mch")
#import dataframe to compare SE-FRET mcherry mean nucleus intensity (594nm)
e3_594 <- read.table(".csv", header=T, sep=",")
head(e3_594)

#plot mean nuclear intensity for mcherry, separate embryos
boxplot(avg_int ~   sample,
        data=e3_594)

# compare mean intensity between conditions, accounting for embryos as a random variable
library(lme4)
lm_e3_594_log <- lmer(log(avg_int) ~  condition + (1 | sample),
                       data=e3_594)
summary(lm_e3_594_log)
plot(lm_e3_594_log)

# test significance of pairwise comparisons 
library(emmeans)
em_e3_594_log <- emmeans(lm_e3_594_log, "condition")
stats_e3_594_log <- pairs(em_e3_594_log)
stats_e3_594_log
