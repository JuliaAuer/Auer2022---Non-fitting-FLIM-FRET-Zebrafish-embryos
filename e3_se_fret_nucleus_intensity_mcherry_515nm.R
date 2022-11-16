#import dataframe to compare SE-FRET mcherry mean nucleus intensity
e3 <- read.table("C:/Users/s1611128/Documents/Julia/Nanog-Cit-mCh-Pou5f3 FRET-FLIM/WIM/e3/e3_non-binned_ints.csv", header=T, sep=",")
head(e3)

#plot mean nuclear intensity for mcherry, separate embryos
boxplot(mean_mch_intensity_per_nucleus ~   sample,
        data=e3)

# compare mean intensity between conditions, accounting for embryos as a random variable
library(lme4)
lm_e3_mean_int_log <- lmer(log(mean_mch_intensity_per_nucleus) ~  condition + (1 | sample),
                         data=e3)
summary(lm_e3_mean_int_log)
plot(lm_e3_mean_int_log)

# test significance of pairwise comparisons 
library(emmeans)
em_e3_mean_int_log <- emmeans(lm_e3_mean_int_log, "condition")
stats_e3_mean_int_log <- pairs(em_e3_mean_int_log)
stats_e3_mean_int_log
