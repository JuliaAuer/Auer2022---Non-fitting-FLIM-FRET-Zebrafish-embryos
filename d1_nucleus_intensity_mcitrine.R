#import dataframe to compare donor-intensity FRET mean nucleus intensity
d1 <- read.table("C:/Users/s1611128/Documents/Julia/Nanog-Cit-mCh-Pou5f3 FRET-FLIM/WIM/d1_d2_analysis/d1_non-binned_int.csv", header=T, sep=",")
head(d1)

#plot mean nuclear intensity for mcitrine, separate embryos
boxplot(mean_intensity_per_nucleus ~   sample,
        data=d1)

# compare log mean intensity between conditions, accounting for embryos as a random variable
library(lme4)
lm_d1_mean_int_log <- lmer(log(mean_intensity_per_nucleus) ~  condition + (1 | sample),
                       data=d1)
summary(lm_d1_mean_int_log)
plot(lm_d1_mean_int_log)

# test significance of pairwise comparisons 
library(emmeans)
em_d1_mean_int_log <- emmeans(lm_d1_mean_int_log, "condition")
stats_d1_mean_int_log <- pairs(em_d1_mean_int_log)
stats_d1_mean_int_log




