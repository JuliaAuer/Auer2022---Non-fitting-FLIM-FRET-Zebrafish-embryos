#import dataframe to compare aat-FRET mean weighted AAT
d1_mwlt <- read.table(".csv", header=T, sep=",")
head(d1_mwlt)

#plot mean weighted AAT for mcitrine, separate embryos
boxplot(mean_weighted_lifetime_per_nucleus ~   sample,
        data=d1_mwlt)

# compare mean weighted AAT between conditions, accounting for embryos as a random variable
library(lme4)
lm_d1_mwlt <- lmer(mean_weighted_lifetime_per_nucleus ~  condition + (1 | sample),
                       data=d1_mwlt)
summary(lm_d1_mwlt)
plot(lm_d1_mwlt)

# test significance of pairwise comparisons 
library(emmeans)
em_d1_mwlt <- emmeans(lm_d1_mwlt, "condition")
stats_d1_mwlt <- pairs(em_d1_mwlt)
stats_d1_mwlt
