#import dataframe to compare aat-FRET mean weighted mfD
d3_mfd <- read.table(".csv", header=T, sep=",")
head(d3_mfd)

#plot mean weighted mfD for mcitrine, separate embryos
boxplot(mean_weighted_mfd_per_nucleus ~   sample,
        data=d3_mfd)

# compare mean weighted mfD between conditions, accounting for embryos as a random variable
library(lme4)
lm_d3_mfd <- lmer(mean_weighted_mfd_per_nucleus ~  condition + (1 | sample),
                   data=d3_mfd)
summary(lm_d3_mfd)
plot(lm_d3_mfd)

# test significance of pairwise comparisons 
library(emmeans)
em_d3_mfd <- emmeans(lm_d3_mfd, "condition")
stats_d3_mfd <- pairs(em_d3_mfd)
stats_d3_mfd
