#import dataframe to compare AccPb-FRET delta mcitrine mean weighted AAT
d1_bleach_mwlt <- read.table(".csv", header=T, sep=",")
head(d1_bleach_mwlt)


#plot delta mean weighted AAT for mcitrine, separate embryos
boxplot(prevpost ~  sample,
        data=d1_bleach_mwlt)

# compare delta mean weighted AAT between conditions, accounting for embryos as a random variable
library(lme4)
lm_d1_bleach_mwlt <- lmer(prevpost ~  condition + (1 | sample),
                   data=d1_bleach_mwlt)
summary(lm_d1_bleach_mwlt)
plot(lm_d1_bleach_mwlt)

# test significance of pairwise comparisons 
library(emmeans)
em_d1_bleach_mwlt <- emmeans(lm_d1_bleach_mwlt, "condition")
stats_d1_bleach_mwlt <- pairs(em_d1_bleach_mwlt)
stats_d1_bleach_mwlt


#import dataframe to compare AccPb-FRET mcitrine mean weighted AAT pre vs. post for each condition
d1_bleach_mwlt_diff <- read.table(".csv", header=T, sep=",")
head(d1_bleach_mwlt_diff)

#  tandem mcherry-mcitrine
#plot mcitrine mean weighted AAT pre vs. post AccPb, separate embryos
boxplot(mean_weighted_lifetime_per_nucleus ~  sample,
        data=d1_bleach_mwlt_diff[ which(d1_bleach_mwlt_diff$condition=='mcherry-mcitrine'), ])

# compare mcitrine mean weighted AAT between pre/post AccPb, accounting for embryos as a random variable
library(lme4)
lm_d1_bleach_mwlt_diff <- lmer(mean_weighted_lifetime_per_nucleus ~  prepost + (1 | sample),
                          data=d1_bleach_mwlt_diff[ which(d1_bleach_mwlt_diff$condition=='mcherry-mcitrine'), ])
summary(lm_d1_bleach_mwlt_diff)
plot(lm_d1_bleach_mwlt_diff)

# test significance of pairwise comparisons 
library(emmeans)
em_d1_bleach_mwlt_diff <- emmeans(lm_d1_bleach_mwlt_diff, "prepost")
stats_d1_bleach_mwlt_diff <- pairs(em_d1_bleach_mwlt_diff)
stats_d1_bleach_mwlt_diff

#  co-injected mcherry+mcitrine
#plot mcitrine mean weighted AAT pre vs. post AccPb, separate embryos
boxplot(mean_weighted_lifetime_per_nucleus ~  sample,
        data=d1_bleach_mwlt_diff[ which(d1_bleach_mwlt_diff$condition=='mcherry+mcitrine'), ])

# compare mcitrine mean weighted AAT between pre/post AccPb, accounting for embryos as a random variable
library(lme4)
lm_d1_bleach_mwlt_diff_neg <- lmer(mean_weighted_lifetime_per_nucleus ~  prepost + (1 | sample),
                               data=d1_bleach_mwlt_diff[ which(d1_bleach_mwlt_diff$condition=='mcherry+mcitrine'), ])
summary(lm_d1_bleach_mwlt_diff_neg)
plot(lm_d1_bleach_mwlt_diff_neg)

# test significance of pairwise comparisons 
library(emmeans)
em_d1_bleach_mwlt_diff_neg <- emmeans(lm_d1_bleach_mwlt_diff_neg, "prepost")
stats_d1_bleach_mwlt_diff_neg <- pairs(em_d1_bleach_mwlt_diff_neg)
stats_d1_bleach_mwlt_diff_neg

