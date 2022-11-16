#import dataframe to compare AccPb-FRET delta mcitrine mean nucleus intensity
d1_bleach <- read.table(".csv", header=T, sep=",")
head(d1_bleach)

#plot delta mean nuclear intensity for mcitrine, separate embryos
boxplot(pre_vs_post ~   sample,
        data=d1_bleach)

# compare delta mean intensity between conditions, accounting for embryos as a random variable
library(lme4)
lm_d1_bleach_int <- lmer(pre_vs_post ~  condition + (1 | sample),
                         data=d1_bleach)
summary(lm_d1_bleach_int)
plot(lm_d1_bleach_int)

# test significance of pairwise comparisons 
library(emmeans)
em_d1_bleach_int <- emmeans(lm_d1_bleach_int, "condition")
stats_d1_bleach_int <- pairs(em_d1_bleach_int)
stats_d1_bleach_int



#import dataframe to compare AccPb-FRET mcitrine mean nucleus intensity pre vs. post for each condition
d1_indiv <- read.table(".csv", header=T, sep=",")
head(d1_indiv)

#  tandem mcherry-mcitrine
#plot mcitrine mean nuclear intensity pre vs. post AccPb, separate embryos
boxplot(mean_cit_intensity_per_nucleus ~   prepost,
        data=d1_indiv[ which(d1_indiv$condition=='mcherry-mcitrine'), ])

# compare mcitrine mean intensity between pre/post AccPb, accounting for embryos as a random variable
library(lme4)
lm_d1_bleach_pos <- lmer(mean_cit_intensity_per_nucleus ~  prepost + (1 | sample),
                         data=d1_indiv[ which(d1_indiv$condition=='mcherry-mcitrine'), ])
summary(lm_d1_bleach_pos)
plot(lm_d1_bleach_pos)

# test significance of pairwise comparisons 
library(emmeans)
em_d1_bleach_pos <- emmeans(lm_d1_bleach_pos, "prepost")
stats_d1_bleach_pos <- pairs(em_d1_bleach_pos)
stats_d1_bleach_pos

#plot mcherry mean nuclear intensity pre vs. post AccPb, separate embryos
boxplot(mean_mch_intensity_per_nucleus ~   prepost,
        data=d1_indiv[ which(d1_indiv$condition=='mcherry-mcitrine'), ])

# compare mcherry mean intensity between pre/post AccPb, accounting for embryos as a random variable
library(lme4)
lm_d1_bleach_pos_mch <- lmer(mean_mch_intensity_per_nucleus ~  prepost + (1 | sample),
                             data=d1_indiv[ which(d1_indiv$condition=='mcherry-mcitrine'), ])
summary(lm_d1_bleach_pos_mch)
plot(lm_d1_bleach_pos_mch)

# test significance of pairwise comparisons 
library(emmeans)
em_d1_bleach_pos_mch <- emmeans(lm_d1_bleach_pos_mch, "prepost")
stats_d1_bleach_pos_mch <- pairs(em_d1_bleach_pos_mch)
stats_d1_bleach_pos_mch



#  co-injected  mcherry+mcitrine
#plot mcitrine mean nuclear intensity pre vs. post AccPb, separate embryos
boxplot(mean_cit_intensity_per_nucleus ~   prepost,
        data=d1_indiv[ which(d1_indiv$condition=='mcherry+mcitrine'), ])

# compare mcitrine mean intensity between pre/post AccPb, accounting for embryos as a random variable
library(lme4)
lm_d1_bleach_neg <- lmer(mean_cit_intensity_per_nucleus ~  prepost + (1 | sample),
                         data=d1_indiv[ which(d1_indiv$condition=='mcherry+mcitrine'), ])
summary(lm_d1_bleach_neg)
plot(lm_d1_bleach_neg)

# test significance of pairwise comparisons 
library(emmeans)
em_d1_bleach_neg <- emmeans(lm_d1_bleach_neg, "prepost")
stats_d1_bleach_neg <- pairs(em_d1_bleach_neg)
stats_d1_bleach_neg


#plot mcherry mean nuclear intensity pre vs. post AccPb, separate embryos
boxplot(mean_mch_intensity_per_nucleus ~   prepost,
        data=d1_indiv[ which(d1_indiv$condition=='mcherry+mcitrine'), ])

# compare mcitrine mean intensity between pre/post AccPb, accounting for embryos as a random variablelibrary(lme4)
lm_d1_bleach_neg_mch <- lmer(mean_mch_intensity_per_nucleus ~  prepost + (1 | sample),
                         data=d1_indiv[ which(d1_indiv$condition=='mcherry+mcitrine'), ])
summary(lm_d1_bleach_neg_mch)
plot(lm_d1_bleach_neg_mch)

# test significance of pairwise comparisons
library(emmeans)
em_d1_bleach_neg_mch <- emmeans(lm_d1_bleach_neg_mch, "prepost")
stats_d1_bleach_neg_mch <- pairs(em_d1_bleach_neg_mch)
stats_d1_bleach_neg_mch

