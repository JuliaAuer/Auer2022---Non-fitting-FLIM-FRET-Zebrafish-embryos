#import dataframe to compare donor photobleaching SE-FRET delta mcherry mean nucleus intensity
e3_bleach <- read.table(".csv", header=T, sep=",")
head(e3_bleach)

#plot delta mean nuclear intensity for mcherry, separate embryos
boxplot(pre_vs_post ~   sample,
        data=e3_bleach)

# compare delta mean intensity between conditions, accounting for embryos as a random variable
library(lme4)
lm_e3_bleach_int <- lmer(pre_vs_post ~  condition + (1 | sample),
                         data=e3_bleach)
summary(lm_e3_bleach_int)
plot(lm_e3_bleach_int)

# test significance of pairwise comparisons 
library(emmeans)
em_e3_bleach_int <- emmeans(lm_e3_bleach_int, "condition")
stats_e3_bleach_int <- pairs(em_e3_bleach_int)
stats_e3_bleach_int




#import dataframe to compare  donor photobleaching SE-FRET mcherry mean nucleus intensity pre vs. post for each condition
e3_indiv <- read.table("C:/Users/s1611128/Documents/Julia/Nanog-Cit-mCh-Pou5f3 FRET-FLIM/WIM/final_wim_scripts/e3_bleach_non-binned_int_r.csv", header=T, sep=",")
head(e3_indiv)

#  tandem mcherry-mcitrine
#plot mcherry mean nuclear intensity pre vs. post AccPb, separate embryos
boxplot(mean_mch_intensity_per_nucleus ~   prepost,
        data=e3_indiv[ which(e3_indiv$condition=='mcherry-mcitrine'), ])

# compare mcherry mean intensity between pre/post AccPb, accounting for embryos as a random variable
library(lme4)
lm_e3_bleach_pos <- lmer(mean_mch_intensity_per_nucleus ~  prepost + (1 | sample),
                         data=e3_indiv[ which(e3_indiv$condition=='mcherry-mcitrine'), ])
summary(lm_e3_bleach_pos)
plot(lm_e3_bleach_pos)

# test significance of pairwise comparisons 
library(emmeans)
em_e3_bleach_pos <- emmeans(lm_e3_bleach_pos, "prepost")
stats_e3_bleach_pos <- pairs(em_e3_bleach_pos)
stats_e3_bleach_pos

#plot mcitrine mean nuclear intensity pre vs. post AccPb, separate embryos
boxplot(mean_cit_intensity_per_nucleus ~   prepost,
        data=e3_indiv[ which(e3_indiv$condition=='mcherry-mcitrine'), ])

# compare mcitrine mean intensity between pre/post AccPb, accounting for embryos as a random variable
library(lme4)
lm_e3_bleach_pos_cit <- lmer(mean_cit_intensity_per_nucleus ~  prepost + (1 | sample),
                             data=e3_indiv[ which(e3_indiv$condition=='mcherry-mcitrine'), ])
summary(lm_e3_bleach_pos_cit)
plot(lm_e3_bleach_pos_cit)

# test significance of pairwise comparisons
library(emmeans)
em_e3_bleach_pos_cit <- emmeans(lm_e3_bleach_pos_cit, "prepost")
stats_e3_bleach_pos_cit <- pairs(em_e3_bleach_pos_cit)
stats_e3_bleach_pos_cit



#  co-injected mcherry+mcitrine
#plot mcherry mean nuclear intensity pre vs. post AccPb, separate embryos
boxplot(mean_mch_intensity_per_nucleus ~   prepost,
        data=e3_indiv[ which(e3_indiv$condition=='mcherry+mcitrine'), ])

# compare mcherry mean intensity between pre/post AccPb, accounting for embryos as a random variable
library(lme4)
lm_e3_bleach_neg <- lmer(mean_mch_intensity_per_nucleus ~  prepost + (1 | sample),
                         data=e3_indiv[ which(e3_indiv$condition=='mcherry+mcitrine'), ])
summary(lm_e3_bleach_neg)
plot(lm_e3_bleach_neg)

# test significance of pairwise comparisons 
library(emmeans)
em_e3_bleach_neg <- emmeans(lm_e3_bleach_neg, "prepost")
stats_e3_bleach_neg <- pairs(em_e3_bleach_neg)
stats_e3_bleach_neg

#plot mcitrine mean nuclear intensity pre vs. post AccPb, separate embryos
boxplot(mean_cit_intensity_per_nucleus ~   prepost,
        data=e3_indiv[ which(e3_indiv$condition=='mcherry+mcitrine'), ])

# compare mcitrine mean intensity between pre/post AccPb, accounting for embryos as a random variable
library(lme4)
lm_e3_bleach_neg_cit <- lmer(mean_cit_intensity_per_nucleus ~  prepost + (1 | sample),
                         data=e3_indiv[ which(e3_indiv$condition=='mcherry+mcitrine'), ])
summary(lm_e3_bleach_neg_cit)
plot(lm_e3_bleach_neg_cit)

# test significance of pairwise comparisons
library(emmeans)
em_e3_bleach_neg_cit <- emmeans(lm_e3_bleach_neg_cit, "prepost")
stats_e3_bleach_neg_cit <- pairs(em_e3_bleach_neg_cit)
stats_e3_bleach_neg_cit

