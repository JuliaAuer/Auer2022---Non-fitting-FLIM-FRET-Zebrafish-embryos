#import dataframe to compare AccPb-FRET delta mcitrine mean weighted mfD
d3_bleach_mfd <- read.table("C:/Users/s1611128/Documents/Julia/Nanog-Cit-mCh-Pou5f3 FRET-FLIM/WIM/d3_bleach/d3_bleach_prevspost_mfd.csv", header=T, sep=",")
head(d3_bleach_mfd)

#plot delta mean weighted mfD for mcitrine, separate embryos
boxplot(mfdprepost ~  sample,
        data=d3_bleach_mfd)

# compare delta mean weighted mfD between conditions, accounting for embryos as a random variable
library(lme4)
lm_d3_bleach_mfd <- lmer(mfdprepost ~  condition + (1 | sample),
                          data=d3_bleach_mfd)
summary(lm_d3_bleach_mfd)
plot(lm_d3_bleach_mfd)

# test significance of pairwise comparisons 
library(emmeans)
em_d3_bleach_mfd <- emmeans(lm_d3_bleach_mfd, "condition")
stats_d3_bleach_mfd <- pairs(em_d3_bleach_mfd)
stats_d3_bleach_mfd



#import dataframe to compare AccPb-FRET  mean weighted mfD pre vs. post for each condition
d3_bleach_mfd_diff <- read.table("C:/Users/s1611128/Documents/Julia/Nanog-Cit-mCh-Pou5f3 FRET-FLIM/WIM/d3_bleach/d3_bleach_r.csv", header=T, sep=",")
head(d3_bleach_mfd_diff)

#  tandem mcherry-mcitrine
#plot mean weighted mfD pre vs. post AccPb, separate embryos
boxplot(mean_weighted_mfd_per_nucleus ~  sample,
        data=d3_bleach_mfd_diff[ which(d3_bleach_mfd_diff$condition=='mcherry-mcitrine'), ])

# compare mean weighted mfD between pre/post AccPb, accounting for embryos as a random variable
library(lme4)
lm_d3_bleach_mfd_diff_pos <- lmer(mean_weighted_mfd_per_nucleus ~  pre_post + (1 | sample),
                                  data=d3_bleach_mfd_diff[ which(d3_bleach_mfd_diff$condition=='mcherry-mcitrine'), ])
summary(lm_d3_bleach_mfd_diff_pos)
plot(lm_d3_bleach_mfd_diff_pos)

# test significance of pairwise comparisons 
library(emmeans)
em_d3_bleach_mfd_diff_pos <- emmeans(lm_d3_bleach_mfd_diff_pos, "pre_post")
stats_d3_bleach_mfd_diff_pos <- pairs(em_d3_bleach_mfd_diff_pos)
stats_d3_bleach_mfd_diff_pos

#  co-injected  mcherry+mcitrine
#plot mean weighted mfD pre vs. post AccPb, separate embryos
boxplot(mean_weighted_mfd_per_nucleus ~  sample,
        data=d3_bleach_mfd_diff[ which(d3_bleach_mfd_diff$condition=='mcherry+mcitrine'), ])

# compare mean weighted mfD between pre/post AccPb, accounting for embryos as a random variable
library(lme4)
lm_d3_bleach_mfd_diff_neg <- lmer(mean_weighted_mfd_per_nucleus ~  pre_post + (1 | sample),
                                  data=d3_bleach_mfd_diff[ which(d3_bleach_mfd_diff$condition=='mcherry+mcitrine'), ])
summary(lm_d3_bleach_mfd_diff_neg)
plot(lm_d3_bleach_mfd_diff_neg)

# test significance of pairwise comparisons 
library(emmeans)
em_d3_bleach_mfd_diff_neg <- emmeans(lm_d3_bleach_mfd_diff_neg, "pre_post")
stats_d3_bleach_mfd_diff_neg <- pairs(em_d3_bleach_mfd_diff_neg)
stats_d3_bleach_mfd_diff_neg
