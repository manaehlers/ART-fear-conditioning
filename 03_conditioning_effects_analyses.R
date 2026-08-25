# ART Project - analyses for experimental manipulation
#
# Last updated: 25/08/2026 - Mana Ehlers

rm(list = ls())

### Load packages
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
library(rstatix)

options(scipen = 999)

#############################################################
##################### Main effects of task ##################
#############################################################

#############################################################
###################### Expectancy Ratings ###################
#############################################################

load("ratings.RData")
ratings <- ratings[which(ratings$id != '145' & ratings$id != '220'),]


##################### acquisition
dataSubset <- ratings[which(ratings$phase == "acq"),]
agg.rate <- with(dataSubset, aggregate(exp_rating, by=list(id=id, stim=stim), FUN=mean, na.rm=T))

group_by(agg.rate, stim) %>%
  summarise(
    count = n(),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )

#visualize data
ggboxplot(agg.rate, x = "stim", y = "x", 
          color = "stim", palette = c("red2","blue1"),
          order = c("CSm", "CSp"),
          ylab = "Expectancy Rating", xlab = "Stimulus")

# Subset weight data before treatment
CSm <- subset(agg.rate,  stim == "CSm", x,
              drop = TRUE)
# subset weight data after treatment
CSp <- subset(agg.rate,  stim == "CSp", x,
              drop = TRUE)

acq_rate <- t.test(CSp, CSm, paired = TRUE)
differences <- CSp - CSm

# Standard deviation of the differences
sd_diff <- sd(differences, na.rm=T)

# Mean difference
mean_diff <- mean(differences, na.rm=T)

# Cohen's d (for paired samples)
cohen_d <- mean_diff / sd_diff

# Output the t-test result and the effect size
acq_rate
cohen_d


################# extinction
load("ratings.RData")
ratings <- ratings[which(ratings$id != '145' & ratings$id != '220'),]


dataSubset <- ratings[which(ratings$phase == "ext"),]
#dataSubset <- ratings[which(ratings$phase == "ext" & ratings$trial > 18),]
agg.rate <- with(dataSubset, aggregate(exp_rating, by=list(id=id, stim=stim), FUN=mean, na.rm=T))

group_by(agg.rate, stim) %>%
  summarise(
    count = n(),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )

#visualize data
ggboxplot(agg.rate, x = "stim", y = "x", 
          color = "stim", palette = c("red2","blue1"),
          order = c("CSm", "CSp"),
          ylab = "Expectancy Rating", xlab = "Stimulus")

# Subset weight data before treatment
CSm <- subset(agg.rate,  stim == "CSm", x,
              drop = TRUE)
# subset weight data after treatment
CSp <- subset(agg.rate,  stim == "CSp", x,
              drop = TRUE)

ext_rate <- t.test(CSp, CSm, paired = TRUE)
differences <- CSp - CSm

# Standard deviation of the differences
sd_diff <- sd(differences, na.rm=T)

# Mean difference
mean_diff <- mean(differences, na.rm=T)

# Cohen's d (for paired samples)
cohen_d <- mean_diff / sd_diff

# Output the t-test result and the effect size
ext_rate
cohen_d

############## extinction ANOVA

load("ratings.RData")
ratings <- ratings[which(ratings$id != '145' & ratings$id != '220'),]


dataSubset <- ratings[which(ratings$phase == "ext"),]
agg.rate <- with(dataSubset, aggregate(exp_rating, by=list(id=id, stim=stim, half=half), FUN=mean, na.rm=T))


res.aov <- anova_test(
  data = agg.rate, dv = x, wid = id,
  within = c(stim, half)
)
get_anova_table(res.aov)


# Pairwise comparisons between stims
pwc1 <- agg.rate %>%
  group_by(half) %>%
  pairwise_t_test(
    x ~ stim, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc1

# Pairwise comparisons between time points
pwc2 <- agg.rate %>%
  group_by(stim) %>%
  pairwise_t_test(
    x ~ half, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2

################# renewal 
load("ratings.RData")
ratings <- ratings[which(ratings$id != '145' & ratings$id != '220'),]


dataSubset <- ratings[which(ratings$phase == "ren"),]
#dataSubset <- ratings[which(ratings$phase == "ren" & ratings$trial > 18),]
agg.rate <- with(dataSubset, aggregate(exp_rating, by=list(id=id, stim=stim), FUN=mean, na.rm=T))

group_by(agg.rate, stim) %>%
  summarise(
    count = n(),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )

#visualize data
ggboxplot(agg.rate, x = "stim", y = "x", 
          color = "stim", palette = c("red2","blue1"),
          order = c("CSm", "CSp"),
          ylab = "Expectancy Rating", xlab = "Stimulus")

# Subset weight data before treatment
CSm <- subset(agg.rate,  stim == "CSm", x,
              drop = TRUE)
# subset weight data after treatment
CSp <- subset(agg.rate,  stim == "CSp", x,
              drop = TRUE)

ren_rate <- t.test(CSp, CSm, paired = TRUE)
differences <- CSp - CSm

# Standard deviation of the differences
sd_diff <- sd(differences, na.rm=T)

# Mean difference
mean_diff <- mean(differences, na.rm=T)

# Cohen's d (for paired samples)
cohen_d <- mean_diff / sd_diff

# Output the t-test result and the effect size
ren_rate
cohen_d

############## renewal ANOVA

load("ratings.RData")
ratings <- ratings[which(ratings$id != '145' & ratings$id != '220'),]


dataSubset <- ratings[which(ratings$phase == "ren"),]
agg.rate <- with(dataSubset, aggregate(exp_rating, by=list(id=id, stim=stim, half=half), FUN=mean, na.rm=T))


res.aov <- anova_test(
  data = agg.rate, dv = x, wid = id,
  within = c(stim, half)
)
get_anova_table(res.aov)


# Pairwise comparisons between stims
pwc1 <- agg.rate %>%
  group_by(half) %>%
  pairwise_t_test(
    x ~ stim, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc1

# Pairwise comparisons between time points
pwc2 <- agg.rate %>%
  group_by(stim) %>%
  pairwise_t_test(
    x ~ half, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2


#############################################################
################## Fear Ratings - post phase ################
#############################################################

############# Acquisition

load("ratings.RData")
ratings <- ratings[which(ratings$id != '145' & ratings$id != '220'),]

dataSubset <- ratings[which(ratings$phase == "acq"),]
agg.rate <- with(dataSubset, aggregate(post_rating, by=list(id=id, stim=stim), FUN=mean, na.rm=T))

group_by(agg.rate, stim) %>%
  summarise(
    count = n(),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )

#visualize data
ggboxplot(agg.rate, x = "stim", y = "x", 
          color = "stim", palette = c("red2","blue1"),
          order = c("CSm", "CSp"),
          ylab = "Post Fear Rating", xlab = "Stimulus")

# Subset weight data before treatment
CSm <- subset(agg.rate,  stim == "CSm", x,
              drop = TRUE)
# subset weight data after treatment
CSp <- subset(agg.rate,  stim == "CSp", x,
              drop = TRUE)

acq_rate <- t.test(CSp, CSm, paired = TRUE)
differences <- CSp - CSm

# Standard deviation of the differences
sd_diff <- sd(differences, na.rm=T)

# Mean difference
mean_diff <- mean(differences, na.rm=T)

# Cohen's d (for paired samples)
cohen_d <- mean_diff / sd_diff

# Output the t-test result and the effect size
acq_rate
cohen_d


########## Extinction

load("ratings.RData")
ratings <- ratings[which(ratings$id != '145' & ratings$id != '220'),]

dataSubset <- ratings[which(ratings$phase == "ext"),]
agg.rate <- with(dataSubset, aggregate(post_rating, by=list(id=id, stim=stim), FUN=mean, na.rm=T))

group_by(agg.rate, stim) %>%
  summarise(
    count = n(),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )

#visualize data
ggboxplot(agg.rate, x = "stim", y = "x", 
          color = "stim", palette = c("red2","blue1"),
          order = c("CSm", "CSp"),
          ylab = "Post Fear Rating", xlab = "Stimulus")

# Subset weight data before treatment
CSm <- subset(agg.rate,  stim == "CSm", x,
              drop = TRUE)
# subset weight data after treatment
CSp <- subset(agg.rate,  stim == "CSp", x,
              drop = TRUE)

ext_rate <- t.test(CSp, CSm, paired = TRUE)
differences <- CSp - CSm

# Standard deviation of the differences
sd_diff <- sd(differences, na.rm=T)

# Mean difference
mean_diff <- mean(differences, na.rm=T)

# Cohen's d (for paired samples)
cohen_d <- mean_diff / sd_diff

# Output the t-test result and the effect size
ext_rate
cohen_d


########## extinction ANOVA
load("ratings.RData")
ratings <- ratings[which(ratings$id != '145' & ratings$id != '220'),]


#extinction
ratings_long <- gather(ratings, timepoint, fear_rating, pre_rating:post_rating, factor_key = TRUE) 

dataSubset <- ratings_long[which(ratings_long$phase == "ext"),]
agg.rate.ext <- with(dataSubset, aggregate(fear_rating, by=list(id=id, stim=stim, timepoint=timepoint), FUN=mean, na.rm=T))

#anova with factors stim and timepoint
agg.rate.ext %>%
  group_by(stim, timepoint) %>%
  get_summary_stats(x, type = "mean_sd")

bxp <- ggboxplot(
  agg.rate.ext, x = "timepoint", y = "x",
  color = "stim", palette = "jco"
)
bxp

res.aov <- anova_test(
  data = agg.rate.ext, dv = x, wid = id,
  within = c(stim, timepoint)
)
get_anova_table(res.aov)

# Pairwise comparisons between stims
pwc1 <- agg.rate.ext %>%
  group_by(timepoint) %>%
  pairwise_t_test(
    x ~ stim, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc1

# Pairwise comparisons between time points
pwc2 <- agg.rate.ext %>%
  group_by(stim) %>%
  pairwise_t_test(
    x ~ timepoint, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2

########## Renewal

load("ratings.RData")
ratings <- ratings[which(ratings$id != '145' & ratings$id != '220'),]

dataSubset <- ratings[which(ratings$phase == "ren"),]
agg.rate <- with(dataSubset, aggregate(post_rating, by=list(id=id, stim=stim), FUN=mean, na.rm=T))

group_by(agg.rate, stim) %>%
  summarise(
    count = n(),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )

#visualize data
ggboxplot(agg.rate, x = "stim", y = "x", 
          color = "stim", palette = c("red2","blue1"),
          order = c("CSm", "CSp"),
          ylab = "Post Fear Rating", xlab = "Stimulus")

# Subset weight data before treatment
CSm <- subset(agg.rate,  stim == "CSm", x,
              drop = TRUE)
# subset weight data after treatment
CSp <- subset(agg.rate,  stim == "CSp", x,
              drop = TRUE)

ren_rate <- t.test(CSp, CSm, paired = TRUE)
differences <- CSp - CSm

# Standard deviation of the differences
sd_diff <- sd(differences, na.rm=T)

# Mean difference
mean_diff <- mean(differences, na.rm=T)

# Cohen's d (for paired samples)
cohen_d <- mean_diff / sd_diff

# Output the t-test result and the effect size
ren_rate
cohen_d

########## renewal ANOVA
load("ratings.RData")
ratings <- ratings[which(ratings$id != '145' & ratings$id != '220'),]


#extinction
ratings_long <- gather(ratings, timepoint, fear_rating, pre_rating:post_rating, factor_key = TRUE) 

dataSubset <- ratings_long[which(ratings_long$phase != "acq" & ratings_long$timepoint == "post_rating"),]

#rename ext post_rating to pre_rating
dataSubset <- dataSubset %>%
  mutate(timepoint = ifelse(phase == "ext" & timepoint == "post_rating", 
                            "pre_rating", 
                            "post_rating"))

agg.rate.ren <- with(dataSubset, aggregate(fear_rating, by=list(id=id, stim=stim, timepoint=timepoint), FUN=mean, na.rm=T))

#anova with factors stim and timepoint
agg.rate.ren %>%
  group_by(stim, timepoint) %>%
  get_summary_stats(x, type = "mean_sd")

bxp <- ggboxplot(
  agg.rate.ren, x = "timepoint", y = "x",
  color = "stim", palette = "jco"
)
bxp

res.aov <- anova_test(
  data = agg.rate.ren, dv = x, wid = id,
  within = c(stim, timepoint)
)
get_anova_table(res.aov)

# Pairwise comparisons between stims
pwc1 <- agg.rate.ren %>%
  group_by(timepoint) %>%
  pairwise_t_test(
    x ~ stim, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc1

# Pairwise comparisons between time points
pwc2 <- agg.rate.ren %>%
  group_by(stim) %>%
  pairwise_t_test(
    x ~ timepoint, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2



#############################################################
############################# SCR ###########################
#############################################################

############## Acquisition

### Load data
load("scr.RData")

#exclude non-responders aka participants that respond to less than 2/3 of all US, that means 4 or 5 missing
scr <- scr[which(scr$id != '006' & scr$id != '014' & scr$id != '018' & scr$id != '021' & scr$id != '032' & scr$id != '040' & scr$id != '048' & scr$id != '060' & scr$id != '062' & scr$id != '063' & scr$id != '067' & scr$id != '072' & scr$id != '084' & scr$id != '087' & scr$id != '094' & scr$id != '096' & scr$id != '099' & scr$id != '101' & scr$id != '102' & scr$id != '103' & scr$id != '107' & scr$id != '110' & scr$id != '142' & scr$id != '144' & scr$id != '145' & scr$id!= '147' & scr$id != '150' & scr$id != '155' & scr$id != '159' & scr$id != '172' & scr$id != '173' & scr$id != '190' & scr$id != '194' & scr$id != '204' & scr$id != '206' & scr$id != '207' & scr$id != '208' & scr$id != '213' & scr$id != '220' & scr$id != '224' & scr$id != '227' & scr$id != '231' & scr$id != '235' & scr$id != '238' & scr$id != '239'&  scr$id != '240' & scr$id != '251' & scr$id != '255' & scr$id != '257' & scr$id != '259' & scr$id != '261' & scr$id != '264' & scr$id != '266 '& scr$id != '268'),]

#acquisition
dataSubset <- scr[which(scr$phase == "acq" & scr$stim != "US"),]
agg.scr <- with(dataSubset, aggregate(log.rc, by=list(id=id, stim=stim), FUN=mean, na.rm=T))

group_by(agg.scr, stim) %>%
  summarise(
    count = n(),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )

#visualize data
ggboxplot(agg.scr, x = "stim", y = "x", 
          color = "stim", palette = c("red2","blue1"),
          order = c("CSm", "CSp"),
          ylab = "SCR [log, rc]", xlab = "Stimulus")

# Subset weight data before treatment
CSm <- subset(agg.scr,  stim == "CSm", x,
              drop = TRUE)
# subset weight data after treatment
CSp <- subset(agg.scr,  stim == "CSp", x,
              drop = TRUE)

acq_scr <- t.test(CSp, CSm, paired = TRUE)

differences <- CSp - CSm

# Standard deviation of the differences
sd_diff <- sd(differences, na.rm=T)

# Mean difference
mean_diff <- mean(differences, na.rm=T)

# Cohen's d (for paired samples)
cohen_d <- mean_diff / sd_diff

acq_scr
cohen_d

############ Extinction

### Load data
load("scr.RData")

#exclude non-responders aka participants that respond to less than 2/3 of all US, that means 4 or 5 missing
scr <- scr[which(scr$id != '006' & scr$id != '014' & scr$id != '018' & scr$id != '021' & scr$id != '032' & scr$id != '040' & scr$id != '048' & scr$id != '060' & scr$id != '062' & scr$id != '063' & scr$id != '067' & scr$id != '072' & scr$id != '084' & scr$id != '087' & scr$id != '094' & scr$id != '096' & scr$id != '099' & scr$id != '101' & scr$id != '102' & scr$id != '103' & scr$id != '107' & scr$id != '110' & scr$id != '142' & scr$id != '144' & scr$id != '145' & scr$id!= '147' & scr$id != '150' & scr$id != '155' & scr$id != '159' & scr$id != '172' & scr$id != '173' & scr$id != '190' & scr$id != '194' & scr$id != '204' & scr$id != '206' & scr$id != '207' & scr$id != '208' & scr$id != '213' & scr$id != '220' & scr$id != '224' & scr$id != '227' & scr$id != '231' & scr$id != '235' & scr$id != '238' & scr$id != '239'&  scr$id != '240' & scr$id != '251' & scr$id != '255' & scr$id != '257' & scr$id != '259' & scr$id != '261' & scr$id != '264' & scr$id != '266 '& scr$id != '268'),]

#dataSubset <- scr[which(scr$phase == "ext"),]
dataSubset <- scr[which(scr$phase == "ext" & scr$trial > 18),]
#dataSubset <- scr[which(scr$phase == "ext" & scr$half == 2),]

agg.scr <- with(dataSubset, aggregate(log.rc, by=list(id=id, stim=stim), FUN=mean, na.rm=T))

group_by(agg.scr, stim) %>%
  summarise(
    count = n(),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )

#visualize data
ggboxplot(agg.scr, x = "stim", y = "x", 
          color = "stim", palette = c("red2","blue1"),
          order = c("CSm", "CSp"),
          ylab = "SCR [log, rc]", xlab = "Stimulus")

# Subset weight data before treatment
CSm <- subset(agg.scr,  stim == "CSm", x,
              drop = TRUE)
# subset weight data after treatment
CSp <- subset(agg.scr,  stim == "CSp", x,
              drop = TRUE)

ext_scr <- t.test(CSp, CSm, paired = TRUE)

differences <- CSp - CSm

# Standard deviation of the differences
sd_diff <- sd(differences, na.rm=T)

# Mean difference
mean_diff <- mean(differences, na.rm=T)

# Cohen's d (for paired samples)
cohen_d <- mean_diff / sd_diff

ext_scr
cohen_d

############ extinction ANOVA
### Load data
load("scr.RData")

#exclude non-responders aka participants that respond to less than 2/3 of all US, that means 4 or 5 missing
scr <- scr[which(scr$id != '006' & scr$id != '014' & scr$id != '018' & scr$id != '021' & scr$id != '032' & scr$id != '040' & scr$id != '048' & scr$id != '060' & scr$id != '062' & scr$id != '063' & scr$id != '067' & scr$id != '072' & scr$id != '084' & scr$id != '087' & scr$id != '094' & scr$id != '096' & scr$id != '099' & scr$id != '101' & scr$id != '102' & scr$id != '103' & scr$id != '107' & scr$id != '110' & scr$id != '142' & scr$id != '144' & scr$id != '145' & scr$id!= '147' & scr$id != '150' & scr$id != '155' & scr$id != '159' & scr$id != '172' & scr$id != '173' & scr$id != '190' & scr$id != '194' & scr$id != '204' & scr$id != '206' & scr$id != '207' & scr$id != '208' & scr$id != '213' & scr$id != '220' & scr$id != '224' & scr$id != '227' & scr$id != '231' & scr$id != '235' & scr$id != '238' & scr$id != '239'&  scr$id != '240' & scr$id != '251' & scr$id != '255' & scr$id != '257' & scr$id != '259' & scr$id != '261' & scr$id != '264' & scr$id != '266 '& scr$id != '268'),]

dataSubset <- scr[which(scr$phase == "ext"),]
agg.scr <- with(dataSubset, aggregate(log.rc, by=list(id=id, stim=stim, half=half), FUN=mean, na.rm=T))


res.aov <- anova_test(
  data = agg.scr, dv = x, wid = id,
  within = c(stim, half)
)
get_anova_table(res.aov)


# Pairwise comparisons between stim
pwc1 <- agg.scr %>%
  group_by(half) %>%
  pairwise_t_test(
    x ~ stim, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc1

# Pairwise comparisons between time points
pwc2 <- agg.scr %>%
  group_by(stim) %>%
  pairwise_t_test(
    x ~ half, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2


############ Renewal

### Load data
load("scr.RData")

#exclude non-responders aka participants that respond to less than 2/3 of all US, that means 4 or 5 missing
scr <- scr[which(scr$id != '006' & scr$id != '014' & scr$id != '018' & scr$id != '021' & scr$id != '032' & scr$id != '040' & scr$id != '048' & scr$id != '060' & scr$id != '062' & scr$id != '063' & scr$id != '067' & scr$id != '072' & scr$id != '084' & scr$id != '087' & scr$id != '094' & scr$id != '096' & scr$id != '099' & scr$id != '101' & scr$id != '102' & scr$id != '103' & scr$id != '107' & scr$id != '110' & scr$id != '142' & scr$id != '144' & scr$id != '145' & scr$id!= '147' & scr$id != '150' & scr$id != '155' & scr$id != '159' & scr$id != '172' & scr$id != '173' & scr$id != '190' & scr$id != '194' & scr$id != '204' & scr$id != '206' & scr$id != '207' & scr$id != '208' & scr$id != '213' & scr$id != '220' & scr$id != '224' & scr$id != '227' & scr$id != '231' & scr$id != '235' & scr$id != '238' & scr$id != '239'&  scr$id != '240' & scr$id != '251' & scr$id != '255' & scr$id != '257' & scr$id != '259' & scr$id != '261' & scr$id != '264' & scr$id != '266 '& scr$id != '268'),]
dataSubset <- scr[which(scr$phase == "ren"),]

agg.scr <- with(dataSubset, aggregate(log.rc, by=list(id=id, stim=stim), FUN=mean, na.rm=T))

group_by(agg.scr, stim) %>%
  summarise(
    count = n(),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )

#visualize data
ggboxplot(agg.scr, x = "stim", y = "x", 
          color = "stim", palette = c("red2","blue1"),
          order = c("CSm", "CSp"),
          ylab = "SCR [log, rc]", xlab = "Stimulus")

# Subset weight data before treatment
CSm <- subset(agg.scr,  stim == "CSm", x,
              drop = TRUE)
# subset weight data after treatment
CSp <- subset(agg.scr,  stim == "CSp", x,
              drop = TRUE)

ren_scr <- t.test(CSp, CSm, paired = TRUE)

differences <- CSp - CSm

# Standard deviation of the differences
sd_diff <- sd(differences, na.rm=T)

# Mean difference
mean_diff <- mean(differences, na.rm=T)

# Cohen's d (for paired samples)
cohen_d <- mean_diff / sd_diff

ren_scr
cohen_d

############ renewal ANOVA
### Load data
load("scr.RData")

#exclude non-responders aka participants that respond to less than 2/3 of all US, that means 4 or 5 missing
scr <- scr[which(scr$id != '006' & scr$id != '014' & scr$id != '018' & scr$id != '021' & scr$id != '032' & scr$id != '040' & scr$id != '048' & scr$id != '060' & scr$id != '062' & scr$id != '063' & scr$id != '067' & scr$id != '072' & scr$id != '084' & scr$id != '087' & scr$id != '094' & scr$id != '096' & scr$id != '099' & scr$id != '101' & scr$id != '102' & scr$id != '103' & scr$id != '107' & scr$id != '110' & scr$id != '142' & scr$id != '144' & scr$id != '145' & scr$id!= '147' & scr$id != '150' & scr$id != '155' & scr$id != '159' & scr$id != '172' & scr$id != '173' & scr$id != '190' & scr$id != '194' & scr$id != '204' & scr$id != '206' & scr$id != '207' & scr$id != '208' & scr$id != '213' & scr$id != '220' & scr$id != '224' & scr$id != '227' & scr$id != '231' & scr$id != '235' & scr$id != '238' & scr$id != '239'&  scr$id != '240' & scr$id != '251' & scr$id != '255' & scr$id != '257' & scr$id != '259' & scr$id != '261' & scr$id != '264' & scr$id != '266 '& scr$id != '268'),]

dataSubset <- scr[which(scr$phase == "ren"),]
agg.scr <- with(dataSubset, aggregate(log.rc, by=list(id=id, stim=stim, half=half), FUN=mean, na.rm=T))


res.aov <- anova_test(
  data = agg.scr, dv = x, wid = id,
  within = c(stim, half)
)
get_anova_table(res.aov)

# Pairwise comparisons between stims
pwc1 <- agg.scr %>%
  group_by(half) %>%
  pairwise_t_test(
    x ~ stim, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc1

# Pairwise comparisons between time points
pwc2 <- agg.scr %>%
  group_by(stim) %>%
  pairwise_t_test(
    x ~ half, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2

#############################################################
########################### FPS #############################
#############################################################

############## Acquisition

load("fps.RData")
fps <- fps[ fps$id != "024" & fps$id != "042" & fps$id != "043" & fps$id != "069" & fps$id != "081" 
            & fps$id != "085" & fps$id != "110" & fps$id != "124" & fps$id != "128" & fps$id != "134" 
            & fps$id != "136" & fps$id != "141" & fps$id != "144" & fps$id != "145 "& fps$id != "152" & fps$id != "156" 
            & fps$id != "159" & fps$id != "161" & fps$id != "162" & fps$id != "163" & fps$id != "173" 
            & fps$id != "175" & fps$id != "176" & fps$id != "178" & fps$id != "181" & fps$id != "183" 
            & fps$id != "184" & fps$id != "186" & fps$id != "220" & fps$id != "242" & fps$id != "246" & fps$id != "249" 
            & fps$id != "254" & fps$id != "255" & fps$id != "257" & fps$id != "268" & fps$id != "270", ]


dataSubset <- fps[which(fps$phase == "acq" & fps$stim != "iti"),]
agg.fps <- with(dataSubset, aggregate(Tscore, by=list(id=id, stim=stim), FUN=mean, na.rm=T))

group_by(agg.fps, stim) %>%
  summarise(
    count = n(),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )

#visualize data
ggboxplot(agg.fps, x = "stim", y = "x", 
          color = "stim", palette = c("red2","blue1"),
          order = c("CSm", "CSp"),
          ylab = "FPS", xlab = "Stimulus")

# Subset weight data before treatment
CSm <- subset(agg.fps,  stim == "CSm", x,
              drop = TRUE)
# subset weight data after treatment
CSp <- subset(agg.fps,  stim == "CSp", x,
              drop = TRUE)

acq_fps <- t.test(CSp, CSm, paired = TRUE)

differences <- CSp - CSm

# Standard deviation of the differences
sd_diff <- sd(differences, na.rm=T)

# Mean difference
mean_diff <- mean(differences, na.rm=T)

# Cohen's d (for paired samples)
cohen_d <- mean_diff / sd_diff

acq_fps
cohen_d

############ Extinction

load("fps.RData")

fps <- fps[ fps$id != "024" & fps$id != "042" & fps$id != "043" & fps$id != "069" & fps$id != "081" 
            & fps$id != "085" & fps$id != "110" & fps$id != "124" & fps$id != "128" & fps$id != "134" 
            & fps$id != "136" & fps$id != "141" & fps$id != "144" & fps$id != "145 "& fps$id != "152" & fps$id != "156" 
            & fps$id != "159" & fps$id != "161" & fps$id != "162" & fps$id != "163" & fps$id != "173" 
            & fps$id != "175" & fps$id != "176" & fps$id != "178" & fps$id != "181" & fps$id != "183" 
            & fps$id != "184" & fps$id != "186" & fps$id != "220" & fps$id != "242" & fps$id != "246" & fps$id != "249" 
            & fps$id != "254" & fps$id != "255" & fps$id != "257" & fps$id != "268" & fps$id != "270", ]

dataSubset <- fps[which(fps$phase == "ext"),]

agg.fps <- with(dataSubset, aggregate(Tscore, by=list(id=id, stim=stim), FUN=mean, na.rm=T))

group_by(agg.fps, stim) %>%
  summarise(
    count = n(),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )

#visualize data
ggboxplot(agg.fps, x = "stim", y = "x", 
          color = "stim", palette = c("red2","blue1"),
          order = c("CSm", "CSp"),
          ylab = "FPS", xlab = "Stimulus")

# Subset weight data before treatment
CSm <- subset(agg.fps,  stim == "CSm", x,
              drop = TRUE)
# subset weight data after treatment
CSp <- subset(agg.fps,  stim == "CSp", x,
              drop = TRUE)

ext_fps <- t.test(CSp, CSm, paired = TRUE)

differences <- CSp - CSm

# Standard deviation of the differences
sd_diff <- sd(differences, na.rm=T)

# Mean difference
mean_diff <- mean(differences, na.rm=T)

# Cohen's d (for paired samples)
cohen_d <- mean_diff / sd_diff

ext_fps
cohen_d

############ extinction last 5 trials
load("fps.RData")
fps <- fps[ fps$id != "024" & fps$id != "042" & fps$id != "043" & fps$id != "069" & fps$id != "081" 
            & fps$id != "085" & fps$id != "110" & fps$id != "124" & fps$id != "128" & fps$id != "134" 
            & fps$id != "136" & fps$id != "141" & fps$id != "144" & fps$id != "145 "& fps$id != "152" & fps$id != "156" 
            & fps$id != "159" & fps$id != "161" & fps$id != "162" & fps$id != "163" & fps$id != "173" 
            & fps$id != "175" & fps$id != "176" & fps$id != "178" & fps$id != "181" & fps$id != "183" 
            & fps$id != "184" & fps$id != "186" & fps$id != "220" & fps$id != "242" & fps$id != "246" & fps$id != "249" 
            & fps$id != "254" & fps$id != "255" & fps$id != "257" & fps$id != "268" & fps$id != "270", ]

dataSubset <- fps[which(fps$phase == "ext"),]

last5 <- dataSubset %>%
  group_by(id) %>%
  arrange(trial, .by_group = TRUE) %>%     # ensure correct order per person
  filter(!is.na(Tscore)) %>%               # keep only non-NA rows
  slice_tail(n = 5) %>%                    # take last 5 rows
  ungroup()

last5<-data.frame(last5)

agg.fps <- with(last5, aggregate(Tscore, by=list(id=id, stim=stim), FUN=mean, na.rm=T))

#exclude people who don't have csp and csm
agg.fps <- agg.fps %>%
  group_by(id) %>%
  filter(all(c("CSp", "CSm") %in% stim))

group_by(agg.fps, stim) %>%
  summarise(
    count = n(),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )

#visualize data
ggboxplot(agg.fps, x = "stim", y = "x", 
          color = "stim", palette = c("red2","blue1"),
          order = c("CSm", "CSp"),
          ylab = "FPS", xlab = "Stimulus")

# Subset weight data before treatment
CSm <- subset(agg.fps,  stim == "CSm", x,
              drop = TRUE)
# subset weight data after treatment
CSp <- subset(agg.fps,  stim == "CSp", x,
              drop = TRUE)


ext_fps <- t.test(CSp, CSm, paired = TRUE)

differences <- CSp - CSm

# Standard deviation of the differences
sd_diff <- sd(differences, na.rm=T)

# Mean difference
mean_diff <- mean(differences, na.rm=T)

# Cohen's d (for paired samples)
cohen_d <- mean_diff / sd_diff

ext_fps
cohen_d

############ extinction ANOVA 
load("fps.RData")
fps <- fps[ fps$id != "024" & fps$id != "042" & fps$id != "043" & fps$id != "069" & fps$id != "081" 
            & fps$id != "085" & fps$id != "110" & fps$id != "124" & fps$id != "128" & fps$id != "134" 
            & fps$id != "136" & fps$id != "141" & fps$id != "144" & fps$id != "145 "& fps$id != "152" & fps$id != "156" 
            & fps$id != "159" & fps$id != "161" & fps$id != "162" & fps$id != "163" & fps$id != "173" 
            & fps$id != "175" & fps$id != "176" & fps$id != "178" & fps$id != "181" & fps$id != "183" 
            & fps$id != "184" & fps$id != "186" & fps$id != "220" & fps$id != "242" & fps$id != "246" & fps$id != "249" 
            & fps$id != "254" & fps$id != "255" & fps$id != "257" & fps$id != "268" & fps$id != "270", ]

dataSubset <- fps[which(fps$phase == "ext"),]

agg.fps <- with(dataSubset, aggregate(Tscore, by=list(id=id, stim=stim, half=half), FUN=mean, na.rm=T))

res.aov <- anova_test(
  data = agg.fps, dv = x, wid = id,
  within = c(stim, half)
)
get_anova_table(res.aov)


# Pairwise comparisons between stim
pwc1 <- agg.fps %>%
  group_by(half) %>%
  pairwise_t_test(
    x ~ stim, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc1

# Pairwise comparisons between time points
pwc2 <- agg.fps %>%
  group_by(stim) %>%
  pairwise_t_test(
    x ~ half, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2

############ Renewal

load("fps.RData")
fps <- fps[ fps$id != "024" & fps$id != "042" & fps$id != "043" & fps$id != "069" & fps$id != "081" 
            & fps$id != "085" & fps$id != "110" & fps$id != "124" & fps$id != "128" & fps$id != "134" 
            & fps$id != "136" & fps$id != "141" & fps$id != "144" & fps$id != "145 "& fps$id != "152" & fps$id != "156" 
            & fps$id != "159" & fps$id != "161" & fps$id != "162" & fps$id != "163" & fps$id != "173" 
            & fps$id != "175" & fps$id != "176" & fps$id != "178" & fps$id != "181" & fps$id != "183" 
            & fps$id != "184" & fps$id != "186" & fps$id != "220" & fps$id != "242" & fps$id != "246" & fps$id != "249" 
            & fps$id != "254" & fps$id != "255" & fps$id != "257" & fps$id != "268" & fps$id != "270", ]

dataSubset <- fps[which(fps$phase == "ren"),]

agg.fps <- with(dataSubset, aggregate(Tscore, by=list(id=id, stim=stim), FUN=mean, na.rm=T))

group_by(agg.fps, stim) %>%
  summarise(
    count = n(),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )

#visualize data
ggboxplot(agg.fps, x = "stim", y = "x", 
          color = "stim", palette = c("red2","blue1"),
          order = c("CSm", "CSp"),
          ylab = "FPS", xlab = "Stimulus")

# Subset weight data before treatment
CSm <- subset(agg.fps,  stim == "CSm", x,
              drop = TRUE)
# subset weight data after treatment
CSp <- subset(agg.fps,  stim == "CSp", x,
              drop = TRUE)

ren_fps <- t.test(CSp, CSm, paired = TRUE)

differences <- CSp - CSm

# Standard deviation of the differences
sd_diff <- sd(differences, na.rm=T)

# Mean difference
mean_diff <- mean(differences, na.rm=T)

# Cohen's d (for paired samples)
cohen_d <- mean_diff / sd_diff

ren_fps
cohen_d