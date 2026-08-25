# ART Project - plots of associations between ARTs and different physiological
# and behavioral indices of fear conditioning (SCR, FPS, Expectancy and Fear Ratings) 
# as well as the correlations with parameters estimates extracted from SPM
#
# Last updated: 25/08/2026 - Mana Ehlers

rm(list = ls())

### Load packages
library(cowplot)
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)

options(scipen = 999)


######################### Data Prep ########################
### Load data
load("questionnaires.RData")

#prep quest data
#mean centering of STAI, NEO-FFI-N und IUS
questionnaires$stai_centered <- questionnaires$stai_trait-mean(questionnaires$stai_trait, na.rm=T)
questionnaires$neo_ffi_n <- questionnaires$neo_ffi_n-mean(questionnaires$neo_ffi_n, na.rm=T)
questionnaires$ius_centered <- questionnaires$ius-mean(questionnaires$ius, na.rm=T)

#z-scores of quest data
questionnaires$stai_trait_z <- (questionnaires$stai_trait-mean(questionnaires$stai_trait, na.rm=T))/sd(questionnaires$stai_trait, na.rm=T)
questionnaires$neo_ffi_n_z <- (questionnaires$neo_ffi_n-mean(questionnaires$neo_ffi_n, na.rm=T))/sd(questionnaires$neo_ffi_n, na.rm=T)
questionnaires$ius_z <- (questionnaires$ius-mean(questionnaires$ius, na.rm=T))/sd(questionnaires$ius, na.rm=T)

#combine z-scores to have one score for each person
questionnaires$combined_z <- rowMeans(questionnaires[, c("ius_z", "stai_trait_z", "neo_ffi_n_z")], na.rm = TRUE)


########################################################################
######################### Plots combined ART score #####################
########################################################################


#################### SCR #########################

#### Acquisition

load("scr.RData")

scr <- scr[which(scr$stim != "US"),]
#exclude non-responders aka participants that respond to less than 2/3 of all US, that means 4 or 5 missing
scr <- scr[which(scr$id != '006' & scr$id != '014' & scr$id != '018' & scr$id != '021' & scr$id != '032' & scr$id != '040' & scr$id != '048' & scr$id != '060' & scr$id != '062' & scr$id != '063' & scr$id != '067' & scr$id != '072' & scr$id != '084' & scr$id != '087' & scr$id != '094' & scr$id != '096' & scr$id != '099' & scr$id != '101' & scr$id != '102' & scr$id != '103' & scr$id != '107' & scr$id != '110' & scr$id != '142' & scr$id != '144' & scr$id != '145' & scr$id!= '147' & scr$id != '150' & scr$id != '155' & scr$id != '159' & scr$id != '172' & scr$id != '173' & scr$id != '190' & scr$id != '194' & scr$id != '204' & scr$id != '206' & scr$id != '207' & scr$id != '208' & scr$id != '213' & scr$id != '220' & scr$id != '224' & scr$id != '227' & scr$id != '231' & scr$id != '235' & scr$id != '238' & scr$id != '239'&  scr$id != '240' & scr$id != '251' & scr$id != '255' & scr$id != '257' & scr$id != '259' & scr$id != '261' & scr$id != '264' & scr$id != '266 '& scr$id != '268'),]

########create data subsets: ACQ
dataSubset <- scr[which(scr$phase == "acq"),]
agg.scr.acq <- with(dataSubset, aggregate(log.rc, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.scr.acq.wide <- reshape(agg.scr.acq, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.scr.acq.wide$scr_acq_CSdiff <- agg.scr.acq.wide$x.CSp-agg.scr.acq.wide$x.CSm
#rename columns
names(agg.scr.acq.wide)[names(agg.scr.acq.wide) == "x.CSm"] <- "scr_acq_CSm"
names(agg.scr.acq.wide)[names(agg.scr.acq.wide) == "x.CSp"] <- "scr_acq_CSp"

dataSubset_scr_acq <- merge(questionnaires, agg.scr.acq.wide,  by="id")

#convert back to long format
dataSubset_scr_acq_long <- gather(dataSubset_scr_acq, stim, scr_acq, scr_acq_CSm:scr_acq_CSdiff, factor_key = TRUE) 

#make stim factor with specific order
dataSubset_scr_acq_long$stim <- factor(dataSubset_scr_acq_long$stim, levels = c("scr_acq_CSdiff", "scr_acq_CSp", "scr_acq_CSm"))

#plot scr and combined score
A <- ggplot(dataSubset_scr_acq_long, aes(x=combined_z, y=scr_acq, color=stim)) +
  geom_point() + 
  labs(
    title = "Acquisition",
    x = NULL,
    y = "SCR (log, rc)"
  ) +
  geom_smooth(method = lm, aes(group = stim), se = FALSE, fullrange = TRUE) +
  scale_color_manual(values = c('gray30', 'red2', 'blue1')) +
  scale_y_continuous(limits = c(-0.15, 0.8)) +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # larger bold title
    axis.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10))
  ) 


####### Extinction

load("scr.RData")

scr <- scr[which(scr$stim != "US"),]
#exclude non-responders aka participants that respond to less than 2/3 of all US, that means 4 or 5 missing
scr <- scr[which(scr$id != '006' & scr$id != '014' & scr$id != '018' & scr$id != '021' & scr$id != '032' & scr$id != '040' & scr$id != '048' & scr$id != '060' & scr$id != '062' & scr$id != '063' & scr$id != '067' & scr$id != '072' & scr$id != '084' & scr$id != '087' & scr$id != '094' & scr$id != '096' & scr$id != '099' & scr$id != '101' & scr$id != '102' & scr$id != '103' & scr$id != '107' & scr$id != '110' & scr$id != '142' & scr$id != '144' & scr$id != '145' & scr$id!= '147' & scr$id != '150' & scr$id != '155' & scr$id != '159' & scr$id != '172' & scr$id != '173' & scr$id != '190' & scr$id != '194' & scr$id != '204' & scr$id != '206' & scr$id != '207' & scr$id != '208' & scr$id != '213' & scr$id != '220' & scr$id != '224' & scr$id != '227' & scr$id != '231' & scr$id != '235' & scr$id != '238' & scr$id != '239'&  scr$id != '240' & scr$id != '251' & scr$id != '255' & scr$id != '257' & scr$id != '259' & scr$id != '261' & scr$id != '264' & scr$id != '266 '& scr$id != '268'),]

########create data subsets: ext
dataSubset <- scr[which(scr$phase == "ext"),]
agg.scr.ext <- with(dataSubset, aggregate(log.rc, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.scr.ext.wide <- reshape(agg.scr.ext, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.scr.ext.wide$scr_ext_CSdiff <- agg.scr.ext.wide$x.CSp-agg.scr.ext.wide$x.CSm
#rename columns
names(agg.scr.ext.wide)[names(agg.scr.ext.wide) == "x.CSm"] <- "scr_ext_CSm"
names(agg.scr.ext.wide)[names(agg.scr.ext.wide) == "x.CSp"] <- "scr_ext_CSp"

dataSubset_scr_ext <- merge(questionnaires, agg.scr.ext.wide,  by="id")

#convert back to long format
dataSubset_scr_ext_long <- gather(dataSubset_scr_ext, stim, scr_ext, scr_ext_CSm:scr_ext_CSdiff, factor_key = TRUE) 

#make stim factor with specific order
dataSubset_scr_ext_long$stim <- factor(dataSubset_scr_ext_long$stim, levels = c("scr_ext_CSdiff", "scr_ext_CSp", "scr_ext_CSm"))


#plot scr and combined score
B <- ggplot(dataSubset_scr_ext_long, aes(x=combined_z, y=scr_ext, color=stim)) +
  geom_point() + 
  labs(
    title = "Extinction",
    x = NULL,
    y = NULL
  ) +
  geom_smooth(method = lm, aes(group = stim), se = FALSE, fullrange = TRUE) +
  scale_color_manual(values = c('gray30', 'red2', 'blue1')) +
  scale_y_continuous(limits = c(-0.15, 0.8)) + 
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # larger bold title
    axis.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 12, face = "bold")
  ) 


####### Renewal

load("scr.RData")

scr <- scr[which(scr$stim != "US"),]
#exclude non-responders aka participants that respond to less than 2/3 of all US, that means 4 or 5 missing
scr <- scr[which(scr$id != '006' & scr$id != '014' & scr$id != '018' & scr$id != '021' & scr$id != '032' & scr$id != '040' & scr$id != '048' & scr$id != '060' & scr$id != '062' & scr$id != '063' & scr$id != '067' & scr$id != '072' & scr$id != '084' & scr$id != '087' & scr$id != '094' & scr$id != '096' & scr$id != '099' & scr$id != '101' & scr$id != '102' & scr$id != '103' & scr$id != '107' & scr$id != '110' & scr$id != '142' & scr$id != '144' & scr$id != '145' & scr$id!= '147' & scr$id != '150' & scr$id != '155' & scr$id != '159' & scr$id != '172' & scr$id != '173' & scr$id != '190' & scr$id != '194' & scr$id != '204' & scr$id != '206' & scr$id != '207' & scr$id != '208' & scr$id != '213' & scr$id != '220' & scr$id != '224' & scr$id != '227' & scr$id != '231' & scr$id != '235' & scr$id != '238' & scr$id != '239'&  scr$id != '240' & scr$id != '251' & scr$id != '255' & scr$id != '257' & scr$id != '259' & scr$id != '261' & scr$id != '264' & scr$id != '266 '& scr$id != '268'),]

########create data subsets: ren
dataSubset <- scr[which(scr$phase == "ren"),]
agg.scr.ren <- with(dataSubset, aggregate(log.rc, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.scr.ren.wide <- reshape(agg.scr.ren, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.scr.ren.wide$scr_ren_CSdiff <- agg.scr.ren.wide$x.CSp-agg.scr.ren.wide$x.CSm
#rename columns
names(agg.scr.ren.wide)[names(agg.scr.ren.wide) == "x.CSm"] <- "scr_ren_CSm"
names(agg.scr.ren.wide)[names(agg.scr.ren.wide) == "x.CSp"] <- "scr_ren_CSp"

dataSubset_scr_ren <- merge(questionnaires, agg.scr.ren.wide,  by="id")

#convert back to long format
dataSubset_scr_ren_long <- gather(dataSubset_scr_ren, stim, scr_ren, scr_ren_CSm:scr_ren_CSdiff, factor_key = TRUE) 

#make stim factor with specific order
dataSubset_scr_ren_long$stim <- factor(dataSubset_scr_ren_long$stim, levels = c("scr_ren_CSdiff", "scr_ren_CSp", "scr_ren_CSm"))

#plot scr and combined score
C <- ggplot(dataSubset_scr_ren_long, aes(x=combined_z, y=scr_ren, color=stim)) +
  geom_point() + 
  labs(
    title = "Renewal",
    x = NULL,
    y = NULL
  ) +
  geom_smooth(method = lm, aes(group = stim), se = FALSE, fullrange = TRUE) +
  scale_color_manual(
    name = "Stimulus",
    values = c('gray30', 'red2', 'blue1'),
    labels = c("CSdiff", "CS+", "CS-")
  )+
  scale_y_continuous(limits = c(-0.15, 0.8)) + 
  theme_classic() +
  theme(
    legend.position = c(0.9, 0.9),   # "top", "bottom", or c(0.8, 0.2)
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # larger bold title
    axis.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 12, face = "bold")
  )


#################### FPS #########################
# Plot J, K, L: FPS

####### Acquisition

load("fps.RData")
fps <- fps[ fps$id != "024" & fps$id != "042" & fps$id != "043" & fps$id != "069" & fps$id != "081" 
            & fps$id != "085" & fps$id != "110" & fps$id != "124" & fps$id != "128" & fps$id != "134" 
            & fps$id != "136" & fps$id != "141" & fps$id != "144" & fps$id != "145 "& fps$id != "152" & fps$id != "156" 
            & fps$id != "159" & fps$id != "161" & fps$id != "162" & fps$id != "163" & fps$id != "173" 
            & fps$id != "175" & fps$id != "176" & fps$id != "178" & fps$id != "181" & fps$id != "183" 
            & fps$id != "184" & fps$id != "186" & fps$id != "220" & fps$id != "242" & fps$id != "246" & fps$id != "249" 
            & fps$id != "254" & fps$id != "255" & fps$id != "257" & fps$id != "268" & fps$id != "270", ]


########create data subsets: acq
dataSubset <- fps[which(fps$phase == "acq" & fps$stim != "iti"),]
agg.fps.acq <- with(dataSubset, aggregate(Tscore, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.fps.acq.wide <- reshape(agg.fps.acq, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.fps.acq.wide$fps_acq_CSdiff <- agg.fps.acq.wide$x.CSp-agg.fps.acq.wide$x.CSm
#rename columns
names(agg.fps.acq.wide)[names(agg.fps.acq.wide) == "x.CSm"] <- "fps_acq_CSm"
names(agg.fps.acq.wide)[names(agg.fps.acq.wide) == "x.CSp"] <- "fps_acq_CSp"

dataSubset_fps_acq <- merge(questionnaires, agg.fps.acq.wide,  by="id")

#convert back to long format
dataSubset_fps_acq_long <- gather(dataSubset_fps_acq, stim, fps_acq, fps_acq_CSm:fps_acq_CSdiff, factor_key = TRUE) 

#make stim factor with specific order
dataSubset_fps_acq_long$stim <- factor(dataSubset_fps_acq_long$stim, levels = c("fps_acq_CSdiff", "fps_acq_CSp", "fps_acq_CSm"))


#plot fps and stai-t
J <- ggplot(dataSubset_fps_acq_long, aes(x=combined_z, y=fps_acq, color=stim)) +
  geom_point() + 
  labs(x = NULL,
       y="Startle Response (T-Score)") +
  geom_smooth(method=lm, aes(group = stim), se=FALSE, fullrange=TRUE) +
  scale_color_manual(values=c('gray30','red2', 'blue1'))+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 14, face = "bold"),
        axis.text  = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)))
#stat_cor(method = "pearson", digits = 3)

######## Extinction

load("fps.RData")
fps <- fps[ fps$id != "024" & fps$id != "042" & fps$id != "043" & fps$id != "069" & fps$id != "081" 
            & fps$id != "085" & fps$id != "110" & fps$id != "124" & fps$id != "128" & fps$id != "134" 
            & fps$id != "136" & fps$id != "141" & fps$id != "144" & fps$id != "145 "& fps$id != "152" & fps$id != "156" 
            & fps$id != "159" & fps$id != "161" & fps$id != "162" & fps$id != "163" & fps$id != "173" 
            & fps$id != "175" & fps$id != "176" & fps$id != "178" & fps$id != "181" & fps$id != "183" 
            & fps$id != "184" & fps$id != "186" & fps$id != "220" & fps$id != "242" & fps$id != "246" & fps$id != "249" 
            & fps$id != "254" & fps$id != "255" & fps$id != "257" & fps$id != "268" & fps$id != "270", ]


########create data subsets: ext
dataSubset <- fps[which(fps$phase == "ext"),]
agg.fps.ext <- with(dataSubset, aggregate(Tscore, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.fps.ext.wide <- reshape(agg.fps.ext, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.fps.ext.wide$fps_ext_CSdiff <- agg.fps.ext.wide$x.CSp-agg.fps.ext.wide$x.CSm
#rename columns
names(agg.fps.ext.wide)[names(agg.fps.ext.wide) == "x.CSm"] <- "fps_ext_CSm"
names(agg.fps.ext.wide)[names(agg.fps.ext.wide) == "x.CSp"] <- "fps_ext_CSp"

dataSubset_fps_ext <- merge(questionnaires, agg.fps.ext.wide,  by="id")

#convert back to long format
dataSubset_fps_ext_long <- gather(dataSubset_fps_ext, stim, fps_ext, fps_ext_CSm:fps_ext_CSdiff, factor_key = TRUE) 

#make stim factor with specific order
dataSubset_fps_ext_long$stim <- factor(dataSubset_fps_ext_long$stim, levels = c("fps_ext_CSdiff", "fps_ext_CSp", "fps_ext_CSm"))


#plot fps and stai-t
K <- ggplot(dataSubset_fps_ext_long, aes(x=combined_z, y=fps_ext, color=stim)) +
  geom_point() + 
  labs(x = NULL, y = NULL) +
  geom_smooth(method=lm, aes(group = stim), se=FALSE, fullrange=TRUE) +
  scale_color_manual(values=c('gray30','red2', 'blue1'))+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 14, face = "bold"),
        axis.text  = element_text(size = 12, face = "bold")) 
#stat_cor(method = "pearson", digits = 3)

######## Renewal

load("fps.RData")
fps <- fps[ fps$id != "024" & fps$id != "042" & fps$id != "043" & fps$id != "069" & fps$id != "081" 
            & fps$id != "085" & fps$id != "110" & fps$id != "124" & fps$id != "128" & fps$id != "134" 
            & fps$id != "136" & fps$id != "141" & fps$id != "144" & fps$id != "145 "& fps$id != "152" & fps$id != "156" 
            & fps$id != "159" & fps$id != "161" & fps$id != "162" & fps$id != "163" & fps$id != "173" 
            & fps$id != "175" & fps$id != "176" & fps$id != "178" & fps$id != "181" & fps$id != "183" 
            & fps$id != "184" & fps$id != "186" & fps$id != "220" & fps$id != "242" & fps$id != "246" & fps$id != "249" 
            & fps$id != "254" & fps$id != "255" & fps$id != "257" & fps$id != "268" & fps$id != "270", ]

########create data subsets: ren
dataSubset <- fps[which(fps$phase == "ren"),]
agg.fps.ren <- with(dataSubset, aggregate(Tscore, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.fps.ren.wide <- reshape(agg.fps.ren, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.fps.ren.wide$fps_ren_CSdiff <- agg.fps.ren.wide$x.CSp-agg.fps.ren.wide$x.CSm
#rename columns
names(agg.fps.ren.wide)[names(agg.fps.ren.wide) == "x.CSm"] <- "fps_ren_CSm"
names(agg.fps.ren.wide)[names(agg.fps.ren.wide) == "x.CSp"] <- "fps_ren_CSp"

dataSubset_fps_ren <- merge(questionnaires, agg.fps.ren.wide,  by="id")

#convert back to long format
dataSubset_fps_ren_long <- gather(dataSubset_fps_ren, stim, fps_ren, fps_ren_CSm:fps_ren_CSdiff, factor_key = TRUE) 

#make stim factor with specific order
dataSubset_fps_ren_long$stim <- factor(dataSubset_fps_ren_long$stim, levels = c("fps_ren_CSdiff", "fps_ren_CSp", "fps_ren_CSm"))


#plot fps and stai-t
L <- ggplot(dataSubset_fps_ren_long, aes(x=combined_z, y=fps_ren, color=stim)) +
  geom_point() + 
  labs(x = NULL, y = NULL) +
  geom_smooth(method=lm, aes(group = stim), se=FALSE, fullrange=TRUE) +
  scale_color_manual(values=c('gray30','red2', 'blue1'))+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 14, face = "bold"),
        axis.text  = element_text(size = 12, face = "bold")) 
#stat_cor(method = "pearson", digits = 3)

#################### Expectancy Ratings #########################

##### Acquisition

load("ratings.RData")

########create data subsets: ACQ
dataSubset <- ratings[which(ratings$phase == "acq"),]
agg.ratings.acq <- with(dataSubset, aggregate(exp_rating, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.ratings.acq.wide <- reshape(agg.ratings.acq, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.ratings.acq.wide$ratings_acq_CSdiff <- agg.ratings.acq.wide$x.CSp-agg.ratings.acq.wide$x.CSm
#rename columns
names(agg.ratings.acq.wide)[names(agg.ratings.acq.wide) == "x.CSm"] <- "ratings_acq_CSm"
names(agg.ratings.acq.wide)[names(agg.ratings.acq.wide) == "x.CSp"] <- "ratings_acq_CSp"

dataSubset_ratings_acq <- merge(questionnaires, agg.ratings.acq.wide,  by="id")

#convert back to long format
dataSubset_ratings_acq_long <- gather(dataSubset_ratings_acq, stim, ratings_acq, ratings_acq_CSm:ratings_acq_CSdiff, factor_key = TRUE) 

#make stim factor with specific order
dataSubset_ratings_acq_long$stim <- factor(dataSubset_ratings_acq_long$stim, levels = c("ratings_acq_CSdiff", "ratings_acq_CSp", "ratings_acq_CSm"))


#plot ratings and stai-t
G <- ggplot(dataSubset_ratings_acq_long, aes(x=combined_z, y=ratings_acq, color=stim)) +
  geom_point() + 
  labs(x = NULL, y="Expectancy Ratings") +
  geom_smooth(method=lm, aes(group = stim), se=FALSE, fullrange=TRUE) +
  scale_color_manual(values=c('gray30','red2', 'blue1'))+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 14, face = "bold"),
        axis.text  = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)))

##### Extinction

load("ratings.RData")

########create data subsets: EXT
dataSubset <- ratings[which(ratings$phase == "ext"),]
agg.ratings.ext <- with(dataSubset, aggregate(exp_rating, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.ratings.ext.wide <- reshape(agg.ratings.ext, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.ratings.ext.wide$ratings_ext_CSdiff <- agg.ratings.ext.wide$x.CSp-agg.ratings.ext.wide$x.CSm
#rename columns
names(agg.ratings.ext.wide)[names(agg.ratings.ext.wide) == "x.CSm"] <- "ratings_ext_CSm"
names(agg.ratings.ext.wide)[names(agg.ratings.ext.wide) == "x.CSp"] <- "ratings_ext_CSp"

dataSubset_ratings_ext <- merge(questionnaires, agg.ratings.ext.wide,  by="id")

#convert back to long format
dataSubset_ratings_ext_long <- gather(dataSubset_ratings_ext, stim, ratings_ext, ratings_ext_CSm:ratings_ext_CSdiff, factor_key = TRUE) 

#make stim factor with specific order
dataSubset_ratings_ext_long$stim <- factor(dataSubset_ratings_ext_long$stim, levels = c("ratings_ext_CSdiff", "ratings_ext_CSp", "ratings_ext_CSm"))


#plot ratings and stai-t
H <- ggplot(dataSubset_ratings_ext_long, aes(x=combined_z, y=ratings_ext, color=stim)) +
  geom_point() + 
  labs(x = NULL,  y = NULL) +
  geom_smooth(method=lm, aes(group = stim), se=FALSE, fullrange=TRUE) +
  scale_color_manual(values=c('gray30','red2', 'blue1'))+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 14, face = "bold"),
        axis.text  = element_text(size = 12, face = "bold")) 

##### Renewal

load("ratings.RData")

########create data subsets: REN
dataSubset <- ratings[which(ratings$phase == "ren"),]
agg.ratings.ren <- with(dataSubset, aggregate(exp_rating, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.ratings.ren.wide <- reshape(agg.ratings.ren, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.ratings.ren.wide$ratings_ren_CSdiff <- agg.ratings.ren.wide$x.CSp-agg.ratings.ren.wide$x.CSm
#rename columns
names(agg.ratings.ren.wide)[names(agg.ratings.ren.wide) == "x.CSm"] <- "ratings_ren_CSm"
names(agg.ratings.ren.wide)[names(agg.ratings.ren.wide) == "x.CSp"] <- "ratings_ren_CSp"

dataSubset_ratings_ren <- merge(questionnaires, agg.ratings.ren.wide,  by="id")

#convert back to long format
dataSubset_ratings_ren_long <- gather(dataSubset_ratings_ren, stim, ratings_ren, ratings_ren_CSm:ratings_ren_CSdiff, factor_key = TRUE) 

#make stim factor with specific order
dataSubset_ratings_ren_long$stim <- factor(dataSubset_ratings_ren_long$stim, levels = c("ratings_ren_CSdiff", "ratings_ren_CSp", "ratings_ren_CSm"))


#plot ratings and stai-t
I <- ggplot(dataSubset_ratings_ren_long, aes(x=combined_z, y=ratings_ren, color=stim)) +
  geom_point() + 
  labs(x = NULL,  y = NULL) +
  geom_smooth(method=lm, aes(group = stim), se=FALSE, fullrange=TRUE) +
  scale_color_manual(values=c('gray30','red2', 'blue1'))+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 14, face = "bold"),
        axis.text  = element_text(size = 12, face = "bold")) 


#################### Fear ratings #########################

#### Acquisition
load("ratings.RData")

########create data subsets: ACQ
dataSubset <- ratings[which(ratings$phase == "acq"),]
agg.ratings.acq <- with(dataSubset, aggregate(post_rating, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.ratings.acq.wide <- reshape(agg.ratings.acq, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.ratings.acq.wide$fear_acq_CSdiff <- agg.ratings.acq.wide$x.CSp-agg.ratings.acq.wide$x.CSm
#rename columns
names(agg.ratings.acq.wide)[names(agg.ratings.acq.wide) == "x.CSm"] <- "fear_acq_CSm"
names(agg.ratings.acq.wide)[names(agg.ratings.acq.wide) == "x.CSp"] <- "fear_acq_CSp"

dataSubset_ratings_acq <- merge(questionnaires, agg.ratings.acq.wide,  by="id")

#convert back to long format
dataSubset_fear_acq_long <- gather(dataSubset_ratings_acq, stim, fear_acq, fear_acq_CSm:fear_acq_CSdiff, factor_key = TRUE) 

#make stim factor with specific order
dataSubset_fear_acq_long$stim <- factor(dataSubset_fear_acq_long$stim, levels = c("fear_acq_CSdiff", "fear_acq_CSp", "fear_acq_CSm"))

#plot ratings and stai-t
D <- ggplot(dataSubset_fear_acq_long, aes(x=combined_z, y=fear_acq, color=stim)) +
  geom_point() + 
  labs(x="Z-Score Anxiety-Related Traits",y="Fear Ratings") +
  geom_smooth(method=lm, aes(group = stim), se=FALSE, fullrange=TRUE) +
  scale_color_manual(values=c('gray30','red2', 'blue1'))+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 14, face = "bold"),
        axis.text  = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10))) 

##### Extinction
load("ratings.RData")

########create data subsets: EXT
dataSubset <- ratings[which(ratings$phase == "ext"),]
agg.ratings.ext <- with(dataSubset, aggregate(post_rating, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.ratings.ext.wide <- reshape(agg.ratings.ext, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.ratings.ext.wide$fear_ext_CSdiff <- agg.ratings.ext.wide$x.CSp-agg.ratings.ext.wide$x.CSm
#rename columns
names(agg.ratings.ext.wide)[names(agg.ratings.ext.wide) == "x.CSm"] <- "fear_ext_CSm"
names(agg.ratings.ext.wide)[names(agg.ratings.ext.wide) == "x.CSp"] <- "fear_ext_CSp"

dataSubset_ratings_ext <- merge(questionnaires, agg.ratings.ext.wide,  by="id")

#convert back to long format
dataSubset_fear_ext_long <- gather(dataSubset_ratings_ext, stim, fear_ext, fear_ext_CSm:fear_ext_CSdiff, factor_key = TRUE) 

#make stim factor with specific order
dataSubset_fear_ext_long$stim <- factor(dataSubset_fear_ext_long$stim, levels = c("fear_ext_CSdiff", "fear_ext_CSp", "fear_ext_CSm"))

#plot ratings and stai-t
E <- ggplot(dataSubset_fear_ext_long, aes(x=combined_z, y=fear_ext, color=stim)) +
  geom_point() + 
  labs(x="Z-Score Anxiety-Related Traits", y = NULL) +
  geom_smooth(method=lm, aes(group = stim), se=FALSE, fullrange=TRUE) +
  scale_color_manual(values=c('gray30','red2', 'blue1'))+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 14, face = "bold"),
        axis.text  = element_text(size = 12, face = "bold")) 

##### Renewal
load("ratings.RData")

########create data subsets: ren
dataSubset <- ratings[which(ratings$phase == "ren"),]
agg.ratings.ren <- with(dataSubset, aggregate(post_rating, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.ratings.ren.wide <- reshape(agg.ratings.ren, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.ratings.ren.wide$fear_ren_CSdiff <- agg.ratings.ren.wide$x.CSp-agg.ratings.ren.wide$x.CSm
#rename columns
names(agg.ratings.ren.wide)[names(agg.ratings.ren.wide) == "x.CSm"] <- "fear_ren_CSm"
names(agg.ratings.ren.wide)[names(agg.ratings.ren.wide) == "x.CSp"] <- "fear_ren_CSp"

dataSubset_ratings_ren <- merge(questionnaires, agg.ratings.ren.wide,  by="id")

#convert back to long format
dataSubset_fear_ren_long <- gather(dataSubset_ratings_ren, stim, fear_ren, fear_ren_CSm:fear_ren_CSdiff, factor_key = TRUE) 

#make stim factor with specific order
dataSubset_fear_ren_long$stim <- factor(dataSubset_fear_ren_long$stim, levels = c("fear_ren_CSdiff", "fear_ren_CSp", "fear_ren_CSm"))

#plot ratings and stai-t
F <- ggplot(dataSubset_fear_ren_long, aes(x=combined_z, y=fear_ren, color=stim)) +
  geom_point() + 
  labs(x="Z-Score Anxiety-Related Traits", y = NULL) +
  geom_smooth(method=lm, aes(group = stim), se=FALSE, fullrange=TRUE) +
  scale_color_manual(values=c('gray30','red2', 'blue1'))+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 14, face = "bold"),
        axis.text  = element_text(size = 12, face = "bold")) 


######################################################################
########################### Combine plots ############################
######################################################################

# Align plots within each row
row1 <- align_plots(A, B, C, align = "v", axis = "l")
row2 <- align_plots(J, K, L, align = "v", axis = "l")
row3 <- align_plots(G, H, I, align = "v", axis = "l")
row4 <- align_plots(D, E, F, align = "v", axis = "l")

# Make each row
row1 <- plot_grid(plotlist = row1, nrow = 1,
                  labels = c("A", "B", "C"), label_size = 14)
row2 <- plot_grid(plotlist = row2, nrow = 1,
                  labels = c("D", "E", "F"), label_size = 14)
row3 <- plot_grid(plotlist = row3, nrow = 1,
                  labels = c("G", "H", "I"), label_size = 14)
row4 <- plot_grid(plotlist = row4, nrow = 1,
                  labels = c("J", "K", "L"), label_size = 14)

# Combine rows
plot_all <- plot_grid(
  row1, row2, row3, row4,
  ncol = 1,
  align = "v"
)

ggsave(
  "art_plot_pub.png",
  plot = plot_all,
  width = 14,
  height = 14,
  units = "in",
  dpi = 600
)


######################################################################
########################### fMRI plots ###############################
######################################################################

######### renewal dACC #######
load("betas_ren_dACC.RData")

#exclude sub-161 as outlier
betas_ren_dACC = betas_ren_dACC[which(betas_ren_dACC$id != '161'),]

#calculate CS diff
betas_ren_dACC$CSdiff<-betas_ren_dACC$CSp - betas_ren_dACC$CSm

dataSubset_betas_ren <- merge(questionnaires, betas_ren_dACC,  by="id")

#convert to long format
dataSubset_betas_ren_long <- gather(dataSubset_betas_ren, stim, betas, CSp:CSdiff, factor_key = TRUE) 


#make stim factor with specific order
dataSubset_betas_ren_long$stim <- factor(dataSubset_betas_ren_long$stim, levels = c("CSdiff", "CSp", "CSm"))

#plot scr and combined score
A <- ggplot(dataSubset_betas_ren_long, aes(x=combined_z, y=betas, color=stim)) +
  geom_point() + 
  labs(
    title = "dACC [-6, 24, 48]",
    x = "Z-Score Anxiety-Related Traits",
    y = "Parameter Estimates"
  ) +
  geom_smooth(method = lm, aes(group = stim), se = FALSE, fullrange = TRUE) +
  scale_color_manual(
    name = "Stimulus",
    values = c('gray30','red2', 'blue1'),
    labels = c("CSdiff", "CS+", "CS-")  # adjust to your stim levels
  ) +
  #scale_y_continuous(limits = c(-5.2, 4.1)) +
  theme_classic() +
  theme(
    #legend.position = c(0.95, 0.1),   # "top", "bottom", or c(0.8, 0.2)
    plot.title = element_text(hjust = 0.1, size = 16, face = "bold"),  # larger bold title
    legend.position = "none",
    axis.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 12, face = "bold"),
  )

plot(A)

######### renewal thalamus #######

load("questionnaires.RData")
#z-scores of quest data
questionnaires$ius_z <- (questionnaires$ius-mean(questionnaires$ius, na.rm=T))/sd(questionnaires$ius, na.rm=T)
questionnaires$stai_trait_z <- (questionnaires$stai_trait-mean(questionnaires$stai_trait, na.rm=T))/sd(questionnaires$stai_trait, na.rm=T)
questionnaires$neo_ffi_n_z <- (questionnaires$neo_ffi_n-mean(questionnaires$neo_ffi_n, na.rm=T))/sd(questionnaires$neo_ffi_n, na.rm=T)

#combine z-scores to have one score for each person
questionnaires$combined_z <- rowMeans(questionnaires[, c("ius_z", "stai_trait_z", "neo_ffi_n_z")], na.rm = TRUE)

load("betas_ren_thalamus.RData")


#calculate CS diff
betas_ren_thalamus$CSdiff<-betas_ren_thalamus$CSp - betas_ren_thalamus$CSm

dataSubset_betas_ren <- merge(questionnaires, betas_ren_thalamus,  by="id")

#convert to long format
dataSubset_betas_ren_long <- gather(dataSubset_betas_ren, stim, betas, CSp:CSdiff, factor_key = TRUE) 


#make stim factor with specific order
dataSubset_betas_ren_long$stim <- factor(dataSubset_betas_ren_long$stim, levels = c("CSdiff", "CSp", "CSm"))

#plot scr and combined score
B <- ggplot(dataSubset_betas_ren_long, aes(x=combined_z, y=betas, color=stim)) +
  geom_point() + 
  labs(
    title = "Thalamus [15, -8, 16]",
    x = "Z-Score Anxiety-Related Traits",
    y = NULL
  ) +
  geom_smooth(method = lm, aes(group = stim), se = FALSE, fullrange = TRUE) +
  scale_color_manual(
    name = "Stimulus",
    values = c('gray30','red2', 'blue1'),
    labels = c("CSdiff", "CS+", "CS-")  # adjust to your stim levels
  ) +
  #scale_y_continuous(limits = c(-5.2, 4.1)) +
  theme_classic() +
  theme(
    #legend.position = c(0.95, 0.1),   # "top", "bottom", or c(0.8, 0.2)
    #plot.title = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.1, size = 16, face = "bold"),  # larger bold title
    legend.position = "none",
    axis.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 12, face = "bold"),
  )

plot(B)

######### renewal insula #######

load("questionnaires.RData")
#z-scores of quest data
questionnaires$ius_z <- (questionnaires$ius-mean(questionnaires$ius, na.rm=T))/sd(questionnaires$ius, na.rm=T)
questionnaires$stai_trait_z <- (questionnaires$stai_trait-mean(questionnaires$stai_trait, na.rm=T))/sd(questionnaires$stai_trait, na.rm=T)
questionnaires$neo_ffi_n_z <- (questionnaires$neo_ffi_n-mean(questionnaires$neo_ffi_n, na.rm=T))/sd(questionnaires$neo_ffi_n, na.rm=T)

#combine z-scores to have one score for each person
questionnaires$combined_z <- rowMeans(questionnaires[, c("ius_z", "stai_trait_z", "neo_ffi_n_z")], na.rm = TRUE)

load("betas_ren_insula.RData")

#calculate CS diff
betas_ren_insula$CSdiff<-betas_ren_insula$CSp - betas_ren_insula$CSm

dataSubset_betas_ren <- merge(questionnaires, betas_ren_insula,  by="id")

#convert to long format
dataSubset_betas_ren_long <- gather(dataSubset_betas_ren, stim, betas, CSp:CSdiff, factor_key = TRUE) 


#make stim factor with specific order
dataSubset_betas_ren_long$stim <- factor(dataSubset_betas_ren_long$stim, levels = c("CSdiff", "CSp", "CSm"))

#plot scr and combined score
C <- ggplot(dataSubset_betas_ren_long, aes(x=combined_z, y=betas, color=stim)) +
  geom_point() + 
  labs(
    title = "Insula [-38, 8, -14]",
    x = "Z-Score Anxiety-Related Traits",
    y = NULL
  ) +
  geom_smooth(method = lm, aes(group = stim), se = FALSE, fullrange = TRUE) +
  scale_color_manual(
    name = "Stimulus",
    values = c('gray30','red2', 'blue1'),
    labels = c("CSdiff", "CS+", "CS-")  # adjust to your stim levels
  ) +
  #scale_y_continuous(limits = c(-5.2, 4.1)) +
  theme_classic() +
  theme(
    #legend.position = c(0.15, 0.16),   # "top", "bottom", or c(0.8, 0.2)
    #plot.title = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.1, size = 16, face = "bold"),  # larger bold title
    legend.position = "none",
    axis.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 12, face = "bold"),
    
  )

plot(C)

plot_all <- ggarrange(
  A, B, C,
  ncol = 3,
  labels = c("A", "B", "C"),
  font.label = list(size = 14, face = "bold")
)

plot_all
