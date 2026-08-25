# ART Project - experimental manipulation plots for SCR, FPS, Expectancy and Fear Ratings
# Figure 3 in publication
#
# Last updated: 25/08/2026 - Mana Ehlers


rm(list = ls())

### Load packages
library(ggplot2)
library(ggpubr)

options(scipen = 999)

# SE-Function
se <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
}

################################################################################
########################## Skin Conductance Response ###########################
################################################################################

#### Acquisition 

### Load data
load("scr.RData")

#exclude non-responders aka participants that respond to less than 2/3 of all US, that means 4 or 5 missing
scr <- scr[which(scr$id != '006' & scr$id != '014' & scr$id != '018' & scr$id != '021' & scr$id != '032' 
                 & scr$id != '040' & scr$id != '048' & scr$id != '060' & scr$id != '062' & scr$id != '063' 
                 & scr$id != '067' & scr$id != '072' & scr$id != '084' & scr$id != '087' & scr$id != '094' 
                 & scr$id != '096' & scr$id != '099' & scr$id != '101' & scr$id != '102' & scr$id != '103' 
                 & scr$id != '107' & scr$id != '110' & scr$id != '142' & scr$id != '144' & scr$id != '145' 
                 & scr$id != '147' & scr$id != '150' & scr$id != '155' & scr$id != '159' & scr$id != '172' 
                 & scr$id != '173' & scr$id != '190' & scr$id != '194' & scr$id != '204' & scr$id != '206' 
                 & scr$id != '207' & scr$id != '208' & scr$id != '213' & scr$id != '220' & scr$id != '224' 
                 & scr$id != '227' & scr$id != '231' & scr$id != '235' & scr$id != '238' & scr$id != '239' 
                 & scr$id != '240' & scr$id != '251' & scr$id != '255' & scr$id != '257' & scr$id != '259' 
                 & scr$id != '261' & scr$id != '264' & scr$id != '266 '& scr$id != '268'),]

# create data subset
dataSubset <- scr[which(scr$stim != 'US' & scr$phase == "acq"),]
  
# Define n for SE
n <- length(unique(dataSubset$id))  

agg.scr <- with(dataSubset, aggregate(log.rc, by=list(stim=stim, trial=trial, phase=phase), FUN=mean, na.rm=T))
agg.scr$se <- with(dataSubset, aggregate(log.rc, by=list(stim=stim, trial= trial, phase=phase), FUN=se))[ ,4]
  
# Add confidence interval
agg.scr$lower <- agg.scr$x - 1.96*agg.scr$se
agg.scr$upper <- agg.scr$x + 1.96*agg.scr$se
  
name_y_lab <- "SCR (log, rc)" 
y_limit <- (round(max(agg.scr$upper), digits=1) + 0.1)
y_break <- 0.05  
  
# Plot
theme_set(theme_bw())
scr_acq <- ggplot(agg.scr, aes(x = trial, y = x, colour = stim, group = stim))+
  geom_point(aes(group=stim), size=2) +
  geom_line(aes(group=stim), size=1.2) +
  geom_ribbon(aes(ymin=lower, ymax=upper, colour= NA, fill=stim), alpha=0.2) +
  scale_colour_manual(values=c("red2","blue1"), name = "Stimulus:", breaks=c("CSp","CSm"),
                        labels=c("CS+", "CS-")) +
  scale_fill_manual(values=c("red2","blue1"), name = "Stimulus:", breaks=c("CSp","CSm"),
                      labels=c("CS+", "CS-")) +
  xlab("Trial") +
  ylab(name_y_lab) + 
  scale_x_continuous(breaks=c(seq(1,11,2))) +
  scale_y_continuous(breaks=seq(0, 0.6, 0.1), expand=c(0,0)) +
  coord_cartesian(ylim = c(0, 0.6)) + 
  ggtitle("Acquisition") +
    
  theme(plot.title = element_text(size=15, face="bold", hjust=0.5, vjust=2),
        axis.text.x = element_text(size=15,  color="black"),
        axis.text.y = element_text(size=15, color="black"), 
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15, margin=margin(0,10,0,0)),
        legend.position = "none",
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
  

##### Extinction

### Load data
load("scr.RData")

#exclude non-responders aka participants that respond to less than 2/3 of all US, that means 4 or 5 missing
scr <- scr[which(scr$id != '006' & scr$id != '014' & scr$id != '018' & scr$id != '021' & scr$id != '032' 
                 & scr$id != '040' & scr$id != '048' & scr$id != '060' & scr$id != '062' & scr$id != '063' 
                 & scr$id != '067' & scr$id != '072' & scr$id != '084' & scr$id != '087' & scr$id != '094' 
                 & scr$id != '096' & scr$id != '099' & scr$id != '101' & scr$id != '102' & scr$id != '103' 
                 & scr$id != '107' & scr$id != '110' & scr$id != '142' & scr$id != '144' & scr$id != '145' 
                 & scr$id != '147' & scr$id != '150' & scr$id != '155' & scr$id != '159' & scr$id != '172' 
                 & scr$id != '173' & scr$id != '190' & scr$id != '194' & scr$id != '204' & scr$id != '206' 
                 & scr$id != '207' & scr$id != '208' & scr$id != '213' & scr$id != '220' & scr$id != '224' 
                 & scr$id != '227' & scr$id != '231' & scr$id != '235' & scr$id != '238' & scr$id != '239' 
                 & scr$id != '240' & scr$id != '251' & scr$id != '255' & scr$id != '257' & scr$id != '259' 
                 & scr$id != '261' & scr$id != '264' & scr$id != '266 '& scr$id != '268'),]

# create data subset
dataSubset <- scr[which(scr$phase == "ext"),]

# Define n for SE
n <- length(unique(dataSubset$id))  

agg.scr <- with(dataSubset, aggregate(log.rc, by=list(stim=stim, trial=trial, phase=phase), FUN=mean, na.rm=T))
agg.scr$se <- with(dataSubset, aggregate(log.rc, by=list(stim=stim, trial= trial, phase=phase), FUN=se))[ ,4]

# Add confidence interval
agg.scr$lower <- agg.scr$x - 1.96*agg.scr$se
agg.scr$upper <- agg.scr$x + 1.96*agg.scr$se

name_y_lab <- "SCR (log, rc)" 
y_limit <- (round(max(agg.scr$upper), digits=1) + 0.1)
y_break <- 0.05  

# Plot
theme_set(theme_bw())
scr_ext <- ggplot(agg.scr, aes(x = trial, y = x, colour = stim, group = stim))+
  geom_point(aes(group=stim), size=2) +
  geom_line(aes(group=stim), size=1.2) +
  geom_ribbon(aes(ymin=lower, ymax=upper, colour= NA, fill=stim), alpha=0.2) +
  scale_colour_manual(values=c("red2","blue1"), name = "Stimulus:", breaks=c("CSp","CSm"),
                      labels=c("CS+", "CS-")) +
  scale_fill_manual(values=c("red2","blue1"), name = "Stimulus:", breaks=c("CSp","CSm"),
                    labels=c("CS+", "CS-")) +
  xlab("Trial") +
  ylab(name_y_lab) + 
  scale_x_continuous(breaks=c(seq(1,23,2))) +
  scale_y_continuous(breaks=seq(0, 0.6, 0.1), expand=c(0,0)) +
  coord_cartesian(ylim = c(0, 0.6)) + 
  ggtitle("24-Hour Delayed Extinction") +
  
  theme(plot.title = element_text(size=15, face="bold", hjust=0.5, vjust=2),
        axis.text.x = element_text(size=15,  color="black"),
        axis.text.y = element_text(size=15, color="black"), 
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15, margin=margin(0,10,0,0)),
        legend.position = "none",
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

#### Renewal

### Load data
load("scr.RData")

#exclude non-responders aka participants that respond to less than 2/3 of all US, that means 4 or 5 missing
scr <- scr[which(scr$id != '006' & scr$id != '014' & scr$id != '018' & scr$id != '021' & scr$id != '032' 
                 & scr$id != '040' & scr$id != '048' & scr$id != '060' & scr$id != '062' & scr$id != '063' 
                 & scr$id != '067' & scr$id != '072' & scr$id != '084' & scr$id != '087' & scr$id != '094' 
                 & scr$id != '096' & scr$id != '099' & scr$id != '101' & scr$id != '102' & scr$id != '103' 
                 & scr$id != '107' & scr$id != '110' & scr$id != '142' & scr$id != '144' & scr$id != '145' 
                 & scr$id != '147' & scr$id != '150' & scr$id != '155' & scr$id != '159' & scr$id != '172' 
                 & scr$id != '173' & scr$id != '190' & scr$id != '194' & scr$id != '204' & scr$id != '206' 
                 & scr$id != '207' & scr$id != '208' & scr$id != '213' & scr$id != '220' & scr$id != '224' 
                 & scr$id != '227' & scr$id != '231' & scr$id != '235' & scr$id != '238' & scr$id != '239' 
                 & scr$id != '240' & scr$id != '251' & scr$id != '255' & scr$id != '257' & scr$id != '259' 
                 & scr$id != '261' & scr$id != '264' & scr$id != '266 '& scr$id != '268'),]

# create data subset
dataSubset <- scr[which(scr$phase == "ren"),]

# Define n for SE
n <- length(unique(dataSubset$id))  

agg.scr <- with(dataSubset, aggregate(log.rc, by=list(stim=stim, trial=trial, phase=phase), FUN=mean, na.rm=T))
agg.scr$se <- with(dataSubset, aggregate(log.rc, by=list(stim=stim, trial= trial, phase=phase), FUN=se))[ ,4]

# Add confidence interval
agg.scr$lower <- agg.scr$x - 1.96*agg.scr$se
agg.scr$upper <- agg.scr$x + 1.96*agg.scr$se

name_y_lab <- "SCR (log, rc)" 
y_limit <- (round(max(agg.scr$upper), digits=1) + 0.1)
y_break <- 0.05  

# Plot
theme_set(theme_bw())
scr_ren <- ggplot(agg.scr, aes(x = trial, y = x, colour = stim, group = stim))+
  geom_point(aes(group=stim), size=2) +
  geom_line(aes(group=stim), size=1.2) +
  geom_ribbon(aes(ymin=lower, ymax=upper, colour= NA, fill=stim), alpha=0.2) +
  scale_colour_manual(values=c("red2","blue1"), name = "Stimulus:", breaks=c("CSp","CSm"),
                      labels=c("CS+", "CS-")) +
  scale_fill_manual(values=c("red2","blue1"), name = "Stimulus:", breaks=c("CSp","CSm"),
                    labels=c("CS+", "CS-")) +
  xlab("Trial") +
  ylab(name_y_lab) + 
  scale_x_continuous(breaks=c(seq(1,11,2))) +
  scale_y_continuous(breaks=seq(0, 0.6, 0.1), expand=c(0,0)) +
  coord_cartesian(ylim = c(0, 0.6)) + 
  ggtitle("Renewal") +
  
  theme(plot.title = element_text(size=15, face="bold", hjust=0.5, vjust=2),
        axis.text.x = element_text(size=15,  color="black"),
        axis.text.y = element_text(size=15, color="black"), 
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15, margin=margin(0,10,0,0)),
        legend.position = "none",
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 



###############################################################################
########################## Fear Potentiated Startle ###########################
###############################################################################

#### Acquisition

load("fps.RData")

fps <- fps[ fps$id != "024" & fps$id != "042" & fps$id != "043" & fps$id != "069" & fps$id != "081" 
            & fps$id != "085" & fps$id != "110" & fps$id != "124" & fps$id != "128" & fps$id != "134" 
            & fps$id != "136" & fps$id != "141" & fps$id != "144" & fps$id != "145" & fps$id != "152" & fps$id != "156" 
            & fps$id != "159" & fps$id != "161" & fps$id != "162" & fps$id != "163" & fps$id != "173" 
            & fps$id != "175" & fps$id != "176" & fps$id != "178" & fps$id != "181" & fps$id != "183" 
            & fps$id != "184" & fps$id != "186" & fps$id != "242" & fps$id != "220" & fps$id != "246" & fps$id != "249" 
            & fps$id != "254" & fps$id != "255" & fps$id != "257" & fps$id != "268" & fps$id != "270", ]

# create data subset
dataSubset <- fps[which(fps$stim != "iti" & fps$phase == "acq"),]
  
# Define n for SE
n <- length(unique(dataSubset$id))

agg.fps <- with(dataSubset, aggregate(Tscore, by=list(stim=stim, trial=trial, phase=phase), FUN=mean, na.rm=T))
agg.fps$se <- with(dataSubset, aggregate(Tscore, by=list(stim=stim, trial=trial, phase=phase), FUN=se))[ ,4]
  
# Add confidence interval
agg.fps$lower <- agg.fps$x - 1.96*agg.fps$se
agg.fps$upper <- agg.fps$x + 1.96*agg.fps$se
  
name_y_lab <- "Startle Response (T-Score)" 
y_low_limit <- round(min(agg.fps$lower))
y_upp_limit <- round(max(agg.fps$upper))
y_break <- 10
  
# Plot
theme_set(theme_bw())
fps_acq <- ggplot(agg.fps, aes(x=trial, y=x, colour=stim, group=stim))+
  geom_point(aes(group=stim), size=2) +
  geom_line(aes(group=stim), size=1.2) +
  geom_ribbon(aes(ymin=lower, ymax=upper, colour= NA, fill=stim), alpha=0.2) +
  scale_colour_manual(values=c("red2","blue1"), name = "Stimulus:", breaks=c("CSp","CSm"),
                        labels=c("CS+", "CS-")) +
  scale_fill_manual(values=c("red2","blue1"), name = "Stimulus:", breaks=c("CSp","CSm"),
                      labels=c("CS+", "CS-")) +
  xlab("Trial") +
  ylab(name_y_lab) + 
  scale_x_continuous(breaks=seq(1,11,2)) +
  scale_y_continuous(breaks=seq(40,70,5), expand=c(0,0)) +
  coord_cartesian(ylim = c(40,70)) + 
  ggtitle("Full sample") +
  theme(plot.title = element_blank(), plot.subtitle = element_blank(),
        axis.text.x = element_text(size=15,  color="black"),
        axis.text.y = element_text(size=15, color="black"), 
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15, margin=margin(0,10,0,0)),
        legend.position = "none",
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


###### Extinction

load("fps.RData")

fps <- fps[ fps$id != "024" & fps$id != "042" & fps$id != "043" & fps$id != "069" & fps$id != "081" 
            & fps$id != "085" & fps$id != "110" & fps$id != "124" & fps$id != "128" & fps$id != "134" 
            & fps$id != "136" & fps$id != "141" & fps$id != "144" & fps$id != "145" & fps$id != "152" & fps$id != "156" 
            & fps$id != "159" & fps$id != "161" & fps$id != "162" & fps$id != "163" & fps$id != "173" 
            & fps$id != "175" & fps$id != "176" & fps$id != "178" & fps$id != "181" & fps$id != "183" 
            & fps$id != "184" & fps$id != "186" & fps$id != "242" & fps$id != "220" & fps$id != "246" & fps$id != "249" 
            & fps$id != "254" & fps$id != "255" & fps$id != "257" & fps$id != "268" & fps$id != "270", ]

# create data subset
dataSubset <- fps[which(fps$stim != "iti" & fps$phase == "ext"),]

# Define n for SE
n <- length(unique(dataSubset$id))

agg.fps <- with(dataSubset, aggregate(Tscore, by=list(stim=stim, trial=trial, phase=phase), FUN=mean, na.rm=T))
agg.fps$se <- with(dataSubset, aggregate(Tscore, by=list(stim=stim, trial=trial, phase=phase), FUN=se))[ ,4]

# Add confidence interval
agg.fps$lower <- agg.fps$x - 1.96*agg.fps$se
agg.fps$upper <- agg.fps$x + 1.96*agg.fps$se

name_y_lab <- "Startle Response (T-Score)" 
y_low_limit <- round(min(agg.fps$lower))
y_upp_limit <- round(max(agg.fps$upper))
y_break <- 10

# Plot
theme_set(theme_bw())
fps_ext <- ggplot(agg.fps, aes(x=trial, y=x, colour=stim, group=stim))+
  geom_point(aes(group=stim), size=2) +
  geom_line(aes(group=stim), size=1.2) +
  geom_ribbon(aes(ymin=lower, ymax=upper, colour= NA, fill=stim), alpha=0.2) +
  scale_colour_manual(values=c("red2","blue1"), name = "Stimulus:", breaks=c("CSp","CSm"),
                      labels=c("CS+", "CS-")) +
  scale_fill_manual(values=c("red2","blue1"), name = "Stimulus:", breaks=c("CSp","CSm"),
                    labels=c("CS+", "CS-")) +
  xlab("Trial") +
  ylab(name_y_lab) + 
  scale_x_continuous(breaks=seq(1,23,2)) +
  scale_y_continuous(breaks=seq(40,70,5), expand=c(0,0)) +
  coord_cartesian(ylim = c(40,70)) + 
  ggtitle("Full sample") +
  theme(plot.title = element_blank(), plot.subtitle = element_blank(),
        axis.text.x = element_text(size=15,  color="black"),
        axis.text.y = element_text(size=15, color="black"), 
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15, margin=margin(0,10,0,0)),
        legend.position = "none",
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
  

###### Renewal

load("fps.RData")

fps <- fps[ fps$id != "024" & fps$id != "042" & fps$id != "043" & fps$id != "069" & fps$id != "081" 
            & fps$id != "085" & fps$id != "110" & fps$id != "124" & fps$id != "128" & fps$id != "134" 
            & fps$id != "136" & fps$id != "141" & fps$id != "144" & fps$id != "145" & fps$id != "152" & fps$id != "156" 
            & fps$id != "159" & fps$id != "161" & fps$id != "162" & fps$id != "163" & fps$id != "173" 
            & fps$id != "175" & fps$id != "176" & fps$id != "178" & fps$id != "181" & fps$id != "183" 
            & fps$id != "184" & fps$id != "186" & fps$id != "242" & fps$id != "220" & fps$id != "246" & fps$id != "249" 
            & fps$id != "254" & fps$id != "255" & fps$id != "257" & fps$id != "268" & fps$id != "270", ]

# create data subset
dataSubset <- fps[which(fps$stim != "iti" & fps$phase == "ren"),]

# Define n for SE
n <- length(unique(dataSubset$id))

agg.fps <- with(dataSubset, aggregate(Tscore, by=list(stim=stim, trial=trial, phase=phase), FUN=mean, na.rm=T))
agg.fps$se <- with(dataSubset, aggregate(Tscore, by=list(stim=stim, trial=trial, phase=phase), FUN=se))[ ,4]

# Add confidence interval
agg.fps$lower <- agg.fps$x - 1.96*agg.fps$se
agg.fps$upper <- agg.fps$x + 1.96*agg.fps$se

name_y_lab <- "Startle Response (T-Score)" 
y_low_limit <- round(min(agg.fps$lower))
y_upp_limit <- round(max(agg.fps$upper))
y_break <- 10

# Plot
theme_set(theme_bw())
fps_ren <- ggplot(agg.fps, aes(x=trial, y=x, colour=stim, group=stim))+
  geom_point(aes(group=stim), size=2) +
  geom_line(aes(group=stim), size=1.2) +
  geom_ribbon(aes(ymin=lower, ymax=upper, colour= NA, fill=stim), alpha=0.2) +
  scale_colour_manual(values=c("red2","blue1"), name = "Stimulus:", breaks=c("CSp","CSm"),
                      labels=c("CS+", "CS-")) +
  scale_fill_manual(values=c("red2","blue1"), name = "Stimulus:", breaks=c("CSp","CSm"),
                    labels=c("CS+", "CS-")) +
  xlab("Trial") +
  ylab(name_y_lab) + 
  scale_x_continuous(breaks=seq(1,11,2)) +
  scale_y_continuous(breaks=seq(40,70,5), expand=c(0,0)) +
  coord_cartesian(ylim = c(40,70)) + 
  ggtitle("Full sample") +
  theme(plot.title = element_blank(), plot.subtitle = element_blank(),
        axis.text.x = element_text(size=15,  color="black"),
        axis.text.y = element_text(size=15, color="black"), 
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15, margin=margin(0,10,0,0)),
        legend.position = "none",
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

###############################################################################
################################ Expectancy Ratings by Trial ##################
###############################################################################

########## Acquisition

### Load data
load("ratings.RData")

ratings <- ratings[which(ratings$id != '145' & ratings$id != '220'),]

# create data subset
dataSubset <- ratings[which(ratings$phase == "acq"),]

# Define n for SE
n <- length(unique(dataSubset$id)); 

# mean of CS+ and CS- for each trial
agg.rate <- with(dataSubset, aggregate(exp_rating, by=list(stim=stim, trial=trial, phase=phase), FUN=mean, na.rm=T))
agg.rate$se <- with(dataSubset, aggregate(exp_rating, by=list(stim=stim, trial=trial, phase=phase), FUN=se))[ ,4] 
    
# Order corresponding to timeline
agg.rate <- agg.rate[order(agg.rate$trial),]
    
# Add confidence interval
agg.rate$lower <- agg.rate$x - 1.96*agg.rate$se
agg.rate$upper <- agg.rate$x + 1.96*agg.rate$se
    
# Plot
theme_set(theme_bw())
exp_rate_acq <- ggplot(agg.rate, aes(x = trial, y = x, colour = stim, group = stim)) +
  geom_point(aes(group=stim), size=2) +
  geom_line(aes(group=stim), size=1.2) +
  geom_ribbon(aes(ymin=lower, ymax=upper, colour= stim, fill=stim), alpha=0.2) +
  scale_colour_manual(values=c("red2","blue1"), name = "Stimulus:", breaks=c("CSp","CSm"),
                          labels=c("CS+", "CS-")) +
  scale_fill_manual(values=c("red2","blue1"), name = "Stimulus:", breaks=c("CSp","CSm"),
                        labels=c("CS+", "CS-")) +
  xlab("Trial") +
  ylab("Expectancy Rating") + 
  scale_x_continuous(breaks = seq(1,11,2)) +
  scale_y_continuous(breaks=seq(0,100,20), expand=c(0,0)) +
  coord_cartesian(ylim = c(0,100)) +
  ggtitle("Full sample") +

  theme(plot.title = element_blank(), plot.subtitle = element_blank(),
        axis.text.x = element_text(size=15,  color="black"),
        axis.text.y = element_text(size=15, color="black"), 
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15, margin=margin(0,10,0,0)),
        legend.position = "none", 
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


########## Extinction

### Load data
load("ratings.RData")

ratings <- ratings[which(ratings$id != '145' & ratings$id != '220'),]

# create data subset
dataSubset <- ratings[which(ratings$phase == "ext"),]

# Define n for SE
n <- length(unique(dataSubset$id)); 

# mean of CS+ and CS- for each trial
agg.rate <- with(dataSubset, aggregate(exp_rating, by=list(stim=stim, trial=trial, phase=phase), FUN=mean, na.rm=T))
agg.rate$se <- with(dataSubset, aggregate(exp_rating, by=list(stim=stim, trial=trial, phase=phase), FUN=se))[ ,4] 

# Order corresponding to timeline
agg.rate <- agg.rate[order(agg.rate$trial),]

# Add confidence interval
agg.rate$lower <- agg.rate$x - 1.96*agg.rate$se
agg.rate$upper <- agg.rate$x + 1.96*agg.rate$se

# Plot
theme_set(theme_bw())
exp_rate_ext <- ggplot(agg.rate, aes(x = trial, y = x, colour = stim, group = stim)) +
  geom_point(aes(group=stim), size=2) +
  geom_line(aes(group=stim), size=1.2) +
  geom_ribbon(aes(ymin=lower, ymax=upper, colour= stim, fill=stim), alpha=0.2) +
  scale_colour_manual(values=c("red2","blue1"), name = "Stimulus:", breaks=c("CSp","CSm"),
                      labels=c("CS+", "CS-")) +
  scale_fill_manual(values=c("red2","blue1"), name = "Stimulus:", breaks=c("CSp","CSm"),
                    labels=c("CS+", "CS-")) +
  xlab("Trial") +
  ylab("Expectancy Rating") + 
  scale_x_continuous(breaks = seq(1,11,2)) +
  scale_y_continuous(breaks=seq(0,100,20), expand=c(0,0)) +
  coord_cartesian(ylim = c(0,100)) +
  ggtitle("Full sample") +
  
  theme(plot.title = element_blank(), plot.subtitle = element_blank(),
        axis.text.x = element_text(size=15,  color="black"),
        axis.text.y = element_text(size=15, color="black"), 
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15, margin=margin(0,10,0,0)),
        legend.position = "none", 
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


########## Renewal

### Load data
load("ratings.RData")

ratings <- ratings[which(ratings$id != '145' & ratings$id != '220'),]

# create data subset
dataSubset <- ratings[which(ratings$phase == "ren"),]

# Define n for SE
n <- length(unique(dataSubset$id)); 

# mean of CS+ and CS- for each trial
agg.rate <- with(dataSubset, aggregate(exp_rating, by=list(stim=stim, trial=trial, phase=phase), FUN=mean, na.rm=T))
agg.rate$se <- with(dataSubset, aggregate(exp_rating, by=list(stim=stim, trial=trial, phase=phase), FUN=se))[ ,4] 

# Order corresponding to timeline
agg.rate <- agg.rate[order(agg.rate$trial),]

# Add confidence interval
agg.rate$lower <- agg.rate$x - 1.96*agg.rate$se
agg.rate$upper <- agg.rate$x + 1.96*agg.rate$se

# Plot
theme_set(theme_bw())
exp_rate_ren <- ggplot(agg.rate, aes(x = trial, y = x, colour = stim, group = stim)) +
  geom_point(aes(group=stim), size=2) +
  geom_line(aes(group=stim), size=1.2) +
  geom_ribbon(aes(ymin=lower, ymax=upper, colour= stim, fill=stim), alpha=0.2) +
  scale_colour_manual(values=c("red2","blue1"), name = "Stimulus:", breaks=c("CSp","CSm"),
                      labels=c("CS+", "CS-")) +
  scale_fill_manual(values=c("red2","blue1"), name = "Stimulus:", breaks=c("CSp","CSm"),
                    labels=c("CS+", "CS-")) +
  xlab("Trial") +
  ylab("Expectancy Rating") + 
  scale_x_continuous(breaks = seq(1,11,2)) +
  scale_y_continuous(breaks=seq(0,100,20), expand=c(0,0)) +
  coord_cartesian(ylim = c(0,100)) +
  ggtitle("Full sample") +
  
  theme(plot.title = element_blank(), plot.subtitle = element_blank(),
        axis.text.x = element_text(size=15,  color="black"),
        axis.text.y = element_text(size=15, color="black"), 
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15, margin=margin(0,10,0,0)),
        legend.position = "none", 
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

    
  

###############################################################################
################### Fear Ratings pre, post phase  ############################
##############################################################################

##### Acquisition

### Load data
load("ratings.RData")

ratings <- ratings[which(ratings$id != '145' & ratings$id != '220'),]

# Create data subset
dataSubset <- ratings

# Define n for SE
n <- length(unique(dataSubset$id))

# Aggregate pre and post ratings
agg.rate.pre <- with(dataSubset, aggregate(pre_rating, by = list(stim = stim, phase = phase), FUN = mean, na.rm = TRUE))
agg.rate.pre$se <- with(dataSubset, aggregate(pre_rating, by = list(stim = stim, phase = phase), FUN = se))[, 3]
colnames(agg.rate.pre)[1:3] <- c("stim", "phase", "x")
agg.rate.pre$time <- "pre"
agg.rate.pre <- na.omit(agg.rate.pre)

agg.rate.post <- with(dataSubset, aggregate(post_rating, by = list(stim = stim, phase = phase), FUN = mean, na.rm = TRUE))
agg.rate.post$se <- with(dataSubset, aggregate(post_rating, by = list(stim = stim, phase = phase), FUN = se))[, 3]
colnames(agg.rate.post)[1:3] <- c("stim", "phase", "x")
agg.rate.post$time <- "post"
agg.rate.post <- na.omit(agg.rate.post)

# Combine
agg.rate.prepost <- rbind(agg.rate.pre, agg.rate.post)
agg.rate.prepost <- agg.rate.prepost[order(agg.rate.prepost$phase, agg.rate.prepost$stim, agg.rate.prepost$time),]

# Add x-axis variable
agg.rate.prepost$x_axis <- ifelse(agg.rate.prepost$time == "pre", 1, 2)

# Confidence intervals
agg.rate.prepost$lower <- agg.rate.prepost$x - 1.96 * agg.rate.prepost$se
agg.rate.prepost$upper <- agg.rate.prepost$x + 1.96 * agg.rate.prepost$se

# Subset for acquisition phase
rate.acq <- subset(agg.rate.prepost, phase == "acq")

# Plot
theme_set(theme_bw())
fear_rate_acq <- ggplot(rate.acq, aes(x = x_axis, y = x, colour = stim, group = stim)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .1) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = stim), alpha = 0.2, colour = NA) +
  scale_colour_manual(values = c("red2", "blue1"), name = "Stimulus:", breaks = c("CSp", "CSm"), labels = c("CS+", "CS-")) +
  scale_fill_manual(values = c("red2", "blue1"), name = "Stimulus:", breaks = c("CSp", "CSm"), labels = c("CS+", "CS-")) +
  scale_x_continuous("", breaks = c(1, 2), labels = c("Pre", "Post")) +
  ylab("Fear Rating (0 - 100)") +
  scale_y_continuous(breaks = seq(0, 100, 20), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(
    axis.text.x = element_text(size = 15, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title.y = element_text(size = 15, margin = margin(0, 10, 0, 0)),
    legend.position = "none",
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.ticks.x = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


###### Extinction

### Load data
load("ratings.RData")

ratings <- ratings[which(ratings$id != '145' & ratings$id != '220'),]

# Create data subset
dataSubset <- ratings

# Define n for SE
n <- length(unique(dataSubset$id))

# Aggregate pre and post ratings
agg.rate.pre <- with(dataSubset, aggregate(pre_rating, by = list(stim = stim, phase = phase), FUN = mean, na.rm = TRUE))
agg.rate.pre$se <- with(dataSubset, aggregate(pre_rating, by = list(stim = stim, phase = phase), FUN = se))[, 3]
colnames(agg.rate.pre)[1:3] <- c("stim", "phase", "x")
agg.rate.pre$time <- "pre"
agg.rate.pre <- na.omit(agg.rate.pre)

agg.rate.post <- with(dataSubset, aggregate(post_rating, by = list(stim = stim, phase = phase), FUN = mean, na.rm = TRUE))
agg.rate.post$se <- with(dataSubset, aggregate(post_rating, by = list(stim = stim, phase = phase), FUN = se))[, 3]
colnames(agg.rate.post)[1:3] <- c("stim", "phase", "x")
agg.rate.post$time <- "post"
agg.rate.post <- na.omit(agg.rate.post)

# Combine
agg.rate.prepost <- rbind(agg.rate.pre, agg.rate.post)
agg.rate.prepost <- agg.rate.prepost[order(agg.rate.prepost$phase, agg.rate.prepost$stim, agg.rate.prepost$time),]

# Add x-axis variable
agg.rate.prepost$x_axis <- ifelse(agg.rate.prepost$time == "pre", 1, 2)

# Confidence intervals
agg.rate.prepost$lower <- agg.rate.prepost$x - 1.96 * agg.rate.prepost$se
agg.rate.prepost$upper <- agg.rate.prepost$x + 1.96 * agg.rate.prepost$se

# Subset for exinction phase
rate.ext <- subset(agg.rate.prepost, phase == "ext")

# Plot
theme_set(theme_bw())
fear_rate_ext <- ggplot(rate.ext, aes(x = x_axis, y = x, colour = stim, group = stim)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .1) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = stim), alpha = 0.2, colour = NA) +
  scale_colour_manual(values = c("red2", "blue1"), name = "Stimulus:", breaks = c("CSp", "CSm"), labels = c("CS+", "CS-")) +
  scale_fill_manual(values = c("red2", "blue1"), name = "Stimulus:", breaks = c("CSp", "CSm"), labels = c("CS+", "CS-")) +
  scale_x_continuous("", breaks = c(1, 2), labels = c("Pre", "Post")) +
  ylab("Fear Rating (0 - 100)") +
  scale_y_continuous(breaks = seq(0, 100, 20), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(
    axis.text.x = element_text(size = 15, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title.y = element_blank(),
    legend.position = "none",
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.ticks.x = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


###### Renewal

### Load data
load("ratings.RData")

ratings <- ratings[which(ratings$id != '145' & ratings$id != '220'),]

# Create data subset
dataSubset <- ratings

# Define n for SE
n <- length(unique(dataSubset$id))

# Aggregate pre and post ratings
agg.rate.pre <- with(dataSubset, aggregate(pre_rating, by = list(stim = stim, phase = phase), FUN = mean, na.rm = TRUE))
agg.rate.pre$se <- with(dataSubset, aggregate(pre_rating, by = list(stim = stim, phase = phase), FUN = se))[, 3]
colnames(agg.rate.pre)[1:3] <- c("stim", "phase", "x")
agg.rate.pre$time <- "pre"
agg.rate.pre <- na.omit(agg.rate.pre)

agg.rate.post <- with(dataSubset, aggregate(post_rating, by = list(stim = stim, phase = phase), FUN = mean, na.rm = TRUE))
agg.rate.post$se <- with(dataSubset, aggregate(post_rating, by = list(stim = stim, phase = phase), FUN = se))[, 3]
colnames(agg.rate.post)[1:3] <- c("stim", "phase", "x")
agg.rate.post$time <- "post"
agg.rate.post <- na.omit(agg.rate.post)

# Combine
agg.rate.prepost <- rbind(agg.rate.pre, agg.rate.post)
agg.rate.prepost <- agg.rate.prepost[order(agg.rate.prepost$phase, agg.rate.prepost$stim, agg.rate.prepost$time),]

# Add x-axis variable
agg.rate.prepost$x_axis <- ifelse(agg.rate.prepost$time == "pre", 1, 2)

# Confidence intervals
agg.rate.prepost$lower <- agg.rate.prepost$x - 1.96 * agg.rate.prepost$se
agg.rate.prepost$upper <- agg.rate.prepost$x + 1.96 * agg.rate.prepost$se

# Separate out the extinction post rows
ext_post <- subset(agg.rate.prepost, phase == "ext" & time == "post")

# Reassign to renewal/pre
ext_post$phase <- "ren"
ext_post$time  <- "pre"
ext_post$x_axis  <- 1

# Combine back with the rest of the data
agg.rate.prepost <- rbind(agg.rate.prepost, ext_post)


# Subset for exinction phase
rate.ren <- subset(agg.rate.prepost, phase == "ren")

# Plot
theme_set(theme_bw())
fear_rate_ren <- ggplot(rate.ren, aes(x = x_axis, y = x, colour = stim, group = stim)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .1) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = stim), alpha = 0.2, colour = NA) +
  scale_colour_manual(values = c("red2", "blue1"), name = "Stimulus:", breaks = c("CSp", "CSm"), labels = c("CS+", "CS-")) +
  scale_fill_manual(values = c("red2", "blue1"), name = "Stimulus:", breaks = c("CSp", "CSm"), labels = c("CS+", "CS-")) +
  scale_x_continuous("", breaks = c(1, 2), labels = c("Pre", "Post")) +
  ylab("Fear Rating (0 - 100)") +
  scale_y_continuous(breaks = seq(0, 100, 20), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(
    axis.text.x = element_text(size = 15, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title.y = element_blank(),
    legend.position = c(0.85, 0.9),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.ticks.x = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


###############################################################################
######################## Combine all plots ####################################
###############################################################################

# Different margins for the three columns
# First column needs extra space on the left for y-axis titles
margin_left <- theme(
  plot.margin = margin(
    t = 5.5,
    r = 2,
    b = 5.5,
    l = 12
  )
)

# Middle column: very little horizontal margin
margin_middle <- theme(
  plot.margin = margin(
    t = 5.5,
    r = 2,
    b = 5.5,
    l = 2
  )
)

# Right column
margin_right <- theme(
  plot.margin = margin(
    t = 5.5,
    r = 5.5,
    b = 5.5,
    l = 2
  )
)

# Apply margins 

# Column 1
scr_acq      <- scr_acq      + margin_left
fps_acq      <- fps_acq      + margin_left
exp_rate_acq <- exp_rate_acq + margin_left
fear_rate_acq <- fear_rate_acq + margin_left

# Column 2
scr_ext      <- scr_ext      + margin_middle
fps_ext      <- fps_ext      + margin_middle
exp_rate_ext <- exp_rate_ext + margin_middle
fear_rate_ext <- fear_rate_ext + margin_middle

# Column 3
scr_ren      <- scr_ren      + margin_right
fps_ren      <- fps_ren      + margin_right
exp_rate_ren <- exp_rate_ren + margin_right
fear_rate_ren <- fear_rate_ren + margin_right


# Combine plots
exp_mani_fig <- cowplot::plot_grid(
  scr_acq, scr_ext, scr_ren,
  fps_acq, fps_ext, fps_ren,
  exp_rate_acq, exp_rate_ext, exp_rate_ren,
  fear_rate_acq, fear_rate_ext, fear_rate_ren,
  
  ncol = 3,
  
  # Align plotting regions and axes
  align = "hv",
  axis = "tblr",
  
  # Preserve relative duration of phases
  rel_widths = c(11, 23, 11),
  
  labels = c(
    "A", "B", "C",
    "D", "E", "F",
    "G", "H", "I",
    "J", "K", "L"
  ),
  
  label_x = 0
)

exp_mani_fig


# Save

ggsave(
  filename = "exp_mani_fig_pub.png",
  plot = exp_mani_fig,
  width = 12,
  height = 12,
  units = "in",
  dpi = 600
)

