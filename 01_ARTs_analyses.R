# ART Project - main analyses of associations between ARTs (combined and separate) and different indices of 
# fear conditioning (SCR, FPS, Expectany and Fear Ratings)
#
# Last updated: 25/08/2026 - Mana Ehlers


rm(list = ls())

### Load packages
library(cowplot)
library(ggplot2)
library(ggcorrplot)
library(gridExtra)
library(tidyr)
library(dplyr)
library(ggpubr)
library(ggpmisc)
library(rstatix)

options(scipen = 999)

############################### Questionnaires ################################

### Overview questionnaires
### Load data
load("questionnaires.RData")

######################### Data Prep ########################

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

# Set up a 1 row, 4 columns layout
par(mfrow = c(1, 4),
    mar = c(4, 5, 2, 1))

# -----------------------------
# A: STAI-T
# -----------------------------

p1 <- ggplot(questionnaires, aes(x = stai_trait)) +
  geom_histogram(
    binwidth = 1,
    boundary = 20,
    fill = "#1B9E77",
    colour = "black",
    linewidth = 0.3
  ) +
  scale_x_continuous(
    limits = c(20, 80),
    breaks = seq(20, 80, by = 20)
  ) +
  scale_y_continuous(
    limits = c(0, 16),
    breaks = seq(0, 16, by = 4),
    expand = c(0, 0)
  ) +
  labs(
    x = "STAI-T",
    y = "Frequency"
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.title.x = element_text(
      face = "bold",
      size = 16,
      margin = margin(t = -40)
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 16
    ),
    axis.text = element_text(face = "bold", size = 16)
  )

# -----------------------------
# B: NEO-FFI-N
# -----------------------------

p2 <- ggplot(questionnaires, aes(x = neo_ffi_n)) +
  geom_histogram(
    binwidth = 1,
    boundary = 0,
    fill = "#D95F02",
    colour = "black",
    linewidth = 0.3
  ) +
  scale_x_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, by = 10)
  ) +
  scale_y_continuous(
    limits = c(0, 16),
    breaks = seq(0, 16, by = 4),
    expand = c(0, 0)
  ) +
  labs(
    x = "NEO-FFI-N",
    y = "Frequency"
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.title.x = element_text(
      face = "bold",
      size = 16,
      margin = margin(t = -40)
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 16
    ),
    axis.text = element_text(face = "bold", size = 16)
  )


# -----------------------------
# C: IUS
# -----------------------------

p3 <- ggplot(questionnaires, aes(x = ius)) +
  geom_histogram(
    binwidth = 2,
    boundary = 20,
    fill = "#7570B3",
    colour = "black",
    linewidth = 0.3
  ) +
  scale_x_continuous(
    limits = c(20, 140),
    breaks = seq(20, 140, by = 20)
  ) +
  scale_y_continuous(
    limits = c(0, 16),
    breaks = seq(0, 16, by = 4),
    expand = c(0, 0)
  ) +
  labs(
    x = "IUS",
    y = "Frequency"
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.title.x = element_text(
      face = "bold",
      size = 16,
      margin = margin(t = -40)
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 16
    ),
    axis.text = element_text(face = "bold", size = 16)
  )


# -----------------------------
# D: Correlations
# -----------------------------
p4 <- ggcorrplot(
  cor_matrix,
  type = "lower",
  hc.order = FALSE,
  show.diag = TRUE,
  lab = TRUE,
  lab_size = 5,
  colors = c("#2166AC", "white", "#B2182B"),
  outline.col = "white"
) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      face = "bold",
      size = 16,
      colour = "black"
    ),
    axis.text.y = element_text(
      face = "bold",
      size = 16,
      colour = "black"
    ),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    panel.grid = element_blank()
  )

# -----------------------------
# Combine into one row
# -----------------------------

Figure2 <- p1 + p2 + p3 + p4 +
  plot_layout(
    nrow = 1,
    widths = c(1, 1, 1, 1)
  ) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    plot.tag = element_text(face = "bold", size = 16)
  )

Figure2


# -----------------------------
# Save at 600 dpi
# -----------------------------

ggsave(
  filename = "Figure2.png",
  plot = Figure2,
  width = 18,
  height = 6,
  units = "in",
  dpi = 600
)


#####################################################################################################################
########################################## Individual Differences ###################################################
#####################################################################################################################

#############################################################
################### SCR log rc ##############################
#############################################################

load("scr.RData")

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

########create data subsets: US
dataSubset <- scr[which(scr$stim == "US"),]
agg.scr.us <- with(dataSubset, aggregate(log.rc, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#rename columns
names(agg.scr.us)[names(agg.scr.us) == "x"] <- "scr_us"

dataSubset_scr_us <- merge(questionnaires, agg.scr.us,  by="id")

#########create data subsets: EXT
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

#########create data subsets: REN
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


################### Correlations combined score and SCR

######## Acquisition
tests <- list(
cor.test(dataSubset_scr_acq$combined_z, dataSubset_scr_acq$scr_acq_CSp, method=c("pearson")), 
cor.test(dataSubset_scr_acq$combined_z, dataSubset_scr_acq$scr_acq_CSm, method=c("pearson")), 
cor.test(dataSubset_scr_acq$combined_z, dataSubset_scr_acq$scr_acq_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)


######## US
cor.test(dataSubset_scr_us$combined_z, dataSubset_scr_us$scr_us, method=c("pearson"))

######## Extinction
tests <- list(
cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")), 
cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)


######## Renewal
tests <- list(
cor.test(dataSubset_scr_ren$combined_z, dataSubset_scr_ren$scr_ren_CSp, method=c("pearson")), 
cor.test(dataSubset_scr_ren$combined_z, dataSubset_scr_ren$scr_ren_CSm, method=c("pearson")), 
cor.test(dataSubset_scr_ren$combined_z, dataSubset_scr_ren$scr_ren_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)


################### Correlations and SCR
######## Acquisition
tests <- list(
  cor.test(dataSubset_scr_acq$stai_trait, dataSubset_scr_acq$scr_acq_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_acq$stai_trait, dataSubset_scr_acq$scr_acq_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_acq$stai_trait, dataSubset_scr_acq$scr_acq_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_scr_acq$neo_ffi_n, dataSubset_scr_acq$scr_acq_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_acq$neo_ffi_n, dataSubset_scr_acq$scr_acq_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_acq$neo_ffi_n, dataSubset_scr_acq$scr_acq_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_scr_acq$ius, dataSubset_scr_acq$scr_acq_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_acq$ius, dataSubset_scr_acq$scr_acq_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_acq$ius, dataSubset_scr_acq$scr_acq_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)


######## Extinction
tests <- list(
  cor.test(dataSubset_scr_ext$stai_trait, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$stai_trait, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$stai_trait, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_scr_ext$neo_ffi_n, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$neo_ffi_n, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$neo_ffi_n, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")),
  cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Renewal
tests <- list(
  cor.test(dataSubset_scr_ren$stai_trait, dataSubset_scr_ren$scr_ren_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_ren$stai_trait, dataSubset_scr_ren$scr_ren_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ren$stai_trait, dataSubset_scr_ren$scr_ren_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_scr_ren$neo_ffi_n, dataSubset_scr_ren$scr_ren_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_ren$neo_ffi_n, dataSubset_scr_ren$scr_ren_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ren$neo_ffi_n, dataSubset_scr_ren$scr_ren_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_scr_ren$ius, dataSubset_scr_ren$scr_ren_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_ren$ius, dataSubset_scr_ren$scr_ren_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ren$ius, dataSubset_scr_ren$scr_ren_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

################### SCR log rc  extinction first half ##############################

load("scr.RData")

#exclude non-responders aka participants that respond to less than 2/3 of all US, that means 4 or 5 missing
scr <- scr[which(scr$id != '006' & scr$id != '014' & scr$id != '018' & scr$id != '021' & scr$id != '032' & scr$id != '040' & scr$id != '048' & scr$id != '060' & scr$id != '062' & scr$id != '063' & scr$id != '067' & scr$id != '072' & scr$id != '084' & scr$id != '087' & scr$id != '094' & scr$id != '096' & scr$id != '099' & scr$id != '101' & scr$id != '102' & scr$id != '103' & scr$id != '107' & scr$id != '110' & scr$id != '142' & scr$id != '144' & scr$id != '145' & scr$id!= '147' & scr$id != '150' & scr$id != '155' & scr$id != '159' & scr$id != '172' & scr$id != '173' & scr$id != '190' & scr$id != '194' & scr$id != '204' & scr$id != '206' & scr$id != '207' & scr$id != '208' & scr$id != '213' & scr$id != '220' & scr$id != '224' & scr$id != '227' & scr$id != '231' & scr$id != '235' & scr$id != '238' & scr$id != '239'&  scr$id != '240' & scr$id != '251' & scr$id != '255' & scr$id != '257' & scr$id != '259' & scr$id != '261' & scr$id != '264' & scr$id != '266 '& scr$id != '268'),]

#########create data subsets: EXT
dataSubset <- scr[which(scr$phase == "ext" & scr$half == 1),]
agg.scr.ext <- with(dataSubset, aggregate(log.rc, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.scr.ext.wide <- reshape(agg.scr.ext, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.scr.ext.wide$scr_ext_CSdiff <- agg.scr.ext.wide$x.CSp-agg.scr.ext.wide$x.CSm
#rename columns
names(agg.scr.ext.wide)[names(agg.scr.ext.wide) == "x.CSm"] <- "scr_ext_CSm"
names(agg.scr.ext.wide)[names(agg.scr.ext.wide) == "x.CSp"] <- "scr_ext_CSp"

dataSubset_scr_ext <- merge(questionnaires, agg.scr.ext.wide,  by="id")


################### Correlations combined score and SCR

######## Extinction
tests <- list(
cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")), 
cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Extinction
tests <- list(
  cor.test(dataSubset_scr_ext$stai_trait, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$stai_trait, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$stai_trait, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_scr_ext$neo_ffi_n, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$neo_ffi_n, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$neo_ffi_n, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")),
  cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)


################### SCR log rc  extinction second half ##############################

load("scr.RData")

#exclude non-responders aka participants that respond to less than 2/3 of all US, that means 4 or 5 missing
scr <- scr[which(scr$id != '006' & scr$id != '014' & scr$id != '018' & scr$id != '021' & scr$id != '032' & scr$id != '040' & scr$id != '048' & scr$id != '060' & scr$id != '062' & scr$id != '063' & scr$id != '067' & scr$id != '072' & scr$id != '084' & scr$id != '087' & scr$id != '094' & scr$id != '096' & scr$id != '099' & scr$id != '101' & scr$id != '102' & scr$id != '103' & scr$id != '107' & scr$id != '110' & scr$id != '142' & scr$id != '144' & scr$id != '145' & scr$id!= '147' & scr$id != '150' & scr$id != '155' & scr$id != '159' & scr$id != '172' & scr$id != '173' & scr$id != '190' & scr$id != '194' & scr$id != '204' & scr$id != '206' & scr$id != '207' & scr$id != '208' & scr$id != '213' & scr$id != '220' & scr$id != '224' & scr$id != '227' & scr$id != '231' & scr$id != '235' & scr$id != '238' & scr$id != '239'&  scr$id != '240' & scr$id != '251' & scr$id != '255' & scr$id != '257' & scr$id != '259' & scr$id != '261' & scr$id != '264' & scr$id != '266 '& scr$id != '268'),]

#########create data subsets: EXT
dataSubset <- scr[which(scr$phase == "ext" & scr$half == 2),]
agg.scr.ext <- with(dataSubset, aggregate(log.rc, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.scr.ext.wide <- reshape(agg.scr.ext, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.scr.ext.wide$scr_ext_CSdiff <- agg.scr.ext.wide$x.CSp-agg.scr.ext.wide$x.CSm
#rename columns
names(agg.scr.ext.wide)[names(agg.scr.ext.wide) == "x.CSm"] <- "scr_ext_CSm"
names(agg.scr.ext.wide)[names(agg.scr.ext.wide) == "x.CSp"] <- "scr_ext_CSp"

dataSubset_scr_ext <- merge(questionnaires, agg.scr.ext.wide,  by="id")


################### Correlations combined score and SCR

######## Extinction
tests <- list(
cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")), 
cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Extinction
tests <- list(
  cor.test(dataSubset_scr_ext$stai_trait, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$stai_trait, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$stai_trait, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_scr_ext$neo_ffi_n, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$neo_ffi_n, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$neo_ffi_n, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")),
  cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)




#############################################################
################### SCR sqrt z-scored #######################
#############################################################

load("scr.RData")

#exclude non-responders aka participants that respond to less than 2/3 of all US, that means 4 or 5 missing
scr <- scr[which(scr$id != '006' & scr$id != '014' & scr$id != '018' & scr$id != '021' & scr$id != '032' & scr$id != '040' & scr$id != '048' & scr$id != '060' & scr$id != '062' & scr$id != '063' & scr$id != '067' & scr$id != '072' & scr$id != '084' & scr$id != '087' & scr$id != '094' & scr$id != '096' & scr$id != '099' & scr$id != '101' & scr$id != '102' & scr$id != '103' & scr$id != '107' & scr$id != '110' & scr$id != '142' & scr$id != '144' & scr$id != '145' & scr$id!= '147' & scr$id != '150' & scr$id != '155' & scr$id != '159' & scr$id != '172' & scr$id != '173' & scr$id != '190' & scr$id != '194' & scr$id != '204' & scr$id != '206' & scr$id != '207' & scr$id != '208' & scr$id != '213' & scr$id != '220' & scr$id != '224' & scr$id != '227' & scr$id != '231' & scr$id != '235' & scr$id != '238' & scr$id != '239'&  scr$id != '240' & scr$id != '251' & scr$id != '255' & scr$id != '257' & scr$id != '259' & scr$id != '261' & scr$id != '264' & scr$id != '266 '& scr$id != '268'),]

########create data subsets: ACQ
dataSubset <- scr[which(scr$phase == "acq"),]
agg.scr.acq <- with(dataSubset, aggregate(sqrt_z, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.scr.acq.wide <- reshape(agg.scr.acq, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.scr.acq.wide$scr_acq_CSdiff <- agg.scr.acq.wide$x.CSp-agg.scr.acq.wide$x.CSm
#rename columns
names(agg.scr.acq.wide)[names(agg.scr.acq.wide) == "x.CSm"] <- "scr_acq_CSm"
names(agg.scr.acq.wide)[names(agg.scr.acq.wide) == "x.CSp"] <- "scr_acq_CSp"

dataSubset_scr_acq <- merge(questionnaires, agg.scr.acq.wide,  by="id")

#########create data subsets: EXT
dataSubset <- scr[which(scr$phase == "ext"),]
agg.scr.ext <- with(dataSubset, aggregate(sqrt_z, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.scr.ext.wide <- reshape(agg.scr.ext, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.scr.ext.wide$scr_ext_CSdiff <- agg.scr.ext.wide$x.CSp-agg.scr.ext.wide$x.CSm
#rename columns
names(agg.scr.ext.wide)[names(agg.scr.ext.wide) == "x.CSm"] <- "scr_ext_CSm"
names(agg.scr.ext.wide)[names(agg.scr.ext.wide) == "x.CSp"] <- "scr_ext_CSp"

dataSubset_scr_ext <- merge(questionnaires, agg.scr.ext.wide,  by="id")

#########create data subsets: REN
dataSubset <- scr[which(scr$phase == "ren"),]
agg.scr.ren <- with(dataSubset, aggregate(sqrt_z, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.scr.ren.wide <- reshape(agg.scr.ren, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.scr.ren.wide$scr_ren_CSdiff <- agg.scr.ren.wide$x.CSp-agg.scr.ren.wide$x.CSm
#rename columns
names(agg.scr.ren.wide)[names(agg.scr.ren.wide) == "x.CSm"] <- "scr_ren_CSm"
names(agg.scr.ren.wide)[names(agg.scr.ren.wide) == "x.CSp"] <- "scr_ren_CSp"

dataSubset_scr_ren <- merge(questionnaires, agg.scr.ren.wide,  by="id")


################### Correlations combined score and SCR
######## Acquisition
tests <- list(
cor.test(dataSubset_scr_acq$combined_z, dataSubset_scr_acq$scr_acq_CSp, method=c("pearson")),
cor.test(dataSubset_scr_acq$combined_z, dataSubset_scr_acq$scr_acq_CSm, method=c("pearson")),
cor.test(dataSubset_scr_acq$combined_z, dataSubset_scr_acq$scr_acq_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)


######## Extinction
tests <- list(
cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")), 
cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson"))
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Renewal
tests <- list(
cor.test(dataSubset_scr_ren$combined_z, dataSubset_scr_ren$scr_ren_CSp, method=c("pearson")),
cor.test(dataSubset_scr_ren$combined_z, dataSubset_scr_ren$scr_ren_CSm, method=c("pearson")), 
cor.test(dataSubset_scr_ren$combined_z, dataSubset_scr_ren$scr_ren_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

################### Correlations individual ARTs and SCR
######## Acquisition
cor.test(dataSubset_scr_acq$ius, dataSubset_scr_acq$scr_acq_CSp, method=c("pearson")) 
cor.test(dataSubset_scr_acq$ius, dataSubset_scr_acq$scr_acq_CSm, method=c("pearson")) 
cor.test(dataSubset_scr_acq$ius, dataSubset_scr_acq$scr_acq_CSdiff, method=c("pearson")) 

######## Extinction
cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson"))
cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")) 
cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 

######## Renewal
cor.test(dataSubset_scr_ren$ius, dataSubset_scr_ren$scr_ren_CSp, method=c("pearson"))
cor.test(dataSubset_scr_ren$ius, dataSubset_scr_ren$scr_ren_CSm, method=c("pearson")) 
cor.test(dataSubset_scr_ren$ius, dataSubset_scr_ren$scr_ren_CSdiff, method=c("pearson")) 


################### SCR sqrt, z  extinction first half ##############################

load("scr.RData")

#exclude non-responders aka participants that respond to less than 2/3 of all US, that means 4 or 5 missing
scr <- scr[which(scr$id != '006' & scr$id != '014' & scr$id != '018' & scr$id != '021' & scr$id != '032' & scr$id != '040' & scr$id != '048' & scr$id != '060' & scr$id != '062' & scr$id != '063' & scr$id != '067' & scr$id != '072' & scr$id != '084' & scr$id != '087' & scr$id != '094' & scr$id != '096' & scr$id != '099' & scr$id != '101' & scr$id != '102' & scr$id != '103' & scr$id != '107' & scr$id != '110' & scr$id != '142' & scr$id != '144' & scr$id != '145' & scr$id!= '147' & scr$id != '150' & scr$id != '155' & scr$id != '159' & scr$id != '172' & scr$id != '173' & scr$id != '190' & scr$id != '194' & scr$id != '204' & scr$id != '206' & scr$id != '207' & scr$id != '208' & scr$id != '213' & scr$id != '220' & scr$id != '224' & scr$id != '227' & scr$id != '231' & scr$id != '235' & scr$id != '238' & scr$id != '239'&  scr$id != '240' & scr$id != '251' & scr$id != '255' & scr$id != '257' & scr$id != '259' & scr$id != '261' & scr$id != '264' & scr$id != '266 '& scr$id != '268'),]

#########create data subsets: EXT
dataSubset <- scr[which(scr$phase == "ext" & scr$half == 1),]
agg.scr.ext <- with(dataSubset, aggregate(sqrt_z, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.scr.ext.wide <- reshape(agg.scr.ext, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.scr.ext.wide$scr_ext_CSdiff <- agg.scr.ext.wide$x.CSp-agg.scr.ext.wide$x.CSm
#rename columns
names(agg.scr.ext.wide)[names(agg.scr.ext.wide) == "x.CSm"] <- "scr_ext_CSm"
names(agg.scr.ext.wide)[names(agg.scr.ext.wide) == "x.CSp"] <- "scr_ext_CSp"

dataSubset_scr_ext <- merge(questionnaires, agg.scr.ext.wide,  by="id")


################### Correlations combined score and SCR

######## Extinction
tests <- list(
  cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

cor.test(dataSubset_scr_ext$stai_trait, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson"))
cor.test(dataSubset_scr_ext$neo_ffi_n, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson"))
cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson"))

cor.test(dataSubset_scr_ext$stai_trait, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson"))
cor.test(dataSubset_scr_ext$neo_ffi_n, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson"))
cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson"))

cor.test(dataSubset_scr_ext$stai_trait, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson"))
cor.test(dataSubset_scr_ext$neo_ffi_n, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson"))
cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson"))


################### SCR sqrt, z  extinction second half ##############################

load("scr.RData")

#exclude non-responders aka participants that respond to less than 2/3 of all US, that means 4 or 5 missing
scr <- scr[which(scr$id != '006' & scr$id != '014' & scr$id != '018' & scr$id != '021' & scr$id != '032' & scr$id != '040' & scr$id != '048' & scr$id != '060' & scr$id != '062' & scr$id != '063' & scr$id != '067' & scr$id != '072' & scr$id != '084' & scr$id != '087' & scr$id != '094' & scr$id != '096' & scr$id != '099' & scr$id != '101' & scr$id != '102' & scr$id != '103' & scr$id != '107' & scr$id != '110' & scr$id != '142' & scr$id != '144' & scr$id != '145' & scr$id!= '147' & scr$id != '150' & scr$id != '155' & scr$id != '159' & scr$id != '172' & scr$id != '173' & scr$id != '190' & scr$id != '194' & scr$id != '204' & scr$id != '206' & scr$id != '207' & scr$id != '208' & scr$id != '213' & scr$id != '220' & scr$id != '224' & scr$id != '227' & scr$id != '231' & scr$id != '235' & scr$id != '238' & scr$id != '239'&  scr$id != '240' & scr$id != '251' & scr$id != '255' & scr$id != '257' & scr$id != '259' & scr$id != '261' & scr$id != '264' & scr$id != '266 '& scr$id != '268'),]

#########create data subsets: EXT
dataSubset <- scr[which(scr$phase == "ext" & scr$half == 2),]
agg.scr.ext <- with(dataSubset, aggregate(sqrt_z, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.scr.ext.wide <- reshape(agg.scr.ext, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.scr.ext.wide$scr_ext_CSdiff <- agg.scr.ext.wide$x.CSp-agg.scr.ext.wide$x.CSm
#rename columns
names(agg.scr.ext.wide)[names(agg.scr.ext.wide) == "x.CSm"] <- "scr_ext_CSm"
names(agg.scr.ext.wide)[names(agg.scr.ext.wide) == "x.CSp"] <- "scr_ext_CSp"

dataSubset_scr_ext <- merge(questionnaires, agg.scr.ext.wide,  by="id")


################### Correlations combined score and SCR

######## Extinction
tests <- list(
  cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")),
  cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)



#############################################################
####################### SCR RAW #############################
#############################################################

load("scr.RData")

#exclude non-responders aka participants that respond to less than 2/3 of all US, that means 4 or 5 missing
scr <- scr[which(scr$id != '006' & scr$id != '014' & scr$id != '018' & scr$id != '021' & scr$id != '032' & scr$id != '040' & scr$id != '048' & scr$id != '060' & scr$id != '062' & scr$id != '063' & scr$id != '067' & scr$id != '072' & scr$id != '084' & scr$id != '087' & scr$id != '094' & scr$id != '096' & scr$id != '099' & scr$id != '101' & scr$id != '102' & scr$id != '103' & scr$id != '107' & scr$id != '110' & scr$id != '142' & scr$id != '144' & scr$id != '145' & scr$id!= '147' & scr$id != '150' & scr$id != '155' & scr$id != '159' & scr$id != '172' & scr$id != '173' & scr$id != '190' & scr$id != '194' & scr$id != '204' & scr$id != '206' & scr$id != '207' & scr$id != '208' & scr$id != '213' & scr$id != '220' & scr$id != '224' & scr$id != '227' & scr$id != '231' & scr$id != '235' & scr$id != '238' & scr$id != '239'&  scr$id != '240' & scr$id != '251' & scr$id != '255' & scr$id != '257' & scr$id != '259' & scr$id != '261' & scr$id != '264' & scr$id != '266 '& scr$id != '268'),]

########create data subsets: ACQ
dataSubset <- scr[which(scr$phase == "acq"),]
agg.scr.acq <- with(dataSubset, aggregate(raw, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.scr.acq.wide <- reshape(agg.scr.acq, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.scr.acq.wide$scr_acq_CSdiff <- agg.scr.acq.wide$x.CSp-agg.scr.acq.wide$x.CSm
#rename columns
names(agg.scr.acq.wide)[names(agg.scr.acq.wide) == "x.CSm"] <- "scr_acq_CSm"
names(agg.scr.acq.wide)[names(agg.scr.acq.wide) == "x.CSp"] <- "scr_acq_CSp"

dataSubset_scr_acq <- merge(questionnaires, agg.scr.acq.wide,  by="id")

#########create data subsets: EXT
dataSubset <- scr[which(scr$phase == "ext"),]
agg.scr.ext <- with(dataSubset, aggregate(raw, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.scr.ext.wide <- reshape(agg.scr.ext, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.scr.ext.wide$scr_ext_CSdiff <- agg.scr.ext.wide$x.CSp-agg.scr.ext.wide$x.CSm
#rename columns
names(agg.scr.ext.wide)[names(agg.scr.ext.wide) == "x.CSm"] <- "scr_ext_CSm"
names(agg.scr.ext.wide)[names(agg.scr.ext.wide) == "x.CSp"] <- "scr_ext_CSp"

dataSubset_scr_ext <- merge(questionnaires, agg.scr.ext.wide,  by="id")

#########create data subsets: REN
dataSubset <- scr[which(scr$phase == "ren"),]
agg.scr.ren <- with(dataSubset, aggregate(raw, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.scr.ren.wide <- reshape(agg.scr.ren, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.scr.ren.wide$scr_ren_CSdiff <- agg.scr.ren.wide$x.CSp-agg.scr.ren.wide$x.CSm
#rename columns
names(agg.scr.ren.wide)[names(agg.scr.ren.wide) == "x.CSm"] <- "scr_ren_CSm"
names(agg.scr.ren.wide)[names(agg.scr.ren.wide) == "x.CSp"] <- "scr_ren_CSp"

dataSubset_scr_ren <- merge(questionnaires, agg.scr.ren.wide,  by="id")


################### Correlations combined score and SCR
######## Acquisition
tests <- list(
cor.test(dataSubset_scr_acq$combined_z, dataSubset_scr_acq$scr_acq_CSp, method=c("pearson")), 
cor.test(dataSubset_scr_acq$combined_z, dataSubset_scr_acq$scr_acq_CSm, method=c("pearson")),
cor.test(dataSubset_scr_acq$combined_z, dataSubset_scr_acq$scr_acq_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Extinction
tests <- list(
cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")), 
cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Renewal
tests <- list(
cor.test(dataSubset_scr_ren$combined_z, dataSubset_scr_ren$scr_ren_CSp, method=c("pearson")),
cor.test(dataSubset_scr_ren$combined_z, dataSubset_scr_ren$scr_ren_CSm, method=c("pearson")), 
cor.test(dataSubset_scr_ren$combined_z, dataSubset_scr_ren$scr_ren_CSdiff, method=c("pearson"))
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Acquisition
tests <- list(
  cor.test(dataSubset_scr_acq$stai_trait, dataSubset_scr_acq$scr_acq_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_acq$stai_trait, dataSubset_scr_acq$scr_acq_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_acq$stai_trait, dataSubset_scr_acq$scr_acq_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_scr_acq$neo_ffi_n, dataSubset_scr_acq$scr_acq_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_acq$neo_ffi_n, dataSubset_scr_acq$scr_acq_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_acq$neo_ffi_n, dataSubset_scr_acq$scr_acq_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_scr_acq$ius, dataSubset_scr_acq$scr_acq_CSp, method=c("pearson")),
  cor.test(dataSubset_scr_acq$ius, dataSubset_scr_acq$scr_acq_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_acq$ius, dataSubset_scr_acq$scr_acq_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Extinction
tests <- list(
  cor.test(dataSubset_scr_ext$stai_trait, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$stai_trait, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$stai_trait, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_scr_ext$neo_ffi_n, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$neo_ffi_n, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$neo_ffi_n, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Renewal
tests <- list(
  cor.test(dataSubset_scr_ren$stai_trait, dataSubset_scr_ren$scr_ren_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_ren$stai_trait, dataSubset_scr_ren$scr_ren_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ren$stai_trait, dataSubset_scr_ren$scr_ren_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_scr_ren$neo_ffi_n, dataSubset_scr_ren$scr_ren_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_ren$neo_ffi_n, dataSubset_scr_ren$scr_ren_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ren$neo_ffi_n, dataSubset_scr_ren$scr_ren_CSdiff, method=c("pearson"))
)

tests <- list(
  cor.test(dataSubset_scr_ren$ius, dataSubset_scr_ren$scr_ren_CSp, method=c("pearson")),
  cor.test(dataSubset_scr_ren$ius, dataSubset_scr_ren$scr_ren_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ren$ius, dataSubset_scr_ren$scr_ren_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

#############################################################
####################### SCR RAW first half ##################
#############################################################

load("scr.RData")

#exclude non-responders aka participants that respond to less than 2/3 of all US, that means 4 or 5 missing
scr <- scr[which(scr$id != '006' & scr$id != '014' & scr$id != '018' & scr$id != '021' & scr$id != '032' & scr$id != '040' & scr$id != '048' & scr$id != '060' & scr$id != '062' & scr$id != '063' & scr$id != '067' & scr$id != '072' & scr$id != '084' & scr$id != '087' & scr$id != '094' & scr$id != '096' & scr$id != '099' & scr$id != '101' & scr$id != '102' & scr$id != '103' & scr$id != '107' & scr$id != '110' & scr$id != '142' & scr$id != '144' & scr$id != '145' & scr$id!= '147' & scr$id != '150' & scr$id != '155' & scr$id != '159' & scr$id != '172' & scr$id != '173' & scr$id != '190' & scr$id != '194' & scr$id != '204' & scr$id != '206' & scr$id != '207' & scr$id != '208' & scr$id != '213' & scr$id != '220' & scr$id != '224' & scr$id != '227' & scr$id != '231' & scr$id != '235' & scr$id != '238' & scr$id != '239'&  scr$id != '240' & scr$id != '251' & scr$id != '255' & scr$id != '257' & scr$id != '259' & scr$id != '261' & scr$id != '264' & scr$id != '266 '& scr$id != '268'),]

#########create data subsets: EXT
dataSubset <- scr[which(scr$phase == "ext" & scr$half == 1),]
agg.scr.ext <- with(dataSubset, aggregate(raw, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.scr.ext.wide <- reshape(agg.scr.ext, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.scr.ext.wide$scr_ext_CSdiff <- agg.scr.ext.wide$x.CSp-agg.scr.ext.wide$x.CSm
#rename columns
names(agg.scr.ext.wide)[names(agg.scr.ext.wide) == "x.CSm"] <- "scr_ext_CSm"
names(agg.scr.ext.wide)[names(agg.scr.ext.wide) == "x.CSp"] <- "scr_ext_CSp"

dataSubset_scr_ext <- merge(questionnaires, agg.scr.ext.wide,  by="id")


################### Correlations combined score and SCR

######## Extinction
tests <- list(
  cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")),
  cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")),
  cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson"))
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Extinction
tests <- list(
  cor.test(dataSubset_scr_ext$stai_trait, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")),
  cor.test(dataSubset_scr_ext$stai_trait, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$stai_trait, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_scr_ext$neo_ffi_n, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$neo_ffi_n, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$neo_ffi_n, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)


############################################################
####################### SCR RAW second half ################
#############################################################

load("scr.RData")

#exclude non-responders aka participants that respond to less than 2/3 of all US, that means 4 or 5 missing
scr <- scr[which(scr$id != '006' & scr$id != '014' & scr$id != '018' & scr$id != '021' & scr$id != '032' & scr$id != '040' & scr$id != '048' & scr$id != '060' & scr$id != '062' & scr$id != '063' & scr$id != '067' & scr$id != '072' & scr$id != '084' & scr$id != '087' & scr$id != '094' & scr$id != '096' & scr$id != '099' & scr$id != '101' & scr$id != '102' & scr$id != '103' & scr$id != '107' & scr$id != '110' & scr$id != '142' & scr$id != '144' & scr$id != '145' & scr$id!= '147' & scr$id != '150' & scr$id != '155' & scr$id != '159' & scr$id != '172' & scr$id != '173' & scr$id != '190' & scr$id != '194' & scr$id != '204' & scr$id != '206' & scr$id != '207' & scr$id != '208' & scr$id != '213' & scr$id != '220' & scr$id != '224' & scr$id != '227' & scr$id != '231' & scr$id != '235' & scr$id != '238' & scr$id != '239'&  scr$id != '240' & scr$id != '251' & scr$id != '255' & scr$id != '257' & scr$id != '259' & scr$id != '261' & scr$id != '264' & scr$id != '266 '& scr$id != '268'),]

#########create data subsets: EXT
dataSubset <- scr[which(scr$phase == "ext" & scr$half == 2),]
agg.scr.ext <- with(dataSubset, aggregate(raw, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.scr.ext.wide <- reshape(agg.scr.ext, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.scr.ext.wide$scr_ext_CSdiff <- agg.scr.ext.wide$x.CSp-agg.scr.ext.wide$x.CSm
#rename columns
names(agg.scr.ext.wide)[names(agg.scr.ext.wide) == "x.CSm"] <- "scr_ext_CSm"
names(agg.scr.ext.wide)[names(agg.scr.ext.wide) == "x.CSp"] <- "scr_ext_CSp"

dataSubset_scr_ext <- merge(questionnaires, agg.scr.ext.wide,  by="id")


################### Correlations combined score and SCR

######## Extinction
tests <- list(
  cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")),
  cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")),
  cor.test(dataSubset_scr_ext$combined_z, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson"))
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Extinction
tests <- list(
  cor.test(dataSubset_scr_ext$stai_trait, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$stai_trait, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$stai_trait, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_scr_ext$neo_ffi_n, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$neo_ffi_n, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$neo_ffi_n, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSp, method=c("pearson")),
  cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_scr_ext$ius, dataSubset_scr_ext$scr_ext_CSdiff, method=c("pearson"))
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)


#############################################################
####################### FPS #################################
#############################################################

load("fps.RData")

fps <- fps[ fps$id != "024" & fps$id != "042" & fps$id != "043" & fps$id != "069" & fps$id != "081" 
            & fps$id != "085" & fps$id != "110" & fps$id != "124" & fps$id != "128" & fps$id != "134" 
            & fps$id != "136" & fps$id != "141" & fps$id != "144" & fps$id != "145 "& fps$id != "152" & fps$id != "156" 
            & fps$id != "159" & fps$id != "161" & fps$id != "162" & fps$id != "163" & fps$id != "173" 
            & fps$id != "175" & fps$id != "176" & fps$id != "178" & fps$id != "181" & fps$id != "183" 
            & fps$id != "184" & fps$id != "186" & fps$id != "220" & fps$id != "242" & fps$id != "246" & fps$id != "249" 
            & fps$id != "254" & fps$id != "255" & fps$id != "257" & fps$id != "268" & fps$id != "270", ]


########create data subsets: ACQ
dataSubset <- fps[which(fps$phase == "acq"),]
agg.fps.acq <- with(dataSubset, aggregate(Tscore, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.fps.acq.wide <- reshape(agg.fps.acq, idvar = "id", timevar = "stim", direction = "wide")
#calculate difpsimination score
agg.fps.acq.wide$fps_acq_CSdiff <- agg.fps.acq.wide$x.CSp-agg.fps.acq.wide$x.CSm
#rename columns
names(agg.fps.acq.wide)[names(agg.fps.acq.wide) == "x.CSm"] <- "fps_acq_CSm"
names(agg.fps.acq.wide)[names(agg.fps.acq.wide) == "x.CSp"] <- "fps_acq_CSp"

dataSubset_fps_acq <- merge(questionnaires, agg.fps.acq.wide,  by="id")

#########create data subsets: EXT
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

#########create data subsets: REN
dataSubset <- fps[which(fps$phase == "ren"),]
agg.fps.ren <- with(dataSubset, aggregate(Tscore, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.fps.ren.wide <- reshape(agg.fps.ren, idvar = "id", timevar = "stim", direction = "wide")
#calculate difpsimination score
agg.fps.ren.wide$fps_ren_CSdiff <- agg.fps.ren.wide$x.CSp-agg.fps.ren.wide$x.CSm
#rename columns
names(agg.fps.ren.wide)[names(agg.fps.ren.wide) == "x.CSm"] <- "fps_ren_CSm"
names(agg.fps.ren.wide)[names(agg.fps.ren.wide) == "x.CSp"] <- "fps_ren_CSp"

dataSubset_fps_ren <- merge(questionnaires, agg.fps.ren.wide,  by="id")


################### Correlations and FPS
######## Acquisition
cor.test(dataSubset_fps_acq$stai_trait, dataSubset_fps_acq$fps_acq_CSp, method=c("pearson"))
cor.test(dataSubset_fps_acq$neo_ffi_n, dataSubset_fps_acq$fps_acq_CSp, method=c("pearson"))
cor.test(dataSubset_fps_acq$ius, dataSubset_fps_acq$fps_acq_CSp, method=c("pearson"))

cor.test(dataSubset_fps_acq$stai_trait, dataSubset_fps_acq$fps_acq_CSm, method=c("pearson"))
cor.test(dataSubset_fps_acq$neo_ffi_n, dataSubset_fps_acq$fps_acq_CSm, method=c("pearson"))
cor.test(dataSubset_fps_acq$ius, dataSubset_fps_acq$fps_acq_CSm, method=c("pearson"))

cor.test(dataSubset_fps_acq$stai_trait, dataSubset_fps_acq$fps_acq_CSdiff, method=c("pearson"))
cor.test(dataSubset_fps_acq$neo_ffi_n, dataSubset_fps_acq$fps_acq_CSdiff, method=c("pearson"))
cor.test(dataSubset_fps_acq$ius, dataSubset_fps_acq$fps_acq_CSdiff, method=c("pearson"))


######## Extinction
cor.test(dataSubset_fps_ext$stai_trait, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson"))
cor.test(dataSubset_fps_ext$neo_ffi_n, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson"))
cor.test(dataSubset_fps_ext$ius, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson"))

cor.test(dataSubset_fps_ext$stai_trait, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson"))
cor.test(dataSubset_fps_ext$neo_ffi_n, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson"))
cor.test(dataSubset_fps_ext$ius, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson"))

cor.test(dataSubset_fps_ext$stai_trait, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson"))
cor.test(dataSubset_fps_ext$neo_ffi_n, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson"))
cor.test(dataSubset_fps_ext$ius, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson"))

######## Renewal
cor.test(dataSubset_fps_ren$stai_trait, dataSubset_fps_ren$fps_ren_CSp, method=c("pearson")) 
cor.test(dataSubset_fps_ren$stai_trait, dataSubset_fps_ren$fps_ren_CSm, method=c("pearson")) 
cor.test(dataSubset_fps_ren$stai_trait, dataSubset_fps_ren$fps_ren_CSdiff, method=c("pearson")) 


################### Correlations combined score and FPS
######## Acquisition
tests <- list(
  cor.test(dataSubset_fps_acq$combined_z, dataSubset_fps_acq$fps_acq_CSp, method=c("pearson")),
cor.test(dataSubset_fps_acq$combined_z, dataSubset_fps_acq$fps_acq_CSm, method=c("pearson")),
cor.test(dataSubset_fps_acq$combined_z, dataSubset_fps_acq$fps_acq_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Extinction
tests <- list(
cor.test(dataSubset_fps_ext$combined_z, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")), 
cor.test(dataSubset_fps_ext$combined_z, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")), 
cor.test(dataSubset_fps_ext$combined_z, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Renewal
tests <- list(
cor.test(dataSubset_fps_ren$combined_z, dataSubset_fps_ren$fps_ren_CSp, method=c("pearson")), 
cor.test(dataSubset_fps_ren$combined_z, dataSubset_fps_ren$fps_ren_CSm, method=c("pearson")), 
cor.test(dataSubset_fps_ren$combined_z, dataSubset_fps_ren$fps_ren_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)


######## Acquisition
tests <- list(
  cor.test(dataSubset_fps_acq$stai_trait, dataSubset_fps_acq$fps_acq_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_acq$stai_trait, dataSubset_fps_acq$fps_acq_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_acq$stai_trait, dataSubset_fps_acq$fps_acq_CSdiff, method=c("pearson"))
)

tests <- list(
  cor.test(dataSubset_fps_acq$neo_ffi_n, dataSubset_fps_acq$fps_acq_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_acq$neo_ffi_n, dataSubset_fps_acq$fps_acq_CSm, method=c("pearson")),
  cor.test(dataSubset_fps_acq$neo_ffi_n, dataSubset_fps_acq$fps_acq_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fps_acq$ius, dataSubset_fps_acq$fps_acq_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_acq$ius, dataSubset_fps_acq$fps_acq_CSm, method=c("pearson")),
  cor.test(dataSubset_fps_acq$ius, dataSubset_fps_acq$fps_acq_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Extinction
tests <- list(
  cor.test(dataSubset_fps_ext$stai_trait, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$stai_trait, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$stai_trait, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fps_ext$neo_ffi_n, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$neo_ffi_n, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$neo_ffi_n, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fps_ext$ius, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$ius, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$ius, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Renewal
tests <- list(
  cor.test(dataSubset_fps_ren$stai_trait, dataSubset_fps_ren$fps_ren_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ren$stai_trait, dataSubset_fps_ren$fps_ren_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ren$stai_trait, dataSubset_fps_ren$fps_ren_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fps_ren$neo_ffi_n, dataSubset_fps_ren$fps_ren_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ren$neo_ffi_n, dataSubset_fps_ren$fps_ren_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ren$neo_ffi_n, dataSubset_fps_ren$fps_ren_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fps_ren$ius, dataSubset_fps_ren$fps_ren_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ren$ius, dataSubset_fps_ren$fps_ren_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ren$ius, dataSubset_fps_ren$fps_ren_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

####################### FPS first half #################################

load("fps.RData")

fps <- fps[ fps$id != "024" & fps$id != "042" & fps$id != "043" & fps$id != "069" & fps$id != "081" 
            & fps$id != "085" & fps$id != "110" & fps$id != "124" & fps$id != "128" & fps$id != "134" 
            & fps$id != "136" & fps$id != "141" & fps$id != "144" & fps$id != "145 "& fps$id != "152" & fps$id != "156" 
            & fps$id != "159" & fps$id != "161" & fps$id != "162" & fps$id != "163" & fps$id != "173" 
            & fps$id != "175" & fps$id != "176" & fps$id != "178" & fps$id != "181" & fps$id != "183" 
            & fps$id != "184" & fps$id != "186" & fps$id != "220" & fps$id != "242" & fps$id != "246" & fps$id != "249" 
            & fps$id != "254" & fps$id != "255" & fps$id != "257" & fps$id != "268" & fps$id != "270", ]


#########create data subsets: EXT
dataSubset <- fps[which(fps$phase == "ext" & fps$half == 1),]
agg.fps.ext <- with(dataSubset, aggregate(Tscore, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.fps.ext.wide <- reshape(agg.fps.ext, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.fps.ext.wide$fps_ext_CSdiff <- agg.fps.ext.wide$x.CSp-agg.fps.ext.wide$x.CSm
#rename columns
names(agg.fps.ext.wide)[names(agg.fps.ext.wide) == "x.CSm"] <- "fps_ext_CSm"
names(agg.fps.ext.wide)[names(agg.fps.ext.wide) == "x.CSp"] <- "fps_ext_CSp"

dataSubset_fps_ext <- merge(questionnaires, agg.fps.ext.wide,  by="id")

#Extinction
tests <- list(
  cor.test(dataSubset_fps_ext$combined_z, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$combined_z, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$combined_z, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Extinction
tests <- list(
  cor.test(dataSubset_fps_ext$stai_trait, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$stai_trait, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$stai_trait, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fps_ext$neo_ffi_n, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$neo_ffi_n, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$neo_ffi_n, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fps_ext$ius, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$ius, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")),
  cor.test(dataSubset_fps_ext$ius, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

####################### FPS second half #################################

load("fps.RData")

fps <- fps[ fps$id != "024" & fps$id != "042" & fps$id != "043" & fps$id != "069" & fps$id != "081" 
            & fps$id != "085" & fps$id != "110" & fps$id != "124" & fps$id != "128" & fps$id != "134" 
            & fps$id != "136" & fps$id != "141" & fps$id != "144" & fps$id != "145 "& fps$id != "152" & fps$id != "156" 
            & fps$id != "159" & fps$id != "161" & fps$id != "162" & fps$id != "163" & fps$id != "173" 
            & fps$id != "175" & fps$id != "176" & fps$id != "178" & fps$id != "181" & fps$id != "183" 
            & fps$id != "184" & fps$id != "186" & fps$id != "220" & fps$id != "242" & fps$id != "246" & fps$id != "249" 
            & fps$id != "254" & fps$id != "255" & fps$id != "257" & fps$id != "268" & fps$id != "270", ]


#########create data subsets: EXT
dataSubset <- fps[which(fps$phase == "ext" & fps$half == 2),]
agg.fps.ext <- with(dataSubset, aggregate(Tscore, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.fps.ext.wide <- reshape(agg.fps.ext, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.fps.ext.wide$fps_ext_CSdiff <- agg.fps.ext.wide$x.CSp-agg.fps.ext.wide$x.CSm
#rename columns
names(agg.fps.ext.wide)[names(agg.fps.ext.wide) == "x.CSm"] <- "fps_ext_CSm"
names(agg.fps.ext.wide)[names(agg.fps.ext.wide) == "x.CSp"] <- "fps_ext_CSp"

dataSubset_fps_ext <- merge(questionnaires, agg.fps.ext.wide,  by="id")

#Extinction
tests <- list(
  cor.test(dataSubset_fps_ext$combined_z, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$combined_z, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$combined_z, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Extinction
tests <- list(
  cor.test(dataSubset_fps_ext$stai_trait, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")),
  cor.test(dataSubset_fps_ext$stai_trait, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")),
  cor.test(dataSubset_fps_ext$stai_trait, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fps_ext$neo_ffi_n, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")),
  cor.test(dataSubset_fps_ext$neo_ffi_n, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$neo_ffi_n, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fps_ext$ius, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$ius, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")),
  cor.test(dataSubset_fps_ext$ius, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

#############################################################
####################### FPS RAW VALUES ######################
#############################################################

load("fps.RData")

fps <- fps[ fps$id != "024" & fps$id != "042" & fps$id != "043" & fps$id != "069" & fps$id != "081" 
            & fps$id != "085" & fps$id != "110" & fps$id != "124" & fps$id != "128" & fps$id != "134" 
            & fps$id != "136" & fps$id != "141" & fps$id != "144" & fps$id != "145 "& fps$id != "152" & fps$id != "156" 
            & fps$id != "159" & fps$id != "161" & fps$id != "162" & fps$id != "163" & fps$id != "173" 
            & fps$id != "175" & fps$id != "176" & fps$id != "178" & fps$id != "181" & fps$id != "183" 
            & fps$id != "184" & fps$id != "186" & fps$id != "220" & fps$id != "242" & fps$id != "246" & fps$id != "249" 
            & fps$id != "254" & fps$id != "255" & fps$id != "257" & fps$id != "268" & fps$id != "270", ]


########create data subsets: ACQ
dataSubset <- fps[which(fps$phase == "acq"),]
agg.fps.acq <- with(dataSubset, aggregate(raw, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.fps.acq.wide <- reshape(agg.fps.acq, idvar = "id", timevar = "stim", direction = "wide")
#calculate difpsimination score
agg.fps.acq.wide$fps_acq_CSdiff <- agg.fps.acq.wide$x.CSp-agg.fps.acq.wide$x.CSm
#rename columns
names(agg.fps.acq.wide)[names(agg.fps.acq.wide) == "x.CSm"] <- "fps_acq_CSm"
names(agg.fps.acq.wide)[names(agg.fps.acq.wide) == "x.CSp"] <- "fps_acq_CSp"

dataSubset_fps_acq <- merge(questionnaires, agg.fps.acq.wide,  by="id")

#########create data subsets: EXT
dataSubset <- fps[which(fps$phase == "ext"),]
agg.fps.ext <- with(dataSubset, aggregate(raw, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.fps.ext.wide <- reshape(agg.fps.ext, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.fps.ext.wide$fps_ext_CSdiff <- agg.fps.ext.wide$x.CSp-agg.fps.ext.wide$x.CSm
#rename columns
names(agg.fps.ext.wide)[names(agg.fps.ext.wide) == "x.CSm"] <- "fps_ext_CSm"
names(agg.fps.ext.wide)[names(agg.fps.ext.wide) == "x.CSp"] <- "fps_ext_CSp"

dataSubset_fps_ext <- merge(questionnaires, agg.fps.ext.wide,  by="id")

#########create data subsets: REN
dataSubset <- fps[which(fps$phase == "ren"),]
agg.fps.ren <- with(dataSubset, aggregate(raw, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.fps.ren.wide <- reshape(agg.fps.ren, idvar = "id", timevar = "stim", direction = "wide")
#calculate difpsimination score
agg.fps.ren.wide$fps_ren_CSdiff <- agg.fps.ren.wide$x.CSp-agg.fps.ren.wide$x.CSm
#rename columns
names(agg.fps.ren.wide)[names(agg.fps.ren.wide) == "x.CSm"] <- "fps_ren_CSm"
names(agg.fps.ren.wide)[names(agg.fps.ren.wide) == "x.CSp"] <- "fps_ren_CSp"

dataSubset_fps_ren <- merge(questionnaires, agg.fps.ren.wide,  by="id")


################### Correlations combined score and FPS
######## Acquisition
tests <- list(
  cor.test(dataSubset_fps_acq$combined_z, dataSubset_fps_acq$fps_acq_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_acq$combined_z, dataSubset_fps_acq$fps_acq_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_acq$combined_z, dataSubset_fps_acq$fps_acq_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Extinction
tests <- list(
  cor.test(dataSubset_fps_ext$combined_z, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$combined_z, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")),
  cor.test(dataSubset_fps_ext$combined_z, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Renewal
tests <- list(
  cor.test(dataSubset_fps_ren$combined_z, dataSubset_fps_ren$fps_ren_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ren$combined_z, dataSubset_fps_ren$fps_ren_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ren$combined_z, dataSubset_fps_ren$fps_ren_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)


######## Acquisition
tests <- list(
  cor.test(dataSubset_fps_acq$stai_trait, dataSubset_fps_acq$fps_acq_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_acq$stai_trait, dataSubset_fps_acq$fps_acq_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_acq$stai_trait, dataSubset_fps_acq$fps_acq_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fps_acq$neo_ffi_n, dataSubset_fps_acq$fps_acq_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_acq$neo_ffi_n, dataSubset_fps_acq$fps_acq_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_acq$neo_ffi_n, dataSubset_fps_acq$fps_acq_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fps_acq$ius, dataSubset_fps_acq$fps_acq_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_acq$ius, dataSubset_fps_acq$fps_acq_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_acq$ius, dataSubset_fps_acq$fps_acq_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Extinction
tests <- list(
  cor.test(dataSubset_fps_ext$stai_trait, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$stai_trait, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$stai_trait, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fps_ext$neo_ffi_n, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$neo_ffi_n, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$neo_ffi_n, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fps_ext$ius, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$ius, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$ius, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Renewal
tests <- list(
  cor.test(dataSubset_fps_ren$stai_trait, dataSubset_fps_ren$fps_ren_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ren$stai_trait, dataSubset_fps_ren$fps_ren_CSm, method=c("pearson")),
  cor.test(dataSubset_fps_ren$stai_trait, dataSubset_fps_ren$fps_ren_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fps_ren$neo_ffi_n, dataSubset_fps_ren$fps_ren_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ren$neo_ffi_n, dataSubset_fps_ren$fps_ren_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ren$neo_ffi_n, dataSubset_fps_ren$fps_ren_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fps_ren$ius, dataSubset_fps_ren$fps_ren_CSp, method=c("pearson")),
  cor.test(dataSubset_fps_ren$ius, dataSubset_fps_ren$fps_ren_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ren$ius, dataSubset_fps_ren$fps_ren_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)



####################### FPS first half #################################

load("fps.RData")

fps <- fps[ fps$id != "024" & fps$id != "042" & fps$id != "043" & fps$id != "069" & fps$id != "081" 
            & fps$id != "085" & fps$id != "110" & fps$id != "124" & fps$id != "128" & fps$id != "134" 
            & fps$id != "136" & fps$id != "141" & fps$id != "144" & fps$id != "145 "& fps$id != "152" & fps$id != "156" 
            & fps$id != "159" & fps$id != "161" & fps$id != "162" & fps$id != "163" & fps$id != "173" 
            & fps$id != "175" & fps$id != "176" & fps$id != "178" & fps$id != "181" & fps$id != "183" 
            & fps$id != "184" & fps$id != "186" & fps$id != "220" & fps$id != "242" & fps$id != "246" & fps$id != "249" 
            & fps$id != "254" & fps$id != "255" & fps$id != "257" & fps$id != "268" & fps$id != "270", ]


#########create data subsets: EXT
dataSubset <- fps[which(fps$phase == "ext" & fps$half == 1),]
agg.fps.ext <- with(dataSubset, aggregate(raw, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.fps.ext.wide <- reshape(agg.fps.ext, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.fps.ext.wide$fps_ext_CSdiff <- agg.fps.ext.wide$x.CSp-agg.fps.ext.wide$x.CSm
#rename columns
names(agg.fps.ext.wide)[names(agg.fps.ext.wide) == "x.CSm"] <- "fps_ext_CSm"
names(agg.fps.ext.wide)[names(agg.fps.ext.wide) == "x.CSp"] <- "fps_ext_CSp"

dataSubset_fps_ext <- merge(questionnaires, agg.fps.ext.wide,  by="id")


######## Extinction
tests <- list(
  cor.test(dataSubset_fps_ext$combined_z, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$combined_z, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$combined_z, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Extinction
tests <- list(
  cor.test(dataSubset_fps_ext$stai_trait, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$stai_trait, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$stai_trait, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fps_ext$neo_ffi_n, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$neo_ffi_n, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$neo_ffi_n, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fps_ext$ius, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")),
  cor.test(dataSubset_fps_ext$ius, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$ius, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

####################### FPS second half #################################

load("fps.RData")

fps <- fps[ fps$id != "024" & fps$id != "042" & fps$id != "043" & fps$id != "069" & fps$id != "081" 
            & fps$id != "085" & fps$id != "110" & fps$id != "124" & fps$id != "128" & fps$id != "134" 
            & fps$id != "136" & fps$id != "141" & fps$id != "144" & fps$id != "145 "& fps$id != "152" & fps$id != "156" 
            & fps$id != "159" & fps$id != "161" & fps$id != "162" & fps$id != "163" & fps$id != "173" 
            & fps$id != "175" & fps$id != "176" & fps$id != "178" & fps$id != "181" & fps$id != "183" 
            & fps$id != "184" & fps$id != "186" & fps$id != "220" & fps$id != "242" & fps$id != "246" & fps$id != "249" 
            & fps$id != "254" & fps$id != "255" & fps$id != "257" & fps$id != "268" & fps$id != "270", ]


#########create data subsets: EXT
dataSubset <- fps[which(fps$phase == "ext" & fps$half == 2),]
agg.fps.ext <- with(dataSubset, aggregate(raw, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.fps.ext.wide <- reshape(agg.fps.ext, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.fps.ext.wide$fps_ext_CSdiff <- agg.fps.ext.wide$x.CSp-agg.fps.ext.wide$x.CSm
#rename columns
names(agg.fps.ext.wide)[names(agg.fps.ext.wide) == "x.CSm"] <- "fps_ext_CSm"
names(agg.fps.ext.wide)[names(agg.fps.ext.wide) == "x.CSp"] <- "fps_ext_CSp"

dataSubset_fps_ext <- merge(questionnaires, agg.fps.ext.wide,  by="id")

######## Extinction
tests <- list(
  cor.test(dataSubset_fps_ext$combined_z, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$combined_z, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$combined_z, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

######## Extinction
tests <- list(
  cor.test(dataSubset_fps_ext$stai_trait, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$stai_trait, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$stai_trait, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fps_ext$neo_ffi_n, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$neo_ffi_n, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$neo_ffi_n, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fps_ext$ius, dataSubset_fps_ext$fps_ext_CSp, method=c("pearson")),
  cor.test(dataSubset_fps_ext$ius, dataSubset_fps_ext$fps_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fps_ext$ius, dataSubset_fps_ext$fps_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)


#############################################################
####################### Expectancy Ratings ##################
#############################################################

### ratings
load("ratings.RData")
ratings <- ratings[which(ratings$id != '145' & ratings$id != '220'),]


######## create data subsets: ACQ
dataSubset <- ratings[which(ratings$phase == "acq"),]
agg.rate.acq <- with(dataSubset, aggregate(exp_rating, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.rate.acq.wide <- reshape(agg.rate.acq, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.rate.acq.wide$rate_acq_CSdiff <- agg.rate.acq.wide$x.CSp-agg.rate.acq.wide$x.CSm
#rename columns
names(agg.rate.acq.wide)[names(agg.rate.acq.wide) == "x.CSm"] <- "rate_acq_CSm"
names(agg.rate.acq.wide)[names(agg.rate.acq.wide) == "x.CSp"] <- "rate_acq_CSp"

dataSubset_rate_acq <- merge(questionnaires, agg.rate.acq.wide,  by="id")

######### create data subsets: EXT
dataSubset <- ratings[which(ratings$phase == "ext"),]
agg.rate.ext <- with(dataSubset, aggregate(exp_rating, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.rate.ext.wide <- reshape(agg.rate.ext, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.rate.ext.wide$rate_ext_CSdiff <- agg.rate.ext.wide$x.CSp-agg.rate.ext.wide$x.CSm
#rename columns
names(agg.rate.ext.wide)[names(agg.rate.ext.wide) == "x.CSm"] <- "rate_ext_CSm"
names(agg.rate.ext.wide)[names(agg.rate.ext.wide) == "x.CSp"] <- "rate_ext_CSp"

dataSubset_rate_ext <- merge(questionnaires, agg.rate.ext.wide,  by="id")

######### create data subsets: REN
dataSubset <- ratings[which(ratings$phase == "ren"),]
agg.rate.ren <- with(dataSubset, aggregate(exp_rating, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.rate.ren.wide <- reshape(agg.rate.ren, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.rate.ren.wide$rate_ren_CSdiff <- agg.rate.ren.wide$x.CSp-agg.rate.ren.wide$x.CSm
#rename columns
names(agg.rate.ren.wide)[names(agg.rate.ren.wide) == "x.CSm"] <- "rate_ren_CSm"
names(agg.rate.ren.wide)[names(agg.rate.ren.wide) == "x.CSp"] <- "rate_ren_CSp"

dataSubset_rate_ren <- merge(questionnaires, agg.rate.ren.wide,  by="id")

################### Correlations combined score and ratings
######## Acquisition
tests <- list(
  cor.test(dataSubset_rate_acq$combined_z, dataSubset_rate_acq$rate_acq_CSp, method=c("pearson")),
  cor.test(dataSubset_rate_acq$combined_z, dataSubset_rate_acq$rate_acq_CSm, method=c("pearson")), 
  cor.test(dataSubset_rate_acq$combined_z, dataSubset_rate_acq$rate_acq_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)


tests <- list(
  cor.test(dataSubset_rate_acq$stai_trait, dataSubset_rate_acq$rate_acq_CSp, method=c("pearson")), 
  cor.test(dataSubset_rate_acq$stai_trait, dataSubset_rate_acq$rate_acq_CSm, method=c("pearson")), 
  cor.test(dataSubset_rate_acq$stai_trait, dataSubset_rate_acq$rate_acq_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

tests <- list(
  cor.test(dataSubset_rate_acq$neo_ffi_n, dataSubset_rate_acq$rate_acq_CSp, method=c("pearson")), 
  cor.test(dataSubset_rate_acq$neo_ffi_n, dataSubset_rate_acq$rate_acq_CSm, method=c("pearson")), 
  cor.test(dataSubset_rate_acq$neo_ffi_n, dataSubset_rate_acq$rate_acq_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

tests <- list(
  cor.test(dataSubset_rate_acq$ius, dataSubset_rate_acq$rate_acq_CSp, method=c("pearson")),
  cor.test(dataSubset_rate_acq$ius, dataSubset_rate_acq$rate_acq_CSm, method=c("pearson")), 
  cor.test(dataSubset_rate_acq$ius, dataSubset_rate_acq$rate_acq_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)


######## Extinction
tests <- list(
  cor.test(dataSubset_rate_ext$combined_z, dataSubset_rate_ext$rate_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_rate_ext$combined_z, dataSubset_rate_ext$rate_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_rate_ext$combined_z, dataSubset_rate_ext$rate_ext_CSdiff, method=c("pearson")) 
) 


# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)



tests <- list(
  cor.test(dataSubset_rate_ext$stai_trait, dataSubset_rate_ext$rate_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_rate_ext$stai_trait, dataSubset_rate_ext$rate_ext_CSm, method=c("pearson")),
  cor.test(dataSubset_rate_ext$stai_trait, dataSubset_rate_ext$rate_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_rate_ext$neo_ffi_n, dataSubset_rate_ext$rate_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_rate_ext$neo_ffi_n, dataSubset_rate_ext$rate_ext_CSm, method=c("pearson")),
  cor.test(dataSubset_rate_ext$neo_ffi_n, dataSubset_rate_ext$rate_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_rate_ext$ius, dataSubset_rate_ext$rate_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_rate_ext$ius, dataSubset_rate_ext$rate_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_rate_ext$ius, dataSubset_rate_ext$rate_ext_CSdiff, method=c("pearson"))
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)


######## Renewal
tests <- list(
  cor.test(dataSubset_rate_ren$combined_z, dataSubset_rate_ren$rate_ren_CSp, method=c("pearson")), 
  cor.test(dataSubset_rate_ren$combined_z, dataSubset_rate_ren$rate_ren_CSm, method=c("pearson")), 
  cor.test(dataSubset_rate_ren$combined_z, dataSubset_rate_ren$rate_ren_CSdiff, method=c("pearson"))
) 

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

tests <- list(
  cor.test(dataSubset_rate_ren$stai_trait, dataSubset_rate_ren$rate_ren_CSp, method=c("pearson")),
  cor.test(dataSubset_rate_ren$stai_trait, dataSubset_rate_ren$rate_ren_CSm, method=c("pearson")),
  cor.test(dataSubset_rate_ren$stai_trait, dataSubset_rate_ren$rate_ren_CSdiff, method=c("pearson"))
)

tests <- list(
  cor.test(dataSubset_rate_ren$neo_ffi_n, dataSubset_rate_ren$rate_ren_CSp, method=c("pearson")),
  cor.test(dataSubset_rate_ren$neo_ffi_n, dataSubset_rate_ren$rate_ren_CSm, method=c("pearson")),
  cor.test(dataSubset_rate_ren$neo_ffi_n, dataSubset_rate_ren$rate_ren_CSdiff, method=c("pearson"))
)

tests <- list(
  cor.test(dataSubset_rate_ren$ius, dataSubset_rate_ren$rate_ren_CSp, method=c("pearson")),
  cor.test(dataSubset_rate_ren$ius, dataSubset_rate_ren$rate_ren_CSm, method=c("pearson")),
  cor.test(dataSubset_rate_ren$ius, dataSubset_rate_ren$rate_ren_CSdiff, method=c("pearson"))
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)


####################### Expectancy Ratings first half ##################

### ratings
load("ratings.RData")
ratings <- ratings[which(ratings$id != '145' & ratings$id != '220'),]


######### create data subsets: EXT
dataSubset <- ratings[which(ratings$phase == "ext" & ratings$half == 1),]
agg.rate.ext <- with(dataSubset, aggregate(exp_rating, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.rate.ext.wide <- reshape(agg.rate.ext, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.rate.ext.wide$rate_ext_CSdiff <- agg.rate.ext.wide$x.CSp-agg.rate.ext.wide$x.CSm
#rename columns
names(agg.rate.ext.wide)[names(agg.rate.ext.wide) == "x.CSm"] <- "rate_ext_CSm"
names(agg.rate.ext.wide)[names(agg.rate.ext.wide) == "x.CSp"] <- "rate_ext_CSp"

dataSubset_rate_ext <- merge(questionnaires, agg.rate.ext.wide,  by="id")


######## Extinction
tests <- list(
  cor.test(dataSubset_rate_ext$combined_z, dataSubset_rate_ext$rate_ext_CSp, method=c("pearson")),
  cor.test(dataSubset_rate_ext$combined_z, dataSubset_rate_ext$rate_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_rate_ext$combined_z, dataSubset_rate_ext$rate_ext_CSdiff, method=c("pearson")) 
) 


# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

tests <- list(
  cor.test(dataSubset_rate_ext$stai_trait, dataSubset_rate_ext$rate_ext_CSp, method=c("pearson")),
  cor.test(dataSubset_rate_ext$stai_trait, dataSubset_rate_ext$rate_ext_CSm, method=c("pearson")),
  cor.test(dataSubset_rate_ext$stai_trait, dataSubset_rate_ext$rate_ext_CSdiff, method=c("pearson"))
)

tests <- list(
  cor.test(dataSubset_rate_ext$neo_ffi_n, dataSubset_rate_ext$rate_ext_CSp, method=c("pearson")),
  cor.test(dataSubset_rate_ext$neo_ffi_n, dataSubset_rate_ext$rate_ext_CSm, method=c("pearson")),
  cor.test(dataSubset_rate_ext$neo_ffi_n, dataSubset_rate_ext$rate_ext_CSdiff, method=c("pearson"))
)

tests <- list(
  cor.test(dataSubset_rate_ext$ius, dataSubset_rate_ext$rate_ext_CSp, method=c("pearson")),
  cor.test(dataSubset_rate_ext$ius, dataSubset_rate_ext$rate_ext_CSm, method=c("pearson")),
  cor.test(dataSubset_rate_ext$ius, dataSubset_rate_ext$rate_ext_CSdiff, method=c("pearson"))
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)


####################### Expectancy Ratings second half ##################

### ratings
load("ratings.RData")
ratings <- ratings[which(ratings$id != '145' & ratings$id != '220'),]


######### create data subsets: EXT
dataSubset <- ratings[which(ratings$phase == "ext" & ratings$half == 2),]
agg.rate.ext <- with(dataSubset, aggregate(exp_rating, by=list(id=id, stim=stim), FUN=mean, na.rm=T))
#reshape into wide format
agg.rate.ext.wide <- reshape(agg.rate.ext, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.rate.ext.wide$rate_ext_CSdiff <- agg.rate.ext.wide$x.CSp-agg.rate.ext.wide$x.CSm
#rename columns
names(agg.rate.ext.wide)[names(agg.rate.ext.wide) == "x.CSm"] <- "rate_ext_CSm"
names(agg.rate.ext.wide)[names(agg.rate.ext.wide) == "x.CSp"] <- "rate_ext_CSp"

dataSubset_rate_ext <- merge(questionnaires, agg.rate.ext.wide,  by="id")


######## Extinction
tests <- list(
  cor.test(dataSubset_rate_ext$combined_z, dataSubset_rate_ext$rate_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_rate_ext$combined_z, dataSubset_rate_ext$rate_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_rate_ext$combined_z, dataSubset_rate_ext$rate_ext_CSdiff, method=c("pearson")) 
) 
# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

tests <- list(
  cor.test(dataSubset_rate_ext$stai_trait, dataSubset_rate_ext$rate_ext_CSp, method=c("pearson")),
  cor.test(dataSubset_rate_ext$stai_trait, dataSubset_rate_ext$rate_ext_CSm, method=c("pearson")),
  cor.test(dataSubset_rate_ext$stai_trait, dataSubset_rate_ext$rate_ext_CSdiff, method=c("pearson"))
)

tests <- list(
  cor.test(dataSubset_rate_ext$neo_ffi_n, dataSubset_rate_ext$rate_ext_CSp, method=c("pearson")),
  cor.test(dataSubset_rate_ext$neo_ffi_n, dataSubset_rate_ext$rate_ext_CSm, method=c("pearson")),
  cor.test(dataSubset_rate_ext$neo_ffi_n, dataSubset_rate_ext$rate_ext_CSdiff, method=c("pearson"))
)

tests <- list(
  cor.test(dataSubset_rate_ext$ius, dataSubset_rate_ext$rate_ext_CSp, method=c("pearson")),
  cor.test(dataSubset_rate_ext$ius, dataSubset_rate_ext$rate_ext_CSm, method=c("pearson")),
  cor.test(dataSubset_rate_ext$ius, dataSubset_rate_ext$rate_ext_CSdiff, method=c("pearson"))
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)


#############################################################
####################### Fear Ratings ##################
#############################################################

### ratings
load("ratings.RData")
ratings <- ratings[which(ratings$id != '145' & ratings$id != '220'),]


######## create data subsets: ACQ
#put pre and post ratings into long format
#convert back to long format
ratings_long <- gather(ratings, timepoint, fear_rating, pre_rating:post_rating, factor_key = TRUE) 

dataSubset <- ratings_long[which(ratings_long$phase == "acq" & ratings_long$timepoint == "post_rating"),]
agg.rate.acq <- with(dataSubset, aggregate(fear_rating, by=list(id=id, stim=stim), FUN=mean, na.rm=T))

#reshape into wide format
agg.rate.acq.wide <- reshape(agg.rate.acq, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.rate.acq.wide$fear_acq_CSdiff <- agg.rate.acq.wide$x.CSp-agg.rate.acq.wide$x.CSm
#rename columns
names(agg.rate.acq.wide)[names(agg.rate.acq.wide) == "x.CSm"] <- "fear_acq_CSm"
names(agg.rate.acq.wide)[names(agg.rate.acq.wide) == "x.CSp"] <- "fear_acq_CSp"

dataSubset_fear_acq <- merge(questionnaires, agg.rate.acq.wide,  by="id")

######### create data subsets: EXT
ratings_long <- gather(ratings, timepoint, fear_rating, pre_rating:post_rating, factor_key = TRUE) 

dataSubset <- ratings_long[which(ratings_long$phase == "ext" & ratings_long$timepoint == "post_rating"),]
agg.rate.ext <- with(dataSubset, aggregate(fear_rating, by=list(id=id, stim=stim), FUN=mean, na.rm=T))

#reshape into wide format
agg.rate.ext.wide <- reshape(agg.rate.ext, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.rate.ext.wide$fear_ext_CSdiff <- agg.rate.ext.wide$x.CSp-agg.rate.ext.wide$x.CSm
#rename columns
names(agg.rate.ext.wide)[names(agg.rate.ext.wide) == "x.CSm"] <- "fear_ext_CSm"
names(agg.rate.ext.wide)[names(agg.rate.ext.wide) == "x.CSp"] <- "fear_ext_CSp"

dataSubset_fear_ext <- merge(questionnaires, agg.rate.ext.wide,  by="id")

######### create data subsets: REN
ratings_long <- gather(ratings, timepoint, fear_rating, pre_rating:post_rating, factor_key = TRUE) 

dataSubset <- ratings_long[which(ratings_long$phase == "ren" & ratings_long$timepoint == "post_rating"),]
agg.rate.ren <- with(dataSubset, aggregate(fear_rating, by=list(id=id, stim=stim), FUN=mean, na.rm=T))

#reshape into wide format
agg.rate.ren.wide <- reshape(agg.rate.ren, idvar = "id", timevar = "stim", direction = "wide")
#calculate discrimination score
agg.rate.ren.wide$fear_ren_CSdiff <- agg.rate.ren.wide$x.CSp-agg.rate.ren.wide$x.CSm
#rename columns
names(agg.rate.ren.wide)[names(agg.rate.ren.wide) == "x.CSm"] <- "fear_ren_CSm"
names(agg.rate.ren.wide)[names(agg.rate.ren.wide) == "x.CSp"] <- "fear_ren_CSp"

dataSubset_fear_ren <- merge(questionnaires, agg.rate.ren.wide,  by="id")


################### Correlations combined score and fear ratings
######## Acquisition
tests <- list(
  cor.test(dataSubset_fear_acq$combined_z, dataSubset_fear_acq$fear_acq_CSp, method=c("pearson")), 
  cor.test(dataSubset_fear_acq$combined_z, dataSubset_fear_acq$fear_acq_CSm, method=c("pearson")), 
  cor.test(dataSubset_fear_acq$combined_z, dataSubset_fear_acq$fear_acq_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

tests <- list(
  cor.test(dataSubset_fear_acq$stai_trait, dataSubset_fear_acq$fear_acq_CSp, method=c("pearson")), 
  cor.test(dataSubset_fear_acq$stai_trait, dataSubset_fear_acq$fear_acq_CSm, method=c("pearson")), 
  cor.test(dataSubset_fear_acq$stai_trait, dataSubset_fear_acq$fear_acq_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fear_acq$neo_ffi_n, dataSubset_fear_acq$fear_acq_CSp, method=c("pearson")),
  cor.test(dataSubset_fear_acq$neo_ffi_n, dataSubset_fear_acq$fear_acq_CSm, method=c("pearson")), 
  cor.test(dataSubset_fear_acq$neo_ffi_n, dataSubset_fear_acq$fear_acq_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fear_acq$ius, dataSubset_fear_acq$fear_acq_CSp, method=c("pearson")), 
  cor.test(dataSubset_fear_acq$ius, dataSubset_fear_acq$fear_acq_CSm, method=c("pearson")),
  cor.test(dataSubset_fear_acq$ius, dataSubset_fear_acq$fear_acq_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)


######## Extinction
tests <- list(
  cor.test(dataSubset_fear_ext$combined_z, dataSubset_fear_ext$fear_ext_CSp, method=c("pearson")),
  cor.test(dataSubset_fear_ext$combined_z, dataSubset_fear_ext$fear_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fear_ext$combined_z, dataSubset_fear_ext$fear_ext_CSdiff, method=c("pearson"))
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

tests <- list(
  cor.test(dataSubset_fear_ext$stai_trait, dataSubset_fear_ext$fear_ext_CSp, method=c("pearson")),
  cor.test(dataSubset_fear_ext$stai_trait, dataSubset_fear_ext$fear_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fear_ext$stai_trait, dataSubset_fear_ext$fear_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fear_ext$neo_ffi_n, dataSubset_fear_ext$fear_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_fear_ext$neo_ffi_n, dataSubset_fear_ext$fear_ext_CSm, method=c("pearson")),
  cor.test(dataSubset_fear_ext$neo_ffi_n, dataSubset_fear_ext$fear_ext_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fear_ext$ius, dataSubset_fear_ext$fear_ext_CSp, method=c("pearson")), 
  cor.test(dataSubset_fear_ext$ius, dataSubset_fear_ext$fear_ext_CSm, method=c("pearson")), 
  cor.test(dataSubset_fear_ext$ius, dataSubset_fear_ext$fear_ext_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)


######## Renewal
tests <- list(
  cor.test(dataSubset_fear_ren$combined_z, dataSubset_fear_ren$fear_ren_CSp, method=c("pearson")), 
  cor.test(dataSubset_fear_ren$combined_z, dataSubset_fear_ren$fear_ren_CSm, method=c("pearson")), 
  cor.test(dataSubset_fear_ren$combined_z, dataSubset_fear_ren$fear_ren_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)

tests <- list(
  cor.test(dataSubset_fear_ren$stai_trait, dataSubset_fear_ren$fear_ren_CSp, method=c("pearson")), 
  cor.test(dataSubset_fear_ren$stai_trait, dataSubset_fear_ren$fear_ren_CSm, method=c("pearson")), 
  cor.test(dataSubset_fear_ren$stai_trait, dataSubset_fear_ren$fear_ren_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fear_ren$neo_ffi_n, dataSubset_fear_ren$fear_ren_CSp, method=c("pearson")), 
  cor.test(dataSubset_fear_ren$neo_ffi_n, dataSubset_fear_ren$fear_ren_CSm, method=c("pearson")), 
  cor.test(dataSubset_fear_ren$neo_ffi_n, dataSubset_fear_ren$fear_ren_CSdiff, method=c("pearson")) 
)

tests <- list(
  cor.test(dataSubset_fear_ren$ius, dataSubset_fear_ren$fear_ren_CSp, method=c("pearson")), 
  cor.test(dataSubset_fear_ren$ius, dataSubset_fear_ren$fear_ren_CSm, method=c("pearson")), 
  cor.test(dataSubset_fear_ren$ius, dataSubset_fear_ren$fear_ren_CSdiff, method=c("pearson")) 
)

# Extract p-values
pvals <- sapply(tests, function(x) x$p.value)

# Adjust using Benjamini-Hochberg
pvals_bh <- p.adjust(pvals, method = "BH")

# Show results
data.frame(
  correlation = c("CSp", "CSm", "CSdiff"),
  raw_p = pvals,
  adjusted_p = pvals_bh
)


