# ART Project - Exploratory Factor Analysis (EFA)
# Last updated 25/08/2026 - Hannah Stiffel 

### load packages
library(psych)
library(dplyr)
library(Hmisc)
library(GPArotation)
library(ggplot2)
library(purrr)
library(cowplot)


# EFA1: STAI-T, NEO-FFI-N, IUS ####################

### load data
load("questionnaires_items_efa.RData")

### structure of questionnaires
# STAI-T: 20 Items (Itemrange 1-4)
# IUS: 27 Items (Itemrange 1-5)
# NEO-FFI-N: 12 Items (Itemrange 1-5)
# Total Items EFA1: 59

## Data preparation ####################
# create subset for EFA1
items_efa1 <- questionnaires_items_efa %>%
  select(id, stai_trait_01:stai_trait_20, neo_ffi_n_01:neo_ffi_n_12, ius_01:ius_27)

ncol(items_efa1) 

# remove ids with missing values
items_cl_efa1 <- na.omit(items_efa1)

# items are originally from a 4-point and 5-point Likert scale -> differences in scales can bias results
# z-scores of items except id column
# (items_z <- scale(items_cl))
items_z_efa1 <- scale(items_cl_efa1[ , setdiff(names(items_cl_efa1), "id")])


## Determine the number of factors using Parallel Analysis Scree Plots ####################
parallel_efa1 <- fa.parallel(items_z_efa1, fa = "fa", n.iter = 100)
# parallel analysis suggests that the number of factors =  5 
# scree test suggests that the number of factors = 2
# theoretical reasoning: 3 factors

# Data preparation for illustration EFA1
df_pa_efa1 <- data.frame(
  Factor = 1:length(parallel_efa1$fa.values),
  Actual = parallel_efa1$fa.values,
  Simulated = parallel_efa1$fa.sim)

### Illustration EFA1: Scree Plot ####################
efa1_plot <- ggplot(df_pa_efa1, aes(x = Factor)) +
  geom_line(aes(y = Actual, color = "Actual Data"), linewidth = 1.3) +
  geom_line(aes(y = Simulated, color = "Simulated Data"), linewidth = 1.3) +
  geom_point(aes(y = Actual, color = "Actual Data", shape = "Actual Data"), size = 3.3) +
  geom_point(aes(y = Simulated, color = "Simulated Data", shape = "Simulated Data"), size = 3.3) +
  geom_hline(yintercept = 1, linetype = "dotted") +  
  scale_color_manual(values = c("Actual Data" = "#59a4cb", "Simulated Data" = "#c14e42")) +
  scale_shape_manual(values = c("Actual Data" = 16, "Simulated Data" = 17)) +
  scale_x_continuous(breaks = seq(0, max(df_pa_efa1$Factor), by = 5)) +
  labs(y = "Eigen values of principal factors", tag = "A") +
  guides(shape = "none",
         color = guide_legend(override.aes = list(shape = c(16, 17)))) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    # legend.position = "none", # prep for final_plot
    plot.tag = element_text(face = "bold", size = 25),
    plot.tag.position = c(0.005, 0.99))
efa1_plot

ggsave2(
  filename = "screeplot_EFA1.pdf",
  plot = efa1_plot,
  width = 15.03,
  height = 8.73,
  units = "in")


## Principal Axis Factor Analysis (PAF) ####################
# factors: 3 
# rotation method: oblimin
paf1_3_ob <- fa(items_z_efa1, 3, fm = "pa", rotate = "oblimin")
print(paf1_3_ob)
print(paf1_3_ob, digits = 2, cut = .3)

# factors: 2
# rotation method: oblimin
paf1_2_ob <- fa(items_z_efa1, 2, fm = "pa", rotate = "oblimin")
print(paf1_2_ob)
print(paf1_2_ob, digits = 2, cut = .3)


# factors: 3
# rotation method: promax
# paf_3_pro <- fa(items_z, 3, fm = "pa", rotate = "promax")
# print(paf_3_pro)
# print(paf_3_pro, digits = 2, cut = .3)

# factors: 2
# rotation method: promax
# paf_2_pro <- fa(items_z, 2, fm = "pa", rotate = "promax")
# print(paf_2_pro)
# print(paf_2_pro, digits = 2, cut = .3)





# EFA2: STAI-T, NEO-FFI-N, ASI, BDI-2 ####################

### load data
load("questionnaires_items_efa.RData")

### structure of questionnaires
# STAI-T: 20 Items (Itemrange 1-4 -> 4)
# NEO-FFI-N: 12 Items (Itemrange 1-5 -> 5)
# ASI: 16 Items (Itemrange 0-4 -> 5)
# BDI: 21 Items (Itemrange 0-3 -> 4)
# Total Items: 69

## Data preparation ####################
# create subset for EFA2
items_efa2 <- questionnaires_items_efa %>%
  select(id, stai_trait_01:stai_trait_20, neo_ffi_n_01:neo_ffi_n_12, asi_01:asi_16, bdi_01:bdi_21)

ncol(items_efa2)

# remove ids with missing values
items_cl_efa2 <- na.omit(items_efa2)

# items are originally from a 4-point and 5-point Likert scale -> differences in scales can bias results
# z-scores of items except id column
# (items_z <- scale(items_cl))
items_z_efa2 <- scale(items_cl_efa2[ , setdiff(names(items_cl_efa2), "id")])


## Determine the number of factors using Parallel Analysis Scree Plots ####################
parallel_efa2 <- fa.parallel(items_z_efa2, fa = "fa", n.iter = 100)
# parallel analysis suggests that the number of factors =  6
# scree test suggests that the number of factors = 2-4
# theoretical reasoning: exploratory

# Data preparation for illustration EFA2
df_pa_efa2 <- data.frame(
  Factor = 1:length(parallel_efa2$fa.values),
  Actual = parallel_efa2$fa.values,
  Simulated = parallel_efa2$fa.sim)

### Illustration EFA2: Scree Plot ####################
efa2_plot <- ggplot(df_pa_efa2, aes(x = Factor)) +
  geom_line(aes(y = Actual, color = "Actual Data"), linewidth = 1.3) +
  geom_line(aes(y = Simulated, color = "Simulated Data"), linewidth = 1.3) +
  geom_point(aes(y = Actual, color = "Actual Data", shape = "Actual Data"), size = 3.3) +
  geom_point(aes(y = Simulated, color = "Simulated Data", shape = "Simulated Data"), size = 3.3) +
  geom_hline(yintercept = 1, linetype = "dotted") + 
  scale_color_manual(values = c("Actual Data" = "#59a4cb", "Simulated Data" = "#c14e42")) +
  scale_shape_manual(values = c("Actual Data" = 16, "Simulated Data" = 17)) +
  scale_x_continuous(breaks = seq(0, max(df_pa_efa2$Factor), by = 5)) +
  labs(y = "Eigen values of principal factors", tag = "B") +
  guides(shape = "none",
         color = guide_legend(override.aes = list(shape = c(16, 17)))) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    # legend.position = "none", # prep for final_plot
    plot.tag = element_text(face = "bold", size = 25),
    plot.tag.position = c(0.005, 0.99))
efa2_plot

ggsave2(
  filename = "screeplot_EFA2.pdf",
  plot = efa2_plot,
  width = 15.03,
  height = 8.73,
  units = "in")


## Principal Axis Factor Analysis (PAF) ####################
# rotation method: oblimin

# factors: 2
# paf2_2_ob_r2 <- fa(items_z_efa2, 2, fm = "pa", rotate = "oblimin")
# print(paf2_2_ob_r2)
# print(paf2_2_ob_r2, digits = 2, cut = .3)

# factors: 3
# paf2_3_ob_r2 <- fa(items_z_efa2, 3, fm = "pa", rotate = "oblimin")
# print(paf2_3_ob_r2)
# print(paf2_3_ob_r2, digits = 2, cut = .3)

# factors: 4
paf2_4_ob_r2 <- fa(items_z_efa2, 4, fm = "pa", rotate = "oblimin")
print(paf2_4_ob_r2)
print(paf2_4_ob_r2, digits = 2, cut = .3)

# factors: 5
# paf2_5_ob_r2 <- fa(items_z_efa2, 5, fm = "pa", rotate = "oblimin")
# print(paf2_5_ob_r2)
# print(paf2_5_ob_r2, digits = 2, cut = .3)

# factors: 6
# paf2_6_ob_r2 <- fa(items_z_efa2, 6, fm = "pa", rotate = "oblimin")
# print(paf2_6_ob_r2)
# print(paf2_6_ob_r2, digits = 2, cut = .3)



