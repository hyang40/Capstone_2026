# =============================================================
# 03b. Baseline comparison_plots
# 
# ZJX Corridor — day of week comparrison+ Clustering
#
# Required File Run: 01_pipeline_baseline.R 
# Output File: plot_A_weekly_profile.png
#            plot_B_dow_total.png
#            plot_C_heatmap_labeled.png
#            plot_D_pairwise_morning.png
#            plot_E_pairwise_afternoon.png
#            plot_F_dendrogram.png
#            table_distance_euclidean.csv
#            table_distance_correlation.csv
#            table_cluster_assignment.csv
#            table_pairwise_all.csv
# =============================================================

# ================================================
# 0. Set Up
# ================================================
##Package
library(tidyverse)
library(lubridate)
library(rstatix)
library(readr)
# working dir
setwd("D:/Capstone/CapstoneSp26")
# read data
df_prep <- read_csv("01_prep_kmxr_data.csv")
df_prep <- df #from 02b.R [1] 9718865
nrow(df_prep) #[1] 10964122


cat("Date Validation\n")
cat(" Start Date:", as.character(min(df$date)), "\n")
cat(" End Datae:", as.character(max(df$date)), "\n")
cat(" Total Date Count:", n_distinct(df$date), "Days\n\n")


# output dir -------------------------------------
plot_dir <- file.path("03_Baseline_EDA")
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}
