rm(list = ls())

# libraries 
library(tidyverse)
library(openxlsx)
library(haven)
library(foreign)
library(data.table)
library(patchwork)
library(cowplot)
library(gridExtra)
# Box Path

if (Sys.getenv("USERNAME") == "HP" && Sys.getenv("COMPUTERNAME") == "RW-5CG2404KFQ") {
  box_root <- file.path("C:","Users","HP", "Box", "IPA_RWA_Project_STARS")
  
} else {
  stop("Define machine-specific Box Path.")
}

box_la_path <- file.path(box_root, "07_Data", "22_FinalAssessmentsIPA",
                         "year_2","7_tracking")
setwd(box_la_path)

# Import data