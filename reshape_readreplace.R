library(foreign)
library(haven)
library(tidyverse)
library(VIM)
library(outliers)
library(ggplot2)
library(scales) 
library(grid)
library(RColorBrewer)
library(psych)
#install.packages("sjlabelled")
library(testit)
library(matrixStats)
library(rio)
library(labelled)
library(openxlsx)
library(data.table)
library(sjlabelled)


setwd("C:/Users/HP/Box/IPA_RWA_Project_STARS/07_Data/22_FinalAssessmentsIPA/")

to_reshape <- read_dta("3_clean/reconciled_clean.dta")
to_reshape <- select(to_reshape,district:p3_leg_q05_sub1)%>%
  select(-school_student,-starts_with("discrep"))



first_reshape <- read_dta("3_clean/first_entry/first_readreplace.dta")
first_reshaped<-first_reshape%>%
  pivot_longer(cols=eng_participation:p3_leg_q05_sub1,names_to = "Question",values_to = "Answer",
               values_drop_na = TRUE)
write_dta(first_reshaped,"3_clean/first_entry/first_reshaped.dta")


second_reshape <- read_dta("3_clean/second_entry/second_readreplace.dta")
dropped0 <- setdiff(as.vector(colnames(second_reshape)),as.vector(colnames(first_reshape)))

second_reshaped<-second_reshape%>%
  select(-v453)%>%
  pivot_longer(cols=eng_participation:p3_leg_q05_sub1,names_to = "Question",values_to = "Answer",
               values_drop_na = TRUE)
write_dta(second_reshaped,"3_clean/second_entry/second_reshaped.dta")


dropped <- setdiff(as.vector(colnames(to_reshape)),as.vector(colnames(first_reshape)))

reshaped <- to_reshape%>%
  select(-c(dropped))%>%
  pivot_longer(cols=eng_participation:p3_leg_q05_sub1,names_to = "Question",values_to = "CorrectValue",
               values_drop_na = TRUE)
write.csv(reshaped,"3_clean/read_replace/correctedvalues.csv",row.names = FALSE)
write_dta(reshaped,"3_clean/read_replace/correctedvalues.dta")





first_reshaped_corrected <- read_dta("3_clean/first_entry/first_reshaped_corrected.dta")
first_corrected <- first_reshaped_corrected%>%
  pivot_wider(id_cols = c("district","sector","school","grade","student_code"),
              names_from =Question,values_from = Answer)

write_dta(first_corrected,"3_clean/first_entry/first_corrected.dta")  
write_dta(first_corrected,"3_clean/read_replace/first_corrected.dta")


second_reshaped_corrected <- read_dta("3_clean/second_entry/second_reshaped_corrected.dta")
second_corrected <- second_reshaped_corrected%>%
  pivot_wider(id_cols = c("district","sector","school","grade","student_code"),
              names_from =Question,values_from = Answer)

write_dta(second_corrected,"3_clean/second_entry/second_corrected.dta")  
write_dta(second_corrected,"3_clean/read_replace/second_corrected.dta")


# RESHAPING THE 338 FORMS TO MAKE RECONCILIATION ADJUSTMENTS####
################################################################

#RECONCILIATION DATA FROM AUG 19####
####################################
rec_aug19 <- read_dta("3_clean/reconciliation/reconciled_missed_aug_16.dta")

#reshaping
rec_aug19_reshaped <- rec_aug19%>%
  select(-c(dropped))%>%
  pivot_longer(cols=eng_participation:p3_leg_q05_sub1,names_to = "Question",values_to = "Answer",
               values_drop_na = TRUE)
write_dta(rec_aug19_reshaped,"3_clean/read_replace/rec_aug19_reshaped.dta")

#FIRST ENTRY#
#############
first_aug19 <- read_dta("3_clean/first_entry/first_missed_aug_19.dta")

#reshaping
first_aug19_reshaped <- first_aug19%>%
  pivot_longer(cols=eng_participation:p3_leg_q05_sub1,names_to = "Question",values_to = "Answer_first",
               values_drop_na = TRUE)
write_dta(first_aug19_reshaped,"3_clean/first_entry/first_aug19_reshaped.dta")

#SECOND ENTRY#
##############
second_aug19<-read_dta("3_clean/second_entry/second_missed_aug_19.dta")

#reshaping
second_aug19_reshaped <- second_aug19%>%
  pivot_longer(cols=eng_participation:p3_leg_q05_sub1,names_to = "Question",values_to = "Answer_second",
               values_drop_na = TRUE)
write_dta(second_aug19_reshaped,"3_clean/second_entry/second_aug19_reshaped.dta")


#RESHAPING BACK TO WIDER FORMAT####
###################################

#FIRST ENTRY#
#############
first_aug19_reshaped_corrected <- read_dta("3_clean/first_entry/first_reshaped_corrected_aug19.dta")

first_corrected_aug19 <- first_aug19_reshaped_corrected%>%
  pivot_wider(id_cols = c("district","sector","school","grade","student_code"),
              names_from =Question,values_from = Answer_first)

#LABELLING THE DATASET APPROPRIATELY#
#####################################
first_clean <- read_dta("3_clean/first_entry/Final Assessment First Entry Clean.dta")
first_clean_labels<- first_clean %>% 
  look_for() %>% 
  select(Question=variable, Label=label)
filtery <- names(first_corrected_aug19)
first_clean_labels <- subset(first_clean_labels,Question%in%filtery)

var.labels <- first_clean_labels%>%
  pivot_wider(names_from = Question,values_from = Label)

label(first_corrected_aug19) <- as.list(var.labels[match(names(first_corrected_aug19), # Assign labels to data frame variables
                                                     names(var.labels))])
write_dta(first_corrected_aug19,"3_clean/first_entry/first_corrected_aug19.dta")

#SECOND ENTRY#
#############
second_aug19_reshaped_corrected <- read_dta("3_clean/second_entry/second_reshaped_corrected_aug19.dta")

second_corrected_aug19 <- second_aug19_reshaped_corrected%>%
  pivot_wider(id_cols = c("district","sector","school","grade","student_code"),
              names_from =Question,values_from = Answer_second)

#LABELLING THE DATASET APPROPRIATELY#
#####################################
second_clean <- read_dta("3_clean/second_entry/Final Assessment Second Entry Clean.dta")
second_clean_labels<- second_clean %>% 
  look_for() %>% 
  select(Question=variable, Label=label)
filtery <- names(second_corrected_aug19)
second_clean_labels <- subset(second_clean_labels,Question%in%filtery)

var.labels <- second_clean_labels%>%
  pivot_wider(names_from = Question,values_from = Label)

label(second_corrected_aug19) <- as.list(var.labels[match(names(second_corrected_aug19), # Assign labels to data frame variables
                                                          names(var.labels))])
write_dta(second_corrected_aug19,"3_clean/second_entry/second_corrected_aug19.dta")
