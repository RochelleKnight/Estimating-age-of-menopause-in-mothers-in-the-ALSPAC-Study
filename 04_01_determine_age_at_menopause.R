rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Estimating-age-of-menopause-in-mothers-in-the-ALSPAC-Study")

library(haven)
library(dplyr)
library(lubridate)     

df <- readRDS(paste0(filestore,"cohort_lmp_variables.rds"))
use <- which(!is.na(df$date_1) | !is.na(df$date_2) | !is.na(df$date_3) | !is.na(df$date_4) | !is.na(df$date_t)| !is.na(df$date_u) | !is.na(df$date_v) | !is.na(df$date_y))
df <- df[use,]

# Determine those who have had surgical menopause
source("04_02_determine_surgical_menopause.R")

# Exclude those who have reported periods stopped by surgery but have not provided a date or age at surgery
length(which(df$exclude_participant == 1))
df <- df %>% filter(exclude_participant == 0)
df$exclude_participant <- NULL

########################################################################
############# Checking inconsistencies and replacing date LMP ##################

# Remove replicated data where a clinic and questionnaire have the same date (month/year) of completion 
source("04_03_replace_clinic_data_with_questionnaire_data.R")

table(df_duplicates_removed %>% select(period3_1,period12_1), useNA = "always")
table(df_duplicates_removed %>% select(period3_2,period12_2), useNA = "always")
table(df_duplicates_removed %>% select(period3_3,period12_3), useNA = "always")
table(df_duplicates_removed %>% select(period3_4,period12_4), useNA = "always")
table(df_duplicates_removed %>% select(period3_t,period12_t), useNA = "always")
table(df_duplicates_removed %>% select(period3_v,period12_v), useNA = "always")
table(df_duplicates_removed %>% select(period3_y,period12_y), useNA = "always")

length(which(!is.na(df_duplicates_removed$date_1) | !is.na(df_duplicates_removed$date_2) | !is.na(df_duplicates_removed$date_3) | !is.na(df_duplicates_removed$date_4) | !is.na(df_duplicates_removed$date_t)| !is.na(df_duplicates_removed$date_u) | !is.na(df_duplicates_removed$date_v) | !is.na(df_duplicates_removed$date_y)))

# Order all variables by date of collection
# Remove unneeded variables
source("04_04_date_order_variables.R")

save.image(paste0(filestore,"age_at_menopause_workspace.RData"))
#load(paste0(filestore,"age_at_menopause_workspace.RData"))
#menopause_df <- readRDS(paste0(filestore,"cohort_date_ordered.rds"))
source("04_05_determine_date_LMP.R")


################################################################################
################################# Time since FMP ###############################
fmp_df$fmp_for_counts <- fmp_df$fmp
fmp_df$time_since_fmp <- floor(time_length(difftime(fmp_df[,"date_attendance_fmp"], fmp_df[,"fmp"]), "days"))
length(which(fmp_df$time_since_fmp > 365))

# Add in estimates using Questionnaire MB
source("04_06_straw_mb.R")
fmp_df <- fmp_df %>% left_join(straw_mb)
fmp_df$diff_age_mb <- fmp_df$age_mb - fmp_df$age_final

use <- which(fmp_df$time_since_fmp <= 365 
             & fmp_df$straw_mb == "post_menopause"
             & fmp_df$diff <= 3)

fmp_df$date_attendance_fmp[use] <- fmp_df$date_mb[use]
fmp_df$age_meno_from_mb <- NA_integer_
fmp_df$age_meno_from_mb[use] <- 1

fmp_df$time_since_fmp <- floor(time_length(difftime(fmp_df[,"date_attendance_fmp"], fmp_df[,"fmp"]), "days"))
length(which(fmp_df$time_since_fmp > 365))

# Has had menopause
menopause <- which(fmp_df$time_since_fmp > 365)
fmp_df$menopause <- 0
fmp_df$menopause[menopause] <- 1
fmp_df$fmp[which(fmp_df$time_since_fmp <= 365)] <- NA

# Age at menopause
for (i in 1:8) {
  fmp_df[,paste0("age_menopause_",i)] <- NA_integer_
  fmp_df$time_since_fmp_years <- time_length(difftime(fmp_df[,paste0("date_",i)], fmp_df[,"fmp"]), "years")
  fmp_df[,paste0("age_menopause_",i)] <- floor(fmp_df[,paste0("age_",i)] - fmp_df$time_since_fmp_years)
}


# Compute row-wise maximum with proper handling of all-NA rows
fmp_df$age_menopause <- apply(fmp_df[,paste0("age_menopause_",c(1:8))], 1, function(x) if (all(is.na(x))) NA else max(x, na.rm = TRUE))
mean(fmp_df$age_menopause, na.rm = T)

table(fmp_df$age_menopause)
length(which(fmp_df$menopause == 1))
length(which(!is.na(fmp_df$age_menopause)))
length(which(fmp_df$menopause == 0 & !is.na(fmp_df$fmp)))
length(which(!is.na(fmp_df$fmp)))

# Add in estimates from Questionnaire U
unique(df$U1021)
df$U1021 <- ifelse(df$U1021 <0, NA, df$U1021)
length(which(!is.na(df$U1021)))

fmp_df <- fmp_df %>% left_join(df %>% select(aln,U1021,periods_stopped_surgery_age))
replace <- which(!is.na(fmp_df$U1021) & !is.na(fmp_df$periods_stopped_surgery_age)
                 & fmp_df$periods_stopped_surgery_age <= fmp_df$U1021)

fmp_df$U1021[replace] <- NA

source("04_06_age_last_period_tidy.R")

fmp_df <- fmp_df %>%
  left_join(tmp %>%
              select(aln,age_menopause_using_age_last_period))

replace <- which(!is.na(fmp_df$age_menopause_using_age_last_period) & !is.na(fmp_df$periods_stopped_surgery_age)
                 & fmp_df$periods_stopped_surgery_age <= fmp_df$age_menopause_using_age_last_period)

fmp_df$age_menopause_using_age_last_period[replace] <- NA

# Self-reported age at menopause
fmp_df$age_menopause_self_report <- pmax(fmp_df$U1021, fmp_df$age_menopause_using_age_last_period, na.rm = TRUE)
replace <- which(fmp_df$age_menopause_self_report < fmp_df$age_final - 2 & is.na(fmp_df$age_menopause))
fmp_df$age_menopause_self_report[replace] <- NA

fmp_df$age_menopause <- ifelse(is.na(fmp_df$age_menopause),fmp_df$age_menopause_self_report,fmp_df$age_menopause)
fmp_df$menopause[which(!is.na(fmp_df$age_menopause))] <- 1

# Keep self-reported age at menopause for data note results
fmp_df$age_menopause_self_report <- pmax(fmp_df$U1021, fmp_df$age_menopause_using_age_last_period, na.rm = TRUE)

fmp_df$U1021 <- NULL
fmp_df$age_menopause_using_age_last_period <- NULL

mean(fmp_df$age_menopause, na.rm = T)
length(which(fmp_df$menopause == 1))
length(which(!is.na(fmp_df$age_menopause)))

mean(fmp_df$age_menopause[which(fmp_df$age_menopause > 40)], na.rm = T)
length(which(fmp_df$age_menopause > 40))


# Save data set
saveRDS(fmp_df, file = paste0(filestore,"fmp_df_new.rds"))
