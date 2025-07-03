# Get date of attendance and age at each questionnaire
# Remove any reported ages where there is no date of attendance

source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Estimating-age-of-menopause-in-mothers-in-the-ALSPAC-Study")

library(haven)
library(dplyr)
library(lubridate) 
library(tidyr)

source("01_date_attendance-function.R")

# Load cohort data set
df <- read_dta(paste0(filestore,"cohort.dta"))
df <- as.data.frame(df)

# FoM 1
df <- date_attendance(df, "fm1a010a", "fm1a010b", "1",F)
df <- age_attendance(df, "fm1a011", "1")

# Any missing age?
nrow(df %>% filter(!is.na(date_1) & is.na(age_1)))

# FoM 2
df<- date_attendance(df,"fm2a010a","fm2a010b","2",F)
df <- age_attendance(df, "fm2a011", "2")

# Any missing age?
nrow(df %>% filter(!is.na(date_2) & is.na(age_2)))

# FoM 3
df<- date_attendance(df,"fm3a010a","fm3a010b","3",F)
df <- age_attendance(df, "fm3a011", "3")

# Any missing age?
nrow(df %>% filter(!is.na(date_3) & is.na(age_3)))

# FoM 4
df <- date_attendance(df, "fm4a010a", "fm4a010b", "4",F)
df <- age_attendance(df, "fm4a011", "4")

# Any missing age?
nrow(df %>% filter(!is.na(date_4) & is.na(age_4)))

# Questionnaire A
df <- date_attendance(df, "a910mm", "a910yy", "a",T)
df <- age_attendance(df,"a901","a")

# Any missing age?
nrow(df %>% filter(!is.na(date_a) & is.na(age_a)))

# Questionnaire B
df <- date_attendance(df, "b922", "b923", "b",T)
df <- age_attendance(df,"b925","b")

# Any missing age?
nrow(df %>% filter(!is.na(date_b) & is.na(age_b)))

# Questionnaire C
df <- date_attendance(df, "c992", "c993", "c",T)
df <- age_attendance(df, "c994", "c")

# Any missing age?
nrow(df %>% filter(!is.na(date_c) & is.na(age_c)))

# Questionnaire D
df <- date_attendance(df, "d992", "d993", "d",T)
df <- age_attendance(df, "d994", "d")

# Any missing age?
nrow(df %>% filter(!is.na(date_d) & is.na(age_d)))

# Questionnaire E
df <- date_attendance(df, "e690", "e691", "e",T)
df <- age_attendance(df, "e694", "e")

# Any missing age?
nrow(df %>% filter(!is.na(date_e) & is.na(age_e)))

# Questionnaire F
df <- date_attendance(df, "f990", "f991", "f",T)
df <- age_attendance(df, "f992", "f")

# Any missing age?
nrow(df %>% filter(!is.na(date_f) & is.na(age_f)))

# Questionnaire G
df <- date_attendance(df, "g991", "g992", "g",T)
df <- age_attendance(df, "g994", "g")

# Any missing age?
nrow(df %>% filter(!is.na(date_g) & is.na(age_g)))

# Questionnaire H
df <- date_attendance(df, "h990", "h990a", "h",T)
df <- age_attendance(df, "h992", "h")

# Any missing age?
nrow(df %>% filter(!is.na(date_h) & is.na(age_h)))

# Questionnaire J
df <- date_attendance(df, "j910", "j910a", "j",T)
df <- age_attendance(df, "j912", "j")

# Any missing age?
nrow(df %>% filter(!is.na(date_j) & is.na(age_j)))

# Questionnaire K
df <- date_attendance(df, "k9990a", "k9990b", "k",F)
df <- age_attendance(df, "k9996a", "k")

# Any missing age?
nrow(df %>% filter(!is.na(date_k) & is.na(age_k)))

# Questionnaire L
df <- date_attendance(df, "l9990a", "l9990b", "l",F)
df <- age_attendance(df, "l9996a", "l")

# Any missing age?
nrow(df %>% filter(!is.na(date_l) & is.na(age_l)))

# Questionnaire M
df <- date_attendance(df, "m9990a", "m9990b", "m",F)
df <- age_attendance(df, "m9996a", "m")

# Any missing age?
nrow(df %>% filter(!is.na(date_m) & is.na(age_m)))

# Questionnaire N
df <- date_attendance(df, "n9990a", "n9990b", "n",F)
df <- age_attendance(df, "n9992", "n")

# Any missing age?
nrow(df %>% filter(!is.na(date_n) & is.na(age_n)))

# Questionnaire P
df <- date_attendance(df, "p9990a", "p9990b", "p",F)
df <- age_attendance(df, "p9996a", "p")

# Any missing age?
nrow(df %>% filter(!is.na(date_p) & is.na(age_p)))

# Questionnaire Q
df <- date_attendance(df, "q9990a", "q9990b", "q",F)
df <- age_attendance(df, "q9996a", "q")

# Any missing age?
nrow(df %>% filter(!is.na(date_q) & is.na(age_q)))

# Questionnaire R
df <- date_attendance(df, "r9990a", "r9990b", "r",F)
df <- age_attendance(df, "r9996a", "r")

# Any missing age?
nrow(df %>% filter(!is.na(date_r) & is.na(age_r)))

# Questionnaire S
df <- date_attendance(df, "s9990a", "s9990b", "s",F)
df <- age_attendance(df, "s9996a", "s")

# Any missing age?
nrow(df %>% filter(!is.na(date_s) & is.na(age_s)))

# Questionnaire T
df <- date_attendance(df, "t9990a", "t9990b", "t",F)
df <- age_attendance(df, "t9994", "t")

# Any missing age?
nrow(df %>% filter(!is.na(date_t) & is.na(age_t)))

# Questionnaire U
df <- date_attendance(df, "U2010", "U2011", "u",F)
df <- age_attendance(df, "U2021", "u")

# Any missing age?
nrow(df %>% filter(!is.na(date_u) & is.na(age_u)))

# Questionnaire V
df <- date_attendance(df, "V9990A", "V9990B", "v",F)
df <- age_attendance(df, "V9996", "v")

# Any missing age?
nrow(df %>% filter(!is.na(date_v) & is.na(age_v)))

# Questionnaire W
df <- date_attendance(df, "W8011", "W8012", "w",F)
df <- age_attendance(df, "W8030", "w")

# Any missing age?
nrow(df %>% filter(!is.na(date_w) & is.na(age_w)))

# Questionnaire XB
df <- date_attendance(df, "xb990", "xb991", "xb",F)
df <- age_attendance(df, "xb995", "xb")

# Any missing age?
nrow(df %>% filter(!is.na(date_xb) & is.na(age_xb)))

# Questionnaire Y
df <- date_attendance(df, "Y9990", "Y9991", "y",F)
df <- age_attendance(df, "Y9992", "y")

# Any missing age?
nrow(df %>% filter(!is.na(date_y) & is.na(age_y)))

# Questionnaire Z
df <- date_attendance(df, "Z6000", "Z6001", "z",F)
df <- age_attendance(df, "Z6500", "z")

# Any missing age?
nrow(df %>% filter(!is.na(date_z) & is.na(age_z)))

# Questionnaire MA
df <- date_attendance(df, "MA9021", "MA9022", "ma",F)
df <- age_attendance(df, "MA9510", "ma")

# Any missing age?
nrow(df %>% filter(!is.na(date_ma) & is.na(age_ma)))

# Questionnaire MB
df <- date_attendance(df, "MB9021", "MB9022", "mb",F)
df <- age_attendance(df, "MB9510", "mb")

# Any missing age?
nrow(df %>% filter(!is.na(date_mb) & is.na(age_mb)))

# COVID I
df <- date_attendance_with_day(df, "covid1m_9620", "covid1m_9621","covid1m_9622", "covid_i",F)
df <- age_attendance(df, "covid1m_9650", "covid_i")

# Any missing age?
nrow(df %>% filter(!is.na(date_covid_i) & is.na(age_covid_i)))

# COVID II
df <- date_attendance_with_day(df, "covid2m_9620", "covid2m_9621","covid2m_9622", "covid_ii",F)
df <- age_attendance(df, "covid2m_9650", "covid_ii")

# Any missing age?
nrow(df %>% filter(!is.na(date_covid_ii) & is.na(age_covid_ii)))

# COVID III
df <- date_attendance_with_day(df, "covid3m_9620", "covid3m_9621","covid3m_9622", "covid_iii",F)
df <- age_attendance(df, "covid3m_9650", "covid_iii")

# Any missing age?
nrow(df %>% filter(!is.na(date_covid_iii) & is.na(age_covid_iii)))

# COVID IV
df <- date_attendance_with_day(df, "covid4m_9620", "covid4m_9621","covid4m_9622", "covid_iv",F)
df <- age_attendance(df, "covid4m_9650", "covid_iv")

# Any missing age?
nrow(df %>% filter(!is.na(date_covid_iv) & is.na(age_covid_iv)))

# COVID V
df <- date_attendance_with_day(df, "covid5m_9620", "covid5m_9621", "covid5m_9622", "covid_v",F)
df <- age_attendance(df, "covid5m_9650", "covid_v")

# Any missing age?
nrow(df %>% filter(!is.na(date_covid_v) & is.na(age_covid_v)))

# COVID VI
df <- date_attendance_with_day(df, "covid6m_9620", "covid6m_9621", "covid6m_9622", "covid_vi",F)
df <- age_attendance(df, "covid6m_9650", "covid_vi")

# Any missing age?
nrow(df %>% filter(!is.na(date_covid_vi) & is.na(age_covid_vi)))

df <- df %>% select(aln,((contains("date_") & !contains("date_string")) | contains("age_")))

saveRDS(df, file = paste0(filestore,"date_age_attendance_raw.rds"))
df <- readRDS(paste0(filestore,"date_age_attendance_raw.rds"))

# Pivot the data to long format
suffix <- colnames(df)[grep("date_",colnames(df))]
suffix <- gsub("date_","",suffix)

# Get the median date of attendance for all timepoints
# If a date of attendance is not available for a attended timepoint, we will use the median date of attendance
date_median <- as.data.frame(matrix(nrow = length(suffix), ncol = 2))
colnames(date_median) <- c("time_point","date")
date_median$date <- as.Date(date_median$date)

for (i in 1:length(suffix)) {
  suffix_i <- suffix[i]
  date_median$time_point[i] <- paste0("_",suffix_i)
  date_median$date[i] <- median(df[,paste0("date_",suffix_i)],na.rm = T)
}

saveRDS(date_median, file = paste0(filestore,"date_attendance_median.rds"))

# Pivot df to long format
suffix <- colnames(df)[grep("date_",colnames(df))]
suffix <- gsub("date","",suffix)
suffix <- suffix[-grep("covid",suffix)]
suffix <- paste0("(", paste(suffix, collapse = "|"), ")$")

df_long <- df %>%
  select(colnames(df)[-grep("covid",colnames(df))]) %>%
  pivot_longer(
    cols = -aln,
    names_to = c(".value", "time_point"),
    names_pattern = paste0("(.*)", suffix)
  )

# COVID columns
suffix <- colnames(df)[grep("date_",colnames(df))]
suffix <- gsub("date","",suffix)
suffix <- suffix[grep("covid",suffix)]
suffix <- paste0("(", paste(suffix, collapse = "|"), ")$")

df_long_covid <- df %>%
  select(c("aln",colnames(df)[grep("covid",colnames(df))])) %>%
  pivot_longer(
    cols = -aln,
    names_to = c(".value", "time_point"),
    names_pattern = paste0("(.*)", suffix)
  )

df_long <- rbind(df_long,df_long_covid)

df_long <- df_long %>%
  arrange(aln)  

rm(df_long_covid)

# EPDS dataframe
epds_colnames <- c("f","g","h","k","l","n","r","t","v","y","covid_v","mb")
epds <- read.csv(paste0(filestore,"epds_prorated_scores.csv"))
epds <- epds %>% select(aln,paste0("epds_prorated_",epds_colnames))

# Replace -1 with NA across all columns
epds <- epds %>%
  mutate(across(paste0("epds_prorated_",epds_colnames), ~ replace(., . == -1, NA)))

epds_long <- epds %>%
  pivot_longer(
    cols = starts_with("epds_prorated_"), # Columns to pivot
    names_to = "time_point",                      
    values_to = "epds_prorated"  
  )%>%
  mutate(time_point = (gsub("epds_prorated", "", time_point)))


df_long <- df_long %>% left_join(epds_long, by = c("aln","time_point"))

df_long <- df_long %>%
  left_join(date_median, by = c("time_point"), suffix = c("_df1", "_df2")) %>%
  mutate(date = case_when(is.na(date_df1) & (!is.na(age) | !is.na(epds_prorated)) ~ date_df2,
                          TRUE ~ date_df1)) %>%
  select(aln,time_point,date,age)

rm(epds,epds_long)

violating_ids <- df_long %>%
  filter(!is.na(date) & !is.na(age)) %>%
  arrange(aln, date) %>%  # Sort by id and date
  group_by(aln) %>%  # Group by id
  mutate(age_check = age < lag(age)) %>%  # Check if age decreases
  filter(age_check) %>%  # Filter rows where age decreases
  pull(aln) %>%  # Get the ids
  unique()  # Ensure unique ids


for(id in violating_ids){
  tmp <- df_long[which(df_long$aln == id),]
  tmp <- tmp %>% filter(!is.na(date) & !is.na(age)) %>% arrange(date)
  
  
  if(length(tmp$age) > 2){
    for(i in 2:(length(tmp$age)-1)){
      if(tmp$age[i-1] == (tmp$age[i]-1) & tmp$age[i+1] == (tmp$age[i]-1)){
        tmp$age[i] <- tmp$age[i]-1
      }
    }
  }
  
  tmp1 <- df_long[which(df_long$aln == id),] %>% select(-age)
  tmp1 <- tmp1 %>% left_join(tmp, by = c("aln", "time_point", "date"))
  
  df_long <- df_long[-which(df_long$aln == id),]
  df_long <- rbind(df_long, tmp1)
  
}

violating_ids <- df_long %>%
  filter(!is.na(date) & !is.na(age)) %>%
  arrange(aln, date) %>%  # Sort by id and date
  group_by(aln) %>%  # Group by id
  mutate(age_check = age < lag(age)) %>%  # Check if age decreases
  filter(age_check) %>%  # Filter rows where age decreases
  pull(aln) %>%  # Get the ids
  unique()  # Ensure unique ids


for(id in violating_ids){
  tmp <- df_long[which(df_long$aln == id),]
  tmp <- tmp %>% filter(!is.na(date) & !is.na(age)) %>% arrange(date)
  
  set_to_na <- c()
  
  if(length(tmp$age) > 2){
    for(i in 1:length(tmp$age)){
      if( all(tmp$age[-i] == sort(tmp$age[-i]))){
        set_to_na <- append(set_to_na,i)
        #changed_ids <- append(changed_ids,id)
      }
    }
  }
  
  tmp$age[set_to_na] <- NA
  
  tmp1 <- df_long[which(df_long$aln == id),] %>% select(-age)
  tmp1 <- tmp1 %>% left_join(tmp, by = c("aln", "time_point", "date"))
  
  df_long <- df_long[-which(df_long$aln == id),]
  df_long <- rbind(df_long, tmp1)
  
}

violating_ids <- df_long %>%
  filter(!is.na(date) & !is.na(age)) %>%
  arrange(aln, date) %>%  # Sort by id and date
  group_by(aln) %>%  # Group by id
  mutate(age_check = age < lag(age)) %>%  # Check if age decreases
  filter(age_check) %>%  # Filter rows where age decreases
  pull(aln) %>%  # Get the ids
  unique()  # Ensure unique ids

for (id in violating_ids) {
  tmp <- df_long[which(df_long$aln == id), ]
  tmp <- tmp %>% filter(!is.na(date) & !is.na(age)) %>% arrange(date)
  
  set_to_na <- c()
  
  # Case 2: Remove 2 ages
  if (length(tmp$age) > 3) {  # Ensure at least 2 ages remain after removal
    combs <- combn(1:length(tmp$age), 2)  # Generate all pairs of indices
    for (j in 1:ncol(combs)) {
      indices_to_remove <- combs[, j]
      if (all(tmp$age[-indices_to_remove] == sort(tmp$age[-indices_to_remove]))) {
        set_to_na <- unique(c(set_to_na, indices_to_remove))
        #changed_ids <- append(changed_ids, id)
      }
    }
  }
  
  # Set identified ages to NA
  tmp$age[set_to_na] <- NA
  
  # Update df_long
  tmp1 <- df_long[which(df_long$aln == id), ] %>% select(-age)
  tmp1 <- tmp1 %>% left_join(tmp, by = c("aln", "time_point", "date"))
  
  df_long <- df_long[-which(df_long$aln == id), ]
  df_long <- rbind(df_long, tmp1)
}

violating_ids <- df_long %>%
  filter(!is.na(date) & !is.na(age)) %>%
  arrange(aln, date) %>%  # Sort by id and date
  group_by(aln) %>%  # Group by id
  mutate(age_check = age < lag(age)) %>%  # Check if age decreases
  filter(age_check) %>%  # Filter rows where age decreases
  pull(aln) %>%  # Get the ids
  unique()  # Ensure unique ids

source("change_ages.R")

df_long <- as.data.frame(df_long)
# Estimating any missing ages
df_complete_ages <- df_long

ids <- unique(unlist(df_long %>% filter(!is.na(date) & is.na(age)) %>% select(aln)))

for(i in ids){
  tmp <- df_long[df_long$aln == i,] %>% filter(!is.na(date)) %>% arrange(date)
  
  missing_ages <- tmp$time_point[which(!is.na(tmp$date) & is.na(tmp$age))]
  
  tmp1 <- df_long %>% filter(aln == i
                             & !is.na(date)
                             & !is.na(age)) %>% arrange(date)
  #k=missing_ages[2]

  if(nrow(tmp1) >0 & length(missing_ages) > 0){
    for(k in missing_ages){
      non_na_rows <- which(!is.na(tmp$age))
     
      previous_age <- ifelse(length(non_na_rows[non_na_rows < which(tmp$time_point == k)])>0,
                             tmp$age[max(non_na_rows[non_na_rows < which(tmp$time_point == k)])],NA_integer_)
      
      following_age <- ifelse(length(non_na_rows[non_na_rows > which(tmp$time_point == k)])>0,
                              tmp$age[min(non_na_rows[non_na_rows > which(tmp$time_point == k)])],NA_integer_)
      
      date_diff <- time_length(difftime(as.Date(tmp[tmp$time_point == k,"date"]), tmp1$date), "years")
      ages <- tmp1$age
      
      if(!is.na(previous_age) & !is.na(following_age)){
        replace_age <- ifelse(floor(mean(ages + date_diff, na.rm = T)) >= previous_age & floor(mean(ages + date_diff, na.rm = T)) <= following_age,
                              floor(mean(ages + date_diff, na.rm = T)), ceiling(mean(ages + date_diff, na.rm = T)))
      }else if(is.na(previous_age) & !is.na(following_age)){
        replace_age <- ifelse(floor(mean(ages + date_diff, na.rm = T)) <= following_age,
                              floor(mean(ages + date_diff, na.rm = T)), ceiling(mean(ages + date_diff, na.rm = T)))
      }else if(!is.na(previous_age) & is.na(following_age)){
        replace_age <- ifelse(floor(mean(ages + date_diff, na.rm = T)) >= previous_age,
                              floor(mean(ages + date_diff, na.rm = T)), ceiling(mean(ages + date_diff, na.rm = T)))
      }
      
      df_complete_ages[df_complete_ages$aln == i & df_complete_ages$time_point == k,"age"] <- replace_age
    }
  }
}

violating_ids <- df_complete_ages %>%
  filter(!is.na(date) & !is.na(age)) %>%
  arrange(aln, date) %>%  # Sort by id and date
  group_by(aln) %>%  # Group by id
  mutate(age_check = age < lag(age)) %>%  # Check if age decreases
  filter(age_check) %>%  # Filter rows where age decreases
  pull(aln) %>%  # Get the ids
  unique()  # Ensure unique ids

source("change_ages_2.R")

library(lubridate)

# Process the data
result <- df_complete_ages %>%
  group_by(aln) %>%
  arrange(aln, date) %>%  # Ensure dates are in order within each ID
  mutate(
    # Calculate years since the first non-NA date
    first_non_na_date = first(date[!is.na(date)]),
    years_since_first_date = as.numeric(difftime(date, first_non_na_date, units = "days")) / 365.25,
    # Calculate expected age (if date is not NA)
    expected_age = ifelse(!is.na(date), first(age[!is.na(age)]) + floor(years_since_first_date), NA),
    # Flag mismatches (if age is not NA)
    age_mismatch = ifelse(!is.na(age) & !is.na(expected_age), abs(age - expected_age) > 5, NA)
  ) %>%
  ungroup()

# Filter rows with mismatches for review
mismatched_rows <- result %>%
  filter(age_mismatch == TRUE)

length(unique(mismatched_rows$aln))
mismatched_rows$diff <- mismatched_rows$age - mismatched_rows$expected_age


# Pivot to wide format
df_complete_ages <- df_complete_ages %>% arrange(aln)

# Pivot to wide format
df_wide <- df_complete_ages %>%
  pivot_wider(
    id_cols = aln,  # Keep `id` as identifier
    names_from = time_point,  # Take names for new columns from `timepoint`
    values_from = c(date, age)  # Take values from `date` and `age`
  )

colnames(df_wide) <- gsub("__","_",colnames(df_wide))

saveRDS(df_wide, file = paste0(filestore,"date_age_attendance.rds"))
