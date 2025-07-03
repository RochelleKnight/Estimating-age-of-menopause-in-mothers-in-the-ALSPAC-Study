rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Estimating-age-of-menopause-in-mothers-in-the-ALSPAC-Study")

library(haven)
library(dplyr)
library(lubridate)  
library(ggplot2)

# Load cohort data set
df <- readRDS(paste0(filestore,"cohort_lmp_variables.rds"))

# Number of G0 women after withdrawal of consent 
length(unique(df$aln))

# Number of women responding to at least one time point
# Number with at least one time point
length(which(!is.na(df$date_1) | !is.na(df$date_2) | !is.na(df$date_3) | !is.na(df$date_4) | !is.na(df$date_t)| !is.na(df$date_u) | !is.na(df$date_v) | !is.na(df$date_y)))

# Number excluded due to reporting a surgical menopause but no date or age reported
tmp <- readRDS(paste0(filestore,"cohort_surgical_menopause.rds"))
length(which(tmp$exclude_participant == 1))

# Number of women with at least one time point after exclusions/censoring
tmp <- readRDS(paste0(filestore,"cohort_date_ordered.rds"))
length(which(!is.na(tmp$date_1)))

# Mean number of responses
# Function to count non-NA values in each row
count_non_na <- function(row) {
  sum(!is.na(row))
}

# Apply the function to each row
tmp$total_timepoints <- apply(tmp[,paste0("date_",1:8)], 1, count_non_na)

# Mean number of responses
mean(tmp$total_timepoints[which(!is.na(tmp$date_1))])
table(tmp$total_timepoints[which(!is.na(tmp$date_1))], useNA = "always")
1629+949+686

median(tmp$total_timepoints[which(!is.na(tmp$date_1))])

# Number with at least one date of LMP
tmp <- readRDS(paste0(filestore,"fmp_df_new.rds"))
length(which(!is.na(tmp$date_1) & (!is.na(tmp$datelmp_1) | !is.na(tmp$datelmp_2) | !is.na(tmp$datelmp_3) | !is.na(tmp$datelmp_4) |
                                    !is.na(tmp$datelmp_5) | !is.na(tmp$datelmp_6) | !is.na(tmp$datelmp_7) | !is.na(tmp$datelmp_8))))


# Number with an age at menopause
length(which(!is.na(tmp$age_menopause)))

# Average age at menopause
mean(tmp$age_menopause, na.rm = T)
median(tmp$age_menopause, na.rm = T)

# SD age at menopause
sd(tmp$age_menopause, na.rm = T)

# Range age at menopause
min(tmp$age_menopause, na.rm = T)
max(tmp$age_menopause, na.rm = T)

# Age at menopause from algorithm
length(which(!is.na(tmp$fmp) & !is.na(tmp$age_menopause)))
use <- which(!is.na(tmp$fmp))

# Average age at menopause
mean(tmp$age_menopause[use], na.rm = T)

# SD age at menopause
sd(tmp$age_menopause[use], na.rm = T)

# Range age at menopause
range(tmp$age_menopause[use], na.rm = T)

# Self-reported
length(which(is.na(tmp$fmp) & !is.na(tmp$age_menopause)))
use <- which(is.na(tmp$fmp) & !is.na(tmp$age_menopause))

# Average age at menopause
mean(tmp$age_menopause[use], na.rm = T)

# SD age at menopause
sd(tmp$age_menopause[use], na.rm = T)

# Range age at menopause
range(tmp$age_menopause[use], na.rm = T)

# Number with menopause before 40
length(which(tmp$age_menopause < 40))
length(which(tmp$age_menopause >55))

# Number with both self-report and algorithm age menopause
use <- which(!is.na(tmp$fmp) & !is.na(tmp$age_menopause_self_report))

cor(tmp$age_menopause[use],tmp$age_menopause_self_report[use])

# Age at final attended timepoint for those with no age at menopause
tmp1 <- tmp %>% select(aln,age_menopause,age_menopause_self_report,paste0("date_",1:8),paste0("datelmp_",1:8),paste0("age_",1:8),
                       paste0("period12_",1:8),paste0("use_straw_",1:8))


tmp1 <- tmp1 %>% filter(is.na(age_menopause) & !is.na(date_1))

# Get column names of those columns
date_col_names <- paste0("date_",1:8)

# Apply across rows
tmp1$last_date_col <- apply(tmp1[date_col_names], 1, function(x) {
  last_non_na <- tail(which(!is.na(x)), 1)
  if (length(last_non_na) == 0) {
    return(NA)
  } else {
    return(date_col_names[last_non_na])
  }
})

tmp1$last_date_col <- gsub("date_", "", tmp1$last_date_col)
tmp1$date_final <- as.Date(NA)
tmp1$datelmp_final <- as.Date(NA)
tmp1$age_final <- NA_integer_
tmp1$period_12_final <- NA_integer_
tmp$use_straw_final <- NA_integer_

for (i in 1:nrow(tmp1)) {
  if (!is.na(tmp1$last_date_col[i])) {
    date_col <- paste0("date_", tmp1$last_date_col[i])
    datelmp_col <- paste0("datelmp_", tmp1$last_date_col[i])
    age_col <- paste0("age_", tmp1$last_date_col[i])
    period12_col <- paste0("period12_", tmp1$last_date_col[i])
    straw_final_col <- paste0("use_straw_", tmp1$last_date_col[i])
    
    tmp1$date_final[i] <- tmp1[i, date_col]
    tmp1$datelmp_final[i] <- tmp1[i, datelmp_col]
    tmp1$age_final[i] <- tmp1[i, age_col]
    tmp1$period_12_final[i] <- tmp1[i,period12_col]
    tmp1$use_straw_final[i] <- tmp1[i,straw_final_col]
  }
}

# Mean age at final timepoint
mean(tmp1$age_final,na.rm = T)
sd(tmp1$age_final,na.rm = T)
range(tmp1$age_final,na.rm = T)


# Those with LMP date
length(which(!is.na(tmp1$datelmp_final)))

# Number who we know are postmenopause
length(which(is.na(tmp1$datelmp_final) & tmp1$period_12_final == 0
       & is.na(tmp1$use_straw_final)))

# Age of those we know are postmenopause
use <- which(is.na(tmp1$datelmp_final) & tmp1$period_12_final == 0
             & is.na(tmp1$use_straw_final))

mean(tmp1$age_final[use],na.rm = T)
sd(tmp1$age_final[use],na.rm = T)
range(tmp1$age_final[use],na.rm = T)

# Number who go in and out of menopause
tmp <- readRDS(paste0(filestore,"fmp_df_new.rds"))
tmp1 <- tmp %>% select(aln,age_menopause,paste0("date_",1:8),paste0("datelmp_",1:8)) %>% filter(!is.na(datelmp_2))

tmp1[,paste0("diff_",1:8)] <- NA

for(i in 1:8){
  tmp1[,paste0("diff_",i)] <- as.numeric(tmp1[,paste0("date_",i)] - tmp1[,paste0("datelmp_",i)])
}

tmp1$issue <- NA


for(i in 1:nrow(tmp1)){
  diff <- as.numeric(unlist(tmp1[i,paste0("diff_",1:8)]))
  greater_365 <- min(which(diff > 365)) 
  less_365 <- max(which(diff <= 365))
  
  if(!is.infinite(greater_365) & !is.infinite(less_365) & greater_365 < less_365){
    tmp1$issue[i] <- T
  }
}

table(tmp1$issue)

length(which(tmp1$issue == T & is.na(tmp1$age_menopause)))


# Mean time between timepoints
tmp1 <- tmp %>% select(aln,paste0("date_",1:8)) %>% filter(!is.na(date_1) & !is.na(date_2))

tmp1[,paste0("diff_",1:7)] <- NA

date_cols <- 1:8

# Get column names of those columns
date_col_names <- names(df)[date_cols]

# Apply across rows
df$last_date_col <- apply(df[date_cols], 1, function(x) {
  last_non_na <- tail(which(!is.na(x)), 1)
  if (length(last_non_na) == 0) {
    return(NA)
  } else {
    return(date_col_names[last_non_na])
  }
})

for(i in 1:7){
  tmp1[,paste0("diff_",i)] <- as.numeric(tmp1[,paste0("date_",i+1)] - tmp1[,paste0("date_",i)])
}

mean(as.matrix(tmp1[,paste0("diff_",1:7)]), na.rm = TRUE)/365.25

max(as.matrix(tmp1[,paste0("diff_",1:7)]), na.rm = TRUE)/365.25

# Histogram of age at menopause
tmp <- readRDS(paste0(filestore,"fmp_df_new.rds"))
hist(tmp$age_menopause)
ggplot(tmp, aes(x = age_menopause)) +
  geom_histogram(binwidth = 1, fill = "grey", color = "black") +
  labs(x = "Age at menopause", y = "Frequency") +
  theme_bw()

ggsave("Histogram age at menopause.png", width = 12, height = 6)

