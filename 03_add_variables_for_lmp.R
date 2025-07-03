rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Estimating-age-of-menopause-in-mothers-in-the-ALSPAC-Study")

library(haven)
library(dplyr)
library(lubridate)     

# Load cohort data set
df <- readRDS(paste0(filestore,"cohort_negatives_set_to_na.rds"))

########################################## FoM 1 #######################################

#Date of attendance
# See "01_get_date_and_age_at_attendance.R" for creating date of attendance variable
unique(df$date_1)

# Age at attendance
# See "get_date_and_age_at_attendance.R" for creating age at attendance variable
unique(df$age_1)
class(df$age_1)
table(df$age_1, useNA = "always")

# Current using contraceptive method (oral contraceptive, injection, implant or coil)
#fm1ob100: Currently taking oral contraceptives: FOM1
#fm1ob101: Currently using contraceptive injection: FOM1
#fm1ob121: Reason why periods stopped: FOM1
unique(df$fm1ob100)
unique(df$fm1ob101)
unique(df$fm1ob121)

# 1 (Yes), 2 (No), other (Missing, Unresolvable, Did not attend clinic etc)
df <- df %>% mutate(contracept_1 =  case_when(fm1ob100 == 2 | fm1ob101 == 2 ~ 0,
                                              TRUE ~ NA))

df <- df %>% mutate(contracept_1 =  case_when(fm1ob100 == 1 | fm1ob101 == 1 | fm1ob121 >=7 & fm1ob121 <12   ~ 1,
                                              TRUE ~ contracept_1))

table(df$contracept_1, useNA = "always")

# Current taking hormone replacement therapt (HRT)
#fm1ob110: Currently taking hormone replacement therapy: FOM1
unique(df$fm1ob110)
df$hrt_1 <- as.numeric(df$fm1ob110)

table(df$hrt_1, useNA = "always")
df <- df %>% mutate(hrt_1 = case_when(hrt_1 == 2 ~ 0,
                                      hrt_1 < 0 ~ NA,
                                      TRUE ~ hrt_1))
table(df$hrt_1, useNA = "always")

#Had period/ menstrual bleeding last 12 months
#fm1ob120: Had a period/menstrual bleeding in the previous 12 months: FOM1
unique(df$fm1ob120)
df$period12_1 <- as.numeric(df$fm1ob120)

#Recode values
table(df$period12_1, useNA = "always")
df <- df %>% mutate(period12_1 = case_when(period12_1 == 2 ~ 0,
                                           period12_1 < 0 ~ NA,
                                           TRUE ~ period12_1))
table(df$period12_1, useNA = "always")

#Had period/ menstrual bleeding last 3 months
#fm1ob126: Period/menstrual bleeding in last 3 months: FOM1
unique(df$fm1ob126)
df$period3_1 <- as.numeric(df$fm1ob126)

# If no period in last 12 months, recode missing as no
table(df$period3_1, useNA = "always")
df$period3_1 <- ifelse(df$period12_1 == 0 & is.na(df$period3_1) & !is.na(df$period12_1) & !is.na(df$date_1),2, df$period3_1)

table(df$period3_1, useNA = "always")

# Recode did not have recent period recoded as no
df <- df %>% mutate(period3_1 = case_when(period3_1 == 2 ~ 0,
                                          period3_1 <0 ~ NA,
                                          TRUE ~ period3_1))
table(df$period3_1, useNA = "always")

# Ensure that period_12 and period_3 align in the information they are giving
# i.e. they are not giving opposing information
# Set any opposing information to missing

# When reported no period in the last 12 months but period3 is missing, set period3 to no
replace <- which(is.na(df$period3_1) & df$period12_1==0 & !is.na(df$date_1))
df$period3_1[replace] <- 0

# When reported period in the last 3 months but period12 is missing, set period12 to yes
replace <- which(df$period3_1 == 1 & is.na(df$period12_1) & !is.na(df$date_1))
df$period12_1[replace] <- 1

# When reported period in the last 3 months but no period in the last 12 months,
# set both to missing 
replace <- which(df$period3_1 == 1 & df$period12_1 == 0 & !is.na(df$date_1))
df$period3_1[replace] <- NA
df$period12_1[replace] <- NA
length(replace)

table(df$period3_1,df$period12_1,useNA = "always")

#fm1ob121: Reason why periods stopped: FOM1
unique(df$fm1ob121)
table(df$fm1ob121)

# If reported reason why period stopped, set period3 to no
replace <- which(df$period3_1 == 1 & df$period12_1 == 0 & !is.na(df$fm1ob121) & !is.na(df$date_1) )
df$period3_1[replace] <- 0

# Else set period12 to no
replace <- which(df$period3_1 == 1 & df$period12_1 == 0 & is.na(df$fm1ob121) & !is.na(df$date_1) )
df$period12_1[replace] <- 0

table(df %>% select(period3_1,period12_1), useNA = "always")

#Reason why periods stopped
#fm1ob121: Reason why periods stopped: FOM1
unique(df$fm1ob121)
table(df$fm1ob121)

# Exclude those (only at this time point) who report periods stopping due to
# surgery, chemotherapy or radiation therapy pregnancy or breast feeding or contraception
df <- df %>% mutate(exclude_timepoint_1 = case_when(is.na(fm1ob121) ~ NA,
                                                    fm1ob121 < 0 ~ NA,
                                                    fm1ob121 %in% c(2,4,5,7,8,9,10,11,12,13) ~ 1,
                                                    TRUE ~ 0))

table(df$exclude_timepoint_1, useNA = "always")

#Date of last menstrual period
#fm1ob123: Day of last period: FOM1
#fm1ob124: Month of last period: FOM1
#fm1ob125 Year of last period: FOM1
unique(df$fm1ob123)
unique(df$fm1ob124)
unique(df$fm1ob125)

df$dlmp_1 <- as.numeric(df$fm1ob123)
df$mlmp_1 <- as.numeric(df$fm1ob124)
df$ylmp_1 <- as.numeric(df$fm1ob125)

# Recode - replace values <0 with NA
table(df$dlmp_1, useNA = "always")
table(df$mlmp_1, useNA = "always")
table(df$ylmp_1, useNA = "always")

df <- df %>% mutate(dlmp_1 = case_when(dlmp_1 <=0 | dlmp_1 >31 ~NA,
                                       TRUE ~ dlmp_1))

df <- df %>% mutate(mlmp_1 = case_when(mlmp_1 <=0 | mlmp_1 >12 ~NA,
                                       TRUE ~ mlmp_1))

df <- df %>% mutate(ylmp_1 = case_when(ylmp_1 <=0 | ylmp_1 > as.numeric(format(Sys.Date(), "%Y")) ~NA,
                                       TRUE ~ ylmp_1))

table(df$dlmp_1, useNA = "always")
table(df$mlmp_1, useNA = "always")
table(df$ylmp_1, useNA = "always")

# Age at last period
df$age_last_period_1 <- NA_integer_

# Regular periods
unique(df$fm1ob130)
table(df$fm1ob130)

df <- df %>% mutate(regperiod_1 = case_when(fm1ob130 %in% c(1,2,3) ~ 1,
                                            fm1ob130 == 4 ~ 0,
                                            TRUE ~ NA_integer_))

table(df$regperiod_1,useNA = "always")

# Remove unneeded columns
#df[,c("fm1ob100","fm1ob101","fm1ob121","fm1ob110","fm1ob120","fm1ob126","fm1ob123","fm1ob124","fm1ob125")] <- NULL

################################### FoM 2 ######################################

#Date of attendance
#Date of attendance
# See "get_date_and_age_at_attendance.R" for creating date of attendance variable
unique(df$date_2)
class(df$date_2)

# Age at attendance
# See "get_date_and_age_at_attendance.R" for creating age at attendance variable
unique(df$age_2)
class(df$age_2)
table(df$age_2, useNA = "always")

# Current using contraceptive method (oral contraceptive, injection, implant or coil)
#fm2ob100: Currently taking oral contraceptives: FOM2
#fm2ob101: Currently using contraceptive injection: FOM2
unique(df$fm2ob100)
unique(df$fm2ob101)

# 1 (Yes), 2 (No), other (Missing, Unresolvable, Did not attend clinic etc)
df <- df %>% mutate(contracept_2 =  case_when(fm2ob100 == 2 | fm2ob101 == 2 ~ 0,
                                              TRUE ~ NA))

df <- df %>% mutate(contracept_2 =  case_when(fm2ob100 == 1 | fm2ob101 == 1 ~ 1,
                                              TRUE ~ contracept_2))

table(df$contracept_2, useNA = "always")

# Current taking hormone replacement therapt (HRT)
#fm2ob110a: Currently taking Hormone Replacement Therapy tablets: FOM2
#fm2ob110b: Currently taking Hormone Replacement Therapy patches: FOM2
#fm2ob110c: Currently taking Hormone Replacement Therapy creams: FOM2
unique(df$fm2ob110a)
unique(df$fm2ob110b)
unique(df$fm2ob110c)

df <- df %>% mutate(hrt_2 = case_when(fm2ob110a==2 | fm2ob110b==2 | fm2ob110c==2 ~ 0,
                                      TRUE ~ NA))

df <- df %>% mutate(hrt_2 = case_when(fm2ob110a==1 | fm2ob110b==1 | fm2ob110c==1 ~ 1,
                                      TRUE ~ hrt_2))

table(df$hrt_2, useNA = "always")

#Had period/ menstrual bleeding last 12 months
#fm2ob120: Had a period/menstrual bleeding in the previous 12 months: FOM2
unique(df$fm2ob120)
df$period12_2 <- as.numeric(df$fm2ob120)

#Recode values
table(df$period12_2, useNA = "always")
df <- df %>% mutate(period12_2 = case_when(period12_2 == 2 ~ 0,
                                           period12_2 < 0 ~ NA,
                                           TRUE ~ period12_2))
table(df$period12_2, useNA = "always")

#Had period/ menstrual bleeding last 3 months
#fm2ob126: Period/menstrual bleeding in last 3 months: FOM2
unique(df$fm2ob126)
df$period3_2 <- as.numeric(df$fm2ob126)

# If no period in last 12 months, replace missing as no
table(df$period3_2, useNA = "always")
df$period3_2 <- ifelse(df$period12_2 == 0 & is.na(df$period3_2) & !is.na(df$period12_2) & !is.na(df$date_2),
                       2,df$period3_2)
table(df$period3_2, useNA = "always")

# Recode <0 as NA
df <- df %>% mutate(period3_2 = case_when(period3_2 == 2 ~ 0,
                                          period3_2 <0 ~ NA,
                                          TRUE ~ period3_2))
table(df$period3_2, useNA = "always")

# Ensure that period_12 and period_3 align in the information they are giving
# i.e. they are not giving opposing information

# When reported period in the last 3 months but period12 is missing, set period12 to yes
replace <- which(df$period3_2 == 1 & is.na(df$period12_2) & !is.na(df$date_2))
df$period12_2[replace] <- 1

# When reported no period in the last 12 months but period3 is missing, set period3 to no
replace <- which(is.na(df$period3_2) & df$period12_2==0 & !is.na(df$date_2))
df$period3_2[replace] <- 0

# When reported period in the last 3 months but no period in the last 12 months,
# set both to missing 
replace <- which(df$period3_2 == 1 & df$period12_2 == 0 & !is.na(df$date_2))
df$period3_2[replace] <- NA
df$period12_2[replace] <- NA
length(replace)

table(df$period3_2,df$period12_2,useNA = "always")

#fm2ob121: Reason why periods stopped: FOM1
unique(df$fm2ob121)
table(df$fm2ob121)

# If reported reason why period stopped, set period3 to no
replace <- which(df$period3_2 == 1 & df$period12_2 == 0 & !is.na(df$fm2ob121) & !is.na(df$date_2) )
df$period3_2[replace] <- 0

# Else set period12 to yes
replace <- which(df$period3_2 == 1 & df$period12_2 == 0 & is.na(df$fm2ob121) & !is.na(df$date_2) )
df$period12_2[replace] <- 1

table(df %>% select(period3_2,period12_2), useNA = "always")

#Reason why periods stopped
#fm2ob121: Reason why periods stopped: FOM2
unique(df$fm2ob121)
table(df$fm2ob121)

# Exclude those (only at this time point) who report periods stopping due to
# surgery, chemotherapy or radiation therapy pregnancy or breast feeding or contraception
df <- df %>% mutate(exclude_timepoint_2 = case_when(is.na(fm2ob121) ~ NA,
                                                    fm2ob121 < 0 ~ NA,
                                                    fm2ob121 %in% c(2,3,5) ~ 1,
                                                    TRUE ~ 0))

table(df$exclude_timepoint_2, useNA = "always")

#Date of last menstrual period
#fm2ob123: Day of last period: FOM2
#fm2ob124: Month of last period: FOM2
#fm2ob124a Year of last period: FOM2
unique(df$fm2ob123)
unique(df$fm2ob124)
unique(df$fm2ob124a)
unique(df$fm2ob125)

df$dlmp_2 <- as.numeric(df$fm2ob123)
df$mlmp_2 <- as.numeric(df$fm2ob124)
df$ylmp_2 <- as.numeric(df$fm2ob124a)
df$ylmp_2a <- as.numeric(df$fm2ob125)

# Recode - replace values <0 with NA
table(df$dlmp_2, useNA = "always")
table(df$mlmp_2, useNA = "always")
table(df$ylmp_2, useNA = "always")
table(df$ylmp_2a, useNA = "always")

df <- df %>% mutate(dlmp_2 = case_when(dlmp_2 <=0 | dlmp_2 >31 ~NA,
                                       TRUE ~ dlmp_2))

df <- df %>% mutate(mlmp_2 = case_when(mlmp_2 <=0 | mlmp_2 >12 ~NA,
                                       TRUE ~ mlmp_2))

df <- df %>% mutate(ylmp_2 = case_when(ylmp_2 <=0 | ylmp_2 > as.numeric(format(Sys.Date(), "%Y")) ~NA,
                                       TRUE ~ ylmp_2))

df <- df %>% mutate(ylmp_2a = case_when(ylmp_2a <=0 | ylmp_2a > as.numeric(format(Sys.Date(), "%Y")) ~NA,
                                        TRUE ~ ylmp_2a))

table(df$dlmp_2, useNA = "always")
table(df$mlmp_2, useNA = "always")
table(df$ylmp_2, useNA = "always")
table(df$ylmp_2a, useNA = "always")

# Replace any missing year of LMP (fm2ob124a) with fm2ob125
# If the women could not remember the data of their LMP, they could just report the year which was reported as a separate variable.
# If we use fm2ob125, remove any days/months reported as these were reported without a year
replace <- which(!is.na(df$ylmp_2a) & is.na(df$ylmp_2))
df$ylmp_2[replace] <- df$ylmp_2a[replace]

df$dlmp_2[replace] <- NA
df$mlmp_2[replace] <- NA

# Age at last period
df$age_last_period_2 <- NA_integer_

# Regular periods
unique(df$fm2ob130)
table(df$fm2ob130)

df <- df %>% mutate(regperiod_2 = case_when(fm2ob130 %in% c(1,2,3) ~ 1,
                                            fm2ob130 == 4 ~ 0,
                                            TRUE ~ NA_integer_))

table(df$regperiod_2,useNA = "always")

# Remove unneeded columns
#df[,c("ylmp_2a","fm2ob100","fm2ob101","fm2ob121","fm2ob110a","fm2ob110b","fm2ob110c","fm2ob120","fm2ob126","fm2ob123","fm2ob124","fm2ob124a","fm2ob125")] <- NULL

################################### FoM 3 ######################################

#Date of attendance
#Date of attendance
#Date of attendance
# See "get_date_and_age_at_attendance.R" for creating date of attendance variable
unique(df$date_3)
class(df$date_3)

# Age at attendance
# See "get_date_and_age_at_attendance.R" for creating age at attendance variable
unique(df$age_3)
class(df$age_3)
table(df$age_3, useNA = "always")

# Current using contraceptive method (oral contraceptive, injection, implant or coil)
#fm3ob100: Currently taking oral contraceptives: FOM3
#fm3ob101: Currently using contraceptive injection: FOM3
#fm3ob121: Reason why periods stopped: FOM3
unique(df$fm3ob100)
unique(df$fm3ob101)
unique(df$fm3ob121)

# 1 (Yes), 2 (No), other (Missing, Unresolvable, Did not attend clinic etc)
df <- df %>% mutate(contracept_3 =  case_when(fm3ob100 == 2 | fm3ob101 == 2 ~ 0,
                                              TRUE ~ NA))

df <- df %>% mutate(contracept_3 =  case_when(fm3ob100 == 1 | fm3ob101 == 1 | fm3ob121 == 5 ~ 1,
                                              TRUE ~ contracept_3))

table(df$contracept_3, useNA = "always")

# Current taking hormone replacement therapt (HRT)
#fm3ob110a: Currently taking Hormone Replacement Therapy tablets: FOM2
#fm3ob110b: Currently taking Hormone Replacement Therapy patches: FOM2
#fm3ob110c: Currently taking Hormone Replacement Therapy creams: FOM2
unique(df$fm3ob110a)
unique(df$fm3ob110b)
unique(df$fm3ob110c)

df <- df %>% mutate(hrt_3 = case_when(fm3ob110a==2 | fm3ob110b==2 | fm3ob110c==2 ~ 0,
                                      TRUE ~ NA))

df <- df %>% mutate(hrt_3 = case_when(fm3ob110a==1 | fm3ob110b==1 | fm3ob110c==1 ~ 1,
                                      TRUE ~ hrt_3))

table(df$hrt_3, useNA = "always")

#Had period/ menstrual bleeding last 12 months
#fm3ob120: Had a period/menstrual bleeding in the previous 12 months: FOM3
unique(df$fm3ob120)
df$period12_3 <- as.numeric(df$fm3ob120)

#Recode values
table(df$period12_3, useNA = "always")
df <- df %>% mutate(period12_3 = case_when(period12_3 == 2 ~ 0,
                                           period12_3 < 0 ~ NA,
                                           TRUE ~ period12_3))
table(df$period12_3, useNA = "always")

#Had period/ menstrual bleeding last 3 months
#fm3ob126: Period/menstrual bleeding in last 3 months: FOM3
unique(df$fm3ob126)
df$period3_3 <- as.numeric(df$fm3ob126)

# If no period in last 12 months, replace missing as no
table(df$period3_3, useNA = "always")
df$period3_3 <- ifelse(df$period12_3 == 0 & is.na(df$period3_3) & !is.na(df$period12_3) & !is.na(df$date_3),
                       2,df$period3_3)
table(df$period3_3, useNA = "always")

# Recode <0 as NA
df <- df %>% mutate(period3_3 = case_when(period3_3 == 2 ~ 0,
                                          period3_3 <0 ~ NA,
                                          TRUE ~ period3_3))
table(df$period3_3, useNA = "always")

# Ensure that period_12 and period_3 align in the information they are giving
# i.e. they are not giving opposing information

# When reported period in the last 3 months but period12 is missing, set period12 to yes
replace <- which(df$period3_3 == 1 & is.na(df$period12_3) & !is.na(df$date_3))
df$period12_3[replace] <- 1

# When reported no period in the last 12 months but period3 is missing, set period3 to no
replace <- which(is.na(df$period3_3) & df$period12_3==0 & !is.na(df$date_3))
df$period3_3[replace] <- 0

# When reported period in the last 3 months but no period in the last 12 months,
# set both to missing 
replace <- which(df$period3_3 == 1 & df$period12_3 == 0 & !is.na(df$date_3))
df$period3_3[replace] <- NA
df$period12_3[replace] <- NA
length(replace)

table(df$period3_3,df$period12_3,useNA = "always")

#fm2ob121: Reason why periods stopped: FOM1
unique(df$fm3ob121)
table(df$fm3ob121)

# If reported reason why period stopped, set period3 to no
replace <- which(df$period3_3 == 1 & df$period12_3 == 0 & !is.na(df$fm3ob121) & !is.na(df$date_3) )
df$period3_3[replace] <- 0

# Else set period12 to yes
replace <- which(df$period3_3 == 1 & df$period12_3 == 0 & is.na(df$fm3ob121) & !is.na(df$date_3) )
df$period12_3[replace] <- 1

table(df %>% select(period3_3,period12_3), useNA = "always")

#Reason why periods stopped
#fm3ob121: Reason why periods stopped: FOM3
unique(df$fm3ob121)

# Exclude those (only at this timepoint) who report periods stopping due to
# surgery, chemotherapy or radiation therapy pregnancy or breast feeding or contraception
df <- df %>% mutate(exclude_timepoint_3 = case_when(is.na(fm3ob121) ~ NA,
                                                    fm3ob121 < 0 ~ NA,
                                                    fm3ob121 %in% c(2,3,5,6) ~ 1,
                                                    TRUE ~ 0))

table(df$exclude_timepoint_3, useNA = "always")

#Date of last menstrual period
#fm3ob123: Day of last period: FOM3
#fm3ob124: Month of last period: FOM3
#fm3ob124a: Year of last period: FOM3
#fm3ob125: Year participants periods stopped
unique(df$fm3ob123)
unique(df$fm3ob124)
unique(df$fm3ob124a)
unique(df$fm3ob125)

df$dlmp_3 <- as.numeric(df$fm3ob123)
df$mlmp_3 <- as.numeric(df$fm3ob124)
df$ylmp_3 <- as.numeric(df$fm3ob124a)
df$ylmp_3a <- as.numeric(df$fm3ob125)

# Recode - replace values <0 with NA
table(df$dlmp_3, useNA = "always")
table(df$mlmp_3, useNA = "always")
table(df$ylmp_3, useNA = "always")
table(df$ylmp_3a, useNA = "always")

df <- df %>% mutate(dlmp_3 = case_when(dlmp_3 <=0 | dlmp_3 >31 ~NA,
                                       TRUE ~ dlmp_3))

df <- df %>% mutate(mlmp_3 = case_when(mlmp_3 <=0 | mlmp_3 >12 ~NA,
                                       TRUE ~ mlmp_3))

df <- df %>% mutate(ylmp_3 = case_when(ylmp_3 <=0 | ylmp_3 > as.numeric(format(Sys.Date(), "%Y")) ~NA,
                                       TRUE ~ ylmp_3))

df <- df %>% mutate(ylmp_3a = case_when(ylmp_3a <=0 | ylmp_3a > as.numeric(format(Sys.Date(), "%Y")) ~NA,
                                        TRUE ~ ylmp_3a))

table(df$dlmp_3, useNA = "always")
table(df$mlmp_3, useNA = "always")
table(df$ylmp_3, useNA = "always")
table(df$ylmp_3a, useNA = "always")

# Replace any missing year of LMP (fm3ob124a) with fm3ob125
# If the women could not remeber the data of their LMP, they could just report the year which was reported as a separate variable.
# If we use fm3ob125, remove any days/months reported as these were reported without a year
replace <- which(!is.na(df$ylmp_3a) & is.na(df$ylmp_3))
df$ylmp_3[replace] <- df$ylmp_3a[replace]

df$dlmp_3[replace] <- NA
df$mlmp_3[replace] <- NA

# Age at last period
df$age_last_period_3 <- NA_integer_

# Regular periods
unique(df$fm3ob130)
table(df$fm3ob130)

df <- df %>% mutate(regperiod_3 = case_when(fm3ob130 %in% c(1,2,3) ~ 1,
                                            fm3ob130 == 4 ~ 0,
                                            TRUE ~ NA_integer_))

table(df$regperiod_3,useNA = "always")


# Remove unneeded columns
#df[,c("ylmp_3a","fm3ob100","fm3ob101","fm3ob121","fm3ob110a","fm3ob110b","fm3ob110c","fm3ob120","fm3ob126","fm3ob123","fm3ob124","fm3ob124a","fm3ob125")] <- NULL

################################### FoM 4 ######################################

#Date of attendance
#Date of attendance
#Date of attendance
# See "get_date_and_age_at_attendance.R" for creating date of attendance variable
unique(df$date_4)
class(df$date_4)

# Age at attendance
# See "get_date_and_age_at_attendance.R" for creating age at attendance variable
unique(df$age_4)
class(df$age_4)
table(df$age_4, useNA = "always")

# Current using contraceptive method (oral contraceptive, injection, implant or coil)
#fm4ob100: Currently taking oral contraceptives: FOM4
#fm4ob101: Currently using contraceptive injection: FOM4
#fm4ob121: Reason why periods stopped: FOM4
unique(df$fm4ob100)
unique(df$fm4ob101)
unique(df$fm4ob121)

# 1 (Yes), 2 (No), other (Missing, Unresolvable, Did not attend clinic etc)
df <- df %>% mutate(contracept_4 = case_when(fm4ob100 ==  2 | fm4ob101 == 2 ~ 0,
                                             TRUE ~ NA))

df <- df %>% mutate(contracept_4 = case_when(fm4ob100 ==  1 | fm4ob101 == 1 | fm4ob121 == 5 ~ 1,
                                             TRUE ~ contracept_4))

table(df$contracept_4, useNA = "always")

# Current taking hormone replacement therapt (HRT)
#fm4ob110a: Currently taking Hormone Replacement Therapy tablets: FOM4
#fm4ob110b: Currently taking Hormone Replacement Therapy patches: FOM4
#fm4ob110c: Currently taking Hormone Replacement Therapy creams: FOM4
unique(df$fm4ob110a)
unique(df$fm4ob110b)
unique(df$fm4ob110c)

df <- df %>% mutate(hrt_4 = case_when(fm4ob110a==2 | fm4ob110b==2 | fm4ob110c==2 ~ 0,
                                      TRUE ~ NA))

df <- df %>% mutate(hrt_4 = case_when(fm4ob110a==1 | fm4ob110b==1 | fm4ob110c==1 ~ 1,
                                      TRUE ~ hrt_4))

table(df$hrt_4, useNA = "always")

#Had period/ menstrual bleeding last 12 months
#fm4ob120: Had a period/menstrual bleeding in the previous 12 months: FOM4
unique(df$fm4ob120)
df$period12_4 <- as.numeric(df$fm4ob120)

#Recode values
table(df$period12_4, useNA = "always")
df <- df %>% mutate(period12_4 = case_when(period12_4 == 2 ~ 0,
                                           period12_4 < 0 ~ NA,
                                           TRUE ~ period12_4))
table(df$period12_4, useNA = "always")

#Had period/ menstrual bleeding last 3 months
#fm4ob126: Period/menstrual bleeding in last 3 months: FOM4
unique(df$fm4ob126)
df$period3_4 <- as.numeric(df$fm4ob126)

# If no period in last 12 months, replace missing as no
table(df$period3_4, useNA = "always")
df$period3_4 <- ifelse(df$period12_4 == 0 & is.na(df$period3_4) & !is.na(df$period12_4) & !is.na(df$date_4),
                       2,df$period3_4)
table(df$period3_4, useNA = "always")

# Recode <0 as NA
df <- df %>% mutate(period3_4 = case_when(period3_4 == 2 ~ 0,
                                          period3_4 <0 ~ NA,
                                          TRUE ~ period3_4))
table(df$period3_4, useNA = "always")

# Ensure that period_12 and period_3 align in the information they are giving
# i.e. they are not giving opposing information

# When reported period in the last 3 months but period12 is missing, set period12 to yes
replace <- which(df$period3_4 == 1 & is.na(df$period12_4) & !is.na(df$date_4))
df$period12_4[replace] <- 1

# When reported no period in the last 12 months but period3 is missing, set period3 to no
replace <- which(is.na(df$period3_4) & df$period12_4==0 & !is.na(df$date_4))
df$period3_4[replace] <- 0

# When reported period in the last 3 months but no period in the last 12 months,
# set both to missing 
replace <- which(df$period3_4 == 1 & df$period12_4 == 0 & !is.na(df$date_4))
df$period3_4[replace] <- NA
df$period12_4[replace] <- NA
length(replace)

table(df$period3_4,df$period12_4,useNA = "always")

#fm2ob121: Reason why periods stopped: FOM1
unique(df$fm4ob121)
table(df$fm4ob121)

# If reported reason why period stopped, set period3 to no
replace <- which(df$period3_4 == 1 & df$period12_4 == 0 & !is.na(df$fm4ob121) & !is.na(df$date_4) )
df$period3_4[replace] <- 0

# Else set period12 to yes
replace <- which(df$period3_4 == 1 & df$period12_4 == 0 & is.na(df$fm4ob121) & !is.na(df$date_4) )
df$period12_3[replace] <- 1

table(df %>% select(period3_4,period12_4), useNA = "always")

#Reason why periods stopped
#fm4ob121: Reason why periods stopped: FOM4
unique(df$fm4ob121)

# Exclude those (only at this timepoint) who report periods stopping due to
# surgery, chemotherapy or radiation therapy pregnancy or breast feeding or contraception
df <- df %>% mutate(exclude_timepoint_4 = case_when(is.na(fm4ob121) ~ NA,
                                                    fm4ob121 < 0 ~ NA,
                                                    fm4ob121 %in% c(2,3,5,6) ~ 1,
                                                    TRUE ~ 0))
table(df$exclude_timepoint_4, useNA = "always")

#Date of last menstrual period
#fm4ob123: Day of last period: FOM4
#fm4ob124: Month of last period: FOM4
#fm4ob124a Year of last period: FOM4
unique(df$fm4ob123)
unique(df$fm4ob124)
unique(df$fm4ob124a)
unique(df$fm4ob125)

df$dlmp_4 <- as.numeric(df$fm4ob123)
df$mlmp_4 <- as.numeric(df$fm4ob124)
df$ylmp_4 <- as.numeric(df$fm4ob124a)
df$ylmp_4a <- as.numeric(df$fm4ob125)

# Recode - replace values <0 with NA
table(df$dlmp_4, useNA = "always")
table(df$mlmp_4, useNA = "always")
table(df$ylmp_4, useNA = "always")
table(df$ylmp_4a, useNA = "always")

df <- df %>% mutate(dlmp_4 = case_when(dlmp_4 <=0 | dlmp_4 >31 ~NA,
                                       TRUE ~ dlmp_4))

df <- df %>% mutate(mlmp_4 = case_when(mlmp_4 <=0 | mlmp_4 >12 ~NA,
                                       TRUE ~ mlmp_4))

df <- df %>% mutate(ylmp_4 = case_when(ylmp_4 <=0 | ylmp_4 > as.numeric(format(Sys.Date(), "%Y")) ~NA,
                                       TRUE ~ ylmp_4))

df <- df %>% mutate(ylmp_4a = case_when(ylmp_4a <=0 | ylmp_4a > as.numeric(format(Sys.Date(), "%Y")) ~NA,
                                        TRUE ~ ylmp_4a))

table(df$dlmp_4, useNA = "always")
table(df$mlmp_4, useNA = "always")
table(df$ylmp_4, useNA = "always")
table(df$ylmp_4a, useNA = "always")

# Replace any missing year of LMP (fm4ob124a) with fm4ob125
# If the women could not remeber the data of their LMP, they could just report the year which was reported as a separate variable.
# If we use fm4ob125, remove any days/months reported as these were reported without a year
replace <- which(!is.na(df$ylmp_4a) & is.na(df$ylmp_4))
df$ylmp_4[replace] <- df$ylmp_4a[replace]

df$dlmp_4[replace] <- NA
df$mlmp_4[replace] <- NA

# Age at last period
df$age_last_period_4 <- NA_integer_

# Regular periods
unique(df$fm4ob130)
table(df$fm4ob130)

df <- df %>% mutate(regperiod_4 = case_when(fm4ob130 %in% c(1,2,3) ~ 1,
                                            fm4ob130 == 4 ~ 0,
                                            TRUE ~ NA_integer_))

table(df$regperiod_4,useNA = "always")

# Remove unneeded columns
#df[,c("ylmp_4a","fm4ob100","fm4ob101","fm4ob121","fm4ob110a","fm4ob110b","fm4ob110c","fm4ob120","fm4ob126","fm4ob123","fm4ob124","fm4ob124a","fm4ob125")] <- NULL

########################### Questionnaire T ####################################

#Date respondent completed questionnaire
#Date of attendance
#Date of attendance
# See "get_date_and_age_at_attendance.R" for creating date of attendance variable
unique(df$date_t)
class(df$date_t)

# Age at attendance
# See "get_date_and_age_at_attendance.R" for creating age at attendance variable
unique(df$age_t)
class(df$age_t)
table(df$age_t, useNA = "always")

# Current using contraceptive method (oral contraceptive, injection, implant or coil)
#t4521: Form of contraception used in past three months: the pill
#t4522: Form of contraception used in past three months: IUCD/coil
unique(df$t4521)
unique(df$t4522)

# 1 (Yes), 2 (No), other (Missing, Unresolvable, Did not attend clinic etc)
df <- df %>% mutate(contracept_t = case_when(t4521 == 2 | t4522 == 2 ~ 0,
                                             TRUE ~ NA))

df <- df %>% mutate(contracept_t = case_when(t4521 == 1 | t4522 == 1 ~ 1,
                                             TRUE ~ contracept_t))

table(df$contracept_t, useNA = "always")

# Current taking hormone replacement therapt (HRT)
#t4961: Respondent currently on HRT
unique(df$t4961)

table(df$t4961, useNA = "always")
df <- df %>% mutate(hrt_t = case_when(t4961 == 2 ~ 0,
                                      t4961 == 1 ~ 1,
                                      TRUE ~ NA))

table(df$hrt_t, useNA = "always")

#Had period/ menstrual bleeding last 12 months
#t4800: Respondent had a period or menstrual bleeding in last twelve months
unique(df$t4800)
df$period12_t <- as.numeric(df$t4800)

#Recode values
table(df$period12_t, useNA = "always")
df <- df %>% mutate(period12_t = case_when(period12_t == 2 ~ 0,
                                           period12_t < 0 ~ NA,
                                           TRUE ~ period12_t))
table(df$period12_t, useNA = "always")

#Had period/ menstrual bleeding last 3 months
#t4810: Respondent had a period or menstrual bleeding in the last three months
unique(df$t4810)
df$period3_t <- as.numeric(df$t4810)

# If no period in last 12 months, replace missing as no
table(df$period3_t, useNA = "always")
df$period3_t <- ifelse(df$period12_t == 0 & is.na(df$period3_t) & !is.na(df$period12_t) & !is.na(df$date_t),
                       2,df$period3_t)
table(df$period3_t, useNA = "always")

# Recode <0 as NA
df <- df %>% mutate(period3_t = case_when(period3_t == 2 ~ 0,
                                          period3_t <0 ~ NA,
                                          TRUE ~ period3_t))
table(df$period3_t, useNA = "always")

# Ensure that period_12 and period_3 align in the information they are giving
# i.e. they are not giving opposing information

# When reported period in the last 3 months but period12 is missing, set period12 to yes
replace <- which(df$period3_t == 1 & is.na(df$period12_t) & !is.na(df$date_t))
df$period12_t[replace] <- 1

# When reported no period in the last 12 months but period3 is missing, set period3 to no
replace <- which(is.na(df$period3_t) & df$period12_t==0 & !is.na(df$date_t))
df$period3_t[replace] <- 0

# When reported period in the last 3 months but no period in the last 12 months,
# set both to missing 
replace <- which(df$period3_t == 1 & df$period12_t == 0 & !is.na(df$date_t))
df$period3_t[replace] <- NA
df$period12_t[replace] <- NA
length(replace)

table(df$period3_t,df$period12_t,useNA = "always")

#Reason why periods stopped
#t4801: Periods stopped by: surgery
#t4802: Periods stopped by: chemotherapy or radiation therapy
#t4803: Periods stopped by: pregnancy or breastfeeding
#t4804: Periods stopped by: no obvious reason/menopause
#t4805: Periods stopped by: other reason
unique(df$t4801)
unique(df$t4802)
unique(df$t4803)
unique(df$t4804)
unique(df$t4805)

# If reported reason why period stopped, set period3 to no
replace <- which(df$period3_t == 1 & df$period12_t == 0 & !is.na(df$date_t) & (df$t4801 == 1 | df$t4802 == 1  | df$t4803 == 1 | df$t4804 == 1 | df$t4805 == 1))
df$period3_t[replace] <- 0

# Else set period12 to yes
replace <- which(df$period3_t == 1 & df$period12_t == 0 & !is.na(df$date_t) )
df$period12_t[replace] <- 1

table(df %>% select(period3_t,period12_t), useNA = "always")

#Reason why periods stopped
#t4801: Periods stopped by: surgery
#t4802: Periods stopped by: chemotherapy or radiation therapy
#t4803: Periods stopped by: pregnancy or breastfeeding
#t4804: Periods stopped by: no obvious reason/menopause
#t4805: Periods stopped by: other reason
unique(df$t4801)
unique(df$t4802)
unique(df$t4803)
unique(df$t4804)
unique(df$t4805)

# Exclude those (only at this time point) who report periods stopping due to
# surgery, chemotherapy or radiation therapy pregnancy or breast feeding or contraception
df<- df %>%
  mutate(across(c(t4801,t4802,t4803,t4804,t4805), ~ ifelse(. < 0, NA, .)))

df <- df %>% mutate(exclude_timepoint_t = case_when(t4802 == 1  | t4803 == 1 | t4805 == 1 ~ 1,
                                                    is.na(t4801) & is.na(t4802) & is.na(t4803) & is.na(t4804) & is.na(t4805) ~ NA,
                                                    TRUE ~ 0))
table(df$exclude_timepoint_t,useNA = "always")

#Date of last menstrual period
#As the questionnaire is missing day of last period this will be imputed at a later stage
#t4811: Last period: month
#t4812: Last period: year
unique(df$t4811)
unique(df$t4812)

df$dlmp_t <- NA_integer_
df$mlmp_t <- as.numeric(df$t4811)
df$ylmp_t <- as.numeric(df$t4812)

# Recode - replace values <0 with NA
table(df$dlmp_t, useNA = "always")
table(df$mlmp_t, useNA = "always")
table(df$ylmp_t, useNA = "always")

df <- df %>% mutate(dlmp_t = case_when(dlmp_t <=0 | dlmp_t >31 ~NA,
                                       TRUE ~ dlmp_t))

df <- df %>% mutate(mlmp_t = case_when(mlmp_t <=0 | mlmp_t >12 ~NA,
                                       TRUE ~ mlmp_t))

df <- df %>% mutate(ylmp_t = case_when(ylmp_t <=0 | ylmp_t > as.numeric(format(Sys.Date(), "%Y")) ~NA,
                                       TRUE ~ ylmp_t))

table(df$dlmp_t, useNA = "always")
table(df$mlmp_t, useNA = "always")
table(df$ylmp_t, useNA = "always")

# Age at last period
unique(df$t4813)
df$age_last_period_t <- as.numeric(df$t4813)
table(df$age_last_period_t)

# Regular periods
unique(df$t4837)
table(df$t4837)

df <- df %>% mutate(regperiod_t = case_when(t4837 %in% c(1,2) ~ 0,
                                            t4837 %in% c(3,4) ~ 1,
                                            TRUE ~ NA_integer_))

table(df$regperiod_t,useNA = "always")

# Remove unneeded columns
#df[,c("t4521","t4522","t4961","t4800","t4810","t4801","t4802","t4803","t4804","t4805","t4811","t4812")] <- NULL

########################### Questionnaire V ####################################

#Date respondent completed questionnaire
#Date of attendance
#Date of attendance
# See "get_date_and_age_at_attendance.R" for creating date of attendance variable
unique(df$date_v)
class(df$date_v)

# Age at attendance
# See "get_date_and_age_at_attendance.R" for creating age at attendance variable
unique(df$age_v)
class(df$age_v)
table(df$age_v, useNA = "always")

# Current using contraceptive method (oral contraceptive, injection, implant or coil)
#V4551: Pill used as contraception in past 3 months
#V4552: IUD (coil, no hormones) used as contraception in past 3 months
#V4553: IUD (with hormones) used as  contraception in past 3 months
#V4559: Injection used as contraception in past 3 months
#V4560: Implant used as contraception in past 3 months
#V4805: Respondent's periods were stopped by contraception
unique(df$V4551)
unique(df$V4552)
unique(df$V4553)
unique(df$V4559)
unique(df$V4560)
unique(df$V4805)

# 1 (Yes), 2 (No), other (Missing, Unresolvable, Did not attend clinic etc)
df <- df %>% mutate(contracept_v = case_when(V4551 ==2 | V4552 == 2 | V4553 == 2 | V4559 == 2 | V4560 == 2 ~ 0,
                                             TRUE ~ NA))

df <- df %>% mutate(contracept_v = case_when(V4551 == 1 | V4553 == 1 | V4559 == 1 | V4560 == 1 | V4805 == 1 ~ 1,
                                             TRUE ~ contracept_v))

table(df$contracept_v, useNA = "always")

# Current taking hormone replacement therapt (HRT)
#V4955: Respondent currently on HRT
unique(df$V4955)

table(df$V4955, useNA = "always")
df <- df %>% mutate(hrt_v = case_when(V4955 == 2 ~ 0,
                                      V4955 == 1 ~ 1,
                                      TRUE ~ NA))

table(df$hrt_v, useNA = "always")

#Had period/ menstrual bleeding last 12 months
#V4800: Respondent had a period or menstrual bleeding in last twelve months
unique(df$V4800)
df$period12_v <- as.numeric(df$V4800)

#Recode values
table(df$period12_v, useNA = "always")
df <- df %>% mutate(period12_v = case_when(period12_v == 2 ~ 0,
                                           period12_v < 0 ~ NA,
                                           TRUE ~ period12_v))
table(df$period12_v, useNA = "always")

#Had period/ menstrual bleeding last 3 months
#V4810: Respondent had a period or menstrual bleeding in the last three months
unique(df$V4810)
df$period3_v <- as.numeric(df$V4810)

# If no period in last 12 months, replace missing as no
table(df$period3_v, useNA = "always")
df$period3_v <- ifelse(df$period12_v == 0 & is.na(df$period3_v) & !is.na(df$period12_v) & !is.na(df$date_v),
                       2,df$period3_v)
table(df$period3_v, useNA = "always")

# Recode <0 as NA
df <- df %>% mutate(period3_v = case_when(period3_v == 2 ~ 0,
                                          period3_v <0 ~ NA,
                                          TRUE ~ period3_v))
table(df$period3_v, useNA = "always")

# Ensure that period_12 and period_3 align in the information they are giving
# i.e. they are not giving opposing information

# When reported period in the last 3 months but period12 is missing, set period12 to yes
replace <- which(df$period3_v == 1 & is.na(df$period12_v) & !is.na(df$date_v))
df$period12_v[replace] <- 1

# When reported no period in the last 12 months but period3 is missing, set period3 to no
replace <- which(is.na(df$period3_v) & df$period12_v==0 & !is.na(df$date_v))
df$period3_v[replace] <- 0

# When reported period in the last 3 months but no period in the last 12 months,
# set both to missing 
replace <- which(df$period3_v == 1 & df$period12_v == 0 & !is.na(df$date_v))
df$period3_v[replace] <- NA
df$period12_v[replace] <- NA
length(replace)

table(df$period3_v,df$period12_v,useNA = "always")

#Reason why periods stopped
#V4801: Respondent's periods were stopped by surgery
#V4802: Respondent's periods stopped by chemotherapy or radiation therapy
#V4803: Respondent's periods were stopped by pregnancy or breastfeeding
#V4804: Respondent's periods were stopped by no obvious reason/menopause
#V4805: Respondent's periods were stopped by contraception
unique(df$V4801)
unique(df$V4802)
unique(df$V4803)
unique(df$V4804)
unique(df$V4805)


# If reported reason why period stopped, set period3 to no
replace <- which(df$period3_v == 1 & df$period12_v == 0 & !is.na(df$date_v) & (df$V4801 == 1 | df$V4802 == 1  | df$V4803 == 1 | df$V4804 == 1 | df$V4805 == 1))
df$period3_v[replace] <- 0

# Else set period12 to yes
replace <- which(df$period3_v == 1 & df$period12_v == 0 & !is.na(df$date_v) )
df$period12_v[replace] <- 1

table(df %>% select(period3_v,period12_v), useNA = "always")

#Reason why periods stopped
#V4801: Respondent's periods were stopped by surgery
#V4802: Respondent's periods stopped by chemotherapy or radiation therapy
#V4803: Respondent's periods were stopped by pregnancy or breastfeeding
#V4804: Respondent's periods were stopped by no obvious reason/menopause
#V4805: Respondent's periods were stopped by contraception
unique(df$V4801)
unique(df$V4802)
unique(df$V4803)
unique(df$V4804)
unique(df$V4805)

# Exclude those (only at this timepoint) who report periods stopping due to
# surgery, chemotherapy or radiation therapy pregnancy or breast feeding or contraception
df<- df %>%
  mutate(across(c(V4801,V4802,V4803,V4804,V4805), ~ ifelse(. < 0, NA, .)))

df <- df %>% mutate(exclude_timepoint_v = case_when(V4802 == 1  | V4803 == 1 | V4805 == 1 ~ 1,
                                                    is.na(V4801) & is.na(V4802) & is.na(V4803) & is.na(V4804) & is.na(V4805) ~ NA,
                                                    TRUE ~ 0))
table(df$exclude_timepoint_v,useNA = "always")

#Date of last menstrual period
#As the questionnaire is missing day of last period this will be imputed at a later stage
#V4811: Last period: month
#V4812: Last period: year
unique(df$V4811)
unique(df$V4812)

df$dlmp_v <- NA_integer_
df$mlmp_v <- as.numeric(df$V4811)
df$ylmp_v <- as.numeric(df$V4812)

# Recode - replace values <0 with NA
table(df$dlmp_v, useNA = "always")
table(df$mlmp_v, useNA = "always")
table(df$ylmp_v, useNA = "always")

df <- df %>% mutate(dlmp_v = case_when(dlmp_v <=0 | dlmp_v >31 ~NA,
                                       TRUE ~ dlmp_v))

df <- df %>% mutate(mlmp_v = case_when(mlmp_v <=0 | mlmp_v >12 ~NA,
                                       TRUE ~ mlmp_v))

df <- df %>% mutate(ylmp_v = case_when(ylmp_v <=0 | ylmp_v > as.numeric(format(Sys.Date(), "%Y")) ~NA,
                                       TRUE ~ ylmp_v))

table(df$dlmp_v, useNA = "always")
table(df$mlmp_v, useNA = "always")
table(df$ylmp_v, useNA = "always")

# Age at last period
unique(df$V4813)
df$age_last_period_v <- as.numeric(df$V4813)
table(df$age_last_period_v)

# Regular periods
unique(df$V4837)
table(df$V4837)

df <- df %>% mutate(regperiod_v = case_when(V4837 %in% c(1,2) ~ 0,
                                            V4837 %in% c(3,4) ~ 1,
                                            TRUE ~ NA_integer_))

table(df$regperiod_v,useNA = "always")

# Remove unneeded columns
#df[,c("V4551","V4552","V4553","V4559","V4560","V4805","V4955","V4800","V4810","V4801","V4802","V4803","V4804","V4811","V4812")] <- NULL


############################## Questionnaire Y #################################

#Date respondent completed questionnaire
#Date of attendance
#Date of attendance
# See "get_date_and_age_at_attendance.R" for creating date of attendance variable
unique(df$date_y)
class(df$date_y)

# Age at attendance
# See "get_date_and_age_at_attendance.R" for creating age at attendance variable
unique(df$age_y)
class(df$age_y)
table(df$age_y, useNA = "always")

# Current using contraceptive method (oral contraceptive, injection, implant or coil)
#Y5001: Contraception in past 3 months: Pill: Y
#Y5002: Contraception in past 3 months: Intrauterine device (no hormones): Y
#Y5003: Contraception in past 3 months: Intrauterine device (with hormones): Y
#Y5008: Contraception in past 3 months: Contraceptive injection: Y
#Y5009: Contraception in past 3 months: Contraceptive implant: Y
#Y5084: Periods stopped by: Contraception: Y
unique(df$Y5001)
unique(df$Y5002)
unique(df$Y5003)
unique(df$Y5008)
unique(df$Y5009)
unique(df$Y5084)

# 1 (Yes), 2 (No), other (Missing, Unresolvable, Did not attend clinic etc)
df <- df %>% mutate(contracept_y =  case_when(Y5001 == 0 | Y5002 == 0 | Y5003 == 0 | Y5008 == 0 | Y5009 == 0 ~ 0,
                                              TRUE ~ NA))

df <- df %>% mutate(contracept_y =  case_when(Y5001 == 1 | Y5003 == 1 | Y5008 == 1 | Y5009 == 1 | df$Y5084 == 1 ~ 1,
                                              TRUE ~ contracept_y))

table(df$contracept_y, useNA = "always")

# Current taking hormone replacement therapt (HRT)
#Y5140: Respondent currently on HRT
unique(df$Y5140)
table(df$Y5140, useNA = "always")
df$hrt_y <- as.numeric(df$Y5140)

#Had period/ menstrual bleeding last 12 months
#Y5070: Respondent had a period or menstrual bleeding in last twelve months
unique(df$Y5070)
df$period12_y <- as.numeric(df$Y5070)
table(df$period12_y, useNA = "always")

#Had period/ menstrual bleeding last 3 months
#Y5100: Respondent had a period or menstrual bleeding in the last three months
unique(df$Y5100)
df$period3_y <- as.numeric(df$Y5100)

# If no period in last 12 months, replace missing as no
table(df$period3_y, useNA = "always")
df$period3_y <- ifelse(df$period12_y == 0 & is.na(df$period3_y) & !is.na(df$period12_y) & !is.na(df$date_y),
                       0,df$period3_y)
table(df$period3_y, useNA = "always")

# Recode <0 as NA
df <- df %>% mutate(period3_y = case_when(period3_y <0 ~ NA,
                                          TRUE ~ period3_y))
table(df$period3_y, useNA = "always")

# Ensure that period_12 and period_3 align in the information they are giving
# i.e. they are not giving opposing information

# When reported period in the last 3 months but period12 is missing, set period12 to yes
replace <- which(df$period3_y == 1 & is.na(df$period12_y) & !is.na(df$date_y))
df$period12_y[replace] <- 1

# When reported no period in the last 12 months but period3 is missing, set period3 to no
replace <- which(is.na(df$period3_y) & df$period12_y==0 & !is.na(df$date_y))
df$period3_y[replace] <- 0

# When reported period in the last 3 months but no period in the last 12 months,
# set both to missing 
replace <- which(df$period3_y == 1 & df$period12_y == 0 & !is.na(df$date_y))
df$period3_y[replace] <- NA
df$period12_y[replace] <- NA
length(replace)

table(df$period3_y,df$period12_y,useNA = "always")


#Reason why periods stopped
#Y5080: Periods stopped by: Surgery: Y
#Y5081: Periods stopped by: Chemotherapy/radiation therapy: Y
#Y5082: Periods stopped by: Pregnancy or breastfeeding: Y
#Y5083: Periods stopped by: Menopause: Y
#Y5084: Periods stopped by: Contraception: Y
#Y5085: Periods stopped by: Other: Y
unique(df$Y5080)
unique(df$Y5081)
unique(df$Y5082)
unique(df$Y5083)
unique(df$Y5084)
unique(df$Y5085)


# If reported reason why period stopped, set period3 to no
replace <- which(df$period3_y == 1 & df$period12_y == 0 & !is.na(df$date_y) & (df$Y5080 == 1 | df$Y5081 == 1  | df$Y5082 == 1 | df$Y5083 == 1 | df$Y5084 == 1 | df$Y5085 == 1))
df$period3_y[replace] <- 0

# Else set period12 to yes
replace <- which(df$period3_y == 1 & df$period12_y == 0 & !is.na(df$date_y) )
df$period12_y[replace] <- 1

table(df %>% select(period3_y,period12_y), useNA = "always")

#Reason why periods stopped
#Y5080: Periods stopped by: Surgery: Y
#Y5081: Periods stopped by: Chemotherapy/radiation therapy: Y
#Y5082: Periods stopped by: Pregnancy or breastfeeding: Y
#Y5083: Periods stopped by: Menopause: Y
#Y5084: Periods stopped by: Contraception: Y
#Y5085: Periods stopped by: Other: Y
unique(df$Y5080)
unique(df$Y5081)
unique(df$Y5082)
unique(df$Y5083)
unique(df$Y5084)
unique(df$Y5085)

# Exclude those (only at this timepoint) who report periods stopping due to
# surgery, chemotherapy or radiation therapy pregnancy or breast feeding or contraception
df<- df %>%
  mutate(across(c(Y5080,Y5081,Y5082,Y5083,Y5084,Y5085), ~ ifelse(. < 0, NA, .)))

df <- df %>% mutate(exclude_timepoint_y = case_when(Y5081 == 1  | Y5082 == 1 | Y5084 == 1 | Y5085 == 1  ~ 1,
                                                    is.na(Y5080) & is.na(Y5081) & is.na(Y5082) & is.na(Y5083) & is.na(Y5084) & is.na(Y5085) ~ NA,
                                                    TRUE ~ 0))
table(df$exclude_timepoint_y,useNA = "always")

#Date of last menstrual period
#As the questionnaire is missing day of last period this will be imputed at a later stage
#Y5101: Last period: month
#Y5102: Last period: year
unique(df$Y5101)
unique(df$Y5102)

df$dlmp_y <- NA_integer_
df$mlmp_y <- as.numeric(df$Y5101)
df$ylmp_y <- as.numeric(df$Y5102)

# Recode - replace values <0 with NA
table(df$dlmp_y, useNA = "always")
table(df$mlmp_y, useNA = "always")
table(df$ylmp_y, useNA = "always")

df <- df %>% mutate(dlmp_y = case_when(dlmp_y <=0 | dlmp_y >31 ~NA,
                                       TRUE ~ dlmp_y))

df <- df %>% mutate(mlmp_y = case_when(mlmp_y <=0 | mlmp_y >12 ~NA,
                                       TRUE ~ mlmp_y))

df <- df %>% mutate(ylmp_y = case_when(ylmp_y <=0 | ylmp_y > as.numeric(format(Sys.Date(), "%Y")) ~NA,
                                       TRUE ~ ylmp_y))

table(df$dlmp_y, useNA = "always")
table(df$mlmp_y, useNA = "always")
table(df$ylmp_y, useNA = "always")

# Age at last period
unique(df$Y5103)
df$age_last_period_y <- as.numeric(df$Y5103)
table(df$age_last_period_y)

# Regular periods
df$regperiod_y <- NA_integer_

# Remove unneeded columns
#df[,c("Y5001","Y5002","Y5003","Y5008","Y5009","Y5084","Y5140","Y5070","Y5100","Y5080","Y5081","Y5082","Y5083","Y5085","Y5101","Y5102")] <- NULL

# Questionnaire U
#Date respondent completed questionnaire
#Date of attendance
#Date of attendance
# See "get_date_and_age_at_attendance.R" for creating date of attendance variable
unique(df$date_u)
class(df$date_u)

# Age at attendance
# See "get_date_and_age_at_attendance.R" for creating age at attendance variable
unique(df$age_u)
class(df$age_u)
table(df$age_u, useNA = "always")

# Current using contraceptive method (oral contraceptive, injection, implant or coil)
# U1030: Respondent currently using oral contraceptive pill
# U1031: Respondent currently using contraceptive injection
# U1032: Respondent currently using contraceptive implant
# U1033: Respondent currently using contraceptive coil with hormone
# U1034: Respondent currently using contraceptive patch
# U1060: Respondent has hormones/pill to regulate periods

unique(df$U1030)
unique(df$U1031)
unique(df$U1032)
unique(df$U1033)
unique(df$U1034)
unique(df$U1060)

# 1 (Yes), 2 (No), other (Missing, Unresolvable, Did not attend clinic etc)
df <- df %>% mutate(contracept_u =  case_when(U1030 == 2 | U1031 == 2 | U1032 == 2 | U1033 == 2 | U1034 == 2 ~ 0,
                                              TRUE ~ NA))

df <- df %>% mutate(contracept_u =  case_when(U1030 == 1 | U1031 == 1 | U1032 == 1 | U1033 == 1 | df$U1034 == 1 | df$U1060 == 1 ~ 1,
                                              TRUE ~ contracept_u))

table(df$contracept_u, useNA = "always")

# Current taking hormone replacement therapt (HRT)
df$hrt_u <- NA_integer_

#Had period/ menstrual bleeding last 12 months
df$period12_u <- NA_integer_

#Had period/ menstrual bleeding last 3 months
df$period3_u <- NA_integer_

#Reason why periods stopped

# Exclude those (only at this timepoint) who report periods stopping due to
# surgery, chemotherapy or radiation therapy pregnancy or breast feeding or contraception

df$exclude_timepoint_u <- NA_integer_

#Date of last menstrual period
#U1040: Last period: day
#U1041: Last period: month
#U1042: Last period: year
unique(df$U1040)
unique(df$U1041)
unique(df$U1042)

df$dlmp_u <- as.numeric(df$U1040)
df$mlmp_u <- as.numeric(df$U1041)
df$ylmp_u <- as.numeric(df$U1042)

# Recode - replace values <0 with NA
table(df$dlmp_u, useNA = "always")
table(df$mlmp_u, useNA = "always")
table(df$ylmp_u, useNA = "always")

df <- df %>% mutate(dlmp_u = case_when(dlmp_u <=0 | dlmp_u >31 ~NA,
                                       TRUE ~ dlmp_u))

df <- df %>% mutate(mlmp_u = case_when(mlmp_u <=0 | mlmp_u >12 ~NA,
                                       TRUE ~ mlmp_u))

df <- df %>% mutate(ylmp_u = case_when(ylmp_u <=0 | ylmp_u > as.numeric(format(Sys.Date(), "%Y")) ~NA,
                                       TRUE ~ ylmp_u))

table(df$dlmp_u, useNA = "always")
table(df$mlmp_u, useNA = "always")
table(df$ylmp_u, useNA = "always")

# Age at last period
df$age_last_period_u <- NA_integer_

# Regular periods
unique(df$U1050)
table(df$U1050)

df <- df %>% mutate(regperiod_u = case_when(U1050 %in% c(1,2,3) ~ 1,
                                            U1050 %in% c(4,5) ~ 0,
                                            TRUE ~ NA_integer_))

table(df$regperiod_u,useNA = "always")

#df$U1021: Age at menopause
unique(df$U1021)
df$U1021 <- as.numeric(df$U1021)
table(df$U1021)

# Questionnaire MB
unique(df$MB4600)
table(df$MB4600)

df <- df %>% mutate(period3_mb = case_when(MB4600 == 1 ~ 1,
                                           MB4600 %in% c(2,3) ~ 0,
                                           TRUE ~ NA_integer_)) %>%
  mutate(period12_mb = case_when(MB4600 %in% c(1,2) ~ 1,
                                 MB4600 == 3 ~ 0,
                                 TRUE ~ NA_integer_))

# Ensure that period_12 and period_3 align in the information they are giving
# i.e. they are not giving opposing information

# When reported period in the last 3 months but period12 is missing, set period12 to yes
replace <- which(df$period3_mb == 1 & is.na(df$period12_mb) & !is.na(df$date_mb))
df$period12_mb[replace] <- 1

# When reported no period in the last 12 months but period3 is missing, set period3 to no
replace <- which(is.na(df$period3_mb) & df$period12_mb==0 & !is.na(df$date_mb))
df$period3_mb[replace] <- 0

# When reported period in the last 3 months but no period in the last 12 months,
# set both to missing 
replace <- which(df$period3_mb == 1 & df$period12_mb == 0 & !is.na(df$date_mb))
df$period3_mb[replace] <- NA
df$period12_mb[replace] <- NA
length(replace)

table(df$period3_mb,df$period12_mb,useNA = "always")

#Reason why periods stopped
#MB4610: Periods stopped by: Surgery: Y
#MB4620: Periods stopped by: Pregnancy or breastfeeding: Y
#MB4630: Periods stopped by: Pregnancy or breastfeeding: Y
#MB4650: Periods stopped by: Menopause: Y
#MB4640: Periods stopped by: Contraception: Y
#MB4660: Periods stopped by: Other: Y
unique(df$MB4610)
table(df$MB4610)

unique(df$MB4620)
table(df$MB4620)

unique(df$MB4630)
table(df$MB4630)

unique(df$MB4650)
table(df$MB4650)

unique(df$MB4640)
table(df$MB4640)

unique(df$MB4660)
table(df$MB4660)

# Exclude those (only at this timepoint) who report periods stopping due to
# surgery, chemotherapy or radiation therapy pregnancy or breast feeding or contraception
df <- df %>% mutate(exclude_timepoint_mb = case_when(MB4620 == 1  | MB4630 == 1 ~ 1,
                                                     TRUE ~ NA_integer_))
table(df$exclude_timepoint_mb,useNA = "always")

# Current menopausal status
unique(df$MB4680)
table(df$MB4680)

df$current_meno_status_mb <- df$MB4680

# HRT
unique(df$MB4790)
unique(df$MB4800)
unique(df$MB4810)
unique(df$MB4820)

# 1 (Yes), 2 (No), other (Missing, Unresolvable, Did not attend clinic etc)
df <- df %>% mutate(hrt_mb =  case_when(MB4790 == 0 | MB4800 == 0 | MB4810 == 0 | MB4820 == 0 ~ 0,
                                              TRUE ~ NA_integer_))

df <- df %>% mutate(hrt_mb =  case_when(MB4790 == 1 | MB4800 == 1 | MB4810 == 1 | MB4820 == 1 ~ 1,
                                              TRUE ~ hrt_mb))

table(df$hrt_mb, useNA = "always")

# Save tidied data frame
saveRDS(df, file = paste0(filestore,"cohort_lmp_variables.rds"))