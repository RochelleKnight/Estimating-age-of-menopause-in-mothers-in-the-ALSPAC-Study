rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Estimating-age-of-menopause-in-mothers-in-the-ALSPAC-Study")

library(haven)
library(dplyr)
library(lubridate)     

# Load cohort data set
df <- read_dta(paste0(filestore,"cohort.dta"))
df <- as.data.frame(df)

dates_ages <- readRDS(paste0(filestore,"date_age_attendance.rds"))
colnames(dates_ages) <- gsub("__","_",colnames(dates_ages))
dates_ages <- dates_ages[,c("aln",paste0("date_",c("1","2","3","4","t","v","y","u","mb")),paste0("age_",c("1","2","3","4","t","v","y","u","mb")))]

df <- df[,c("aln",
            "fm1ob100","fm1ob101","fm1ob121","fm1ob110","fm1ob120","fm1ob126","fm1ob123","fm1ob124","fm1ob125","fm1ob130",
            "fm2ob100","fm2ob101","fm2ob121","fm2ob110a","fm2ob110b","fm2ob110c","fm2ob120","fm2ob126","fm2ob123","fm2ob124","fm2ob124a","fm2ob125","fm2ob130",
            "fm3ob100","fm3ob101","fm3ob121","fm3ob110a","fm3ob110b","fm3ob110c","fm3ob120","fm3ob126","fm3ob123","fm3ob124","fm3ob124a","fm3ob125","fm3ob130",
            "fm4ob100","fm4ob101","fm4ob121","fm4ob110a","fm4ob110b","fm4ob110c","fm4ob120","fm4ob126","fm4ob123","fm4ob124","fm4ob124a","fm4ob125","fm4ob130",
            "t4521","t4522","t4961","t4800","t4810","t4801","t4802","t4803","t4804","t4805","t4811","t4812","t4837",
            "t4700","t4701","t4702","t4703","t4730","t4731","t4732","t4733","t4710","t4711","t4712","t4713","t4720","t4721","t4722","t4723","t4813",
            "V4551","V4552","V4553","V4559","V4560","V4805","V4955","V4800","V4810","V4801","V4802","V4803","V4804","V4811","V4812","V4837",
            "V4700","V4701","V4702","V4703","V4730","V4731","V4732","V4733","V4710","V4711","V4712","V4713","V4720","V4721","V4722","V4723","V4813",
            "Y5001","Y5002","Y5003","Y5008","Y5009","Y5084","Y5140","Y5070","Y5100","Y5080","Y5081","Y5082","Y5083","Y5085","Y5101","Y5102",
            "Y5020","Y5021","Y5022","Y5023","Y5050","Y5051","Y5052","Y5053","Y5040","Y5041","Y5042","Y5043","Y5030","Y5031","Y5032","Y5033","Y5103",
            "U1021","U1030","U1031","U1032","U1033","U1034","U1060","U1040","U1041","U1042","U1050",
            "MB4600","MB4610","MB4620","MB4630","MB4640","MB4650","MB4660","MB4670","MB4680","MB4790","MB4800","MB4810","MB4820")]

df <- df %>% left_join(dates_ages)

########################################## FoM 1 #######################################
unique(df$fm1ob100)
unique(df$fm1ob101)
unique(df$fm1ob121)
unique(df$fm1ob110)
unique(df$fm1ob120)
unique(df$fm1ob123)
unique(df$fm1ob124)
unique(df$fm1ob125)

df[, c("fm1ob100","fm1ob101","fm1ob121","fm1ob110","fm1ob120","fm1ob123","fm1ob124","fm1ob125")][df[, c("fm1ob100","fm1ob101","fm1ob121","fm1ob110","fm1ob120","fm1ob123","fm1ob124","fm1ob125")] < 0] <- NA

# Irregular periods
unique(df$fm1ob130)
table(df$fm1ob130)

df$fm1ob130[which(df$fm1ob130 == -2)] <- 4
df$fm1ob130[which(df$fm1ob130 < 0)] <- NA

table(df$fm1ob130)

# Period/menstrual bleeding in last 3 months
unique(df$fm1ob126)
table(df$fm1ob126, useNA = "always")
df$fm1ob126[which(df$fm1ob126 == -2)] <- 2
df$fm1ob126[which(df$fm1ob126 < 0)] <- NA
unique(df$fm1ob120)
table(df$fm1ob120, useNA = "always")

# Any women with responses to FoM 1 but no date of attendance
which(is.na(df[, "date_1"]) & rowSums(!is.na(df[, c("age_1",grep("fm1", names(df), value = TRUE))])) > 0) #None

################################### FoM 2 ######################################
unique(df$fm2ob100)
unique(df$fm2ob101)
unique(df$fm2ob121)
unique(df$fm2ob110a)
unique(df$fm2ob110b)
unique(df$fm2ob110c)
unique(df$fm2ob120)
unique(df$fm2ob126)
unique(df$fm2ob123)
unique(df$fm2ob124)
unique(df$fm2ob124a)
unique(df$fm2ob125)

df[, c("fm2ob100","fm2ob101","fm2ob121","fm2ob110a","fm2ob110b","fm2ob110c","fm2ob120","fm2ob126","fm2ob123","fm2ob124","fm2ob124a","fm2ob125","fm2ob130")][df[, c("fm2ob100","fm2ob101","fm2ob121","fm2ob110a","fm2ob110b","fm2ob110c","fm2ob120","fm2ob126","fm2ob123","fm2ob124","fm2ob124a","fm2ob125","fm2ob130")] < 0] <- NA

# Any women with responses to FoM 2 but no date of attendance
which(is.na(df[, "date_2"]) & rowSums(!is.na(df[, c("age_2",grep("fm2", names(df), value = TRUE))])) > 0) #None

################################### FoM 3 ######################################
unique(df$fm3ob100)
unique(df$fm3ob101)
unique(df$fm3ob121)
unique(df$fm3ob110a)
unique(df$fm3ob110b)
unique(df$fm3ob110c)
unique(df$fm3ob120)
unique(df$fm3ob126)
unique(df$fm3ob123)
unique(df$fm3ob124)
unique(df$fm3ob124a)
unique(df$fm3ob125)
unique(df$fm3ob130)

df[, c("fm3ob100","fm3ob101","fm3ob121","fm3ob110a","fm3ob110b","fm3ob110c","fm3ob120","fm3ob126","fm3ob123","fm3ob124","fm3ob124a","fm3ob125","fm3ob130")][df[, c("fm3ob100","fm3ob101","fm3ob121","fm3ob110a","fm3ob110b","fm3ob110c","fm3ob120","fm3ob126","fm3ob123","fm3ob124","fm3ob124a","fm3ob125","fm3ob130")] < 0] <- NA

# Any women with responses to FoM 3 but no date of attendance
which(is.na(df[, "date_3"]) & rowSums(!is.na(df[, c("age_3",grep("fm3", names(df), value = TRUE))])) > 0) #None

################################### FoM 4 ######################################
unique(df$fm4ob100)
unique(df$fm4ob101)
unique(df$fm4ob121)
unique(df$fm4ob110a)
unique(df$fm4ob110b)
unique(df$fm4ob110c)
unique(df$fm4ob120)
unique(df$fm4ob126)
unique(df$fm4ob123)
unique(df$fm4ob124)
unique(df$fm4ob124a)
unique(df$fm4ob125)
unique(df$fm4ob130)

df[, c("fm4ob100","fm4ob101","fm4ob121","fm4ob110a","fm4ob110b","fm4ob110c","fm4ob120","fm4ob126","fm4ob123","fm4ob124","fm4ob124a","fm4ob125","fm4ob130")][df[, c("fm4ob100","fm4ob101","fm4ob121","fm4ob110a","fm4ob110b","fm4ob110c","fm4ob120","fm4ob126","fm4ob123","fm4ob124","fm4ob124a","fm4ob125","fm4ob130")] < 0] <- NA

# Any women with responses to FoM 4 but no date of attendance
which(is.na(df[, "date_4"]) & rowSums(!is.na(df[, c("age_4",grep("fm4", names(df), value = TRUE))])) > 0) #None

############################## Questionnaire T #################################
unique(df$t4521)
unique(df$t4522)
unique(df$t4800)
unique(df$t4810)
unique(df$t4801)
unique(df$t4802)
unique(df$t4803)
unique(df$t4804)
unique(df$t4805)
unique(df$t4811)
unique(df$t4812)
unique(df$t4700)
unique(df$t4701)
unique(df$t4702)
unique(df$t4703)
unique(df$t4730)
unique(df$t4731)
unique(df$t4732)
unique(df$t4733)
unique(df$t4710)
unique(df$t4711)
unique(df$t4712)
unique(df$t4713)
unique(df$t4720)
unique(df$t4721)
unique(df$t4722)
unique(df$t4723)
unique(df$t4837)
table(df$t4837)
unique(df$t4813)

# Age at last period
unique(df$t4813)
df$t4813[which(df$t4813 < 0)] <- NA
df$t4813[which(df$t4813 > df$age_t)] <- NA
table(df$t4813)

# Irregular periods
unique(df$t4837)
table(df$t4837)

df$t4837[which(df$t4837 == -2)] <- 1
df$t4837[which(df$t4837 < 0)] <- NA

table(df$t4837)

# Respondent currently on HRT
unique(df$t4961)
table(df$t4961)
df$t4961[which(df$t4961 == -2)] <- 2

t_variables <- c("t4521","t4522","t4961","t4800","t4810","t4801","t4802","t4803","t4804","t4805","t4811","t4812",
                 "t4700","t4701","t4702","t4703","t4730","t4731","t4732","t4733","t4710","t4711","t4712","t4713","t4720","t4721","t4722","t4723","t4813")

df[,t_variables][df[, t_variables] < 0] <- NA

# Any women with responses to Questionnaire T but no date of attendance
which(is.na(df[, "date_t"]) & rowSums(!is.na(df[, c("age_t",grep("^t", names(df), value = TRUE))])) > 0) #None

############################## Questionnaire V #################################
unique(df$V4551)
unique(df$V4552)
unique(df$V4553)
unique(df$V4559)
unique(df$V4560)
unique(df$V4805)
unique(df$V4800)
unique(df$V4801)
unique(df$V4802)
unique(df$V4803)
unique(df$V4804)
unique(df$V4811)
unique(df$V4812)
unique(df$V4700)
unique(df$V4701)
unique(df$V4702)
unique(df$V4703)
unique(df$V4730)
unique(df$V4731)
unique(df$V4732)
unique(df$V4733)
unique(df$V4710)
unique(df$V4711)
unique(df$V4712)
unique(df$V4713)
unique(df$V4720)
unique(df$V4721)
unique(df$V4722)
unique(df$V4723)
unique(df$V4837)
unique(df$V4813)

# Age at last period
unique(df$V4813)
df$V4813[which(df$V4813 < 0)] <- NA
df$V4813[which(df$V4813 > df$age_v)] <- NA
table(df$V4813)

# Respondent currently on HRT
unique(df$V4955)
table(df$V4955)
df$V4955[which(df$V4955 == -2)] <- 2

# Respondent has had a period or menstrual bleeding in the last 3 months
unique(df$V4810)
table(df$V4810)
df$V4810[which(df$V4810 == -3)] <- 2

v_variables <- c("V4551","V4552","V4553","V4559","V4560","V4805","V4955","V4800","V4810","V4801","V4802","V4803","V4804","V4811","V4812",
                 "V4700","V4701","V4702","V4703","V4730","V4731","V4732","V4733","V4710","V4711","V4712","V4713","V4720","V4721","V4722","V4723","V4837","V4813")

df[,v_variables][df[,v_variables] < 0] <- NA

# Any women with responses to Questionnaire T but no date of attendance
which(is.na(df[, "date_v"]) & rowSums(!is.na(df[, c("age_v",grep("^V", names(df), value = TRUE))])) > 0) #None

############################## Questionnaire Y #################################
unique(df$Y5001)
unique(df$Y5002)
unique(df$Y5003)
unique(df$Y5008)
unique(df$Y5009)
unique(df$Y5084)
unique(df$Y5070)
unique(df$Y5080)
unique(df$Y5081)
unique(df$Y5082)
unique(df$Y5083)
unique(df$Y5085)
unique(df$Y5101)
unique(df$Y5102)
unique(df$Y5020)
unique(df$Y5021)
unique(df$Y5022)
unique(df$Y5023)
unique(df$Y5050)
unique(df$Y5051)
unique(df$Y5052)
unique(df$Y5053)
unique(df$Y5040)
unique(df$Y5041)
unique(df$Y5042)
unique(df$Y5043)
unique(df$Y5030)
unique(df$Y5031)
unique(df$Y5032)
unique(df$Y5033)
unique(df$Y5103)
table(df$Y5103)

# Had period/menstrual bleeding in past 12 months
# Currently on HRT
unique(df$Y5070)
table(df$Y5070)
df$Y5070[which(df$Y5070 == -2)] <- 0

#Add in those who have said no period in the past 12 months in age at last period question
table(df$Y5103)
df$Y5070[which(df$Y5103 == -2)] <- 0

# Age at last period
unique(df$Y5103)
df$Y5103[which(df$Y5103 < 0)] <- NA
df$Y5103[which(df$Y5103 > df$age_y)] <- NA
table(df$Y5103)

# Had period/menstrual bleeding in past 3 months
unique(df$Y5100)
table(df$Y5100)
df$Y5100[which(df$Y5100 == -2)] <- 0

y_variables <- c("Y5001","Y5002","Y5003","Y5008","Y5009","Y5084","Y5140","Y5070","Y5100","Y5080","Y5081","Y5082","Y5083","Y5085","Y5101","Y5102",
                 "Y5020","Y5021","Y5022","Y5023","Y5050","Y5051","Y5052","Y5053","Y5040","Y5041","Y5042","Y5043","Y5030","Y5031","Y5032","Y5033","Y5103")

df[,y_variables][df[,y_variables] < 0] <- NA

# Any women with responses to Questionnaire T but no date of attendance
which(is.na(df[, "date_y"]) & rowSums(!is.na(df[, c("age_y",grep("^Y", names(df), value = TRUE))])) > 0) #None

############################### Questionnaire U ################################
unique(df$U1030)
unique(df$U1031)
unique(df$U1032)
unique(df$U1033)
unique(df$U1034)
unique(df$U1060)
unique(df$U1040)
unique(df$U1041)
unique(df$U1042)
unique(df$U1021)
unique(df$U1050)

u_variables <- c("U1021","U1030","U1031","U1032","U1033","U1034","U1060","U1040","U1041","U1042","U1050")
df[,u_variables][df[,u_variables] < 0] <- NA

# Any women with responses to Questionnaire U but no date of attendance
which(is.na(df[, "date_u"]) & rowSums(!is.na(df[, c("age_u",grep("^U", names(df), value = TRUE))])) > 0) #None

############################### Questionnaire MB ################################
unique(df$MB4600)
unique(df$MB4610)
unique(df$MB4620)
unique(df$MB4630)
unique(df$MB4640)
unique(df$MB4650)
unique(df$MB4660)
unique(df$MB4670)
unique(df$MB4680)
unique(df$MB4790)
unique(df$MB4800)
unique(df$MB4810)
unique(df$MB4820)

mb_variables <- c("MB4600","MB4610","MB4620","MB4630","MB4640","MB4650","MB4660","MB4670","MB4680","MB4790","MB4800","MB4810","MB4820")
df[,mb_variables][df[,mb_variables] < 0] <- NA

# Any women with responses to Questionnaire MB but no date of attendance
which(is.na(df[, "date_mb"]) & rowSums(!is.na(df[, c("age_mb",grep("^MB", names(df), value = TRUE))])) > 0) #None

# Save tidied data frame
saveRDS(df, file = paste0(filestore,"cohort_negatives_set_to_na.rds"))
