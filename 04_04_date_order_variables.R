# We use data from FoM1, FoM2, FoM3, FoM4 and questionnaires T, U, V & Y
# This data needs to be in chronological date order however there is overlap between the data collection of
# some of the clinics and questionnaires. For this reason each participant will have completed the 
# clinics and questionnaires in different orders.

# In this script, for each participant we look at the date of completion of each clinic and questionnaire and order
# all of the data accordingly.

# In the input df, the column names have suffix 1, 2, 3, 4, t, u, v, y which refers to the clinic number/questionnaire letter
# In the ordered output df, the column names have suffix 1, 2, 3, 4, 5, 6, 7, 8 which refers to the date order or the variables

library(dplyr)
library(stringr)

variable_list <- c("contracept","hrt","exclude_timepoint","dlmp","mlmp","ylmp","period3","period12","age","age_last_period","regperiod")
non_date_variable_list <- c("contracept","hrt","exclude_timepoint","dlmp","mlmp","ylmp","period3","period12","age","age_last_period","regperiod") 
variable_suffix <- c("1","2","3","4","t","u","v","y")

# Select needed columns from df
selected_columns <- unlist(c("aln", paste0("date_",variable_suffix),lapply(variable_list, function(variable_list) paste0(variable_list,"_", variable_suffix))))

# Select columns from df
tmp <- df_duplicates_removed %>% select(all_of(selected_columns))
tmp <- tmp %>% left_join(df %>% select("aln","periods_stopped_surgery_date","periods_stopped_surgery_age"))

# Create menopause_df 
menopause_df <- as.data.frame(matrix(ncol = ncol(tmp),nrow = nrow(tmp)))

# Create column names for menopause_df
date_name <- paste0("date_", 1:8)
variable_names <- date_name

#Using variable list (eg hrt, dlmp) create list of column names (eg hrt_1, hrt_2) and add to list (variable_names)
# E.g. Creates hrt_name = (hrt_1,hrt_2,hrt_3,hrt_4) etc 
# Creates variable_names = c(hrt_x,dlmp_x,mlmp_x) etc
for (variable in variable_list) {
  assign(paste0(variable,"_name"),paste0(variable,"_",1:8))
  variable_names <- c(variable_names,get(paste0(variable,"_name")))
}

# Rename columns of menopause_df
colnames(menopause_df) <- c("aln", variable_names,"periods_stopped_surgery_date","periods_stopped_surgery_age")

# Convert aln to numeric
menopause_df$aln <- as.numeric(menopause_df$aln)

# Convert date columns to Date type
menopause_df[c("periods_stopped_surgery_date",date_name)] <- lapply(menopause_df[c("periods_stopped_surgery_date",date_name)], as.Date, origin = "1970-01-01")

# Index of any data with a clinic and questionnaire on the same date
index <- c()

# Loop through rows of menopause_df
for (i in seq_len(nrow(tmp))) {
  
  # Extract dates for each row
  dates <- as.Date(unlist(tmp[i, paste0("date_",variable_suffix)], use.names = TRUE))
  
  # Remove NA values and sort dates
  dates <- sort(dates[!is.na(dates)])
  
  if(any(duplicated(dates))==T){
    
    duplicates <- dates[duplicated(dates)]
    
    for(k in 1:length(duplicates)){
      duplicate_variable <- duplicates[k]
      duplicate_suffix <- str_replace(labels(dates)[which(dates == duplicate_variable)], "date_", "")
      
      replacements <- c()
      for(variable in variable_list){
        replacements <- c(replacements,(is.na(tmp[i,paste0(variable,"_",duplicate_suffix[1])]) & !is.na(tmp[i,paste0(variable,"_",duplicate_suffix[2])])))
      }
      
      if(any(replacements)==T){
        index <- c(index,i)
      }
    }
  }
  
  # Determine the order of the date variable labels
  date_order <- str_replace(labels(dates), "date_", "")
  date_order <-  date_order[date_order != ""]
  
  # Order the non date variables
  # For each variable extract the columns from tmp in the order according to ordered dates
  # If dates contains only NAs (i.e. the participant did not attend any clinics/questionnaires create empty vector)
  for (variable in non_date_variable_list) {
    if(!all(is.na(dates))){
      assign(variable,as.numeric(unlist(tmp[i, paste0(variable,"_",date_order)]))) 
    }else{
      assign(variable, as.numeric())
    }
  }
  
  # Exclusions
  # Remove all time points where contraceptives are used/where they are still used in subsequent time points
  # We only want to be left with time points where no contraceptives are used at that time point and any time points
  # after
  
  # timepoint <- NA
  # for(j in 1:length(contracept)){
  #   if(all(is.na(contracept[j:length(contracept)]) | contracept[j:length(contracept)] == 0)){
  #     timepoint <- j
  #     break
  #   }
  # }
  # 
  # 
  # if(is.na(timepoint)){
  #   replace <- c(1:length(contracept))
  # }else if(timepoint == 1){
  #   replace <- c()
  # }else{
  #   replace <- c(1:(timepoint-1))
  # }
  # 
  # # Collect together the data for each variable
  # data_list <- list(contracept,hrt,exclude_timepoint,dlmp,mlmp,ylmp,period3,period12,age)
  # 
  # for (j in 1:length(data_list)) {
  #   if(length(replace)> 0){
  #     data <- data_list[[j]]
  #     data <- data[-replace]
  #     assign(variable_list[j],data)
  #   }
  # }
  # 
  # if(length(replace)> 0){
  #   dates <- dates[-replace]
  # }
  
  # Remove any time points that occur after surgery that stops periods
  
  periods_stopped_surgery_date <- tmp[i,"periods_stopped_surgery_date"]
  periods_stopped_surgery_age <- tmp[i,"periods_stopped_surgery_age"]
  
  if(!is.na(periods_stopped_surgery_date)){
    replace <- as.numeric(which(dates >= periods_stopped_surgery_date))
  }else if(!is.na(periods_stopped_surgery_age)){
    replace <- as.numeric(which(age >= periods_stopped_surgery_age))
  }else{
    replace <- c()
  }
  
  # Collect together the data for each variable
  data_list <- list(contracept,hrt,exclude_timepoint,dlmp,mlmp,ylmp,period3,period12,age,age_last_period,regperiod)
  
  for (j in 1:length(data_list)) {
    if(length(replace)> 0){
      data <- data_list[[j]]
      data <- data[-replace]
      assign(variable_list[j],data)
    }
  }
  
  if(length(replace)> 0){
    dates <- dates[-replace]
  }
  
  # Exclude timepoints where contraception or hrt is used, or chemotherapy, pregnancy or breastfeeding, or other reason is reason for
  # cause of stopped periods
  replace <- which(hrt == 1 | exclude_timepoint == 1 | contracept == 1)
  
  # Collect together the data for each variable
  data_list <- list(contracept,hrt,exclude_timepoint,dlmp,mlmp,ylmp,period3,period12,age,age_last_period,regperiod)

  for (j in 1:length(data_list)) {
    if(length(replace)> 0){
      data <- data_list[[j]]
      data <- data[-replace]
      assign(variable_list[j],data)
    }
  }
  
  if(length(replace)> 0){
    dates <- dates[-replace]
  }
  
  # Add NA so that date variable is of the correct length
  dates <- c(dates,rep(NA,length(date_name)-length(dates)))
  
  # Add NAs to the variable vectors (e.g. hrt, dlmp) so that they are the correct length
  # Create what will become the column names e.g hrt_1,hrt_2,hrt_3 etc
  names_list <- lapply(variable_list, function(variable_list) paste0(variable_list,"_", 1:8))
  
  # Collect together the data for each variable
  data_list <- list(contracept,hrt,exclude_timepoint,dlmp,mlmp,ylmp,period3,period12,age,age_last_period,regperiod)
  
  # Loop over each variable and add NAs so that the vector is the correct length
  
  for (j in seq_along(names_list)) {
    # Extract hrt variable name and its data
    name <- names_list[[j]]
    data <- data_list[[j]]
    
    # Append NA values to hrt
    data <- c(data, rep(NA, length(name) - length(data)))
    assign(variable_list[j],data)
  }
  
  # Assign sorted dates to menopause_df
  
  menopause_df[i,1] <- tmp[i,1]
  menopause_df[i,date_name] <- dates
  menopause_df[i,c("periods_stopped_surgery_date","periods_stopped_surgery_age")] <- tmp[i,c("periods_stopped_surgery_date","periods_stopped_surgery_age")]
  
  # Assign sorted variables to menopause_df
  for(variable in variable_list){
    menopause_df[i,get(paste0(variable,"_name"))] <- get(variable)
  }
}

print(paste0("Patient row index where there are duplicate dates: ", index))

rm(list = setdiff(ls(), c("df","df_duplicates_removed","menopause_df","filestore")))

saveRDS(menopause_df, file = paste0(filestore,"cohort_date_ordered.rds"))

length(which(!is.na(menopause_df$date_1)))

before <- df_duplicates_removed$aln
after <- menopause_df$aln[which(!is.na(menopause_df$date_1))]

use <- which(!before %in% after)

flowchart <- df_duplicates_removed[use,]
flowchart <- flowchart %>% left_join(df %>% select("aln","periods_stopped_surgery_date","periods_stopped_surgery_age"))

saveRDS(flowchart, file = paste0(filestore,"flowchart_df.rds"))

