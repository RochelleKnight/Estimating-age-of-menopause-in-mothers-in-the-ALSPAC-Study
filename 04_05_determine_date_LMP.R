library(lubridate)
############################ IMPUTE DATE OF LMP ################################
tmp <- menopause_df %>% select(aln,paste0(c("date_","dlmp_","mlmp_","ylmp_","period3_","period12_","contracept_","hrt_","age_","age_last_period_"),1),
                               paste0(c("date_","dlmp_","mlmp_","ylmp_","period3_","period12_","contracept_","hrt_","age_","age_last_period_"),2),
                               paste0(c("date_","dlmp_","mlmp_","ylmp_","period3_","period12_","contracept_","hrt_","age_","age_last_period_"),3),
                               paste0(c("date_","dlmp_","mlmp_","ylmp_","period3_","period12_","contracept_","hrt_","age_","age_last_period_"),4),
                               paste0(c("date_","dlmp_","mlmp_","ylmp_","period3_","period12_","contracept_","hrt_","age_","age_last_period_"),5),
                               paste0(c("date_","dlmp_","mlmp_","ylmp_","period3_","period12_","contracept_","hrt_","age_","age_last_period_"),6),
                               paste0(c("date_","dlmp_","mlmp_","ylmp_","period3_","period12_","contracept_","hrt_","age_","age_last_period_"),7),
                               paste0(c("date_","dlmp_","mlmp_","ylmp_","period3_","period12_","contracept_","hrt_","age_","age_last_period_"),8)) %>%
  filter(!is.na(date_1))

for (i in 1:8) {
  print(paste0("Working on timepoint ",i))
  dlmp <- paste0("dlmp_",i)
  mlmp <- paste0("mlmp_",i)
  ylmp <- paste0("ylmp_",i)
  date_attendance <- paste0("date_",i)
  period12 <- paste0("period12_",i)
  period3 <- paste0("period3_",i)
  datelmp_impute <- paste0("datelmp_impute_",i)
  datelmp_replace <- paste0("datelmp_replace_",i)
  datelmp_history <- paste0("datelmp_history_",i)
  datelmp <- paste0("datelmp_",i)
  dif_date_attendance_minus_impute <- paste0("dif_date_attendance_minus_impute_",i)
  dif_date_impute_minus_attendance <- paste0("dif_date_impute_minus_attendance_",i)
  year_reported <- paste0("year_reported_",i)
  reported_datelmp_period_history_dif_before <- paste0("reported_datelmp_period_history_dif_before_",i)
  reported_datelmp_period_history_dif_after <- paste0("reported_datelmp_period_history_dif_after_",i)
  use_straw <- paste0("use_straw_",i)
  
  age <- paste0("age_",i)
  
  tmp[,use_straw] <- NA_integer_
  tmp$use_previous_timepoint <- NA_integer_
  tmp$ylmp_missing <- NA_integer_
  tmp$ylmp_missing[which(is.na(tmp[,ylmp]) & !is.na(tmp[,date_attendance]))] <- 1
  tmp$ylmp_present <- NA_integer_
  tmp$ylmp_present[which(!is.na(tmp[,ylmp]) & !is.na(tmp[,date_attendance]))] <- 1
  
  if(i>1){
    date_attendance_previous <- paste0("date_",i-1)
    datelmp_previous <- paste0("datelmp_",i-1)
    age_last_period_previous <- paste0("age_last_period_",i-1)
    age_previous <- paste0("age_",i-1)
  }
  
  # Add indicator variable for those with both reported year of LMP
  tmp[,year_reported] <- ifelse(!is.na(tmp[,ylmp]) & !is.na(tmp[,date_attendance]), 1, 0)
  
  # Remove any reported days/months of LMP where no year has been provided
  replace <- which((!is.na(tmp[,dlmp]) & !is.na(tmp[,mlmp]) & is.na(tmp[,ylmp])) | (!is.na(tmp[,dlmp]) & is.na(tmp[,mlmp]) & !is.na(tmp[,ylmp])))
  tmp[replace,dlmp] <- NA
  
  replace <- which(!is.na(tmp[,mlmp]) & is.na(tmp[,ylmp]))
  tmp[replace,mlmp] <- NA
  
  ################ If only day is missing, replace it with 15 ####################
  
  tmp[which(is.na(tmp[,dlmp]) & !is.na(tmp[,mlmp]) & !is.na(tmp[,ylmp]) & !is.na(tmp[,date_attendance])),dlmp] <- 15
  
  ### If only year is available, and did not have period in the last 12 months ###
  
  #ylmp = year of attendance -1, then set datelmp to midpoint between 01/01/ylmp and date of attendance - 365
  replace <- which(is.na(tmp[,mlmp]) & !is.na(tmp[,ylmp]) & !is.na(tmp[,date_attendance]) & tmp[,period12] == 0
                   & tmp[,ylmp] == (year(tmp[,date_attendance]) -1))
  
  #01/01/ylmp + ((date_attendance-365) - (01/01/ylmp))/2
  datelmp_string <- as.Date(paste0("01/01/",tmp[replace,ylmp]),format = "%m/%d/%Y") + ((tmp[replace,date_attendance] - 365.25) - as.Date(paste0("01/01/",tmp[replace,ylmp]), format = "%m/%d/%Y"))/2
  tmp[replace,dlmp] <- format(datelmp_string, "%d")
  tmp[replace,mlmp] <- format(datelmp_string, "%m")
  
  # ylmp < year of attendance -1, then set datelmp to 06/15/ylmp
  replace <- which(is.na(tmp[,mlmp]) & !is.na(tmp[,ylmp]) & !is.na(tmp[,date_attendance]) & tmp[,period12] == 0
                   & tmp[,ylmp] < (year(tmp[,date_attendance]) -1))
  
  tmp[replace,dlmp] <- 15
  tmp[replace,mlmp] <- 6
  
  ### If only year is available, and had period in the last 12 months but not in the last 3 months ###
  
  # ylmp = year of attendance, then set datelmp to midpoint between 01/01/ylmp and date of attendance - 91.3
  replace <- which(is.na(tmp[,mlmp]) & !is.na(tmp[,ylmp]) & !is.na(tmp[,date_attendance]) 
                   & tmp[,period12] == 1 & tmp[,period3] ==0 
                   & tmp[,ylmp] == year(tmp[,date_attendance])
                   & as.Date(paste0("01/01/",tmp[,ylmp]),format = "%m/%d/%Y") <= (tmp[,date_attendance] - 91.3) )
  
  datelmp_string <- as.Date(paste0("01/01/",tmp[replace,ylmp]),format = "%m/%d/%Y") + ((tmp[replace,date_attendance] - 91.3) - as.Date(paste0("01/01/",tmp[replace,ylmp]), format = "%m/%d/%Y"))/2
  
  tmp[replace,dlmp] <- format(datelmp_string, "%d")
  tmp[replace,mlmp] <- format(datelmp_string, "%m")
  
  # ylmp = year of attendance -1 and 31/12/ylmp_x is before date of attendance - 91.3, 
  #then set datelmp to midpoint between date of attendance - 365 and 31/12/ylmp
  replace <- which(is.na(tmp[,mlmp]) & !is.na(tmp[,ylmp]) & !is.na(tmp[,date_attendance]) 
                   & tmp[,period12] == 1 & tmp[,period3] ==0 
                   & tmp[,ylmp] == (year(tmp[,date_attendance])-1)
                   & as.Date(paste0("12/31/",tmp[,ylmp]),format = "%m/%d/%Y") <= (tmp[,date_attendance] - 91.3))
  
  datelmp_string <- (tmp[replace,date_attendance] - 365.25) + (as.Date(paste0("12/31/",tmp[replace,ylmp]), format = "%m/%d/%Y") - (tmp[replace,date_attendance] - 365.25))/2
  
  tmp[replace,dlmp] <- format(datelmp_string, "%d")
  tmp[replace,mlmp] <- format(datelmp_string, "%m")
  
  # ylmp = year of attendance -1 and 31/12/ylmp_x is within 3 months from date of attendance, 
  #then set datelmp to midpoint between date of attendance - 365 and date of attendance - 91.3
  replace <- which(is.na(tmp[,mlmp]) & !is.na(tmp[,ylmp]) & !is.na(tmp[,date_attendance]) 
                   & tmp[,period12] == 1 & tmp[,period3] ==0 
                   & tmp[,ylmp] == (year(tmp[,date_attendance])-1)
                   & as.Date(paste0("12/31/",tmp[,ylmp]),format = "%m/%d/%Y") > (tmp[,date_attendance] - 91.3))
  
  datelmp_string <- (tmp[replace,date_attendance] - 365.25) + ((tmp[replace,date_attendance] - 91.3) - (tmp[replace,date_attendance] - 365.25))/2
  
  tmp[replace,dlmp] <- format(datelmp_string, "%d")
  tmp[replace,mlmp] <- format(datelmp_string, "%m")
  
  ### If only year is available, and had period in the last 3 months ###
  
  # ylmp = year of attendance and 01/01/ylmp is before date of attendance - 91.3,
  # then set datelmp to midpoint between date of attendance - 91.3 and date of attendance
  
  replace <- which(is.na(tmp[,mlmp]) & !is.na(tmp[,ylmp]) & !is.na(tmp[,date_attendance]) 
                   & tmp[,period12] == 1 & tmp[,period3] == 1 
                   & tmp[,ylmp] == year(tmp[,date_attendance])
                   & as.Date(paste0("01/01/",tmp[,ylmp]),format = "%m/%d/%Y") <= (tmp[,date_attendance] - 91.3))
  
  datelmp_string <- (tmp[replace,date_attendance] - 91.3) + ((tmp[replace,date_attendance]) - (tmp[replace,date_attendance] - 91.3))/2
  
  tmp[replace,dlmp] <- format(datelmp_string, "%d")
  tmp[replace,mlmp] <- format(datelmp_string, "%m")
  
  # ylmp = year of attendance and 01/01/ylmp is after date of attendance - 91.3,
  # then set datelmp to midpoint between 01/01/ylmp and date of attendance
  
  replace <- which(is.na(tmp[,mlmp]) & !is.na(tmp[,ylmp]) & !is.na(tmp[,date_attendance]) 
                   & tmp[,period12] == 1 & tmp[,period3] == 1 
                   & tmp[,ylmp] == year(tmp[,date_attendance])
                   & as.Date(paste0("01/01/",tmp[,ylmp]),format = "%m/%d/%Y") > (tmp[,date_attendance] - 91.3))
  
  datelmp_string <- as.Date(paste0("01/01/",tmp[replace,ylmp]),format = "%m/%d/%Y") + ((tmp[replace,date_attendance]) - as.Date(paste0("01/01/",tmp[replace,ylmp]),format = "%m/%d/%Y"))/2
  
  tmp[replace,dlmp] <- format(datelmp_string, "%d")
  tmp[replace,mlmp] <- format(datelmp_string, "%m")
  
  # ylmp = year of attendance -1 and 31/12/ylmp is after date of attendance - 91.3,
  # then set datelmp to midpoint between date of attendance - 91.3 and 31/12/ylmp
  
  replace <- which(is.na(tmp[,mlmp]) & !is.na(tmp[,ylmp]) & !is.na(tmp[,date_attendance]) 
                   & tmp[,period12] == 1 & tmp[,period3] == 1 
                   & tmp[,ylmp] == (year(tmp[,date_attendance])-1)
                   & as.Date(paste0("12/31/",tmp[,ylmp]),format = "%m/%d/%Y") > (tmp[,date_attendance] - 91.3))
  
  datelmp_string <- (tmp[replace,date_attendance] - 91.3) + ((tmp[replace,date_attendance] - 91.3) - as.Date(paste0("12/31/",tmp[replace,ylmp]),format = "%m/%d/%Y"))/2
  
  tmp[replace,dlmp] <- format(datelmp_string, "%d")
  tmp[replace,mlmp] <- format(datelmp_string, "%m")
  
  ### If only year is available, and missing response for period in the last 3 and 12 months ###

  #ylmp = year of attendance, then set datelmp to midpoint between 01/01/ylmp and date of attendance
  replace <- which(is.na(tmp[,mlmp]) & !is.na(tmp[,ylmp]) & !is.na(tmp[,date_attendance]) & is.na(tmp[,period12])
                   & is.na(tmp[,period3]) & tmp[,ylmp] == (year(tmp[,date_attendance])))

  #01/01/ylmp + (date_attendance - 01/01/ylmp)/2
  datelmp_string <- as.Date(paste0("01/01/",tmp[replace,ylmp]),format = "%m/%d/%Y") + (tmp[replace,date_attendance] - (as.Date(paste0("01/01/",tmp[replace,ylmp]), format = "%m/%d/%Y")))/2
  tmp[replace,dlmp] <- format(datelmp_string, "%d")
  tmp[replace,mlmp] <- format(datelmp_string, "%m")

  # ylmp < year of attendance, then set datelmp to 06/15/ylmp
  replace <- which(is.na(tmp[,mlmp]) & !is.na(tmp[,ylmp]) & !is.na(tmp[,date_attendance]) & is.na(tmp[,period12])
                   & is.na(tmp[,period3]) & tmp[,ylmp] < (year(tmp[,date_attendance])))

  tmp[replace,dlmp] <- 15
  tmp[replace,mlmp] <- 6

  ### If only year is available, and not had period in the last 3 months but missing response for period in the last 12 months ###

  # ylmp = year of attendance, then set datelmp to midpoint between 01/01/ylmp and date of attendance - 91.3
  replace <- which(is.na(tmp[,mlmp]) & !is.na(tmp[,ylmp]) & !is.na(tmp[,date_attendance])
                   & is.na(tmp[,period12]) & tmp[,period3] == 0
                   & tmp[,ylmp] == year(tmp[,date_attendance])
                   & as.Date(paste0("01/01/",tmp[,ylmp]),format = "%m/%d/%Y") <= (tmp[,date_attendance] - 91.3) )

  datelmp_string <- as.Date(paste0("01/01/",tmp[replace,ylmp]),format = "%m/%d/%Y") + ((tmp[replace,date_attendance] - 91.3) - as.Date(paste0("01/01/",tmp[replace,ylmp]), format = "%m/%d/%Y"))/2

  tmp[replace,dlmp] <- format(datelmp_string, "%d")
  tmp[replace,mlmp] <- format(datelmp_string, "%m")

  # ylmp = year of attendance -1 and 31/12/ylmp_x is before date of attendance - 91.3,
  #then set datelmp to midpoint between 15/06/ylmp
  replace <- which(is.na(tmp[,mlmp]) & !is.na(tmp[,ylmp]) & !is.na(tmp[,date_attendance])
                   & is.na(tmp[,period12]) & tmp[,period3] == 0
                   & tmp[,ylmp] == (year(tmp[,date_attendance])-1)
                   & as.Date(paste0("12/31/",tmp[,ylmp]),format = "%m/%d/%Y") <= (tmp[,date_attendance] - 91.3))

  tmp[replace,dlmp] <- 15
  tmp[replace,mlmp] <- 6

  # ylmp = year of attendance -1 and 31/12/ylmp_x is within 3 months from date of attendance,
  #then set datelmp to midpoint between 01/01/ylmp and date of attendance - 91.3
  replace <- which(is.na(tmp[,mlmp]) & !is.na(tmp[,ylmp]) & !is.na(tmp[,date_attendance])
                   & is.na(tmp[,period12]) & tmp[,period3] == 0
                   & tmp[,ylmp] == (year(tmp[,date_attendance])-1)
                   & as.Date(paste0("12/31/",tmp[,ylmp]),format = "%m/%d/%Y") > (tmp[,date_attendance] - 91.3))

  datelmp_string <- as.Date(paste0("01/01/",tmp[replace,ylmp]),format = "%m/%d/%Y") + ((tmp[replace,date_attendance] - 91.3) - as.Date(paste0("01/01/",tmp[replace,ylmp]), format = "%m/%d/%Y"))/2

  tmp[replace,dlmp] <- format(datelmp_string, "%d")
  tmp[replace,mlmp] <- format(datelmp_string, "%m")

  # ylmp < year of attendance -1, then set datelmp to 06/15/ylmp
  replace <- which(is.na(tmp[,mlmp]) & !is.na(tmp[,ylmp]) & !is.na(tmp[,date_attendance])
                   & is.na(tmp[,period12]) & tmp[,period3] == 0
                   & tmp[,ylmp] < (year(tmp[,date_attendance]) -1))

  tmp[replace,dlmp] <- 15
  tmp[replace,mlmp] <- 6

  # Create datelmp_impute
  
  tmp[,datelmp_impute] <- paste0(tmp[,mlmp],"/",tmp[,dlmp],"/",tmp[,ylmp])
  tmp[,datelmp_impute] <- as.Date(tmp[,datelmp_impute], format = "%m/%d/%Y" )
  
  ################## USE PERIOD HISTORY FOR DATE OF LMP ##########################
  tmp[,datelmp_history] <- as.Date(NA)
  
  tmp[which(tmp[,period3] == 1 & !is.na(tmp[,date_attendance])),datelmp_history] <- tmp[which(tmp[,period3] == 1 & !is.na(tmp[,date_attendance])),date_attendance] - 45.65
  tmp[which(tmp[,period12] == 1 & tmp[,period3] == 0 & !is.na(tmp[,date_attendance])),datelmp_history] <- tmp[which(tmp[,period12] == 1 & tmp[,period3] == 0 & !is.na(tmp[,date_attendance])),date_attendance] - 228.28
  
  ####### Check datelmp_impute against date of attendance and period history ######
  
  # Difference between reported date LMP and period history 
  tmp[,dif_date_attendance_minus_impute] <- ifelse(!is.na(tmp[,datelmp_impute]) & !is.na(tmp[,date_attendance]), tmp[,date_attendance] - tmp[,datelmp_impute], NA )
  
  # Period in the last 3 months
  # Check if reported period is more than 3 months ago
  tmp[,reported_datelmp_period_history_dif_before] <- ifelse(tmp[,period3] == 1 & tmp[,dif_date_attendance_minus_impute] > 91, tmp[,dif_date_attendance_minus_impute] - 91, NA)
  tmp[,reported_datelmp_period_history_dif_before] <- ifelse(tmp[,period3] == 1 & tmp[,dif_date_attendance_minus_impute] < 0 , 0 - tmp[,dif_date_attendance_minus_impute], NA)
  
  # Period in last 12 months but not in the last 3 months
  tmp[,reported_datelmp_period_history_dif_before] <- ifelse(tmp[,period3] == 0 & tmp[,period12] == 1 & tmp[,dif_date_attendance_minus_impute] > 365, tmp[,dif_date_attendance_minus_impute] - 365, tmp[,reported_datelmp_period_history_dif_before])
  tmp[,reported_datelmp_period_history_dif_before] <- ifelse(tmp[,period3] == 0 & tmp[,period12] == 1 & tmp[,dif_date_attendance_minus_impute] < 92, 92 - tmp[,dif_date_attendance_minus_impute], tmp[,reported_datelmp_period_history_dif_before])
  
  # Period in last 12 months but missing response for last 3 months
  tmp[,reported_datelmp_period_history_dif_before] <- ifelse(is.na(tmp[,period3]) & tmp[,period12] == 1 & tmp[,dif_date_attendance_minus_impute] > 365, tmp[,dif_date_attendance_minus_impute] - 365, tmp[,reported_datelmp_period_history_dif_before])
  
  # No period in the last 12 months
  tmp[,reported_datelmp_period_history_dif_before] <- ifelse(tmp[,period3] == 0 & tmp[,period12] == 0 & tmp[,dif_date_attendance_minus_impute] < 366, 366 - tmp[,dif_date_attendance_minus_impute], tmp[,reported_datelmp_period_history_dif_before])
  
  # If the difference between reported date of LMP and period history is more than allowed threshold, 
  #then set both datelmp_impute and datelmp_history to NA
  # For period in the last 3 months
  replace <- which(tmp[,reported_datelmp_period_history_dif_before] > 91 & tmp[,period3] == 1 & tmp[,period12] == 1)
  tmp[replace,datelmp_impute] <- NA
  tmp[replace,datelmp_history] <- NA
  tmp[replace,use_straw] <- 0
  
  # For period in the last 12 months, but not in the last 3 months
  replace <- which(tmp[,reported_datelmp_period_history_dif_before] > 180 & tmp[,period3] == 0 & tmp[,period12] == 1)
  tmp[replace,datelmp_impute] <- NA
  tmp[replace,datelmp_history] <- NA
  tmp[replace,use_straw] <- 0
  
  # For period in the last 12 months, but missing response for in the last 3 months
  replace <- which(tmp[,reported_datelmp_period_history_dif_before] > 180 & is.na(tmp[,period3]) & tmp[,period12] == 1)
  tmp[replace,datelmp_impute] <- NA
  tmp[replace,datelmp_history] <- NA
  tmp[replace,use_straw] <- 0
  
  # For no period in the last 12 months
  replace <- which(tmp[,reported_datelmp_period_history_dif_before] > 180 & tmp[,period3] == 0 & tmp[,period12] == 0)
  tmp[replace,datelmp_impute] <- NA
  tmp[replace,datelmp_history] <- NA
  tmp[replace,use_straw] <- 0
  
  # If reported date of LMP is more than 31 days after date of attendance then set to NA 
  tmp[,dif_date_impute_minus_attendance] <- ifelse(!is.na(tmp[,datelmp_impute]) & !is.na(tmp[,date_attendance]), tmp[,datelmp_impute] - tmp[,date_attendance], NA )
  
  replace <- which(tmp[,dif_date_impute_minus_attendance] > 31)
  tmp[replace,datelmp_impute] <- NA
  
  # Only use datelmp_history if there is no reported information for date of LMP or reported date was more than 31 days 
  # after date of attendance
  replace <- which(tmp[,dif_date_impute_minus_attendance] > 31 | tmp$ylmp_missing == 1)
  tmp[-replace,datelmp_history] <- NA
  
  if(i == 2){
    replace <- which(!is.na(tmp[,datelmp_history]) & !is.na(tmp[,date_attendance_previous]) & !is.na(tmp[,datelmp_previous])  & tmp[,datelmp_history] < tmp[,date_attendance_previous])
    print(paste0(length(replace), " dates LMP where datelmp_history is prior to date of attendace at previous timepoint, set to NA"))
    tmp[replace,datelmp_history] <- NA

    tmp$use_previous_timepoint[replace] <- 1
    
  }else if(i > 2){
    tmp$previous_lmp_date_index <- NULL
    tmp$previous_lmp_date_index <- apply(tmp[,paste0("datelmp_",c(1:(i-1)))], 1, function(row) {
      cols <- which(!is.na(row)) # Identify non-NA column indices
      if (length(cols) == 0) {
        return(NA) # If no non-NA values, return NA
      }
      max(cols) # Otherwise, return the largest column index
    })
    
    tmp$date_attendance_for_previous_lmp <- NULL
    tmp$date_attendance_for_previous_lmp <- NA_Date_
    for (j in 1:nrow(tmp)) {
      if(!is.na(tmp$previous_lmp_date_index[j])){
        tmp$date_attendance_for_previous_lmp[j] <- tmp[j,paste0("date_",tmp$previous_lmp_date_index[j])]
      }
    }
    
    replace <- which(!is.na(tmp[,datelmp_history]) & !is.na(tmp[,"date_attendance_for_previous_lmp"]) & tmp[,datelmp_history] < tmp[,"date_attendance_for_previous_lmp"])
    tmp$use_previous_timepoint[replace] <- 1
    
    tmp[replace,datelmp_history] <- NA
    print(paste0(length(replace), " dates LMP where datelmp_history is prior to date of attendace at previous timepoint, set to NA"))
    length(which(tmp$use_previous_timepoint == 1))
  }
  
  tmp[,dif_date_attendance_minus_impute] <- NULL
  tmp[,dif_date_impute_minus_attendance] <- NULL
  
  # If datelmp_impute is prior to date of attendance at previous timepoint then set to NA
  if(i == 2){
    replace <- which(!is.na(tmp[,datelmp_impute]) & !is.na(tmp[,date_attendance_previous]) & !is.na(tmp[,datelmp_previous]) & tmp[,datelmp_impute] < tmp[,date_attendance_previous]) 
    tmp$use_previous_timepoint[replace] <- 1
    
    tmp[replace,datelmp_impute] <- NA
    tmp[replace,datelmp_history] <- NA
    
  }else if(i > 2){
    
    tmp$previous_lmp_date_index <- NULL
    tmp$previous_lmp_date_index <- apply(tmp[,paste0("datelmp_",c(1:(i-1)))], 1, function(row) {
      cols <- which(!is.na(row)) # Identify non-NA column indices
      if (length(cols) == 0) {
        return(NA) # If no non-NA values, return NA
      }
      max(cols) # Otherwise, return the largest column index
    })
    
    tmp$date_attendance_for_previous_lmp <- NULL
    tmp$date_attendance_for_previous_lmp <- NA_Date_
    for (j in 1:nrow(tmp)) {
      if(!is.na(tmp$previous_lmp_date_index[j])){
        tmp$date_attendance_for_previous_lmp[j] <- tmp[j,paste0("date_",tmp$previous_lmp_date_index[j])]
      }
    }
    
    replace <- which(!is.na(tmp[,datelmp_impute]) & !is.na(tmp[,"date_attendance_for_previous_lmp"]) & tmp[,datelmp_impute] < tmp[,"date_attendance_for_previous_lmp"])
    tmp$use_previous_timepoint[replace] <- 1
   
    tmp[replace,datelmp_impute] <- NA
    print(paste0(length(replace), " dates LMP where datelmp_impute is prior to date of attendace at previous timepoint, set to NA"))
    
  }
  
  ################### REPLACE WITH PREVIOUS DATE OF LMP ##########################
  
  if(i>1){
    
    if(i==2){
      #tmp$date_diff_previous <- as.numeric(tmp[,date_attendance] - tmp[,date_attendance_previous])
      tmp$age_diff_previous <- as.numeric(tmp[,age] - tmp[,age_previous])
      
      #replace <- which(tmp$ylmp_missing == 1 & tmp[,period12] == 0 & (tmp[,period3] == 0 | is.na(tmp[,period3])) & tmp$date_diff_previous <= 365)
      replace <- which(tmp$ylmp_missing == 1 & tmp[,period12] == 0 & (tmp[,period3] == 0 | is.na(tmp[,period3])) & tmp$age_diff_previous <= 3)
      
      tmp$use_previous_timepoint[replace] <- 1
      
      tmp[which(!is.na(tmp[,date_attendance])),datelmp_replace] <- tmp[which(!is.na(tmp[,date_attendance])),datelmp_previous]
      tmp[which(is.na(tmp$use_previous_timepoint)),datelmp_replace] <- NA

    }else{
      
      # Get those who have a date of attendance and at least one non-NA date of LMP from previous timepoint
      attended <- which(!is.na(tmp[,date_attendance]))
      rows_with_all_na <- apply(tmp[,paste0("datelmp_",1:(i-1))], 1, function(row) all(is.na(row)))
      replace <- intersect(attended,which(!rows_with_all_na))
      
      # Set datelmp_replace as the maximum date of LMP from previous timepoints (this will be the last non-NA date)
      tmp[replace,datelmp_replace] <- apply(tmp[replace,paste0("datelmp_",1:(i-1))], 1, function(row) max(row, na.rm = TRUE))
      tmp[,datelmp_replace] <- as.Date(tmp[,datelmp_replace])
      
      tmp$max_index <- apply(tmp[,c(paste0("datelmp_",1:(i-1)),datelmp_replace)], 1, function(row) {
        date_lmp_previous_index <- row[1:i-1]
        date_lmp_previous_chosen <- row[i]
        matching_indices <- which(date_lmp_previous_index == date_lmp_previous_chosen)
        # Return the max index or NA if no matches
        if (length(matching_indices) == 0) NA else max(matching_indices)
      })
      
      tmp$date_previous <- NA_Date_
      tmp$age_previous <- NA_integer_
      
      for (j in 1:nrow(tmp)) {
        if(!is.na(tmp$max_index[j])){
          tmp$date_previous[j] <- tmp[j,paste0("date_",tmp$max_index[j])]
          tmp$age_previous[j] <- tmp[j,paste0("age_",tmp$max_index[j])]
        }else{
          tmp$date_previous[j] <- NA_Date_
          tmp$age_previous[j] <- NA_integer_
        }
      }
      
      tmp$date_diff_previous <- NULL
      tmp$age_diff_previous <- NULL
      tmp$date_diff_previous <- as.numeric(tmp[,date_attendance] - tmp$date_previous)
      tmp$age_diff_previous <- as.numeric(tmp[,age] - tmp$age_previous)
      
      #replace <- which(tmp$ylmp_missing == 1 & tmp[,period12] == 0 & (tmp[,period3] == 0 | is.na(tmp[,period3])) & tmp$date_diff_previous <= 365)
      replace <- which(tmp$ylmp_missing == 1 & tmp[,period12] == 0 & (tmp[,period3] == 0 | is.na(tmp[,period3])) & tmp$age_diff_previous <= 3)
      
      tmp$use_previous_timepoint[replace] <- 1
      tmp[which(is.na(tmp$use_previous_timepoint)),datelmp_replace] <- NA

    }
  }
  # Determine date of LMP
  tmp[,datelmp] <- tmp[,datelmp_impute]
  
  replace <- which(is.na(tmp[,datelmp]))
  tmp[replace,datelmp] <- tmp[replace,datelmp_history]
  
  replace <- which(is.na(tmp[,datelmp]))
  tmp[replace,datelmp] <- tmp[replace,datelmp_replace]
  
  # Replace date of attendance with date of LMP if date of LMP is after date of attendance
  # For datelmp_impute we allow this date to be up to 31 days post attendance
  # Date of attendance will only be changed in these cases
  
  replace <- which(tmp[,datelmp] > tmp[,date_attendance]
                   & !is.na(tmp[,datelmp])
                   & !is.na(tmp[,date_attendance]))
  
  tmp[replace, date_attendance] <- tmp[replace, datelmp]
  
  print(paste0("Timepoint ",i,": ",length(replace)," dates of attendance after date of LMP"))
  print(paste0("Timepoint ",i,": ",length(which(!is.na(tmp[,datelmp])))," out of ", length(which(!is.na(tmp[,date_attendance]))), " participants with date LMP"))
  
  replace <- which(tmp[,"ylmp_present"] == 1 & is.na(tmp[,datelmp]))
  tmp[replace,use_straw] <- 0
  
}

tmp$final_lmp_index <- apply(tmp[,paste0("datelmp_",c(1:8))], 1, function(row) {
  cols <- which(!is.na(row)) # Identify non-NA column indices
  if (length(cols) == 0) {
    return(NA) # If no non-NA values, return NA
  }
  max(cols) # Otherwise, return the largest column index
})

tmp$fmp <- as.Date(NA)

for(i in 1:nrow(tmp)){
  if(!is.na(tmp$final_lmp_index[i])){
    tmp$fmp[i] <- tmp[i,paste0("datelmp_", tmp$final_lmp_index[i])]
  }
}
print(paste0(length(which(!is.na(tmp$fmp)))," out of ",nrow(tmp), " participants with date of LMP"))

tmp$date_attendance_fmp <- as.Date(NA)
for(i in 1:nrow(tmp)){
  if(!is.na(tmp$final_lmp_index[i])){
    tmp$date_attendance_fmp[i] <- tmp[i,paste0("date_", tmp$final_lmp_index[i])]
  }
}

length(which(!is.na(tmp$date_attendance_fmp)))

length(which(tmp$date_attendance_fmp - tmp$fmp > 365))

tmp$age_final <- NA
for(i in 1:nrow(tmp)){
  if(!is.na(tmp$final_lmp_index[i])){
    tmp$age_final[i] <- tmp[i,paste0("age_", tmp$final_lmp_index[i])]
  }
}

length(which(!is.na(tmp$age_final)))

mean(tmp$age_final, na.rm = T)

fmp_df <- menopause_df
fmp_df[,paste0("date_",1:8)] <- NULL

tmp <- tmp[,c("aln",paste0("date_",1:8),paste0("datelmp_",1:8),paste0("age_last_period_",1:8),paste0("use_straw_",1:8),"fmp","date_attendance_fmp","age_final")]
fmp_df <- fmp_df %>% left_join(tmp)


rm(list = setdiff(ls(), c("df","df_duplicates_removed","menopause_df","tmp","counts_total","fmp_df","filestore")))
