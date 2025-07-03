# Get date of attendance
date_attendance <- function(tmp, month, year, name,pre_2000){
  print(unique(tmp[,month]))
  print(unique(tmp[,year]))
  
  if(pre_2000 == T){
    tmp[,year] <- ifelse(tmp[,year] <0 | is.na(tmp[,year]), NA, tmp[,year])
    tmp[which(!is.na(tmp[,year]) & tmp[,year] > 0),year][] <- paste0("19",tmp[!is.na(tmp[,year]) & tmp[,year] > 0,year])
    print(unique(tmp[,year]))
  }
  
  #Only use valid dates
  tmp[,paste0("date_string_",name)] <- ifelse(tmp[,month] >= 1 & tmp[,month] <= 12 & !is.na(tmp[,month])
                                              & tmp[,year] > 0 & tmp[,year] <= as.numeric(format(Sys.Date(), "%Y")) & !is.na(tmp[,year]),
                                              paste0(tmp[,month],"/15/",tmp[,year]),NA)
  #Convert to date format
  tmp[,paste0("date_",name)] <- as.Date(tmp[,paste0("date_string_",name)], format = "%m/%d/%Y")
  print(unique(tmp[,paste0("date_",name)]))
  
  return(tmp)
}

# Get date of attendance
date_attendance_with_day <- function(tmp, day, month, year, name,pre_2000){
  print(unique(tmp[,day]))
  print(unique(tmp[,month]))
  print(unique(tmp[,year]))
  
  if(pre_2000 == T){
    tmp[,year] <- ifelse(tmp[,year] <0 | is.na(tmp[,year]), NA, tmp[,year])
    tmp[which(!is.na(tmp[,year]) & tmp[,year] > 0),year][] <- paste0("19",tmp[!is.na(tmp[,year]) & tmp[,year] > 0,year])
    print(unique(tmp[,year]))
  }
  
  #Only use valid dates
  tmp[,paste0("date_string_",name)] <- ifelse(tmp[,day] >= 1 & tmp[,day] <= 31 & !is.na(tmp[,day])
                                              & tmp[,month] >= 1 & tmp[,month] <= 12 & !is.na(tmp[,month])
                                              & tmp[,year] > 0 & tmp[,year] <= as.numeric(format(Sys.Date(), "%Y")) & !is.na(tmp[,year]),
                                              paste0(tmp[,month],"/",tmp[,day],"/", tmp[,year]),NA)
  
  
  #Convert to date format
  tmp[,paste0("date_",name)] <- as.Date(tmp[,paste0("date_string_",name)], format = "%m/%d/%Y")
  print(unique(tmp[,paste0("date_",name)]))
  
  return(tmp)
}

# Get age at attendance
age_attendance <- function(tmp,age,name){
  # Age at attendance
  print(unique(tmp[,age]))
  tmp[,paste0("age_",name)] <- tmp[,age]
  print(table(tmp[,paste0("age_",name)], useNA = "always"))
  
  tmp[,paste0("age_",name)] <- ifelse(tmp[,paste0("age_",name)] < 0 | is.na(tmp[,paste0("age_",name)]), NA,tmp[,paste0("age_",name)])
  print(table(tmp[,paste0("age_",name)], useNA = "always"))
  
  return(tmp)
}


