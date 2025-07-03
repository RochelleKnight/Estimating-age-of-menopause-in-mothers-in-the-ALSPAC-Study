# We use data from FoM1, FoM2, FoM3, FoM4 and questionnaires T, U, V & Y
# This data needs to be in chronological date order however there is overlap between the data collection of
# some of the clinics and questionnaires. For this reason each participant will have completed the 
# clinics and questionnaires in different orders.

# There are some participants with dates of completion (month/year) for a clinic and a questionnaire that are the same.
# In these instances we favour the clinic data as this was collected in person by trained personnel.
# If any of the clinic data is missing, we update it using the questionnaire data where possible.
# The questionnaire data is then all set to NA.

library(dplyr)
library(stringr)

# Variables of interest to be updated where possible
variable_list <- c("contracept","hrt","exclude_timepoint","dlmp","mlmp","ylmp","period3","period12","age","age_last_period","regperiod")
variable_list_use_higher <- c("contracept","hrt","exclude_timepoint","period3","period12","age_last_period","regperiod")

variable_suffix <- c("1","2","3","4","t","u","v","y")

# Select needed columns from df
selected_columns <- unlist(c("aln", paste0("date_",variable_suffix),lapply(variable_list, function(variable_list) paste0(variable_list,"_", variable_suffix))))

# Select columns from df
df_duplicates_removed <- df %>% select(all_of(selected_columns))

# Loop through rows of menopause_df
for (i in seq_len(nrow(df_duplicates_removed))) {
  # Extract dates for each row
  dates <- as.Date(unlist(df_duplicates_removed[i, paste0("date_",variable_suffix)], use.names = TRUE))
  
  # Remove NA values
  dates <- dates[!is.na(dates)]
  
  # If there are duplicate dates i.e a clinic and questionnaire with the same date, start loop
  if(any(duplicated(dates))==T){
    
    # Get the duplicated date
    duplicates <- dates[duplicated(dates)]
    
    # Loop over the duplicated dates e.g. there could be multiple clinics/questionnaires with the same date
    for(k in 1:length(duplicates)){
      
      # Get the duplicated date
      duplicate_variable <- duplicates[k]
      
      # Get the date label which shows where duplicate data is e.g. which clinic & questionnaire
      duplicate_suffix <- str_replace(labels(dates)[which(dates == duplicate_variable)], "date_", "")
      
      # Check that the duplicated dates are from a clinic and a questionnaire and not two clinics with the 
      # same date or two questionnaires with the same date
      # Loop stops with error message
      if(!duplicate_suffix[1] %in% c("1","2","3","4") | !duplicate_suffix[2] %in% c("t","u","v","y")){
        print(duplicate_suffix)
        stop()
      }
      
      # Set the questionnaire completion date to NA
      # All questionnaire date at the duplicate date will be set to NA
      df_duplicates_removed[i,paste0("date_",duplicate_suffix[2])] <- NA
      
      # Loop over the variables of interest e.g hrt
      for(variable in variable_list){
        
        # Checks to see if there is data to be replaced
        # i.e the clicic data is NA and the questionnaire data is not NA
        # If data to be replaced will print statements
        if((is.na(df_duplicates_removed[i,paste0(variable,"_",duplicate_suffix[1])]) & !is.na(df_duplicates_removed[i,paste0(variable,"_",duplicate_suffix[2])])) == T){
          print(paste0("Replacing row ",i))
          print(paste0("Replacing variable ",variable))
          print(paste0("Using timepoints ", duplicate_suffix))
        }
        
        # Replace clinic data with questionnaire data if clinic data is NA and questionnaire data is not NA
        df_duplicates_removed[i,paste0(variable,"_",duplicate_suffix[1])] <- ifelse((is.na(df_duplicates_removed[i,paste0(variable,"_",duplicate_suffix[1])]) & !is.na(df_duplicates_removed[i,paste0(variable,"_",duplicate_suffix[2])])),
                                                                                    df_duplicates_removed[i,paste0(variable,"_",duplicate_suffix[2])],df_duplicates_removed[i,paste0(variable,"_",duplicate_suffix[1])])
        
        
        if(variable %in% variable_list_use_higher){
          
          if(!is.na(df_duplicates_removed[i,paste0(variable,"_",duplicate_suffix[1])]) & !is.na(df_duplicates_removed[i,paste0(variable,"_",duplicate_suffix[2])])
             & df_duplicates_removed[i,paste0(variable,"_",duplicate_suffix[1])] < df_duplicates_removed[i,paste0(variable,"_",duplicate_suffix[2])]){
            print(paste0("Replacing row ",i))
            print(paste0("Replacing higher variable ",variable))
            print(paste0("Using timepoints ", duplicate_suffix))
          }
          
          # Replace clinic data with questionnaire data if clinic data response is less than questionnaire response
          df_duplicates_removed[i,paste0(variable,"_",duplicate_suffix[1])] <- ifelse(!is.na(df_duplicates_removed[i,paste0(variable,"_",duplicate_suffix[1])]) & !is.na(df_duplicates_removed[i,paste0(variable,"_",duplicate_suffix[2])])
                                                                                      & df_duplicates_removed[i,paste0(variable,"_",duplicate_suffix[1])] < df_duplicates_removed[i,paste0(variable,"_",duplicate_suffix[2])],
                                                                                      df_duplicates_removed[i,paste0(variable,"_",duplicate_suffix[2])],df_duplicates_removed[i,paste0(variable,"_",duplicate_suffix[1])])
        }
        
        # Set questionnaire variable to NA
        df_duplicates_removed[i,paste0(variable,"_",duplicate_suffix[2])] <- NA
      }
    }
  }
}


rm(list = setdiff(ls(), c("df","df_duplicates_removed","filestore")))
