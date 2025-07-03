tmp <- menopause_df %>% select(aln,paste0("age_",1:8),paste0("age_last_period_",1:8))

for (i in 2:8) {
  if(i == 2 ){
    replace <- which(!is.na(tmp[,paste0("age_last_period_",i)]) 
                     & !is.na(tmp[,paste0("age_last_period_",(i-1))]) 
                     & tmp[,paste0("age_last_period_",i)] < tmp[,paste0("age_",(i-1))]
                     & tmp[,paste0("age_last_period_",i)] != tmp[,paste0("age_last_period_",(i-1))])
    
    tmp[replace,paste0("age_last_period_",i)] <- NA
    
  }else if(i > 2){
    
    tmp$age_last_period_previous_index <- NULL
    tmp$age_last_period_previous_index <- NA_integer_
    
    tmp$age_last_period_previous_index <- apply(tmp[,paste0("age_last_period_",c(1:(i-1)))], 1, function(row) {
      cols <- which(!is.na(row)) # Identify non-NA column indices
      if (length(cols) == 0) {
        return(NA) # If no non-NA values, return NA
      }
      max(cols) # Otherwise, return the largest column index
    })
    
    tmp$age_previous <- NULL
    tmp$age_previous <- NA_integer_
    
    for (j in 1:nrow(tmp)) {
      if(!is.na(tmp$age_last_period_previous_index[j])){
        tmp$age_previous[j] <- tmp[j,paste0("age_",tmp$age_last_period_previous_index[j])]
      }
    }
    
    replace <- which(tmp[,paste0("age_last_period_",i)] < tmp[,"age_previous"] & tmp[,paste0("age_last_period_",i)] != tmp[,paste0("age_last_period_",(i-1))] )
    tmp[replace,paste0("age_last_period_",i)] <- NA
    
  }
}


tmp$age_last_period_final_index <- apply(tmp[,paste0("age_last_period_",1:8)], 1, function(row) {
  cols <- which(!is.na(row)) # Identify non-NA column indices
  if (length(cols) == 0) {
    return(NA) # If no non-NA values, return NA
  }
  max(cols) # Otherwise, return the largest column index
})

tmp$age_last_period_final <- NA_integer_
tmp$age_final <- NA_integer_

for (i in 1:nrow(tmp)) {
  if (!is.na(tmp$age_last_period_final_index[i])) {
    # Extract the index from age_last_period_final_index
    index <- tmp$age_last_period_final_index[i]
    
    # Assign the corresponding value to age_last_period_final
    tmp$age_last_period_final[i] <- tmp[i, paste0("age_last_period_", index)]
    
    # Assign the corresponding value to age_final
    tmp$age_final[i] <- tmp[i, paste0("age_", index)]
  }
}

tmp$diff <- tmp$age_final - tmp$age_last_period_final

tmp$age_menopause_using_age_last_period <- tmp$age_last_period_final

replace <- which(tmp$diff<= 1)

tmp$age_menopause_using_age_last_period[replace] <- NA
mean(tmp$age_menopause_using_age_last_period, na.rm = T)
