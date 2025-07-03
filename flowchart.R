rm(list = ls())
source("~/PhD/file_paths_for_scripts/menopause_variables_script.R")
setwd("~/GitHub/Estimating-age-of-menopause-in-mothers-in-the-ALSPAC-Study")

library(dplyr)

# Load cohort data set
df <- readRDS(paste0(filestore,"cohort_lmp_variables.rds"))


# Number ALSPAC recruited
length(unique(df$aln))

# Number participants at each timepoint

for(i in c("1","2","3","4","t","u","v","y","mb")){
  print(i)
  print(length(unique(df$aln[which(!is.na(df[,paste0("date_",i)]))])))
}

# Year of completion
for(i in c("1","2","3","4","t","u","v","y","mb")){
  print(i)
  print(paste0(year(min(df[,paste0("date_",i)],na.rm = T))," - ",year(max(df[,paste0("date_",i)],na.rm = T))))
}

# Mean age
for(i in c("1","t","u","2","v","3","4","y","mb")){
  print(i)
  print(mean(df[,paste0("age_",i)],na.rm = T))
}

for(i in c("1","t","u","2","v","3","4","y","mb")){
  print(i)
  print(sd(df[,paste0("age_",i)],na.rm = T))
}

# Number of women responding to at least one time point
# Number with at least one time point
length(which(!is.na(df$date_1) | !is.na(df$date_2) | !is.na(df$date_3) | !is.na(df$date_4) | !is.na(df$date_t)| !is.na(df$date_u) | !is.na(df$date_v) | !is.na(df$date_y)))


flowchart <- readRDS(paste0(filestore,"flowchart_df.rds"))

flowchart$date_min <- apply(flowchart[,paste0("date_",c("1","2","3","4","t","u","v","y"))], 1, function(x) min(x, na.rm = TRUE))

flowchart$col_indices <- apply(flowchart[,paste0("date_",c("1","2","3","4","t","u","v","y"))], 1, function(x) {
  valid_indices <- which(x == min(x, na.rm = TRUE))  # Find indices of min values
  if (length(valid_indices) > 0) valid_indices[1] else NA  # Return the first index or NA
})

flowchart$age_min <- NA_integer_
colnames <- c("1","2","3","4","t","u","v","y")

for (i in 1:nrow(flowchart)) {
  if(!is.na(flowchart$col_indices[i])){
    index <- colnames[flowchart$col_indices[i]]
    flowchart$age_min[i] <- flowchart[i,paste0("age_",index)]
  }
}

length(which(flowchart$periods_stopped_surgery_date <= flowchart$date_min |
        flowchart$periods_stopped_surgery_age <= flowchart$age_min))

flowchart <- flowchart[-which(flowchart$periods_stopped_surgery_date <= flowchart$date_min |
                             flowchart$periods_stopped_surgery_age <= flowchart$age_min),]


# Contraception or HRT is used at all timepoints

flowchart$all_contracept_hrt <- NA
#i=3
for(i in 1:nrow(flowchart)){
  dates <- as.Date(unlist(flowchart[i, paste0("date_",colnames)]))
  contracept <- unlist(flowchart[i, paste0("contracept_",colnames)])
  hrt <- unlist(flowchart[i, paste0("hrt_",colnames)])
  
  #identical(as.numeric(which(!is.na(dates))),as.numeric(which(contracept == 1 | hrt == 1)))

  if(identical(as.numeric(which(!is.na(dates))),as.numeric(which(contracept == 1 | hrt == 1)))){
    flowchart$all_contracept_hrt[i] <- T
  }
}

table(flowchart$all_contracept_hrt)


# Other medical reason is used at all timepoints

flowchart$all_other <- NA

for(i in 1:nrow(flowchart)){
  dates <- as.Date(unlist(flowchart[i, paste0("date_",colnames)]))
  exclude <- unlist(flowchart[i, paste0("exclude_timepoint_",colnames)])
  
  if(identical(as.numeric(which(!is.na(dates))),as.numeric(which(exclude == 1)))){
    flowchart$all_other[i] <- T
  }
}

table(flowchart$all_other)

flowchart$all_other[which(flowchart$all_contracept_hrt == T)] <- NA

table(flowchart$all_other)

which(flowchart$all_contracept == T & flowchart$all_hrt == T )


table(flowchart$all_contracept_hrt,flowchart$all_other)

tmp <- flowchart %>% filter(is.na(all_contracept_hrt)
                            & is.na(all_other))
                            

# Number left after censoring
tmp <- readRDS(paste0(filestore,"cohort_date_ordered.rds"))
length(which(!is.na(tmp$date_1)))

# Number with at least one date of LMP
tmp <- readRDS(paste0(filestore,"fmp_df_new.rds"))
length(which(!is.na(tmp$date_1)))
length(which(!is.na(tmp$date_1) & (!is.na(tmp$datelmp_1) | !is.na(tmp$datelmp_2) | !is.na(tmp$datelmp_3) | !is.na(tmp$datelmp_4) |
                                      !is.na(tmp$datelmp_5) | !is.na(tmp$datelmp_6) | !is.na(tmp$datelmp_7) | !is.na(tmp$datelmp_8))))

# Number with no dates of LMP
length(which(!is.na(tmp$date_1) & (is.na(tmp$datelmp_1) & is.na(tmp$datelmp_2) & is.na(tmp$datelmp_3) & is.na(tmp$datelmp_4) &
                                     is.na(tmp$datelmp_5) & is.na(tmp$datelmp_6) & is.na(tmp$datelmp_7) & is.na(tmp$datelmp_8))))


# Those with at least one date of LMP
use <- which(!is.na(tmp$date_1) & (!is.na(tmp$datelmp_1) | !is.na(tmp$datelmp_2) | !is.na(tmp$datelmp_3) | !is.na(tmp$datelmp_4) |
                                     !is.na(tmp$datelmp_5) | !is.na(tmp$datelmp_6) | !is.na(tmp$datelmp_7) | !is.na(tmp$datelmp_8)))

tmp_lmp <- tmp[use,]

# Number with age at menopause from algorithm
length(which(!is.na(tmp_lmp$fmp) & !is.na(tmp_lmp$age_menopause)))
length(which(is.na(tmp_lmp$fmp) & !is.na(tmp_lmp$age_menopause)))
length(which(!is.na(tmp_lmp$age_menopause)))
length(which(is.na(tmp_lmp$age_menopause)))

# Those with no date of LMP
use <- which(!is.na(tmp$date_1) & (is.na(tmp$datelmp_1) & is.na(tmp$datelmp_2) & is.na(tmp$datelmp_3) & is.na(tmp$datelmp_4) &
                                            is.na(tmp$datelmp_5) & is.na(tmp$datelmp_6) & is.na(tmp$datelmp_7) & is.na(tmp$datelmp_8)))

tmp_no_lmp <- tmp[use,]

# Number with age at menopause from algorithm
length(which(!is.na(tmp_no_lmp$age_menopause)))
length(which(is.na(tmp_no_lmp$age_menopause)))

# Total with age at menopause
length(which(!is.na(tmp$age_menopause)))

# Self-reported age at menopause but did not pass censoring
use_1 <- tmp_lmp$aln[which(!is.na(tmp_lmp$fmp) & !is.na(tmp_lmp$age_menopause))]
use_2 <- tmp_lmp$aln[which(is.na(tmp_lmp$fmp) & !is.na(tmp_lmp$age_menopause))]
use_3 <- tmp_no_lmp$aln[which(!is.na(tmp_no_lmp$age_menopause))]


tmp1 <- tmp %>% filter(!aln %in% use_1
                       & !aln %in% use_2
                       & !aln %in% use_3
                       & !is.na(age_menopause))

# Age at final attended timepoint for those with no age at menopause
tmp_no_meno <- tmp %>% filter(!is.na(date_1) & is.na(age_menopause))

# Get column names of those columns
date_col_names <- paste0("date_",1:8)

# Apply across rows
tmp_no_meno$last_date_col <- apply(tmp_no_meno[date_col_names], 1, function(x) {
  last_non_na <- tail(which(!is.na(x)), 1)
  if (length(last_non_na) == 0) {
    return(NA)
  } else {
    return(date_col_names[last_non_na])
  }
})

tmp_no_meno$last_date_col <- gsub("date_", "", tmp_no_meno$last_date_col)

tmp_no_meno$age_max <- NA_integer_
for (i in 1:nrow(tmp_no_meno)) {
  if (!is.na(tmp_no_meno$last_date_col[i])) {
    age_col <- paste0("age_", tmp_no_meno$last_date_col[i])
    tmp_no_meno$age_max[i] <- tmp_no_meno[i, age_col]
  }
}

tmp_no_meno$age_max

mean(tmp_no_meno$age_max,na.rm = T)
sd(tmp_no_meno$age_max,na.rm = T)
range(tmp_no_meno$age_max,na.rm = T)

table(tmp_no_meno$age_max,useNA = "always")



