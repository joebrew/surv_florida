########### HAVING FETCHED ALL THE OLD DATA,
########### RUN THIS TO DOWNSIZE IT A BIT
########### AND GET IT IN NICE CSVs

library(dplyr)

###################
#DEFINE AND SET WD
###################
public <- '/home/joebrew/Documents/surv_florida'
private <- paste0(public, '/data')
private_historical <- paste0(public, '/data/historical')


#####
# READ IN ALL THE COUNTIES
#####
setwd(public)
counties <- read.csv('counties.csv')

######
# SET WORKING DIRECTORY FOR HISTORICAL
#####
setwd(private_historical)


#####
# LOOP THROUGH EACH COUNTY, CONVERTING THE HISTORICAL DATA TO CSVs
#####
for (i in 2:nrow(counties)){ # SKIPPING ALACHUA, SINCE ALREADY DONE
  county <- counties$x[i]
  
  county_hitorical_dir <- paste0(private_historical, '/', county)
  setwd(county_hitorical_dir)
  
  #READ IN DATA FROM ESSENCE
  my_files <- dir()
  for (i in my_files){
    if (grepl("txt", i)){
      assign(x = gsub(".txt", "", i),
             value = read.table(i, sep= ",", header=TRUE,
                                stringsAsFactors = FALSE))
    }
  }
  
  #ADD A CCDDCATEGORY COLUMN
  gi$cat <- "gi"
  ili$cat <- "ili"
  neuro$cat <- "neuro"
  rash$cat <- "rash"
  resp$cat <- "resp"
  
  #COMBINE THE 5 TRACKED SYMPTOM FILES
  sym <- as.data.frame(rbind(gi, ili, neuro, rash, resp))
  
  # Now that they're combined, remove the individual symptom files
  rm(gi, ili, neuro, rash, resp)
  
  #CLEAN AND COMBINE RECORDS OF INTEREST
  roi <- rbind(roi, roi2) ; roi <- roi[!duplicated(roi$MedRecNo),] ; rm(roi2)
  
  ###################
  #FORMAT ALL DATES
  ###################
  
  # First, write functions for standardizing dates
  datify <- function(x){as.Date(x, "%m/%d/%Y")}
  datify2 <- function(x){as.Date(x, "%Y-%m-%d")}
  
  # Now apply those functions to our data
  sym$Date <- datify(sym$Date)
  alless$Date <- datify(alless$Date)
  alless2$Date <- datify(alless2$Date)
  roi$Date <- datify(roi$Date)
  
  ###################
  #CLEAN UP RACE COLUMN
  ###################
  
  racify <- function(data){
    black <- 'black|blac|afri|afro|afr'
    white <- "cauc|whit"
    asian <- 'asia|chin|japa|kore'
    hispanic <- 'spani|hispa|mexic|lati'
    ifelse(grepl(black, tolower(data$Race_flat)), "black",
           ifelse(grepl(white, tolower(data$Race_flat)), "white",
                  ifelse(grepl(asian, tolower(data$Race_flat)), 'asian',
                         ifelse(grepl(hispanic,tolower(data$Race_flat)), 'hispanic', 'other'))))
  }
  sym$race <- racify(data = sym)
  alless$race <- racify(data = alless)
  alless2$race <- racify(data = alless2)
  roi$race <- racify(data = roi)
  
  ###################
  # ELIMINATE ROWS FROM ALLESS2 IF RESIDENT OF COUNTY IN QUESTION
  # (since repeated in alless)
  ###################
  alless2 <- alless2[which(tolower(alless2$Region) != county),]
  
  
  ###################
  #COMBINE THE OLD AND NEW DATA
  ###################
  keep_columns <- c("Date", "Age", "Zipcode",
                    "MedRecNo", "Sex", "CCDD",
                    "Region", "Time", "HalfHour", 
                    "Hospital", "Region.of.the.Hospital",
                    "CCDDCategory_flat", "Race_flat",
                    "Disposition.Category",
                    "HospitalName",
                    "HospitalZipCode", "race")
  
  alless <- alless[,keep_columns]
  alless2 <- alless2[,keep_columns]
  sym <- sym[,c(keep_columns, "cat")]
  roi <- roi[,keep_columns]
  
  ###################
  #WRITE HISTORICAL CSVs
  ###################
  write.csv(sym, "sym_old_updated.csv", row.names = FALSE)
  write.csv(alless, "alless_old_updated.csv", row.names = FALSE)
  write.csv(alless2, "alless2_old_updated.csv", row.names = FALSE)
  write.csv(roi, "roi_old_updated.csv", row.names = FALSE)
  
  #####
  # DELETE THE TXT FILES
  #####
  remove_these <- dir()[grepl('.txt', dir())]
  for (i in 1:length(remove_these)){
    file.remove(remove_these[i])
  }
  
  #####
  # GO BACK TO MAIN HISTORICAL FOLDER
  #####
  setwd(private_historical)
}



