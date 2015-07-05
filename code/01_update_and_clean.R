###################
#LOAD  PACKAGES
###################
library(RColorBrewer)
library(xtable)
library(gdata)
library(plyr)
library(maps)
#library(mapdata)
#library(maptools)
#library(rJava)
#library(OpenStreetMap)
#library(rgdal)
library(shapefiles)
library(SemiPar)
library(wordcloud)
#library(RCurl)
library(classInt)
library(data.table)
library(dplyr)

###################
#SET DATE / TIME PARAMETERS
###################
today <- Sys.Date() 
yesterday <- today - 1

###################
#DEFINE AND SET WD
###################
if ( Sys.info()["sysname"] == "Linux" ){
  # Joe's linux
  if(Sys.info()["user"] == "joebrew"){
    private_today <- paste0("/media/joebrew/JB/fdoh/private/surv/", today)
    private <- "/media/joebrew/JB/fdoh/private/surv"
    private_historical <- "/media/joebrew/JB/fdoh/private/surv/historical"
    public_gis <- "/media/joebrew/JB/fdoh/private/surv/gis"
  }
  # Ben's linux
  else {
    private_today <- paste0("/home/benbrew/Documents/private/surv/", today)
    private <- "/home/benbrew/Documents/private/surv/"
    private_historical <- "/home/benbrew/Documents/private/surv/historical"
    public_gis <- "/home/benbrew/Documents/surv_public/gis"
  }
  # Joe's Windows computers:
} else {
  private_today <- paste0("E:/fdoh/private/surv/", today)
  private <- "E:/fdoh/private/surv"
  private_historical <- "E:/fdoh/private/surv/historical"
  public_gis <- "E:/fdoh/private/surv/gis"
}

# SET WD
setwd(private_today) 

###################
#READ IN DATA FROM ESSENCE
###################
setwd(private_today) 

my_files <- dir()
for (i in my_files){
  if (grepl("txt", i)){
    assign(x = gsub(".txt", "", i),
           value = read.table(i, sep= ",", header=TRUE,
                              stringsAsFactors = FALSE))
  }
}

###################
#ADD A CCDDCATEGORY COLUMN
###################
gi$cat <- "gi"
ili$cat <- "ili"
neuro$cat <- "neuro"
rash$cat <- "rash"
resp$cat <- "resp"

###################
#COMBINE THE 5 TRACKED SYMPTOM FILES
###################
sym <- as.data.frame(rbind(gi, ili, neuro, rash, resp))

# Now that they're combined, remove the individual symptom files
rm(gi, ili, neuro, rash, resp)

###################
#CLEAN AND COMBINE RECORDS OF INTEREST
###################
roi <- rbind(roi, roi2) ; roi <- roi[!duplicated(roi$MedRecNo),] ; rm(roi2)

###################
#READ IN OLD FILES
###################
setwd(private_historical)

alless_old <- read.csv("alless_old_updated.csv")
sym_old <- read.csv("sym_old_updated.csv")

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
# apply to the old data too
alless_old$Date <- datify2(alless_old$Date)
sym_old$Date <- datify2(sym_old$Date)

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
# ELIMINATE ROWS FROM ALLESS2 IF ALACHUA RESIDENT
###################
alless2 <- alless2[which(alless2$Region != "Alachua"),]

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

alless_old <- alless_old[,keep_columns]
alless <- alless[,keep_columns]
sym <- sym[,c(keep_columns, "cat")]
sym_old <- sym_old[,c(keep_columns, "cat")]

# Update the old with the new
sym_old <- rbind(sym_old[which(sym_old$Date < min(sym$Date)),], sym)
alless_old <- rbind(alless_old[which(alless_old$Date < min(alless$Date)),],alless)

# Remove the new stuff
rm(alless, sym)


##################
# COMBINE SYM AND ALLESS
##################
alachua <- left_join(x = alless_old,
               y = sym_old)

# Specify which ones are non-essence named
alachua$cat <- as.character(alachua$cat)
alachua$cat[which(is.na(alachua$cat))] <- "none"

# Eliminate duplicates
alachua <- alachua[!duplicated(alachua),]

###################
#WRITE HISTORICAL CSVs
###################
setwd(private_historical)
write.csv(sym_old, "sym_old_updated.csv")
write.csv(alless_old, "alless_old_updated.csv")

###################
# WRITE R IMAGE FILE TO READ IN FOR THE NEXT FILE
###################
rm(alless_old, sym_old, keep_columns, my_files)
setwd(private_today)
save.image("cleaned.RData")

