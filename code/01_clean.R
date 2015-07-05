county <- 'alachua'

###################
#SET DATE / TIME PARAMETERS
###################
today <- Sys.Date() 

# Adjust for oddity
# today <- today - 1
yesterday <- today - 1

###################
#DEFINE AND SET WD
###################
public <- '/home/joebrew/Documents/surv_florida'
private <- paste0(public, '/data')
private_today <- paste0(public, '/data/', today)
private_historical <- paste0(public, '/data/historical')

setwd(paste0(private_today, '/', county))


###################
#READ IN DATA FROM ESSENCE
###################

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
# # apply to the old data too
# alless_old$Date <- datify2(alless_old$Date)
# sym_old$Date <- datify2(sym_old$Date)

###################
#CLEAN UP RACE COLUMN
###################

black <- "BLA*|AFR*|*BLA|*AFR|Bla*|Afr*|*Bla*|*Afr"
white <- "WHI*|CAUC*|*WHI|*CAUC|Whi*|Cauc*|*Whi|*Cauc"

racify <- function(data){
  ifelse(grepl(black, data$Race_flat), "black",
         ifelse(grepl(white, data$Race_flat), "white",
                "other"))
}
sym$race <- racify(data = sym)
alless$race <- racify(data = alless)
alless2$race <- racify(data = alless2)

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


