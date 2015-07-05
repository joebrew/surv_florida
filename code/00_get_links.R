# GETTING INITIAL HISTORICAL DATA OR NOT?
old <- FALSE

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

#####
# SOURCE HELPER FUNCTIONS
#####
source(paste0(public, '/code/functions.R'))

######
# SET WORKING DIRECTORY FOR TODAY (CREATE IF NEEDED)
#####
setwd(private)
if(!dir.exists(private_today)){
  dir.create(private_today)
} 
setwd(private_today) 


#####
# CREATE A DATAFRAME OF TODAY'S LINKS FOR EVERY COUNTY
#####
counties <- read.csv(paste0(public, '/counties.csv'))
for (i in 1:nrow(counties)){
  county <- counties$x[i]
  df_link(county)
}

######
# IF FETCHING OLD HISTORICAL DATA ONLY
#####
if(old){
  setwd(private_historical)
  
  for (i in 1:nrow(counties)){
    county <- counties$x[i]
    cat(paste0('Working on ', county, '\n'))
    
    county_historical_dir <- paste0(private_historical, '/',county)
    if(!dir.exists(county_historical_dir)){
      dir.create(county_historical_dir)
    } 
    setwd(county_historical_dir) 
    # Write the csv of historical links
    df_link(county,
            historical = TRUE,
            parent_dir = private_historical)
    setwd(private_historical)
    cat(paste0('Done with ', county, '\n'))
    
  }
}

