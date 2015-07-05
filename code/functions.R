# Function to open link
open_link <- function(syndrome){
  browseURL(df$link[which(df$file == paste0(syndrome, ".txt"))])
}

# Function to get link
get_link <- function(syndrome = NULL,
                     patient_location = "alachua",
                     hospital_location = NULL,
                     start_date = NULL,
                     end_date = NULL,
                     text = TRUE,
                     days_back = 15){
  
  # Patient location
  if(!is.null(patient_location)){
    patient_location <- paste0("patientLoc=", patient_location, "&")
  } else{
    patient_location <- ""
  }
  
  # Hospital location
  if(!is.null(hospital_location)){
    hospital_location <- paste0("geography=", hospital_location, "&")
  } else {
    hospital_location <- ""
  }
  
  # Syndrome
  if(!is.null(syndrome)){
    syndrome <- paste0("medicalGrouping=", syndrome, "&")
  } else {
    syndrome <- ""
  }
  
  # Start date
  if(is.null(start_date)){
    start_date <- paste0("startDate=", format(Sys.Date() - days_back, format = "%d%b%Y"), "&")
  } else {
    start_date <- format(start_date, format = "%d%b%Y")
  }
  
  # End date
  if(is.null(end_date)){
    end_date <- paste0("endDate=", format(Sys.Date() - 1, format = "%d%b%Y"), "&")
  } else {
    end_date <- format(end_date, format = "%d%b%Y")
  }
  
  if(text){
    
    if(is.null(hospital_location)){
      link <- paste0("https://www.essencefl.com/florida_5_1_14/servlet/PlainDataDetailsServlet?ageCDCILI=all&", 
                     start_date, 
                     "medicalGroupingSystem=essencesyndromes&initPulseOx=all&sex=all&geographySystem=hospitalregion&predomRace=all&dom=all&patientClass=all&timeResolution=daily&doy=all&censusRaceBlackPercGroup=all&", 
                     end_date, "
                     dow=all&clinicalImpression=all&ageTenYear=all&detector=probrepswitch&geography=all&", 
                     patient_location, 
                     "age=all&dischargeDiagnosis=all&year=all&medicalSubGrouping=all&datasource=va_hosp&censusRaceAsianPercGroup=all&percentParam=noPercent&", 
                     syndrome, 
                     "timeInterval=all&aqtTarget=datadetails&hospitalGrouping=all&agerange=all&censusRaceHawaiianPercGroup=all&ccddFreeText=all&predomHispanic=all&initTemp=all&diagnosisType=all&censusRaceAmerIndPercGroup=all&dispositionCategory=all&medianIncomeGroup=all&agedistribute=all&month=all&ccddCategory=all&censusRaceOtherPercGroup=all&censusRaceWhitePercGroup=all&week=all&quarter=all")
      
    } else {
      link <- paste0("https://www.essencefl.com/florida_5_1_14/servlet/PlainDataDetailsServlet?ageCDCILI=all&", 
                     start_date, 
                     "medicalGroupingSystem=essencesyndromes&geographySystem=hospitalregion&dom=all&timeResolution=daily&doy=all&",
                     end_date, "dow=all&detector=probrepswitch&", 
                     patient_location, 
                     hospital_location, 
                     "year=all&datasource=va_hosp&percentParam=noPercent&", 
                     syndrome, 
                     "aqtTarget=DataDetails&month=all&week=all&quarter=all")
      #                      "medicalGroupingSystem=essencesyndromes&initPulseOx=all&sex=all&geographySystem=hospitalregion&predomRace=all&dom=all&patientClass=all&timeResolution=daily&doy=all&censusRaceBlackPercGroup=all&", 
      #                      end_date, 
      #                      "dow=all&clinicalImpression=all&ageTenYear=all&detector=probrepswitch&", 
      #                      hospital_location, 
      #                      patient_location, 
      #                      "age=all&dischargeDiagnosis=all&year=all&medicalSubGrouping=all&datasource=va_hosp&censusRaceAsianPercGroup=all&percentParam=noPercent&", 
      #                      syndrome, 
      #                      "timeInterval=all&aqtTarget=datadetails&hospitalGrouping=all&agerange=all&censusRaceHawaiianPercGroup=all&ccddFreeText=all&predomHispanic=all&initTemp=all&diagnosisType=all&censusRaceAmerIndPercGroup=all&dispositionCategory=all&medianIncomeGroup=all&agedistribute=all&month=all&ccddCategory=all&censusRaceOtherPercGroup=all&censusRaceWhitePercGroup=all&week=all&quarter=all")
      
    }
    
  } else{
    link <- paste0("https://www.essencefl.com/florida_5_1_14/servlet/DataDetailsServlet?", 
                   start_date, 
                   "medicalGroupingSystem=essencesyndromes&geographySystem=hospitalregion&dom=all&timeResolution=daily&doy=all&",
                   end_date, "dow=all&detector=probrepswitch&", 
                   patient_location, 
                   hospital_location, 
                   "year=all&datasource=va_hosp&percentParam=noPercent&", 
                   syndrome, 
                   "aqtTarget=DataDetails&month=all&week=all&quarter=all")
  }
  
  return(link)  
}

# Function to create dataframe of today's links for a certain county
df_link <- function(county = 'alachua', 
                    parent_dir = NULL,
                    historical = FALSE){
  
  if(historical){
    db <- 395
  } else {
    db <- 15
  }

  # CREATE DATAFRAME OF links
  df <- data.frame(file = paste0(c("alless", "alless2", "roi", "roi2",
                                   "gi", "ili", "neuro", "rash", "resp"), ".txt"))
  df$link <- NA
  df$link[which(df$file == "alless.txt")] <- get_link(patient_location = county,
                                                      days_back = db)
  df$link[which(df$file == "alless2.txt")] <- get_link(patient_location = NULL,
                                                       hospital_location = county,
                                                       days_back = db)
  df$link[which(df$file == "roi.txt")] <- get_link(patient_location = county,
                                                   syndrome = "RecordsOfInterest",
                                                   days_back = db)
  df$link[which(df$file == "roi2.txt")] <- get_link(patient_location = NULL,
                                                    hospital_location = county,
                                                    syndrome = "RecordsOfInterest",
                                                    days_back = db)
  df$link[which(df$file == "gi.txt")] <- get_link(patient_location = county, syndrome = "gi",
                                                  days_back = db)
  df$link[which(df$file == "ili.txt")] <- get_link(patient_location = county, syndrome = "ili",
                                                   days_back = db)
  df$link[which(df$file == "neuro.txt")] <- get_link(patient_location = county, syndrome = "neuro",
                                                     days_back = db)
  df$link[which(df$file == "rash.txt")] <- get_link(patient_location = county, syndrome = "rash",
                                                    days_back = db)
  df$link[which(df$file == "resp.txt")] <- get_link(patient_location = county, syndrome = "resp",
                                                    days_back = db)
  
  # WRITE A CSV WITH MOST RECENT LINKS
  if(is.null(parent_dir)){
    parent_dir <- getwd()
  }
  
  directory <- paste0(parent_dir, '/', county)
  
    if(!dir.exists(directory)){
    dir.create(directory)
    setwd(directory)
  } 
  setwd(directory) 
  write.csv(df, ifelse(historical, 'historical_links.csv', 'todays_links.csv'))
  setwd(parent_dir)
}
