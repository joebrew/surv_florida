###################
#LOAD  PACKAGES
###################
library(RColorBrewer)
library(xtable)
library(gdata)
library(plyr)
library(maps)
library(mapdata)
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
today <- Sys.Date() - 31
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


###################
#READ IN UPDATED DATA
###################
# SET WD
setwd(private_today)
load("cleaned.RData")

#####
# GET COUNTS AMONG ALACHUA RESIDENTS FOR ALL SYMPTOMS (cat = symptom)
#####
sym <- alachua %>% 
  group_by(Date, cat) %>%
  summarise(visit= n())

#####
# SUBSET SYM INTO APPROPRIATELY NAMED DATAFRAMES FOR EACH CAT
#####
ili <- sym[which(sym$cat=="ili"),]
gi <- sym[which(sym$cat=="gi"),]
neuro <- sym[which(sym$cat=="neuro"),]  
rash <- sym[which(sym$cat=="rash"),]
resp <- sym[which(sym$cat=="resp"),]

#####
# CREATE ONE DATAFRAME WITH ED COUNTS FOR ALL DATES, ZIP CODES AND SYMPTOMS
#####
zip_df <- alachua %>% 
  group_by(Date, Zipcode, cat) %>%
  summarise(visits=n())

# ADD WEEK NUMBER, WRITTEN DATE, AND DAY OF WEEK COLUMNS TO zip_df 
#for written dat
zip_df$written_date <- format(zip_df$Date, format = "%B %d, %Y")
#for week number
zip_df$week_number <- format(zip_df$Date, format = "%U")
#for day of week
zip_df$dow <- format(zip_df$Date, format = "%a")
#zip_df$dow <- format(zip_df$Date, format = "%A") # Capital A would give entire day of week

# ADD A day_num COLUMN WHICH WILL BE DAYS SINCE 2012-01-01
# define start_date, which is the date we'll be subtracting from our other dates
start_date <- as.Date("2012-01-01", format = "%Y-%m-%d")

# create the day_num column, which is simply date minus
zip_df$day_num <- as.numeric(zip_df$Date - start_date)


# ADD A "SEASON" COLUMN (winter/spring/summer/fall)
# FIRST, CREATE A DOY (day of year) COLUMN
zip_df$DOY <- format(zip_df$Date, format = "%j")

zip_df$DOY <- as.numeric(zip_df$DOY)

zip_df$season <- factor(ifelse(zip_df$DOY >= 1 & zip_df$DOY <= 81, "winter",
                    ifelse(zip_df$DOY >= 82 & zip_df$DOY <= 172, "spring",
                      ifelse(zip_df$DOY >= 173 & zip_df$DOY <= 264, "summmer",
                         ifelse(zip_df$DOY >= 265 & zip_df$DOY <= 355, "fall", "winter")))))

# Create a factor version of zip code
zip_df$Zipcode_fac <- as.factor(zip_df$Zipcode)

#####
# Convert zip_df back to regular dataframe (not dplyr's local data frame object)
#####
zip_df <- data.frame(zip_df)

######
# WRITE A REGRESSION MODEL WHICH PREDICTS
# visits AS A FUNCTION OF cat, day_num, day and Zipcode
######
#add weather as a variable 

library(weatherData)

# Get weather for just one day
getWeatherForDate("GNV", "2014-04-14")

# Get weather for a period of time

x <- getSummarizedWeather("GNV", start_date = as.Date("2012-01-01", format = "%Y-%m-%d"),
                          end_date = as.Date("2015-01-23", format = "%Y-%m-%d"),
                          opt_custom_columns = TRUE,
                          custom_columns = c(2,4,20))

y <- getSummarizedWeather("GNV", start_date = as.Date("2013-02-02", format = "%Y-%m-%d"),
                          end_date = as.Date("2015-01-23", format = "%Y-%m-%d"),
                          opt_custom_columns = TRUE,
                          custom_columns = c(2,4,20))

z <- getSummarizedWeather("GNV", start_date = as.Date("2014-03-07", format = "%Y-%m-%d"),
                          end_date = as.Date("2015-01-23", format = "%Y-%m-%d"),
                          opt_custom_columns = TRUE,
                          custom_columns = c(2,4,20))

weather <- rbind(x, y, z)

#assign number to dates so i can drop the most recent dates as df only goes until the 
#23rd of jan. no need to do this  when i update the code. 
start_date <- as.Date("2012-01-01", format = "%Y-%m-%d")

# create the day_num column, which is simply date minus
weather$Date <- as.Date(weather$Date)
weather$day_num <- as.numeric(weather$Date - start_date)

#drop dates passed the 23rd of jan

weather[which(weather$Date==today),]

#nevermind it only went up to the 23rd 
#merge weather and zip_df 

df_final <- left_join(x = zip_df, 
                y = weather)

#creat column for city, county using zipcodes
zip_df$county <- factor(ifelse(zip_df$Zipcode_fac == "32601", "Gainesville, AC",
                               ifelse(zip_df$Zipcode_fac == "32602", "Gainesville, AC",
                                      ifelse(zip_df$Zipcode_fac == "32603", "Gainesville, AC",
ifelse(zip_df$Zipcode_fac == "32604", "Gainesville, AC",
       ifelse(zip_df$Zipcode_fac == "32605", "Gainesville, AC",  
              ifelse(zip_df$Zipcode_fac == "32606", "Gainesville, AC",
                     ifelse(zip_df$Zipcode_fac == "32607", "Gainesville, AC",
                            ifelse(zip_df$Zipcode_fac == "32608", "Gainesville, AC",
                                   ifelse(zip_df$Zipcode_fac == "32609", "Gainesville, AC",
ifelse(zip_df$Zipcode_fac == "32610", "Gainesville, AC",
       ifelse(zip_df$Zipcode_fac == "32611", "Gainesville, AC",
              ifelse(zip_df$Zipcode_fac == "32612", "Gainesville, AC",
                     ifelse(zip_df$Zipcode_fac == "32614", "Gainesville, AC",
                            ifelse(zip_df$Zipcode_fac == "32615", "Santa Fe, AC",
                                   ifelse(zip_df$Zipcode_fac == "32616", "Alachua, AC",
ifelse(zip_df$Zipcode_fac == "32618", "Archer, AC",
       ifelse(zip_df$Zipcode_fac == "32627", "Gainesville, AC",
              ifelse(zip_df$Zipcode_fac == "32631", "Earleton, AC",
                     ifelse(zip_df$Zipcode_fac == "32633", "Evinston, MC",
                            ifelse(zip_df$Zipcode_fac == "32635", "Gainesville, AC",
                                   ifelse(zip_df$Zipcode_fac == "32640", "Hawthorne, Cross Creek, PC",
ifelse(zip_df$Zipcode_fac == "32641", "Gainesville, AC",
       ifelse(zip_df$Zipcode_fac == "32643", "High Springs, AC",
              ifelse(zip_df$Zipcode_fac == "32653", "Gainesville, AC",
                     ifelse(zip_df$Zipcode_fac == "32654", "Island Grove, AC",
                            ifelse(zip_df$Zipcode_fac == "32655", "High Springs, AC",
                                   ifelse(zip_df$Zipcode_fac == "32658", "La Crosse, AC",
ifelse(zip_df$Zipcode_fac == "32662", "Lochloosa, AC",
       ifelse(zip_df$Zipcode_fac == "32667", "Michanopy, AC",
              ifelse(zip_df$Zipcode_fac == "32669", "Newberry, Tioga, Jonesville, AC", 
                     "Waldo, AC")))))))))))))))))))))))))))))))
                     
                              


# Create new data frame with all observations except yesterday
model_data <- zip_df[which(zip_df$Date != yesterday),]

#Create model called zip_fit.
zip_fit <- lm(visits ~ cat + day_num + dow + Zipcode_fac + season,
          data = model_data)

summary(zip_fit)

#Create model called zip_fit1 with city/county in place of zip code. 

zip_fit1 <- lm(visits ~ cat + day_num + dow + county + season,
              data = model_data)

summary(zip_fit1)
######
# ADD A predicted COLUMN TO zip_df
# WITH THE NUMBER OF VISITS WE WOULD HAVE PREDICTED
######
zip_df$predicted <- predict(zip_fit, 
                            newdata = zip_df)
summary(zip_df$predicted)
head(zip_df, n = 10)

######
# CALCULATE CONFIDENCE/PREDICTION INTERVALS FOR OUR PREDICITION
######
# confidence_intervals
confidence_intervals <- data.frame(predict(object = zip_fit,
                                           interval = "confidence",
                                           newdata = zip_df,
                                           level = 0.95))
# prediction_intervals
prediction_intervals <- data.frame(predict(object = zip_fit,
                                           interval = "prediction",
                                           newdata = zip_df,
                                           level = 0.95))

######
# USING prediction_intervals, MAKE A lwr AND upr
# COLUMN IN zip_df
######
zip_df$lwr <- prediction_intervals$lwr
zip_df$upr <- prediction_intervals$upr


######
# MAKE A COLUMN IN zip_df CALLED "ALERT"
# THIS SHOULD BE A BOOLEAN
# TRUE IF visits > upr, OTHERWISE IT'S FALSE
######

zip_df$alert <- ifelse(zip_df$visits > zip_df$upr, TRUE, FALSE)

#create column to indicate if visits is lower than lwr

zip_df$undr <- ifelse(zip_df$visits < zip_df$lwr, TRUE, FALSE)

# Visualize how our model is doing
# by plotting observed vs. predicted
# and coloring by alert status
my_colors <- adjustcolor(ifelse(zip_df$alert, "darkred", "black"), alpha.f = 0.2)
plot(x = zip_df$predicted,
     y = zip_df$visits,
     col = my_colors,
     pch = 16)

# with visits under our lower bound instead of alert

my_colors <- adjustcolor(ifelse(zip_df$undr, "darkred", "black"), alpha.f = 0.2)
plot(x = zip_df$predicted,
     y = zip_df$visits,
     col = my_colors,
     pch = 16)

#together- both underestimates- all visits outside of out CI. 

my_colors <- adjustcolor(ifelse(zip_df$undr, "darkblue",
                                ifelse(zip_df$alert, "darkred", "black")), alpha.f = 0.2)
plot(x = zip_df$predicted,
     y = zip_df$visits,
     col = my_colors,
     pch = 16)

#We would want a linear relationship right? and ideally a model that does not over or under
#predict too much. It seems like our model is bad when there are high number of visits, it 
#underpredicts. 

#For starters, maybe we can restric zip_df$predicted to be positive, since it 
#doesn't make sense to have negative visits.

zip_df$predicted[zip_df$predicted < 0] <- 0


my_colors <- adjustcolor(ifelse(zip_df$undr, "darkblue",
                                ifelse(zip_df$alert, "darkred", "black")), alpha.f = 0.2)
plot(x = zip_df$predicted,
     y = zip_df$visits,
     col = my_colors,
     pch = 16)


# Not bad, but it looks like it's pretty heavily skewed towards the large (in absolute)
# zip codes - consider using logarithmic transformation?


plot(x= log(zip_df$predicted),
     y = log(zip_df$visits),
     col = my_colors,
     pch = 16)

#taking the log of the variables gives us the NaNs warning because R can't take the log of
#a negative number (zip_df$predicted has neg visits) unless we transform our variables into complex numbers

zip_df$predicted_com <- as.complex(zip_df$predicted)

plot(x= log(zip_df$predicted_com),
     y = log(zip_df$visits),
     col = my_colors,
     pch = 16)

#this changes the graph a bit, but still gives a warning message of "imaginary parts
#discarded, but also we the log(0) is undefined so I think it's fucking up our graph a bit. 
#maybe the log is not the best solution.

######
# SUBSET zip_df TO MAKE A NEW DATAFRAME CALLED alerts
# THIS SHOULD BE ONLY THE OBSERVATIONS FROM YESTERDAY
# WHICH HAVE A < 0.05 CHANCE OF TAKING PLACE
######

alerts <- zip_df[which(zip_df$ALERTS== TRUE & zip_df$Date== yesterday)]

###############################################################################

# Time series : write function to visualize each symptom
## make a dataframe using dplyr, grouping on Date and cat
## then write a function that takes as argument category
## and plots a time series visualization of the number of cases for that category
## plot should show four things: 1) predicted number, 2) observed number ¾) 95% conf range

#create new object with 3 columns: date, cat, and visits
cat_df <- alachua %>% 
  group_by(Date, cat) %>%
  summarise(visits= n())

#make new column with day of year for cat_df

cat_df$DOY <- format(cat_df$Date, format = "%j")

#make it numeric

cat_df$DOY <- as.numeric(cat_df$DOY)

#create column for seasons
cat_df$season <- factor(ifelse(cat_df$DOY >= 1 & cat_df$DOY <= 81, "winter",
                               ifelse(cat_df$DOY >= 82 & cat_df$DOY <= 172, "spring",
                                      ifelse(cat_df$DOY >= 173 & cat_df$DOY <= 264, "summmer",
                                             ifelse(cat_df$DOY >= 265 & cat_df$DOY <= 355, "fall", "winter")))))


#create column for day of week in cat_df
cat_df$dow <- format(cat_df$Date, format = "%a")

# Create a column of days since 2012-01-01 like before

cat_df$day_num <- as.numeric(cat_df$Date - start_date)

# Convert zicat_df back to regular dataframe 

cat_df <- data.frame(cat_df)

# Now build a regression

# like before, create new data frame with all observations except yesterday
cat_data <- cat_df[which(cat_df$Date != yesterday),]

#Create model called cat_fit.
cat_fit <- lm(visits ~ cat + day_num + dow  + season,
              data = cat_data)

summary(cat_fit)

cat_df$predicted <- predict(cat_fit, 
                            newdata = cat_df)
summary(cat_df$predicted)
head(cat_df, n = 10)

##########################################################################################
# confidence_intervals
confidence_intervals_cat <- data.frame(predict(object = cat_fit,
                                           interval = "confidence",
                                           newdata = cat_df,
                                           level = 0.95))
# prediction_intervals
prediction_intervals_cat <- data.frame(predict(object = cat_fit,
                                           interval = "prediction",
                                           newdata = cat_df,
                                           level = 0.95))

######
# USING prediction_intervals, MAKE A lwr AND upr
# COLUMN IN zip_df
######
cat_df$lwr <- prediction_intervals_cat$lwr
cat_df$upr <- prediction_intervals_cat$upr


######
# MAKE A COLUMN IN zip_df CALLED "ALERT"
# THIS SHOULD BE A BOOLEAN
# TRUE IF visits > upr, OTHERWISE IT'S FALSE
######

cat_df$alert <- ifelse(cat_df$visits > cat_df$upr, TRUE, FALSE)

#create column to indicate if visits is lower than lwr

cat_df$undr <- ifelse(cat_df$visits < cat_df$lwr, TRUE, FALSE)



#making visual like before just with cat data instead
#together- both underestimates and overestimates- all visits outside of out CI. 

my_colors <- adjustcolor(ifelse(cat_df$undr, "darkblue",
                                ifelse(cat_df$alert, "darkred", "black")), alpha.f = 0.2)
plot(x = cat_df$predicted,
     y = cat_df$visits,
     col = my_colors,
     pch = 16)

#why is there this huge break in the graph?


#make negative values zero since negative visits are impossible
cat_df$predicted[cat_df$predicted < 0] <- 0

#round up and down decimal points less than 1, because log of a decimal less that 1 is negative,
#which we dont want. 
cat_df$predicted[cat_df$predicted < .5] <- 0
cat_df$predicted[cat_df$predicted > .5 & cat_df$predicted < 1] <- 1

my_colors <- adjustcolor(ifelse(cat_df$undr, "darkblue",
                                ifelse(cat_df$alert, "darkred", "black")), alpha.f = 0.2)

#with log

plot(x= log(cat_df$predicted),
     y = log(cat_df$visits),
     col = my_colors,
     pch = 16)


axes_sc <-pretty(c(0,6))
plot(x= log(cat_df$predicted_com),
     y = log(cat_df$visits),
     col = my_colors,
     pch = 16,
     xlim=c(0,max(axes_sc)),
     ylim=c(0,max(axes_sc)),
     yaxt="n",
     axes=F
)

axis(1,at=axes_sc)
axis(2,at=axes_sc,las=1)
lines(1,1)
abline(lm(cat_df$predicted ~ cat_df$visits))


#having issues. i will continue to work on this because i think our prediction with
#out the zipcodes is a better one, but i am having trouble getting rid of the zero
#in the prediction model. 

#what should I work on next?
################################ FOR NOW, IGNORE EVERYTHING BELOW THIS LINE

# ###################
# #READ IN GIS COORDINATES FOR ZIP CODES
# ###################
# setwd(public_gis)
# joelatlong <- read.csv("joelatlong.csv", header=TRUE, sep=",") #This is the GIS data for zip code
# setwd(private_today)
# 
# ###################
# #ASSIGN BL TO sym_old AND ALLESS_old
# ###################
# sym_old$bl <- grepl(bl.range, sym_old$Date) 
# alless_old$bl <- grepl(bl.range, alless_old$Date)
# 
# ###################
# #MAP SET UP
# ###################
# setwd(paste0(private, "/gis/alachuazipcodes"))
# zip.map <- readShapePoly("ACDPS_zipcode.shp")
# zip.map$Zipcode <- zip.map$ZIP
# labelpos <- data.frame(do.call(rbind, lapply(zip.map@polygons, function(x) x@labpt)))
# names(labelpos) <- c("x","y")                        
# zip.map@data <- data.frame(zip.map@data, labelpos)
# zip.map$labelpos <- labelpos
# zip.map$labelposx <- labelpos$x
# zip.map$labelposy <- labelpos$y
# zippy <- unique(sort(zip.map$ZIP))
# zippy <- as.numeric(as.character(zippy))
# zip.map$text <- 1
# for (i in zippy){zip.map$text[which(zip.map$ZIP == i)] <- i }
# 
# ###################
# #CREATE THE ZIP DATAFRAME FOR DATA BY ZIPCODE
# ###################
# zip <- as.data.frame(zip.map$Zipcode)
# colnames(zip) <- "Zipcode"
# 
# ###################
# # ADD COLUMN NAMES TO ZIP
# ###################
# for (i in tolower(symnames)){
#   for (j in c("", "week", "bl", "per")){
#     zip[,paste0(i,j)] <- NA
#   }
# }
# 
# ###################
# # POPULATE WITH VALUES
# ###################
# #YESTERDAY
# for (i in zip$Zipcode){
#   for (j in tolower(symnames)){    
#     zip[which(zip$Zipcode == i),grepl(j, colnames(zip))==TRUE &
#           grepl("week", colnames(zip)) == FALSE &
#           grepl("bl", colnames(zip)) == FALSE &
#           grepl("per", colnames(zip)) == FALSE] <-
#       nrow(sym[which(sym$cat == j &
#                        sym$Zipcode == i &
#                        sym$Date == yesterday),])}}
# 
# #WEEK
# for (i in zip$Zipcode){
#   for (j in tolower(symnames)){    
#     zip[which(zip$Zipcode == i),grepl(j, colnames(zip))==TRUE &
#           grepl("week", colnames(zip)) == TRUE &
#           grepl("bl", colnames(zip)) == FALSE  &
#           grepl("per", colnames(zip)) == FALSE] <-
#       nrow(sym[which(sym$cat == j &
#                        sym$Zipcode == i &
#                        sym$Date <= yesterday &
#                        sym$Date >= yesterday-6),])}}
# 
# 
# #BL
# for (i in zip$Zipcode){
#   for (j in tolower(symnames)){    
#     zip[which(zip$Zipcode == i),grepl(j, colnames(zip))==TRUE &
#           grepl("week", colnames(zip)) == FALSE &
#           grepl("bl", colnames(zip)) == TRUE  &
#           grepl("per", colnames(zip)) == FALSE] <-
#       nrow(sym_old[which(sym_old$cat == j &
#                            sym_old$Zipcode == i &
#                            sym_old$bl == TRUE),])/length(strsplit(bl.range, "[|]")[[1]])}}
# 
# #PER
# for (j in tolower(symnames)){
#   zip[,grepl(j, colnames(zip))==TRUE &
#         grepl("week", colnames(zip)) == FALSE &
#         grepl("bl", colnames(zip)) == FALSE  &
#         grepl("per", colnames(zip)) == TRUE] <- 
#     (zip[,grepl(j, colnames(zip))==TRUE &
#            grepl("week", colnames(zip)) == FALSE &
#            grepl("bl", colnames(zip)) == FALSE  &
#            grepl("per", colnames(zip)) == FALSE] + 0.0001) /
#     (zip[,grepl(j, colnames(zip))==TRUE &
#            grepl("week", colnames(zip)) == FALSE &
#            grepl("bl", colnames(zip)) == TRUE  &
#            grepl("per", colnames(zip)) == FALSE] + 0.0001)
# }
# 
# ###################
# #SUBSET FOR JUST YESTERDAY'S CASES
# ###################
# giyest <- sym[which(sym$Date == yesterday &
#                       sym$cat == "gi"),]
# giyest <- giyest[order(giyest$Zipcode),]
# 
# iliyest <- sym[which(sym$Date == yesterday &
#                        sym$cat == "ili"),]
# iliyest <- iliyest[order(iliyest$Zipcode),]
# 
# neuroyest <- sym[which(sym$Date == yesterday &
#                          sym$cat == "neuro"),]
# neuroyest <- neuroyest[order(neuroyest$Zipcode),]
# 
# rashyest <- sym[which(sym$Date == yesterday &
#                         sym$cat == "rash"),]
# rashyest <- rashyest[order(rashyest$Zipcode),]
# 
# respyest <- sym[which(sym$Date == yesterday &
#                         sym$cat == "resp"),]
# respyest <- respyest[order(respyest$Zipcode),]
# 
# ###################
# #HEAT DF
# ###################
# heat <- as.data.frame(unique(sort(alless_old$Date)))
# colnames(heat) <- "Date"
# 
# #Create columns for each symname
# for (i in tolower(symnames)){
#   heat[,i] <- NA}
# 
# #Populate columns with daily cases
# for (i in tolower(symnames)){
#   for (j in heat$Date){
#     heat[which(heat$Date == j),
#          colnames(heat[which(grepl(i, colnames(heat)) == TRUE)])] <-
#       nrow(sym_old[which(sym_old$cat == i &
#                            sym_old$Date == j),])
#   }
# }
# heat <- heat[which(heat$Date < today),]
# 
# ###################
# #CREATE DIST DF FOR DISTRIBUTION OF BASELINE OBS (HISTOGRAMS)
# ###################
# dist <- heat[which(grepl(bl.range, heat$Date)==TRUE),]
# 
# ############################################################
# ####################
# 
# ###################
# # HEAT CALENDAR FUNCTION
# ###################
# #MATH
# #CREATE THE CALENDAR HEAT FUNCTION (NOT MINE) ##############
# calendarHeat <- function(dates, 
#                          values, 
#                          ncolors=99, 
#                          color="g2r", 
#                          varname="Values",
#                          date.form = "%Y-%m-%d", ...) {
#   require(lattice)
#   require(grid)
#   require(chron)
#   if (class(dates) == "character" | class(dates) == "factor" ) {
#     dates <- strptime(dates, date.form)
#   }
#   caldat <- data.frame(value = values, dates = dates)
#   min.date <- as.Date(paste(format(min(dates), "%Y"),
#                             "-1-1",sep = ""))
#   max.date <- as.Date(paste(format(max(dates), "%Y"),
#                             "-12-31", sep = ""))
#   dates.f <- data.frame(date.seq = seq(min.date, max.date, by="days"))
#   
#   # Merge moves data by one day, avoid
#   caldat <- data.frame(date.seq = seq(min.date, max.date, by="days"), value = NA)
#   dates <- as.Date(dates) 
#   caldat$value[match(dates, caldat$date.seq)] <- values
#   
#   caldat$dotw <- as.numeric(format(caldat$date.seq, "%w"))
#   caldat$woty <- as.numeric(format(caldat$date.seq, "%U")) + 1
#   caldat$yr <- as.factor(format(caldat$date.seq, "%Y"))
#   caldat$month <- as.numeric(format(caldat$date.seq, "%m"))
#   yrs <- as.character(unique(caldat$yr))
#   d.loc <- as.numeric()                        
#   for (m in min(yrs):max(yrs)) {
#     d.subset <- which(caldat$yr == m)  
#     sub.seq <- seq(1,length(d.subset))
#     d.loc <- c(d.loc, sub.seq)
#   }  
#   caldat <- cbind(caldat, seq=d.loc)
#   
#   #color styles
#   r2b <- c("#0571B0", "#92C5DE", "#F7F7F7", "#F4A582", "#CA0020") #red to blue                                                                               
#   r2g <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384")   #red to green
#   g2r <- rev(r2g)
#   w2b <- c("#045A8D", "#2B8CBE", "#74A9CF", "#BDC9E1", "#F1EEF6")   #white to blue
#   w2b <- rev(w2b)
#   
#   assign("col.sty", get(color))
#   calendar.pal <- colorRampPalette((col.sty), space = "Lab")
#   def.theme <- lattice.getOption("default.theme")
#   cal.theme <-
#     function() {  
#       theme <-
#         list(
#           strip.background = list(col = "transparent"),
#           strip.border = list(col = "transparent"),
#           axis.line = list(col="transparent"),
#           par.strip.text=list(cex=0.8))
#     }
#   lattice.options(default.theme = cal.theme)
#   yrs <- (unique(caldat$yr))
#   nyr <- length(yrs)
#   print(cal.plot <- levelplot(value~woty*dotw | yr, data=caldat,
#                               as.table=TRUE,
#                               aspect=.12,
#                               layout = c(1, nyr%%7),
#                               between = list(x=0, y=c(1,1)),
#                               strip=TRUE,
#                               main = paste("Calendar Heat Map of ", varname, sep = ""),
#                               scales = list(
#                                 x = list(
#                                   at= c(seq(2.9, 52, by=4.42)),
#                                   labels = month.abb,
#                                   alternating = c(1, rep(0, (nyr-1))),
#                                   tck=0,
#                                   cex = 0.7),
#                                 y=list(
#                                   at = c(0, 1, 2, 3, 4, 5, 6),
#                                   labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
#                                              "Friday", "Saturday"),
#                                   alternating = 1,
#                                   cex = 0.6,
#                                   tck=0)),
#                               xlim =c(0.4, 54.6),
#                               ylim=c(6.6,-0.6),
#                               cuts= ncolors - 1,
#                               col.regions = (calendar.pal(ncolors)),
#                               xlab="" ,
#                               ylab="",
#                               colorkey= list(col = calendar.pal(ncolors), width = 0.6, height = 0.5),
#                               subscripts=TRUE
#   ) )
#   panel.locs <- trellis.currentLayout()
#   for (row in 1:nrow(panel.locs)) {
#     for (column in 1:ncol(panel.locs))  {
#       if (panel.locs[row, column] > 0)
#       {
#         trellis.focus("panel", row = row, column = column,
#                       highlight = FALSE)
#         xyetc <- trellis.panelArgs()
#         subs <- caldat[xyetc$subscripts,]
#         dates.fsubs <- caldat[caldat$yr == unique(subs$yr),]
#         y.start <- dates.fsubs$dotw[1]
#         y.end   <- dates.fsubs$dotw[nrow(dates.fsubs)]
#         dates.len <- nrow(dates.fsubs)
#         adj.start <- dates.fsubs$woty[1]
#         
#         for (k in 0:6) {
#           if (k < y.start) {
#             x.start <- adj.start + 0.5
#           } else {
#             x.start <- adj.start - 0.5
#           }
#           if (k > y.end) {
#             x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] - 0.5
#           } else {
#             x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] + 0.5
#           }
#           grid.lines(x = c(x.start, x.finis), y = c(k -0.5, k - 0.5), 
#                      default.units = "native", gp=gpar(col = "grey", lwd = 1))
#         }
#         if (adj.start <  2) {
#           grid.lines(x = c( 0.5,  0.5), y = c(6.5, y.start-0.5), 
#                      default.units = "native", gp=gpar(col = "grey", lwd = 1))
#           grid.lines(x = c(1.5, 1.5), y = c(6.5, -0.5), default.units = "native",
#                      gp=gpar(col = "grey", lwd = 1))
#           grid.lines(x = c(x.finis, x.finis), 
#                      y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
#                      gp=gpar(col = "grey", lwd = 1))
#           if (dates.fsubs$dotw[dates.len] != 6) {
#             grid.lines(x = c(x.finis + 1, x.finis + 1), 
#                        y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
#                        gp=gpar(col = "grey", lwd = 1))
#           }
#           grid.lines(x = c(x.finis, x.finis), 
#                      y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
#                      gp=gpar(col = "grey", lwd = 1))
#         }
#         for (n in 1:51) {
#           grid.lines(x = c(n + 1.5, n + 1.5), 
#                      y = c(-0.5, 6.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
#         }
#         x.start <- adj.start - 0.5
#         
#         if (y.start > 0) {
#           grid.lines(x = c(x.start, x.start + 1),
#                      y = c(y.start - 0.5, y.start -  0.5), default.units = "native",
#                      gp=gpar(col = "black", lwd = 1.75))
#           grid.lines(x = c(x.start + 1, x.start + 1),
#                      y = c(y.start - 0.5 , -0.5), default.units = "native",
#                      gp=gpar(col = "black", lwd = 1.75))
#           grid.lines(x = c(x.start, x.start),
#                      y = c(y.start - 0.5, 6.5), default.units = "native",
#                      gp=gpar(col = "black", lwd = 1.75))
#           if (y.end < 6  ) {
#             grid.lines(x = c(x.start + 1, x.finis + 1),
#                        y = c(-0.5, -0.5), default.units = "native",
#                        gp=gpar(col = "black", lwd = 1.75))
#             grid.lines(x = c(x.start, x.finis),
#                        y = c(6.5, 6.5), default.units = "native",
#                        gp=gpar(col = "black", lwd = 1.75))
#           } else {
#             grid.lines(x = c(x.start + 1, x.finis),
#                        y = c(-0.5, -0.5), default.units = "native",
#                        gp=gpar(col = "black", lwd = 1.75))
#             grid.lines(x = c(x.start, x.finis),
#                        y = c(6.5, 6.5), default.units = "native",
#                        gp=gpar(col = "black", lwd = 1.75))
#           }
#         } else {
#           grid.lines(x = c(x.start, x.start),
#                      y = c( - 0.5, 6.5), default.units = "native",
#                      gp=gpar(col = "black", lwd = 1.75))
#         }
#         
#         if (y.start == 0 ) {
#           if (y.end < 6  ) {
#             grid.lines(x = c(x.start, x.finis + 1),
#                        y = c(-0.5, -0.5), default.units = "native",
#                        gp=gpar(col = "black", lwd = 1.75))
#             grid.lines(x = c(x.start, x.finis),
#                        y = c(6.5, 6.5), default.units = "native",
#                        gp=gpar(col = "black", lwd = 1.75))
#           } else {
#             grid.lines(x = c(x.start + 1, x.finis),
#                        y = c(-0.5, -0.5), default.units = "native",
#                        gp=gpar(col = "black", lwd = 1.75))
#             grid.lines(x = c(x.start, x.finis),
#                        y = c(6.5, 6.5), default.units = "native",
#                        gp=gpar(col = "black", lwd = 1.75))
#           }
#         }
#         for (j in 1:12)  {
#           last.month <- max(dates.fsubs$seq[dates.fsubs$month == j])
#           x.last.m <- dates.fsubs$woty[last.month] + 0.5
#           y.last.m <- dates.fsubs$dotw[last.month] + 0.5
#           grid.lines(x = c(x.last.m, x.last.m), y = c(-0.5, y.last.m),
#                      default.units = "native", gp=gpar(col = "black", lwd = 1.75))
#           if ((y.last.m) < 6) {
#             grid.lines(x = c(x.last.m, x.last.m - 1), y = c(y.last.m, y.last.m),
#                        default.units = "native", gp=gpar(col = "black", lwd = 1.75))
#             grid.lines(x = c(x.last.m - 1, x.last.m - 1), y = c(y.last.m, 6.5),
#                        default.units = "native", gp=gpar(col = "black", lwd = 1.75))
#           } else {
#             grid.lines(x = c(x.last.m, x.last.m), y = c(- 0.5, 6.5),
#                        default.units = "native", gp=gpar(col = "black", lwd = 1.75))
#           }
#         }
#       }
#     }
#     trellis.unfocus()
#   } 
#   lattice.options(default.theme = def.theme)
# }
# 
# ###################
# # BASELINES AND MATH
# ###################
# blgi <- sum(dist$gi) / length(dist$gi)
# blili <- sum(dist$ili) / length(dist$ili)
# blneuro <- sum(dist$neuro) / length(dist$neuro)
# blrash <- sum(dist$rash) / length(dist$rash)
# blresp <- sum(dist$resp) / length(dist$resp)
# 
# 
# #CREATE 5% AND 95% RANGES
# blgiq <- quantile(dist$gi, c(.05, .95), na.rm=T) 
# bliliq <- quantile(dist$ili, c(.05, .95), na.rm=T) 
# blneuroq <- quantile(dist$neuro, c(.05, .95), na.rm=T) 
# blrashq <- quantile(dist$rash, c(.05, .95), na.rm=T) 
# blrespq <- quantile(dist$resp, c(.05, .95), na.rm=T) 
# 
# ###################
# #GEOGRAPHICAL FLAG
# ###################
# geoCluster <- as.data.frame(zip$Zipcode)
# colnames(geoCluster) <- "Zipcode"
# 
# for (i in tolower(symnames)){
#   geoCluster[,i] <- ""
# }
# 
# 
# #I SHOULD BE USING REGRESSION!!!!
# #for (i in zip$Zipcode){
# #  for (j in tolower(symnames)){
# #    heat[,paste0(j,i)] <- NA  }}
# 
# #for (i in zip$Zipcode){
# #for (j in tolower(symnames)){
# #    for (k in heat$Date){
# #      heat[which(heat$Date == k),
# #           paste0(j,i)] <-
# #        nrow(sym_old[which(sym_old$cat == j &
# #                            sym_old$Zipcode == i &
# #                            sym_old$Date == k),])}}}
# 
# #save.image("E:/fdoh/private/blablabla.RDATA")
# 
# #I want to flag any zipcode with a symptom incidence of 
# #greater than 4 cases (at 125% level)
# #or greater than 2 cases (at 150% level)
# #or greater than 1 case (at 200% level)
# for (i in tolower(symnames)){
#   for (j in geoCluster$Zipcode){
#     geoCluster[which(geoCluster$Zipcode == j), i] <-
#       ifelse(zip[which(zip$Zipcode == j),paste0(i, "per")] > 1.25 &
#                zip[which(zip$Zipcode == j), i] > 4,
#              "CHECK",
#              ifelse(zip[which(zip$Zipcode == j),paste0(i, "per")] > 1.5 &
#                       zip[which(zip$Zipcode == j), i] > 2,
#                     "CHECK",
#                     ifelse(zip[which(zip$Zipcode == j),paste0(i, "per")] > 2 &
#                              zip[which(zip$Zipcode == j), i] > 1,
#                            "CHECK",
#                            "")))
#   }
# }
# 
# 
# 
# ###################
# #RECORD OF INTEREST TABLES
# ###################
# roc <- roi[c("Date", "Age", "MedRecNo", "Sex", "CCDD", "Region", "Hospital")]
# roc$Date <- as.character(roc$Date)
# 
# ###################
# #MAP PARAMETERS
# ###################
# #joewatercolor <- openmap(c(29.96, -82.7), c(29.35,-81.9),
# #                         type="stamen-watercolor")
# #joemapwatercolor <- openproj(joewatercolor, projection = "+proj=longlat")
# 
# ###################
# #WORD CLOUD
# ###################
# remove <- "[|]|[(]|[])]|/|;|:|[(*]|&|-[)]|[(]|[-]|[--])"
# myWords <- unlist(strsplit(as.character(gsub(remove,"",
#                                              toupper(alless$CCDD[which(alless$Date == yesterday)]))), " "))
# 
# myWordsBL <- unlist(strsplit(as.character(gsub(remove,"",
#                                                toupper(alless_old$CCDD[which(alless_old$bl == TRUE)]))), " "))
# 
# 
# 
# myWordsDF <- as.data.frame(table(myWords))
# colnames(myWordsDF) <- c("word","count")
# myWordsBLDF <- as.data.frame(table(myWordsBL))
# colnames(myWordsBLDF) <- c("word","count")
# combinedDF <- rbind(myWordsDF, myWordsBLDF)
# 
# myWordsTable <- table(myWords)
# myWordsBLTable <- table(myWordsBL)
# 
# 
# 
# combinedWords <- c(myWords, myWordsBL)
# 
# myCloud <- as.data.frame(as.character(unique(sort(combinedWords))))
# colnames(myCloud) <- "word"
# 
# 
# myCloud$recent <- 0
# for (i in myCloud$word){
#   myCloud$recent[which(myCloud$word == i)] <-
#     sum(myWordsDF$count[which(myWordsDF$word == i)])} 
# 
# myCloud$bl <- 0
# for (i in myCloud$word){
#   myCloud$bl[which(myCloud$word == i)] <-
#     sum(myWordsBLDF$count[which(myWordsBLDF$word == i)])/
#     length(unique(sort(alless_old$Date[which(alless_old$bl == TRUE)])))}
# 
# myCloud$prop <- 1+ ((myCloud$recent+0.9) / (myCloud$bl+0.9)) 
# myCloud$color <- ifelse(myCloud$recent <1,
#                         "grey",
#                         "black")
# 
# myCloud <- myCloud[which(myCloud$recent >0),]
# myCloud <- myCloud[order(myCloud$prop),]
# myCloud$word <- as.character(myCloud$word)
# myCloud <- myCloud[which(nchar(myCloud$word) > 1),]
# myCloud$recentCubicRoot <- myCloud$recent^(1/3)
# 
# myCloud$colorCat<- cut(myCloud$recentCubicRoot, 9, labels=FALSE)
# cloudColors <- colorRampPalette(brewer.pal(9, "Blues"))(11)
# cloudColors <- rev(cloudColors[1:11])
# myCloud$color <- cloudColors[myCloud$colorCat]
# myCloud <- myCloud[which(regexpr(paste0("AND|CANT|TODAY|ENCOUNTER|WITHOUT|",
#                                         "NOT|OF|LIKE|THERAPY|UNSPECIFIED|OR|IN",
#                                         "THIS|INITIAL|AT"), myCloud$word) <1),]
# 
# wordcloud(words=myCloud$word,
#           freq=myCloud$prop^3,
#           scale=c(2,0.00001),
#           max.words=Inf,
#           random.order=FALSE,
#           rot.per=0,
#           colors=myCloud$color[order(myCloud$prop^3)],
#           ordered.colors=FALSE)
# 
# ###################
# #FLAG TABLE
# ###################
# 
# #FUNCTION TO CONVERT TEXT TO REGEXPR SEARCH TERM
# regexFun <- function(x){
#   paste(substr(x,1,5),"*","|",
#         tolower(substr(x,1,5)),"*|",
#         capwords(tolower(substr(x, 1,5))),"*",sep="", collapse=NULL)}
# 
# 
# wordsOfInterest <- as.data.frame(myCloud[which(ifelse(myCloud$bl <=0, 
#                                                       myCloud$recent >= 3, 
#                                                       myCloud$recent > 3*(myCloud$bl)) & 
#                                                  myCloud$recent >= 3),])
# wordsOfInterest <- wordsOfInterest[order(wordsOfInterest$recent,
#                                          decreasing=TRUE),]
# 
# flag <- as.data.frame(cbind(wordsOfInterest$word,
#                             wordsOfInterest$recent,
#                             round(wordsOfInterest$bl, digits=2)))
# colnames(flag) <- c("Word", "Yesterday", "Baseline")
# flag$Baseline <- as.numeric(as.character(flag$Baseline))
# 
# #CAPITALIZE FIRST LETTER FUNCTION
# capwords <- function(s, strict = FALSE) {
#   cap <- function(s) paste(toupper(substring(s, 1, 1)),
# {s <- substring(s, 2); if(strict) tolower(s) else s},
# sep = "", collapse = " " )
# sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))}
# 
# 
# #CREATE SEARCH TERMS
# heat30 <- heat[which(heat$Date > max(heat$Date)-30),]
# 
# flagSearch <- as.data.frame(matrix(rep(NA, length(flag$Word)*30), nrow=30))
# colnames(flagSearch) <-flag$Word
# flagSearch <- cbind(heat30$Date, flagSearch)
# colnames(flagSearch)[1] <- "Date"
# 
# newColStart <- length(colnames(heat30))+1
# newColEnd <- length(colnames(heat30))+length(flagSearch$Word)
# #colnames(heat30)[newColStart:newColEnd] <-  flagSearch$Word
# 
# 
# for(i in flagSearch$Date){
#   for (j in colnames(flagSearch[2:(length(flag$Word)+1)])){
#     flagSearch[which(flagSearch$Date == i),j] <-
#       nrow(alless_old[which(alless_old$Date == i &
#                               regexpr(regexFun(j), alless_old$CCDD)>=0),])}}
# par(mar=c(2,2,2,1))
# par(mfrow=c(ceiling(length(flag$Word)/3),3))
# for (i in colnames(flagSearch[2:(length(flag$Word)+1)])){
#   plot(flagSearch$Date, flagSearch[,i], main=i, xlab="Date", ylab="Cases", lty=6,
#        lwd=1, col=adjustcolor("red", alpha.f=0.6), type="l", xaxt="n")
#   points(flagSearch$Date, flagSearch[,i], pch=16, cex=1, col=adjustcolor("black", alpha.f=0.4))
#   axis(side=1, at=flagSearch$Date, labels=format(flagSearch$Date, format="%d %b"), las=3, cex.axis=0.4)
#   abline(h=flag$Baseline[which(flag$Word == i)], lwd=3, col=adjustcolor("blue", alpha.f=0.2))
#   legend(x="topleft", bty="n", border=FALSE, cex=0.6, lwd=3, col=adjustcolor("blue", alpha.f=0.2),
#          legend="2012 seasonal baseline")}
# par(mfrow=c(1,1))
# 
# ###################
# #ILI YEAR TO YEAR COMPARISON
# ###################
# heat$j <- as.numeric(format(heat$Date, format="%j"))
# 
# #2013
# #ili13= spm(heat$ili[which(heat$Date >= "2013-01-01")]~
# #             f(heat$j[which(heat$Date >= "2013-01-01")], spar=20), omit.missing=TRUE)
# #2012
# #ili12= spm(heat$ili[which(heat$Date < "2013-01-01" &
# #                            heat$Date >= "2012-01-01")]~
# #             f(heat$j[which(heat$Date < "2013-01-01" &
# #                              heat$Date >= "2012-01-01")], spar=20), omit.missing=TRUE)
# 
# #plot(heat$j, heat$ili, type="n", main="ILI: 2012 vs. 2013",
# #     xlab="Date", ylab="Daily cases", xlim=c(250,365))
# #points(heat$j[which(heat$Date < "2013-01-01")],
# #       heat$ili[which(heat$Date < "2013-01-01")],
# #       pch=16, col=adjustcolor("dodgerblue3", alpha.f=0.3))
# #points(heat$j[which(heat$Date > "2013-01-01")],
# #       heat$ili[which(heat$Date > "2013-01-01")],
# #       pch=16, col=adjustcolor("darkred", alpha.f=0.3))
# #lines(ili12,shade.col=adjustcolor("dodgerblue3", alpha.f=0.3),
# #      rug.col="blue", col="blue")
# #lines(ili13,shade.col=adjustcolor("darkred", alpha.f=0.3),
# #      rug.col="darkred", col="darkred")
# 
# #legend(x="topleft", pch=16, col=c("dodgerblue2", "darkred"),
# #       legend=c("2012", "2013"), lwd=2)
# 
# ###################
# #
# ###################
# # jj <- map("county", "florida")
# # plot(jj, type="l")
# 
# ###################
# # FUNCTION TIME!!!!
# ###################
# 
# #### MapCases
# MapCases <- function(variable, color){
#   plotvar <- variable
#   nclr <- max(plotvar+1, na.rm=TRUE) # number of bins
#   plotclr <- c("white", brewer.pal(nclr, color))
#   cuts <- cut(plotvar, breaks=nclr,labels=FALSE)
#   mapCols <- plotclr[cuts]
#   plot(zip.map, border="grey", col=mapCols, main="Cases yesterday")
#   legend("bottomleft", # position
#          legend = seq(0,max(plotvar, na.rm=TRUE), 1), 
#          title = "Cases",
#          fill = plotclr,
#          cex = 1 - (0.03*max(plotvar, na.rm=TRUE)),
#          bty = "n",
#          border="grey") # border
# }
# 
# #### MapCasesWeek
# MapCasesWeek <- function(variable, color){
#   plotvar<-variable
#   nclr<- 5 # number of bins (3-8)
#   min<- floor(min(plotvar))
#   max<- ceiling(max(plotvar))
#   breaks<- (max-min) / nclr
#   plotclr<- brewer.pal(nclr, color)
#   class<- classIntervals(plotvar, nclr, style ="fixed", fixedBreaks=seq(min, max, breaks))
#   
#   colcode<- findColours(class, plotclr)
#   colcode2<-gsub(",","-", gsub("[[]|[)]|[]]","", names(attr(colcode, "table"))))
#   
#   plot(zip.map, border="grey", col=colcode, main="Cases yesterday")
#   legend("bottomleft", # position
#          legend = colcode2, 
#          title = "Cases",
#          fill = plotclr,
#          cex = 0.6,
#          bty = "n",
#          border="grey") # border
# }
# 
# 
# 
# 
# #MapProp
# MapProp <- function(variable, color){
#   plotvar <- variable
#   nclr <- 5# number of bins
#   plotcat <- ifelse(variable==0, 0,
#                     ifelse(variable >0 & variable <1, 1,
#                            ifelse(variable==1, 2,
#                                   ifelse(variable>1 & variable <=2, 3,
#                                          ifelse(variable>2, 4,
#                                                 0))))) 
#   plotclr <- c("white", brewer.pal(4, color))
#   cuts <- cut(plotvar, breaks=nclr,labels=FALSE)
#   mapCols <- plotclr[cuts]
#   legText <- as.character(100*seq(0,4, 1))
#   legText <- c("0",
#                "< 100",
#                "100",
#                "101 - 200",
#                "> 200")
#   plot(zip.map, border="grey", col=mapCols, main="Yesterday relative to baseline")
#   legend("bottomleft", # position
#          legend = legText, 
#          title = "Cases\n(% of expected)",
#          fill = plotclr,
#          cex = 0.56,
#          bty = "n",
#          border="grey") # border
# }
# 
# ############ DETAILS
# par(mfrow=c(1,1))
# 
# HistFun <- function(variable, color){
#   hist(variable, breaks=15, col=adjustcolor("black", alpha.f=0.5),
#        main="Cases", cex.lab=0.6, cex.main=0.8,
#        xlab="Baseline daily cases",
#        ylab="Frequency", border=FALSE)
#   abline(v=heat$gi[which(heat$Date == yesterday)], lwd=14, lty=1,
#          col=adjustcolor(color, alpha.f=0.5))
#   legend(x="topright",
#          lty=1, lwd=3, legend="Yesterday's\nobservation", cex=0.5, 
#          col=adjustcolor(color, alpha.f=0.5),bty="n")
# }
# 
# AgeHistFun <- function(variable, baseline, color){
#   hist(baseline, main="Age", cex.main=0.8, col=adjustcolor("black", alpha.f=0.5), border=FALSE,
#        ylab="Frequency", xlab="Age", freq=FALSE, cex.lab=0.6)
#   hist(variable,
#        col=adjustcolor(color, alpha.f=0.4), ylab="frequency", xlab="age", freq=FALSE, add=TRUE,
#        border=FALSE)
#   legend(x="topright", fill=adjustcolor(c("black", color), alpha.f=0.5), bty="n", border=FALSE,
#          legend=c("2012 baseline", "Yesterday"), cex=0.6)
# }
# 
# TimeHistFun <- function(variable, baseline, color){
#   hist(baseline, freq=FALSE, border=FALSE, col=adjustcolor("black", alpha.f=0.5), 
#        main="Check-in time", xlab="Time of day", ylab="Frequency", xaxt="n", ylim=c(0,0.06), cex=0.6, cex.main=0.8,
#        cex.lab=0.6)
#   hist(variable, freq=FALSE, border=FALSE, 
#        col=adjustcolor(color, alpha.f=0.5), xaxt="n", add=TRUE, ylim=c(0,0.06))
#   axis(1, at=c(1,12,24,36,50), labels=c("midnight","6am","noon","6pm","midnight"), cex.axis=0.6)
#   legend(x="topright", fill=adjustcolor(c("black", color), alpha.f=0.5), bty="n", border=FALSE,
#          legend=c("2012 baseline", "Yesterday"), cex=0.6)
#   
# }
# 
# SexFun <- function(variable, baseline, color){
#   prop <- table(baseline)/length(baseline) 
#   barplot(prop, ylim=c(0,1), col=adjustcolor("black",alpha.f=0.5), border=FALSE)
#   prop <- table(variable)/length(variable)
#   barplot(prop, ylim=c(0,1), col=adjustcolor(color,alpha.f=0.5), add=TRUE, border=FALSE, 
#           main="Sex", cex.main=0.8, xlab=NA)
#   legend(x="topright", fill=adjustcolor(c("black", color), alpha.f=0.5), 
#          legend=c("2012 baseline", "Yesterday"), border=FALSE, bty="n", cex=0.6)
# }
# 
# 
# SexFun <- function(variable, baseline, color){
#   variable <- factor(variable, levels=c("F", "M"))
#   baseline <- factor(baseline, levels=c("F", "M"))
#   prop <- table(baseline)/length(baseline) 
#   barplot(prop, ylim=c(0,1), col=adjustcolor("black",alpha.f=0.5), border=FALSE)
#   prop <- table(variable)/length(variable)
#   barplot(prop, ylim=c(0,1), col=adjustcolor(color,alpha.f=0.5), add=TRUE, border=FALSE, 
#           main="Sex", cex.main=0.8, xlab=NA)
#   legend(x="topright", fill=adjustcolor(c("black", color), alpha.f=0.5), 
#          legend=c("2012 baseline", "Yesterday"), border=FALSE, bty="n", cex=0.6)
# }
# 
# 
# RaceFun <- function(variable, baseline, color){
#   
#   prop <- table(baseline)/length(baseline) 
#   barplot(prop, ylim=c(0,1), col=adjustcolor("black",alpha.f=0.5), border=FALSE,
#           xaxt="n")
#   prop <- table(variable)/length(variable)
#   barplot(prop, ylim=c(0,1), col=adjustcolor(color,alpha.f=0.5), add=TRUE, border=FALSE, 
#           main="Race", cex.main=0.8)
#   legend(x="topright", fill=adjustcolor(c("black", color), alpha.f=0.5), 
#          legend=c("2012 baseline", "Yesterday"), border=FALSE, bty="n", cex=0.6)
# }
# 
# TimeSeriesFun <- function(variable, color){
#   plot(heat$Date, variable, type="n", xlab="Date", ylab="Cases",
#        xlim=c(yesterday-q, yesterday), cex.axis=0.75, cex.main=0.8, cex.lab=0.6)
#   axis(side=1, at=heat$Date, labels=format(heat$Date, format="%d\n%b"), cex.axis=0.6)
#   lines(heat$Date, variable, lty=1, col="darkgrey")
#   points(heat$Date, variable, pch=1, col="black")
#   points(heat$Date, variable, pch=20, col=color)
#   mm <- c(c(heat$Date, today), c(today,rev(heat$Date)))
#   zzblq <-c(rep(min(quantile(variable[which(heat$Date <= today - 351 &
#                                               heat$Date >= today - 380)],
#                              c(.05, .95), na.rm=T)),length(heat$Date)+1), 
#             rev(rep(max(quantile(variable[which(heat$Date <= today - 351 &
#                                                   heat$Date >= today - 380)],
#                                  c(.05, .95), na.rm=T)),length(heat$Date)+1)))
#   polygon(mm, zzblq, col=adjustcolor(color, alpha.f=0.3), border=FALSE)
#   abline(h=sum(variable[which(heat$Date <= today - 351 &
#                                 heat$Date >= today - 380)])/30, 
#          col=adjustcolor(color, alpha.f=0.3), lty=1, lwd=5)
#   legend(x="topright", legend=c("Expected", "Normal variation"),
#          lty=c(1,1), col=c(color,adjustcolor(color, alpha.f=0.3)), ncol=1,
#          x.intersp=0.2, y.intersp=0.8, bor=T,cex=0.6, lwd=c(1,4), bty="n")  
# }
# 
# 
# HistFun(variable = dist$gi, 
#         color = symcols[1])
# 
# AgeHistFun(variable = giyest$Age,
#            baseline = sym_old$Age[which(sym_old$bl == TRUE &
#                                           sym_old$cat == "gi")],
#            color = symcols[1])
# 
# TimeHistFun(variable = giyest$HalfHour,
#             baseline = sym_old$HalfHour[which(sym_old$bl == TRUE &
#                                                 sym_old$cat == "gi")],
#             color = symcols[1]) 
# 
# SexFun(variable = giyest$Sex,
#        baseline = sym_old$Sex[which(sym_old$bl == TRUE &
#                                       sym_old$cat == "gi")],
#        color= symcols[1])
# 
# RaceFun(variable = giyest$race,
#         baseline = sym_old$race[which(sym_old$bl == TRUE &
#                                         sym_old$cat == "gi")],
#         color=symcols[1])
# 
# TimeSeriesFun(variable = heat$ili,
#               color=symcols[2])
# 
# 
# ###################
# #MAKE OVERALL (LAST 7 days)
# ###################
# overall <- heat[which(heat$Date > yesterday - 7),]
# 
# 
# ###################
# #WEEKLY AND DAILY BARPLOT
# ###################
# weeklysums <- c(
#   (sum(overall$gi)/blgi/7)*100,
#   (sum(overall$ili)/blili/7)*100,
#   (sum(overall$neuro)/blneuro/7)*100,
#   (sum(overall$rash)/blrash/7)*100,
#   (sum(overall$resp)/blresp/7)*100)
# weeklysums <-round(weeklysums, digits=1)
# 
# dailysums <- c(
#   (overall$gi[7]/blgi)*100,
#   (overall$ili[7]/blili)*100,
#   (overall$neuro[7]/blneuro)*100,
#   (overall$rash[7]/blrash)*100,
#   (overall$resp[7]/blresp)*100)
# dailysums <-round(dailysums, digits=1)
# 
# ###################
# # SPECIAL SEARCHES
# ###################
# rabies <- alless_old[which(regexpr("rabies*|RABIES*|Rabies*|bite*|Bite*|BITE*", alless_old$CCDD)>0 &
#                              grepl("insect|INSECT", alless_old$CCDD) == FALSE),]
# rabiesyest <- rabies[which(rabies$Date == yesterday),]
# rabies7 <- rabies[which(rabies$Date >= yesterday-6),]
# 
# ###################
# # FUNCTION FOR EXAMINING ANY TERM IN DEPTH
# ###################
# nadiaFun <- function(x){
#   y <- alless_old[which(regexpr(regexFun(x), alless_old$CCDD)>0),]
#   z <- y[which(y$Date == yesterday),]
#   View(z)}
# 
# examineFun <- function(x){
#   alless_old[which(regexpr(regexFun(x), alless_old$CCDD)>0 &
#                      alless_old$Date > yesterday - 6),]
# }
# 
# spice <- examineFun("MARIJUANA|SYNTHETIC|K2|SPICE")
# 
# ebola <- examineFun("LIBERIA|SIERRA LEONE|CONGO|GUINEA|AFRICA|EBOLA")
# 
# wnv <- alless[which(grepl(regexFun("WEST NILE|WNV|OCULAR"), alless$CCDD)>0 &
#                       grepl(regexFun("MOQUITO"), alless$CCDD)  &
#                       alless$Date > yesterday - 6),]
# 
# #nadiaFun("SCABIES")
# #nadiaFun("BITE")
# #nadiaFun("STREP")
# 
# 
# 
# 
# 
# ####################
# # MERS detection
# ####################
# # mersWords <- paste(regexFun(c("Saudi","Arab","Emirat","UAE","Qatar","Oman",
# #                         "Jordan","Kuwait","Yemen","Leban","Lebon","Middle",
# #                         "mers", "MERS", "Mers")), collapse="|")
# # mersSymptoms <- paste(regexFun(c("respir","severe","sob","shortness","breath",
# #                                 "fever","pneumo","diar","vomit","naus")),
# #                                collapse="|")
# # mers <- alless[which(grepl(mersWords, alless$CCDD) == TRUE &
# #                        grepl(mersSymptoms, alless$CCDD) == TRUE),] 
# # 
# # mers2 <- alless2[which(grepl(mersWords, alless2$CCDD) == TRUE &
# #                        grepl(mersSymptoms, alless2$CCDD) == TRUE),] 
# # 
# # mers <- rbind(mers,mers2)
# # 
# # 
# # nrow(mers)
# 
# ###################
# # SURFACE FUNCTION
# ##################
# 
# 
# library(gstat)
# library(geoR)
# library(rgdal)
# library(scatterplot3d)
# library(RColorBrewer)
# 
# # Read in boundary
# #boundary <- readOGR("E:/fdoh/private/surv/Alachua_Boundary", "Alachua_Boundary")
# 
# boundary <- zip.map # have to do this due to lack of projection system
# library(maptools)
# boundary <- unionSpatialPolygons(boundary, rep(1, length(boundary@polygons)))
# 
# # # Read in population
# # pop <- readOGR("E:/fdoh/private/surv/Alachua_CT_POP", "Alachua_CT_POP")
# 
# # Define color vector
# my_colors <- colorRampPalette(c("blue", "red"))(100)
# 
# SurfaceFun <- function(disease = "ili",
#                        boundary_shape = boundary){
#   
#   
#   
#   # getting coordinates of alachua boundary
#   boundary_points <- boundary@polygons[[1]]@Polygons
#   boundary_points <- boundary_points[[1]]@coords
#   
#   # Get trap locations and data values
#   a <- data.frame("x" = zip.map$x,
#                   "y" = zip.map$y,
#                   "z" = zip[,paste0(disease, "per")])
#   # Make into a geodata object
#   b <- as.geodata(a)
#   
#   # Predict multiple points in Alachua County's boundary
#   x <- seq(min(boundary_points[,1]), max(boundary_points[,1]), length = 100)
#   y <- seq(min(boundary_points[,2]), max(boundary_points[,2]), length = 100)
#   
#   # Make a grid of those points
#   pred.grid <- expand.grid(x,y)
#   
#   
#   # kriging calculations
#   kc <- krige.conv(geodata = b, coords = b$coords, data = b$data,
#                    locations = pred.grid,
#                    borders = boundary_points,
#                    #borders = boundary@polygons,
#                    # borders = ALACHUA BORDERS!,
#                    krige = krige.control(type.krige = "ok",
#                                          cov.pars = c(5000,10000000))) #10, 3.33 # what is this?
#   
#   
#   
#   # Plot!
#   # displaying predicted values
#   image(kc, loc = pred.grid, 
#         col = my_colors,
#         xlab=NA, ylab=NA,
#         xaxt = "n",
#         yaxt = "n",
#         xpd = NA,
#         bty = "n")
#   
#   # Define percentiles for legend
#   legtemp <-  round(quantile(kc$predict, probs = seq(0,1,, length = 10)))
#   
#   legend(x="topright",
#          fill = my_colors[c(1,11,22,33,44,55,66,77,88,100)],
#          legend = c(legtemp[1], NA, NA, legtemp[4], NA, NA, legtemp[7], NA, NA, legtemp[10]),
#          border = FALSE,
#          bty = "n",
#          ncol = 1,
#          y.intersp = 0.5,
#          title = "Interpolation",
#          cex = 0.75)
# }
# SurfaceFun("neuro")
# 
# 
# 
# ######################
# #******SAVE
# #******IMAGE
# ######################
# save.image(paste(private, "/",
#                  today,
#                  "/",
#                  "zap.Rdata", sep=""))
# 
# par(mfrow=c(1,1))
# 
# tot.time <- as.numeric(Sys.time() - start.time)
# cat(paste0("\n\n\n\n\n\n\n\n",  "Congratulations!!!", 
#            "\n", "That took about ", round(tot.time, digits=0), " minutes",
#            "\n\n",
#            "Now you can run the zap.Rnw file in order to produce a surveillance report",
#            "\n\n\n") )
# 
# # RESPIRATORY AMONG YOUTH
# library(dplyr)
# 
# sym_old$kid <- ifelse(sym_old$Age >= 18, FALSE, TRUE)
# x <- sym_old %>%
#   filter(cat == "gi", kid == TRUE) %>%
#   group_by(Date) %>%
#   summarise(count = n())
# 
# x
# plot(x$Date, x$count, xlim = c(max(x$Date) - 20, max(x$Date)),
#      xlab = "Last 20 days", ylab = "Cases",
#      type = "l", col = "darkblue")
