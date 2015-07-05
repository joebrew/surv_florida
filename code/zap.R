#reads in files and producers charts and graphs
###################
#SET DATE / TIME PARAMETERS
###################
start.time <- Sys.time()
today <- Sys.Date()  
yesterday <- today - 1

###################
#DEFINE AND SET WD
###################
print('setting working directories')
if ( Sys.info()["sysname"] == "Linux" ){
  
  private_surv <- paste0("/media/joebrew/JB/fdoh/private/surv/", today)
  private <- "/media/joebrew/JB/fdoh/private/surv"
  private_historical <- "/media/joebrew/JB/fdoh/private/surv/historical"
  public_gis <- "/media/joebrew/JB/fdoh/private/surv/gis"
  
} else {
  
  private_surv <- paste0("E:/fdoh/private/surv/", today)
  private <- "E:/fdoh/private/surv"
  private_historical <- "E:/fdoh/private/surv/historical"
  public_gis <- "E:/fdoh/private/surv/gis"
  
}

setwd(private_surv) 


###################
#SET Q (HOW FAR TO GO BACK IN TS PLOTS)
###################
q <- 15 #HOW FAR BACK DO I WANT PLOTS TO GO?

###################
#LOAD  PACKAGES
###################
print('loading packages')
library(RColorBrewer)
library(xtable)
library(gdata)
library(plyr)
library(maps)
library(mapdata)
library(maptools)
library(rJava)
library(rgdal)
library(OpenStreetMap)
library(shapefiles)
library(SemiPar)
library(wordcloud)
library(RCurl)
library(classInt)
library(data.table)
library(dplyr)
library(gstat)
library(geoR)
#library(scatterplot3d)
library(RColorBrewer)
library(maptools)
cat('packages loaded\n')

###################
#SET GRAPHICAL PARAMETERS
###################
# def.par <- par(no.readonly = TRUE)
# par(def.par)

###################
#SET COLORS FOR SYNDROMES
###################
symcols <- colorRampPalette(brewer.pal(8, "Set1"))(8)
symnames <- c("GI", "ILI","Neuro", "Rash", "Resp")

###################
#READ IN DATA FROM ESSENCE
###################
cat('reading in data\n\n')
setwd(private_surv) 

read_data <- function(x, ...){
  cat(paste0('reading in ', gsub('.txt', '', x), '\n'))
  temp <- read.table(x, sep = ',', header = TRUE, ...)
  return(temp)
}

alless <- read_data('alless.txt')
alless2 <- read_data('alless2.txt')
gi <- read_data('gi.txt')
ili <- read_data('ili.txt')
neuro <- read_data('neuro.txt')
rash <- read_data('rash.txt')
resp <- read_data('resp.txt')
roi <- read_data('roi.txt', colClasses = 'character')
roi2 <- read_data('roi2.txt', colClasses = 'character')
cat('\n\n\ndata read in\n\n')

###################
#ADD A CCDDCATEGORY COLUMN
###################
cat('cleaning data\n')
gi$cat <- "gi"
ili$cat <- "ili"
neuro$cat <- "neuro"
rash$cat <- "rash"
resp$cat <- "resp"

###################
#CLEAN AND COMBINE RECORDS OF INTEREST
###################
roi2$Sex <- as.character(roi2$Sex)
roi$Sex <- as.character(roi$Sex)

roi <- rbind(roi, roi2)
#roi$Sex[which(roi$Sex=="FALSE")] <- "F"
rm(roi2)
cat('data cleaned\n')

###################
#READ IN BASELINE FILES
###################
cat('\nreading in historical files')

setwd(private_historical)

alless1213 <- read.csv("alless1213updated.csv")
symOld <- read.csv("symOldUpdated.csv")

###################
#READ IN GIS COORDINATES FOR ZIP CODES
###################
cat('\nreading in GIS data\n')
setwd(public_gis)
joelatlong <- read.csv("joelatlong.csv", header=TRUE, sep=",") #This is the GIS data for zip code
setwd(private_surv)

###################
#COMBINE THE 5 TRACKED SYMPTOM FILES
###################
cat('\nreformatting data\n')
sym <- as.data.frame(rbind(gi, ili, neuro, rash, resp))

###################
#FORMAT ALL DATES
###################
mydatefunction <- function(x){as.Date(x, "%m/%d/%Y")}
mydatefunctionbl <- function(x){as.Date(x, "%Y-%m-%d")}

sym$Date <- mydatefunction(sym$Date)
alless$Date <- mydatefunction(alless$Date)
alless2$Date <- mydatefunction(alless2$Date)

roi$Date <- mydatefunction(roi$Date)

alless1213$Date <- mydatefunctionbl(alless1213$Date)
symOld$Date <- mydatefunctionbl(symOld$Date)

gi$Date <- mydatefunction(gi$Date)
ili$Date <- mydatefunction(ili$Date)
neuro$Date <- mydatefunction(neuro$Date)
rash$Date <- mydatefunction(rash$Date)
resp$Date <- mydatefunction(resp$Date)

###################
#ASSIGN DAYS TO BE USED AS BASELINE PERIOD
###################
bl.range <- paste(paste(seq(yesterday-380, yesterday-351, 1), collapse="|"),
                  paste(seq(yesterday-745, yesterday-716, 1), collapse="|"), sep="|")

###################
#CLEAN UP RACE COLUMN
###################
cat('\nmore cleaning (race, etc.)\n')
black <- "BLA*|AFR*|*BLA|*AFR|Bla*|Afr*|*Bla*|*Afr"
white <- "WHI*|CAUC*|*WHI|*CAUC|Whi*|Cauc*|*Whi|*Cauc"

#symOld
symOld$race <- factor(ifelse(regexpr(black, symOld$Race_flat) >0, "black",
                             ifelse(regexpr(white, symOld$Race_flat) >0, "white",
                                    "other")))

#sym
sym$race <- factor(ifelse(regexpr(black, sym$Race_flat) >0, "black",
                          ifelse(regexpr(white, sym$Race_flat) >0, "white",
                                 "other")))
#alless
alless$race <- factor(ifelse(regexpr(black, alless$Race_flat) >0, "black",
                             ifelse(regexpr(white, alless$Race_flat) >0, "white",
                                    "other")))

alless1213$race <- factor(ifelse(regexpr(black, alless1213$Race_flat) >0, "black",
                                 ifelse(regexpr(white, alless1213$Race_flat) >0, "white",
                                        "other")))

#alless2
alless2$race <- factor(ifelse(regexpr(black, alless2$Race_flat) >0, "black",
                              ifelse(regexpr(white, alless2$Race_flat) >0, "white",
                                     "other")))


###################
#COMBINE THE OLD AND NEW DATA
###################
cat('\nupdating historical database\n')
symOld$X <- NULL
alless1213$X <- NULL
#gi1213$X <- NULL
#ili1213$X <- NULL
#neuro1213$X <- NULL
#rash1213$X <- NULL
#resp1213$X <- NULL

alless1213 <- alless1213[,c("Date", "Age", "Zipcode",
                            "MedRecNo", "Sex", "CCDD",
                            "Region", "Time", "HalfHour", 
                            "Hospital", "Region.of.the.Hospital",
                            "CCDDCategory_flat", "Race_flat",
                            "Disposition.Category",
                            "HospitalName",
                            "HospitalZipCode", "race")]


alless <- alless[,c("Date", "Age", "Zipcode",
                    "MedRecNo", "Sex", "CCDD",
                    "Region", "Time", "HalfHour", 
                    "Hospital", "Region.of.the.Hospital",
                    "CCDDCategory_flat", "Race_flat",
                    "Disposition.Category",
                    "HospitalName",
                    "HospitalZipCode", "race")]


sym <- sym[,c("Date", "Age", "Zipcode",
              "MedRecNo", "Sex", "CCDD",
              "Region", "Time", "HalfHour", 
              "Hospital", "Region.of.the.Hospital",
              "CCDDCategory_flat", "Race_flat",
              "Disposition.Category",
              "HospitalName",
              "HospitalZipCode", "race", "cat")]


symOld <- symOld[,c("Date", "Age", "Zipcode",
                    "MedRecNo", "Sex", "CCDD",
                    "Region", "Time", "HalfHour", 
                    "Hospital", "Region.of.the.Hospital",
                    "CCDDCategory_flat", "Race_flat",
                    "Disposition.Category",
                    "HospitalName",
                    "HospitalZipCode", "race", "cat")]


symOld <- rbind(symOld[which(symOld$Date < min(sym$Date)),], sym)
alless1213 <- rbind(alless1213[which(alless1213$Date < min(alless$Date)),],alless)


###################
#WRITE HISTORICAL CSVs
###################
cat('\n  writing new historical csvs\n')
setwd(private_historical)
write.csv(symOld, "symOldUpdated.csv"); cat('\n  writing symOldUpdated.csv')
write.csv(alless1213, "alless1213updated.csv"); cat('\n writing alless1213updated.csv')
#write.csv(alless1213, "alless1213updated_backup_2015-06-11.csv")
cat('\nhistorical csvs written\n')


###################
#ASSIGN BL TO SYMOLD AND ALLESS1213
###################
symOld$bl <- grepl(bl.range, symOld$Date) 
alless1213$bl <- grepl(bl.range, alless1213$Date)

###################
#MAP SET UP
###################
cat('\nstarting map stuff\n')
setwd(paste0(private, "/gis/alachuazipcodes"))
zip.map <- readShapePoly("ACDPS_zipcode.shp")
zip.map$Zipcode <- zip.map$ZIP
labelpos <- data.frame(do.call(rbind, lapply(zip.map@polygons, function(x) x@labpt)))
names(labelpos) <- c("x","y")                        
zip.map@data <- data.frame(zip.map@data, labelpos)
zip.map$labelpos <- labelpos
zip.map$labelposx <- labelpos$x
zip.map$labelposy <- labelpos$y
zippy <- unique(sort(zip.map$ZIP))
zippy <- as.numeric(as.character(zippy))
zip.map$text <- 1
for (i in zippy){zip.map$text[which(zip.map$ZIP == i)] <- i }

###################
#CREATE THE ZIP DATAFRAME FOR DATA BY ZIPCODE
###################
zip <- as.data.frame(zip.map$Zipcode)
colnames(zip) <- "Zipcode"

###################
# ADD COLUMN NAMES TO ZIP
###################
for (i in tolower(symnames)){
  for (j in c("", "week", "bl", "per")){
    zip[,paste0(i,j)] <- NA
  }
}

###################
# POPULATE WITH VALUES
###################
cat('\ngetting zip code specific counts\n')
#YESTERDAY
for (i in zip$Zipcode){
  for (j in tolower(symnames)){   
    
    cat(paste0('YESTERDAY | ', 'zip code: ', i, ' <<>> ', 'syndrome: ', j, '\n'))
    
    zip[which(zip$Zipcode == i),grepl(j, colnames(zip))==TRUE &
          grepl("week", colnames(zip)) == FALSE &
          grepl("bl", colnames(zip)) == FALSE &
          grepl("per", colnames(zip)) == FALSE] <-
      nrow(sym[which(sym$cat == j &
                       sym$Zipcode == i &
                       sym$Date == yesterday),])}}

#WEEK
for (i in zip$Zipcode){
  for (j in tolower(symnames)){    
    
    cat(paste0('LAST WEEK | ', 'zip code: ', i, ' <<>> ', 'syndrome: ', j, '\n'))
    
    zip[which(zip$Zipcode == i),grepl(j, colnames(zip))==TRUE &
          grepl("week", colnames(zip)) == TRUE &
          grepl("bl", colnames(zip)) == FALSE  &
          grepl("per", colnames(zip)) == FALSE] <-
      nrow(sym[which(sym$cat == j &
                       sym$Zipcode == i &
                       sym$Date <= yesterday &
                       sym$Date >= yesterday-6),])}}


#BL
for (i in zip$Zipcode){
  for (j in tolower(symnames)){    
    
    cat(paste0('BASELINE | ', 'zip code: ', i, ' <<>> ', 'syndrome: ', j, '\n'))
    
    zip[which(zip$Zipcode == i),grepl(j, colnames(zip))==TRUE &
          grepl("week", colnames(zip)) == FALSE &
          grepl("bl", colnames(zip)) == TRUE  &
          grepl("per", colnames(zip)) == FALSE] <-
      nrow(symOld[which(symOld$cat == j &
                          symOld$Zipcode == i &
                          symOld$bl == TRUE),])/length(strsplit(bl.range, "[|]")[[1]])}}

#PER
for (j in tolower(symnames)){
  
  cat(paste0('PERCENTAGE | ',  'syndrome: ', j, '\n'))
  
  zip[,grepl(j, colnames(zip))==TRUE &
        grepl("week", colnames(zip)) == FALSE &
        grepl("bl", colnames(zip)) == FALSE  &
        grepl("per", colnames(zip)) == TRUE] <- 
    (zip[,grepl(j, colnames(zip))==TRUE &
           grepl("week", colnames(zip)) == FALSE &
           grepl("bl", colnames(zip)) == FALSE  &
           grepl("per", colnames(zip)) == FALSE] + 0.0001) /
    (zip[,grepl(j, colnames(zip))==TRUE &
           grepl("week", colnames(zip)) == FALSE &
           grepl("bl", colnames(zip)) == TRUE  &
           grepl("per", colnames(zip)) == FALSE] + 0.0001)
}

###################
#SUBSET FOR JUST YESTERDAY'S CASES
###################
cat('\nyesterday-specific subsets\n')

giyest <- sym[which(sym$Date == yesterday &
                      sym$cat == "gi"),]
giyest <- giyest[order(giyest$Zipcode),]

iliyest <- sym[which(sym$Date == yesterday &
                       sym$cat == "ili"),]
iliyest <- iliyest[order(iliyest$Zipcode),]

neuroyest <- sym[which(sym$Date == yesterday &
                         sym$cat == "neuro"),]
neuroyest <- neuroyest[order(neuroyest$Zipcode),]

rashyest <- sym[which(sym$Date == yesterday &
                        sym$cat == "rash"),]
rashyest <- rashyest[order(rashyest$Zipcode),]

respyest <- sym[which(sym$Date == yesterday &
                        sym$cat == "resp"),]
respyest <- respyest[order(respyest$Zipcode),]

###################
#HEAT DF
###################
cat('\nheat calendar \n')
heat <- as.data.frame(unique(sort(alless1213$Date)))
colnames(heat) <- "Date"

#Create columns for each symname
for (i in tolower(symnames)){
  heat[,i] <- NA}

#Populate columns with daily cases
for (i in tolower(symnames)){
  
  cat(paste0('\nheat calendar for ', i))
  
  for (j in 1:nrow(heat)){
    if(format(heat$Date[j], '%m-%d') == '01-01'){
      cat(paste0('\n     ', format(heat$Date[j], '%Y')))
    }
      
    
    heat[which(heat$Date == heat$Date[j]),
         colnames(heat[which(grepl(i, colnames(heat)) == TRUE)])] <-
      nrow(symOld[which(symOld$cat == i &
                          symOld$Date == heat$Date[j]),])
  }
}
heat <- heat[which(heat$Date < today),]

###################
#CREATE DIST DF FOR DISTRIBUTION OF BASELINE OBS (HISTOGRAMS)
###################
dist <- heat[which(grepl(bl.range, heat$Date)==TRUE),]

############################################################
####################

###################
# HEAT CALENDAR FUNCTION
###################
cat('\n\nmaking heat calendar visualization\n')
#MATH
#CREATE THE CALENDAR HEAT FUNCTION (NOT MINE) ##############
calendarHeat <- function(dates, 
                         values, 
                         ncolors=99, 
                         color="g2r", 
                         varname="Values",
                         date.form = "%Y-%m-%d", ...) {
  require(lattice)
  require(grid)
  require(chron)
  if (class(dates) == "character" | class(dates) == "factor" ) {
    dates <- strptime(dates, date.form)
  }
  caldat <- data.frame(value = values, dates = dates)
  min.date <- as.Date(paste(format(min(dates), "%Y"),
                            "-1-1",sep = ""))
  max.date <- as.Date(paste(format(max(dates), "%Y"),
                            "-12-31", sep = ""))
  dates.f <- data.frame(date.seq = seq(min.date, max.date, by="days"))
  
  # Merge moves data by one day, avoid
  caldat <- data.frame(date.seq = seq(min.date, max.date, by="days"), value = NA)
  dates <- as.Date(dates) 
  caldat$value[match(dates, caldat$date.seq)] <- values
  
  caldat$dotw <- as.numeric(format(caldat$date.seq, "%w"))
  caldat$woty <- as.numeric(format(caldat$date.seq, "%U")) + 1
  caldat$yr <- as.factor(format(caldat$date.seq, "%Y"))
  caldat$month <- as.numeric(format(caldat$date.seq, "%m"))
  yrs <- as.character(unique(caldat$yr))
  d.loc <- as.numeric()                        
  for (m in min(yrs):max(yrs)) {
    d.subset <- which(caldat$yr == m)  
    sub.seq <- seq(1,length(d.subset))
    d.loc <- c(d.loc, sub.seq)
  }  
  caldat <- cbind(caldat, seq=d.loc)
  
  #color styles
  r2b <- c("#0571B0", "#92C5DE", "#F7F7F7", "#F4A582", "#CA0020") #red to blue                                                                               
  r2g <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384")   #red to green
  g2r <- rev(r2g)
  w2b <- c("#045A8D", "#2B8CBE", "#74A9CF", "#BDC9E1", "#F1EEF6")   #white to blue
  w2b <- rev(w2b)
  
  assign("col.sty", get(color))
  calendar.pal <- colorRampPalette((col.sty), space = "Lab")
  def.theme <- lattice.getOption("default.theme")
  cal.theme <-
    function() {  
      theme <-
        list(
          strip.background = list(col = "transparent"),
          strip.border = list(col = "transparent"),
          axis.line = list(col="transparent"),
          par.strip.text=list(cex=0.8))
    }
  lattice.options(default.theme = cal.theme)
  yrs <- (unique(caldat$yr))
  nyr <- length(yrs)
  print(cal.plot <- levelplot(value~woty*dotw | yr, data=caldat,
                              as.table=TRUE,
                              aspect=.12,
                              layout = c(1, nyr%%7),
                              between = list(x=0, y=c(1,1)),
                              strip=TRUE,
                              main = paste("Calendar Heat Map of ", varname, sep = ""),
                              scales = list(
                                x = list(
                                  at= c(seq(2.9, 52, by=4.42)),
                                  labels = month.abb,
                                  alternating = c(1, rep(0, (nyr-1))),
                                  tck=0,
                                  cex = 0.7),
                                y=list(
                                  at = c(0, 1, 2, 3, 4, 5, 6),
                                  labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                                             "Friday", "Saturday"),
                                  alternating = 1,
                                  cex = 0.6,
                                  tck=0)),
                              xlim =c(0.4, 54.6),
                              ylim=c(6.6,-0.6),
                              cuts= ncolors - 1,
                              col.regions = (calendar.pal(ncolors)),
                              xlab="" ,
                              ylab="",
                              colorkey= list(col = calendar.pal(ncolors), width = 0.6, height = 0.5),
                              subscripts=TRUE
  ) )
  panel.locs <- trellis.currentLayout()
  for (row in 1:nrow(panel.locs)) {
    for (column in 1:ncol(panel.locs))  {
      if (panel.locs[row, column] > 0)
      {
        trellis.focus("panel", row = row, column = column,
                      highlight = FALSE)
        xyetc <- trellis.panelArgs()
        subs <- caldat[xyetc$subscripts,]
        dates.fsubs <- caldat[caldat$yr == unique(subs$yr),]
        y.start <- dates.fsubs$dotw[1]
        y.end   <- dates.fsubs$dotw[nrow(dates.fsubs)]
        dates.len <- nrow(dates.fsubs)
        adj.start <- dates.fsubs$woty[1]
        
        for (k in 0:6) {
          if (k < y.start) {
            x.start <- adj.start + 0.5
          } else {
            x.start <- adj.start - 0.5
          }
          if (k > y.end) {
            x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] - 0.5
          } else {
            x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] + 0.5
          }
          grid.lines(x = c(x.start, x.finis), y = c(k -0.5, k - 0.5), 
                     default.units = "native", gp=gpar(col = "grey", lwd = 1))
        }
        if (adj.start <  2) {
          grid.lines(x = c( 0.5,  0.5), y = c(6.5, y.start-0.5), 
                     default.units = "native", gp=gpar(col = "grey", lwd = 1))
          grid.lines(x = c(1.5, 1.5), y = c(6.5, -0.5), default.units = "native",
                     gp=gpar(col = "grey", lwd = 1))
          grid.lines(x = c(x.finis, x.finis), 
                     y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                     gp=gpar(col = "grey", lwd = 1))
          if (dates.fsubs$dotw[dates.len] != 6) {
            grid.lines(x = c(x.finis + 1, x.finis + 1), 
                       y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                       gp=gpar(col = "grey", lwd = 1))
          }
          grid.lines(x = c(x.finis, x.finis), 
                     y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                     gp=gpar(col = "grey", lwd = 1))
        }
        for (n in 1:51) {
          grid.lines(x = c(n + 1.5, n + 1.5), 
                     y = c(-0.5, 6.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
        }
        x.start <- adj.start - 0.5
        
        if (y.start > 0) {
          grid.lines(x = c(x.start, x.start + 1),
                     y = c(y.start - 0.5, y.start -  0.5), default.units = "native",
                     gp=gpar(col = "black", lwd = 1.75))
          grid.lines(x = c(x.start + 1, x.start + 1),
                     y = c(y.start - 0.5 , -0.5), default.units = "native",
                     gp=gpar(col = "black", lwd = 1.75))
          grid.lines(x = c(x.start, x.start),
                     y = c(y.start - 0.5, 6.5), default.units = "native",
                     gp=gpar(col = "black", lwd = 1.75))
          if (y.end < 6  ) {
            grid.lines(x = c(x.start + 1, x.finis + 1),
                       y = c(-0.5, -0.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
          } else {
            grid.lines(x = c(x.start + 1, x.finis),
                       y = c(-0.5, -0.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
          }
        } else {
          grid.lines(x = c(x.start, x.start),
                     y = c( - 0.5, 6.5), default.units = "native",
                     gp=gpar(col = "black", lwd = 1.75))
        }
        
        if (y.start == 0 ) {
          if (y.end < 6  ) {
            grid.lines(x = c(x.start, x.finis + 1),
                       y = c(-0.5, -0.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
          } else {
            grid.lines(x = c(x.start + 1, x.finis),
                       y = c(-0.5, -0.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5), default.units = "native",
                       gp=gpar(col = "black", lwd = 1.75))
          }
        }
        for (j in 1:12)  {
          last.month <- max(dates.fsubs$seq[dates.fsubs$month == j])
          x.last.m <- dates.fsubs$woty[last.month] + 0.5
          y.last.m <- dates.fsubs$dotw[last.month] + 0.5
          grid.lines(x = c(x.last.m, x.last.m), y = c(-0.5, y.last.m),
                     default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          if ((y.last.m) < 6) {
            grid.lines(x = c(x.last.m, x.last.m - 1), y = c(y.last.m, y.last.m),
                       default.units = "native", gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.last.m - 1, x.last.m - 1), y = c(y.last.m, 6.5),
                       default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          } else {
            grid.lines(x = c(x.last.m, x.last.m), y = c(- 0.5, 6.5),
                       default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          }
        }
      }
    }
    trellis.unfocus()
  } 
  lattice.options(default.theme = def.theme)
}

###################
# BASELINES AND MATH
###################
cat('\nbaseline arithmetic\n')
blgi <- sum(dist$gi) / length(dist$gi)
blili <- sum(dist$ili) / length(dist$ili)
blneuro <- sum(dist$neuro) / length(dist$neuro)
blrash <- sum(dist$rash) / length(dist$rash)
blresp <- sum(dist$resp) / length(dist$resp)


#CREATE 5% AND 95% RANGES
cat('\n95% ranges\n')
blgiq <- quantile(dist$gi, c(.05, .95), na.rm=T) 
bliliq <- quantile(dist$ili, c(.05, .95), na.rm=T) 
blneuroq <- quantile(dist$neuro, c(.05, .95), na.rm=T) 
blrashq <- quantile(dist$rash, c(.05, .95), na.rm=T) 
blrespq <- quantile(dist$resp, c(.05, .95), na.rm=T) 

###################
#GEOGRAPHICAL FLAG
###################
cat('\ncluster flagging \n')
geoCluster <- as.data.frame(zip$Zipcode)
colnames(geoCluster) <- "Zipcode"

for (i in tolower(symnames)){
  geoCluster[,i] <- ""
}


#I SHOULD BE USING REGRESSION!!!!
#for (i in zip$Zipcode){
#  for (j in tolower(symnames)){
#    heat[,paste0(j,i)] <- NA  }}

#for (i in zip$Zipcode){
#for (j in tolower(symnames)){
#    for (k in heat$Date){
#      heat[which(heat$Date == k),
#           paste0(j,i)] <-
#        nrow(symOld[which(symOld$cat == j &
#                            symOld$Zipcode == i &
#                            symOld$Date == k),])}}}

#save.image("E:/fdoh/private/blablabla.RDATA")

#I want to flag any zipcode with a symptom incidence of 
#greater than 4 cases (at 125% level)
#or greater than 2 cases (at 150% level)
#or greater than 1 case (at 200% level)
for (i in tolower(symnames)){
  
  cat(paste0('\n\ncluster flagging for ', i))
  
  for (j in geoCluster$Zipcode){
    
    cat(paste0('\n    zip code: ', j))
    
    
    geoCluster[which(geoCluster$Zipcode == j), i] <-
      ifelse(zip[which(zip$Zipcode == j),paste0(i, "per")] > 1.25 &
               zip[which(zip$Zipcode == j), i] > 4,
             "CHECK",
             ifelse(zip[which(zip$Zipcode == j),paste0(i, "per")] > 1.5 &
                      zip[which(zip$Zipcode == j), i] > 2,
                    "CHECK",
                    ifelse(zip[which(zip$Zipcode == j),paste0(i, "per")] > 2 &
                             zip[which(zip$Zipcode == j), i] > 1,
                           "CHECK",
                           "")))
  }
}



###################
#RECORD OF INTEREST TABLES
###################
cat('\nROI table clean up\n')
roc <- roi[c("Date", "Age", "MedRecNo", "Sex", "CCDD", "Region", "Hospital")]
roc$Date <- as.character(roc$Date)

###################
#MAP PARAMETERS
###################
#joewatercolor <- openmap(c(29.96, -82.7), c(29.35,-81.9),
#                         type="stamen-watercolor")
#joemapwatercolor <- openproj(joewatercolor, projection = "+proj=longlat")

###################
#WORD CLOUD
###################
cat('\nstarting word cloud\n')
remove <- "[|]|[(]|[])]|/|;|:|[(*]|&|-[)]|[(]|[-]|[--])"
myWords <- unlist(strsplit(as.character(gsub(remove,"",
                                             toupper(alless$CCDD[which(alless$Date == yesterday)]))), " "))

myWordsBL <- unlist(strsplit(as.character(gsub(remove,"",
                                               toupper(alless1213$CCDD[which(alless1213$bl == TRUE)]))), " "))



myWordsDF <- as.data.frame(table(myWords))
colnames(myWordsDF) <- c("word","count")
myWordsBLDF <- as.data.frame(table(myWordsBL))
colnames(myWordsBLDF) <- c("word","count")
combinedDF <- rbind(myWordsDF, myWordsBLDF)

myWordsTable <- table(myWords)
myWordsBLTable <- table(myWordsBL)



combinedWords <- c(myWords, myWordsBL)

myCloud <- as.data.frame(as.character(unique(sort(combinedWords))))
colnames(myCloud) <- "word"

cat('\n')
myCloud$recent <- 0
for (i in myCloud$word){
  cat(paste0('tabulating recent instances of word "', i, '"\n'))
  myCloud$recent[which(myCloud$word == i)] <-
    sum(myWordsDF$count[which(myWordsDF$word == i)])} 

myCloud$bl <- 0
for (i in myCloud$word){
  cat(paste0('tabulating baseline instances of word "', i, '"\n'))
  
  myCloud$bl[which(myCloud$word == i)] <-
    sum(myWordsBLDF$count[which(myWordsBLDF$word == i)])/
    length(unique(sort(alless1213$Date[which(alless1213$bl == TRUE)])))}

cat(paste0('\n\n cleaning up word cloud tables \n'))
myCloud$prop <- 1+ ((myCloud$recent+0.9) / (myCloud$bl+0.9)) 
myCloud$color <- ifelse(myCloud$recent <1,
                        "grey",
                        "black")

myCloud <- myCloud[which(myCloud$recent >0),]
myCloud <- myCloud[order(myCloud$prop),]
myCloud$word <- as.character(myCloud$word)
myCloud <- myCloud[which(nchar(myCloud$word) > 1),]
myCloud$recentCubicRoot <- myCloud$recent^(1/3)

myCloud$colorCat<- cut(myCloud$recentCubicRoot, 9, labels=FALSE)
cloudColors <- colorRampPalette(brewer.pal(9, "Blues"))(11)
cloudColors <- rev(cloudColors[1:11])
myCloud$color <- cloudColors[myCloud$colorCat]
myCloud <- myCloud[which(regexpr(paste0("AND|CANT|TODAY|ENCOUNTER|WITHOUT|",
                                        "NOT|OF|LIKE|THERAPY|UNSPECIFIED|OR|IN",
                                        "THIS|INITIAL|AT"), myCloud$word) <1),]


# wordcloud(words=myCloud$word,
#           freq=myCloud$prop^3,
#           scale=c(2,0.00001),
#           max.words=Inf,
#           random.order=FALSE,
#           rot.per=0,
#           colors=myCloud$color[order(myCloud$prop^3)],
#           ordered.colors=FALSE)
cat('\nword cloud done\n')


###################
#FLAG TABLE
###################
cat('\nstarting flag table\n')
#FUNCTION TO CONVERT TEXT TO REGEXPR SEARCH TERM
regexFun <- function(x){
  paste(substr(x,1,5),"*","|",
        tolower(substr(x,1,5)),"*|",
        capwords(tolower(substr(x, 1,5))),"*",sep="", collapse=NULL)}


wordsOfInterest <- as.data.frame(myCloud[which(ifelse(myCloud$bl <=0, 
                                                      myCloud$recent >= 3, 
                                                      myCloud$recent > 3*(myCloud$bl)) & 
                                                 myCloud$recent >= 3),])
wordsOfInterest <- wordsOfInterest[order(wordsOfInterest$recent,
                                         decreasing=TRUE),]

flag <- as.data.frame(cbind(wordsOfInterest$word,
                            wordsOfInterest$recent,
                            round(wordsOfInterest$bl, digits=2)))
colnames(flag) <- c("Word", "Yesterday", "Baseline")
flag$Baseline <- as.numeric(as.character(flag$Baseline))

# remove numeric terms
flag <- flag[which(!grepl('[[:digit:]]', flag$Word)),]

#CAPITALIZE FIRST LETTER FUNCTION
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))}


#CREATE SEARCH TERMS
heat15 <- heat[which(heat$Date > max(heat$Date)-15),]

flagSearch <- as.data.frame(matrix(rep(NA, length(flag$Word)*15), nrow=15))
colnames(flagSearch) <-flag$Word
flagSearch <- cbind(heat15$Date, flagSearch)
colnames(flagSearch)[1] <- "Date"

newColStart <- length(colnames(heat15))+1
newColEnd <- length(colnames(heat15))+length(flagSearch$Word)
#colnames(heat15)[newColStart:newColEnd] <-  flagSearch$Word

cat('\n\ngetting historical counts for flagged terms\n')
for(i in 1:nrow(flagSearch)){
  cat(paste0('\n', flagSearch$Date[i], '\n'))
  for (j in colnames(flagSearch[2:(length(flag$Word)+1)])){
    cat(paste0('---- ', j, '\n'))
    flagSearch[which(flagSearch$Date == flagSearch$Date[i]),j] <-
      nrow(alless1213[which(alless1213$Date == flagSearch$Date[i] &
                              regexpr(regexFun(j), alless1213$CCDD)>=0),])}}

# cat('\n plotting flagged terms\n')
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
cat('Flag table done\n')
###################
#ILI YEAR TO YEAR COMPARISON
###################
#heat$j <- as.numeric(format(heat$Date, format="%j"))

#2013
#ili13= spm(heat$ili[which(heat$Date >= "2013-01-01")]~
#             f(heat$j[which(heat$Date >= "2013-01-01")], spar=20), omit.missing=TRUE)
#2012
#ili12= spm(heat$ili[which(heat$Date < "2013-01-01" &
#                            heat$Date >= "2012-01-01")]~
#             f(heat$j[which(heat$Date < "2013-01-01" &
#                              heat$Date >= "2012-01-01")], spar=20), omit.missing=TRUE)

#plot(heat$j, heat$ili, type="n", main="ILI: 2012 vs. 2013",
#     xlab="Date", ylab="Daily cases", xlim=c(250,365))
#points(heat$j[which(heat$Date < "2013-01-01")],
#       heat$ili[which(heat$Date < "2013-01-01")],
#       pch=16, col=adjustcolor("dodgerblue3", alpha.f=0.3))
#points(heat$j[which(heat$Date > "2013-01-01")],
#       heat$ili[which(heat$Date > "2013-01-01")],
#       pch=16, col=adjustcolor("darkred", alpha.f=0.3))
#lines(ili12,shade.col=adjustcolor("dodgerblue3", alpha.f=0.3),
#      rug.col="blue", col="blue")
#lines(ili13,shade.col=adjustcolor("darkred", alpha.f=0.3),
#      rug.col="darkred", col="darkred")

#legend(x="topleft", pch=16, col=c("dodgerblue2", "darkred"),
#       legend=c("2012", "2013"), lwd=2)

###################
#
###################
# jj <- map("county", "florida")
# plot(jj, type="l")

###################
# FUNCTION TIME!!!!
###################
cat('\ndefining visualization functions\n')
#### MapCases
MapCases <- function(variable, color){
  plotvar <- variable
  nclr <- max(plotvar+1, na.rm=TRUE) # number of bins
  plotclr <- c("white", brewer.pal(nclr, color))
  cuts <- cut(plotvar, breaks=nclr,labels=FALSE)
  mapCols <- plotclr[cuts]
  plot(zip.map, border="grey", col=mapCols, main="Cases yesterday")
  legend("bottomleft", # position
         legend = seq(0,max(plotvar, na.rm=TRUE), 1), 
         title = "Cases",
         fill = plotclr,
         cex = 1 - (0.03*max(plotvar, na.rm=TRUE)),
         bty = "n",
         border="grey") # border
}

#### MapCasesWeek
MapCasesWeek <- function(variable, color){
  plotvar<-variable
  nclr<- 5 # number of bins (3-8)
  min<- floor(min(plotvar))
  max<- ceiling(max(plotvar))
  breaks<- (max-min) / nclr
  plotclr<- brewer.pal(nclr, color)
  class<- classIntervals(plotvar, nclr, style ="fixed", fixedBreaks=seq(min, max, breaks))
  
  colcode<- findColours(class, plotclr)
  colcode2<-gsub(",","-", gsub("[[]|[)]|[]]","", names(attr(colcode, "table"))))
  
  plot(zip.map, border="grey", col=colcode, main="Cases yesterday")
  legend("bottomleft", # position
         legend = colcode2, 
         title = "Cases",
         fill = plotclr,
         cex = 0.6,
         bty = "n",
         border="grey") # border
}




#MapProp
MapProp <- function(variable, color){
  plotvar <- variable
  nclr <- 5# number of bins
  plotcat <- ifelse(variable==0, 0,
                    ifelse(variable >0 & variable <1, 1,
                           ifelse(variable==1, 2,
                                  ifelse(variable>1 & variable <=2, 3,
                                         ifelse(variable>2, 4,
                                                0))))) 
  plotclr <- c("white", brewer.pal(4, color))
  cuts <- cut(plotvar, breaks=nclr,labels=FALSE)
  mapCols <- plotclr[cuts]
  legText <- as.character(100*seq(0,4, 1))
  legText <- c("0",
               "< 100",
               "100",
               "101 - 200",
               "> 200")
  plot(zip.map, border="grey", col=mapCols, main="Yesterday relative to baseline")
  legend("bottomleft", # position
         legend = legText, 
         title = "Cases\n(% of expected)",
         fill = plotclr,
         cex = 0.56,
         bty = "n",
         border="grey") # border
}

############ DETAILS
par(mfrow=c(1,1))

HistFun <- function(variable, color){
  hist(variable, breaks=15, col=adjustcolor("black", alpha.f=0.5),
       main="Cases", cex.lab=0.6, cex.main=0.8,
       xlab="Baseline daily cases",
       ylab="Frequency", border=FALSE)
  abline(v=heat$gi[which(heat$Date == yesterday)], lwd=14, lty=1,
         col=adjustcolor(color, alpha.f=0.5))
  legend(x="topright",
         lty=1, lwd=3, legend="Yesterday's\nobservation", cex=0.5, 
         col=adjustcolor(color, alpha.f=0.5),bty="n")
}

AgeHistFun <- function(variable, baseline, color){
  hist(baseline, main="Age", cex.main=0.8, col=adjustcolor("black", alpha.f=0.5), border=FALSE,
       ylab="Frequency", xlab="Age", freq=FALSE, cex.lab=0.6)
  hist(variable,
       col=adjustcolor(color, alpha.f=0.4), ylab="frequency", xlab="age", freq=FALSE, add=TRUE,
       border=FALSE)
  legend(x="topright", fill=adjustcolor(c("black", color), alpha.f=0.5), bty="n", border=FALSE,
         legend=c("2012 baseline", "Yesterday"), cex=0.6)
}

TimeHistFun <- function(variable, baseline, color){
  hist(baseline, freq=FALSE, border=FALSE, col=adjustcolor("black", alpha.f=0.5), 
       main="Check-in time", xlab="Time of day", ylab="Frequency", xaxt="n", ylim=c(0,0.06), cex=0.6, cex.main=0.8,
       cex.lab=0.6)
  hist(variable, freq=FALSE, border=FALSE, 
       col=adjustcolor(color, alpha.f=0.5), xaxt="n", add=TRUE, ylim=c(0,0.06))
  axis(1, at=c(1,12,24,36,50), labels=c("midnight","6am","noon","6pm","midnight"), cex.axis=0.6)
  legend(x="topright", fill=adjustcolor(c("black", color), alpha.f=0.5), bty="n", border=FALSE,
         legend=c("2012 baseline", "Yesterday"), cex=0.6)
  
}

SexFun <- function(variable, baseline, color){
  prop <- table(baseline)/length(baseline) 
  barplot(prop, ylim=c(0,1), col=adjustcolor("black",alpha.f=0.5), border=FALSE)
  prop <- table(variable)/length(variable)
  barplot(prop, ylim=c(0,1), col=adjustcolor(color,alpha.f=0.5), add=TRUE, border=FALSE, 
          main="Sex", cex.main=0.8, xlab=NA)
  legend(x="topright", fill=adjustcolor(c("black", color), alpha.f=0.5), 
         legend=c("2012 baseline", "Yesterday"), border=FALSE, bty="n", cex=0.6)
}


SexFun <- function(variable, baseline, color){
  variable <- factor(variable, levels=c("F", "M"))
  baseline <- factor(baseline, levels=c("F", "M"))
  prop <- table(baseline)/length(baseline) 
  barplot(prop, ylim=c(0,1), col=adjustcolor("black",alpha.f=0.5), border=FALSE)
  prop <- table(variable)/length(variable)
  barplot(prop, ylim=c(0,1), col=adjustcolor(color,alpha.f=0.5), add=TRUE, border=FALSE, 
          main="Sex", cex.main=0.8, xlab=NA)
  legend(x="topright", fill=adjustcolor(c("black", color), alpha.f=0.5), 
         legend=c("2012 baseline", "Yesterday"), border=FALSE, bty="n", cex=0.6)
}


RaceFun <- function(variable, baseline, color){
  
  prop <- table(baseline)/length(baseline) 
  barplot(prop, ylim=c(0,1), col=adjustcolor("black",alpha.f=0.5), border=FALSE,
          xaxt="n")
  prop <- table(variable)/length(variable)
  barplot(prop, ylim=c(0,1), col=adjustcolor(color,alpha.f=0.5), add=TRUE, border=FALSE, 
          main="Race", cex.main=0.8)
  legend(x="topright", fill=adjustcolor(c("black", color), alpha.f=0.5), 
         legend=c("2012 baseline", "Yesterday"), border=FALSE, bty="n", cex=0.6)
}

TimeSeriesFun <- function(variable, color){
  plot(heat$Date, variable, type="n", xlab="Date", ylab="Cases",
       xlim=c(yesterday-q, yesterday), cex.axis=0.75, cex.main=0.8, cex.lab=0.6)
  axis(side=1, at=heat$Date, labels=format(heat$Date, format="%d\n%b"), cex.axis=0.6)
  lines(heat$Date, variable, lty=1, col="darkgrey")
  points(heat$Date, variable, pch=1, col="black")
  points(heat$Date, variable, pch=20, col=color)
  mm <- c(c(heat$Date, today), c(today,rev(heat$Date)))
  zzblq <-c(rep(min(quantile(variable[which(heat$Date <= today - 351 &
                                              heat$Date >= today - 380)],
                             c(.05, .95), na.rm=T)),length(heat$Date)+1), 
            rev(rep(max(quantile(variable[which(heat$Date <= today - 351 &
                                                  heat$Date >= today - 380)],
                                 c(.05, .95), na.rm=T)),length(heat$Date)+1)))
  polygon(mm, zzblq, col=adjustcolor(color, alpha.f=0.3), border=FALSE)
  abline(h=sum(variable[which(heat$Date <= today - 351 &
                                heat$Date >= today - 380)])/30, 
         col=adjustcolor(color, alpha.f=0.3), lty=1, lwd=5)
  legend(x="topright", legend=c("Expected", "Normal variation"),
         lty=c(1,1), col=c(color,adjustcolor(color, alpha.f=0.3)), ncol=1,
         x.intersp=0.2, y.intersp=0.8, bor=T,cex=0.6, lwd=c(1,4), bty="n")  
}


# HistFun(variable = dist$gi, 
#         color = symcols[1])
# 
# AgeHistFun(variable = giyest$Age,
#            baseline = symOld$Age[which(symOld$bl == TRUE &
#                                          symOld$cat == "gi")],
#            color = symcols[1])
# 
# TimeHistFun(variable = giyest$HalfHour,
#             baseline = symOld$HalfHour[which(symOld$bl == TRUE &
#                                                symOld$cat == "gi")],
#             color = symcols[1]) 
# 
# SexFun(variable = giyest$Sex,
#        baseline = symOld$Sex[which(symOld$bl == TRUE &
#                                      symOld$cat == "gi")],
#        color= symcols[1])
# 
# RaceFun(variable = giyest$race,
#         baseline = symOld$race[which(symOld$bl == TRUE &
#                                        symOld$cat == "gi")],
#         color=symcols[1])
# 
# TimeSeriesFun(variable = heat$ili,
#               color=symcols[2])

cat('\nvisualization functions done\n')

###################
#MAKE OVERALL (LAST 7 days)
###################
cat('\n making weekly plots\n')
overall <- heat[which(heat$Date > yesterday - 7),]


###################
#WEEKLY AND DAILY BARPLOT
###################
weeklysums <- c(
  (sum(overall$gi)/blgi/7)*100,
  (sum(overall$ili)/blili/7)*100,
  (sum(overall$neuro)/blneuro/7)*100,
  (sum(overall$rash)/blrash/7)*100,
  (sum(overall$resp)/blresp/7)*100)
weeklysums <-round(weeklysums, digits=1)

dailysums <- c(
  (overall$gi[7]/blgi)*100,
  (overall$ili[7]/blili)*100,
  (overall$neuro[7]/blneuro)*100,
  (overall$rash[7]/blrash)*100,
  (overall$resp[7]/blresp)*100)
dailysums <-round(dailysums, digits=1)

###################
# SPECIAL SEARCHES
###################
cat('\nspecial searches\n')
rabies <- alless1213[which(regexpr("rabies*|RABIES*|Rabies*|bite*|Bite*|BITE*", alless1213$CCDD)>0 &
                             grepl("insect|INSECT", alless1213$CCDD) == FALSE),]
rabiesyest <- rabies[which(rabies$Date == yesterday),]
rabies7 <- rabies[which(rabies$Date >= yesterday-6),]

###################
# FUNCTION FOR EXAMINING ANY TERM IN DEPTH
###################
cat('\nfunction for examining any term in depth\n')
nadiaFun <- function(x){
  y <- alless1213[which(regexpr(regexFun(x), alless1213$CCDD)>0),]
  z <- y[which(y$Date == yesterday),]
  View(z)}

examineFun <- function(x){
  alless1213[which(regexpr(regexFun(x), alless1213$CCDD)>0 &
                     alless1213$Date > yesterday - 6),]
}

spice <- examineFun("MARIJUANA|SYNTHETIC|K2|SPICE")

ebola <- examineFun("LIBERIA|SIERRA LEONE|CONGO|GUINEA|AFRICA|EBOLA")

wnv <- alless[which(grepl(regexFun("WEST NILE|WNV|OCULAR"), alless$CCDD)>0 &
                      grepl(regexFun("MOQUITO"), alless$CCDD)  &
                      alless$Date > yesterday - 6),]

#nadiaFun("SCABIES")
#nadiaFun("BITE")
#nadiaFun("STREP")





####################
# MERS detection
####################
# mersWords <- paste(regexFun(c("Saudi","Arab","Emirat","UAE","Qatar","Oman",
#                         "Jordan","Kuwait","Yemen","Leban","Lebon","Middle",
#                         "mers", "MERS", "Mers")), collapse="|")
# mersSymptoms <- paste(regexFun(c("respir","severe","sob","shortness","breath",
#                                 "fever","pneumo","diar","vomit","naus")),
#                                collapse="|")
# mers <- alless[which(grepl(mersWords, alless$CCDD) == TRUE &
#                        grepl(mersSymptoms, alless$CCDD) == TRUE),] 
# 
# mers2 <- alless2[which(grepl(mersWords, alless2$CCDD) == TRUE &
#                        grepl(mersSymptoms, alless2$CCDD) == TRUE),] 
# 
# mers <- rbind(mers,mers2)
# 
# 
# nrow(mers)
cat('\nkipping surface function\n')
# ###################
# # SURFACE FUNCTION
# ##################
# 
# 
# 
# # Read in boundary
# #boundary <- readOGR("E:/fdoh/private/surv/Alachua_Boundary", "Alachua_Boundary")
# 
# boundary <- zip.map # have to do this due to lack of projection system
# boundary <- unionSpatialPolygons(boundary, rep(1, length(boundary@polygons)))
# 
# # # Read in population
# # pop <- readOGR("E:/fdoh/private/surv/Alachua_CT_POP", "Alachua_CT_POP")
# 
# # Define color vector
# my_colors <- colorRampPalette(c("blue", "red"))(100)
# # 
# # SurfaceFun <- function(disease = "ili",
# #                        boundary_shape = boundary){
# #   
# #   
# #   
# #   # getting coordinates of alachua boundary
# #   boundary_points <- boundary@polygons[[1]]@Polygons
# #   boundary_points <- boundary_points[[1]]@coords
# #   
# #   # Get trap locations and data values
# #   a <- data.frame("x" = zip.map$x,
# #                   "y" = zip.map$y,
# #                   "z" = zip[,paste0(disease, "per")])
# #   # Make into a geodata object
# #   b <- as.geodata(a)
# #   
# #   # Predict multiple points in Alachua County's boundary
# #   x <- seq(min(boundary_points[,1]), max(boundary_points[,1]), length = 100)
# #   y <- seq(min(boundary_points[,2]), max(boundary_points[,2]), length = 100)
# #   
# #   # Make a grid of those points
# #   pred.grid <- expand.grid(x,y)
# #   
# #   
# #   # kriging calculations
# #   kc <- krige.conv(geodata = b, coords = b$coords, data = b$data,
# #                    locations = pred.grid,
# #                    borders = boundary_points,
# #                    #borders = boundary@polygons,
# #                    # borders = ALACHUA BORDERS!,
# #                    krige = krige.control(type.krige = "ok",
# #                                          cov.pars = c(5000,10000000))) #10, 3.33 # what is this?
# #   
# #   
# #   
# #   # Plot!
# #   # displaying predicted values
# #   image(kc, loc = pred.grid, 
# #         col = my_colors,
# #         xlab=NA, ylab=NA,
# #         xaxt = "n",
# #         yaxt = "n",
# #         xpd = NA,
# #         bty = "n")
# #   
# #   # Define percentiles for legend
# #   legtemp <-  round(quantile(kc$predict, probs = seq(0,1,, length = 10)))
# #   
# #   legend(x="topright",
# #          fill = my_colors[c(1,11,22,33,44,55,66,77,88,100)],
# #          legend = c(legtemp[1], NA, NA, legtemp[4], NA, NA, legtemp[7], NA, NA, legtemp[10]),
# #          border = FALSE,
# #          bty = "n",
# #          ncol = 1,
# #          y.intersp = 0.5,
# #          title = "Interpolation",
# #          cex = 0.75)
# # }
# # SurfaceFun("neuro")



######################
#******SAVE
#******IMAGE
######################
cat('\nsaving image... \n\n\n')
save.image(paste(private, "/",
                 today,
                 "/",
                 "zap.Rdata", sep=""))
cat('\n-----------IMAGE SAVED!-----------\n\n')

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
# symOld$kid <- ifelse(symOld$Age >= 18, FALSE, TRUE)
# x <- symOld %>%
#   filter(cat == "gi", kid == TRUE) %>%
#   group_by(Date) %>%
#   summarise(count = n())
# 
# x
# plot(x$Date, x$count, xlim = c(max(x$Date) - 20, max(x$Date)),
#      xlab = "Last 20 days", ylab = "Cases",
#      type = "l", col = "darkblue")
