par(mar=c(5,2,2,1))
par(oma=c(0,0,0,0))
today <- Sys.Date()  


###################
#DEFINE AND SET WD
###################

if ( Sys.info()["sysname"] == "Linux" ){
  
  originalwd <- paste0("/media/joebrew/JB/fdoh/private/surv/", today)
  survwd <- "/media/joebrew/JB/fdoh/private/surv"
  historicalwd <- "/media/joebrew/JB/fdoh/private/surv/historical"
  giswd <- "/media/joebrew/JB/fdoh/private/surv/gis"
  
} else {
  
  originalwd <- paste0("E:/fdoh/private/surv/", today)
  survwd <- "E:/fdoh/private/surv"
  historicalwd <- "E:/fdoh/private/surv/historical"
  giswd <- "E:/fdoh/private/surv/gis"
  
}
setwd(paste(survwd, "/",
            today, sep=""))
Sweave('zap.Rnw')
tools::texi2pdf('zap.tex')
