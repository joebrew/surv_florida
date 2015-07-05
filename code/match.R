##### 
# LOAD PACKAGES
#####
library(dplyr)

#####
# DEFINE DIRECTORY LOCATIONS
#####
if(Sys.info()['user'] == 'joebrew'){
  public <- '/home/joebrew/Documents/tony_b'
  private <- '/home/joebrew/Documents/private_data/boselli/'
} else if(Sys.info()['user'] == 'benbrew'){
  public <- '/home/benbrew/Documents/tony_b'
  private <- '/home/benbrew/Documents/private'
}

#####
# READ IN TONY AND FDOE DATA
#####
setwd(public)
school <- read.csv("public_schools.csv", stringsAsFactors = FALSE)
setwd(private)
tony <- read.csv("merged.csv", stringsAsFactors = FALSE)

#####
# CLEAN UP DATAFRAME
#####

# Make lowercase columnnames in tony
names(tony) <- tolower(names(tony))

# Fix percent vaccinated in tony
tony$percent_vaccinated <- 
  as.numeric(
    gsub("%", "", tony$percent_vaccinated))

# Make lowercase district / county names
school$district <- tolower(school$district)
tony$district <- tolower(tony$county)

# Remove periods from district names
school$district <- gsub('[.]', '', school$district)

#make column lower case
school$school <- tolower(school$school)
tony$school <- tolower(tony$school)

# Remove trailing/leading whitespaces from all columns
remove_trail <- function(var){
  if(is.numeric(var)){
    x <- as.numeric(gsub("^\\s+|\\s+$", "", as.character(var)))
  } else {
    x <- gsub("^\\s+|\\s+$", "", as.character(var))
  }
  return(x)
}

for (i in 1:ncol(tony)){
  tony[,i] <- remove_trail(tony[,i])
}

for (i in 1:ncol(school)){
  school[,i] <- remove_trail(school[,i])
}


#remove common words withing school
remove_junk <- function(df){
  df$school <- gsub("school", "", df$school)
  df$school <- gsub("schl ", "", df$school)
  df$school <- gsub("schl", "", df$school)
  df$school <- gsub("center", "", df$school)
  df$school <- gsub("youth", "", df$school)
  df$school <- gsub("academy", "", df$school)
  df$school <- gsub("senior", "", df$school)
  df$school <- gsub("sch$", "", df$school)
  df$school <- gsub("scho ", "", df$school)
  df$school <- gsub("program", "", df$school)
  df$school <- gsub("prog ", "", df$school)
  df$school <- gsub("[.]", "", df$school)
  df$school <- gsub(",", "", df$school)
  df$school <- gsub("/", "", df$school)
  df$school <- gsub("-", "", df$school)
  df$school <- gsub(" s ", "", df$school)
  df$school <- gsub(" oo ", "", df$school)
  df$school <- gsub("   ", " ", df$school)
  df$school <- gsub("  ", " ", df$school)
  df$school <- gsub("[(]|[)]", "", df$school)
  df$school <- remove_trail(df$school)
  
  return(df)
}

school <- remove_junk(school)
tony <- remove_junk(tony)


# try our best to guess school age group from tony's school names
tony$type_t <- ifelse(
  grepl(" elem$| es$| elementary$| primary$| k-8$| k6$| elemtary$| k8$| elemetary$| elemenary$", tony$school),
  "elem",
  ifelse(grepl(" mid$| middle$| ms$| jh$| junior$| intermediate$", tony$school),
         "mid",
         ifelse(grepl(" high$| hs$| upper$| prep$", tony$school),
                "high",
                ifelse(grepl(" pk$| prek$| pre-k$", tony$school),
                       "pre-k",
                       NA))))

# Clean up a bit more
clean_again <- function(df){
    df$school <- gsub("elementary", "es", df$school)
    df$school <- gsub("middle", "ms", df$school)
    df$school <- gsub("js", "ms", df$school)
    df$school <- gsub("high", "hs", df$school)
    return(df)
}
school <- clean_again(school); tony <- clean_again(tony)


#####
# LOOP THROUGH EACH NAME OF TONY TO GET BEST MATCH IN SCHOOL
#####

#make new name holder for tony
tony$name <- NA
# make match score indicator for tony
tony$match_score <- NA
#make new colummn school
school$name <- school$school

# NEW WAY
for(i in 1:nrow(tony)){
  
  # Define which school we're working with
  temp <- tony$school[i]  
  
  # Define which county we're looking in
  county <- tony$district[i]
  
  # Define vector of possible matches (in that county)
  posibs <- school$school[which(school$district == county)]
  
  # Restrict possible matches to only those with same first letter
  #posibs <- posibs[which(substr(posibs, 1, 1) == substr(temp, 1, 1))] 
  
  # Make matrix of match scores
  mat <- adist(x=temp, y=posibs)
  
  # Reward any school with 2 points for matching first 3 letters
  mat <- ifelse(substr(posibs,1,3) == substr(temp, 1,3), mat - 2, mat)
    
  # Which of the posibs is the best match?
  ind <-which.min(mat)[1]
  
  # Set a threshold - if the best match (min(mat)) is greater than 10, 
  # we're considering this "unmatchabel"

  # Assign that best match back to tony
  tony$name[i] <- posibs[ind]
  
  # Assign match score to tony as well
  if(length(mat) < 1){
    tony$match_score[i] <- NA
  } else{
    tony$match_score[i] <- min(mat, na.rm = TRUE)
  }
}

# Explore match score (remember that low is good!)
hist(tony$match_score) # most are great!
tally(group_by(tbl_df(tony), match_score)) 
table(is.na(tony$match_score)) # a few couldn't get any match at all

# Let's keep only those with a match_score of less than
tony_small <- tony[which(tony$match_score <= 1),]


#final 
final <- left_join(x=tony_small,
                   y=school, 
                   by="name")

# Clean up final
final <- final[,c("school.x", "name", "school_number", "totmem", "type", "type_t", "year",
                  "total_black", "total_hispanic", "total_asian", "free_reduced", "per_fr",
                  "number_vaccinated", "number_enrolled", "percent_vaccinated", "county", "match_score")]

# Remove 2013 and duplicate rows
final <- final[order(final$match_score),]
final <- final[which(final$year == 2014),]
final <- final[!duplicated(final$name),]

#new columns for percent black, percent asian, percent hispanic
final$per_black <- (final$total_black/final$totmem)*100
final$per_asian <- (final$total_asian/final$totmem)*100
final$per_hispanic <- (final$total_hispanic/final$totmem)*100
final$per_black <- round(final$per_black, 2)
final$per_asian <- round(final$per_asian, 2)
final$per_hispanic <- round(final$per_hispanic, 2)

school$per_black <- (school$total_black/school$totmem)*100
school$per_asian <- (school$total_asian/school$totmem)*100
school$per_hispanic <- (school$total_hispanic/school$totmem)*100
school$per_black <- round(school$per_black, 2)
school$per_asian <- round(school$per_asian, 2)
school$per_hispanic <- round(school$per_hispanic, 2)

# fix first column name
names(final)[1] <- "name_from_tony"

# Create an imm rate based on school population (rather than tony's denoms)
final$imm_rate <- final$number_vaccinated / final$totmem * 100

# Look at correlation between our calculated imm rate and tony's
plot(final$imm_rate, final$percent_vaccinated)

# Let's remove any observations that are more than 15 percent off from tony's
final <- final[which(sqrt((final$imm_rate - final$percent_vaccinated)^2) < 10),]

# Replot
plot(final$imm_rate, final$percent_vaccinated)


#make chart for avg immunization for type of grade (elem, mid, high) 
by_type <- tony %>%
  group_by(type_t) %>% 
  summarise(avg_vac = mean(percent_vaccinated, na.rm =TRUE))

#get rid of NA 
by_type <- by_type[which(!is.na(by_type$type)),]

#order the columns
by_type <- arrange(by_type, avg_vac)

#barplot by type of grade and avg vaccination achieved 
bp <- barplot(by_type$avg_vac,
        names.arg = by_type$type_t,
        col="lightblue",
        ylab = "Immunization rate",
        ylim = c(0, max(by_type$avg_vac) * 1.1),
        las=1)
box("plot")
text(x = bp[,1],
     y = by_type$avg_vac,
     pos = 1,
     labels = paste0(round(by_type$avg_vac, digits = 2), "%"))  
abline(h = seq(0,20,2), col = adjustcolor("black", alpha.f = 0.2), lty = 3)
title(main = "Immunization (among matched/modeled schools only)")
#plot total population of school and immunization rate

plot(final$totmem, final$percent_vaccinated,
     main="School Population and Immunization",
     xlab="Total Population of School",
     ylab="Percent Vaccinated",
     xlim=c(0, 3200),
     ylim=c(0, 50),
     pch=16,
     col=adjustcolor("lightblue", alpha.f=0.4))
abline(lm(final$percent_vaccinated ~ final$totmem))
#ADD LOESS LINE
lox <- final$totmem
loy <- final$percent_vaccinated
lw1 <- loess(loy ~ lox, span=0.75)
j <- order(lox)
lines(lox[j],lw1$fitted[j],col=adjustcolor("red", alpha.f = 0.4),
      lty=1,
      lwd = 3)


#plot county and average immunization, using tony's larger dataset
by_county <- tony %>% 
  group_by(county) %>%
  summarise(avg_vac = mean(percent_vaccinated, na.rm=TRUE))


# order
by_county <- arrange(by_county, avg_vac)

#how do I fit the names onto the X axis?
cols <- adjustcolor(colorRampPalette(c("yellow", "blue"))(nrow(by_county)), alpha.f = 0.5)
bp <- barplot(by_county$avg_vac,
        names.arg = by_county$county,
        cex.names = 0.7,
        las = 3,
        col = cols,
        ylim = c(0, max(by_county$avg_vac) * 1.1))
text(x = bp[,1],
     y = by_county$avg_vac,
     pos = 3,
     labels = paste0(round(by_county$avg_vac, digits = 1), "%"),
     cex = 0.7,
     col = adjustcolor("black", alpha.f = 0.7))
box("plot")
abline(h = seq(0,30, 2), col = adjustcolor("black", alpha.f = 0.2), lty = 3)
title(main = "Immunization Rate by County (Matched Schools)")

#vaccination by free lunch
type_cols <- rainbow(length(levels(factor(final$type))))
plot(final$per_fr, final$percent_vaccinated,
     main="Free Lunch and Vaccination Rate",
     xlab="Percent Free Lunch",
     ylab="Percent Vaccinated",
     pch=16,
     cex = final$totmem/ 1000,
     col = adjustcolor(type_cols[as.numeric(factor(final$type))], alpha.f = 0.6))
legend(x = "topleft",
       pch = 16,
       col = type_cols,
       legend = levels(factor(final$type)))

#not really much of a trend here

#black 

plot(final$per_black, final$percent_vaccinated,
     main="Percent Black and Vaccination Rate",
     xlab="Percent Black",
     ylab="Percent Vaccinated",
     pch=16,
     cex = final$totmem/ 1000,
     col = adjustcolor(type_cols[as.numeric(factor(final$type))], alpha.f = 0.6))

legend(x = "topleft",
       pch = 16,
       col = type_cols,
       legend = levels(factor(final$type)))
#hispanic 

plot(final$per_hispanic, final$percent_vaccinated,
     main="Percent Hispanic and Vaccination Rate",
     xlab="Percent Hispanic",
     ylab="Percent Vaccinated",
     pch=16,
     col=adjustcolor("blue", alpha.f=0.6))

#asian 

plot(final$per_hispanic, final$percent_vaccinated,
     main="Percent Asian and Vaccination Rate",
     xlab="Percent Asian",
     ylab="Percent Vaccinated",
     pch=16,
     col=adjustcolor("blue", alpha.f=0.6))

#not very interesting with race...

#basic linear model 

mod <- lm(number_vaccinated ~ 
            totmem + 
            type + 
            per_black +
            #per_hispanic + 
            #per_asian + 
            per_fr, 
          data=final)
summary(mod)

# MAKE VISUALS OF PREDICTIONS
fake <- expand.grid(totmem = 1000,
                    type = unique(final$type),
                    per_black = 0:100,
                    per_fr = 0:100)
fake$predicted <- predict(mod, newdata = fake)
fake_int <- data.frame(predict(object = mod,
                               interval = "prediction",
                               newdata = fake,
                               level = 0.95))
fake$lwr <- fake_int$lwr
fake$upr <- fake_int$upr

# Plot fake predictions
plot_fake <- function(per_black = 20,
                      type = "elem",
                      col = "red",
                      lwd = 3, 
                      add = FALSE,
                      ylim = c(0, 200)){
  col <- adjustcolor(col, alpha.f = 0.6)
  df <- fake[which(fake$per_black == per_black &
                     fake$type == type),]
  if(add){
    lines(df$per_fr, df$predicted, type = "l", col = col, lwd = lwd)
    lines(df$per_fr, df$lwr, col = col, lwd = 1, lty = 3)
    lines(df$per_fr, df$upr, col = col, lwd = 1, lty = 3)
    
  } else{
    plot(df$per_fr, df$predicted, type = "l", col = col, lwd = lwd,
         ylim = ylim)
    lines(df$per_fr, df$lwr, col = col, lwd = 1, lty = 3)
    lines(df$per_fr, df$upr, col = col, lwd = 1, lty = 3)
    
  }
}
plot_fake()
plot_fake(type = "mid", add = TRUE, col = "green")
plot_fake(type = "high", add = TRUE, col = "blue")

# PREDICT OVER ALL THE SCHOOLS
school$predicted <- predict(mod, newdata = school)
school_int <- data.frame(predict(object = mod,
                               interval = "prediction",
                               newdata = school,
                               level = 0.95))
school$lwr <- school_int$lwr
school$upr <- school_int$upr

# Create predicted RATES
school$predicted_rate <- school$predicted / school$totmem * 100
school$lwr_rate <- school$lwr / school$totmem * 100
school$upr_rate <- school$upr / school$totmem * 100

# Order schools by predicted imm rate
school <- school[rev(order(school$predicted_rate)),]

# Remove NAs
school <- school[which(!is.na(school$predicted_rate)),]

# Remove those with screwy denominators (ie, predicted rate greater than denom)
school <- school[which(school$predicted_rate < 100),]

#predict 

final$predicted <- predict(mod, 
                            newdata = final)
summary(final$predicted)
head(final, n = 10)


# prediction_intervals
prediction_intervals <- data.frame(predict(object = mod,
                                           interval = "prediction",
                                           newdata = final,
                                           level = 0.95))

#create columns for lower and upper bound of confidence intervals
final$lwr <- prediction_intervals$lwr
final$upr <- prediction_intervals$upr

#create columns to show if we over or under estimate the immunization rate
final$undest <- ifelse(final$percent_vaccinated > final$upr, TRUE, FALSE)
final$overest <- ifelse(final$percent_vaccinated < final$lwr, TRUE, FALSE)

#the confidence intervals are pretty large... 

#we seem to underestimate the real level of vaccination.
#the red observations are outside are CI, which is already pretty large.
my_colors <- adjustcolor(ifelse(final$undest, "darkred", "black"), alpha.f = 0.2)
plot(x = final$predicted,
     y = final$percent_vaccinated,
     col = my_colors,
     pch = 16)

#we start to overestimate at higher levels of vacination. 
my_colors <- adjustcolor(ifelse(final$overest, "darkred", "black"), alpha.f = 0.2)
plot(x = final$predicted,
     y = final$percent_vaccinated,
     col = my_colors,
     pch = 16)

#together- at higher levels of vaccination the model isn't great..
my_colors <- adjustcolor(ifelse(final$undest, "darkred",
                                ifelse(final$overest, "darkred", "darkgreen")), alpha.f = 0.4)

axes_sc<-pretty(c(0,40))
plot(x = final$predicted,
     y = final$percent_vaccinated,
     col = my_colors,
     pch = 16,
     main="Accuracy of Prediction Model",
     xlab="Predicted Immunization Rate",
     ylab="Observed Immunization Rate",
     xlim=c(0,max(axes_sc)),
     ylim=c(0,max(axes_sc)),
     yaxt="n",
     axes=F
)
axis(1,at=axes_sc)
axis(2,at=axes_sc,las=1)
lines(1,1)
