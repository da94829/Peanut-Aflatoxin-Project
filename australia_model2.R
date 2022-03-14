library(dplyr)
library(tibble)
library(tidyverse)
library(data.table)
data <-read.csv("C:\\Users\\da94_\\OneDrive - University of Florida\\PEANUT PROJECT\\AustraliaModelData\\irrigation_Model_DK.csv", header = T)
names(data) <-c('Date', 'day', 'tmn', 'tmx', 'Water', 'ET')
AWC <- 0.1
head(data)
#___________________________________________
# Set some funtions
#___________________________________________
#environment


tmean<-function(tmx,tmn){
  tmean<-(tmx+tmn)/2
  return(tmean)
}


DailyGDD <-function(tmn,tmx){
  
  DailyGDD <- (tmn+tmx)/2 - 13.3
  
  return(DailyGDD)
}

CorrectedGDD <-function(tmn,tmx,DailyGDD){
  
  if(DailyGDD >= 0) CorrectedGDD <- DailyGDD else CorrectedGDD <- 0
  return(CorrectedGDD)
}

cumGDDs <-function(tmn,tmx,CorrectedGDD){
  
  cumGDDs <- cumsum(CorrectedGDD)  
  return(cumGDDs)
}


PAW<-function(tmn,tmx,CorrectedGDD,AWC){
  cumGDDs1 <- min(cumGDDs(tmn,tmx,CorrectedGDD))
  if(cumGDDs < 750)   rd<- 12+(36-12)*(cumGDDs-cumGDDs1)/(750-cumGDDs1) else rd <-36
  paw <- AWC * rd
  return(paw)
}


KcCurve <- function(tmn,tmx,CorrectedGDD, cumGDDs){
  
  if(cumGDDs >= 0 | cumGDDs < 325)   kc <- 0.3
  else if(cumGDDs >= 325 | cumGDDs < 800)   kc <- 0.3+((cumGDDs-325)/475)*(1-0.3)
  else if(cumGDDs >= 800 | cumGDDs < 1350)   kc <- 1
  else if(cumGDDs >= 1350 | cumGDDs < 1650)   kc <- 1+((cumGDDs-1350)/300)*(0.6-1)
  else if(cumGDDs >= 1650 | cumGDDs < 1700)   kc <- 0.6+((cumGDDs-1650)/50)*(0.2-0.4)
  else if(cumGDDs >= 1700) kc <- 0.2
  return(kc)
  
}

fswb1 <- function(ET,Water,paw,kc,){
  PAW(tmn,tmx,CorrectedGDD,AWC)
  fswb <- paw-(ET*kc)+Water
  return(fswb)
  
}

swb1 <- function(paw,fswb1){
  if(fswb1>paw) swb<- paw else swb<-fswb1
  return(swb)
  
}
fswb <- function(ET,Water){ 
  
  rowShift <- function(x, shiftLen = 1L) {
    r <- (1L + shiftLen):(length(x) + shiftLen)
    r[r<1] <- NA
    return(x[r])
  }
  max(0,rowShift(swb1())-(ET*kc)+Water)
}



myModel<-function(localEnvironment,day_data,localManagement1, localManagement2){
  
  #Describe the system
  
  #Environment
  env<-read.table(localEnvironment, header = T, sep=',')
  environment<-env[env$day == day_data,]
  
  #Management
  planting_date <- day_data #start
  dap <- localManagement1 #Days after planting, depands on variety
  awc <-localManagement2 #soil type
  
  #Initialize variables
  day<-planting_date # the date as a number
  dap<- 0
  soil_temp<-0
  cum_gdd<-0
  
  #Define start (planting) and stop (exit criteria)
  
  while(day>= planting_date){
    
    #calculate rates or daily values
    daily_tmx<-environment$tmx[environment$day==day]  # the day of the simulation
    daily_tmn<-environment$tmn[environment$day==day]
    daily_water<-environment$Water[environment$day==day]
    daily_et<-environment$ET[environment$day==day]
    
    
    daily_gdd<-DailyGDD(daily_tmn,daily_tmx)
    daily_correctedgdd<-CorrectedGDD(daily_tmn,daily_tmx,daily_gdd)
    daily_tmean<-tmean(daily_tmn,daily_tmx) #differential form in the future
    daily_paw<-PAW(daily_tmn,daily_tmx,daily_correctedgdd,awc)
    first_fswb<-fswb1(daily_et,daily_water)
    first_swb<-swb1(daily_paw,first_fswb)
    daily_kc<-KcCurve(daily_tmn,daily_tmx,daily_correctedgdd,)
    today_fswb<-fswb(daily_et,daily_water)
    
    #integrate rates or daily values
    s_tmean<-0.2842 * daily_tmean^1.358
    cumGDDs<-cumGDDs + daily_gdd
    if(day == planting_date) fwb <- first_fswb else fwb <- today_fswb
    today_adt<-daily_paw*0.2
    if(daily_paw<22 | daily_paw>35) today_atf<-0 
    else if(daily_paw>21 && daily_paw<31) today_atf<- 3*(daily_paw-22)/(30-22)
    else if(daily_paw>29 && daily_paw<36) today_atf<- 3*(35-daily_paw)/(35-30)
    
    if(first_fswb<today_adt) daily_atf20 <- today_atf else daily_atf20 <- 0
    
    #record values
    if(fswb<(0.51*daily_paw)) irrigation_recommendation <- "irrigate"
    else if(fswb<(0.6*daily_paw)) irrigation_recommendation <- "check field"
    else irrigation_recommendation <- "adequate soil moisture"
    
    daily_out<-c(day, s_tmean, cumGDDs, fwb, today_adt, today_atf, daily_atf20, irrigation_recommendation)
    if(doy == planting_date) output<- daily_out else output<- rbind(output, daily_out)
    
    
    #report & exit
    if(planting_date == planting_date + localManagement1){
      output<-as.data.frame(output)
      names(output)<-list("doy", "SoilTmean","cumGDDs", "FinalWaterBalance","Threshole","ATF","ATF20", "IrrigationRecommendation")
      break
    }
    
    #Advance to next day
    day <- day + 1
  } #while loop 
  return(output)
  write.csv(output, "output.csv", row.names = F,)
}


weather<-"C:\\Users\\da94_\\OneDrive - University of Florida\\PEANUT PROJECT\\AustraliaModelData\\irrigation_Model_DK.csv"
day<-43951
DAP<-199
AWC<-0.1
myModel(weather,day,DAP,AWC) 
head(output)
