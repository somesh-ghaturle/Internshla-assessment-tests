
############################
##Aggregating Data to 5MIN##
############################

##CONDITIONS##

#Condition1-In one timeblock if there are no missing values,aggregate by mean

#Codition2-In one timeblock if there is atleast one missing values,aggregate by mean,na.rm=T

#Condition3- For character values input the last obsevation of the series


##Genral Note- Data is from October 20th,2018 to Novermber 19th,2019


## Load Files Of Inverter

p_inverter <- list.files("/Users/somesh/Downloads/cleantech_solar/Raw",pattern = "-I.*.txt",full.names = TRUE,recursive = TRUE) 

## Load Files of WMS

p_wms <- list.files("/Users/somesh/Downloads/cleantech_solar/Raw",pattern = "WMS.*.txt",full.names = TRUE,recursive = TRUE) 

p_wms_check <- list.files("/Users/somesh/Downloads/cleantech_solar/Gen1",pattern = "WMS.*.txt",full.names = TRUE,recursive = TRUE) 

## Load files of MFM

p_mfm <- list.files("/Users/somesh/Downloads/cleantech_solar/Raw",pattern = "MFM.*.txt",full.names = TRUE,recursive = TRUE) 


###Load Packages###


library(lubridate)

library(data.table)

library(dplyr)



##Function To Aggregate Data To 5 Min Interval##


aggregating_fun <- function(var,Tstamp_column){
  
  l <- lapply(var,fread,sep="\t",stringsAsFactors=FALSE)
  
  df <- rbindlist(l)
  
  df <- data.frame(df)
  
  #Converitng to R readable timestamp
  
  ts <- df[,Tstamp_column]
    
  ts <- as.POSIXct(ts,"%Y-%m-%d %H:%M:%S")
  
  df <- cbind(df,ts)
  
  colnames(df)[colnames(df)=="ts"] <-"Tstamp" 
  
  # Rounding to minutes
  
  df$Tstamp <- round_date(df$Tstamp,"minute")
  
  ##Remove Columns With Entire NA'S
  
  df_clean <- df[ , ! apply(df , 2 , function(x) all(is.na(x)) ) ]
  
  ## Round to zero only the first timestamp
  
  df_clean$Tstamp[1] <- df_clean$Tstamp[1]-60
  
  df_agg <- df_clean %>% group_by(Tstamp = cut(Tstamp, breaks="5 min")) %>% summarise_all(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else last(.))) 
  
  df_agg$DAY <- day(df_agg$Tstamp)
  
  df_agg$MONTH <- month(df_agg$Tstamp)
  
  df_agg$YEAR <- year(df_agg$Tstamp)
  
  df_agg$DM <- paste(df_agg$DAY,df_agg$MONTH,sep="_")
  
  df_agg$YM <- paste(df_agg$YEAR,df_agg$MONTH,sep="-")
  
return(df_agg)  
  
  
}  
  
##Create Aggregated Data Frames For Each Station


df_wms_agg <-aggregating_fun(p_wms,"w23")

df_mfm_agg <-aggregating_fun(p_mfm,"m63")

df_inverter_agg <- aggregating_fun(p_inverter,"i32")



##Set Working Directory For WMS,MFM,INVERTER Files To Be Exported##

## The total nmber of days are 30 so 30 files in each 

setwd("E:/GEN-1")


for(i in unique(df_inverter_agg$DM)){
  df_output <- df_inverter_agg[df_inverter_agg$DM==i,]
  Y <- unique(df_output$YEAR)[1]
  write.table(df_output, paste("IN-023C","-",Y,"-",i,"-","INVERTER",".txt",sep=""))
}




