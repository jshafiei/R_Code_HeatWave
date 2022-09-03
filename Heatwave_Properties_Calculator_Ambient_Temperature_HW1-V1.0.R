# Goal: This script calculates the properties of Heatwave based on HW1 family definition, which is as follows:
#At least two consecutive days with both maximum and minimum temperatures more than X percentile
#of historical maximum and minimum data; X= 85, 90, 95, and 99.
# Required data: minimum and maximum daily temperatures for every day during study time
# Results: Different components of heatwave during the study time. These components are:

#Number of Hot Days
#Severity of hot days (Above minimum)
#Severity of hot days (Above maximum)
#Number of Heatwaves
#Total length of heatwaves
#Longest heatwaves
#Severity of heatwaves(Above minimum)
#Severity of heatwaves(Above maximum)
#First day for heatwave 
#Last day for heatwave


#Other properties can be calculated by abovementioned properties. For example, duration=last day - first day
#-------------------------------
#Version=V1.0
#Author: Javad Shafiei Shiva
#Affliation:Civil and Environmental Engineering Department, Syracuse University
#jshafiei@syr.edu
#315-925-9198
#Origin Date:December 2017
#Last Update: 7/19/2018
#-------------------------------



#To run this code please follow these steps:

# STEP 1:

#The user need to create a .csv file, in which the number of following columns include the mentioned data. The data should 
#be prepared in daily steps, starting first day of each year until last day of last year. This dataset has been provided for 10 cities in the main folder

#Column1: Date as: M/D/Y
#Column2: Maximum daily temperature (C or F)
#Column3: Average daily temperature (C or F)
#Column4: Minimum daily temperature (C or F)
#Column5: Average daily Dew Point (%) : Optional and will not be used in this version of code
#Column6: Minimum daily Dew Point (%) : Optional and will not be used in this version of code
#Column7: Minimum daily Dew Point (%) : Optional and will not be used in this version ofcode
#Column8:Date as: Maximum daily Humidity (%) : Optional and will not be used in this version of code
#Column9: Average daily Humidity (%) : Optional and will not be used in this version of code
#Column10: Minimum daily Humidity (%) : Optional and will not be used in this version of code
#Column11: Average Daily Apparent Temperature (C or F) : Optional and will not be used in this version of code : Will be used in the another code to calculate Heatwave 
#based on average apparent temperature (Not provided here)!


# STEP 2:

# Navigate the working directory to the folder contained .csv files

# STEP 3:
#Run the code. In this example, there is a pre-defined working directory, that SHOULD be modified based on the user's Working Directory. Please check the Tips (??) for each step of code:

# ?? : Reading Data and Setting Quantile value (.85,.9,.95, and .99): Based on your need, the used should manualy set the "Q"
setwd("H:/PhD/PhD/Papers/2- All Heatwaves Are Local/Earth's Future/Input_Temperature_Humidity")

rm(list=ls(all=TRUE))
  start.time <- Sys.time()


# In this example, I have named each .csv file as : City name_start year-end year_Temperature_Humidity
   Data<-read.csv(file="Dallas_1950-2016_Temperature_Humidity.csv",stringsAsFactors = FALSE)
 
#?? : Insert the Q as 0.Q==> 85th Percentile==> 0.85
   
        Q<-.85

#Initial Computation of number of days and etc.... "Days_Real": Real number of days in each year

# ?? : I calculate real number of days in each year to double check the availability of data FOR EACH DAY in .csv file
      
yearLength <- function(year) 365 + (year %% 4 == 0)
Begin=1950
End=2016
Duration=End-Begin+1
attach(Data)
Dates <- as.POSIXlt(Date, format="%m/%d/%Y")
Year<-Dates$year+1900
Days_Real<-yearLength(Begin:End)

#Counting number of available days in data for each year Duration "Days"

YearC<-matrix(nrow=Duration, ncol=2,0)
YearC[,1]<-c(Begin:End)
for (N in 1:Duration) {        
  for (i in 1:length(Year)) {
    if (Year[i]==Begin-1+N) 
    { (YearC[N,2]<-YearC[N,2]+1)}
    
  }
}
Days<-YearC[,2]

# Calculation of Heat .... based on the Tmax and Tmin
# ?? :HWC is a matrix, defined for each "Q"

HWC<-matrix(nrow=Duration, ncol=15,0)
HWC[,1]<-c(Begin:End)
HWC[,2]<-Days
HWC[,13]<-Days_Real
HWC[1,11]<-1
HWC[1,12]<-Days[1]

# ??: First and last days for each matrix is the culumative number of days (of first and last days) for years in row

#Counter the first day of year N in the raw = (HWC[N,11])
#Counter the last day of year N in the raw = (HWC[N,12])

for (N in 2:Duration) {
  (HWC[N,12]<-HWC[N-1,12]+Days[N])
}
for (N in 2:Duration) {
  (HWC[N,11]<-HWC[N-1,12]+1)
}
Data[,11]<-as.numeric(Data[,11])
Min<-quantile(Data[,4], Q)
Max<-quantile(Data[,2], Q)
Ave<-quantile(Data[,11], Q)

#Counting Hot Days= (HWC[N,3])= Both Tmin and Tmax exceed the thresholds
#Severity has been defined as the amount of temperature above maximum threshold + amount of temperature above minimum threshold
#?? : A heatwave including 2 days with maximum and minimum daily temperatures equal to 30,25,32,26 and threshold in that region equal to 28 and 24 ==>
# The Severity (or Intensity) = (30-28)+(25-24)+(32-28)+(26-24)=2+1+4+2=9

#Counting Severity for each individual hot day (Above minimum)= (HWC[N,4])
#Counting Severity for each individual hot day (Above Maximum)= (HWC[N,5])
#Number of Heatwaves = (HWC[N,6])
#Total length of heatwaves = (HWC[N,7])
# ?? : Total length of heatwaves are calculated in each calendar year

for (N in 1:Duration) 
{ 
  { 
    HD<-numeric(HWC[N,12]-HWC[N,11]+1)     
    for (i in HWC[N,11]:HWC[N,12]) {
      
      if ((Data[i,4]>= Min) && (Data[i,2]>= Max)) {
        (HD[i-HWC[N,11]+1]<-1) &&
          ((HWC[N,3]<-HWC[N,3]+1))&&
          ((HWC[N,4]<-HWC[N,4]+(Data[i,4]- Min)))&&
          ((HWC[N,5]<-HWC[N,5]+(Data[i,2]- Max)))
              }
            HD<-c(HD)
      D<-rle(HD)$lengths[rle(HD)$values==1]
      
      # Number of Heatwaves = (HWC[N,6])
      
      HWC[N,6]<-sum (rle(HD)$lengths[rle(HD)$values==1]>=2)
      
      # TOtal length of heatwaves = (HWC[N,7])
      
      HWC[N,7]<-sum(D)-sum(D<2)
      
      #HWC[,14]=First day for heatwave based on (Tmix,max)
      #HWC[,15]=Last day for heatwave based on (Tmix,max)
      
      runs = rle(HD > 0)
      myruns = which(runs$values == TRUE & runs$lengths >= 2)
      runs.lengths.cumsum = cumsum(runs$lengths)
      ends = runs.lengths.cumsum[myruns]
      newindex = ifelse(myruns>1, myruns-1, 0)
      starts = runs.lengths.cumsum[newindex] + 1
      if (0 %in% newindex) starts = c(1,starts)
      
            if ((length(starts))==0) {(HWC[N,14]<-0)
      } else {
        (HWC[N,14]<-starts[1])
      }
      
            if ((length(ends))==0) {(HWC[N,15]<-0)
      } else {
        (HWC[N,15]<-ends[(length(ends))])
      }
    }
  }
  #Deleting HD and D from the memory for next calculation
  rm(HD,D)
}

#longest heatwaves day = (HWC[N,8])
for (N in 1:Duration) 
{
  HD<-numeric(HWC[N,12]-HWC[N,11]+1)     
  for (i in HWC[N,11]:HWC[N,12]) {
    if ((Data[i,4]>= Min) && (Data[i,2]>= Max)) {
      ((HD[i-HWC[N,11]+1]<-1))
      
    }
  }
  #longest heatwaves day = (HWC[N,8])
  HWC[N,8]<-max(((rle(HD)$lengths[rle(HD)$values==1])),0)
  #Deleting HD and D from the memory for next calculation
  rm(HD)
}
#Counting Severity for each individual heatwave event(Above minimum) and then accumulating these values = (HWC[N,9])
#Counting Severity for each individual heatwave event(Above maximum) and then accumulating these values= (HWC[N,10])

for (N in 1:Duration)
{
  for (i in HWC[N,11]:HWC[N,12]) 
  {
    if (i==1)
    {if (
      (Data[i,4]>= Min & Data[i,2]>= Max) && 
      ((Data[i+1,4]>= Min && Data[i+1,2]>= Max))
    ) 
    {((HWC[N,9]<-HWC[N,9]+Data[i,4]- Min) && (HWC[N,10]<-HWC[N,10]+Data[i,2]- Max))}
    }
    
        if (i!=1 && i!=length (Data[,1]))
    {if (
      (Data[i,4]>= Min & Data[i,2]>= Max) && 
      ((Data[i+1,4]>= Min && Data[i+1,2]>= Max)|(Data[i-1,4]>= Min && Data[i-1,2]>= Max))
    ) 
    {((HWC[N,9]<-(HWC[N,9]+Data[i,4]- Min)) && (HWC[N,10]<-(HWC[N,10]+Data[i,2]- Max)))}
    }
    
    if (i==length (Data[,1]))
    {if (
      (Data[i,4]>= Min & Data[i,2]>= Max) && 
      ((Data[i-1,4]>= Min && Data[i-1,2]>= Max))
    ) 
    {((HWC[N,9]<-HWC[N,9]+Data[i,4]- Min) && (HWC[N,10]<-HWC[N,10]+Data[i,2]- Max))}
    }
      }  
}

#?? : As a summary, check the following definition for each column of matrix for the heatwave properties definitions

#|||||||||||||||||||||||||||||||||||||||||||||||| Definitions ||||||||||||||||||||||||||||||||||||||||||||||||

#HWC[,1]=Year
#HWC[,2]=Days_Data
#HWC[,3]=Number of Hot Days(Tmax,max)
#HWC[,4]=Severity of hot days (Above minimum)
#HWC[,5]=Severity of hot days (Above maximum)
#HWC[,6]=Number of Heatwaves(Tmax,max)
#HWC[,7]=Total length of heatwaves(Tmax,max)
#HWC[,8]=Longest heatwaves(Tmax,max,day)
#HWC[,9]=Severity of heatwaves(Above minimum)
#HWC[,10]=Severity of heatwaves(Above maximum)
#HWC[,11]=Counter for the first day of year N in the raw 
#HWC[,12]=Counter for the last day of year N in the raw 
#HWC[,13]=Days_Real
#HWC[,14]=First day for heatwave based on (Tmax,max)
##HWC[,15]=Last day for heatwave based on (Tmax,max)


# STEP 3:
#?? : Creating output file
#---------------- OUTPUT FILE ---------------- # ---------------- OUTPUT FILE ---------------- #---------------- OUTPUT FILE ---------------- #
Output_HW01<-data.frame(HWC[,1],HWC[,3],HWC[,4],HWC[,5],HWC[,6],HWC[,7],HWC[,8],HWC[,9],HWC[,10],HWC[,14],HWC[,15])
colnames(Output_HW01) <- c("Year","Number of Hot Days","Severity of hot days (Above minimum)",
                            "Severity of hot days (Above maximum)","Number of Heatwaves",
                            "Total length of heatwaves","Longest heatwaves",
                            "Severity of heatwaves(Above minimum)","Severity of heatwaves(Above maximum)",
                            "First day of heatwave" ,"Last day of heatwave")
#?? : Writing table. For each Q, this code will create one output matrix. There is another code in the folder, named "Creating_Heatwave_Properties_Single_File_HW_City"
#Run this code to make one file based on 5 different matrix based on (Q=85, Q=90, Q=95, and Q=99)
#write.table(Output_HW01,"DAL_HW01A.out",quote=FALSE,append=FALSE,sep=" | ",row.names=FALSE)

write.table(Output_HW01,file="DAL_HW01A.csv",row.names=FALSE,qmethod = "double")

# Naming!
#?? :
# The heatwave based on HW1 Definiton (Check the start of the script for guide) and different Q values will be entitles as follows:
#Q=85 ==> HW1A
#Q=90 ==> HW1B
#Q=95 ==> HW1C
#Q=99==> HW1D


finish.time <- Sys.time()
Total.Time<-finish.time-start.time
Total.Time
#The output matrix file will be created as .csv file in the same working directory. The user can change the directory!
# You are almost done, the code will show the total time. For 67 year (tested period) on an average/good machine (i7, 3.4 GHz, Ram=16), the time will be a few minutes.

