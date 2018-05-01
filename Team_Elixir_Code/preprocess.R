#preprocessing: 

#libraries needed:

library(plyr)
library(mice)
library(zoo)
library(readxl)
library(readr)

#datasets needed
summer <- read_csv("/Team_Elixir_Dataset/summer.csv")
IOC_Code <- read_excel("/Team_Elixir_Dataset/IOC_Code.xlsx")
GDP_all <- read_excel("/Team_Elixir_Dataset/GDP_all.xlsx")


#Getting data seperately for each country
mytable<-xtabs(~Year+Country, data = summer)
View(mytable)

#binding the previous years perfomance
mytable$Medal_Count->lst
initial <- 0
as.data.frame(lst)->lst
rbind(initial,lst)->lst
cbind(mytable,lst[1:3969,])->mytable
names(mytable)<-c("Year","Country","Medal_Count","prev_perf")

#setting intial performances to 0 i.e for the year 1896 (no previous data - first olympics)
for(c in mytable$Year){
  if(c == 1896)
    mytable[c,4]<-0
}

#adding the country names column to the dataset. 
names(mytable)[names(mytable) == "Country"] <- "IOC_code"
IOC_Code <- subset(IOC_Code , select = c(Country,`Int Olympic Committee code`))
names(IOC_Code)[names(IOC_Code) == "Int Olympic Committee code"] <- "IOC_code"
Finall<- merge(IOC_Code,mytable,by="IOC_code")

write.xlsx(mytable,"~/5thSEM/Team_Elixir_Dataset/Medals_country.xlsx")

#==================================================================