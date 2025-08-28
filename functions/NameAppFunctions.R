library(tidyverse)
library(gridExtra)
#setwd("C:/Users/jason/Desktop/NamePopularityApp/functions")

load("StateData")
load("NationalData")
StatesData=NAME_DATA_STATES


NationalGraphFunction=function(location,n,year,sex){
  if(location== "US"){
    if(sex == "F"){
      ggplot(data = filter(NameDataUS, YEAR==year & SEX=="F")[1:n,],aes(x=      reorder(NAME,+PERCENTAGE),y=PERCENTAGE))+geom_bar(stat="identity",fill="pink",color="black")+coord_flip()+ylab("Percent of Babies Born")+xlab("Name")+ggtitle(paste("Top",n,"Girl Names in US in",year))
      
    }
    else if(sex == "M"){
      ggplot(data = filter(NameDataUS, YEAR==year & SEX=="M")[1:n,],aes(x= reorder(NAME,+PERCENTAGE),y=PERCENTAGE))+geom_bar(stat="identity",fill="deepskyblue4",color="black")+coord_flip()+ylab("Percent of Babies Born")+ xlab("Name")+ggtitle(paste("Top",n,"Boy Names in US in",year))
    }
  }
  
  
  else{
    if(sex == "F"){
      ggplot(data = filter(StatesData, YEAR==year & SEX=="F" & STATE==location)[1:n,],aes(x=      reorder(NAME,+PERCENTAGE),y=PERCENTAGE))+geom_bar(stat="identity",fill="pink",color="black")+coord_flip()+ylab("Percent of Babies Born")+xlab("Name")+ggtitle(paste("Top",n,"Girl Names in",location,"in", year))
      
    }
    else if(sex == "M"){
      ggplot(data = filter(StatesData, YEAR==year & SEX=="M" & STATE==location)[1:n,],aes(x= reorder(NAME,+PERCENTAGE),y=PERCENTAGE))+geom_bar(stat="identity",fill="deepskyblue4",color="black")+coord_flip()+ylab("Percent of Babies Born")+ xlab("Name")+ggtitle(paste("Top",n,"Boy Names in",location,"in", year))
    }
    
    
    
    
  }
}





FormatFunction=function(STRING){
  STRING1=toupper(substr(STRING, 1,1))
  STRING2=tolower(substr(STRING,2,str_length(STRING)))
  String=paste(STRING1,STRING2,sep = "")
  return(String)
}


  

PopularityOverTimeFunction=function(Location,Name1,Name2=NULL,Name3=NULL,Name4=NULL,Name5=NULL,Name6=NULL,Name7=NULL,Name8=NULL,LowerYear,UpperYear,sex){
  
  if(Location=="US"){
    
    
    if(sex=="M"){
      if(is.null(Name1)==FALSE & is.null(Name2)==TRUE & is.null(Name3)==TRUE & is.null(Name4)==TRUE & is.null(Name5)==TRUE & is.null(Name6)==TRUE & is.null(Name7)==TRUE & is.null(Name8)==TRUE   ){
        
        ggplot(data = filter(NameDataUS, NAME==FormatFunction(Name1)  & SEX=="M" & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Boy Babies With Name")+ggtitle("Popularity of Boy Names in US")
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==TRUE & is.null(Name4)==TRUE & is.null(Name5)==TRUE & is.null(Name6)==TRUE & is.null(Name7)==TRUE & is.null(Name8)==TRUE ){
        ggplot(data = filter(NameDataUS, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2)) & SEX=="M" & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Boy Babies With Name")+ggtitle("Popularity of Boy Names in US")
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==TRUE & is.null(Name5)==TRUE & is.null(Name6)==TRUE & is.null(Name7)==TRUE & is.null(Name8)==TRUE ){
        ggplot(data = filter(NameDataUS, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3)) & SEX=="M" & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Boy Babies With Name")+ggtitle("Popularity of Boy Names in US")
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE & is.null(Name5)==TRUE & is.null(Name6)==TRUE & is.null(Name7)==TRUE & is.null(Name8)==TRUE ){
        ggplot(data = filter(NameDataUS, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3) | NAME==FormatFunction(Name4)) & SEX=="M" & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Boy Babies With Name")+ggtitle("Popularity of Boy Names in US")
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE & is.null(Name5)==FALSE & is.null(Name6)==TRUE & is.null(Name7)==TRUE & is.null(Name8)==TRUE ){
        ggplot(data = filter(NameDataUS, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3) | NAME==FormatFunction(Name4) | NAME==FormatFunction(Name5)) & SEX=="M" & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Boy Babies With Name")+ggtitle("Popularity of Boy Names in US")
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE & is.null(Name5)==FALSE & is.null(Name6)==FALSE & is.null(Name7)==TRUE & is.null(Name8)==TRUE ){
        ggplot(data = filter(NameDataUS, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3) | NAME==FormatFunction(Name4) | NAME==FormatFunction(Name5) | NAME==FormatFunction(Name6)) & SEX=="M" & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Boy Babies With Name")+ggtitle("Popularity of Boy Names in US")
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE & is.null(Name5)==FALSE & is.null(Name6)==FALSE & is.null(Name7)==FALSE & is.null(Name8)==TRUE ){
        ggplot(data = filter(NameDataUS, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3) | NAME==FormatFunction(Name4) | NAME==FormatFunction(Name5) | NAME==FormatFunction(Name6) | NAME==FormatFunction(Name7)) & SEX=="M" & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Boy Babies With Name")+ggtitle("Popularity of Boy Names in US")
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE & is.null(Name5)==FALSE & is.null(Name6)==FALSE & is.null(Name7)==FALSE & is.null(Name8)==FALSE ){
        ggplot(data = filter(NameDataUS, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3) | NAME==FormatFunction(Name4) | NAME==FormatFunction(Name5) | NAME==FormatFunction(Name6) | NAME==FormatFunction(Name7) | NAME==FormatFunction(Name8)) & SEX=="M" & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Boy Babies With Name")+ggtitle("Popularity of Boy Names in US")
        
      }
      
      
    }
    else{
      
      if(is.null(Name1)==FALSE & is.null(Name2)==TRUE & is.null(Name3)==TRUE & is.null(Name4)==TRUE & is.null(Name5)==TRUE & is.null(Name6)==TRUE & is.null(Name7)==TRUE & is.null(Name8)==TRUE   ){
        ggplot(data = filter(NameDataUS, NAME==FormatFunction(Name1)  & SEX=="F" & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Girl Babies With Name")+ggtitle("Popularity of Girl Names in US")
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==TRUE & is.null(Name4)==TRUE & is.null(Name5)==TRUE & is.null(Name6)==TRUE & is.null(Name7)==TRUE & is.null(Name8)==TRUE ){
        ggplot(data = filter(NameDataUS, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2)) & SEX=="F" & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Girl Babies With Name")+ggtitle("Popularity of Girl Names in US")
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==TRUE & is.null(Name5)==TRUE & is.null(Name6)==TRUE & is.null(Name7)==TRUE & is.null(Name8)==TRUE ){
        ggplot(data = filter(NameDataUS, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3)) & SEX=="F" & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Girl Babies With Name")+ggtitle("Popularity of Girl Names in US")
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE & is.null(Name5)==TRUE & is.null(Name6)==TRUE & is.null(Name7)==TRUE & is.null(Name8)==TRUE ){
        ggplot(data = filter(NameDataUS, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3) | NAME==FormatFunction(Name4)) & SEX=="F" & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Girl Babies With Name")+ggtitle("Popularity of Girl Names in US")
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE & is.null(Name5)==FALSE & is.null(Name6)==TRUE & is.null(Name7)==TRUE & is.null(Name8)==TRUE ){
        ggplot(data = filter(NameDataUS, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3) | NAME==FormatFunction(Name4) | NAME==FormatFunction(Name5)) & SEX=="F" & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Girl Babies With Name")+ggtitle("Popularity of Girl Names in US")
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE & is.null(Name5)==FALSE & is.null(Name6)==FALSE & is.null(Name7)==TRUE & is.null(Name8)==TRUE ){
        ggplot(data = filter(NameDataUS, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3) | NAME==FormatFunction(Name4) | NAME==FormatFunction(Name5) | NAME==FormatFunction(Name6)) & SEX=="F" & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Girl Babies With Name")+ggtitle("Popularity of Girl Names in US")
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE & is.null(Name5)==FALSE & is.null(Name6)==FALSE & is.null(Name7)==FALSE & is.null(Name8)==TRUE ){
        ggplot(data = filter(NameDataUS, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3) | NAME==FormatFunction(Name4) | NAME==FormatFunction(Name5) | NAME==FormatFunction(Name6) | NAME==FormatFunction(Name7)) & SEX=="F" & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Girl Babies With Name")+ggtitle("Popularity of Girl Names in US")
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE & is.null(Name5)==FALSE & is.null(Name6)==FALSE & is.null(Name7)==FALSE & is.null(Name8)==FALSE ){
        ggplot(data = filter(NameDataUS, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3) | NAME==FormatFunction(Name4) | NAME==FormatFunction(Name5) | NAME==FormatFunction(Name6) | NAME==FormatFunction(Name7) | NAME==FormatFunction(Name8)) & SEX=="F" & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Girl Babies With Name")+ggtitle("Popularity of Girl Names in US")
        
      }
      
      
      
      
      
      
    }
    
    
  } 
  else{ 
    if(sex=="M"){
      if(is.null(Name1)==FALSE & is.null(Name2)==TRUE & is.null(Name3)==TRUE & is.null(Name4)==TRUE & is.null(Name5)==TRUE & is.null(Name6)==TRUE & is.null(Name7)==TRUE & is.null(Name8)==TRUE   ){
        ggplot(data = filter(StatesData, NAME==FormatFunction(Name1)  & SEX=="M" & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Boy Babies With Name")+ggtitle(paste("Popularity of Boy Names in",Location))
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==TRUE & is.null(Name4)==TRUE & is.null(Name5)==TRUE & is.null(Name6)==TRUE & is.null(Name7)==TRUE & is.null(Name8)==TRUE ){
        ggplot(data = filter(StatesData, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2)) & SEX=="M" & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Boy Babies With Name")+ggtitle(paste("Popularity of Boy Names in",Location))
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==TRUE & is.null(Name5)==TRUE & is.null(Name6)==TRUE & is.null(Name7)==TRUE & is.null(Name8)==TRUE ){
        ggplot(data = filter(StatesData, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3)) & SEX=="M" & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Boy Babies With  Name")+ggtitle(paste("Popularity of Boy Names in",Location))
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE & is.null(Name5)==TRUE & is.null(Name6)==TRUE & is.null(Name7)==TRUE & is.null(Name8)==TRUE ){
        ggplot(data = filter(StatesData, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3) | NAME==FormatFunction(Name4)) & SEX=="M" & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Boy Babies With Name")+ggtitle(paste("Popularity of Boy Names in",Location))
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE & is.null(Name5)==FALSE & is.null(Name6)==TRUE & is.null(Name7)==TRUE & is.null(Name8)==TRUE ){
        ggplot(data = filter(StatesData, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3) | NAME==FormatFunction(Name4) | NAME==FormatFunction(Name5)) & SEX=="M" & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Boy Babies With Name")+ggtitle(paste("Popularity of Boy Names in",Location))
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE & is.null(Name5)==FALSE & is.null(Name6)==FALSE & is.null(Name7)==TRUE & is.null(Name8)==TRUE ){
        ggplot(data = filter(StatesData, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3) | NAME==FormatFunction(Name4) | NAME==FormatFunction(Name5) | NAME==FormatFunction(Name6)) & SEX=="M" & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Boy Babies With Name")+ggtitle(paste("Popularity of Boy Names in",Location))
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE & is.null(Name5)==FALSE & is.null(Name6)==FALSE & is.null(Name7)==FALSE & is.null(Name8)==TRUE ){
        ggplot(data = filter(StatesData, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3) | NAME==FormatFunction(Name4) | NAME==FormatFunction(Name5) | NAME==FormatFunction(Name6) | NAME==FormatFunction(Name7)) & SEX=="M" & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Boy Babies With Name")+ggtitle(paste("Popularity of Boy Names in",Location))
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE & is.null(Name5)==FALSE & is.null(Name6)==FALSE & is.null(Name7)==FALSE & is.null(Name8)==FALSE ){
        ggplot(data = filter(StatesData, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3) | NAME==FormatFunction(Name4) | NAME==FormatFunction(Name5) | NAME==FormatFunction(Name6) | NAME==FormatFunction(Name7) | NAME==FormatFunction(Name8)) & SEX=="M" & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Boy Babies With Name")+ggtitle(paste("Popularity of Boy Names in",Location))
        
      }
      
      
    }
    else{
      
      if(is.null(Name1)==FALSE & is.null(Name2)==TRUE & is.null(Name3)==TRUE & is.null(Name4)==TRUE & is.null(Name5)==TRUE & is.null(Name6)==TRUE & is.null(Name7)==TRUE & is.null(Name8)==TRUE   ){
        ggplot(data = filter(StatesData, NAME==FormatFunction(Name1)  & SEX=="F" & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Girl Babies With Name")+ggtitle(paste("Popularity of Girl Names in",Location))
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==TRUE & is.null(Name4)==TRUE & is.null(Name5)==TRUE & is.null(Name6)==TRUE & is.null(Name7)==TRUE & is.null(Name8)==TRUE ){
        ggplot(data = filter(StatesData, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2)) & SEX=="F" & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Girl Babies With Name")+ggtitle(paste("Popularity of Girl Names in",Location))
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==TRUE & is.null(Name5)==TRUE & is.null(Name6)==TRUE & is.null(Name7)==TRUE & is.null(Name8)==TRUE ){
        ggplot(data = filter(StatesData, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3)) & SEX=="F" & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Girl Babies With Name")+ggtitle(paste("Popularity of Girl Names in",Location))
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE & is.null(Name5)==TRUE & is.null(Name6)==TRUE & is.null(Name7)==TRUE & is.null(Name8)==TRUE ){
        ggplot(data = filter(StatesData, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3) | NAME==FormatFunction(Name4)) & SEX=="F" & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Girl Babies With Name")+ggtitle(paste("Popularity of Girl Names in",Location))
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE & is.null(Name5)==FALSE & is.null(Name6)==TRUE & is.null(Name7)==TRUE & is.null(Name8)==TRUE ){
        ggplot(data = filter(StatesData, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3) | NAME==FormatFunction(Name4) | NAME==FormatFunction(Name5)) & SEX=="F" & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Girl Babies With Name")+ggtitle(paste("Popularity of Girl Names in",Location))
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE & is.null(Name5)==FALSE & is.null(Name6)==FALSE & is.null(Name7)==TRUE & is.null(Name8)==TRUE ){
        ggplot(data = filter(StatesData, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3) | NAME==FormatFunction(Name4) | NAME==FormatFunction(Name5) | NAME==FormatFunction(Name6)) & SEX=="F" & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Girl Babies With Name")+ggtitle(paste("Popularity of Girl Names in",Location))
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE & is.null(Name5)==FALSE & is.null(Name6)==FALSE & is.null(Name7)==FALSE & is.null(Name8)==TRUE ){
        ggplot(data = filter(StatesData, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3) | NAME==FormatFunction(Name4) | NAME==FormatFunction(Name5) | NAME==FormatFunction(Name6) | NAME==FormatFunction(Name7)) & SEX=="F" & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Girl Babies With Name")+ggtitle(paste("Popularity of Girl Names in",Location))
        
      }
      else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE & is.null(Name5)==FALSE & is.null(Name6)==FALSE & is.null(Name7)==FALSE & is.null(Name8)==FALSE ){
        ggplot(data = filter(StatesData, (NAME==FormatFunction(Name1) | NAME==FormatFunction(Name2) | NAME==FormatFunction(Name3) | NAME==FormatFunction(Name4) | NAME==FormatFunction(Name5) | NAME==FormatFunction(Name6) | NAME==FormatFunction(Name7) | NAME==FormatFunction(Name8)) & SEX=="F" & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location),aes(x=YEAR,y = PERCENTAGE))+geom_line(aes(color = NAME))+ylab("Percent Of Girl Babies With Name")+ggtitle(paste("Popularity of Girl Names in",Location))
        
      }
      
      
      
      
      
      
    }  
    
    
    
    
    
    
  }
  
  
  
  
  
  
  
  
  
  
}











NationalGraphFunctionComparison=function(location1,location2,n,year,sex){
  if(location1== "US"){
    if(sex == "F"){
      p1=ggplot(data = filter(NameDataUS, YEAR==year & SEX=="F")[1:n,],aes(x=      reorder(NAME,+PERCENTAGE),y=PERCENTAGE))+geom_bar(stat="identity",fill="pink",color="black")+coord_flip()+ylab("Percent of Babies Born")+xlab("Name")+ggtitle(paste("Top",n,"Girl Names in US in",year))
      
    }
    else if(sex == "M"){
      p1=ggplot(data = filter(NameDataUS, YEAR==year & SEX=="M")[1:n,],aes(x= reorder(NAME,+PERCENTAGE),y=PERCENTAGE))+geom_bar(stat="identity",fill="deepskyblue4",color="black")+coord_flip()+ylab("Percent of Babies Born")+ xlab("Name")+ggtitle(paste("Top",n,"Boy Names in US in",year))
    }
  }
  
  
  else{
    if(sex == "F"){
      p1=ggplot(data = filter(StatesData, YEAR==year & SEX=="F" & STATE==location1)[1:n,],aes(x=      reorder(NAME,+PERCENTAGE),y=PERCENTAGE))+geom_bar(stat="identity",fill="pink",color="black")+coord_flip()+ylab("Percent of Babies Born")+xlab("Name")+ggtitle(paste("Top",n,"Girl Names in",location1,"in", year))
      
    }
    else if(sex == "M"){
      p1=ggplot(data = filter(StatesData, YEAR==year & SEX=="M" & STATE==location1)[1:n,],aes(x= reorder(NAME,+PERCENTAGE),y=PERCENTAGE))+geom_bar(stat="identity",fill="deepskyblue4",color="black")+coord_flip()+ylab("Percent of Babies Born")+ xlab("Name")+ggtitle(paste("Top",n,"Boy Names in",location1,"in", year))
    }
    
    
    
    
  }
  if(location2== "US"){
    if(sex == "F"){
      p2=ggplot(data = filter(NameDataUS, YEAR==year & SEX=="F")[1:n,],aes(x=      reorder(NAME,+PERCENTAGE),y=PERCENTAGE))+geom_bar(stat="identity",fill="pink",color="black")+coord_flip()+ylab("Percent of Babies Born")+xlab("Name")+ggtitle(paste("Top",n,"Girl Names in US in",year))
      
    }
    else if(sex == "M"){
      p2=ggplot(data = filter(NameDataUS, YEAR==year & SEX=="M")[1:n,],aes(x= reorder(NAME,+PERCENTAGE),y=PERCENTAGE))+geom_bar(stat="identity",fill="deepskyblue4",color="black")+coord_flip()+ylab("Percent of Babies Born")+ xlab("Name")+ggtitle(paste("Top",n,"Boy Names in US in",year))
    }
  }
  
  
  else{
    if(sex == "F"){
      p2=ggplot(data = filter(StatesData, YEAR==year & SEX=="F" & STATE==location2)[1:n,],aes(x=      reorder(NAME,+PERCENTAGE),y=PERCENTAGE))+geom_bar(stat="identity",fill="pink",color="black")+coord_flip()+ylab("Percent of Babies Born")+xlab("Name")+ggtitle(paste("Top",n,"Girl Names in",location2,"in", year))
      
    }
    else if(sex == "M"){
      p2=ggplot(data = filter(StatesData, YEAR==year & SEX=="M" & STATE==location2)[1:n,],aes(x= reorder(NAME,+PERCENTAGE),y=PERCENTAGE))+geom_bar(stat="identity",fill="deepskyblue4",color="black")+coord_flip()+ylab("Percent of Babies Born")+ xlab("Name")+ggtitle(paste("Top",n,"Boy Names in",location2,"in", year))
    }
    
    
    
    
  }  
  
  
 grid.arrange(p1,p2) 
  
  
}





















PopularityCompareOverTime=function(Location1,Location2,Name1,Name2=NULL,Name3=NULL,Name4=NULL,LowerYear,UpperYear,sex){
  if(Location1=="US"& Location2!="US"){
    if(is.null(Name1)==FALSE & is.null(Name2)==TRUE & is.null(Name3)==TRUE & is.null(Name4)==TRUE){
      ggplot()+geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location2),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        ggtitle(paste("Popularity of Names in",Location1,"(solid) and",Location2,"(dashed)",sep = " "))
      
    }
    else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==TRUE & is.null(Name4)==TRUE){
      ggplot()+geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location2),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location2),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        ggtitle(paste("Popularity of Names in",Location1,"(solid) and",Location2,"(dashed)",sep = " "))
        
    }
    else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==TRUE){
      ggplot()+geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location2),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location2),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name3) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name3) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location2),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        ggtitle(paste("Popularity of Names in",Location1,"(solid) and",Location2,"(dashed)",sep = " "))
    }
    else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE){
      ggplot()+geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location2),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location2),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name3) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name3) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location2),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name4) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name4) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location2),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        ggtitle(paste("Popularity of Names in",Location1,"(solid) and",Location2,"(dashed)",sep = " "))
      
      
      
    }
    
    
    
  }
  else if(Location1=="US"& Location2=="US"){
    if(is.null(Name1)==FALSE & is.null(Name2)==TRUE & is.null(Name3)==TRUE & is.null(Name4)==TRUE){
      ggplot()+geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear ),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        ggtitle(paste("Popularity of Names in",Location1,"(solid) and",Location2,"(dashed)",sep = " "))
      
      
    }
    else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==TRUE & is.null(Name4)==TRUE){
      ggplot()+geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear ),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        ggtitle(paste("Popularity of Names in",Location1,"(solid) and",Location2,"(dashed)",sep = " "))
      
      
    }
    else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==TRUE){
      ggplot()+geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear ),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear ),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name3) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name3) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear ),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        ggtitle(paste("Popularity of Names in",Location1,"(solid) and",Location2,"(dashed)",sep = " "))
    }
    else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE){
      ggplot()+geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear ),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear ),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name3) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name3) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear ),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name4) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name4) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        ggtitle(paste("Popularity of Names in",Location1,"(solid) and",Location2,"(dashed)",sep = " "))
      
      
      
    }
  }
  else if(Location1!="US" & Location2=="US"){
    
    if(is.null(Name1)==FALSE & is.null(Name2)==TRUE & is.null(Name3)==TRUE & is.null(Name4)==TRUE){
      ggplot()+geom_line(data = filter(StatesData, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location1),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear ),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        ggtitle(paste("Popularity of Names in",Location1,"(solid) and",Location2,"(dashed)",sep = " "))
      
      
    }
    else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==TRUE & is.null(Name4)==TRUE){
      ggplot()+geom_line(data = filter(StatesData, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear& STATE==Location1),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear ),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear& STATE==Location1),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        ggtitle(paste("Popularity of Names in",Location1,"(solid) and",Location2,"(dashed)",sep = " "))
      
      
    }
    else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==TRUE){
      ggplot()+geom_line(data = filter(StatesData, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear& STATE==Location1),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear ),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear& STATE==Location1),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear ),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name3) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear& STATE==Location1),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name3) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear ),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        ggtitle(paste("Popularity of Names in",Location1,"(solid) and",Location2,"(dashed)",sep = " "))
    }
    else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE){
      ggplot()+geom_line(data = filter(StatesData, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear& STATE==Location1),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear ),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear& STATE==Location1),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear ),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name3) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear& STATE==Location1),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name3) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear ),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name4) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear& STATE==Location1),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(NameDataUS, NAME==FormatFunction(Name4) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        ggtitle(paste("Popularity of Names in",Location1,"(solid) and",Location2,"(dashed)",sep = " "))
      
      
      
    }
    
    
  }
  else if(Location1!="US" & Location2!="US"){
    if(is.null(Name1)==FALSE & is.null(Name2)==TRUE & is.null(Name3)==TRUE & is.null(Name4)==TRUE){
      ggplot()+geom_line(data = filter(StatesData, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location1),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location2),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        ggtitle(paste("Popularity of Names in",Location1,"(solid) and",Location2,"(dashed)",sep = " "))
      
      
    }
    else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==TRUE & is.null(Name4)==TRUE){
      ggplot()+geom_line(data = filter(StatesData, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear& STATE==Location1),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location2),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear& STATE==Location1),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear& STATE==Location2),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        ggtitle(paste("Popularity of Names in",Location1,"(solid) and",Location2,"(dashed)",sep = " "))
      
      
    }
    else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==TRUE){
      ggplot()+geom_line(data = filter(StatesData, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear& STATE==Location1),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location2),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear& STATE==Location1),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location2),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name3) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear& STATE==Location1),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name3) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location2),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        ggtitle(paste("Popularity of Names in",Location1,"(solid) and",Location2,"(dashed)",sep = " "))
    }
    else if(is.null(Name1)==FALSE & is.null(Name2)==FALSE & is.null(Name3)==FALSE & is.null(Name4)==FALSE){
      ggplot()+geom_line(data = filter(StatesData, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear& STATE==Location1),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name1) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location2),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear& STATE==Location1),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name2) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear & STATE==Location2),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name3) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear& STATE==Location1),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name3) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear& STATE==Location2 ),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name4) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear& STATE==Location1),aes(x=YEAR,y = PERCENTAGE,color = NAME))+
        geom_line(data = filter(StatesData, NAME==FormatFunction(Name4) & SEX==sex & YEAR>= LowerYear & YEAR <=UpperYear& STATE==Location2),aes(x=YEAR,y = PERCENTAGE,color = NAME),linetype = "dashed")+
        ggtitle(paste("Popularity of Names in",Location1,"(solid) and",Location2,"(dashed)",sep = " "))
      
      
      
    }
    
    
    
    
    
    
    
    
  }  
    
    
    
    
 
}













