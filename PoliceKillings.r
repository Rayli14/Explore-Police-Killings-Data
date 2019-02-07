##setwd
cd="/Users/rli/Desktop/midterm/"
#library
library(readxl)
library(ggplot2)
library(mice)
library(wordcloud)
library(wordcloud2)
library(stringr)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(lubridate)


###########################Problem 1.1 
##load data

pkdataset1 <- read_excel(paste(cd,"PoliceKillings.xlsx",sep=""),sheet="PKDataset1")
pkdataset2 <- read_excel(paste(cd,"PoliceKillings.xlsx",sep=""),sheet="PKDataset2")
##data size 
dim(pkdataset1)
dim(pkdataset2)

## the type of data and variables
str(pkdataset1)
str(pkdataset2)

##what's inside the data 
names(pkdataset1)
names(pkdataset2)

##NA
md.pattern(pkdataset1)
md.pattern(pkdataset2)


#differences
table(pkdataset1$raceethnicity)
table(pkdataset2$race)

table(pkdataset1$gender)
table(pkdataset2$gender)

table(pkdataset1$armed)
table(pkdataset2$armed)



###########################Problem 1.2 
#
pkdataset2$gender[pkdataset2$gender=="F"]="Female"
pkdataset2$gender[pkdataset2$gender=="M"]="Male"

pkdataset1$age=as.numeric(pkdataset1$age)
md.pattern(pkdataset1)
#Handel NA in Sheet1
pkdataset1=na.omit(pkdataset1)
pkdataset1$date=substring(pkdataset1$month,1,3)
mo2Num = function(x) match(tolower(x), tolower(month.abb))
pkdataset1$date=mo2Num(pkdataset1$date)
pkdataset1$date=ifelse(nchar(pkdataset1$date)==1,paste("0",pkdataset1$date,sep=""),pkdataset1$date)
pkdataset1$day=ifelse(nchar(pkdataset1$day)==1,paste("0",pkdataset1$day,sep=""),pkdataset1$day)
pkdataset1$date=paste(pkdataset1$year,"-",pkdataset1$date,"-",pkdataset1$day,sep="")
pkdataset1$state=toupper(pkdataset1$state)
pkdataset2$state=toupper(pkdataset2$state)
pkdataset2$date=as.character(pkdataset2$date)

##Handle NA in Sheet2
pkdataset2$age[is.na(pkdataset2$age)]=median(pkdataset2$age,na.rm=T)
##Mode
getMode = function(x){
	library(dplyr)
	t=data.frame(table(x)) %>% arrange(desc(Freq)) 
	mode=as.character(t[1,1])
	return(mode)
}

pkdataset2$race[is.na(pkdataset2$race)]=getMode(pkdataset2$race)
pkdataset2$armed[is.na(pkdataset2$armed)]=getMode(pkdataset2$armed)
pkdataset2$flee[is.na(pkdataset2$flee)]=getMode(pkdataset2$flee)
pkdataset1<-na.omit(pkdataset1)
pkdataset2<-na.omit(pkdataset2)
names(pkdataset1)<-c("id","name","age","gender","race","month","day","year","streetaddress","city","state","manner_of_death","lawenforcementagency","armed","date")
pkd1<-pkdataset1[,c(2,3,4,5,10,11,12,14,15)]
pkd1$group="pkd1"
pkd2<-pkdataset2[,c(2,3,4,5,6,7,8,9,10)]
pkd2$race
pkd2$group="pkd2"
pkd<-rbind(pkd1,pkd2)
##################Problem 1.3
#p1 age
g<-ggplot(pkd,aes(x=group,y=age,color=factor(group)))+
  geom_boxplot()+theme(plot.title  = element_text(hjust = 0.5))+
  labs(title="Boxplot: People's Ages in different datasets")
g

##piechart
plotpolar=function(x,title) {

	library(ggplot2)
	
	dt = data.frame(table(x))
	names(dt)=c("B","A")
	
	if(nrow(dt)>5){
		top_right="right"
	}else{top_right="top"}

	dt = dt[order(dt$A, decreasing = TRUE),]
	myLabel = as.vector(dt$B)   
	myLabel = paste(myLabel, "(", round(dt$A / sum(dt$A) * 100, 2), "%)", sep = "")   


	p = ggplot(dt, aes(x = "", y = A, fill = B)) + 
	  geom_bar(stat = "identity", width = 1) +    
	  coord_polar(theta = "y") + 
	  labs(x = "", y = "", title = title) + 
	  theme(axis.ticks = element_blank()) + 
	  theme(legend.title = element_blank(), legend.position = top_right) + 
	  scale_fill_discrete(breaks = dt$B, labels = myLabel) +  theme(plot.title  = element_text(hjust = 0.5))+
	  theme(axis.text.x = element_blank()) 
	
	return(p)
}

#p2 gender
g1=plotpolar(pkdataset1$gender,"DB1: Gender distribution") 
g2=plotpolar(pkdataset2$gender,"DB2: Gender distribution") 
grid.arrange(g1,g2,ncol=2)

#bar plot
plotbar=function(x,title,topn=nrow(table(x))){ ##topn is First/last depends on order
  
  library(ggplot2)
  library(ggthemes)
  
  dt = data.frame(table(x))
  names(dt)=c("obj","val")
  dt = dt[order(dt$val, decreasing = T),]
  dt$obj=factor(dt$obj,levels=dt$obj)
  
  dt = dt[1:topn,]
  if(nrow(dt)>5){
    top_right="right"
  }else{top_right="top"}
  p = ggplot(dt, aes(x = obj, y = val, fill = obj, group = factor(1))) + 
    geom_bar(stat = "identity", width = 0.5)+
    theme_economist() + labs(title=title)+ theme(plot.title  = element_text(hjust = 0.5),legend.position = top_right)+
    geom_text(aes(label = val, vjust = -0.8, hjust = 0.5, color = obj), show.legend  = FALSE) +  
    ylim(min(dt$val, 0)*1.1, max(dt$val)*1.1)
  return(p)
  
}

##flip_bar chart

flipplotbar=function(x,title,topn=nrow(table(x))){ ##topn is First/last depends on order

	library(ggplot2)
	library(ggthemes)
	
	dt = data.frame(table(x))
	names(dt)=c("obj","val")
	dt = dt[order(dt$val, decreasing = F),]
	dt$obj=factor(dt$obj,levels=dt$obj)
	
	dt = dt[1:topn,]
	if(nrow(dt)>5){
	  top_right="right"
	}else{top_right="top"}
	p = ggplot(dt, aes(x = obj, y = val, fill = obj, group = factor(1))) + 
		geom_bar(stat = "identity", width = 0.5)+coord_flip()+
		theme_economist() + labs(title=title)+ theme(plot.title  = element_text(hjust = 0.5),legend.position = top_right)+
		geom_text(aes(label = val, vjust = -0.8, hjust = 0.5, color = obj), show.legend  = FALSE) +  
		ylim(min(dt$val, 0)*1.1, max(dt$val)*1.1)
	return(p)

}
#p3 race
g1=flipplotbar(pkdataset1$race,"DB1: Race distribution") 
g2=flipplotbar(pkdataset2$race,"DB2: Race distribution")
grid.arrange(g1,g2,ncol=2)

#p4 month
g1=plotbar(month(pkdataset1$date),"DB1: month distribution") 
g2=plotbar(month(pkdataset2$date),"DB2: month distribution")
grid.arrange(g1,g2,nrow=2)

#p5 year
g1=plotpolar(year(pkdataset1$date),"DB1: Year distribution") 
g2=plotpolar(year(pkdataset2$date),"DB2: Year distribution")
grid.arrange(g1,g2,ncol=2)

##wordcloud

wordcl=function(x,choice=1){ ##choice 

	library(wordcloud)
	library(wordcloud2)
	
	dt = data.frame(table(x))
	names(dt)=c("char","freq")
	
	if(choice==1){p=wordcloud2(dt[which(dt$freq>=8),],size=1, color = "random-light", backgroundColor = "white")}
	if(choice==2){
	   p=wordcloud(dt$char,dt$freq
	  ,min.freq = 1  
	  ,random.order = F 
	  ,colors = brewer.pal(8,"Dark2") 
	  ,scale = 3.5*c(1,0.25) 
	  ,max.words = 100 )
		
	}
	if(choice==3){p=wordcloud2(dt,size=2, color = "random-light", backgroundColor = "white")}
	
	return(p)

}
#p6 state
wordcl(pkdataset1$state,2)
wordcl(pkdataset2$state,2)

#p7 city Choosing frequncy>=8
wordcl(pkdataset1$city,1)
wordcl(pkdataset2$city,1)

#p8 classification(pkd1) and manner of death(pkd2)
g1=plotbar(pkdataset1$manner_of_death,"DB1: Classification distribution")
g2=plotbar(pkdataset2$manner_of_death,"DB2: Manner of death distribution")
grid.arrange(g1,g2,ncol=2)

#p9  armed distribution
g1=plotbar(pkdataset1$armed,"DB1: Armed distribution")
g2=plotbar(pkdataset2$armed,"DB2: Armed distribution")
grid.arrange(g1,g2)
#Choosing top 8
g1=plotbar(pkdataset1$armed,"DB1: Top 8 Armed distribution",8)
g2=plotbar(pkdataset2$armed,"DB2: Top 8 Armed distribution",8)
grid.arrange(g1,g2)

#p10 DB2: Threat_level
plotbar(pkdataset2$threat_level,"DB2: Threat_level distribution")

#p11 DB2: mental illness
plotpolar(pkdataset2$signs_of_mental_illness,"DB2: Mental illness distribution")

#p12 DB2: Flee
plotbar(pkdataset2$flee,"DB2: Flee distribution")

#p13 DB2: Body Camera
plotpolar(pkdataset2$body_camera,"DB2: Body Camera distribution")


##For correlation
#stack plot
plotstack=function(x,y,title="correlation"){

	library(ggplot2)
	data=data.frame(x,y)
	names(data)=c("x","y")
	p=ggplot(data,aes(x=x,fill=y))+geom_histogram(position="fill",stat = "count")+
	labs(title=title)
	
	return(p)
	}
plotstack(pkdataset1$gender,pkdataset1$armed,"correlation between gender and armed ")
plotstack(pkdataset1$manner_of_death,pkdataset1$armed,"correlation between manner_of_death and armed ")
plotstack(pkdataset2$gender,pkdataset2$threat_level,"correlation between gender and threat level ")
p1=plotstack(pkdataset2$signs_of_mental_illness,pkdataset2$threat_level,"correlation between mental status and threat level ")
p2=plotstack(pkdataset2$signs_of_mental_illness,pkdataset2$flee,"correlation between mental status and flee ")
grid.arrange(p2,p1,ncol=2)
