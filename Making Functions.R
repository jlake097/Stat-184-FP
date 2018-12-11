#For Loop Scripp for Rushing Stats
library(rvest)
library(data.table)
library(xml2)
library(XML)


## years joe pa
url2004 <- "http://www.espn.com/college-football/team/stats/_/id/213/year/2004"
url2005 <- "http://www.espn.com/college-football/team/stats/_/id/213/year/2005"
url2006<- "http://www.espn.com/college-football/team/stats/_/id/213/year/2006"
url2007<- "http://www.espn.com/college-football/team/stats/_/id/213/year/2007"
url2008<- "http://www.espn.com/college-football/team/stats/_/id/213/year/2008"
url2009 <- "http://www.espn.com/college-football/team/stats/_/id/213/year/2009"
url2010 <- "http://www.espn.com/college-football/team/stats/_/id/213/year/2010"
url2011 <- "http://www.espn.com/college-football/team/stats/_/id/213/year/2011"


### O'Brien
url2012 <- "http://www.espn.com/college-football/team/stats/_/id/213/year/2012"
url2013 <- "http://www.espn.com/college-football/team/stats/_/id/213/year/2013"

## James Franklin
url2014 <- "http://www.espn.com/college-football/team/stats/_/id/213/year/2014"
url2015 <- "http://www.espn.com/college-football/team/stats/_/id/213/year/2015"
url2016 <- "http://www.espn.com/college-football/team/stats/_/id/213/year/2016"
url2017 <- "http://www.espn.com/college-football/team/stats/_/id/213/year/2017"
url2018 <- "http://www.espn.com/college-football/team/stats/_/id/213/year/2018"


### Figure out a way to scrape the data for the games won and lost during the season
Season18 <- "http://www.espn.com/college-football/team/schedule/_/id/213/penn-state-nittany-lions"
tables<-html_table(read_html(Season18),fill = TRUE)


# Make Tables for Coaches with Data of Interest
url_list <- c("url2004","url2005","url2006","url2007","url2008","url2009","url2010","url2011","url2012","url2013","url2014","url2015","url2016","url2017","url2018")
year_list<-c("2004","2005","2006","2007","2008","2009","2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
coach_list<-c("JP","JP","JP","JP","JP","JP","JP","JP","OB","OB","JF","JF","JF","JF","JF")



GetTable<-function(x,y,z){
  Tables_P<-grep("Passing Statistics",x)
  Tables_R<-grep("Rushing Statistics",y)
  Tables_Rec<-grep("Receiving Statistics",z)
  
}



Rushing<-NULL
Kicking<-NULL
ReceivingStats<-NULL
Passing<-NULL
Tables<-NULL

for(i in 1:length(url_list)){
  url<-paste("http://www.espn.com/college-football/team/stats/_/id/213/year/",year_list[i],sep = "")
  Tables<-html_table(read_html(url), fill = TRUE)
  
  
  {
  GetTable(Tables,Tables,Tables)
  
  PassingStats<-Tables[[Table_P]][-1,]
  PassingH<-PassingStats[1,]
  RowsP<-nrow(PassingStats)
  PassingStats<-PassingStats[RowsP,]
  PassingH<-as.character(PassingH)
  setnames(PassingStats,PassingH)
  PassingStats$coach<-coach_list[i]
  PassingStats$year<-year_list[i]
  Passing<-rbind(Passing,PassingStats)
  
  Receiving<-Tables[[Table_Rec]][-1,]
  HeadReceiving<-Receiving[1,]
  Receiving<-Receiving[-1,]
  RowRec<-nrow(Receiving)
  Receiving<-Receiving[RowRec,]
  HeadReceiving<-as.character(HeadReceiving)
  setnames(Receiving,HeadReceiving)
  Receiving$coach<-coach_list[i]
  Receiving$year<-year_list[i]
  ReceivingStats<-rbind(ReceivingStats,Receiving)
  
  RushingStats<-Tables[[Table_R]][-1,]
  HeadRushing<-RushingStats[1,]
  RowsR<-nrow(RushingStats)
  RushingStats<-RushingStats[RowsR,]
  HeadRushing<-as.character(HeadRushing)
  setnames(RushingStats,HeadRushing)
  RushingStats$coach<-coach_list[i]
  RushingStats$year<-year_list[i]
  Rushing<-rbind(Rushing,RushingStats)
  }
  message(year_list[i])
}

###table manipulation

library(dplyr)
library(reshape2)
Kicking <- data.table(Kicking)
DK<-Kicking[,c("XPM","XPA","XP%","FGM", "FGA", "FG%", "1-19", "20-29","30-39","40-49","50+", "LNG","PTS","coach", "year" )]

DR<-Rushing[,c("CAR","YDS","TD","coach","year")]
ReceivingStats<-data.table(ReceivingStats)
Passing<-data.table(Passing)


Rushing<-data.table(Rushing)
DR<-Rushing[,c("CAR","YDS","TD","coach","year")]
setnames(DR,"YDS","Rush_YDS")
setnames(DR,"CAR","Rush_CAR")
setnames(DR,"TD","Rush_TDs")

ReceivingStats<-data.table(ReceivingStats)
DRec<-ReceivingStats[,c("REC","YDS","LONG","TD","coach","year")]
setnames(DRec,"YDS","Rec_YDS")
setnames(DRec,"LONG","Rec_LONG")
setnames(DRec,"TD","Rec_TD")

Passing<-data.table(Passing)
DP<-Passing[,c("CMP","ATT","YDS","CMP%","YDS/A","TD","INT","RAT","coach","year")]
setnames(DP,"TD","Pass_TDs")
setnames(DP,"CMP","Pass_CMP")
setnames(DP,"ATT","Pass_ATT")
setnames(DP,"CMP%","Pass_CMP%")
setnames(DP,"YDS/A","Pass_YDS/A")
setnames(DP,"INT","Pass_INT")
setnames(DP,"RAT","Pass_RAT")

setkey(DRec,year,coach)
setkey(DP,year,coach)
setkey(DR,year,coach)

DRec[order(year,coach)]
DPc[order(year,coach)]

Stats<-merge(DRec,DP,all.x=T)
Stats<-merge(Stats,DR,all.x=T)
Stats<-data.table(Stats)


###Making New Variables

Stats$Pass_ATT<-as.numeric(Stats$Pass_ATT)
Stats$Pass_INT<-as.numeric(Stats$Pass_INT)
Stats$ATT_per_INT<-Stats$Pass_ATT/Stats$Pass_INT

Stats$Rush_YDS<-as.numeric(Stats$Rush_YDS)
Stats$Rush_CAR<-as.numeric(Stats$Rush_CAR)
Stats$YDS_per_Carry<-Stats$Rush_YDS/Stats$Rush_CAR


Stats$Rec_TD<-as.numeric(Stats$Rec_TD)
Stats$Rec_YDS<-as.numeric(Stats$Rec_YDS)
Stats$RecYDS_Per_TD<-Stats$Rec_YDS<-Stats$Rec_TD

##Make Graphs
library(ggplot2)
RecTD<-ggplot(Stats,aes(x=year,y=Rec_TD,col=coach,))+geom_col()+ggtitle("That Ratio of Rec YDs Each Coach Got on A Given Year")
RushTD<-ggplot(Stats,aes(x=year,y=Rush_TDs,col=coach))+geom_col()+ggtitle("Rusing TD garnered each Season by each Coach")
Rec_per_TD<-ggplot(Stats,aes(x=year,y=RecYDS_Per_TD,col=coach))+geom_col()+ggtitle("The Ratio of Yards per TD Under the Last Three Head Coaches")

ggsave("RushTD.pdf")
ggsave("Rec_per_TD.pdf")
ggsave("RecTD.pdf")
