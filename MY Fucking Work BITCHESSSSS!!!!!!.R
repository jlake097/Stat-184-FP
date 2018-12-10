#For Loop Scripp for Rushing Stats
library(rvest)
library(data.table)
library(xml2)
library(XML)

setwd<-("~/Desktop/Stat_184/Final Project Folder")
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


Rushing<-NULL
Kicking<-NULL
ReceivingStats<-NULL
Passing<-NULL
Tables<-NULL
for(i in 1:length(url_list)){
  url<-paste("http://www.espn.com/college-football/team/stats/_/id/213/year/",year_list[i],sep = "")
  Tables<-html_table(read_html(url), fill = TRUE)
  
  Table_P<-grep("Passing Statistics",Tables)
  PassingStats<-Tables[[Table_P]][-1,]
  PassingH<-PassingStats[1,]
  RowsP<-nrow(PassingStats)
  PassingStats<-PassingStats[RowsP,]
  PassingH<-as.character(PassingH)
  setnames(PassingStats,PassingH)
  PassingStats$coach<-coach_list[i]
  PassingStats$year<-year_list[i]
  Passing<-rbind(Passing,PassingStats)

  Table_R<-grep("Rushing Statistics",Tables)
    RushingStats<-Tables[[Table_R]][-1,]
    HeadRushing<-RushingStats[1,]
    RowsR<-nrow(RushingStats)
    RushingStats<-RushingStats[RowsR,]
    HeadRushing<-as.character(HeadRushing)
    setnames(RushingStats,HeadRushing)
    RushingStats$coach<-coach_list[i]
    RushingStats$year<-year_list[i]
    Rushing<-rbind(Rushing,RushingStats)
    
    
    
    Table_Rec<-grep("Receiving Statistics",Tables)
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
    
    
    
    Table_K<-grep("Kicking Statistics", Tables)
      KickingStats<-Tables[[Table_K]][-1,]
      HeadKicking<-KickingStats[1,]
      RowsK<-nrow(KickingStats)
      KickingStats<-KickingStats[RowsK,]
      HeadKicking<-as.character(HeadKicking)
      setnames(KickingStats,HeadKicking)
      KickingStats$coach<-coach_list[i]
      KickingStats$year<-year_list[i]
      Kicking<-rbind(Kicking,KickingStats)
}

###table manipulation

library(dplyr)
library(reshape2)
Kicking <- data.table(Kicking)
DK<-dcast(Kicking, year, ~ variable, length,value.var = T)
