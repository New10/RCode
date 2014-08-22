

###########################
# File: Historical Weekly.R
# Description: Analyze Points Per Touch for QB/WR/RB
# JC Newton and Isaac Petersen
###########################


########################################
#Fantasy Scoring Breakdown 
########################################
Yds.point <- .1 # Points per yard gains WR/RB/TE
TD.point<-6 # Points per receiving rushing TD
Rec.point<-0.5 # Points per reception
#Rec.point<-0 # Points per reception
QBTD.point<-4 #Points per QB TD
Int.point<- -2 #Points per interception
Pass.point<-0.04 #Points per pass yards
Comp.point<-0.2 #Points per completion

#Libraries
library("XML")
library("ggplot2")
library("stringr")
library("plyr")
library("gridExtra")



#Functions
source()

#Specify info to scrape
years <- 2012:2012 #2011:2013
weeks <- 17

#Scrape data
qb <- list()
rb <- list()
rb1 <- list()
rb2 <- list()
rb3 <- list()
wr <- list()
wr1 <- list()
wr2 <- list()
wr3 <- list()
wr4 <- list()
wr5 <- list()
wr6 <- list()
pb <- txtProgressBar(min = 1, max = weeks, style = 3)
for(i in 1:weeks){
  setTxtProgressBar(pb, i)
  qb[[i]] <- readHTMLTable(paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=", head(years, 1), "&year_max=", tail(years, 1), "&season_start=1&season_end=-1&age_min=0&age_max=99&game_type=R&league_id=&team_id=&opp_id=&game_num_min=0&game_num_max=99&week_num_min=", i, "&week_num_max=", i, "&game_day_of_week=&game_location=&game_result=&handedness=&is_active=&is_hof=&c1stat=pass_att&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=pass_att", sep=""), stringsAsFactors = FALSE)$stats
  rb1[[i]] <- readHTMLTable(paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=", head(years, 1), "&year_max=", tail(years, 1), "&season_start=1&season_end=-1&age_min=0&age_max=99&game_type=R&league_id=&team_id=&opp_id=&game_num_min=0&game_num_max=99&week_num_min=", i, "&week_num_max=", i, "&game_day_of_week=&game_location=&game_result=&handedness=&is_active=&is_hof=&c1stat=rush_att&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=rush_yds", sep=""), stringsAsFactors = FALSE)$stats
  rb2[[i]] <- readHTMLTable(paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=", head(years, 1), "&year_max=", tail(years, 1), "&season_start=1&season_end=-1&age_min=0&age_max=99&league_id=&team_id=&opp_id=&game_type=R&game_num_min=0&game_num_max=99&week_num_min=", i, "&week_num_max=", i, "&game_day_of_week=&game_month=&game_location=&game_result=&is_active=&handedness=&is_hof=&c1stat=rush_att&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=rush_yds&order_by_asc=&offset=100", sep=""), stringsAsFactors = FALSE)$stats
  rb3[[i]] <- readHTMLTable(paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=", head(years, 1), "&year_max=", tail(years, 1), "&season_start=1&season_end=-1&age_min=0&age_max=99&league_id=&team_id=&opp_id=&game_type=R&game_num_min=0&game_num_max=99&week_num_min=", i, "&week_num_max=", i, "&game_day_of_week=&game_month=&game_location=&game_result=&is_active=&handedness=&is_hof=&c1stat=rush_att&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=rush_yds&order_by_asc=&offset=200", sep=""), stringsAsFactors = FALSE)$stats
  wr1[[i]] <- readHTMLTable(paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=", head(years, 1), "&year_max=", tail(years, 1), "&season_start=1&season_end=-1&age_min=0&age_max=99&game_type=R&league_id=&team_id=&opp_id=&game_num_min=0&game_num_max=99&week_num_min=", i, "&week_num_max=", i, "&game_day_of_week=&game_location=&game_result=&handedness=&is_active=&is_hof=&c1stat=rec&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=rec_yds", sep=""), stringsAsFactors = FALSE)$stats
  wr2[[i]] <- readHTMLTable(paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=", head(years, 1), "&year_max=", tail(years, 1), "&season_start=1&season_end=-1&age_min=0&age_max=99&league_id=&team_id=&opp_id=&game_type=R&game_num_min=0&game_num_max=99&week_num_min=", i, "&week_num_max=", i, "&game_day_of_week=&game_month=&game_location=&game_result=&is_active=&handedness=&is_hof=&c1stat=rec&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=rec_yds&order_by_asc=&offset=100", sep=""), stringsAsFactors = FALSE)$stats
  wr3[[i]] <- readHTMLTable(paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=", head(years, 1), "&year_max=", tail(years, 1), "&season_start=1&season_end=-1&age_min=0&age_max=99&league_id=&team_id=&opp_id=&game_type=R&game_num_min=0&game_num_max=99&week_num_min=", i, "&week_num_max=", i, "&game_day_of_week=&game_month=&game_location=&game_result=&is_active=&handedness=&is_hof=&c1stat=rec&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=rec_yds&order_by_asc=&offset=200", sep=""), stringsAsFactors = FALSE)$stats
  wr4[[i]] <- readHTMLTable(paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=", head(years, 1), "&year_max=", tail(years, 1), "&season_start=1&season_end=-1&age_min=0&age_max=99&league_id=&team_id=&opp_id=&game_type=R&game_num_min=0&game_num_max=99&week_num_min=", i, "&week_num_max=", i, "&game_day_of_week=&game_month=&game_location=&game_result=&is_active=&handedness=&is_hof=&c1stat=rec&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=rec_yds&order_by_asc=&offset=300", sep=""), stringsAsFactors = FALSE)$stats
  wr5[[i]] <- readHTMLTable(paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=", head(years, 1), "&year_max=", tail(years, 1), "&season_start=1&season_end=-1&age_min=0&age_max=99&league_id=&team_id=&opp_id=&game_type=R&game_num_min=0&game_num_max=99&week_num_min=", i, "&week_num_max=", i, "&game_day_of_week=&game_month=&game_location=&game_result=&is_active=&handedness=&is_hof=&c1stat=rec&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=rec_yds&order_by_asc=&offset=400", sep=""), stringsAsFactors = FALSE)$stats
  wr6[[i]] <- readHTMLTable(paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi?request=1&match=game&year_min=", head(years, 1), "&year_max=", tail(years, 1), "&season_start=1&season_end=-1&age_min=0&age_max=99&league_id=&team_id=&opp_id=&game_type=R&game_num_min=0&game_num_max=99&week_num_min=", i, "&week_num_max=", i, "&game_day_of_week=&game_month=&game_location=&game_result=&is_active=&handedness=&is_hof=&c1stat=rec&c1comp=gt&c1val=1&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=rec_yds&order_by_asc=&offset=500", sep=""), stringsAsFactors = FALSE)$stats
}

qbDF <- do.call(rbind, qb)
rb1DF <- do.call(rbind, rb1)
rb2DF <- do.call(rbind, rb2)
rb3DF <- do.call(rbind, rb3)
wr1DF <- do.call(rbind, wr1)
wr2DF <- do.call(rbind, wr2)
wr3DF <- do.call(rbind, wr3)
wr4DF <- do.call(rbind, wr4)
wr5DF <- do.call(rbind, wr5)
wr6DF <- do.call(rbind, wr6)


rbDF <- rbind(rb1DF, rb2DF, rb3DF)
wrDF <- rbind(wr1DF, wr2DF, wr3DF, wr4DF, wr5DF, wr6DF)

#Variable names
names(qbDF) <- c("rank","player","age","date","league","team","blank","opponent","result","game","week","day","passComp","passAtt","passCompPct","passYds","passTds","passInt","passRating","passYdsPerAtt","passYdsPerAttAdj")
names(rbDF) <- c("rank","player","age","date","league","team","blank","opponent","result","game","week","day","rushAtt","rushYds","rushYdsPerAtt","rushTds")
names(wrDF) <- c("rank","player","age","date","league","team","blank","opponent","result","game","week","day","rec","recYds","ydsPerRec","recTds")

#Player name
qbDF$name <- nameMerge(qbDF$player)
rbDF$name <- nameMerge(rbDF$player)
wrDF$name <- nameMerge(wrDF$player)

#Year
qbDF$year <- str_trim(sapply(str_split(qbDF$date, "\\-"), "[", 1))
rbDF$year <- str_trim(sapply(str_split(rbDF$date, "\\-"), "[", 1))
wrDF$year <- str_trim(sapply(str_split(wrDF$date, "\\-"), "[", 1))


##############################################
# Running Back and Wide Receiver
##############################################
weeklyData <- rbind.fill(rbDF,wrDF)


#Cleanup data frames
weeklyData <- weeklyData[-which(weeklyData$rank == "Rk"),]

#Convert to numeric

weeklyData[,c("rank","player","year","week","rushAtt","rushYds","rushYdsPerAtt","rushTds","rec","recYds","ydsPerRec","recTds")] <- convert.magic(weeklyData[,c("rank","player","year","week","rushAtt","rushYds","rushYdsPerAtt","rushTds","rec","recYds","ydsPerRec","recTds")], "numeric")
weeklyData[is.na(weeklyData)] <- 0

#sum using ddply...

weeklyData <- ddply(weeklyData, c("name","year","week"), summarise,
                     rushYds    = sum(rushYds),
                     rushTds = sum(rushTds),
                     recYds = sum(recYds),
                     recTds= sum(recTds),
                     rushAtt = sum(rushAtt),
                     rec = sum(rec))




# Estimate Points Per Touch
weeklyData<- transform(weeklyData,
               Value = (rec*Rec.point+rushYds*Yds.point+ rushTds*TD.point+recYds*Yds.point+recTds*TD.point),
               PPT = (rec*Rec.point+rushYds*Yds.point+ rushTds*TD.point+recYds*Yds.point+recTds*TD.point)/(rushAtt+rec),
               Touches = (rushAtt+rec)
              )
 
#Subset Data
weeklyData <- weeklyData[,c("name","year","week","rushAtt","rushYds","rushTds","rec","recYds","recTds","Value", "PPT","Touches")]

weeklyData <- weeklyData[order(weeklyData$year, weeklyData$week, weeklyData$name),]

datasummary <- ddply(weeklyData, c("name"), summarise,
                    Value = sum(Value),
                    Games    = length(PPT),
                    Touch=sum(Touches),
                    muPPT = mean(PPT),
                    muTouch = mean(Touches),
                    sdPPT = sd(PPT),
                    sdTouch = sd(Touches),
                    cvPPT = sd(PPT)/mean(PPT))

datasummary[which(datasummary$name == "DANNYWOODHEAD" ), ]
datasummary[which(datasummary$name == "JAMAALCHARLES" ), ]
datasummary[which(datasummary$name == "JOIQUEBELL" ), ]
datasummary[which(datasummary$name == "STEVENJACKSON" ), ]
datasummary[which(datasummary$name == "SHANEVEREEN" ), ]
datasummary[which(datasummary$name == "TOBYGERHART" ), ]


#############################
# Filter data
#############################
datasummary<-datasummary[which(datasummary$Touch >=100 & datasummary$muPPT <=1.6 & datasummary$muPPT >=.6), ] #You can change the number of Touches to get a larger matrix of RB/WR stats

datasummary<-datasummary[order(datasummary$muPPT, decreasing = TRUE),]

datasummary<-datasummary[c(1:30),]

df<-data.frame(x=datasummary$name, y=cbind(datasummary$muPPT, datasummary$cvPPT, datasummary$Touch, datasummary$Value))
df$x = factor(df$x,levels=df$x,ordered=TRUE)

plot1<-ggplot(data=df, aes(x=x, y=y.1, order=y.1))+geom_bar(fill="grey",colour="white", colour="grey", stat="identity")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ guides(fill=FALSE)+coord_flip()+ theme(axis.text.y=element_text(size = rel(1), colour = "black"), axis.text.x=element_text(size = rel(1), colour = "black"), axis.title.x=element_text(size = rel(1)), axis.title.y=element_text(size = rel(1))) +xlab("Player")+ylab("Points Per Touch") 

plot2<-ggplot(data=df, aes(x=x, y=y.2))+geom_bar(fille="grey", colour="white", stat="identity")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ guides(fill=FALSE)+coord_flip() + theme(axis.text.y=element_text(size = rel(1), colour = "black"), axis.text.x=element_text(size = rel(1), colour = "black"), axis.title.x=element_text(size = rel(1)), axis.title.y=element_text(size = rel(1))) +xlab("Player")+ylab("Coefficient of Variation (PPT)") 

plot3<-ggplot(data=df, aes(x=x, y=y.3))+geom_bar(fill="blue", colour="grey", stat="identity")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ guides(fill=FALSE)+coord_flip() + theme(axis.text.y=element_text(size = rel(1), colour = "black"), axis.text.x=element_text(size = rel(1), colour = "black"), axis.title.x=element_text(size = rel(1)), axis.title.y=element_text(size = rel(1))) +xlab("Player")+ylab("Touches") 

plot4<-ggplot(data=df, aes(x=x, y=y.4))+geom_bar(fill="orange", colour="grey", stat="identity")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ guides(fill=FALSE)+coord_flip() + theme(axis.text.y=element_text(size = rel(1), colour = "black"), axis.text.x=element_text(size = rel(1), colour = "black"), axis.title.x=element_text(size = rel(1)), axis.title.y=element_text(size = rel(1))) +xlab("Player")+ylab("Est. Fantasy Points") 

grid.arrange(plot1, plot3, plot2, plot4, ncol=4, nrow=1)








