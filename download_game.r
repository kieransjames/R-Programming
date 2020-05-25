require(httr)
require(dplyr)
require(jsonlite)
require(caret)

ShotQualityModelAllPreds=readRDS("/Users/kieransjames/ShotQualityModelAllPreds.RData")

set.seed(11)



download_game = function(game_url){
  
  game_data = fromJSON(game_url) #downloading the JSON
  pbp_lists = game_data$liveData$plays$allPlays #extracting the allPlays list so that we can work with the contents more conveniently
  
  pbp_without_players = data.frame(pbp_lists$result$event,
                              pbp_lists$result$eventCode,
                              pbp_lists$result$eventTypeId,
                              pbp_lists$result$description,
                              pbp_lists$result$secondaryType,
                              pbp_lists$result$penaltySeverity,
                              pbp_lists$result$penaltyMinutes,
                              pbp_lists$result$gameWinningGoal,
                              pbp_lists$result$emptyNet,
                              pbp_lists$about$eventIdx,
                              pbp_lists$about$eventId,
                              pbp_lists$about$period,
                              pbp_lists$about$periodType,
                              pbp_lists$about$ordinalNum,
                              pbp_lists$about$periodTime,
                              pbp_lists$about$periodTimeRemaining,
                              pbp_lists$about$dateTime,
                              pbp_lists$about$goals$away,
                              pbp_lists$about$goals$home,
                              pbp_lists$coordinates,
                              pbp_lists$team)
  
  pbp_without_players = mutate(pbp_without_players,periodTime=as.character(pbp_lists$about$periodTime),
                               period = as.character(pbp_lists$about$period),
                               eventType = as.character(pbp_lists$result$event))#adding character columns for the join later

  # the reason why we add each column as above rather than sensibly adding the dataframes,
  # is that some of the dataframes also have nested dataframes within themselves, and its a mess to clear up otherwise
  
  pbp_complete = mutate(pbp_without_players,
                        Shooter=NA,
                        Goalie=NA,
                        Winner=NA,
                        Loser=NA,
                        Blocker=NA,
                        Hitter=NA,
                        Hittee=NA,
                        PlayerID=NA,
                        PenaltyOn=NA,
                        DrewBy=NA,
                        Scorer=NA,
                        PrimaryAssist=NA,
                        SecondaryAssist=NA,
                        Goalie=NA,
                        Season=game_data$gameData$game$season,
                        Date=substr(game_data$gameData$datetime$dateTime,1,10),
                        GameID = game_data$gameData$game$pk,
                        StrengthState=NA,
                        AwayTeam = game_data$gameData$teams$away$triCode,
                        HomeTeam = game_data$gameData$teams$home$triCode,
                        distance=ifelse((90-abs(x)>0),((90-abs(x))^2+y^2)^0.5,-(((90-abs(x))^2+y^2)^0.5)),
                        angle=atan(abs(y)/(90-abs(x)))/pi*180,
                        SecondaryType=pbp_lists$result$secondaryType,
                        PenaltyMinutes=pbp_lists$result$penaltyMinutes,
                        PenaltySeverity=pbp_lists$result$penaltySeverity,
                        GoalsHome=pbp_lists$about$goals$home,GoalsAway=pbp_lists$about$goals$away,
                        PeriodType = game_data$liveData$plays$allPlays$about$periodType
                        )
  
  # we add in all of the columns we'll need to describe events and other such things
  
  #  a perhaps messy for loop that goes into every 'player' list and finds the relevant players (since they're stored as lists)
  for(i in 1:length(game_data$liveData$plays$allPlays$players)){
    type = pbp_lists$result$eventTypeId[i]
    if(type=="FACEOFF"){
      pbp_complete$Winner[i] = game_data$liveData$plays$allPlays$players[[i]][1,1][1,2]
      pbp_complete$Loser[i] = game_data$liveData$plays$allPlays$players[[i]][2,1][1,2]
    }
    else if(type=="BLOCKED_SHOT"){
      pbp_complete$Blocker[i] = game_data$liveData$plays$allPlays$players[[i]][1,1][1,2]
      pbp_complete$Shooter[i] = game_data$liveData$plays$allPlays$players[[i]][2,1][1,2]
    }
    else if(type=="HIT"){
      pbp_complete$Hitter[i] = game_data$liveData$plays$allPlays$players[[i]][1,1][1,2]
      pbp_complete$Hittee[i] = game_data$liveData$plays$allPlays$players[[i]][2,1][1,2]
    }
    else if(type=="SHOT"){
      pbp_complete$Shooter[i] = game_data$liveData$plays$allPlays$players[[i]][1,1][1,2]
      pbp_complete$Goalie[i] = game_data$liveData$plays$allPlays$players[[i]][2,1][1,2]
    }
    else if(type=="GIVEAWAY"){
      pbp_complete$PlayerID[i] = game_data$liveData$plays$allPlays$players[[i]][1,1][1,2]
    }
    else if(type=="TAKEAWAY"){
      pbp_complete$PlayerID[i] = game_data$liveData$plays$allPlays$players[[i]][1,1][1,2]
    }
    else if(type=="MISSED_SHOT"){
      pbp_complete$Shooter[i] = game_data$liveData$plays$allPlays$players[[i]][1,1][1,2]
    }
    else if(type=="PENALTY"){
      pbp_complete$PenaltyOn[i] = game_data$liveData$plays$allPlays$players[[i]][1,1][1,2]
      pbp_complete$DrewBy[i] = game_data$liveData$plays$allPlays$players[[i]][2,1][1,2]
    }
    else if(type=="GOAL"){
      pbp_complete$Scorer[i] = game_data$liveData$plays$allPlays$players[[i]][1,1][1,2]
      pbp_complete$PrimaryAssist[i] = game_data$liveData$plays$allPlays$players[[i]][2,1][1,2]
      pbp_complete$SecondaryAssist[i] = game_data$liveData$plays$allPlays$players[[i]][3,1][1,2]
      pbp_complete$Goalie[i] = game_data$liveData$plays$allPlays$players[[i]][4,1][1,2]
    }
  
  }
  
  #since we don't have on ice data, strength state data, or zone data, we need to supplement with NHL play by play pdfs
  # we do this by parsing the HTML source of the play by play files found on NHL.com, since those files have data that is missing from the API
  html_pbp = paste("http://www.nhl.com/scores/htmlreports/",game_data$gameData$game$season,"/PL02",
                    substr(game_data$gameData$game$pk,7,10),".HTM",sep="")
  html_text = suppressWarnings(readLines(html_pbp))
  
  
  

  indicesEven = grep(pattern="^<tr class=\"evenColor\">$",html_text) #index of all even events
  indicesOdd = grep(pattern="<tr class=\"	oddColor\">",html_text) #index of all odd events
  indices = c(indicesOdd,indicesEven) %>% sort(decreasing = FALSE)
  indices = indices[1:length(indices)-1]
  
  

  extra_pbp = as.data.frame(matrix(rep(NA,20*length(indices)),nrow=length(indices),ncol=20)) #blank data frame to fill
  colnames(extra_pbp) = c("Period","Time","Type","Zone","A1","A2","A3","A4","A5","A6","H1","H2","H3","H4","H5","H6","Strength","AwayEmptyNet",
                          "HomeEmptyNet","EmptyEvent")

  
  
  j=1

  for(i in indices){

    extra_pbp[j,1] = substr(html_text[i+2],gregexpr("</td>",html_text[i+2])[[1]][1]-1,gregexpr("</td>",html_text[i+2])[[1]][1]-1)
    extra_pbp[j,2] = substr(html_text[i+4],gregexpr("center\">",html_text[i+4])[[1]][1]+8,gregexpr("<br>",html_text[i+4])[[1]][1]-1)
    extra_pbp[j,3] = substr(html_text[i+5],gregexpr("center\">",html_text[i+5])[[1]][1]+8,gregexpr("</td>",html_text[i+5])[[1]][1]-1)
    if(grepl("<td class=\" + bborder\">&nbsp;</td>",html_text[i+6]))
      extra_pbp[j,20]="Yes"
    else
      extra_pbp[j,20]="No"
    if(gregexpr("Zone",html_text[i+6])[[1]][1]!=-1){
      index = gregexpr("Zone",html_text[i+6])[[1]][1]
      extra_pbp[j,4] = substr(html_text[i+6],index-5,index-3)
    }

    k=14 #counts how many lines from the beginning of the event we are
    l=5 #counts which column we're at



    repeat{
      extra_pbp[j,l] = substr(html_text[i+k],gregexpr("-",html_text[i+k])[[1]][1]+2,gregexpr("\">",html_text[i+k])[[1]][1]-1)
      extra_pbp[j,18] = substr(html_text[i+k],gregexpr("-",html_text[i+k])[[1]][1]-7,gregexpr("-",html_text[i+k])[[1]][1]-7)
      l=l+1
      k=k+13
      if(!grepl("nbsp",html_text[i+k-5])[[1]][1])
        break
    }
    k=k+5  #normal jumps are 13, but jumps between teams are of 18 lines
    l=11
    
    repeat{
      extra_pbp[j,l] = substr(html_text[i+k],gregexpr("-",html_text[i+k])[[1]][1]+2,gregexpr("\">",html_text[i+k])[[1]][1]-1)
      extra_pbp[j,19] = substr(html_text[i+k],gregexpr("-",html_text[i+k])[[1]][1]-7,gregexpr("-",html_text[i+k])[[1]][1]-7)
      l=l+1
      k=k+13
      if(!grepl("nbsp",html_text[i+k-5])[[1]][1])
        break
    }

    j=j+1 #keeps track of the rows, since the for loop 'i' variable is tracking where we are in the grep index

  }

  for(m in 1:length(indices)){ #adding in the strength state that we can now precisely calculate, relative to the home team

    home_skaters = 5-sum(is.na(extra_pbp[m,11:16]))
    away_skaters = 5-sum(is.na(extra_pbp[m,5:10]))
    extra_pbp$Strength[m] = paste(home_skaters,"v",away_skaters,sep="")
    
    if(nchar(extra_pbp$Time[m])==4)
      extra_pbp$Time[m] = paste("0",extra_pbp$Time[m],sep="")

  }
  
  lut = c("FAC" ="Faceoff","SHOT"="Shot","GIVE"="Giveaway","TAKE"="Takeaway","HIT"="Hit","MISS"="Missed Shot",
          "BLOCK"="Blocked Shot","STOP"="Stoppage","GOAL"="Goal","PENL"="Penalty","PSTR"="Period Start",
          "GEND"="Game","PEND"="Period End","CHL"="Official Challenge")
  
 
  extra_pbp$Type = lut[extra_pbp$Type] #converting the names of our eventType in order to ensure that they have compatible names
  extra_pbp = filter(extra_pbp,
                     Type != "PGSTR" & Type!="ANTHEM" & Type != "PGEND" & Type!="Game" & Type!="DELPEN"
                     & EmptyEvent!="Yes") #filtering out elements that dont add value and arent in API
  pbp_complete = filter(pbp_complete,
                        eventType!="Game Scheduled" & eventType!="Period Ready" & eventType!="Period Official"
                        & eventType!="Game End" & eventType!="Shootout Complete" & eventType!="Game Official"
                        & eventType!="Early Intermission Start" & eventType!="Early Intermission End")#filtering out non-common elements
  
  if(nrow(extra_pbp)!=nrow(pbp_complete)){
    print("The sources don't have the same number of events.")
    }# a small check that tells us whether the two sources arent matching up perfectly, which could cause problems
  
  # View(pbp_complete)
  # View(extra_pbp)  #these are here in case the code bugs out. Often theres a discrepancy in the columns caused by a foreign event
  
  
  
  pbp_complete = mutate(pbp_complete,eventindex = seq(1,nrow(pbp_complete),by=1))#adding event indices
  extra_pbp = mutate(extra_pbp,eventIndex=seq(1,nrow(extra_pbp),by=1))#adding event indices
  
  
  
  #joining the two data frames
  pbp_complete = cbind(pbp_complete,extra_pbp)
  
  
  #Adding columns for rebound and rush, modifying events to character, adjusting strength states
  pbp_augmented = mutate(pbp_complete,   
                         Rebound = 0,
                         Rush = 0,
                         HomeNetEmpty = ifelse(pbp_complete$HomeEmptyNet=="G","No","Yes"),
                         AwayNetEmpty = ifelse(pbp_complete$AwayEmptyNet=="G","No","Yes"),
                         GameCode = game_data$gameData$game$pk,
                         Strength = ifelse(extra_pbp$HomeEmptyNet=="G",
                                           Strength,
                                           paste(as.character(1+as.numeric(substr(Strength,1,1))),
                                                 "v",
                                                 substr(Strength,3,3),
                                                 sep="")),
                         Strength = ifelse(extra_pbp$AwayEmptyNet=="G",
                                           Strength,
                                           paste(substr(Strength,1,1),
                                                 "v",
                                                 as.character(1+as.numeric(substr(Strength,3,3))),
                                                 sep="")),
                         StrengthState = ifelse(triCode==HomeTeam,
                                                Strength,
                                                paste(substr(Strength,3,3),"v",substr(Strength,1,1),sep="")))
  
  #creating rebound data
  for(r in 2:nrow(pbp_augmented)){
    
    time1 = paste("2000-01-01 00:",pbp_augmented$periodTime[r-1],sep="") %>%
      as.POSIXlt()
    time2 = paste("2000-01-01 00:",pbp_augmented$periodTime[r],sep="") %>%
      as.POSIXlt()
    diff=as.numeric(time2-time1)
    
    if(pbp_augmented$eventType[r-1]!="Shot" | diff>2 | pbp_augmented$eventType[r]!="Shot")
      next
    else pbp_augmented$Rebound[r]=1
    
  }
  
  # creating rush data
  for(r in 2:nrow(pbp_augmented)){
    
    time1 = paste("2000-01-01 00:",pbp_augmented$periodTime[r-1],sep="") %>%
      as.POSIXlt()
    time2 = paste("2000-01-01 00:",pbp_augmented$periodTime[r],sep="") %>%
      as.POSIXlt()
    diff=as.numeric(time2-time1)
    
    
    if(diff>4 | pbp_augmented$eventType[r]!="Shot")
      next
    if(is.na(pbp_augmented$Zone[r-1]) | is.na(pbp_augmented$triCode[r-1]))
      next
    if(pbp_augmented$Zone[r-1]=="Def" & pbp_augmented$triCode[r-1]!=pbp_augmented$triCode[r])
      next
    if(pbp_augmented$Zone[r-1]=="Off" & pbp_augmented$triCode[r-1]==pbp_augmented$triCode[r])
      next
    else pbp_augmented$Rush[r]=1
  }
  
  pbp_refined = select(pbp_augmented,eventindex,GameCode,Date,eventType,SecondaryType,
                       periodTime,triCode,period,PeriodType,StrengthState,
                       x,y,distance,angle,Zone,Rebound,Rush,Season,
                       HomeTeam,AwayTeam,HomeNetEmpty,AwayNetEmpty,
                       PenaltyMinutes,PenaltySeverity,GoalsHome,GoalsAway,
                       Shooter,Goalie,Winner,Loser,Blocker,Hitter,Hittee,PlayerID,
                       PenaltyOn,DrewBy,Scorer,PrimaryAssist,SecondaryAssist,
                       A1,A2,A3,A4,A5,A6,H1,H2,H3,H4,H5,H6)
  
  # Score adjustment process
  
  pbp_refined = mutate(pbp_refined,ScoreState = ifelse(triCode==HomeTeam,
                                                       GoalsHome-GoalsAway,
                                                       GoalsAway-GoalsHome))
  
  
  pbp_refined = mutate(pbp_refined,
                      CorsiShot = ifelse(eventType == "Blocked Shot" | eventType == "Missed Shot" | eventType == "Shot" | eventType =="Goal",1,0),
                      FenwickShot = ifelse(eventType == "Missed Shot" | eventType == "Shot" | eventType =="Goal",1,0),
                      Shot = ifelse(eventType == "Shot" | eventType =="Goal",1,0),
                      Goal=ifelse(eventType =="Goal",1,0),
                      Penalty=ifelse(eventType=="Penalty",1,0)) %>%
    mutate(VenueAdjCorsi = 1*CorsiShot,
           VenueAdjFenwick = 1*FenwickShot,
           VenueAdjShot = 1*Shot,
           VenueAdjGoal = 1*Goal,
           ScoreAdjCorsi=1*CorsiShot,
           ScoreAdjFenwick=1*FenwickShot,
           ScoreAdjShot=1*Shot,
           ScoreAdjGoal=1*Goal)
  
  HomeTeam = pbp_refined$HomeTeam[1]
  AwayTeam = pbp_refined$AwayTeam[1]
  
  
  #Now we perform the adjustments
  
  #EV venue adjustment for Home
  pbp_refined$VenueAdjCorsi[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                 & pbp_refined$triCode==pbp_refined$HomeTeam)]=
    pbp_refined$CorsiShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                               & pbp_refined$triCode==pbp_refined$HomeTeam)]*0.986971353
  
  pbp_refined$VenueAdjFenwick[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                   & pbp_refined$triCode==pbp_refined$HomeTeam)] = 
    0.976945444*pbp_refined$FenwickShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                             & pbp_refined$triCode==pbp_refined$HomeTeam)]
  
  pbp_refined$VenueAdjShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                & pbp_refined$triCode==pbp_refined$HomeTeam)] = 
    0.979423348*pbp_refined$Shot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                      & pbp_refined$triCode==pbp_refined$HomeTeam)]
  
  pbp_refined$VenueAdjGoal[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                & pbp_refined$triCode==pbp_refined$HomeTeam)] =   
    0.958766234*pbp_refined$Goal[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                      & pbp_refined$triCode==pbp_refined$HomeTeam)]
  
  #EV venue adjustment for Away
  pbp_refined$VenueAdjCorsi[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                 & pbp_refined$triCode==pbp_refined$AwayTeam)] =
    1.013377221*pbp_refined$CorsiShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                           & pbp_refined$triCode==pbp_refined$AwayTeam)]
  
  pbp_refined$VenueAdjFenwick[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                   & pbp_refined$triCode==pbp_refined$AwayTeam)] = 
    1.024168966*pbp_refined$FenwickShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                             & pbp_refined$triCode==pbp_refined$AwayTeam)]
  
  pbp_refined$VenueAdjShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                & pbp_refined$triCode==pbp_refined$AwayTeam)] =
    1.021459793*pbp_refined$Shot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                      & pbp_refined$triCode==pbp_refined$AwayTeam)]
  
  pbp_refined$VenueAdjGoal[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                & pbp_refined$triCode==pbp_refined$AwayTeam)] = 
    1.044939844*pbp_refined$Goal[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                      & pbp_refined$triCode==pbp_refined$AwayTeam)]
  
  #PP venue adjustment for Home 
  pbp_refined$VenueAdjCorsi[which((pbp_refined$StrengthState=="5v4"|pbp_refined$StrengthState=="5v3"|pbp_refined$StrengthState=="4v3"
                                  |pbp_refined$StrengthState=="6v4"|pbp_refined$StrengthState=="6v3") 
                                 & pbp_refined$triCode==pbp_refined$HomeTeam)] = 
    0.969930578*pbp_refined$CorsiShot[which((pbp_refined$StrengthState=="5v4"|pbp_refined$StrengthState=="5v3"|pbp_refined$StrengthState=="4v3"
                                            |pbp_refined$StrengthState=="6v4"|pbp_refined$StrengthState=="6v3") 
                                           & pbp_refined$triCode==pbp_refined$HomeTeam)]
  
  pbp_refined$VenueAdjFenwick[which((pbp_refined$StrengthState=="5v4"|pbp_refined$StrengthState=="5v3"|pbp_refined$StrengthState=="4v3"
                                    |pbp_refined$StrengthState=="6v4"|pbp_refined$StrengthState=="6v3") 
                                   & pbp_refined$triCode==pbp_refined$HomeTeam)] = 
    0.968203064*pbp_refined$FenwickShot[which((pbp_refined$StrengthState=="5v4"|pbp_refined$StrengthState=="5v3"|pbp_refined$StrengthState=="4v3"
                                              |pbp_refined$StrengthState=="6v4"|pbp_refined$StrengthState=="6v3") 
                                             & pbp_refined$triCode==pbp_refined$HomeTeam)]
  
  pbp_refined$VenueAdjShot[which((pbp_refined$StrengthState=="5v4"|pbp_refined$StrengthState=="5v3"|pbp_refined$StrengthState=="4v3"
                                 |pbp_refined$StrengthState=="6v4"|pbp_refined$StrengthState=="6v3") 
                                & pbp_refined$triCode==pbp_refined$HomeTeam)] =  
    0.975040916*pbp_refined$Shot[which((pbp_refined$StrengthState=="5v4"|pbp_refined$StrengthState=="5v3"|pbp_refined$StrengthState=="4v3"
                                       |pbp_refined$StrengthState=="6v4"|pbp_refined$StrengthState=="6v3") 
                                      & pbp_refined$triCode==pbp_refined$HomeTeam)]
  
  pbp_refined$VenueAdjGoal[which((pbp_refined$StrengthState=="5v4"|pbp_refined$StrengthState=="5v3"|pbp_refined$StrengthState=="4v3"
                                 |pbp_refined$StrengthState=="6v4"|pbp_refined$StrengthState=="6v3") 
                                & pbp_refined$triCode==pbp_refined$HomeTeam)] = 
    0.956738829*pbp_refined$Goal[which((pbp_refined$StrengthState=="5v4"|pbp_refined$StrengthState=="5v3"|pbp_refined$StrengthState=="4v3"
                                       |pbp_refined$StrengthState=="6v4"|pbp_refined$StrengthState=="6v3") 
                                      & pbp_refined$triCode==pbp_refined$HomeTeam)]
  
  
  #PP venue adjustment for Away 
  pbp_refined$VenueAdjCorsi[which((pbp_refined$StrengthState=="5v4"|pbp_refined$StrengthState=="5v3"|pbp_refined$StrengthState=="4v3"
                                  |pbp_refined$StrengthState=="6v4"|pbp_refined$StrengthState=="6v3") 
                                 & pbp_refined$triCode==pbp_refined$AwayTeam)] = 
    1.034410768*pbp_refined$CorsiShot[which((pbp_refined$StrengthState=="5v4"|pbp_refined$StrengthState=="5v3"|pbp_refined$StrengthState=="4v3"
                                            |pbp_refined$StrengthState=="6v4"|pbp_refined$StrengthState=="6v3") 
                                           & pbp_refined$triCode==pbp_refined$AwayTeam)]
  
  pbp_refined$VenueAdjFenwick[which((pbp_refined$StrengthState=="5v4"|pbp_refined$StrengthState=="5v3"|pbp_refined$StrengthState=="4v3"
                                    |pbp_refined$StrengthState=="6v4"|pbp_refined$StrengthState=="6v3") 
                                   & pbp_refined$triCode==pbp_refined$AwayTeam)] = 
    1.036527204*pbp_refined$FenwickShot[which((pbp_refined$StrengthState=="5v4"|pbp_refined$StrengthState=="5v3"|pbp_refined$StrengthState=="4v3"
                                              |pbp_refined$StrengthState=="6v4"|pbp_refined$StrengthState=="6v3") 
                                             & pbp_refined$triCode==pbp_refined$AwayTeam)]
  
  pbp_refined$VenueAdjShot[which((pbp_refined$StrengthState=="5v4"|pbp_refined$StrengthState=="5v3"|pbp_refined$StrengthState=="4v3"
                                 |pbp_refined$StrengthState=="6v4"|pbp_refined$StrengthState=="6v3") 
                                & pbp_refined$triCode==pbp_refined$AwayTeam)] = 
    1.028243512*pbp_refined$Shot[which((pbp_refined$StrengthState=="5v4"|pbp_refined$StrengthState=="5v3"|pbp_refined$StrengthState=="4v3"
                                       |pbp_refined$StrengthState=="6v4"|pbp_refined$StrengthState=="6v3") 
                                      & pbp_refined$triCode==pbp_refined$AwayTeam)]
  
  pbp_refined$VenueAdjGoal[which((pbp_refined$StrengthState=="5v4"|pbp_refined$StrengthState=="5v3"|pbp_refined$StrengthState=="4v3"
                                 |pbp_refined$StrengthState=="6v4"|pbp_refined$StrengthState=="6v3") 
                                & pbp_refined$triCode==pbp_refined$AwayTeam)] = 
    1.050994363*pbp_refined$Goal[which((pbp_refined$StrengthState=="5v4"|pbp_refined$StrengthState=="5v3"|pbp_refined$StrengthState=="4v3"
                                       |pbp_refined$StrengthState=="6v4"|pbp_refined$StrengthState=="6v3") 
                                      & pbp_refined$triCode==pbp_refined$AwayTeam)]
  
  
  #EV score adjustment for state 1
  pbp_refined$ScoreAdjCorsi[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                 & pbp_refined$ScoreState==1)] = 
    1.032458755*pbp_refined$CorsiShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                           & pbp_refined$ScoreState==1)]
  
  pbp_refined$ScoreAdjFenwick[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                   & pbp_refined$ScoreState==1)] = 
    1.090843596*pbp_refined$FenwickShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                             & pbp_refined$ScoreState==1)]
  
  pbp_refined$ScoreAdjShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                & pbp_refined$ScoreState==1)] =
    1.085989012*pbp_refined$Shot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                      & pbp_refined$ScoreState==1)]
  
  pbp_refined$ScoreAdjGoal[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                & pbp_refined$ScoreState==2)] =
    0.991549919*pbp_refined$Goal[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                      & pbp_refined$ScoreState==2)]
  
  #EV score adjustment for state 2
  pbp_refined$ScoreAdjCorsi[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                 & pbp_refined$ScoreState==2)] =
    1.051536429*pbp_refined$CorsiShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                           & pbp_refined$ScoreState==2)]
  
  pbp_refined$ScoreAdjFenwick[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                   & pbp_refined$ScoreState==2)] = 
    1.141030147*pbp_refined$FenwickShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                             & pbp_refined$ScoreState==2)]
  
  pbp_refined$ScoreAdjShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                & pbp_refined$ScoreState==2)] =
    1.138354059*pbp_refined$Shot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                      & pbp_refined$ScoreState==2)]
  
  pbp_refined$ScoreAdjGoal[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                & pbp_refined$ScoreState==3)] =
    0.937821812*pbp_refined$Goal[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                      & pbp_refined$ScoreState==3)]
  
  #EV score adjustment for state 3
  pbp_refined$ScoreAdjCorsi[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                 & pbp_refined$ScoreState==3)] = 
    1.080070139*pbp_refined$CorsiShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                           & pbp_refined$ScoreState==3)]
  
  pbp_refined$ScoreAdjFenwick[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                   & pbp_refined$ScoreState==3)] = 
    1.196670141*pbp_refined$FenwickShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                             & pbp_refined$ScoreState==3)]
  
  pbp_refined$ScoreAdjShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                & pbp_refined$ScoreState==3)] =
    1.198199572*pbp_refined$Shot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                      & pbp_refined$ScoreState==3)]
  
  pbp_refined$ScoreAdjGoal[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                & pbp_refined$ScoreState==4)] =
    0.987128971*pbp_refined$Goal[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                      & pbp_refined$ScoreState==4)]
  
  
  #EV score adjustment for state 4+
  pbp_refined$ScoreAdjCorsi[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                 & pbp_refined$ScoreState>3)] = 
    1.079437973*pbp_refined$CorsiShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                           & pbp_refined$ScoreState>3)]
  
  pbp_refined$ScoreAdjFenwick[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                   & pbp_refined$ScoreState>3)] = 
    1.16730703*pbp_refined$FenwickShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                            & pbp_refined$ScoreState>3)]
  
  pbp_refined$ScoreAdjShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                & pbp_refined$ScoreState>3)] =
    1.177140606*pbp_refined$Shot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                      & pbp_refined$ScoreState>3)]
  
  pbp_refined$ScoreAdjGoal[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                & pbp_refined$ScoreState>4)] =
    0.995451994*pbp_refined$Goal[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                      & pbp_refined$ScoreState>4)]
  
  
  #EV score adjustment for state -1
  pbp_refined$ScoreAdjCorsi[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                 & pbp_refined$ScoreState==-1)] = 
    0.998742185*pbp_refined$CorsiShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                           & pbp_refined$ScoreState==-1)]
  
  pbp_refined$ScoreAdjFenwick[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                   & pbp_refined$ScoreState==-1)] = 
    0.954070617*pbp_refined$FenwickShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                             & pbp_refined$ScoreState==-1)]
  
  pbp_refined$ScoreAdjShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                & pbp_refined$ScoreState==-1)] =
    0.95568828*pbp_refined$Shot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                     & pbp_refined$ScoreState==-1)]
  
  pbp_refined$ScoreAdjGoal[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                & pbp_refined$ScoreState==0)] =
    0.939417092*pbp_refined$Goal[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                      & pbp_refined$ScoreState==0)]
  
  #EV score adjustment for state -2
  pbp_refined$ScoreAdjCorsi[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                 & pbp_refined$ScoreState==-2)] = 
    1.006533521*pbp_refined$CorsiShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                           & pbp_refined$ScoreState==-2)]
  
  pbp_refined$ScoreAdjFenwick[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                   & pbp_refined$ScoreState==-2)] = 
    0.935396178*pbp_refined$FenwickShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                             & pbp_refined$ScoreState==-2)]
  
  pbp_refined$ScoreAdjShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                & pbp_refined$ScoreState==-2)] =
    0.937637503*pbp_refined$Shot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                      & pbp_refined$ScoreState==-2)]
  
  pbp_refined$ScoreAdjGoal[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                & pbp_refined$ScoreState==-1)] =
    0.915624112*pbp_refined$Goal[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                      & pbp_refined$ScoreState==-1)]
  
  #EV score adjustment for state -3
  pbp_refined$ScoreAdjCorsi[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                 & pbp_refined$ScoreState==-3)] = 
    1.022796621*pbp_refined$CorsiShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                           & pbp_refined$ScoreState==-3)]
  
  pbp_refined$ScoreAdjFenwick[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                   & pbp_refined$ScoreState==-3)] =
    0.932526986*pbp_refined$FenwickShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                             & pbp_refined$ScoreState==-3)]
  
  pbp_refined$ScoreAdjShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                & pbp_refined$ScoreState==-3)] =
    0.932784349*pbp_refined$Shot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                      & pbp_refined$ScoreState==-3)]
  
  pbp_refined$ScoreAdjGoal[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                & pbp_refined$ScoreState==-2)] =
    0.955549416*pbp_refined$Goal[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                      & pbp_refined$ScoreState==-2)]
  
  
  #EV score adjustment for state -4 and below
  pbp_refined$ScoreAdjCorsi[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                 & pbp_refined$ScoreState<(-3))] = 
    1.037576358*pbp_refined$CorsiShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                           & pbp_refined$ScoreState<(-3))]
  
  pbp_refined$ScoreAdjFenwick[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                   & pbp_refined$ScoreState<(-3))] = 
    0.935031977*pbp_refined$FenwickShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                             & pbp_refined$ScoreState<(-3))]
  
  pbp_refined$ScoreAdjShot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                & pbp_refined$ScoreState<(-3))] =
    0.940298369*pbp_refined$Shot[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                      & pbp_refined$ScoreState<(-3))]
  
  pbp_refined$ScoreAdjGoal[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                & pbp_refined$ScoreState<(-2))] =
    1.075037696*pbp_refined$Goal[which((pbp_refined$StrengthState=="5v5"|pbp_refined$StrengthState=="4v4"|pbp_refined$StrengthState=="3v3") 
                                      & pbp_refined$ScoreState<(-2))]
  
  #In short, venue adjustment is performed for EV and PP situations, score adjustment for EV situations 
  
  
  pbp_refined =pbp_refined %>% mutate(ScoreVenueAdjCorsi = ScoreAdjCorsi*VenueAdjCorsi,
                                    ScoreVenueAdjFenwick = ScoreAdjFenwick*VenueAdjFenwick,
                                    ScoreVenueAdjShot = ScoreAdjShot*VenueAdjShot,
                                    ScoreVenueAdjGoal = ScoreAdjGoal*VenueAdjGoal)
  
  
  
  # Shot Quality

 
  
  qualityInfo= filter(pbp_refined,
         eventType=="Shot" | eventType=="Goal") %>%
    filter(!is.na(SecondaryType) & !is.na(angle) & !is.na(distance) & Zone!="Def") %>%
    mutate(HomeOrAway = ifelse(triCode==HomeTeam,1,0)) %>%
    mutate(StrengthState = as.factor(StrengthState),SecondaryType = as.factor(SecondaryType)) %>%
    select(HomeOrAway,SecondaryType,distance,angle,Rebound,Rush,StrengthState,ScoreState)
  
  
  # The game won't have all of the factor levels that went into the training of the model, so we need to add those in
  # Otherwise, for lack of a more precise explanation, the model gives really screwy results
  # So we need to make sure that the factor levels are all there and in the right order
  
  
  dummyRow = data.frame(HomeOrAway=0,SecondaryType="Wrist Shot",distance=1,angle=1,Rebound=0,Rush=0,StrengthState="5v5",ScoreState=0)
  levels(dummyRow$SecondaryType) = factor(c("Backhand","Deflected","Slap Shot","Snap Shot","Tip-In","Wrap-around","Wrist Shot" ))
  levels(dummyRow$StrengthState) = factor(c("0v0","0v1","1v0","1v1","1v4","1v5","3v3","3v4","3v5","4v1","4v3","4v4","4v5","4v6","5v0","5v1","5v2","5v3","5v4","5v5","5v6","6v3","6v4","6v5"))
  
  qualityInfoPrime = rbind(dummyRow,qualityInfo)
  
  ShotQualityProbs = predict(ShotQualityModelAllPreds,newdata = qualityInfoPrime)
  ShotQualityProbs=ShotQualityProbs[-1]
  
  
  
   
  pbp_refined = mutate(pbp_refined,ShotQualityProb=NA)

  pbp_refined$ShotQualityProb[which(pbp_refined$Shot==1 & !is.na(pbp_refined$SecondaryType) 
                                    & !is.na(pbp_refined$angle) & !is.na(pbp_refined$distance) 
                                    & pbp_refined$Zone!="Def")] = ShotQualityProbs

  return(pbp_refined)
  
  
}