###NBA_SportVU FUNCTIONS

library(RCurl)
library(jsonlite)
library(dplyr)
library(sp)



factorconvert <- function(f){as.numeric(levels(f))[f]}

sportvu_convert_json <- function (file.name)
{
  # Much of the process is from http://tcbanalytics.com/blog/nba-movement-data-R.html#.VnX8d4RiOCQ
  # Takes a json and converts it into a dataframe
  the.data.file<-fromJSON(file.name)
  ##Get the sports vu data
  moments <- the.data.file$events$moments
  
  ##Function for extracting infomration from JSON
  extractbb <- function (listbb)
  {#df <- unlist(listbb,recursive = FALSE)
    df <- listbb
    # str(df)
    quarters <- unlist(lapply(df, function(x) x[[1]]))
    game.clock <- unlist(lapply(df, function(x) x[[3]]))
    shot.clock <- unlist(lapply(df, function(x) ifelse(is.null(x[[4]]), 'NA', x[[4]])))
    moment.details <- (lapply(df, function(x) x[[6]]))
    x3 <-  mapply(cbind, moment.details, game.clock, shot.clock,quarters, SIMPLIFY=F)
    x4 <- do.call('rbind', x3)
    return (x4)
  }
  
  test2 <- lapply(moments, function (x) {extractbb(x)})
  lengthmm <- the.data.file$events$eventId
  test2 <- mapply(cbind, test2, "event.id"=lengthmm, SIMPLIFY=F)
  
  #Remove events that are NAs
  final <- (lapply(test2, function(x) {
    if ((length(unlist(x)))<=1) {x <- NA} 
    return(x)
  }))
  
  ###Merge the file
  test2 <- do.call('rbind', final)
  test2 <- as.data.frame(test2)
  test2[test2 == "NA" ] = NA
  all.movement <- test2
  #all.movement<-test2[order(test2$game.clock),]
  
  ##Lets join the movement to player id
  headers = c("team_id", "player_id", "x_loc", "y_loc", "radius", "game_clock", "shot_clock", "quarter","event.id")
  colnames(all.movement) <- headers
  all.movement<-data.frame(all.movement)
  all.movement<-all.movement[order(all.movement$game_clock),]
  
  home.players <- the.data.file$events$home$players[[1]]
  away.players <- the.data.file$events$visitor$players[[1]]
  colnames(home.players)[3] <- "player_id"
  colnames(away.players)[3] <- "player_id"
  
  ## Add the player name information to each movement moment
  home.movements<-merge(home.players, all.movement, by="player_id")
  away.movements<-merge(away.players, all.movement, by="player_id")
  ball.movement<-all.movement %>% filter(player_id == -1)
  ball.movement$jersey <- NA
  ball.movement$position <- NA
  ball.movement$team_id <- NA
  ball.movement$lastname <- "ball"
  ball.movement$firstname <- NA
  all.movements <- rbind(home.movements, away.movements,ball.movement)
  all.movements[, 6:13] <- lapply(all.movements[, 6:13], factorconvert)
  all.movements <- as.data.frame(all.movements) %>% dplyr::arrange(quarter,desc(game_clock),x_loc,y_loc)
  return(all.movements)
}



get_pbp <- function(gameid){
  #Grabs the play by play data from the NBA site
  URL1 <- paste("http://stats.nba.com/stats/playbyplayv2?EndPeriod=10&EndRange=55800&GameID=",gameid,"&RangeType=2&StartPeriod=1&StartRange=0",sep = "")
  the.data.file<-fromJSON(URL1)
  test <-the.data.file$resultSets$rowSet
  test2 <- test[[1]]
  test3 <- data.frame(test2)
  coltest <- the.data.file$resultSets$headers
  colnames(test3) <- coltest[[1]]
  return (test3)
  }


player_position1 <- function(df, eventid,gameclock){
  dfall <- df %>% filter(game_clock == gameclock, event.id==eventid)  %>% 
    filter(lastname!="ball") %>% select (team_id,x_loc,y_loc,jersey)
  colnames(dfall) <- c('ID','X','Y','jersey')
  return(dfall)
}

chull_plot <- function(df, eventid, gameclock) {
  df2 <- player_position1(df, eventid, gameclock)
  df_hull2 <- df2 %>% filter(ID == min(ID)) %>% select(X,Y)
  df_hull3 <- df2 %>% filter(ID == max(ID)) %>% select(X,Y)
  c.hull2 <- chull(df_hull2)
  c.hull3 <- chull(df_hull3)
  c.hull2 <- c(c.hull2, c.hull2[1])
  c.hull3 <- c(c.hull3, c.hull3[1])
  df2 <- as.data.frame(cbind(1,df_hull2[c.hull2 ,]$X,df_hull2[c.hull2 ,]$Y))
  df3 <- as.data.frame(cbind(2,df_hull3[c.hull3 ,]$X,df_hull3[c.hull3 ,]$Y))
  dfall <- rbind(df2,df3)
  colnames(dfall) <- c('ID','X','Y')
  return(dfall)
}


ball_position1 <- function(df,eventid,gameclock){
  dfall <- df %>% filter(game_clock == gameclock, event.id==eventid)  %>% 
    filter(lastname=="ball") %>% select (team_id,x_loc,y_loc,jersey)
  colnames(dfall) <- c('ID','X','Y','jersey')
  return(dfall)
}


#Forked from Ed Küpfer
#https://gist.github.com/edkupfer
#https://gist.github.com/asteves/7266330

library(ggplot2)

fullcourt <- function () {
  
  palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
            "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
  
  #Generate Data for the 3 point linea
  # Define the circle; add a point at the center if the 'pie slice' if the shape is to be filled
  circleFun <- function(center=c(0,5.25), diameter=20.9, npoints=20000, start=0, end=1, filled=TRUE){
    tt <- seq(start*pi, end*pi, length.out=npoints)
    df <- data.frame(
      y = center[1] + diameter / 2 * cos(tt),
      x = center[2] + diameter / 2 * sin(tt)
    )
    return(df)
  }
  
  halfCircle <- circleFun(c(0, 5.25), 20.9*2, start=0, end=1, filled=FALSE) 
  ggplot(data=data.frame(y=1,x=1),aes(x,y))+
    ###halfcourt line:
    geom_path(data=data.frame(x=c(47,47),y=c(0,50)))+
    ###outside boy:
    geom_path(data=data.frame(y=c(0,0,50,50,0),x=c(0,94,94,0,0)))+
    ###solid FT semicircle above FT line:
    geom_path(data=data.frame(y=c((-6000:(-1)/1000)+25,(1:6000/1000)+25),x=c(19+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(y=y,x=x))+
    geom_path(data=data.frame(y=c((-6000:(-1)/1000)+25,(1:6000/1000)+25),x=c(75+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(y=y,x=x))+
    ###dashed FT semicircle below FT line:
    geom_path(data=data.frame(y=c((-6000:(-1)/1000)+25,(1:6000/1000)+25),x=c(19-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(y=y,x=x),linetype='dashed')+
    geom_path(data=data.frame(y=c((-6000:(-1)/1000)+25,(1:6000/1000)+25),x=c(75-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(y=y,x=x),linetype='dashed')+
    ###kex:
    geom_path(data=data.frame(y=c(17,17,33,33,17),x=c(0,19,19,0,0)))+
    geom_path(data=data.frame(y=c(17,17,33,33,17),x=c(94,75,75,94,94)))+
    ###boy inside the kex:
    geom_path(data=data.frame(y=c(19,19,31,31,19),x=c(0,19,19,0,0)))+
    geom_path(data=data.frame(y=c(19,19,31,31,19),x=c(94,75,75,94,94)))+
    ###restricted area semicircle:
    geom_path(data=data.frame(y=c((-4000:(-1)/1000)+25,(1:4000/1000)+25),x=c(5.25+sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2))),aes(y=y,x=x))+
    geom_path(data=data.frame(y=c((-4000:(-1)/1000)+25,(1:4000/1000)+25),x=c(88.75-sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2))),aes(y=y,x=x))+
    ###halfcourt semicircle:
    geom_path(data=data.frame(y=c((-6000:(-1)/1000)+25,(1:6000/1000)+25),x=c(47-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(y=y,x=x))+
    geom_path(data=data.frame(y=c((-6000:(-1)/1000)+25,(1:6000/1000)+25),x=c(47+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(y=y,x=x))+
    ###rim:
    geom_path(data=data.frame(y=c((-750:(-1)/1000)+25,(1:750/1000)+25,(750:1/1000)+25,(-1:-750/1000)+25),x=c(c(5.25+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(5.25-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))),aes(y=y,x=x))+
    geom_path(data=data.frame(y=c((-750:(-1)/1000)+25,(1:750/1000)+25,(750:1/1000)+25,(-1:-750/1000)+25),x=c(c(88.75+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(88.75-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))),aes(y=y,x=x))+
    ###backboard:
    geom_path(data=data.frame(y=c(22,28),x=c(4,4)),lineend='butt')+
    geom_path(data=data.frame(y=c(22,28),x=c(90,90)),lineend='butt')+
    ###three-point line:
    # geom_path(data=data.frame(y=c(-21,-21,-21000:(-1)/1000,1:21000/1000,21,21),x=c(0,169/12,5.25+sqrt(23.75^2-c(-21000:(-1)/1000,1:21000/1000)^2),169/12,0)),aes(y=y,x=x))+
    ###fiy aspect ratio to 1:1
    geom_path(data=halfCircle,aes(x=x,y=y+25))+
    ###Complete the three-point line 
    geom_path(data=data.frame(y=c(4.1,4.1,45.9,45.9),x=c(5.25,0,0,5.25)))+
    geom_path(data=halfCircle,aes(x=94-x,y=y+25))+
    geom_path(data=data.frame(y=c(4.1,4.1,45.9,45.9),x=c(88.75,94,94,88.75)))+
    coord_fixed()+
    
    ###Clean up the Court 
    theme_bw()+theme(panel.grid=element_blank(), legend.title=element_blank(), panel.border=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank(),legend.position="top")}
