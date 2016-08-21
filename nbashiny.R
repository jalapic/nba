#globals
setwd("C:/Users/curley1/Dropbox/Work/R/nba")
mydf<-readRDS("nbagamedata.RData")
source("https://raw.githubusercontent.com/jalapic/nba/master/playbyplayfunctions.R")

library(RCurl)
library(jsonlite)
library(dplyr)
library(sp)
library(ggplot2)
library(data.table)
library(gganimate)
library(shiny)




ui <- basicPage(
  
  titlePanel("NBA Play-by-Play Animations"),
  
  mainPanel(
  
  h6("This is a proof-of-principle shiny app. It isn't meant to be too user-friendly right now"),

  h6("It creates animated visualizations of NBA play-by-play data using gganimate. The purpose of this app is to demo how to do this using shiny - something which is quite difficult if compiling hundreds of images into an animation."),  

  h6("You will need some serious patience to use it! - it is very slow to compile the animations."),

  h5("Don't worry! They are being processed - it really is this slow right now!"),
  
  
    selectInput("select", label = h3("Select play"), 
              choices = list("Choice 1" = 56, "Choice 2" = 451, "Choice 3" = 452, "Choice 4" = 389), 
              selected = 56)
  
  ),
  
  imageOutput("plot1")
  )

server <- function(input, output) {
  

  
  
    
  output$plot1 <- renderImage({
    
    #Get Data

    val <- as.numeric(input$select)
    
    id303 <- mydf[which(mydf$event.id == val),]
    
    clocktimes = rev(sort(unique(id303$game_clock)))
    
    fulldf=list()
    
    for(i in seq_along(clocktimes)){
      
      dplayer <- player_position1(df=id303, val,clocktimes[i]) #Gets positions of players
      dchull <- chull_plot(df=id303, val,clocktimes[i])       #Gets area of convex hull
      ballpos <- ball_position1(df=id303, val,clocktimes[i])  #Gets position of ball
      dchull$jersey = "NA"
      dplayer$valx = 'player'
      dchull$valx = 'hull'
      ballpos$valx  = 'ball'
      fulldf[[i]] = rbind(dplayer,dchull,ballpos)
    }
    
    
    fulldf = Map(cbind,fulldf,timebin=1:length(fulldf))  #add time unit 
    
    xxx <- which(lapply(fulldf, nrow) %>% unlist > 23)
    
    playdf = data.table::rbindlist(fulldf)

    playdf2 = playdf %>% filter(! timebin %in% xxx) %>% filter(timebin<464) #rework this later - trying to not overplot.
    
    # now make the animation
    p = fullcourt() + 
      geom_point(data=playdf2 %>% filter(valx=="player"),aes(x=X,y=Y,group=ID,color=factor(ID),frame=timebin),size=6) +
      geom_text(data=playdf2 %>% filter(valx=="player"),aes(x=X,y=Y,group=ID,frame=timebin,label=jersey),color='black') +
      geom_polygon(data=playdf2 %>% filter(valx=="hull"),aes(x=X,y=Y,group=ID,fill=factor(ID),frame=timebin),alpha = 0.2) + 
      geom_point(data=playdf2 %>% filter(valx=="ball"),aes(x=X,y=Y,frame=timebin),color='darkorange',size=3) +
      scale_color_manual(values=c("lightsteelblue2","orangered2")) +
      scale_fill_manual(values=c("lightsteelblue2","orangered2")) +
      theme(legend.position="none")
    
    
    
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    
    gg_animate(p, "outfile.gif", title_frame =F, ani.width = 600, ani.height = 450, interval=0.1)  
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)
  }

shinyApp(ui, server)
