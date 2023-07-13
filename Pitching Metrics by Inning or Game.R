library(plyr)
library(tidyverse)
library(ggplot2)
library(plotly)


  # load csv
  csv <- read.csv("sample csv.csv") %>%
    mutate(Date = as.Date(Date, "%m/%d/%Y"))
  # see pitchers in data set
  unique(csv$Pitcher)
  # Average Velo or other metric by inning ----
  
  
{  # filter by specific pitcher and group by inning!
  pitcher <- csv %>% 
    filter(grepl("Schlotman" , Pitcher)) %>%
    # factor TaggedPitchType so that the order is always the same
    dplyr:: mutate(TaggedPitchType = factor(TaggedPitchType, levels = c("Fastball", "Sinker", "Cutter","Curveball",
                                                                        "Slider", "Changeup", "Splitter", 'Knuckleball')),
                   # recode TaggedPitchType so the pitch names are shorter
                   TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT', Splitter = 'SPL', Knuckleball = 'KN' )) %>%
    group_by(Date, Inning, Pitcher, TaggedPitchType) %>%
    # create all potential variables for the Y axis
    summarise(VeloAvg = mean(RelSpeed, na.rm = TRUE), SpinAvg = mean(SpinRate, na.rm = TRUE), VB_Avg = mean(InducedVertBreak, na.rm = T), 
              HB_Avg = mean(HorzBreak, na.rm = T), VAA_Avg = mean(VertApprAngle, na.rm = TRUE), PitchCount = n())
  
  # create a text hover for the plotly
  pitch_info = paste('\nPitches: ', pitcher$PitchCount,
                     '\nAvg Velo:', round(pitcher$VeloAvg,1),
                     '\nAvg Spin:', round(pitcher$SpinAvg,1),
                     '\nAvg VB:', round(pitcher$VB_Avg,1),
                     '\nAvg HB:', round(pitcher$HB_Avg,1),
                     '\nAvg VAA:', round(pitcher$VAA_Avg,1 ))
  
  m <- list( # manually set margins for plotly
    l = 50,
    r = 50,
    b = 50,
    t = 50
  )
  
  ggplotly(
    ggplot(data = pitcher, aes(x = Inning, y = VeloAvg, color = TaggedPitchType, group = TaggedPitchType, label = pitch_info ) ) +
      geom_point( size = 2, alpha = .75) +
      geom_line() +
      scale_x_continuous(breaks = seq(1, 9), labels = seq(1, 9)) +
      scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                    'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) + # , na.rm = TRUE)+
      labs(x = "Date", y = "Pitch Velocity (MPH)", color = " " )+ #, title = "Pitch Velocity") +
      theme_bw() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5), axis.text = element_text(size = 12)) +
      theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_text(size = 14)) 
    )  %>% 
    layout(autosize = T,
           margin = m,
           showlegend = TRUE,
           legend = list(orientation = "v",   # show entries horizontally
                         xanchor = "left",  # use left side of legend as anchor
                         x = 1, # place legend on right side. values are 0 for left, .5 for center, 1 for right
                         y = .5),  # place legend in middle. values are 0 for bottom, .5 for center, 1 for top
           # plotly titles can sometimes be messy, so here is a way i found to get title and subtitle
           title = list(text = paste0('Velo by Inning',
                                      '<br>',
                                      '<sup>',
                                      pitcher$Pitcher[1], ' - ', format(pitcher$Date[1],"%b %d" ) ) ),
           # change the orientation of the xaxis inning labels, and add a range slider at the bottom
           xaxis = list(tickangle = -45 , rangeslider = list(thickness = .1),
                        tickfont = list(size = 12) ),
           yaxis = list(tickfont = list(size = 12) ) )
  
  
}
  
  
  
  
  
  
  # Average Velo or other metric by date/game ----
  # load csv
{  csv <- read.csv("sample two games.csv") %>%
    mutate(Date = as.Date(Date, "%m/%d/%Y"))

  # see pitchers in data set
  unique(csv2$Pitcher)

  unique(csv2$Pitcher )
  
  # filter by specific pitcher and group from game to game!
   pitcher <- csv2 %>% 
    filter(grepl("Schlotman" , Pitcher)) %>%
    # factor TaggedPitchType so that the order is always the same
    dplyr:: mutate(TaggedPitchType = factor(TaggedPitchType, levels = c("Fastball", "Sinker", "Cutter","Curveball",
                                                                        "Slider", "Changeup", "Splitter", 'Knuckleball')),
                   # recode TaggedPitchType so the pitch names are shorter
                   TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Other = 'OT', Splitter = 'SPL', Knuckleball = 'KN' )) %>%
    group_by(Date, Pitcher, TaggedPitchType) %>%
   # create all potential variables for the Y axis
   summarise(VeloAvg = mean(RelSpeed, na.rm = TRUE), SpinAvg = mean(SpinRate, na.rm = TRUE), VB_Avg = mean(InducedVertBreak, na.rm = T), 
              HB_Avg = mean(HorzBreak, na.rm = T), VAA_Avg = mean(VertApprAngle, na.rm = TRUE),PitchCount = n()) %>%
    arrange(Date)  %>%
    dplyr:: mutate(Date = format(Date, "%b %d, %y"),
                   Date = factor(Date) )
 
 # create a text hover for the plotly
 pitch_info = paste(pitcher$Date,
                    '\nPitches:', pitcher$PitchCount,
                    '\nAvg Velo:', round(pitcher$VeloAvg,1),
                    '\nAvg Spin:', round(pitcher$SpinAvg,1),
                    '\nAvg VB:', round(pitcher$VB_Avg,1),
                    '\nAvg HB:', round(pitcher$HB_Avg,1),
                    '\nAvg VAA:', round(pitcher$VAA_Avg,1 ))
   
 m <- list( # manually set margin
   l = 50,
   r = 50,
   b = 50,
   t = 50
 )
  
  ggplotly(
    ggplot(data = pitcher, aes(x = Date, y = VeloAvg, color = TaggedPitchType, group = TaggedPitchType, label = pitch_info ) ) +
      geom_point( size = 2, alpha = .75) +
      geom_line() +
      scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                    'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) + # , na.rm = TRUE)+
      labs(x = "Date", y = "Pitch Velocity (MPH)", color = " " )+ #, title = "Pitch Velocity") +
      theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), axis.text = element_text(size = 12)) +
      theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_text(size = 14)))  %>% 
    layout(autosize = T,
           margin = m,
           showlegend = TRUE,
           legend = list(orientation = "v",   # show entries horizontally
                         xanchor = "left",  # use left side of legend as anchor
                         x = 1, # place legend on right side. values are 0 for left, .5 for center, 1 for right
                         y = .5),  # place legend in middle. values are 0 for bottom, .5 for center, 1 for top
         
             # plotly titles can sometimes be messy, so here is a way i found to get title and subtitle
           title = list(text = paste0('Velo by Game',
                                      '<br>',
                                      '<sup>',
                                      pitcher$Pitcher[1] ) ),
           # change the orientation of the xaxis inning labels, and add a range slider at the bottom
           xaxis = list(tickangle = -45 , rangeslider = list(thickness = .1),
                        tickfont = list(size = 12) ),
           yaxis = list(tickfont = list(size = 12) ) )
  
 }
  