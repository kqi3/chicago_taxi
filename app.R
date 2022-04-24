#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)
library(plyr)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(lubridate)
library(data.table)
library(dplyr)
library(tidyr)
library(scales)
library(DT)
library(leaflet)
library(ggmap)
library(maps)
library(mapdata)
library(ggthemes)
library(sp)
library(stringr)
library(rgdal)
# 
# chicago_taxi <- read.csv("Taxi_Trips_-_2019.csv",sep=",",quote="",na.strings ="NA",stringsAsFactors = FALSE )
# taxi<- chicago_taxi[c("Trip.Start.Timestamp","Trip.Seconds","Trip.Miles","Pickup.Community.Area","Dropoff.Community.Area","Company")]
# chicago_taxi<-NULL

taxi5 <- do.call(rbind,lapply(list.files(pattern="*.csv"), fread) )
taxi5$V1<-NULL
taxi5$X <-NULL
taxi5 <- data.frame(taxi5)

# # temp = list.files(pattern="*.csv")
# # data_joined2 <- lapply(temp, read.csv)
# # taxi5 <- rbind.fill(data_joined2)
# taxi5$V1<-NULL
# taxi5$X<-NULL
# taxi5$newDatetime2 <- strptime(taxi5$Trip.Start.Timestamp,format = "%m/%d/%Y %I:%M:%S %p")
# taxi5$newDatetime3 <- ymd_hms(taxi5$newDatetime2,tz=Sys.timezone())
# # 
# taxi1<- subset(taxi, taxi$Trip.Seconds>=60 & taxi$Trip.Seconds<=18000)
# taxi2<- subset(taxi1,taxi1$Trip.Miles>=0.5 & taxi1$Trip.Miles<=100)
# taxi3<- subset(taxi2,taxi2$Pickup.Community.Area>=1 & taxi2$Pickup.Community.Area<=77)
# taxi4<- subset(taxi3,taxi3$Dropoff.Community.Area>=1 & taxi3$Dropoff.Community.Area<=77)
# # #delete quotes in company names
# taxi5<- as.data.frame(sapply(taxi4,function(x) gsub("\"","",x)))
# #abbreviate company name
# # taxi5$company_abbr<- abbreviate(taxi5$Company,10,method = "both")
# # taxi5$newDatetime<-NULL
# # taxi5$newDatetime2<-NULL
# # 
# taxi5$date <- as.Date(taxi5$newDatetime2)
# taxi5$month <- month(taxi5$date)
# taxi5$day <- day(taxi5$date)
# taxi5$hour<- hour(taxi5$newDatetime2)
# #
# taxi5$day_week <- wday(as.Date(taxi5$date))
#taxi5$rides<-1
#taxi5$rides<- as.integer(taxi5$rides)
taxi5$date<-as.Date(strptime(taxi5$Trip.Start.Timestamp,format = "%m/%d/%Y %I:%M:%S %p"))
taxi5$hour<-hour(strptime(taxi5$Trip.Start.Timestamp,format = "%m/%d/%Y %I:%M:%S %p"))
# taxi5$hour_12<-format(strptime(taxi5$Trip.Start.Timestamp,format = "%m/%d/%Y %I:%M:%S %p"),"%I%p")
daily_plot<-taxi5%>%group_by(date)%>%dplyr ::summarise(rides=n())
hour24_plot<- taxi5%>%group_by(hour)%>%dplyr::summarise(rides=n())
day0fweek_plot <-taxi5%>%group_by(dayofweek=weekdays(date))%>%dplyr::summarise(rides=n())
month_plot <-    taxi5%>%group_by(month= month.abb[month(date)] )%>%dplyr::summarise(rides=n())
chicago_spdf<- readOGR(dsn=paste0(getwd(),"/Boundaries - Community Areas (current)/"),layer = "geo_export_8df5a09e-dda2-4cf5-af68-cf5d2bc5f459",verbose=FALSE)


# taxi5$time <- taxi5$Trip.Start.Timestamp
# # #extract time and AM/PM from starttimestamp
# taxi5$time2<- gsub(".+? ","",taxi5$time)
# taxi5$time3<-gsub("^\\S* ","",taxi5$time)
# taxi5$time4 <- str_extract(taxi5$time3, "^\\d{2}")
# taxi5$time2 <- tolower(taxi5$time2)
# taxi5$am_pm <- paste(taxi5$time4,taxi5$time2,sep="")
# taxi5$time<-NULL
# taxi5$time2<-NULL
# taxi5$time3<-NULL
# taxi5$time4<-NULL
miles_breaks<-c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,10,20,100)
miles_tags<-c("[0.5-1)","[1-1.5)","[1.5-2)","[2-2.5)","[2.5-3)","[3-3.5)","[3.5-4)","[4-4.5)","[4.5-5)","[5-10)","[10-20)","[20-100)")
kilo_breaks<-c(0.8,1.6,2.4,3.2,4,4.8,5.6,6.4,7.2,8,16,32,160.9)
kilo_tags <-c("[0.8-1.6)","[1.6-2.4)","[2.4-3.2)","[3.2-4)","[4-4.8)","[4.8-5.6)","[5.6-6.4)","[6.4-7.2)","[7.2-8)","[8-16)","[16-32)","[32-161)")
time_breaks<-c(0,3.5,7,10.5,14,17.5,21,24.5,28,31.5,301)
time_tags <-c("[0-3.5)","[3.5-7)","[7-10.5)","[10.5-14)","[14-17.5)","[17.5-21)","[21-24.5)","[24.5-28)","[28-31.5)","[31.5-301)")
community_info <- read.csv("./taxi_data/CommAreas.csv")
community_info <- community_info[c("AREA_NUMBE","COMMUNITY","SHAPE_AREA","SHAPE_LEN")]
colnames(community_info)<- c("area_number","community","SHAPE_AREA","SHAPE_LEN")
community_info$community<- tolower(community_info$community)
community_level<-levels(as.factor(community_info$community))

#load("~/Documents/cs424project3/chicago_taxi/.RData")
#load("/srv/shiny-server/g7/chicago_taxi/.RData")
# Define UI for application that draws a histogram
distanceSequence<- c("kilometers","miles")
ampmSequence<-c("12am","01am","02am","03am","04am","05am","06am","07am","08am","09am","10am","11am","12pm","01pm","02pm","03pm","04pm","05pm","06pm","07pm","08pm","09pm","10pm","11pm")
hourSequence<-c("12hourbyAM/PM","24hour")
weekdaySequence<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
monthSequence <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
fromtoSequence <- c("from","to")
initial_chicago_lat = 41.881832
initial_chicago_lng = -87.623177
initial_chicago_zoom = 12

#taxi5$month <- month.abb[taxi5$month]
ui <- dashboardPage(
  dashboardHeader(title="CS 424 Project 3 Spring 2022-Chicago Taxi"),
  dashboardSidebar(
    HTML("<br><br><br>"),
    HTML("<br><br><br>"),
    HTML("<br><br><br>"),
    sidebarMenu( id="menu1",
                 HTML("<br><br><br><br><br>"),
                 menuItem("Dashboard", tabName = "dashboard", icon = icon("clock")),
                 menuItem("About", tabName = "about", icon = icon("info"))
    ),
    selectInput("distanceselection", "Select the kilometers or miles", distanceSequence, selected = "miles"),
    selectInput("hourselection","Select the hour format",hourSequence,selected ="24hour"),
    selectInput("fromtoselection","Select from/to",fromtoSequence,selected ="from"),
    selectInput("communityselection","Select community",community_level,selected ="oakland")
    ),
  dashboardBody(
    tabItems(
        tabItem(tabName = "dashboard", 
            fluidRow(
                column(6, 
                       fluidRow(
                         box(title = "Bar Chart by Day of Year from Jan 1 through Dec 31", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 12,
                             plotOutput("totalridesbyday"),height = 500),
                         
                       ), # row 1   /col 1
                       fluidRow(
                         box(title = "Chicago TAXI Leaflet", solidHeader = TRUE, status = "primary", width = 8,  
                             leafletOutput("leaf_taxi", height = 550)
                             
                         ),
                         box(title = "Table by Day of Year", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 4,
                             dataTableOutput("bydayofyear_table"),height=600)
                         
                       ),
                       
                ), # row 5 / column 1,col 2
                HTML("<br>"),
                HTML("<br>"),
                HTML("<br>"),
                HTML("<br>"),
                column(6,
                       
                       fluidRow(
                         box(title = "Bar Chart By Hour Of Day Based On Start Time", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 5,
                             plotOutput("byhourstart"),height = 500),
                         box(title = "By Hour Of Day(Table)", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 2,
                             dataTableOutput("byhourstart_table"),height = 500),
                         box(title = "by day of week", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 3,
                             plotOutput("bydayofweek"),height = 500),
                         box(title = "By Day of Week(Table)", status = "primary", solidHeader = TRUE, collapsible=TRUE, width =2,
                             dataTableOutput("bydayofweek_table"),height = 500)
                       ), # row 1
                       
                       
                       fluidRow(
                         box(title = "Total Rides by Each Month", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 4,
                             plotOutput("totalbymonth"),height = 500),
                         box(title = "Total Rides by Each Month(Table)", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 2,
                             dataTableOutput("total_month_table"),height = 500),
                         box(title = "By Binned Miles/Kilometer", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 4,
                             plotOutput("bybinmiles"),height = 500),
                         box(title = "By Binned Miles/Kilometer(Table)", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 2,
                             dataTableOutput("bybinmiles_table"),height = 500)
                         
                       ), # row 2  /col1,col2
                       fluidRow(
                         box(title = "Total Rides by Binned Time ", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 4,
                             plotOutput("bybintime"),height = 500),
                         box(title = "Total Rides by Binned Time(Table)", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 2,
                             dataTableOutput("bybintime_table"),height = 500),
                         # box(title = "By Binned Miles/Kilometer", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 4,
                         #     plotOutput("bybinmiles"),height = 500),
                         # box(title = "By Binned Miles/Kilometer(Table)", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 2,
                         #     dataTableOutput("bybinmiles_table"),height = 500)
                         
                       ), # row 3  /col1,col2
                       
                   
                ) # end col 2
                
              )  # end fluidRow
      ), # end tab item 
      
      tabItem(tabName = "about",
              h2("About Group"),
              p("This project 3 was made by Kai Qi and Haoxuan Zeng for CS 424 Spring 2022"),
              p("The original data is available from the Chicago Data Portal. The 2019 data is available at:
https://data.cityofchicago.org/Transportation/Taxi-Trips-2019/h4cq-z3dy"),
              p("Community area boundaries are also available on the Chicago data portal - https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6"),
              p("The project was writen during at the end of Apr 20th to Apr,23th,2022")
      ) # end tab item 
    )
  )
    
   
)# end page

# Define server logic required to draw a histogram
server <- function(input, output) {
  # increase the default font size
  theme_set(theme_grey(base_size = 15)) 
  #define a reactive data to reflect in bar char 
  jusOneYearbyDayReactive <- reactive({daily_plot})
  
  
 #Output bar chart by day of year  
  output$totalridesbyday <-renderPlot({
    justOneYearbyDayAll <- jusOneYearbyDayReactive()
    
    ggplot(justOneYearbyDayAll, aes(x=date,y=rides) )+geom_bar(fill="steelblue",stat='identity',width=0.7)+
      labs(title="The Total Taxi Trips in Chicago by Day of Year",  x="day", y = "The total trips number")+
      theme(plot.title = element_text(hjust=0.5, face="bold"),axis.text.x=element_text(angle=45,hjust=1,vjust=1))+
      scale_y_continuous(labels=label_comma())
  })
  #bar chart by hour of day base on starttime
  output$byhourstart<- renderPlot({
    if(input$hourselection=="24hour"){
      by_hour_df<- hour24_plot
      colnames(by_hour_df)<- c("hour_24","total_rides")
      ggplot(by_hour_df)+geom_bar(aes(x=hour_24,y=total_rides),stat='identity',fill="steelblue")+labs(title="The number of rides by hour of day based on start time",  x="24 hours of day", y = "The total trips number")+theme(plot.title = element_text(hjust=0.5, face="bold"))+
        scale_y_continuous(labels=label_comma())
    }
    else{
      by_hour_df<-hour24_plot
      by_hour_df$hour_ampm <-ampmSequence
      
      colnames(by_hour_df)<-c("hour","total_rides","hour_ampm")
      by_hour_df$hour_ampm <-factor(by_hour_df$hour_ampm,levels=ampmSequence)
      ggplot(by_hour_df)+geom_bar(aes(x=hour_ampm,y=total_rides),stat='identity',fill='steelblue')+labs(title="The number of rides by hour of day based on start time",  x="12 hour am/pm of day", y = "The total trips number")+theme(plot.title = element_text(hjust=0.5, face="bold"))+
        scale_y_continuous(labels=label_comma())
    }
  })
  #output barchart rides by day of week
  output$bydayofweek <- renderPlot({
    bydayofweek_df <- day0fweek_plot
    
    bydayofweek_df$dayofweek <- factor(bydayofweek_df$dayofweek,levels=weekdaySequence)
    ggplot(bydayofweek_df)+geom_bar(aes(x=dayofweek,y=rides),stat='identity',fill='steelblue')+labs(title="By Day of Week (Monday through Sunday)",  x="weekdays", y = "The total trips number")+theme(plot.title = element_text(hjust=0.5, face="bold"))+
      scale_y_continuous(labels=label_comma())

  })
  # 
  #output bar chart rides by month of year
  output$totalbymonth <- renderPlot({
    bymonthofyear_df <- month_plot
    colnames(bymonthofyear_df)<- c("monthofyear","rides")
    bymonthofyear_df$monthofyear <- factor(bymonthofyear_df$monthofyear,levels = monthSequence)
    ggplot(bymonthofyear_df,  aes(fill=bymonthofyear_df$monthofyear,x=bymonthofyear_df$monthofyear, y=bymonthofyear_df$rides)) + geom_col(fill="steelblue")+labs(title="The total rides  each month in a year",
                                                                                                                                 x="Month from January to December", y = "The total rides")+scale_fill_hue(breaks=monthSequence)+theme(plot.title = element_text(hjust=0.5, face="bold"))+scale_y_continuous(labels=label_comma())


  })
  #bar chart by binned miles
  output$bybinmiles<- renderPlot({
   
    if(input$distanceselection=="miles"){
      group_tags<- cut(taxi5$Trip.Miles,breaks = miles_breaks,include.lowest = TRUE,right = FALSE,labels = miles_tags)
      
      ggplot(data = as_tibble(group_tags), mapping = aes(x=value)) + 
        geom_bar(fill="steelblue",color="white",alpha=0.9) + 
        labs(title='By Binne Miles',x='By binned miles',y='Count') +
        theme(plot.title = element_text(hjust=0.5, face="bold"))+
        scale_y_continuous(labels=label_comma())
     
    }
    else{
      kilo_group_tags<- cut(as.numeric(format(round(taxi5$Trip.Miles*1.60934,1),nsmall=1)), breaks=kilo_breaks,include.lowest=TRUE,right=FALSE,labels=kilo_tags )
      
      ggplot(data = as_tibble(kilo_group_tags), mapping = aes(x=value)) + 
        geom_bar(fill="steelblue",color="white",alpha=0.9) + 
        labs(title='By Binne Kilometers',x='By binned kilometers',y='Count') +
        theme(plot.title = element_text(hjust=0.5, face="bold"))+
        scale_y_continuous(labels=label_comma())
      
    }
    
  })# 
  #bar char by binned time trips
  output$bybintime<- renderPlot({
    time_group_tags<- cut(as.numeric(format(round(taxi5$Trip.Seconds/60,1),nsmall=1)),breaks = time_breaks,include.lowest = TRUE,right = FALSE,labels = time_tags)
    
    ggplot(data = as_tibble(time_group_tags), mapping = aes(x=value)) + 
      geom_bar(fill="steelblue",color="white",alpha=0.9) + 
      labs(title='By Binned Time',x='By binned time Unit: minutes',y='Count') +
      theme(plot.title = element_text(hjust=0.5, face="bold"))+
      scale_y_continuous(labels=label_comma())
    
    
  })
  # 
  #Output table by day of year
  output$bydayofyear_table <- renderDataTable({
    byday <- jusOneYearbyDayReactive()
    #byday <- aggregate.data.frame(justOneYearbyDayAll$rides, by=list(justOneYearbyDayAll$date), FUN = sum)
    colnames(byday)<- c("Day","Rides")
    byday
  },rownames=FALSE,options=list(lengthMenu = list(c(5, 8), c('5', '8')), list(searching = FALSE),autoWidth=TRUE, order=list(1, 'asc')))
  # 
  # #Output table by hour of day based on starttime
  output$byhourstart_table <- renderDataTable({
    if(input$hourselection=="24hour"){
      by_hour_df<- hour24_plot
      colnames(by_hour_df)<- c("hour_24","total_rides")
      by_hour_df
    }
    else{
      by_hour_df<-hour24_plot
      by_hour_df$hour_ampm <-ampmSequence
      
      colnames(by_hour_df)<-c("hour","total_rides","hour_ampm")
      by_hour_df$hour_ampm <-factor(by_hour_df$hour_ampm,levels=ampmSequence)
      by_hour_df%>%select("hour_ampm","total_rides")
    }
  },rownames=FALSE,options=list(lengthMenu = list(c(4, 6), c('4', '6')), list(searching = FALSE),autoWidth=TRUE, order=list(1, 'asc')))
  # 
  #Output table by day of week
  output$bydayofweek_table <- renderDataTable({
    bydayofweek_df <- day0fweek_plot
    
    bydayofweek_df$dayofweek <-factor(bydayofweek_df$dayofweek,levels=weekdaySequence)
    bydayofweek_df
  },rownames=FALSE,options=list(lengthMenu = list(c(7, 7), c('7', '7')), list(searching = FALSE),autoWidth=TRUE, order=list(1, 'asc')))

  #Output table by month of year
  output$total_month_table<- renderDataTable({
    bymonthofyear_df <- month_plot
    colnames(bymonthofyear_df)<- c("month","rides")
    bymonthofyear_df$month <- factor(bymonthofyear_df$month, levels= monthSequence)
    bymonthofyear_df
  },rownames=FALSE,options=list(lengthMenu = list(c(3, 6), c('3', '6')), list(searching = FALSE),autoWidth=TRUE, order=list(1, 'asc')))
 #binned mile/kilo table 
  output$bybinmiles_table <- renderDataTable({
    mile_plot<- taxi5%>%mutate(mile_bin=cut(Trip.Miles,breaks=c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,10,20,100),include.lowest = TRUE,right = FALSE) )%>%group_by(mile_bin)%>%dplyr::summarise(rides=n())
    kilo_plot<- taxi5%>%mutate(kilo_bin=cut(as.numeric(format(round(taxi5$Trip.Miles*1.60934,1),nsmall=1)),breaks=c(0.8,1.6,2.4,3.2,4,4.8,5.6,6.4,7.2,8,16,32,161), include.lowerst=TRUE,right=FALSE) )%>%group_by(kilo_bin)%>%dplyr::summarise(rides=n() )  
    
    if(input$distanceselection=="miles"){
      mile_plot$miles<-miles_tags
      mile_df <- select(mile_plot,"miles","rides")
      mile_df$miles <-factor(mile_df$miles,levels=miles_tags,ordered = TRUE)
      mile_df
    }
    else{
      kilo_plot$kilometer <- kilo_tags
      kilo_df<- select(kilo_plot,"kilometer","rides")
      kilo_df$kilometer<- factor(kilo_df$kilometer,levels=kilo_tags,ordered=TRUE)
      kilo_df
    }
  },rownames=FALSE,options=list(lengthMenu = list(c(3, 6), c('3', '6')), list(searching = FALSE),autoWidth=TRUE, order=list(1, 'asc')))
  # by bin time table
  output$bybintime_table <- renderDataTable({
    time_plot<- taxi5%>%mutate(minutes_bin=cut(as.numeric(format(round(taxi5$Trip.Seconds/60,1),nsmall=1)),breaks=time_breaks, include.lowerst=TRUE,right=FALSE) )%>%group_by(minutes_bin)%>%dplyr::summarise(rides=n() )  

    time_plot$minutes <- time_tags
    time_df<- select(time_plot,"minutes","rides")
    time_df$minutes<- factor(time_df$minutes,levels=time_tags,ordered=TRUE)
    time_df
    
  },rownames=FALSE,options=list(lengthMenu = list(c(3, 6), c('3', '6')), list(searching = FALSE),autoWidth=TRUE, order=list(1, 'asc')))
  # 
  observe({
    
    
    chicago_map <- leaflet() %>%
      
      addTiles(
        group = "OSM"
      ) %>%  # Add default OpenStreetMap map tiles
      
      addProviderTiles(
        providers$CartoDB.Positron, group = "Light"
      ) %>%
      addProviderTiles(
        providers$CartoDB.DarkMatter, group = "viridis"
      ) %>%
     setView(
        lng = initial_chicago_lng,
        lat = initial_chicago_lat, 
        zoom = initial_chicago_zoom
      ) %>%
      addPolygons(data=chicago_spdf,weight=5,col = 'blue') %>% 
      addLayersControl(
        baseGroups = c("OSM", "Light", "Dark"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    output$leaf_taxi <-renderLeaflet({ 
      chicago_map
    })
    
  })
  
  # observe({
  #   input$fromtoselection
  #   proxy <- leafletProxy('leaf_taxi')
  #   if(input$fromtoselection=="from"){
  #     all_pickupdata<- dplyr::left_join(x=taxi5,y=community_info,by=c("Pickup.Community.Area"="area_number"))%>%group_by(community)%>%dplyr::summarise(rides=n())
  #     
  #     pal<-colorBin(palette="" ,domain =all_pickupdata$rides ,bins=10,reverse = TRUE )
  #     proxy%>% clearMarkers() %>%
  #       clearShapes()%>%
  #       addPolygons(data=chicago_spdf,weight=0.7,color=~pal(as.numeric(all_pickupdata$rides)),fillOpacity = 0.5,stroke = TRUE)
  #     
  #      
  #   }
  #   else{
  #   }
  #   
  #   
  # }) 
  # 
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
