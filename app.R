library(markdown)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(stringr)
library(readr)
library(plotly)
library(tidyverse)
library(readr)
library(purrr)
library(tm)
library(wordcloud)
library(rvest)
library(geojsonio)
library(sp) 
library(ggthemes)
library(DT)
library(scales)
library(viridisLite)
library(viridis)
library(RColorBrewer)
library(tidyverse) 
library(ggplot2) 
library(IRdisplay) 
library(leaflet) 
library(httr)



ui <- navbarPage("Fight Covid-19!",
                 
          tabPanel("Research Introduction",icon = icon("question-circle"),
                          
                              tags$img(src = "https://images.squarespace-cdn.com/content/v1/59b9f24c64b05fd6531db026/1584645555929-OQSG7B2V8T0KPRHQPZLR/ke17ZwdGBToddI8pDm48kDFgITcRoterXoQdllT5ciUUqsxRUqqbr1mOJYKfIPR7LoDQ9mXPOjoJoqy81S2I8N_N4V1vUb5AoIIIbLZhVYxCRW4BPu10St3TBAUQYVKcV7ZyRJyI8bwZiMJRrgPaAKqUaXS0tb9q_dTyNVba_kClt3J5x-w6oTQbPni4jzRa/122.jpg?format=2500w"
                                       ,width = 1500,height=650),
                              h1(icon("home"), 'Research Background'),
                              p("Coronavirus (COVID-19) is an illness caused by a virus that has spread throughout the world. COVID-19 symptoms can range from mild (or no symptoms) to severe illness. Since the first identified case in December 2019, the virus has resulted in an ongoing pandemic and causes more than 14.7 million cases across 188 countries and territories. The US has one of the most serious coronavirus outbreaks. According to Johns Hopkins University, there are over 380 million confirmed cases and over 141k deaths in the us. Covid19 also exerted negative impacts on the economy. The unemployment rate is an important indicator that shows the consequences of the pandemic.
"),
                        
              
                  h1(icon("question-circle"), 'Research Question'),
                  tags$ul(
                     tags$li("How can we have an overview of the spread of covid19 across the US since March?"),
                     tags$li("How can we isolate different states and compare their confirmed cases of covid19 over time?"),
                     tags$li("How can we have an overview of the unemployment rate across the US since January?"),
                     tags$li("What's the trend of unemployement rate in 2020?")
                   ),
                   
                   
                  h1(icon("youtube-play"), 'Demo'),
                  HTML('<iframe width=50% height="500"
                   src="https://www.youtube.com/embed/Hh6ddL4mJDY"
                   frameborder="0"allowfullscreen></iframe'),
                 br(),
                   p("To more about the data visualization about the project, please watch the video."),
                  
                  
                  
                  h1(icon("database"), 'Dataset Description/Source'),
                  p("We used data from CSSEGISandData, U.S Census Bureau, U.S. Bureau of Labor Statistics. Basically, data from CSSEGISandData was used for covid-19 mapping and the time-series graphing. 
                    We chose confirmed cases of the 15th of each month (April, May, June, July), then merged them with the population data from U.S Census Bureau of each state to calculate the infection rate. 
                    Also, we plotted the confirmed cases, deaths over time by state level. For the unemployment rate analysis, we used unemployment data of each state from U.S. Bureau of Labor Statistics through 
                    January to June, to create the overview map and barchart. The following links are the sources:"),
                  tags$ul(
                     tags$li(a(href = "https://github.com/CSSEGISandData/COVID-19", target = "_blank", "CSSEGISandData/COVID-19, CSSEGISandData")),
                     tags$li(a(href = "https://github.com/dreamRs/shinyWidgets", "dreamRs/shinyWidgets, dreamRs")),
                     tags$li(a(href = "https://www.bls.gov/lau/", "U.S. BUREAU OF LABOR STATISTICS, Unemployment rate by states")),
                     tags$li(a(href = "https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports_us", "CSSEGISandData/COVID-19"))
                   ),
                   
      
                   
                  h1(icon("pencil"), 'References'),
                  tags$ul(
                    tags$li(a(href = "https://rstudio.github.io/leaflet/choropleths.html", target = "_blank", "Leaflet for R, Choropleths"))),
                  
              
                       
                  h1(icon("group"), 'Team Information'),
                              widgetUserBox(
                                title = "Yanling(Holly) Hao",
                                subtitle = "Carey Business School (Information Systems)",
                                width = 6,
                                background = TRUE,
                                backgroundUrl = "https://www.solidbackgrounds.com/images/2560x1440/2560x1440-pink-solid-color-background.jpg",
                                "She tries to learn more about programming",
                                footer = tags$a(href = "https://www.linkedin.com/in/yanling-hao-hao6/", "Her linkedin Page")
                              ), 
                            widgetUserBox(
                                title = "Leah Fang",
                                subtitle = "Carey Business School (Information Systems)",
                                width = 6,
                                background = TRUE,
                                backgroundUrl = "https://www.solidbackgrounds.com/images/2560x1440/2560x1440-pink-solid-color-background.jpg",
                                "She loves collecting music instruments",
                                footer = tags$a(href = "https://www.linkedin.com/in/liuyi-fang/", "Her linkedin Page")
                              ), 
                   
                   widgetUserBox(
                     title = "Mandy Cheng",
                     subtitle = "Carey Business School (Business Analytics and Risk Management)",
                     width = 6,
                     background = TRUE,
                     backgroundUrl = "https://www.solidbackgrounds.com/images/2560x1440/2560x1440-pink-solid-color-background.jpg",
                     "She loves data stroy"
                   ), 
                   
                   widgetUserBox(
                     title = "Yue(Yvonne) Hao",
                     subtitle = "Carey Business School (Information Systems)",
                     width = 6,
                     background = TRUE,
                     backgroundUrl = "https://www.solidbackgrounds.com/images/2560x1440/2560x1440-pink-solid-color-background.jpg",
                     "She has a super cute cat",
                     footer = tags$a(href = "https://www.linkedin.com/in/yue-hao-08143515b/", "Her linkedin Page")
                   ), 
          ),
            
                 
         
           navbarMenu("COVID-19 Analysis",icon = icon("question-circle"),
            tabPanel("Case Map",icon = icon("map"),
                     sidebarLayout(
                       sidebarPanel(
                         selectInput(
                           inputId = "input1", label = "Month",
                           choices = c("April","May","June","July"), 
                           selected = "July"
                         )
                       ),
                       mainPanel(
                         h1("COVID-19 Infection Rate"),
                         leafletOutput("plot1"),
                         h5("This graph shows the covid19 infection rate change from April to July. The darker the color is, the higher the infection rate is in that area. 
                            From this map, it's obvious that the infection rates in most areas have been increasing, and the virus has spreaded from the east coast to the western and southern US over these months."),
                         br(),
                         h1("Infection Rate: Top5 and Tail5"),
                         h5("We also created the best states with the 5 lowest infection rates and worst states with the highest infection rates by months. We can see New York and New Jersey are two sates having the highest infection rates. Covid in Arizona and Lousiana has been aggravating. 
                            Most small islands take conrtol of the situation well."),
                         plotOutput("topbar")
                       )
                     )
                      ),
                      
            tabPanel("Case Trend",icon = icon("line-chart"),
              sidebarLayout(
              sidebarPanel(
                selectInput(
                  inputId = "state", label = 'State', 
                  choices = c(''),
                  selected = 'New York',
                  multiple = TRUE)
              ),
              mainPanel(
                plotlyOutput("conf_st"),
                plotlyOutput("deat_st"),
                h5('These two line charts show the monthly confirmed/death cases of each state in 2020. By changing different months, we could find that New York shows highest cases among all states.'),
                plotlyOutput("conf_log_st"),
               plotlyOutput("deat_log_st"),
               h5('These two line charts show the log scale monthly confirmed/death cases of each state in 2020. Log chart could see the growth rate directly')
            )
          ) )
          
           ),

          navbarMenu("Unemployment Rate Analysis",icon = icon("question-circle"),
          tabPanel("Rate Map", icon = icon("map"),
                   sidebarLayout(
                     sidebarPanel( selectInput(inputId = 'month2',
                                               label = 'Select month in 2020:',
                                               choices = c(''),
                                               selected = 1)
                     ),
                     mainPanel(
                       h1('Unemployemnt Rate in 2020 by Month'),
                       leafletOutput("myMap", width="100%"),
                       h5('The map shows the monthly unemployment rate of each state in 2020. By changing different months, we could find that the purple circle is getting bigger from Jun to April.
                          The unemployment rate is almost twice then the same month of last year. After Apirl, the size of the circle decreases a little bit. Overall, we could find that the unemployment rate
                          is much higehr than Jau, and this is the impact of Covid-19.')
                     )
                   )),
          
          

          tabPanel("Rate Trend", icon = icon("line-chart"),
                   sidebarLayout(
                     sidebarPanel(
                       selectInput(
                         inputId = "state2",label = h3("Select a State"),
                         choices = c("Alabama", "Alaska",  "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Guam", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Minor Outlying Islands", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina",
                                     "North Dakota", "Northern Mariana Islands", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island", 
                                     "South Carolina", "South Dakota", "Tennessee", "Texas", "U.S. Virgin Islands", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"))),
                     
                    mainPanel(plotOutput("myplot"), p("* Red line is the national average unemployment rate for each month."),
                              h5("This bar chart shows the monthly unemployment rate of each state in the US from Jan 2020 to Jun 2020. The x-axis represented the month, and the y-axis represented the unemployment rate. The red line here shows the monthly national's average unemployment rate. In this way, we can easily compare it with the state's rate. The dropdown list on the left side contains all the States in the US. The bar chart is dynamic, and it will change based on the different selection from the dropdown list. For example, if we select the District of Columbia, we can see the bars for each month are all below the red line, which means, compare with the national average, has a lower unemployment rate. "))
                   )),
          
         
           tabPanel("Data",icon = icon("table"),
                   mainPanel(
                     h1('Unemployemnt Rate for 2020'),
                     dataTableOutput("myTable")
                   )))
 
)


          


server <- function(input, output, session) {
  # read data
  statedata <- read.csv("staterate.csv")
  geodata <- read.csv("geodata.csv")
  statedata = statedata %>%
    gather(Month, Unemployment, -state)
  
  
  ts_total_st <- read_csv('ts_total_st.csv')
  
  
  states <- geojson_read("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", what = "sp")
  
  
  
  data <- read_csv('data.csv')
  data$Date <- as.Date(data$Date, format="%m/%d/%y")
  ############# unemployment map #######
  updateSelectInput(session = session,
                    inputId = 'month2',
                    choices = unique(statedata$Month))
  
  output$myMap = renderLeaflet({
    filteredData <- statedata %>% filter(Month==input$month2)
    df <- merge(geodata, filteredData, by = 'state', all.x = TRUE)
    
    # create the map
    r <- df %>% leaflet() %>% addTiles() %>%
      setView(-95,37, zoom=3) %>% 
      addProviderTiles(providers$Stamen.Toner, group = "Toner")%>% 
      addLayersControl(baseGroups = c("Toner", "OSM"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addCircles(lng= ~Longitude, lat=~Latitude, radius = ~log(Unemployment)*60000,
                 label = paste0("The unemployment rate in ", df$state,":", '\n',df$Unemployment)) 
    
    return(r)
    
  })
  
  
  ######### covid map #####################
  
  statedata2 = reactive({
    return(read.csv(paste0(input$input1,".csv")))
  })
  
  #### map function
  map <- function(monthdoc) {
    combined = merge(states, statedata2(), by.x='name', by.y='state', all.x=TRUE)
    
    bins = seq(0,0.025,0.005)
    pal <- colorBin("YlOrRd", domain = combined$Ratio, bins = bins)
    labels <- sprintf(
      "<strong>%s</strong><br/> Infection Rate: %g ",
      combined$name, combined$Ratio
    ) %>% lapply(htmltools::HTML)
    
    m <- leaflet(combined) %>%
      setView(-96, 37.8, 3) %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addPolygons(
        fillColor = ~pal(Ratio),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend("bottomright", pal = pal, values = ~combined$Ratio,
                title = "Infection Rate" ,
                opacity = 0.7)
    return(m)
  }
  
  ### top bar plot function
  top_bar = function(month) {
    top_5 = head(statedata2() %>% arrange(-Ratio),5)
    tail_5 = tail(statedata2() %>% arrange(-Ratio),5)
    filtered = rbind(top_5, tail_5)
    
    filtered %>%
      ggplot(mapping = aes(x=reorder(state,Ratio), y=Ratio)) +
      geom_bar(stat='identity', aes(fill=(Ratio>=quantile(Ratio,0.5))), width = 0.7) +
      coord_flip() +
      labs(x='State', fill='High or Low infection') +
      scale_fill_manual(values = c('#9ecae1','#b56e51'), labels=c('Low','High'))
  }
  
  ### output
  output$plot1 <- renderLeaflet({
    map(input$input1)
  })
  
  output$topbar = renderPlot({
    top_bar(input$input1)
  })
  
  
  ######## covid line chart #########
  
  
  updateSelectInput(session = session,
                    inputId = 'state',
                    choices = unique(ts_total_st$Province_State),
                    selected = 'New York')
  
  output$deat_st <- renderPlotly({
    deat_st<- ts_total_st%>%
      filter(Province_State %in% input$state) %>%
      ggplot(aes(x=Date, y=Deaths,  group=Province_State, color=Province_State)) +
      geom_line()+ 
      scale_y_continuous(labels = comma)+
      labs(x = "", y = "", title =  "Number of COVID-19 Deaths Cases by State Level")
    
    ggplotly(deat_st)
  })
  
  output$deat_log_st = renderPlotly({
    deat_log_st<- ts_total_st %>%
      filter(Province_State %in% input$state)%>%
      ggplot( aes(x=Date, y=log10(Deaths),  group=Province_State, color=Province_State)) +
      geom_line()+ 
      scale_y_continuous(labels = comma)+
      labs(x = "", y = "", title =  "Number of COVID-19 Deaths Cases by State Level (Logarithmic Scale)")
    
    ggplotly(deat_log_st)
  })
  
  output$conf_st = renderPlotly({
    conf_st<- ts_total_st%>%
      filter(Province_State %in% input$state)%>%
      ggplot(aes(x=Date, y=Confirmed,  group=Province_State, color=Province_State)) +
      geom_line()+ 
      scale_y_continuous(labels = comma)+
      labs(x = "", y = "", title =  "Number of COVID-19 Confirmed Cases by State Level")
    
    ggplotly(conf_st)
  })
  
  output$conf_log_st = renderPlotly({
    conf_log_st<-ts_total_st%>%
      filter(Province_State %in% input$state)%>%
      ggplot( aes(x=Date, y=log10(Confirmed),  group=Province_State, color=Province_State)) +
      geom_line()+ 
      scale_y_continuous(labels = comma)+
      labs(x = "", y = "", title =  "Number of COVID-19 Confirmed Cases by State Level (Logarithmic Scale)")
    
    
    ggplotly(conf_log_st)
  })
  
  output$myTable = renderDataTable(
    return(datatable(statedata, rownames= FALSE))
  )
  
  
  ######## unemployment rate trend #######
  
  
  h <- 11.1  #natioanl average rate
  data<-data %>% gather("state", "rate", 2:53)
  
  myplot <- function(inputstate){
    data %>% filter(state == input$state2) %>% 
      ggplot (aes(x = Date)) +
      geom_col(aes(y = rate), fill = "lightskyblue") + 
      geom_hline(aes(yintercept=h),color = "red") +
      theme(plot.title = element_text(size = rel(2))) +
      labs(x = "", y = "", title =  "Monthly Unemployment Rate, Jan 2020 to June 2020" ) 
  }
  
  output$myplot <- renderPlot(
    myplot(data[,input$state2])
  )
  
  
  
  
}

shinyApp(ui = ui, server = server)

