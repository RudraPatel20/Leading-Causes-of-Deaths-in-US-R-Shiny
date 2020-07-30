library(shiny)
library(tidyverse)
library(ggplot2)
library(readr)
library(plotly)
library(shinythemes)
library(maps)
library(mapproj)
library(viridis)
library(rworldmap)

df <- read.csv("./data/NCHS_-_Leading_Causes_of_Death__United_States (2).csv")

data <- read.csv("./data/cardiovascular-disease-death-rates.csv")
data <- data %>% 
  rename(
    Deaths = Deaths...Cardiovascular.diseases...Sex..Both...Age..Age.standardized..Rate...deaths.per.100.000.individuals.
  )

region <- c("All States" ="United States", "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
            "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii",
            "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
            "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
            "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York",
            "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
            "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
            "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

causes <- c("Unintentional injuries", "Alzheimer's disease", "Stroke", "CLRD", "Diabetes", 
            "Heart disease", "Influenza and pneumonia", "Suicide", "Cancer", "Kidney disease")



ui <- fluidPage(
  
  theme = shinytheme("superhero"),
  titlePanel("Cause of Deaths in United States 1999-2017"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12,
               sliderInput("years",
                           h3("Select a Year:"),
                           min = 1999,
                           max = 2017,
                           value = 2008),
               hr(),
               selectInput("region", h3("Select a Region:"), choices = region, selected = "All States"),
               hr(),
               checkboxGroupInput("cause", h3("Select Causes to Display:"), choices = causes, selected = causes))
      ),
      fluidRow(
        column(12,
               wellPanel(style = "color:white", textOutput("definition"))
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Trends",
                 plotlyOutput("barchart"),
                 plotlyOutput("linechart")
        ),
        tabPanel("U.S. Deaths",
                 fluidRow(
                   column(6, 
                          h4("Year")
                   ),
                   column(6,
                          h4("Total Deaths")
                   )
                 ),
                 fluidRow(
                   column(6, 
                          wellPanel (
                            div(textOutput("year"),style = "font-size:125%")
                          )
                   ),
                   column(6,
                          
                          wellPanel (
                            div(textOutput("totaldeaths"),style = "font-size:125%")
                          )
                          
                   )
                 ),
                 plotlyOutput("map", width =1200, height = 700)
        ),
        tabPanel("U.S. Death Rate",
                 fluidRow(
                   column(6,
                          h4("Year")
                   ),
                   column(6,
                          h4("Average Death Rate")
                   )
                 ),
                 fluidRow(
                   column(6,
                          wellPanel (
                            div(textOutput("year1"),style = "font-size:125%")
                          )
                   ),
                   column(6,
                          
                          wellPanel (
                            div(textOutput("totaldeathrate"),style = "font-size:125%")
                          )
                          
                   )
                 ),
                 plotlyOutput("map2", width = 1200, height = 700)
        ),
        tabPanel("World Data on Heart Disease",
                 fluidRow(
                   column(12,
                          wellPanel(style = "color:white", textOutput("information"))
                   ),
                   column(4,
                          h4("Year")
                   ),
                   column(4,
                          h4("Country with Highest Death Rate")
                   ),
                   column(4,
                          h4("Death Rate per 100,000 people")
                   )
                 ),
                 fluidRow(
                   column(4,
                          wellPanel (
                            div(textOutput("year2"),style = "font-size:125%")
                          )
                   ),
                   column(4,
                          
                          wellPanel (
                            div(textOutput("totaldeathrateworld"),style = "font-size:125%")
                          )
                   ),
                   column(4,     
                          wellPanel (
                            div(textOutput("numberdeath"),style = "font-size:125%")
                          )   
                   )
                 ),
                 plotlyOutput("map3", height = 630)
        )
      )          
    )
  )
)


server <- function(input, output,session) {
  
  output$definition <- renderText({
    
    paste("AGE-ADJUSTED DEATH RATE is a death rate that controls for the effects of differences in
population age distributions. Direct age-adjustment (or age standardization) is the same as calculating a weighted average.
          In short, Deaths in 100,000 population can be regarded as Age-adjusted Death Rate.")
    
  })
  
  output$barchart <- renderPlotly({
    
    a <- df %>%
      filter(Year == input$years) %>%
      filter(Cause.Name %in% input$cause) %>%
      filter(State == input$region) %>%
      ggplot() +
      aes(x =reorder(Cause.Name, Deaths), weight = Deaths) +
      geom_bar(fill = "#582929") +
      scale_fill_continuous(type = "viridis")+
      labs(x = "Causes", y = "Count", title = "Cause of Death in United States", caption = "* CLRD: Chronic Low Respiratory Diseases") +
      theme(axis.ticks.x = element_blank())+
      coord_flip() +
      hrbrthemes::theme_modern_rc()
    ggplotly(a)
    
  })
  
  output$linechart <- renderPlotly({
    
    b <- df %>%
      filter(State == input$region) %>%
      filter(Cause.Name %in% "All causes") %>%
      ggplot() +
      aes(x = Year, y = Age.adjusted.Death.Rate) +
      geom_line(size = 1L, colour = "#99515a") +
      labs(x = "Year", y = "Rate per 100,000 U.S. standard population", title = "Age-adjusted Death Rates for All causes: 1999-2017") +
      hrbrthemes::theme_modern_rc()
    ggplotly(b)
    
  })
  
  output$year <- renderText({
    
    paste(input$years)
    
  })
  
  output$totaldeaths <- renderText({
    
    x <-  df %>% 
      filter(State == "United States") %>% 
      filter(Cause.Name == "All causes") %>%
      filter(Year == input$years)
    paste(x$Deaths)
    
  })
  
  output$map <- renderPlotly({
    
    states <- map_data("state")
    df <- df %>% 
      filter(Cause.Name == "All causes") %>% 
      filter(State != "United States") %>% 
      filter(Year == input$years)
    
    df$region <- tolower(df$State)
    
    choro <- left_join(states, df)
    
    centroid <- aggregate(cbind(long,lat)~State, data=choro, FUN=mean)
    
    a <- ggplot(choro, aes(long, lat)) +
      geom_polygon(aes(group = group, fill = Deaths)) +
      geom_text(data = centroid, mapping = aes(x=long, y=lat, label=State), hjust=1, vjust=1, size = 3, color = "white") +
      coord_map(projection = "albers",  lat0 = 45.5, lat1 = 29.5)+
      labs(title = "Deaths in United States")+
      scale_fill_viridis_c()+
      theme(axis.text = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            panel.background = element_rect(fill = "gray"),
            plot.title = element_text(family = "Helvetica" , size = 20, hjust = 0.5))+
      scale_fill_viridis(option = "viridis")
    ggplotly(a)
    
  })
  
  output$year1 <- renderText({
    
    paste(input$years)
    
  })
  
  output$totaldeathrate <- renderText({
    
    x <-  df %>%
      filter(State == "United States") %>%
      filter(Cause.Name == "All causes") %>%
      filter(Year == input$years)
    paste(x$Age.adjusted.Death.Rate)
    
  })
  
  output$map2 <- renderPlotly({
    
    states <- map_data("state")
    df <- df %>% 
      filter(Cause.Name == "All causes") %>% 
      filter(State != "United States") %>% 
      filter(Year == input$years)
    
    df$region <- tolower(df$State)
    
    choro <- left_join(states, df, sort = FALSE, by = "region")
    
    centroid <- aggregate(cbind(long,lat) ~ State, data=choro, FUN=mean)
    
    a <- ggplot(choro, aes(long, lat)) +
      geom_polygon(aes(group = group, fill = Age.adjusted.Death.Rate)) +
      geom_text(data = centroid, mapping = aes(x=long, y=lat, label=State), hjust=1, vjust=1, size = 3, color = "white", family = "Helvetica") +
      coord_map(projection = "albers",  lat0 = 45.5, lat1 = 29.5)+
      labs(title = "Death Rate in United States")+
      scale_fill_viridis("Age Adjusted\nDeath Rate", option = "heat")+
      theme(axis.text = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            panel.background = element_rect(fill = "gray"),
            plot.title = element_text(family = "Helvetica" , size = 20, hjust = 0.5)
      )
    ggplotly(a)
    
    
  })
  
  output$information <- renderText({
    
    paste("The below world map represents age adjusted dath rate around the world due to Heart Diseases.")
    
  })
  
  output$year2 <- renderText({
    
    paste(input$years)
    
  })
  
  output$totaldeathrateworld <- renderText({
    
    x <-  data %>%
      filter(Year == input$years) %>%
      filter(Deaths == max(Deaths))
    paste(x$Entity)
    
  })
  
  output$numberdeath <- renderText({
    
    x <-  data %>%
      filter(Year == input$years) %>%
      filter(Deaths == max(Deaths))
    paste(x$Deaths)
    
  })
  
  output$map3 <- renderPlotly({
    
    data <- data %>% 
      filter(Year == input$years)
    
    world_map <- plot_ly(data, type='choropleth', locations=data$Code, z=data$Deaths, text=data$Entity, colorscale = "Heat", title = "abc")
    
    world_map <- world_map %>% colorbar(title = "Age adjusted<br>Death Rate") 
    
    world_map
    
  }) 
}

shinyApp(ui = ui, server = server)