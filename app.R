library(shiny)
library(maps)
library(mapproj)
library(sf)
library(ggplot2)
library(tigris)
library(dplyr)
library(stringr)
library(tidyverse)
library(shinyjs)


ga_election_data <- read_csv("data/ga_election_data.csv") %>%
  janitor::clean_names() %>%
  mutate(prop_pre_eday = (((absentee_by_mail_votes + advanced_voting_votes)/total_votes))*100)

map_data <- counties("Georgia", cb = TRUE, progress_bar = FALSE)

map_data_longer <- left_join(map_data, ga_election_data, by = c("NAME" = "county"))

bar_data <- ga_election_data %>%
  group_by(candidate) %>%
  summarise(sum_abmv = sum(absentee_by_mail_votes),
         sum_avv = sum(advanced_voting_votes),
         sum_edv = sum(election_day_votes),
         sum_pv = sum(provisional_votes)) %>%
  pivot_longer(!candidate, names_to = "voting", values_to = "count")

ui <- fluidPage(
  
  titlePanel("Georgia: 2020 US Presidential Election Results"),
  
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      radioButtons("plot",
                  label = "Distribution of Votes:",
                  choices = c("Percent Map", "Bar Count"),
                  selected = "Percent Map"
                  ),
      selectInput("candidate",
                  label = "Candidate:",
                  choices = c("Joseph R. Biden",
                              "Donald J. Trump"),
                  selected = "Joseph R. Biden"
                  ),
      br(),
      p(strong("State Capital:"), "Atlanta"),
      p(strong("Population:"), "10,711,908"),
      br(),
      p(strong("State Location in the US:")),
      img(src = "georgia_usa.png", height = 125, width = 225)
    ),
    
    mainPanel(
      plotOutput("map1"),
      plotOutput("map2")
    )
  )
)

server <- function(input, output) {

  output$map1 <- renderPlot({
    map_data_longer_candidate <- map_data_longer %>%
      filter(candidate == input$candidate)
    
    caption <- if_else(input$candidate == "Joseph R. Biden", "Democratic Nominee",
                       "Republican Nominee")
    ggplot() + 
      geom_sf(data = map_data_longer_candidate, mapping = aes(fill = prop_pre_eday)) + 
      coord_sf() +
      theme_void() +
      labs(title = "Percentage of Votes from Early Voting",
           subtitle = input$candidate,
           caption = caption) +
      scale_fill_gradient2(name = "", low = "#1AFF1A", mid = "white", high = "#5D3A9B", 
                           limits = c(50, 100), midpoint = 75, breaks = c(50, 75, 100),
                           labels = c("50%", "75%", "100%")) +
      theme(plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12),
            legend.justification = "top")
})
  
  output$map2 <- renderPlot({
    bar_data_filter <- bar_data %>%
      filter(candidate == input$candidate)
    
  caption <- if_else(input$candidate == "Joseph R. Biden", "Democratic Nominee",
                       "Republican Nominee")
    
    ggplot(data = bar_data_filter, aes(x = voting, y = count)) + 
      geom_col() +
      labs(title = "Count of Votes by Voting Method",
           subtitle = input$candidate,
           caption = caption) + 
      scale_y_continuous(breaks = seq(0, 1500000, 250000),
                         limits = c(0,1500000),
                         labels = scales::label_dollar(big.mark = ",", prefix = NULL)) +
      scale_x_discrete(name = "", labels = c("Absentee By Mail Votes",
                                  "Adavanced Voting Votes",
                                  "Election Day Votes",
                                  "Provisional Votes")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12))
  })
  
  observeEvent(input$plot,{
    req(input$plot)
    if(input$plot == "Percent Map"){
      hide("map2")
      show("map1")
      }
    else{
      hide("map1")
      show("map2")
    }
  })
  
}

shinyApp(ui = ui, server = server)
