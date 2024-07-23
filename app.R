library(shiny)
library(ggplot2)
library(dplyr)


spotify_data <- read.csv("cleaned_spotify_data.csv")

top_artists <- spotify_data %>%
  group_by(artist.s._name) %>%
  summarise(total_streams = sum(streams)) %>%
  top_n(10, total_streams) %>%
  arrange(desc(total_streams)) %>%
  pull(artist.s._name)

spotify_data$streams_billion <- spotify_data$streams / 1e9

# UI
ui <- fluidPage(
  titlePanel("Compare Top 10 Artists"),
  sidebarLayout(
    sidebarPanel(
      selectInput("artist1", "Select Artist 1", choices = top_artists),
      selectInput("artist2", "Select Artist 2", choices = top_artists, selected = top_artists[2])
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Stream Comparison", plotOutput("streamsPlot")),
        tabPanel("Danceability Analysis", plotOutput("danceabilityPlot")),
        tabPanel("Energy Levels", plotOutput("energyPlot")),
        tabPanel("Acousticness Distribution", plotOutput("acousticnessPlot")),
        tabPanel("Stream Histograms", plotOutput("histogramPlot")),
        tabPanel("Danceability vs Energy", plotOutput("scatterPlot"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  artist_data <- reactive({
    spotify_data %>%
      filter(artist.s._name %in% c(input$artist1, input$artist2))
  })
  
  output$streamsPlot <- renderPlot({
    ggplot(artist_data(), aes(x = artist.s._name, y = streams_billion, fill = artist.s._name)) +
      geom_boxplot() +
      labs(title = "Streams Comparison (in billions)", x = "Artist", y = "Streams (in billions)") +
      theme_minimal()
  })
  
  output$danceabilityPlot <- renderPlot({
    ggplot(artist_data(), aes(x = danceability_., fill = artist.s._name)) +
      geom_density(alpha = 0.7) +
      labs(title = "Danceability Density Comparison", x = "Danceability", y = "Density") +
      theme_minimal()
  })
  
  output$energyPlot <- renderPlot({
    ggplot(artist_data(), aes(x = artist.s._name, y = energy_., fill = artist.s._name)) +
      geom_violin() +
      labs(title = "Energy Levels Comparison", x = "Artist", y = "Energy") +
      theme_minimal()
  })
  
  output$acousticnessPlot <- renderPlot({
    ggplot(artist_data(), aes(x = acousticness_., fill = artist.s._name)) +
      geom_histogram(position = "dodge", bins = 30, alpha = 0.7) +
      labs(title = "Acousticness Distribution", x = "Acousticness", y = "Count") +
      theme_minimal()
  })
  
  output$histogramPlot <- renderPlot({
    ggplot(artist_data(), aes(x = streams_billion, fill = artist.s._name)) +
      geom_histogram(position = "dodge", bins = 30, alpha = 0.7) +
      labs(title = "Streams Histogram (in billions)", x = "Streams (in billions)", y = "Count") +
      theme_minimal()
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(artist_data(), aes(x = danceability_., y = energy_., color = artist.s._name)) +
      geom_point(alpha = 0.7, size = 3) +
      labs(title = "Danceability vs Energy", x = "Danceability", y = "Energy") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
