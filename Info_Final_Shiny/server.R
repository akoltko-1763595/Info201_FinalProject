library(shiny)
library(ggplot2)
library(tidyr)
source("../analysis.R")

server <- function(input, output) {
  
  ## Question 1: Does Greatness Hold Up? (Sam)
  output$plotQ1 <- renderPlot({

  plot <- ggplot(data = popularity_comparison_data) +
    geom_smooth(se = F, mapping = aes(x = Year, y = Spotify_popularity, color = "spotify"),
                size = 2) +
    geom_smooth(se = F, mapping = aes(x = Year, y = RS_popularity, color = "rolling_stone"),
                size = 2) +
    labs(title = "Ranking Trends Over Time", y = "Popularity", x = "Year") +
    theme_minimal() +
    theme(plot.title = element_text(size=20, hjust = 0.5),
          axis.text.x = element_text(size = 15, angle = 50, vjust = 0.5),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 15, vjust = 0),
          axis.title.y = element_text(size = 15, vjust = 2),
          legend.title = element_blank(),
          text = element_text(size = 15)) +
    scale_colour_manual(name="Legend",
                        values=c(spotify="#739E88", rolling_stone="#DE646C"))
    
  plot
  })
  
  output$plotQ1num2 <- renderPlot({
    
    popularity_comparison_data <- popularity_comparison_data %>% 
      filter(
        Artist == input$band_choice
      )
    
    popularity_comparison_data <- 
      popularity_comparison_data %>% 
      select(
        Artist,
        RS_popularity,
        Spotify_popularity
      ) %>% 
      gather(
        key = "Rank",
        value = "Popularities",
        RS_popularity, 
        Spotify_popularity
      ) %>% 
      group_by(Artist, Rank) %>% 
      summarise(
        Popularities = mean(Popularities)
      )
    
    plot2 <- ggplot(data = popularity_comparison_data) +
      theme_minimal() +
      geom_col(mapping = aes(x = Rank, y = Popularities, fill = Rank)) +
      scale_color_manual(aesthetics = "fill", values = c(Spotify_popularity="#739E88", RS_popularity="#DE646C")) +
      labs(title = paste0("Rankings for ", input$band_choice), y = "Popularity") +
      ylim(0,300)+
      theme(
        plot.title = element_text(size=20, hjust = 0.5),
        legend.title = element_blank(),
        text = element_text(size = 15)
      )
    plot2
  })

  
  ## Question 2: What Makes an album great? (Andrew)
  
  
  
  
  ## Question 3: What do fans and critics agree on? (Alex)
  
  
  
  
  ## Quesiton 4: How well do sales dictate greatness? (Spencer)

}