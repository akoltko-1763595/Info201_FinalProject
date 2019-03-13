library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
source("analysis.R")

server <- function(input, output) {
  ## Not Functional
  #output$danceability <- renderText ({
  #  track <- songs %>% filter(input$song_choice == Song) %>% select(track_id) %>% pull()
  #  features <- getFeatures(track, my_token)
  #  features <- features[2] %>% pull()
  #  features
  #})
  
  output$dance_plot <- renderPlot ({
    p <- ggplot(data = songs) +
      geom_point(mapping = aes(x = Genre, y = danceability))
    
    p
  })
  
  ## Question 1: Does Greatness Hold Up? (Sam)
  output$plotQ1 <- renderPlot({

  plot <- ggplot(data = popularity_comparison_data) +
    geom_smooth(se = F, mapping = aes(x = Year, y = Spotify_popularity, color = "Spotify"),
                size = 2) +
    geom_smooth(se = F, mapping = aes(x = Year, y = RS_popularity, color = "Rolling Stone"),
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
                        values=c(Spotify="#739E88", "Rolling Stone"="#DE646C"))
    
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

  
  years <- popularity_comparison_data %>% 
    group_by(Artist) %>% 
    summarise(
      avg_year = round(mean(Year))
    )
  
  avg_year_data <- full_join(
    years,
    popularity_comparison_data,
    by = "Artist"
  )
  
  output$plotQ1num3 <- renderPlot({
    plot3 <- ggplot(data = avg_year_data) +
      geom_point(mapping = aes(x = avg_year, y = rescale(Spotify_popularity, to = c(1, 500))), color = "red") +
      #geom_point(mapping = aes(x = avg_year, y = Place), color = "blue") +
      theme_minimal()
      
      plot3
  })


  ## Question 2: What Makes an album great? (Andrew)
  output$plotQ2Overall <- renderPlot({
    plot_to_plot_q2 + labs(title = "Overall Ranking vs Specific Ranking by category")
  })
  
  output$plotQ2Artist <- renderPlot({
    ggplot(data = RS_SP_AS_pop %>% 
                    filter(Artist == input$artist_choice_q2) %>% 
                    select(Artist, RS_Place, Spotify_Place, AS_Place, Overall_Place) %>% 
                    gather(key = "Category", value = "Place", 2:5)) + 
      geom_col(mapping = aes(x = Category, y = Place, fill = Category)) + 
      scale_fill_manual(
        name = "Categories",
        values = c(
          "Spotify_Place"="#739E88", 
          "RS_Place"="#DE646C", 
          "Overall_Place" = "Black", 
          "AS_Place" = "#e6e600"
        )
      ) + 
      labs(
        title = paste("Score by Category for", input$artist_choice_q2)
      )
  })
  
  output$artistDetails <- renderUI({
    dataset <- RS_SP_AS_pop %>% filter(Artist == input$artist_choice_q2)
    dataset[is.na(dataset)] <- 0
    
    to_return <- HTML(
      paste0(input$artist_choice_q2," had: ", "<ul><li> ", dataset$num_of_great_albums  ," great album",ifelse(dataset$num_of_great_albums == 1,"","s")," and ",
              dataset$num_of_great_songs, " great song",ifelse(dataset$num_of_great_songs == 1,"","s"),"</li><li>a rating of ",dataset$Spotify_Popularity,"% on Spotify</li><li>At least ",
              dataset$probable_sales," million records sold worldwide</li></ul>")
    )
    
    to_return
  })
  
  ## Question 3: What do fans and critics agree on? (Alex)
  
  output$plotQ3_num1 <- renderPlot({
    plot_1 <- ggplot(data = genre_ranking) +
      geom_col(mapping = aes(x = Genre, y = score, fill = category)) +
      labs(
        title = "Genre Scores",
        x = "Genres",
        y = "Scores"
      ) +
      coord_flip() +
      theme_minimal() +
      theme(plot.title = element_text(size=20, hjust = 0.5),
            axis.text.y = element_text(size = 15),
            axis.title.x = element_text(size = 15, vjust = 0),
            axis.title.y = element_text(size = 15, vjust = 2),
            legend.title = element_blank(),
            text = element_text(size = 15)) +
      scale_fill_manual(name="Legend",
                          values=c(fan_ranking="#739E88", critic_ranking="#DE646C"))
    
    plot_1
  })
  
  output$plotQ3_num2 <- renderPlot({
    chosen_genre <- genre_ranking %>% 
      filter(Genre == input$genre_choice)
    plot_2 <- ggplot(data = chosen_genre) +
      geom_col(mapping = aes(x = category, y = score, fill = category)) +
      labs(
        title = paste("Critic Score vs Fan Score for", input$genre_choice, "music"),
        x = "Critics vs. Fans",
        y = "Score"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size=20, hjust = 0.5),
            axis.text.y = element_text(size = 15),
            axis.title.x = element_text(size = 15, vjust = 0),
            axis.title.y = element_text(size = 15, vjust = 2),
            legend.title = element_blank(),
            text = element_text(size = 15)) + 
      scale_fill_manual(name="Legend",
                        values=c(fan_ranking="#739E88", critic_ranking="#DE646C"))
    
    plot_2
  })
  
  
  ## Question 4: How well do sales dictate greatness? (Spencer)

  # Sales versus Rank
  output$plotQ4 <- renderPlot({
    plot <- ggplot(data = combined_best_and_sales, mapping = aes(Probable, Place)) +
      geom_point(colour  = "#e6e600", alpha = .7) +
      geom_smooth(se = F, size = 2, colour = "#DE646C") +
      scale_y_continuous(limits = c(0, 500)) +
      labs(title = "How Album Sales Dictates Greatness",
           x = "Sales (millions)", y = "Greatness Ranking") +
      theme_minimal() +
      theme(plot.title = element_text(size = 20, hjust = .5),
            axis.text.x = element_text(size = 15, angle = 50, vjust = .5),
            axis.text.y = element_text(size = 15),
            axis.title.x = element_text(size = 15, vjust = 0),
            axis.title.y = element_text(size = 15, vjust = 2),
            text = element_text(size = 15))
    
    plot
  })
  
  # Ranking and Sales over Time
  output$plotQ4num2 <- renderPlot({
    df <- group_by(combined_best_and_sales, Year) %>%
      summarise(MSales = round(mean(Probable), digits = 2), MPlace = round(mean(Place), digits = 2))
    
    plot <- ggplot(data = df) +
      geom_point(colour  = "#DE646C", alpha = .7, mapping = aes(Year, MPlace)) +
      geom_smooth(se = F, size = 2, mapping = aes(Year, MPlace, color = "Avg. Greatness Rank")) +
      geom_point(colour  = "#e6e600", alpha = .7, mapping = aes(Year, MSales)) +
      geom_smooth(se = F, size = 2, mapping = aes(Year, MSales, color = "Avg. Sales (millions)")) +
      labs(title = "Album Sales & Greatness Over Time", x = "Year", y = NULL) +
      theme_minimal() +
      theme(plot.title = element_text(size = 20, hjust = .5),
            axis.text.x = element_text(size = 15, angle = 50, vjust = .5),
            axis.text.y = element_text(size = 15),
            axis.title.x = element_text(size = 15, vjust = 0),
            axis.title.y = element_text(size = 15, vjust = 2),
            text = element_text(size = 15)) +
      scale_colour_manual(name = "Key", values = c("Avg. Greatness Rank" = "#DE646C", "Avg. Sales (millions)" = "#e6e600"))
    
    plot
  })
  
  # Correlation coefficient for plot 2
  output$textQ4 <- renderText({
    df <- group_by(combined_best_and_sales, Year) %>%
      summarise(MSales = round(mean(Probable), digits = 2), MPlace = round(mean(Place), digits = 2))
    
    text <- paste("Correlation coefficient between year and average greatness:",
                  round(cor(df$Year, df$MPlace), digits = 3))
    text
  })
  
  # Genre Popularity & Sales
  output$plotQ4num3 <- renderPlot({
    df <- group_by(combined_best_and_sales, Genre) %>%
      summarise(MSales = round(mean(Probable), digits = 2), MPlace = round(mean(Place), digits = 2))
    
    AvgSales <- round(mean(df$MSales), digits = 2)
    AvgPlace <- round(mean(df$MPlace), digits = 2)
    
    plot <- ggplot(data = filter(df, Genre == input$genre_choice2)) +
      geom_vline(xintercept = 100, colour = "#e6e600", size = 2) +
      geom_hline(yintercept = 100, colour = "#DE646C", size = 2) +
      geom_point(colour = "gray", size = 4, mapping = aes(MSales / AvgSales * 100, MPlace / AvgPlace * 100)) +
      labs(title = paste(input$genre_choice2, "Sales & Greatness as % of All Genre Average"),
           x = "% of Average Sales (millions)", y = "% of Average Greatness Ranking") +
      scale_y_continuous(limits = c(0, 200)) +
      scale_x_continuous(limits = c(0, 200)) +
      theme_minimal() +
      theme(plot.title = element_text(size = 20, hjust = .5),
            axis.text.x = element_text(size = 15, angle = 50, vjust = .5),
            axis.text.y = element_text(size = 15),
            axis.title.x = element_text(size = 15, vjust = 0),
            axis.title.y = element_text(size = 15, vjust = 2),
            text = element_text(size = 15))
    
    plot
  })
  
  
  output$bibli <- renderUI({
    HTML(
      paste0(
        '<ul><li><a href = "https://www.cs.ubc.ca/~davet/music/list/Best9.html">Song Data</a></li>',
        '<li><a href = "https://raw.githubusercontent.com/Currie32/500-Greatest-Albums/master/albumlist.csv">Album Data</a></li>',
        '<li><a href = "https://www.rollingstone.com/music/music-lists/500-greatest-albums-of-all-time-156826/"> Rolling Stones\' Album List</a></li>',
        '<li><a href = "https://www.rollingstone.com/music/music-lists/500-greatest-songs-of-all-time-151127/"> Rolling Stones\' Song List</a></li>',
        '<li><a href = "https://tsort.info/music/faq_album_sales.htm">Song Sales Data from Tsort</a></li>'
        
      )
    )
    
  })
}