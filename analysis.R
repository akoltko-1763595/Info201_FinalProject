library(Rspotify)
library(dplyr)
library(scales)
#source("spotify_token.R")

#Run this once to update Spotify data
#source("spotify_data_file_creation.R")

albums <- read.csv("data/RSAlbumsWithSpotifyData.csv", stringsAsFactors = FALSE)
songs <- read.csv("data/RSSongsWithSpotifyData.csv", stringsAsFactors = FALSE)

## Primary Analytics



## Question 1: Does Greatness Hold Up? (Sam)
albums <- albums %>% arrange(desc(Popularity)) %>% head(300)
songs <- songs %>%  arrange(desc(Popularity)) %>% head(300)
albums$Spotify_popularity <- rescale(albums$Popularity, to = c(1, 300))
songs$Spotify_popularity <- rescale(songs$Popularity, to = c(1, 300))

top_300_artists <- function(songs, albums) {
  albums$album_score <- 501 - albums$Place
  songs$song_score <- 501 - songs$Place
  
  artist_album_data <- albums %>% 
    group_by(Artist) %>% 
    summarize(
      total_album_score = sum(album_score),
      num_of_great_albums = length(album_score)
    )
  
  artist_song_data <- songs %>% 
    group_by(Artist) %>% 
    summarize(
      total_song_score = sum(song_score),
      num_of_great_songs = length(song_score)
    )
  
  combined_data <- full_join(artist_album_data, artist_song_data, by = "Artist")
  combined_data$RS_popularity <- rowSums(cbind(combined_data$total_album_score, combined_data$total_song_score), na.rm = TRUE)
  
  combined_data <- arrange(combined_data, desc(RS_popularity))
  
  return(combined_data %>% arrange(desc(RS_popularity)) %>% head(300))
}

popularity_comparison_data <-
  left_join(
    albums,
    top_300_artists(songs, albums) %>% transmute(Artist = Artist, RS_popularity = rescale(top_300_artists(songs, albums)$RS_popularity, to = c(1,300))),
    by = c("Artist")
  )

#Negative means more popular on Spotify, positive means more popular with Rolling Stone
popularity_comparison_data <- popularity_comparison_data %>% 
  mutate(
    pop_change = RS_popularity - Spotify_popularity
  )

## Question 2: What Makes an album great? (Andrew)




## Question 3: What do fans and critics agree on? (Alex)




## Question 4: How well do sales dictate greatness? (Spencer)
library("openxlsx")
library("dplyr")

best_albums <- read.csv("data/RollingStonesTop500Albums.csv", stringsAsFactors = FALSE) %>%
  select(Artist, Album, Year, Genre, Subgenre, Place)

# Going to use the Probable sales data instead of Minimal or Range
record_sales <- read.xlsx("data/CombinedRecordSales.xlsx") %>%
  select(Artist, Album.Title, Probable) # Numbers are in millions
colnames(record_sales)[2] <- "Album"

# Combined the "best" albums with the best selling albums of all time
# Of 300 possible overlapping albums, only 41 actually do
# Of these 41, almost all of them are Rock and from the 80s and 90s
combined_best_and_sales <- left_join(best_albums, record_sales, by = "Album") %>%
  filter(!is.na(Probable)) %>%
  select(Artist.x, Album, Year, Genre, Subgenre, Place, Probable)
colnames(combined_best_and_sales)[1] <- "Artist"

View(record_sales)
View(best_albums)
View(combined_best_and_sales)




# Testing while I wait for someone to push to Shiny files
# Need to fix the y-axis values because they're incredibly high for essentially no reason at all
library("ggplot2")

# Sales versus Rank
ggplot(data = combined_best_and_sales, mapping = aes(Probable, Place)) +
  geom_point(mapping = aes(color = Year)) +
  geom_smooth() +
  scale_y_continuous(limits = c(0, 500)) +
  labs(
    title = "Total Sales versus Album Rank", # plot title
    x = "Probable Album Sales", # x-axis label
    y = "Rolling Stones' Ranking", # y-axis label
    color = "Year" # legend label for the "color" property
  )

# Year versus Rank
ggplot(data = combined_best_and_sales, mapping = aes(Year, Place)) +
  geom_col(mapping = aes(fill = Genre)) +
  scale_x_continuous(breaks = combined_best_and_sales$Year) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  # theme(panel.grid.minor=element_blank(),
        # panel.grid.major=element_blank())

# Genre versus Sales
ggplot(data = combined_best_and_sales, mapping = aes(Genre, Probable)) +
  geom_col()



