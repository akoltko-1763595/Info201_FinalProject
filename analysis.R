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




## Quesiton 4: How well do sales dictate greatness? (Spencer)



