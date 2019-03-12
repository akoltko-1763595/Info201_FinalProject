#library(Rspotify)
library(dplyr)
library(tidyr)
library(scales)
#library(openxlsx)
#source("../spotify_token.R")

#Run this once to update Spotify data
#source("spotify_data_file_creation.R")
if (sys.nframe() == 0){
  albums <- read.csv("data/RSAlbumsWithSpotifyData.csv", stringsAsFactors = FALSE)
  songs <- read.csv("data/RSSongsWithSpotifyData.csv", stringsAsFactors = FALSE)
  album_sales <- read.csv("data/CombinedRecordSales.csv", stringsAsFactors = FALSE)
  best_albums <- read.csv("data/RollingStonesTop500Albums.csv", stringsAsFactors = FALSE)
  #record_sales <- read.xlsx("data/CombinedRecordSales.xlsx")
} else {
  albums <- read.csv("../data/RSAlbumsWithSpotifyData.csv", stringsAsFactors = FALSE)
  songs <- read.csv("../data/RSSongsWithSpotifyData.csv", stringsAsFactors = FALSE)
  album_sales <- read.csv("../data/CombinedRecordSales.csv", stringsAsFactors = FALSE)
  best_albums <- read.csv("../data/RollingStonesTop500Albums.csv", stringsAsFactors = FALSE)
  #record_sales <- read.xlsx("../data/CombinedRecordSales.xlsx")
}



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
top_artists_RS <- function(songs, albums) {
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
  
  return(combined_data %>% arrange(desc(RS_popularity)))
}

spotify_pop <-
  full_join(
    songs,
    albums,
    by = c("Artist", "Popularity", "Followers" )
  ) %>% 
    group_by(Artist) %>% 
    summarize(
      Spotify_Popularity = round(mean(Popularity)),
      Spotify_Followers = round(mean(Followers))
    ) %>% 
  arrange(desc(Spotify_Popularity), desc(Spotify_Followers))

spotify_pop$Spotify_Place = rownames(spotify_pop)
RS_pop <- top_artists_RS(songs, albums)
RS_pop$RS_Place = rownames(RS_pop)

RS_SP_pop <-
  full_join(
    RS_pop,
    spotify_pop,
    by = c("Artist")
  )


#album_sales <- read.csv("data/CombinedRecordSales.csv", stringsAsFactors = FALSE)

AS_pop <- 
  album_sales %>% 
  group_by(Artist) %>% 
  summarize(
     minimum_sales = sum(Minimal),
     probable_sales = sum(Probable)
  ) %>% 
  arrange(desc(minimum_sales), desc(probable_sales)) 
  
AS_pop$AS_Place = rownames(AS_pop)
AS_pop$Artist = substring(AS_pop$Artist, 2)

RS_SP_AS_pop <-
  full_join(
    RS_SP_pop,
    AS_pop,
    by = c("Artist")
  )

## Question 3: What do fans and critics agree on? (Alex)

# Compare Genre critic scores & fan scores
# Bar graph that shows the differenc between fans and critics

songs$song_popularity <- rescale(songs$song_popularity, to = c(1, 500))

genre_frame <- songs %>% 
  select(Genre, Place, song_popularity) %>% 
  filter(Genre != "TBD") %>% 
  arrange(Genre)

# Combine Scores
genre_ranking <- genre_frame %>% 
  group_by(Genre) %>% 
  summarize(
    critic_ranking = sum(501 - Place),
    fan_ranking = sum(501 - song_popularity)
  ) %>% 
  gather(key = category, value = score, -Genre)


## Question 4: How well do sales dictate greatness? (Spencer)
#best_albums <- read.csv("data/RollingStonesTop500Albums.csv", stringsAsFactors = FALSE) %>%
#  select(Artist, Album, Year, Genre, Subgenre, Place)

# Going to use the Probable sales data instead of Minimal or Range
#record_sales <- read.xlsx("data/CombinedRecordSales.xlsx") %>%
#  select(Artist, Album.Title, Probable) # Numbers are in millions
#colnames(record_sales)[2] <- "Album"

# Combined the "best" albums with the best selling albums of all time
# Of 300 possible overlapping albums, only 41 actually do
# Of these 41, almost all of them are Rock and from the 80s and 90s
#combined_best_and_sales <- left_join(best_albums, record_sales, by = "Album") %>%
#  filter(!is.na(Probable)) %>%
#  select(Artist.x, Album, Year, Genre, Subgenre, Place, Probable)
#colnames(combined_best_and_sales)[1] <- "Artist"

