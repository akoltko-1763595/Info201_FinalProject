library(Rspotify)
library(dplyr)
library(scales)
library(openxlsx)
#source("spotify_token.R")

#Run this once to update Spotify data
#source("spotify_data_file_creation.R")
if (sys.nframe() == 0){
  albums <- read.csv("data/RSAlbumsWithSpotifyData.csv", stringsAsFactors = FALSE)
  songs <- read.csv("data/RSSongsWithSpotifyData.csv", stringsAsFactors = FALSE)
} else {
  albums <- read.csv("../data/RSAlbumsWithSpotifyData.csv", stringsAsFactors = FALSE)
  songs <- read.csv("../data/RSSongsWithSpotifyData.csv", stringsAsFactors = FALSE)
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


album_sales <- read.csv("data/CombinedRecordSales.csv", stringsAsFactors = FALSE)

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

# Compare song place vs song popularity
# See which song has the closest popularity and place and which album has the closest popularity and place
# Bar graph that shows the similarity between the two variables

fan_critic_similarity <- function(spotify_songs, spotify_albums) { # Change these later
  spotify_songs$Difference <- abs(spotify_songs$Place - spotify_songs$Popularity)
  #spotify_songs <- filter(spotify_songs, Difference == min(Difference) & Difference == max(Difference))
  
  spotify_albums$Difference <- abs(spotify_albums$Place - spotify_albums$Popularity)
  #spotify_albums <- filter(spotify_albums, Difference == min(Difference) & Difference == max(Difference))
}

spotify_songs$Difference <- abs(spotify_songs$Place - spotify_songs$Popularity)
#spotify_songs <- filter(spotify_songs, Difference == min(Difference) & Difference == max(Difference))
  
spotify_albums$Difference <- abs(spotify_albums$Place - spotify_albums$Popularity)
#spotify_albums <- filter(spotify_albums, Difference == min(Difference) & Difference == max(Difference))

#RS_Spotify_albums <- albums %>%
#  select(Place, Artist, Album, Followers, Popularity) %>% 
#  arrange(desc(Popularity)) %>% 
#  head(300)
#RS_Spotify_songs <- songs %>%
#  select(Place, Artist, Song, Followers, Popularity) %>% 
#  arrange(desc(Popularity)) %>% 
#  head(300)

# Check to see which scores are the closest
#RS_Spotify_albums$Score <- abs(RS_Spotify_albums$Place - (RS_Spotify_albums$Popularity * 5))
#RS_Spotify_songs$Score <- abs(RS_Spotify_songs$Place - (RS_Spotify_songs$Popularity * 5))
#RS_Spotify_albums <- RS_Spotify_albums %>% 
#  arrange(desc(Score))
#RS_Spotify_songs <- RS_Spotify_songs %>% 
#  arrange(desc(Score))
#View(RS_Spotify_albums)
#View(RS_Spotify_songs)

## Question 4: How well do sales dictate greatness? (Spencer)
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

