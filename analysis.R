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
  inner_join(
    RS_SP_pop,
    AS_pop,
    by = c("Artist")
  )

  

RS_SP_AS_pop <- RS_SP_AS_pop %>% arrange(RS_Place)
RS_SP_AS_pop$RS_Place = rownames(RS_SP_AS_pop)
RS_SP_AS_pop <- RS_SP_AS_pop %>% arrange(Spotify_Place)
RS_SP_AS_pop$Spotify_Place = rownames(RS_SP_AS_pop)
RS_SP_AS_pop <- RS_SP_AS_pop %>% arrange(desc(minimum_sales), desc(probable_sales))
RS_SP_AS_pop$AS_Place = rownames(RS_SP_AS_pop)

  
  
RS_SP_AS_pop <-
  RS_SP_AS_pop %>% 
  mutate(
    RS_Place = as.numeric(RS_Place),
    AS_Place = as.numeric(AS_Place),
    Spotify_Place = as.numeric(Spotify_Place)
  ) %>% 
  mutate(
    Avg_Place = (RS_Place + AS_Place + Spotify_Place)/3
  ) %>% 
  arrange(Avg_Place) 

RS_SP_AS_pop$Overall_Place = as.numeric(rownames(RS_SP_AS_pop))

RS_SP_AS_pop <-
  RS_SP_AS_pop %>% 
  mutate(
    RS_Dist = abs(Overall_Place - RS_Place),
    Spotify_Dist = abs(Overall_Place - Spotify_Place),
    AS_Dist = abs(Overall_Place - AS_Place)
  )
library(ggplot2)

ggplot(data = RS_SP_AS_pop) +
  geom_smooth(mapping = aes(x = Overall_Place, y = Avg_Place, color = "Average"), se = F, size = 1.5) +
  geom_smooth(mapping = aes(x = Overall_Place, y = RS_Place, color = "Rolling Stone"), se = F, size = 1.5) +
  geom_smooth(mapping = aes(x = Overall_Place, y = Spotify_Place, color = "Spotify"), se = F, size = 1.5) +
  geom_smooth(mapping = aes(x = Overall_Place, y = AS_Place, color = "Total Sales"), se = F, size = 1.5) + 
  scale_color_manual(
    name = "Legend",
    values = c(
      Spotify="#739E88", 
      "Rolling Stone"="#DE646C", 
      Average = "Black", 
      "Total Sales" = "#e6e600"
    )
  ) + 
  labs(
    x = "Overall Place", 
    y = "Individual Ranking"
  ) 

## Question 3: What do fans and critics agree on? (Alex)




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

