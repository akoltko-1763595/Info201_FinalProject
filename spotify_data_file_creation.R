library(Rspotify)
library(dplyr)
source("spotify_token.R")

albums <- read.csv("data/RollingStonesTop500Albums.csv", stringsAsFactors = FALSE)
songs <- read.csv("data/RollingStonesTop500Songs.csv", stringsAsFactors = FALSE)

#Create blank columns to be added to
albums$Artist_Id <- NA
albums$Followers <- NA
albums$Popularity <- NA

songs$Artist_Id <- NA
songs$Followers <- NA
songs$Popularity <- NA
songs$song_popularity <- NA

#For every Artist in the Rolling Stone list, retrieve their artist_id, popularity, and followers and add to albums
for (num in 1:500) {
  filtered_albums <- albums %>% 
    filter(Place == num)
  current_artist <- searchArtist(filtered_albums$Artist, token = my_token)
  if (nrow(current_artist) > 1) {
    current_artist <- current_artist %>% head(1)
  }
  albums$Artist_Id[num] = current_artist$id
  albums$Followers[num] = current_artist$followers
  albums$Popularity[num] = current_artist$popularity
}

#For every Artist in the Rolling Stone list, retrieve their artist_id, popularity, and followers and add to songs
for (num in 1:500) {
  filtered_songs <- songs %>% 
    filter(Place == num)
  current_artist <- searchArtist(filtered_songs$Artist, token = my_token)
  if (nrow(current_artist) > 1) {
    current_artist <- current_artist %>% head(1)
  }
  songs$Artist_Id[num] = current_artist$id
  songs$Followers[num] = current_artist$followers
  songs$Popularity[num] = current_artist$popularity
}

for (num in 1:500) {
  filtered_songs <- songs %>% 
    filter(Place == num)
  current_song <- searchTrack(filtered_songs$Song, token = my_token)
  if (nrow(current_song) > 1) {
    current_song <- current_song %>% head(1)
  }
  songs$song_popularity[num] = current_song$popularity
}

#New csv files with spotify data
write.csv(albums, "data/RSAlbumsWithSpotifyData.csv")
write.csv(songs, "data/RSSongsWithSpotifyData.csv")


