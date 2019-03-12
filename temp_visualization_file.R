library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)


album_file <- read.csv("data/RollingStonesTop500Albums.csv", stringsAsFactors = FALSE)
song_file <- read.csv("data/RollingStonesTop500Songs.csv", stringsAsFactors = FALSE)

band_scores <- function(song_file, album_file) {
  album_file$album_score <- 501 - album_file$Place
  song_file$song_score <- 501 - song_file$Place
  
  artist_album_data <- album_file %>% 
    group_by(Artist) %>% 
    summarize(
      total_album_score = sum(album_score),
      num_of_great_albums = length(album_score)
    )
  
  artist_song_data <- song_file %>% 
    group_by(Artist) %>% 
    summarize(
      total_song_score = sum(song_score),
      num_of_great_songs = length(song_score)
    )
  
  combined_data <- full_join(artist_album_data, artist_song_data, by = "Artist")
  combined_data$overall_place <- rowSums(cbind(combined_data$total_album_score, combined_data$total_song_score), na.rm = TRUE)
  
  combined_data <- arrange(combined_data, desc(overall_place))
  
  return(combined_data)
}

data <- band_scores(song_file, album_file)

data_spread <- data %>% 
  gather(
    key = "Scores",
    value = "Score_values",
    total_album_score, total_song_score
  ) %>% arrange(desc(overall_place)) %>% 
  head(20)

data <- data %>% 
  arrange(desc(overall_place)) %>% 
  head(10) %>% 
  arrange(overall_place) %>% 
  mutate(Artist = factor(Artist, Artist))


plot <- ggplot(data = data_spread) +
  geom_col(mapping = aes(x = Artist, y = Score_values, fill = Scores), position = "stack") +
  scale_fill_manual(values=c("#739E88", "#DE646C")) +
  #geom_col(mapping = aes(x = Artist, y = data_spread$Score_values, fill = data_spread$Scores), position = "stack") +
  coord_flip() +
  labs(title = "The Top 10 Bands According to Rolling Stone!", x = "Bands", y = "Score") +
  theme_minimal() +
  theme(
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),
    plot.title = element_text(size=20, hjust = 0.5)
  )

plot
