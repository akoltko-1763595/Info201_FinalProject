#install.packages("Rspotify")
library(Rspotify)
library(httr)
library(jsonlite)

# Use spotifyOAuth to gain authenticatoin
# Client_id and client_secret on Spotify for Developers dashboard
my_client_id <- "your-key-here"
my_client_secret <- "your-key-here"

# Redirect URI needed to be whitelisted on Spotify for Developers dashboard, URI found in rejected redirect URI, decoded
my_token <- spotifyOAuth(app_id="spotify_token",client_id = my_client_id,client_secret = my_client_secret)

