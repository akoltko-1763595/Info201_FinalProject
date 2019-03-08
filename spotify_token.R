#install.packages("Rspotify")
library(Rspotify)
library(httr)
library(jsonlite)

# Use spotifyOAuth to gain authenticatoin
# Client_id and client_secret on Spotify for Developers dashboard
# Redirect URI needed to be whitelisted on Spotify for Developers dashboard, URI found in rejected redirect URI, decoded
my_token <- spotifyOAuth(app_id="spotify_token",client_id = my_client_id,client_secret = my_client_secret)

my_client_id <- "185d2155b3d64e4687a9fd43b6988613"
my_client_secret <- "d6703d27da4548a6be4974552a8e8d55"
