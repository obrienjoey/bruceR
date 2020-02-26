library(spotifyr)
library(tidyverse)
library(rvest)
library(httr)
library(purrr)
library(RColorBrewer)
library(highcharter)

readRDS('spotify_keys.rds') # two data objects SPOTIFY_CLIENT_ID
# and SPOTIFY_CLIENT_SECRET used to access the spotifyR package.

Sys.setenv(SPOTIFY_CLIENT_ID)
Sys.setenv(SPOTIFY_CLIENT_SECRET)

spotify_df <- get_artist_audio_features('bruce springsteen')

spotify_df %>%
  head()

spotify_df %>%
  select(album_name) %>%
  unique()

non_studio_albums <- c('Bruce Springsteen & The E Street Band Live 1975-85',
                       'Chapter and Verse', 'Greatest Hits', "Hammersmith Odeon, London '75",
                       'In Concert/MTV Plugged (Live)','Live In Dublin','Live in New York City',
                       'Live in New York City (Alben für die Ewigkeit)',
                       'Springsteen on Broadway', 'Tracks',
                       'The Essential Bruce Springsteen (Bonus Disc)', 'The Ties That Bind: The River Collection',
                       'Western Stars - Songs From The Film','Springsteen on Broadway')

spotify_df_clean <- spotify_df %>%
  filter(!album_name %in% non_studio_albums)

### genius scraping

token <- 'vDjxB7k7bPkjpGVrkt4IZVDUnMVgv2jte3TOo5xtnIRW_GcEVARc66Z24LrwpdPx'

genius_get_artists <- function(artist_name, n_results = 10) {
  baseURL <- 'https://api.genius.com/search?q=' 
  requestURL <- paste0(baseURL, gsub(' ', '%20', artist_name),
                       '&per_page=', n_results,
                       '&access_token=', token)
  
  res <- GET(requestURL) %>% content %>% .$response %>% .$hits
  
  map_df(1:length(res), function(x) {
    tmp <- res[[x]]$result$primary_artist
    list(
      artist_id = tmp$id,
      artist_name = tmp$name
    )
  }) %>% unique
}

genius_artists <- genius_get_artists('bruce springsteen')
genius_artists %>% head()

baseURL <- 'https://api.genius.com/artists/' 
requestURL <- paste0(baseURL, genius_artists$artist_id[1], '/songs')

track_lyric_urls <- list()
i <- 1
while (i > 0) {
  tmp <- GET(requestURL, query = list(access_token = token, per_page = 50, page = i)) %>% content %>% .$response
  track_lyric_urls <- c(track_lyric_urls, tmp$songs)
  if (!is.null(tmp$next_page)) {
    i <- tmp$next_page
  } else {
    break
  }
}

length(track_lyric_urls)
summary(track_lyric_urls[[1]])

filtered_track_lyric_titles <- c()
filtered_track_lyric_urls <- c()

for (i in 1:length(track_lyric_urls)){
  if (track_lyric_urls[[i]]$primary_artist$name == "Bruce Springsteen") {
    filtered_track_lyric_urls <- append(filtered_track_lyric_urls, track_lyric_urls[[i]]$url)
    filtered_track_lyric_titles <- append(filtered_track_lyric_titles, track_lyric_urls[[i]]$title)
  }
}

length(spotify_df_clean$track_name[spotify_df_clean$track_name %in% filtered_track_lyric_titles])
## this gives 170 not the 246 present so there are some naming issues

spotify_df_clean$track_name[! spotify_df_clean$track_name %in%filtered_track_lyric_titles]


#######################################
### manual cleaning

filtered_track_lyric_titles[filtered_track_lyric_titles == 'Across The Border'] <- "Across the Border"
filtered_track_lyric_titles[filtered_track_lyric_titles == 'All the Way Home'] <- "All The Way Home"
filtered_track_lyric_titles[filtered_track_lyric_titles == "Ain't Good Enough For You"] <- "Ain't Good Enough for You"
filtered_track_lyric_titles[filtered_track_lyric_titles == "American Skin (41 Shots)"] <- "American Skin (41 Shots) - Studio Version"
filtered_track_lyric_titles[filtered_track_lyric_titles == 'Because The Night'] <- "Because the Night"
filtered_track_lyric_titles[filtered_track_lyric_titles == 'Blinded by the Light'] <- "Blinded By The Light"
filtered_track_lyric_titles[filtered_track_lyric_titles == 'Book Of Dreams'] <- "Book of Dreams"
filtered_track_lyric_titles[filtered_track_lyric_titles == 'City Of Night'] <- "City of Night"
filtered_track_lyric_titles[filtered_track_lyric_titles == "Countin' On A Miracle"] <- "Countin' On a Miracle"
filtered_track_lyric_titles[filtered_track_lyric_titles == 'Crush On You'] <- "Crush on You"
filtered_track_lyric_titles[filtered_track_lyric_titles == 'Dancing In The Dark'] <- "Dancing In the Dark"
filtered_track_lyric_titles[filtered_track_lyric_titles == 'Does This Bus Stop At 82nd Street?'] <- "Does This Bus Stop at 82nd Street?"
filtered_track_lyric_titles[filtered_track_lyric_titles == 'Down In The Hole'] <- "Down in the Hole"
filtered_track_lyric_titles[filtered_track_lyric_titles == 'Eyes On The Prize'] <- "Eyes on the Prize"
filtered_track_lyric_titles[filtered_track_lyric_titles == 'Frankie Fell In Love'] <- "Frankie Fell in Love"
filtered_track_lyric_titles[filtered_track_lyric_titles == 'Further On (Up The Road)'] <- "Further On (Up the Road)"
filtered_track_lyric_titles[filtered_track_lyric_titles == 'Girls in Their Summer Clothes'] <- "Girls In Their Summer Clothes"
filtered_track_lyric_titles[filtered_track_lyric_titles == 'How Can a Poor Man Stand Such Times And Live'] <- "How Can a Poor Man Stand Such Times and Live"
filtered_track_lyric_titles[filtered_track_lyric_titles == 'How Can I Keep From Singing'] <- "How Can I Keep from Singing"
filtered_track_lyric_titles[filtered_track_lyric_titles == 'Hunter Of Invisible Game'] <- "Hunter of Invisible Game"
filtered_track_lyric_titles[filtered_track_lyric_titles == "I'm A Rocker"] <- "I'm a Rocker"
filtered_track_lyric_titles[filtered_track_lyric_titles == "Into The Fire"] <- "Into the Fire"
filtered_track_lyric_titles[filtered_track_lyric_titles == "It's A Shame"] <- "It's a Shame"
filtered_track_lyric_titles[filtered_track_lyric_titles == "Land Of Hope And Dreams"] <- "Land of Hope and Dreams"
filtered_track_lyric_titles[filtered_track_lyric_titles == "Livin' In The Future"] <- "Livin' In the Future"
filtered_track_lyric_titles[filtered_track_lyric_titles == "Lost In The Flood"] <- "Lost in the Flood"
filtered_track_lyric_titles[332] <- "Mary's Place"
filtered_track_lyric_titles[filtered_track_lyric_titles == "Mansion on the Hill"] <- "Mansion On the Hill"
filtered_track_lyric_titles[filtered_track_lyric_titles == "Mary Queen Of Arkansas"] <- "Mary Queen of Arkansas"
filtered_track_lyric_titles[filtered_track_lyric_titles == "My City Of Ruins"] <- "My City of Ruins"
filtered_track_lyric_titles[filtered_track_lyric_titles == "Out in the Street"] <- "Out In the Street"
filtered_track_lyric_titles[filtered_track_lyric_titles == "Racing In The Street ('78)"] <- "Racing in the Street - '78"
filtered_track_lyric_titles[filtered_track_lyric_titles == "Roll Of The Dice"] <- "Roll of the Dice"
filtered_track_lyric_titles[filtered_track_lyric_titles == "Souls Of The Departed"] <- "Souls of the Departed"
filtered_track_lyric_titles[filtered_track_lyric_titles == "Talk To Me"] <- "Talk to Me"
filtered_track_lyric_titles[filtered_track_lyric_titles == "Tenth Avenue Freeze Out"] <- "Tenth Avenue Freeze-Out"
filtered_track_lyric_titles[filtered_track_lyric_titles == "This Is Your Sword"] <- "This is Your Sword"
filtered_track_lyric_titles[filtered_track_lyric_titles == "Tougher Than The Rest"] <- "Tougher Than the Rest"
filtered_track_lyric_titles[578] <- "Waitin' on a Sunny Day"
filtered_track_lyric_titles[filtered_track_lyric_titles == "Walk Like A Man"] <- "Walk Like a Man"
filtered_track_lyric_titles[filtered_track_lyric_titles == "We Take Care Of Our Own"] <- "We Take Care of Our Own"
filtered_track_lyric_titles[filtered_track_lyric_titles == "Working On A Dream"] <- "Working On a Dream"
filtered_track_lyric_titles[filtered_track_lyric_titles == "Working On The Highway"] <- "Working on the Highway"
filtered_track_lyric_titles[filtered_track_lyric_titles == "Wreck On The Highway"] <- "Wreck on the Highway"
filtered_track_lyric_titles[filtered_track_lyric_titles == "Wrong Side Of The Street"] <- "Wrong Side of the Street"
filtered_track_lyric_titles[616] <- "You're Missing"
filtered_track_lyric_titles[288] <- "Let's Be Friends (Skin to Skin)"
filtered_track_lyric_titles[408] <- "Racing in the Street - '78"
filtered_track_lyric_titles[116] <- "Countin' On a Miracle"
filtered_track_lyric_titles[6] <- "57 Channels (And Nothin' On)"
#filtered_track_lyric_titles[504] <-  

spotify_df_clean$track_name[!spotify_df_clean$track_name %in% filtered_track_lyric_titles]

