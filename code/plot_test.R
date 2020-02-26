spotify_df <- get_artist_audio_features('bruce springsteen')

spotify_df %>%
  head()

### remove non-studio albums

spotify_df %>% 
  select(album_name) %>%
  unique()

non_studio_albums <- c('Western Stars - Songs From The Film',
                       'Springsteen on Broadway',
                       'Chapter and Verse',
                       'Live In Dublin',
                       "Hammersmith Odeon, London '75",
                       'Live in New York City',
                       'Live in New York City (Alben für die Ewigkeit)',
                       'Tracks',
                       'Bruce Springsteen & The E Street Band Live 1975-85',
                       'The Ties That Bind: The River Collection',
                       'The Promise')

spotify_df <- spotify_df %>%
  filter(! album_name %in% non_studio_albums)

temp <- spotify_df %>%
  select(album_images)
urls <- c()
for(i in 1:nrow(temp)){
  urls <- c(urls, temp[[1]][[i]][1,2])
}

spotify_df$album_img <- urls


plot_df <- spotify_df %>% 
              rowwise %>% 
              mutate(tooltip = paste0('<a style = "margin-right:', max(max(nchar(track_name), nchar(album_name)) * 7, 55), 'px">', # dynamic sizing
                          '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                          '<b>Album:</b> ', album_name,
                          '<br><b>Track:</b> ', track_name)) %>% 
              ungroup

avg_line <- plot_df %>% 
                group_by(album_release_date, album_name, album_img) %>% 
                summarise(avg = mean(valence)) %>% 
                ungroup %>% 
                transmute(x = as.numeric(as.factor(album_release_date)), 
                y = avg,
                tooltip = paste0('<a style = "margin-right:55px">',
                             '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                             '<b>Album:</b> ', album_name,
                             '<br><b>Average Valence:</b> ', round(avg, 2),
                             '</a>'))

plot_track_df <- plot_df %>% 
                  mutate(tooltip = paste0(tooltip, '<br><b>Valence:</b> ', valence, '</a>'),
                         album_number = as.numeric(as.factor(album_release_date))) %>% 
                  ungroup

colourCount = length(unique(spotify_df$album_release_date))
getPalette = colorRampPalette(brewer.pal(12, "Accent"))

album_chart <- hchart(plot_track_df, 'scatter', hcaes(x = as.numeric(as.factor(album_release_date)), y = valence, group = album_name)) %>% 
  hc_add_series(data = avg_line, type = 'line') %>%
  hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>% 
  hc_colors(c(getPalette(colourCount))) %>% 
  hc_xAxis(title = list(text = 'Album'), labels = list(enabled = F)) %>% 
  hc_yAxis(max = 1, title = list(text = 'Valence')) %>% 
  hc_title(text = 'Springsteen Song Valence by Album') %>% 
  hc_subtitle(text = 'Tramps like us...') %>% 
  hc_add_theme(hc_theme_smpl())
album_chart$x$hc_opts$series[[19]]$name <- 'Album Averages'
album_chart

class(album_chart)

library(htmlwidgets)
library(htmltools)

currentWD <- getwd()
dir.create("static")
dir.create("static/leaflet")
setwd("static/leaflet")
saveWidget(album_chart, "Bruce_album.html")
setwd(currentWD)
