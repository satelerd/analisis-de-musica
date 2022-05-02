library("ggplot2")
library("FactoMineR")
library("factoextra")


#Lectura de Data y limpieza de esta.
rds <- readRDS("beats.rds")
songs_df <- as.data.frame(rds)

# Limpiamos los datos que no necesitamos
#optimize <- 1
optimize <- 200000
songs_clean_df <- head(songs_df, - optimize)    
songs_clean_df = subset(songs_clean_df, select = -c(key_mode, mode_name, key_name, album_name, external_urls.spotify, track_uri, type, track_preview_url, is_local, track_href, explicit, disc_number, time_signature, analysis_url, mode, album_release_date_precision, album_release_year, album_release_date, album_type, album_id, artist_id))
songs_numeric_df = subset(songs_clean_df, select = -c(artist_name, track_id, track_name)) # Eliminamos los datos que no son numericos

# Ahora realizamos una reduccion de dimencionalidad
songs_scale <- scale(songs_numeric_df)

songs_pca <- PCA(songs_scale, 
                   scale.unit = FALSE,
                   graph = F, 
                   ncp = 10) #default: 5)

songs_pca$eig

# Graficamos los datos
plot.PCA(songs_pca, 
         choix = c("ind"),
         habillage = 1,
         select = "contrib5",
         invisible = "quali")
plot.PCA(songs_pca, choix = c("var"))


# Usamos Elbow Method para poder obtener la cantidad de clusters optima
RNGkind(sample.kind = "Rounding")
kmeansTunning <- function(data, maxK) {
  withinall <- NULL
  total_k <- NULL
  for (i in 2:maxK) {
    set.seed(1)
    temp <- kmeans(data,i)$tot.withinss
    withinall <- append(withinall, temp)
    total_k <- append(total_k,i)
  }
  plot(x = total_k, y = withinall, type = "o", xlab = "Number of Cluster", ylab = "Total within")
}

kmeansTunning(songs_scale, maxK = 8)
# Obtenemos que 6 clusters es lo ideal


# Hacemos el k-means con 6 clusters
RNGkind(sample.kind = "Rounding")
set.seed(100)
spot_km <- kmeans(x = songs_scale,centers = 6)

# Visualizamos
fviz_cluster(spot_km, data = songs_scale)


# ------------------------------------------
#Agregamos los clusters a la tabla
songs_numeric_df$cluster <- spot_km$cluster
rmarkdown::paged_table(songs_numeric_df)

# Ahora hay que crear una funcion en la que pida un input del tracknumber y nos devuelva el cluster.
create_playlist <- function(track_number) {
  cluster <- songs_numeric_df$cluster[track_number]
  # print(cluster)

  # create a loop to get all the tracks in the same cluster
  duration_ms <- 0
  max_duration <- 10800000
  track_list <- list()
  # for (i in 1:nrow(songs_numeric_df)) {
  #   if (songs_numeric_df$cluster[i] == cluster) {
  #     #print(songs_numeric_df$track_number[i])
  #     track_list <- append(track_list, i)
  #     duration_ms <- duration_ms + songs_numeric_df$duration_ms[i]
  #     # make a if that checks if the duration is greater than the max duration
  #     if (duration_ms > max_duration) {
  #       # stop the loop
  #       last_track <- i
  #       break
  #     }
  #   }
  # }

  # now do the same, but instead of a loop, do random numbers testing
  while (duration_ms < max_duration) {
    # get a random number from 1 to the number of tracks in song_numeric_df
    random_track <- sample(1:nrow(songs_numeric_df), 1)
    print(random_track)
    print(songs_numeric_df$cluster[random_track])
    if (songs_numeric_df$cluster[random_track] == cluster) {
      track_list <- append(track_list, random_track)
      duration_ms <- duration_ms + songs_numeric_df$duration_ms[random_track]
    }
  }

  
  # now delete all tracks from tracklist that come after the last track
  # track_list <- track_list[1:last_track]
  
  return(track_list)
}

playlist <- create_playlist(29521)
print(playlist)


