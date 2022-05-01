library("ggplot2")
library("FactoMineR")
library("factoextra")


#Lectura de Data y limpieza de esta.
rds <- readRDS("beats.rds")
songs_df <- as.data.frame(rds)

# Limpiamos los datos que no necesitamos
#songs_clean_df <- df[-c(100000:nrow(df)), ]    # Descomentar para optimizar (data incompleto)
songs_clean_df = subset(songs_df, select = -c(key_mode, mode_name, key_name, album_name, external_urls.spotify, track_uri, type, track_preview_url, is_local, track_href, explicit, disc_number, time_signature, analysis_url, mode, album_release_date_precision, album_release_year, album_release_date, album_type, album_id, artist_id))
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


# Eliminamos los outliers para evitar problemas ------------------ Esto hay que hacerle fine tuning
songs_no_out <- songs_numeric_df[-c(67066,26176),]
songs_scale_no_out <- scale(songs_no_out)
# str(songs_scale_no_out)

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

kmeansTunning(songs_scale_no_out, maxK = 8)
# Obtenemos que 6 clusters es lo ideal


# Hacemos el k-means con 6 clusters
RNGkind(sample.kind = "Rounding")
set.seed(100)
spot_km <- kmeans(x = songs_scale_no_out,centers = 6)

# Visualizamos
fviz_cluster(spot_km, data = songs_scale_no_out)


# ------------------------------------------
#Agregamos los clusters a la tabla
songs_no_out$cluster <- spot_km$cluster
rmarkdown::paged_table(songs_no_out)

# Ahora hay que crear una funcion en la que pida un input del tracknumber y nos devuelva el cluster.
create_playlist <- function(track_number) {
  cluster <- songs_no_out$cluster[track_number]
  # print(cluster)

  # create a loop to get all the tracks in the same cluster
  track_list <- list()
  for (i in 1:nrow(songs_no_out)) {
    if (songs_no_out$cluster[i] == cluster) {
      #print(songs_no_out$track_number[i])
      track_list <- append(track_list, i)
    }
  }

  #create a loop from the track list and for every track, get the "duration_ms" and add it to a variable starting at 0
  duration_ms <- 0
  max_duration <- 10800000
  for (i in track_list) {
    duration_ms <- duration_ms + songs_no_out$duration_ms[i]
    # make a if that checks if the duration is greater than the max duration
    if (duration_ms > max_duration) {
      # stop the loop
      last_track <- i
      break
    }
  }
  
  # now delete all tracks from tracklist that come after the last track
  track_list <- track_list[1:last_track]
  
  return(track_list)
}

playlist <- create_playlist(1)
# print the playlist
print(playlist)
