library("ggplot2")
library("FactoMineR")
library("factoextra")
library("gtools")
library("ClusterR")
library("cluster")


#Lectura de Data y limpieza de esta.
rds <- readRDS("beats.rds")
songs_df <- as.data.frame(rds)

#PUNTO 1 (Limpieza de datos)

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

#PUNTO 2 (Selección de hiperparámetros para el modelo 1)

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

#PUNTO 3 (Selección de hiperparámetros para el modelo 2)
#K-medoids clustering (PAM)

datos1 <- as.data.frame(songs_scale_no_out)
datos <- datos1[-c(65536:nrow(df)), ]
fviz_nbclust(x = datos, FUNcluster = pam, method = "wss", k.max = 15,
             diss = dist(datos, method = "manhattan"))

set.seed(1)
pam_clusters <- pam(x = datos, k = 4, metric = "manhattan")
pam_clusters
fviz_cluster(object = pam_clusters, data = datos, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")

#Punto 4 (Selección del modelo final entre modelo 1 y modelo 2)


