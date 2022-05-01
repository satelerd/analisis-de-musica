library("ggplot2")
library("FactoMineR")
library("factoextra")


#Lectura de Data y limpieza de esta.
rds <- readRDS("beats.rds")
df <- as.data.frame(rds)

# Limpiamos los datos que no necesitamos
row_clean_df <- df[-c(100000:nrow(df)), ]
clean_df = subset(row_clean_df, select = -c(key_mode, mode_name, key_name, album_name, external_urls.spotify, track_uri, type, track_preview_url, is_local, track_href, explicit, disc_number, time_signature, analysis_url, mode, album_release_date_precision, album_release_year, album_release_date, album_type, album_id, artist_id))
numeric_df = subset(clean_df, select = -c(artist_name, track_id, track_name))

# Ahora realizamos una reduccion de dimencionalidad
df_scale <- scale(numeric_df)

pca_df <- PCA(df_scale, 
                   scale.unit = FALSE,
                   graph = F, 
                   ncp = 10) #default: 5)

pca_df$eig

# Graficamos los datos
plot.PCA(pca_df, 
         choix = c("ind"),
         habillage = 1,
         select = "contrib5",
         invisible = "quali")
plot.PCA(pca_df, choix = c("var"))