# analisis-de-musica
Trabajo para minería de datos UAI

### Grupo
Sebastian Diaz, Daniel Sateler y Valentina Kaufmann

### Descripción

El objetivo principal de este encargo es crear un programa computacional que tomando alguna
canción de referencia permita crear una lista de reproducción de 3 horas de duración con
canciones “similares” a la canción de referencia. La base de datos incluye 447.622 canciones,
con 36 de las variables descritas en la documentación de la API. 

### Estructura
El codigo se divide en 4 puntos:
- Limpieza de datos
- Selección de hiperparámetros para el modelo 1: K-means
- Selección de hiperparámetros para el modelo 2: K-medoids clustering (PAM)
- Selección del modelo final entre modelo 1 y modelo 2
