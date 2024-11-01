library("arules")
library("ggplot2")

data_migration <-read.csv('/home/jesfrin/Documentos/maestria/4to trimestre/mineria-de-datos/tareas/tarea3/MIGRACION_BDP.csv', sep = ",")

data_age_and_year <- data_migration[data_migration$PEI5 < 9999, c("PEI4", "PEI5")]

cluster_age_year <- kmeans(data_age_and_year, centers=3)

ggplot(data_age_and_year, aes(x = PEI4, y = PEI5, color = as.factor(cluster_age_year$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster_age_year$centers), aes(x=PEI4, y = PEI5), color = "black", size=4, shape=17)+
  labs(title = "Edad vs Año de migracion",
       x = "Edad",
       y = "Año de Migración"   
       )+
  theme_minimal()




data_location <- data_migration[data_migration$PEI5 < 9999, c("MUNICIPIO", "ZONA")]
data_location$MUNICIPIO <- as.numeric(as.character(data_location$MUNICIPIO))
data_location$ZONA <- as.numeric(as.character(data_location$ZONA))


cluster_location <- kmeans(data_location, centers = 4)


ggplot(data_location, aes(x = MUNICIPIO, y = ZONA, color = as.factor(cluster_location$cluster))) +
  geom_point() +
  geom_point(data = as.data.frame(cluster_location$centers), aes(x = MUNICIPIO, y = ZONA), color = "black", size = 4, shape = 17) +
  labs(title = "Municipio vs Zona",
       x = "Municipio",
       y = "Zona") +
  theme_minimal()

