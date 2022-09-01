# <- read.csv("C:/Users/Stefanny/OneDrive/estadística/Anime_limpio.csv")
Anime_limpio <- read.csv("F:/2022-2/ESTADÍSTICA I/Anime_limpio.csv")
Anime_limpio
str(Anime_limpio)
library(fdth)

# Separació de variables  -------------------------------------------------

vars =  c('TV', 'Movie')
VarBin = filter(Anime_limpio, type == vars )
dim(VarBin)
VarTV = filter(VarBin, type == 'TV' )
VarMovies = filter(VarBin, type == 'Movie' )
tipos = unique(VarBin$type)
tipos
rating =  unique(VarBin$rating)
rating

# Histograma de estimaciones ----------------------------------------------
# Muestreo repetido -------------------------------------------------------
n = 100 
m = 1500
sample_new = sample(VarTV$rating, n*m, replace = TRUE)
Muestreo_repetido =matrix(sample_new, nrow = n, ncol =                            m)
Muestreo_repetido[,2]
medias_muestralesTv = apply(Muestreo_repetido, 2, mean)
mean(medias_muestralesTv)
MiuTv = mean(VarTV$rating)
MiuTv
# Sample Movies
sample_Movie = sample(VarMovies$rating, n*m, replace = TRUE)
Muestreo_Movie =matrix(sample_Movie, nrow = n, ncol =
                            m)
Muestreo_Movie[,2]
medias_muestralesM = apply(Muestreo_Movie, 2, mean) 
mean(medias_muestralesM)
MiuMovie = mean(VarMovies$rating)
MiuMovie

# Medias ------------------------------------------------------------------
mean(medias_muestralesM)
MiuMovie
mean(medias_muestralesTv)
MiuTv
sd(medias_muestralesM)
sd(medias_muestralesTv)
# Aproximación por medio de normal ----------------------------------------
histograma = hist(medias_muestralesM, 
                  ylim = c(0,3), 
                  col = 'steelblue',
                  freq = FALSE,
                  breaks = 20,
                  main = 'Distribución de medias muestrales')

# Aproximacion con la curva de densidad -----------------------------------
curve(dnorm(x, mean = mean(medias_muestralesM), sd = sd(medias_muestralesM)),
      col = 'red', lwd = "2", add = TRUE)





# Intervalos de confianza -------------------------------------------------



qnorm(0.025)

















