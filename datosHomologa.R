####################################
#Creado por Fernando Dorantes Nieto
#                                   <(°) 
#                                     ( >)"
#                                      /|
####################################
library(magrittr)
c("data.table", "dplyr","tidyr","lubridate","tseries", 
  "astsa","forecast","ggplot2", "lattice", "dygraphs",
  "XLConnect", "palettetown", "xts", "Rfacebook",
  "twitteR", "readxl") %>%  
  sapply(require, character.only=T)


setwd("~/local/TimeSeries18/")


# Facebook ----------------------------------------------------------------
datos1 <- read.csv("datos/facebook/2016.csv", header = T)

datos1 <- datos1 %>% 
  select(Date, Lifetime.Total.Likes, Daily.Total.Reach, Daily.Organic.Reach) %>% 
  data.table %>% 
  .[, Date := as.Date(Date)]



archivos <- list.files("datos/facebook/", pattern = "*.xlsx", full.names = T)

datos2 <- lapply(archivos, function(hoja){
  Y <- read_excel(hoja, sheet = 1 ) %>% 
    data.frame
  names(Y)<- gsub(".Total", "", names(Y))
  names(Y)<- gsub("Alcance.total.por.día", "Alcance.total.Diario", names(Y))
  names(Y)<- gsub("Alcance.orgánico.por.día", "Alcance.orgánico.Diario",
                  names(Y))
  names(Y) <- iconv(names(Y), to = "ASCII//TRANSLIT")
  Y <- Y %>%
    select(Fecha, Total.de.Me.gusta, Alcance.total.Diario, 
           Alcance.organico.Diario) %>%
    data.table %>%
    .[, Fecha := as.Date(Fecha)]
  return(Y)
}) %>% do.call("rbind", .)

names(datos1) <- names(datos2)

datosHomologaFB <- rbind(datos1, datos2) %>% 
  unique

#### datos raros de FB
maxAlcance <- datosHomologaFB %>% 
  group_by(Fecha) %>% 
  summarise(alcanceMax = max(Alcance.organico.Diario))

maxTotal <- datosHomologaFB %>% 
  group_by(Fecha) %>% 
  summarise(totalMax = max(Total.de.Me.gusta))


datosHomologaFB <- datosHomologaFB %>% 
  left_join(maxAlcance, by="Fecha") %>% 
  filter( Alcance.organico.Diario == alcanceMax) %>% 
  select(- alcanceMax)

datosHomologaFB <- datosHomologaFB %>% 
  left_join(maxTotal, by="Fecha") %>% 
  filter( Total.de.Me.gusta == totalMax) %>% 
  select(-totalMax)

datosHomologaFB %>% 
  write.csv("datos/facebook/datosHomologadosFB.csv",
            row.names=F)



# Twitter -----------------------------------------------------------------
datos1 <- read_excel("datos/twitter/TW - Engagement - SEAT México - 2017-09-25.xlsx",
                     sheet = 3, skip = 1) %>% 
  data.frame()

datos2 <- read_excel("datos/twitter/TW - Followers - SEAT México - 2017-09-25.xlsx",
                     sheet = 2, skip = 1) %>% 
  data.frame()

datos1 <- datos1 %>% 
  select(Date, Likes, Replies, Retweets, Total.Interactions)

datos2 <- datos2 %>% 
  select(Date, Followers)

datosHomologaTW <- datos2 %>% 
  left_join(datos1, by="Date")

names(datosHomologaTW)<- c("Fecha", "Seguidores", "MeGusta", "Respuestas",
                           "Retweets", "Totales")

datosHomologaTW %>% 
  write.csv("datos/twitter/datosHomologadosTW.csv",
            row.names=F)


# Instagram ---------------------------------------------------------------
datos1 <- read_excel("datos/instagram/IG - Engagement - SEAT México - 2017-09-25.xlsx",
                     sheet = 3, skip = 1) %>% 
  data.frame()

datos2 <- read_excel("datos/instagram/IG - Followers - SEAT México - 2017-09-25.xlsx",
                     sheet = 2, skip = 1) %>% 
  data.frame()

datos1 <- datos1 %>% 
  select(Date, Likes, Comments,  Total.Interactions)

datos2 <- datos2 %>% 
  select(Date, Followers)

datosHomologaIN <- datos2 %>% 
  left_join(datos1, by="Date")

names(datosHomologaIN) <- c("Fecha", "Seguidores", "MeGusta", "Comentarios", 
                            "Totales")

datosHomologaIN %>% 
  write.csv("datos/instagram/datosHomologadosIN.csv",
            row.names=F)



# YouTube -----------------------------------------------------------------
datos1 <- read.csv("datos/youtube/insights/suscritos.csv",
                   header = T)
datos2 <- read.csv("datos/youtube/visualizaciones/vistas.csv",
                   header = T)

datos1 <- datos1 %>% 
  select(date, subscribers)

datos2 <- datos2 %>% 
  select(date, views)

datosHomologaYT <- datos1 %>%
  left_join(datos2, by="date")

names(datosHomologaYT) <- c("Fecha", "Subscriptores", "Vistas")

datosHomologaYT %>% 
  write.csv("datos/youtube/datosHomologadosYT.csv",
            row.names=F)



