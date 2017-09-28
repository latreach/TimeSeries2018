 ####################################   
#Creado por Fernando Dorantes Nieto <(°) 
#                                     ( >)"
#                                      /| 
####################################

######
#Cargando las librerías
######
# Librerias ---------------------------------------------------------------
library(magrittr)
c("data.table", "dplyr", "tidyr", "lubridate", "Rfacebook", "ggplot2", "googlesheets") %>% 
  sapply(require,character.only=T)


# Realizando la conexión APIS REDES SOCIALES

# Conexión facebook API ---------------------------------------------------
fb_oauth <- fbOAuth(app_id="1611650985792093", 
                    app_secret="85db5a49077d074e84b5ce0a19659893", 
                    extended_permissions = TRUE)
save(fb_oauth, file="fb_oauth")
load("~/fb_oauth")
idFB_seat = 113144262054871


setwd("~/local/TimeSeries18/")
# Funciones ---------------------------------------------------------------
"%!in%" <- function(x,y)!("%in%"(x,y))

seatFB <- getPage(idFB_seat, n = 1e5, since="2016-01-01", until= Sys.Date(),
                    feed=F, reactions=F, token = fb_oauth)

seatFB %>% 
  write.csv("datos/facebook/Posteo_seat2016.csv",
            row.names=F)


token<- "EAACEdEose0cBANgEwLLLzJdAfrZAZC4TVONbJ3cuWMa27edko94kGbqZBsjtyc82uMTZA5JtSYzvoZB9kjCcZASCZBgw8lZB5uXH1Atf4RQsWcMrf5Rbj2eiQbwaVS7Gq0zUzZAszs9fGYbQQjfcwRQ75avJ1dxcoSs3YVkfmQFlEHoVoIuJHcKdpxX3qywoxLOgZD"

strfb <- "https://graph.facebook.com/v2.10/"
stringimprReachPost <- "/insights/post_impressions_organic_unique"
stringReactions <- "/insights/post_reactions_by_type_total"
strtk <- "?access_token="

impressionsFbPost <- lapply(seatFB$id, function(d){
  z <- readLines(paste0(strfb, d,  stringimprReachPost, strtk, token ))
  z <- unlist(z) %>% fromJSON
  z <- z %>% unlist %>%  
    unname %>% .[3]
  print(d)
  Y <- data.frame(impressionsPost= as.numeric(z), id= d)
  return(Y)
}) %>% 
  do.call("rbind", .)

impressionsFbPost %>% 
  write.csv("datos/facebook/impressionsFbPost.csv", row.names=F)


reaccionesFbPost <- lapply(seatFB$id, function(d){
  z <- readLines(paste0(strfb, d,  stringReactions, strtk, token ))
  z <- unlist(z) %>% fromJSON
  z <- z %>%  .$data
  z <- z %>%  unlist
  z <- z[grepl("[0-9]", z)]
  z <- z[!grepl("[A-Za-z]", z)]
  z <- z %>% unname %>% as.numeric %>%  sum
  Y <- data.frame(reacciones= as.numeric(z), id= d)
  print(d)
  return(Y)
}) %>% 
  do.call("rbind", .)

reaccionesFbPost %>% 
  write.csv("datos/facebook/reaccionesPost.csv", row.names=F)


fechasPost <- seatFB %>% 
  separate(created_time, c("Fecha", "Hora"),
           sep="T") %>% 
  select(id, Fecha, comments_count, shares_count )

unionAlcance <- merge(reaccionesFbPost, impressionsFbPost, by="id")

unionAlcance <- unionAlcance %>% 
  left_join(fechasPost, by="id") %>% 
  filter(impressionsPost!=0)

unionAlcance %>% 
    data.table %>% 
    .[, interaccionesTotal := rowSums(.[,c(2,5,6)])] %>% 
    filter(interaccionesTotal<impressionsPost) %>% 
    select(Fecha, interaccionesTotal, impressionsPost) %>% 
    group_by(Fecha) %>% 
    summarise_all(funs(sum)) %>% 
    data.frame %>% 
    data.table %>% 
    .[, porcentajeImpresiones := interaccionesTotal/impressionsPost] %>% 
    .[, porcentajeImpresiones := porcentajeImpresiones*100] %>% 
    filter(porcentajeImpresiones<90) %>% 
    summarise(mean(porcentajeImpresiones))
    write.csv("datos/facebook/porcentajeImpressionsPost.csv", row.names=F)


