####################################
#Creado por Fernando Dorantes Nieto
#                                   <(°) 
#                                     ( >)"
#                                      /|
####################################


library(magrittr)
c("dplyr","tidyr","lubridate","tseries", "astsa","forecast","ggplot2", 
  "lattice", "dygraphs","XLConnect", "palettetown", "xts", "Rfacebook",
  "twitteR") %>%  
  sapply(require, character.only=T)




setwd("~/local/TimeSeries18/")


# Funciones ---------------------------------------------------------------

"%!in%" <- function(x,y)!("%in%"(x,y))

# Datos -------------------------------------------------------------------
fb <- read.csv("datos/facebook/datosHomologadosFB.csv",
               header = T)

tw <- read.csv("datos/twitter/datosHomologadosTW.csv")

ins <- read.csv("datos/instagram/datosHomologadosIN.csv")

yt <- read.csv("datos/youtube/datosHomologadosYT.csv")


######## ENGANCHE
interaccionesEnganche <- c(443306,410142, 364830,359057,491580,466709,453217,
                           369618,308420, 263196,184779,94236,74403,74767,129481,
                           144873,160474,177282,228588,176236,177472,255553,94847,
                           77876,121251,162493,92327,81270,66835,
                           75607,81879,77246,50976,67341,64934,63627,
                           69444,70018, 63454,57732,
                           57065, 64103,66721, 55491, 53966, 54594,52520,
                           49986,49121,55990, 46112, 43016, 44710, 30034,
                           32858, 43748)

FansTotales <- c(356210, 444359, 524536, 614642,694754, 766517, 809811, 834032, 
                878055, 960524, 993974,1067261, 1070950, 1124330, 1170256, 
                1225499, 1266445,1303574,1362823, 1429278, 1477808,1559615, 
                1572561,1576928, 1580582, 1590939, 1591753,1551294, 1556634,
                1559181, 1558787,1560633,1562967,1564392,1566641,1572749,
                1574437,1574426,1576277, 1551294, 1556634, 1559181,1558787,
                1560633, 1562967,1599785,1604375,1611016,1615297,1624452,
                1632920, 1651192, 1662813, 1672601, 1674674, 1681819)



# Globales ----------------------------------------------------------------

fechaFinal <- as.Date("2019-01-01")



# Pronósticos facebook ----------------------------------------------------

###FANS TOTALES
totalesFansDiario <- xts(fb[,2], as.Date(fb[,1])) 

secuenciaTotales <- seq_along(totalesFansDiario)

modeloLineal <- lm(coredata(totalesFansDiario) ~ secuenciaTotales) 
diferenciaLineal <- difftime(fechaFinal, time(last(totalesFansDiario)))
diferenciaLineal <- ceiling(as.numeric(diferenciaLineal))

Time_predLineal <- 1:(length(totalesFansDiario) + (diferenciaLineal))

predLineal <- predict(modeloLineal, 
                      newdata = list(secuenciaTotales = Time_predLineal))

predLineal <- xts(predLineal, seq(ymd(20160101), by="day", 
                                  length.out = length(predLineal)))

fansLineal <- cbind(totalesFansDiario, predLineal) 

ggFansLineal <- data.frame(fecha = time(fansLineal), 
                        data.frame(fansLineal))



############ ENGANCHE #####################
fechas = seq(ymd('2013-01-01'),ymd('2017-08-01'), by = 'months')
suma = ((interaccionesEnganche)/FansTotales)*100
engage = xts(suma,fechas)
Tiempo = seq_along(engage)

expFct <- function(x, beta1, beta2, beta3){exp(-beta1 * x)/(beta2 + beta3 * x)}

###### Modelo
nolineal <- nls(coredata(engage)~expFct(x = Tiempo ,beta1, beta2, beta3),
                start = list(beta1 = 0.1, beta2 = 0.02, beta3 = 0.03))


diferenciaEngage <- difftime(fechaFinal, last(fechas))
diferenciaEngage <- as.numeric(diferenciaEngage)/30
diferenciaEngage <- floor(diferenciaEngage)-1

TIME_PRED <- 1:(length(Tiempo) + diferenciaEngage)

pred.en <- xts(predict(nolineal, newdata = list(Tiempo = TIME_PRED)),
              seq(ymd(20130101), by = "month", length.out = length(engage) + 
                    diferenciaEngage))

engagement <- cbind(engage, pred.en)
names(engagement) <- c("Observado","No lineal")
tiempoe <- rep(time(engagement), 2)
grafoEn <- cbind(engage %>%  na.omit(), pred.en )
names(grafoEn) <- c("Engagement", "Estimación (No lineal)")

ggGrafoEn <- data.frame(fecha = time(grafoEn), 
                       data.frame(grafoEn))

observadoAgos <- engage["2017-08-01"] %>% 
  as.numeric %>% 
  round(digits=2)

enero <- pred.en["2018-01-01"]  %>% 
  as.numeric %>% 
  round(digits=2)

junio <- pred.en["2018-06-01"] %>% 
  as.numeric %>% 
  round(digits=2)

diciembre <- pred.en["2018-12-01"] %>% 
  as.numeric %>% 
  round(digits=2)

correlacion <- cor(coredata(engage), predict(nolineal))

ggGrafoEn %>% 
  gather(tipo, valor, -fecha) %>% 
  ggplot(aes(x = fecha, y=valor, color=tipo))+
  geom_line()+
  geom_point()+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "top")+
  scale_color_manual(values = c("darkred", "steelblue"),
                     name="", labels=c("Observado", "Estimación"))+
  scale_x_date(date_breaks = "2 months")+
  scale_y_continuous(limits = c(0,130), breaks=seq(0, 130, 10))+
  xlab("")+
  ylab("Enganche %")+
  geom_vline(xintercept = as.numeric(as.Date("2017-08-01")),
             linetype=4, color="darkred")+
  geom_vline(xintercept = as.numeric(as.Date("2018-01-01")),
             linetype=4, color="steelblue")+
  geom_vline(xintercept = as.numeric(as.Date("2018-06-01")),
             linetype=4, color="steelblue")+
  geom_vline(xintercept = as.numeric(as.Date("2018-12-01")),
             linetype=4, color="steelblue")+
  geom_text(aes(x = as.Date('2017-07-01'), y= 100, 
                label= paste('Observado', "2017-08-01",  
                             observadoAgos, "%",  sep="\t")), angle=90,
            color="darkred")+ 
  geom_text(aes(x = as.Date('2017-12-01'), y= 100, 
                label= paste('Pronóstico', '2018-01-01',  
                             enero, "%",  sep="\t")), angle=90) +
  geom_text(aes(x = as.Date('2018-05-01'), y= 100, 
                label= paste('Pronóstico', '2018-06-01',  
                             junio, "%",  sep="\t")), angle=90) +
  geom_text(aes(x = as.Date('2018-11-01'), y= 100, 
                label= paste('Pronóstico', '2018-12-01',  
                             diciembre, "%",  sep="\t")), angle=90) 
  


ggGrafoEn %>% 
  filter(fecha>"2015-01-01") %>% 
  gather(tipo, valor, -fecha) %>% 
  ggplot(aes(x = fecha, y=valor, color=tipo))+
  geom_line()+
  geom_point()+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "top") +
  scale_color_manual(values = c("darkred", "steelblue"),
                     name="", labels=c("Observado", "Estimación"))+
  scale_x_date(date_breaks = "2 months")+
  scale_y_continuous(limits = c(0,12), breaks=1:12)+
  xlab("")+
  ylab("Enganche %")+
  geom_vline(xintercept = as.numeric(as.Date("2018-01-01")) )+
  geom_vline(xintercept = as.numeric(as.Date("2017-08-01")),
             linetype=4, color="darkred")+
  geom_vline(xintercept = as.numeric(as.Date("2018-01-01")),
             linetype=4, color="steelblue")+
  geom_vline(xintercept = as.numeric(as.Date("2018-06-01")),
             linetype=4, color="steelblue")+
  geom_vline(xintercept = as.numeric(as.Date("2018-12-01")),
             linetype=4, color="steelblue")+
  geom_text(aes(x = as.Date('2017-07-01'), y= 7, 
                label= paste('Observado', "2017-08-01",  
                             observadoAgos, "%",  sep="\t")), angle=90,
            color="darkred")+ 
  geom_text(aes(x = as.Date('2017-12-01'), y= 7, 
                label= paste('Pronóstico', '2018-01-01',  
                             enero, "%",  sep="\t")), angle=90) +
  geom_text(aes(x = as.Date('2018-05-01'), y= 7, 
                label= paste('Pronóstico', '2018-06-01',  
                             junio, "%",  sep="\t")), angle=90) +
  geom_text(aes(x = as.Date('2018-11-01'), y= 7, 
                label= paste('Pronóstico', '2018-12-01',  
                             diciembre, "%",  sep="\t")), angle=90) 







# Pronósticos twitter -----------------------------------------------------




# Pronósticos YouTube -----------------------------------------------------




# Pronósticos sitio web ---------------------------------------------------




