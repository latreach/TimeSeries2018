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

sitioWeb <- read.csv("datos/sitio_web/seatWebPage.csv",
                     header = T)

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

### FANS TOTALES ###########
totalesFansDiario <- xts(fb[,2], as.Date(fb[,1])) 

secuenciaTotales <- seq_along(totalesFansDiario)

modeloLineal <- lm(coredata(totalesFansDiario) ~ secuenciaTotales) 
diferenciaLineal <- difftime(fechaFinal, time(last(totalesFansDiario)))
diferenciaLineal <- ceiling(as.numeric(diferenciaLineal)) +2 

Time_predLineal <- 1:(length(totalesFansDiario) + (diferenciaLineal))

predLineal <- predict(modeloLineal, 
                      newdata = list(secuenciaTotales = Time_predLineal))

predLineal <- xts(predLineal, seq(ymd(20160101), by="day", 
                                  length.out = length(predLineal)))

fansLineal <- cbind(totalesFansDiario, predLineal) 
names(fansLineal)<- c("Observado", "Pronostico")
ggFansLineal <- data.frame(fecha = time(fansLineal), 
                        data.frame(fansLineal))

observadoEneroL <- totalesFansDiario["2017-01-01"] %>% 
  as.numeric %>% 
  round(digits=2)

observadoAgosL <- totalesFansDiario["2017-08-01"] %>% 
  as.numeric %>% 
  round(digits=2)

crecimiento1LObs <- (observadoAgosL -observadoEneroL)/observadoEneroL*100

eneroL <- predLineal["2018-01-01"]  %>% 
  as.numeric %>% 
  round(digits=2)

crecimiento1L <- (eneroL - observadoEneroL)/observadoEneroL*100

junioL <- predLineal["2018-06-01"] %>% 
  as.numeric %>% 
  round(digits=2)

crecimiento2L <- (junioL -observadoEneroL)/observadoEneroL*100

diciembreL <- predLineal["2019-01-01"] %>% 
  as.numeric %>% 
  round(digits=2)

crecimiento3L <- (diciembreL -observadoEneroL)/observadoEneroL*100



ggFansLineal %>% 
  gather(tipo, valor, -fecha) %>% 
  ggplot(aes(x = fecha, y=valor, color=tipo))+
  geom_line()+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "top")+
  scale_color_manual(values = c("darkred", "steelblue"),
                     name="", labels=c("Observado", "Estimación"))+
  scale_x_date(date_breaks = "2 months")+
  xlab("")+
  ylab("Fans totales")+
  geom_vline(xintercept = as.numeric(as.Date("2017-01-01")),
             linetype=4, color="darkred")+
  geom_vline(xintercept = as.numeric(as.Date("2017-08-01")),
             linetype=4, color="darkred")+
  geom_vline(xintercept = as.numeric(as.Date("2018-01-01")),
             linetype=4, color="steelblue")+
  geom_vline(xintercept = as.numeric(as.Date("2018-06-01")),
             linetype=4, color="steelblue")+
  geom_vline(xintercept = as.numeric(as.Date("2019-01-01")),
             linetype=4, color="steelblue")+
  geom_text(aes(x = as.Date('2016-10-01'), y= 1.72e6, 
                label= paste('Observado', "2017-01-01",
                             "\n",
                             format(observadoEneroL, big.mark = ","), 
                             sep=" ")),
            color="darkred")+
  geom_text(aes(x = as.Date('2017-05-01'), y= 1.75e6, 
                label= paste('Observado',  "2017-08-01",
                             "\n",
                             format(observadoAgosL, big.mark = ","), 
                             sep=" ")), 
            # angle=90,
            color="darkred")+
  geom_text(aes(x = as.Date('2017-05-01'), y= 1.70e6, 
                label= paste( round(crecimiento1LObs, 2), "%", 
                              "crecimiento", 
                              "\n",
                              format((observadoAgosL -observadoEneroL),
                                     big.mark = ","), 
                              "nuevos fans","*",
                             sep=" ")), 
            # angle=90,
            color="darkred")+
  geom_text(aes(x = as.Date('2017-10-15'), y= 1.66e6, 
                label= paste('Pronóstico', "\n", "2018-01-01",
                             "\n",
                             format(eneroL, big.mark = ","), 
                             sep=" ")), 
            color="steelblue")+
  geom_text(aes(x = as.Date('2017-10-15'), y= 1.63e6, 
                label= paste( round(crecimiento1L, 2), "%", 
                              "crecimiento", 
                              "\n",
                              format(floor(eneroL -observadoEneroL),
                                     big.mark = ","), "\n", 
                              "nuevos fans","*",
                              sep=" ")), 
            # angle=90,
            color="steelblue")+
  geom_text(aes(x = as.Date('2018-04-01'), y= 1.70e6, 
                label= paste('Pronóstico', "\n", "2018-06-01",
                             "\n",
                             format(junioL, big.mark = ","), 
                             sep=" ")), 
            color="steelblue")+
  geom_text(aes(x = as.Date('2018-03-25'), y= 1.66e6, 
                label= paste( round(crecimiento2L, 2), "%", 
                              "crecimiento", 
                              "\n",
                              format(floor(junioL - observadoEneroL),
                                     big.mark = ","), "\n", 
                              "nuevos fans","*",
                              sep=" ")), 
            # angle=90,
            color="steelblue") +
  geom_text(aes(x = as.Date('2018-09-01'), y= 1.73e6, 
                label= paste('Pronóstico', "\n", "2019-01-01",
                             "\n",
                             format(diciembreL, big.mark = ","), 
                             sep=" ")), 
            color="steelblue")+
  geom_text(aes(x = as.Date('2018-09-01'), y= 1.70e6, 
                label= paste( round(crecimiento3L, 2), "%", 
                              "crecimiento", 
                              "\n",
                              format(floor(diciembreL - observadoEneroL),
                                     big.mark = ","), "\n", 
                              "nuevos fans","*",
                              sep=" ")), 
            # angle=90,
            color="steelblue") +
  geom_text(aes(x = as.Date('2016-05-01'), y= 1.79e6,
                label= paste("*", 'Cifras con respecto al 2017-01-01',
                             sep=" ")),
            color="black", size=4)




  

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

observadoEnero <- engage["2017-01-01"] %>% 
  as.numeric %>% 
  round(digits=2)

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


############ ALCANCE #####################
alcanceOrg <- fb %>% 
  data.table() %>% 
  .[, alcancePorcentaje := Alcance.organico.Diario/Total.de.Me.gusta] %>% 
  .[, alcancePorcentaje := alcancePorcentaje*100] 

alcancets <- xts(alcanceOrg$alcancePorcentaje, as.Date(alcanceOrg$Fecha)) 

# Pronósticos twitter -----------------------------------------------------

######### SEGUIDORES ##

seguidoresTWts <- xts(tw$Seguidores, as.Date(tw$Fecha))
seguidoresTWts <- seguidoresTWts['2015-12-01/'] 
fechasTW <- seq(ymd('2015-12-01'),ymd('2017-09-24'), by = 'days')
Tiempotw <- seq_along(seguidoresTWts)

diferenciaTWSeguidor <- difftime(fechaFinal, last(fechasTW)) %>% 
  as.numeric

diferenciaTWSeguidor <- floor(diferenciaTWSeguidor)


TIME_Prtw <- 1:(length(Tiempotw) + diferenciaTWSeguidor)


modeloLogistico <- nls(coredata(seguidoresTWts) ~ K*P0*exp(R*Tiempotw)/(K+P0 *(exp(R*Tiempotw)-1)),
                   start = list(P0 = min(coredata(seguidoresTWts), na.rm = T), 
                                K  = max(coredata(seguidoresTWts), na.rm = T), 
                                R = 0.01
                   )
) 

predTWSeguidor <- predict(modeloLogistico, 
                        newdata = list(Tiempotw = TIME_Prtw))

predTWSeguidor <- xts(predTWSeguidor, seq(ymd(20151201), by="day", 
                                  length.out = length(predTWSeguidor)))


### Falló este modelo
### Modelo lineal
seguidoresTWts <- seguidoresTWts['2016-01-01/'] 
fechasTW <- seq(ymd('2016-01-01'),ymd('2017-09-24'), by = 'days')
Tiempotw <- seq_along(seguidoresTWts)

diferenciaTWSeguidor <- difftime(fechaFinal, last(fechasTW)) %>% 
  as.numeric

diferenciaTWSeguidor <- floor(diferenciaTWSeguidor)

TIME_Prtw <- 1:(length(Tiempotw) + diferenciaTWSeguidor)

modeloLinealTW <- lm(coredata(seguidoresTWts)~Tiempotw) 

predLinealTW <- predict(modeloLinealTW, 
                      newdata = list(Tiempotw = TIME_Prtw))

predLinealTW <- xts(predLinealTW, seq(ymd(20160101), by="day", 
                                  length.out = length(predLinealTW)))

seguidoresTWLineal <- cbind(seguidoresTWts, predLinealTW) 

names(seguidoresTWLineal)<- c("Observado", "Pronostico")

ggseguidorTw <- data.frame(fecha = time(seguidoresTWLineal), 
                           data.frame(seguidoresTWLineal))


observadoEneroTWL <- seguidoresTWts["2017-01-01"] %>% 
  as.numeric 

observadoAgostoTWL <- seguidoresTWts["2017-08-01"] %>% 
  as.numeric

crecimiento1LTWObs <- (observadoAgostoTWL -observadoEneroTWL)/observadoEneroTWL*100

eneroLtw <- predLinealTW["2018-01-01"]  %>% 
  as.numeric %>% 
  floor

crecimiento1TW <- (eneroLtw - observadoEneroTWL)/observadoEneroTWL*100

junioLtw <- predLinealTW["2018-06-01"] %>% 
  as.numeric %>% 
  floor

crecimiento2TW <- (junioLtw -observadoEneroTWL)/observadoEneroTWL*100

diciembreLtw <- predLinealTW["2019-01-01"] %>% 
  as.numeric %>% 
  floor

crecimiento3TW <- (diciembreLtw -observadoEneroTWL)/observadoEneroTWL*100



ggseguidorTw %>% 
  gather(tipo, valor, -fecha) %>% 
  ggplot(aes(x = fecha, y=valor, color=tipo))+
  geom_line()+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "top")+
  scale_color_manual(values = c("darkred", "steelblue"),
                     name="", labels=c("Observado", "Estimación"))+
  scale_x_date(date_breaks = "2 months")+
  xlab("")+
  ylab("Seguidores TW")+
  geom_vline(xintercept = as.numeric(as.Date("2017-01-01")),
             linetype=4, color="darkred")+
  geom_vline(xintercept = as.numeric(as.Date("2017-08-01")),
             linetype=4, color="darkred")+
  geom_vline(xintercept = as.numeric(as.Date("2018-01-01")),
             linetype=4, color="steelblue")+
  geom_vline(xintercept = as.numeric(as.Date("2018-06-01")),
             linetype=4, color="steelblue")+
  geom_vline(xintercept = as.numeric(as.Date("2019-01-01")),
             linetype=4, color="steelblue")+
  geom_text(aes(x = as.Date('2016-10-01'), y= 3.2e5, 
                label= paste('Observado', "2017-01-01",
                             "\n",
                             format(observadoEneroTWL, big.mark = ","), 
                             sep=" ")),
            color="darkred")+
  geom_text(aes(x = as.Date('2017-05-01'), y= 3.2e5, 
                label= paste('Observado',  "2017-08-01",
                             "\n",
                             format(observadoAgostoTWL, big.mark = ","), 
                             sep=" ")), 
            # angle=90,
            color="darkred")+
  geom_text(aes(x = as.Date('2017-05-01'), y= 3.1e5, 
                label= paste( round(crecimiento1LTWObs, 2), "%", 
                              "crecimiento", 
                              "\n",
                              format((observadoAgostoTWL -observadoEneroTWL),
                                     big.mark = ","), 
                              "nuevos seguidores","*",
                              sep=" ")), 
            # angle=90,
            color="darkred")+
  geom_text(aes(x = as.Date('2017-10-15'), y= 3.05e5, 
                label= paste('Pronóstico', "\n", "2018-01-01",
                             "\n",
                             format(eneroLtw, big.mark = ","), 
                             sep=" ")), 
            color="steelblue")+
  geom_text(aes(x = as.Date('2017-10-15'), y= 2.95e5, 
                label= paste( round(crecimiento1L, 2), "%", 
                              "crecimiento", 
                              "\n",
                              format(floor(eneroLtw -observadoEneroTWL),
                                     big.mark = ","), "\n", 
                              "nuevos seguidores","*",
                              sep=" ")), 
            # angle=90,
            color="steelblue")+
  geom_text(aes(x = as.Date('2018-04-01'), y= 3.1e5, 
                label= paste('Pronóstico', "\n", "2018-06-01",
                             "\n",
                             format(junioLtw, big.mark = ","), 
                             sep=" ")), 
            color="steelblue")+
  geom_text(aes(x = as.Date('2018-03-25'), y= 3e5, 
                label= paste( round(crecimiento2L, 2), "%", 
                              "crecimiento", 
                              "\n",
                              format(floor(junioLtw - observadoEneroTWL),
                                     big.mark = ","), "\n", 
                              "nuevos seguidores","*",
                              sep=" ")), 
            # angle=90,
            color="steelblue") +
  geom_text(aes(x = as.Date('2018-09-01'), y= 3.2e5, 
                label= paste('Pronóstico', "\n", "2019-01-01",
                             "\n",
                             format(diciembreLtw, big.mark = ","), 
                             sep=" ")), 
            color="steelblue")+
  geom_text(aes(x = as.Date('2018-09-01'), y= 3.1e5, 
                label= paste( round(crecimiento3L, 2), "%", 
                              "crecimiento", 
                              "\n",
                              format(floor(diciembreLtw - observadoEneroTWL),
                                     big.mark = ","), "\n", 
                              "nuevos seguidores","*",
                              sep=" ")), 
            # angle=90,
            color="steelblue") +
  geom_text(aes(x = as.Date('2016-05-01'), y= 3.3e5,
                label= paste("*", 'Cifras con respecto al 2017-01-01',
                             sep=" ")),
            color="black", size=4)



gustaTWts <- xts(tw$MeGusta, as.Date(tw$Fecha)) 
RTTWts <- xts(tw$Retweets, as.Date(tw$Fecha))







# Pronósticos Instagram  -----------------------------------------------------
seguidoresIns <- xts(ins$Seguidores, as.Date(ins$Fecha)) 
seguidoresIns <- seguidoresIns['2015-01-01/']

fechasIns <- seq(ymd('2015-01-01'),ymd('2017-09-24'), by = 'days')
TiempoIns <- seq_along(seguidoresIns)

diferenciaInsSeguidor <- difftime(fechaFinal, last(fechasIns)) %>% 
  as.numeric

diferenciaInsSeguidor <- floor(diferenciaInsSeguidor)


TIME_InsSeg <- 1:(length(TiempoIns) + diferenciaInsSeguidor)


modeloLogisticoIns <- nls(coredata(seguidoresIns) ~ K*P0*exp(R*TiempoIns)/(K+P0 *(exp(R*TiempoIns)-1)),
                       start = list(P0 = min(coredata(seguidoresIns), 
                                             na.rm = T), 
                                    # K  = max(coredata(seguidoresIns),
                                    #          na.rm = T), 
                                    K  = 5e4, 
                                    R = 0.1
                       )
) 

predInsSeg <- predict(modeloLogisticoIns, 
                          newdata = list(TiempoIns = TIME_InsSeg))

predInsSeg <- xts(predInsSeg, seq(ymd(20150101), by="day", 
                                          length.out = length(predInsSeg)))


t1 <- 5000
modeloInstagramIniciales <- lm(car::logit(seguidoresIns/1000)~TiempoIns)
t2<- modeloInstagramIniciales$coefficients[1] %>%  as.numeric
t3<- modeloInstagramIniciales$coefficients[2] %>%  as.numeric

nlmIns <- nls(coredata(seguidoresIns)~theta1/(1+exp(-(theta2+theta3*TiempoIns))),
              start = list(
                theta1 = t1,
                theta2 = t2,
                theta3 = t3
              ),
              trace=T
) 

predInst <- predict(nlmIns, 
                        newdata = list(TiempoIns = TIME_InsSeg))

plot(seguidoresIns) %>% plot

xts(ins$Totales, as.Date(ins$Fecha)) %>% na.omit %>%  plot

# Pronósticos YouTube -----------------------------------------------------
crecimientoYT <- Reduce(yt$Subscriptores, f= sum, accumulate = T)
xts(crecimientoYT, as.Date(yt$Fecha))  %>%  plot
xts(crecimientoYT, as.Date(yt$Fecha))["2016-01-01/"]  %>%  plot
xts(yt$Vistas, as.Date(yt$Fecha))  %>%  plot




# Pronósticos sitio web ---------------------------------------------------
sitioWeb[sitioWeb==0] <-NA


######### VISITAS ##
visitasTs <- xts(sitioWeb$Visitas, as.Date(sitioWeb$Date))['2015-07-01/'] %>% 
  na.omit 

configuraTs <- xts(sitioWeb$CarConfiguration, as.Date(sitioWeb$Date))['2015-07-01/'] %>% 
  na.omit()



returnVisitsTs <- xts(sitioWeb$ReturnVisits, as.Date(sitioWeb$Date))['2015-07-01/'] %>% 
  na.omit()

timerateTs <- xts(sitioWeb$TimeRate, as.Date(sitioWeb$Date))['2015-07-01/'] %>% 
  na.omit()

#### Impronosticables

testTs <- xts(sitioWeb$TestDrive, as.Date(sitioWeb$Date))['2015-07-01/'] %>% 
  na.omit()

dealerTs <- xts(sitioWeb$DealerSearch, as.Date(sitioWeb$Date))['2015-07-01/'] %>% 
  na.omit()


testTs %>%  dygraph()



