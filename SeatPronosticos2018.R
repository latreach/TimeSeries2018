####################################
#Creado por Fernando Dorantes Nieto
#                                   <(°) 
#                                     ( >)"
#                                      /|
####################################


library(magrittr)
c("data.table", "dplyr", "tidyr", "lubridate", "tseries", "astsa",
  "forecast","ggplot2", "lattice", "dygraphs","XLConnect", "palettetown",
  "xts", "Rfacebook", "twitteR") %>%  
  sapply(require, character.only=T)



setwd("~/local/TimeSeries18/")


# Funciones ---------------------------------------------------------------

"%!in%" <- function(x,y)!("%in%"(x,y))

porcentajeLabel <- function(valor){
  valor <- paste0(valor, "%")
  return(valor)
}
# Datos -------------------------------------------------------------------
fb <- read.csv("datos/facebook/datosHomologadosFB.csv",
               header = T)

impressionsPost <- read.csv("datos/facebook/porcentajeImpressionsPost.csv",
                    header = T)

inbox <- read.csv("datos/facebook/mensajesHistoricoInbox.csv", header = T,
                  stringsAsFactors = F)

tw <- read.csv("datos/twitter/datosHomologadosTW.csv",
               header = T)

ins <- read.csv("datos/instagram/datosHomologadosIN.csv",
                header = T)

yt <- read.csv("datos/youtube/datosHomologadosYT.csv",
               header = T)


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

fechaFinal2 <- as.Date("2018-06-01")

idFB_seat = 113144262054871
 
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


png("imagenes/seguidoresFB.png", width = 1200, height = 800, res=100)
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

dev.off()


  

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

png("imagenes/engancheFB.png", width = 1200, height = 800, res=100)
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
dev.off()

png("imagenes/engancheFB15.png", width = 1200, height = 800, res=100)
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

dev.off()
############ ALCANCE #####################
alcanceOrg <- fb %>% 
  data.table() %>% 
  .[, alcancePorcentaje := Alcance.organico.Diario/Total.de.Me.gusta] %>% 
  .[, alcancePorcentaje := alcancePorcentaje*100] 

alcancets <- xts(alcanceOrg$alcancePorcentaje, as.Date(alcanceOrg$Fecha)) 

diferenciaAlcance <- difftime(fechaFinal2, time(last(alcancets)))
diferenciaAlcance <- floor(as.numeric(diferenciaAlcance)) +1


arimaAlcance <- alcancets %>%  
  coredata() %>%
  ts %>% 
  Arima(order= c(2,0,2)) %>% 
  forecast(h=diferenciaAlcance +1)


pronosticoAlcance <- xts(arimaAlcance$mean, 
                         seq.POSIXt(as.POSIXct(last(alcancets)), 
                                    length.out =  diferenciaAlcance+1, 
                                    by="day"))

alcanceTS <- cbind(alcancets, pronosticoAlcance) 
names(alcanceTS)<- c("Observado", "Pronostico")

ggalcanceTS <- data.frame(fecha = time(alcanceTS), data.frame(alcanceTS))

promedioAlcanceO <- mean(alcanceTS["2017-01-01/2017-09-24"], na.rm=T)
maxAlcanceO <- max(alcanceTS["2017-01-01/2017-09-24"], na.rm=T)
minAlcanceO<-min(alcanceTS["2017-01-01/2017-09-24"], na.rm=T)
promedioAlcanceP<-mean(pronosticoAlcance, na.rm=T)

png("imagenes/alcanceFB.png", width = 1200, height = 800, res=100)
ggalcanceTS %>% 
  gather(tipo, valor, -fecha) %>% 
  ggplot(aes(x = fecha, y= valor, color=tipo))+
  geom_line()+
  theme_classic()+
  theme(legend.position = "top",
        axis.text.x = element_text(angle=45, hjust=1))+
  scale_color_manual(values=c("steelblue", "darkred"),
                     labels=c("Observado", "Pronóstico"), name="")+
  scale_y_continuous(limits=c(0,35), breaks=seq(0,35,5), labels = porcentajeLabel)+
  scale_x_date(date_breaks = "2 months")+
  xlab("")+
  ylab("Alcance %")+
  geom_vline(xintercept = as.numeric(as.Date("2017-01-01")),
              linetype=4, color="darkgreen")+
  geom_vline(xintercept = as.numeric(as.Date("2017-09-24")),
              linetype=4, color="darkgreen")+
  geom_text(aes(x = as.Date('2017-05-01'), y= 35, 
                label= paste("2017-01-01/2017-09-24")),
            color="steelblue") +
 geom_text(aes(x = as.Date('2017-05-01'), y= 32, 
                label= paste(
                  "Máximo",
                  ceiling(maxAlcanceO), "%", "\n",
                  "Minimo", 
                  round(minAlcanceO, 2), "%", "\n",
                  "Promedio",
                  ceiling(promedioAlcanceO), "%",
                  sep=" "
                )),
            color="steelblue") +
  geom_text(aes(x = as.Date('2018-02-01'), y= 35, 
                label= paste("2017-10-01/2018-06-01")),
            color="darkred") +
  geom_text(aes(x = as.Date('2018-02-01'), y= 30, 
                label= paste(
                  "Pronóstico Promedio",
                  ceiling(promedioAlcanceP), "%",
                  sep=" "
                )),
            color="darkred")+
  geom_vline(xintercept = as.numeric(as.Date("2018-06-01")),
             linetype=4, color="darkred")+
  geom_text(aes(x = as.Date('2016-06-01'), y= 35, 
                label= paste(
                  "Alcance % =",
                  "(Alcance orgánico/Número total de seguidores)*100",
                  sep=" "
                )),
            color="black")

dev.off()

############ IMPRESSIONS  POST #####################

impressionsPost <- impressionsPost %>%
  select(-porcentajeImpresiones) %>% 
  data.table %>% 
  .[, Fecha := as.Date(Fecha)] %>% 
  .[, mes   := month(Fecha)] %>% 
  .[, anio  := year(Fecha)] %>%
  select(-Fecha) %>% 
  group_by(anio, mes) %>% 
  summarise_all(funs(sum)) %>% 
  data.table %>% 
  .[, Fecha := paste0(anio, "-",  mes, "-", "01")] %>% 
  .[, porcentajeImpresiones := interaccionesTotal/impressionsPost] %>% 
  .[, porcentajeImpresiones := porcentajeImpresiones*100] %>% 
  select(Fecha, interaccionesTotal, impressionsPost, porcentajeImpresiones) 

impressionsTS1 <- xts(log10(impressionsPost$porcentajeImpresiones), 
                     as.Date(impressionsPost$Fecha))
impressionsTS <- xts(impressionsPost$porcentajeImpresiones, 
                     as.Date(impressionsPost$Fecha))

impressionsTS  <- impressionsTS['2015-09-01/2017-08-01'] 
impressionsTS %>% plot


TiempoIm = seq_along(impressionsTS )

expFct <- function(x, beta1, beta2, beta3){exp(-beta1 * x)/(beta2 + beta3 * x)}

###### Modelo
nlsIm <- nls(coredata(impressionsTS)~expFct(x = TiempoIm ,beta1, beta2, beta3),
                start = list(beta1 = 0.1, beta2 = 0.02, beta3 = 0.03))

diffimpresiones <- difftime(fechaFinal2, time(last(impressionsTS)))
diffimpresiones <- as.numeric(diffimpresiones)/30
diffimpresiones <- floor(diffimpresiones)


TimePIm <- 1:(length(TiempoIm) + diffimpresiones)

predIm <- xts(predict(nlsIm, newdata = list(TiempoIm = TimePIm)),
               seq(ymd(20150901), by = "month", length.out = length(impressionsTS) + 
                     diffimpresiones))

imprPredict <- cbind(impressionsTS, predIm)

names(imprPredict) <- c("Observado","No lineal")

# arimaImpressions <- impressionsTS %>% 
#   ts %>% 
#   coredata() %>% 
#   auto.arima() %>% 
#   forecast(h=diffimpresiones) 


# pronosticoImpresiones <- xts(arimaImpressions$mean, 
#                          seq.POSIXt(as.POSIXct(last(impressionsTS)), 
#                                     length.out =  diffimpresiones,
#                                     by="month"))

# impressionsTime <- cbind(impressionsTS, pronosticoImpresiones) 
# names(impressionsTime)<- c("Observado", "Pronostico")
# impressionsTime %>% dygraph()

ggimprPredict <- data.frame(fecha = time(imprPredict), 
                                data.frame(imprPredict))


imprEne18 <- round(predIm["2018-01-01"], 2)
imprJun18 <- round(predIm["2018-06-01"], 2)


ggimprPredict %>%  
  gather(tipo, valor, -fecha) %>% 
  ggplot(aes(x = fecha, y= valor, color= tipo))+
  geom_line()+
  geom_point()+
  theme_classic()+
  scale_color_manual(values=c("steelblue", "darkred"), name="",
                     labels= c("Pronóstico", "Observado"))+
  scale_y_continuous(breaks = seq(0,4,.2), limit=c(1, 4), labels = porcentajeLabel)+
  scale_x_date(date_breaks = "2 months")+
  theme(legend.position = "top", axis.text.x =  element_text(angle=45, hjust=1))+
  xlab("" )+
  ylab("Porcentaje de:  \n interacciones totales/impresiones orgánicas")+
  geom_vline(xintercept = as.numeric(as.Date("2018-01-01")),
             linetype=4, color="steelblue")+
  geom_vline(xintercept = as.numeric(as.Date("2018-06-01")),
             linetype=4, color="steelblue")+
  geom_text(aes(x = as.Date('2017-12-01'), y= 2.5, 
                label= paste('Pronóstico', '2018-01-01',  
                             imprEne18, "%",  sep=" ")), angle=90) +
  geom_text(aes(x = as.Date('2018-05-01'), y= 2.5, 
                label= paste('Pronóstico', '2018-06-01',  
                             imprJun18, "%",  sep=" ")), angle=90) 
  
  

######### INBOX ###########################
inboxDiario <- inbox %>% 
  data.table %>% 
  .[, Fecha := as.Date(Fecha)] %>% 
  filter(id_Usuario_Desde != idFB_seat) %>% 
  group_by(Fecha) %>% 
  summarise(n = n()) %>% 
  data.frame


inboxMensual <- inbox %>% 
  data.table %>% 
  .[, Fecha := as.Date(Fecha)] %>% 
  .[, Mes := month(Fecha)] %>% 
  .[, Anio := year(Fecha)] %>% 
  filter(id_Usuario_Desde != idFB_seat) %>% 
  group_by(Anio, Mes) %>% 
  summarise(n = n()) %>%
  data.table %>% 
  .[, Fecha := paste0(Anio, "-", Mes, "-", "01")] %>% 
  .[, Fecha := as.Date(Fecha)] 

inboxts <- xts(inboxDiario$n, as.Date(inboxDiario$Fecha) )
inboxtsMes <- xts(inboxMensual$n, as.Date(inboxMensual$Fecha) )

inboxts["2016-01-01/2017-08-01"] %>%  plot
x11()
inboxts %>%  plot
inboxtsMes["/2017-08-01"] %>%  plot

######### RESPONSE TIME Y RESPONSE RATE ##############
AnswersSeat <- inbox %>% 
  filter(id_Usuario_Desde!=idFB_seat) %>% 
  select(idConversacion, Fecha, Hora, Mensaje) %>% 
  data.table() %>% 
  .[order(idConversacion, Fecha, Hora)] %>%
  group_by( idConversacion) %>% 
  # group_by( idConversacion, Fecha, Hora) %>% 
  filter(row_number()==1) %>% 
  data.frame 

ResponseSeat <- inbox %>% 
  filter(id_Usuario_Desde==idFB_seat) %>% 
  select(idConversacion, Fecha, Hora, Mensaje) %>% 
  data.table() %>% 
  .[order(idConversacion, Fecha, Hora)] %>%
  group_by( idConversacion) %>% 
  # group_by( idConversacion, Fecha, Hora) %>% 
  filter(row_number()==1) %>% 
  data.frame 

AnswersSeat %>% 
  filter(idConversacion=="t_100002244135489")

test <-AnswersSeat %>% 
  left_join(ResponseSeat, by="idConversacion") %>% 
  data.table %>% 
  .[, FechaPregunta := paste(Fecha.x, Hora.x, sep=" ")] %>%
  .[, FechaPregunta := gsub("+0000", "", FechaPregunta, fixed=T)] %>% 
  .[, FechaPregunta := as.POSIXct(FechaPregunta,  format="%Y-%m-%d %H:%M:%S")] %>% 
  .[, FechaRespuesta := paste(Fecha.y, Hora.y, sep=" ")] %>% 
  .[, FechaRespuesta := gsub("+0000", "", FechaRespuesta, fixed=T)] %>% 
  .[, FechaRespuesta := as.POSIXct(FechaRespuesta, format="%Y-%m-%d %H:%M:%S")] %>% 
  .[, diferencia := difftime(FechaRespuesta, FechaPregunta, units="hours")] %>% 
  .[, diferencia := as.numeric(diferencia)] %>% 
  group_by(Fecha.x ) %>% 
  summarise(tiempoPromedio  = mean(diferencia)) %>%
  na.omit %>% 
  data.frame %>% 
  filter(tiempoPromedio==abs(tiempoPromedio))


AnswersSeat %>% 
  left_join(ResponseSeat, by="idConversacion") %>% 
  data.table %>% 
  .[, FechaPregunta := paste(Fecha.x, Hora.x, sep=" ")] %>%
  .[, FechaPregunta := as.POSIXct(FechaPregunta)] %>% 
  .[, FechaRespuesta := paste(Fecha.y, Hora.y, sep=" ")] %>% 
  .[, FechaRespuesta := gsub("+0000", "", FechaRespuesta, fixed=T)] %>% 
  .[, FechaRespuesta := as.POSIXct(FechaRespuesta, format="%Y-%m-%d %H:%M:%S")] %>% 
  .[, diferencia := difftime(FechaRespuesta, FechaPregunta, units="hours")] %>% 
  .[, diferencia := as.numeric(diferencia)] %>% 
  group_by(Fecha.x ) %>% 
  summarise(tiempoPromedio  = sum(diferencia)) %>% 
  filter(Fecha.x> "2015-09-01") %>% 
  filter(Fecha.x< "2015-11-01")  %>% 
  na.omit %>%  data.frame()

inbox %>% 
  filter(idConversacion=="t_100002244135489")

inbox %>% 
  filter(idConversacion=="t_1166642808")


AnswersSeat %>% 
  left_join(ResponseSeat, by="idConversacion") %>% 
  data.table %>% 
  .[, FechaPregunta := paste(Fecha.x, Hora.x, sep=" ")] %>%
  .[, FechaPregunta := gsub("+0000", "", FechaPregunta, fixed=T)] %>% 
  .[, FechaPregunta := as.POSIXct(FechaPregunta,  format="%Y-%m-%d %H:%M:%S")] %>% 
  .[, FechaRespuesta := paste(Fecha.y, Hora.y, sep=" ")] %>% 
  .[, FechaRespuesta := gsub("+0000", "", FechaRespuesta, fixed=T)] %>% 
  .[, FechaRespuesta := as.POSIXct(FechaRespuesta, format="%Y-%m-%d %H:%M:%S")] %>% 
  .[, diferencia := difftime(FechaRespuesta, FechaPregunta, units="hours")] %>% 
  .[, diferencia := as.numeric(diferencia)] %>% 
  filter(Fecha.x=="2015-10-21")

xts(test$tiempoPromedio, as.Date(test$Fecha.x))["2017-08-01/"] %>% mean

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


png("imagenes/seguidoresTW.png", width = 1200, height = 800, res=100)
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

dev.off()
############ ME GUSTA TWITTER #####################
gustaTWts <- xts(tw$MeGusta, as.Date(tw$Fecha)) 

diferenciaGustaTW <- difftime(fechaFinal2, time(last(gustaTWts)))
diferenciaGustaTW <- floor(as.numeric(diferenciaGustaTW)) +1


arimaGustatw <- gustaTWts %>%  
  coredata() %>%
  ts %>% 
  Arima(order= c(2,0,2), include.drift = T) %>% 
  forecast(h=diferenciaGustaTW +2) 


pronosticoGustatw <- xts(arimaGustatw$mean, 
                         seq.POSIXt(as.POSIXct(last(gustaTWts)), 
                                    length.out =  diferenciaGustaTW+2, by="day"))

gustatwTS <- cbind(gustaTWts, pronosticoGustatw) 
names(gustatwTS)<- c("Observado", "Pronostico")

gggustaTS <- data.frame(fecha = time(gustatwTS), data.frame(gustatwTS ))

promedioGustatwO <- mean(gustatwTS["2017-01-01/2017-09-24"], na.rm=T)
maxGustatwO <- max(gustatwTS["2017-01-01/2017-09-24"], na.rm=T)
minGustatwO <- min(gustatwTS["2017-01-01/2017-09-24"], na.rm=T)
promedioGustatwP <-mean(gustatwTS, na.rm=T)
gustaTw18 <- gustatwTS["2018-06-01"] %>% 
  as.numeric %>% na.omit %>%  ceiling()

png("imagenes/meGustaTW.png", width = 1200, height = 800, res=100)
gggustaTS %>% 
  gather(tipo, valor, -fecha) %>% 
  ggplot(aes(x = fecha, y= valor, color=tipo))+
  geom_line()+
  theme_classic()+
  theme(legend.position = "top",
        axis.text.x = element_text(angle=45, hjust=1))+
  scale_color_manual(values=c("steelblue", "darkred"),
                     labels=c("Observado", "Pronóstico"), name="")+
  scale_x_date(date_breaks = "2 months")+
  xlab("")+
  ylab("Me gusta Twitter")+
  geom_vline(xintercept = as.numeric(as.Date("2017-01-01")),
             linetype=4, color="darkgreen")+
  geom_vline(xintercept = as.numeric(as.Date("2017-09-24")),
             linetype=4, color="darkgreen")+
  geom_text(aes(x = as.Date('2017-05-01'), y= 800, 
                label= paste("2017-01-01/2017-09-24")),
            color="steelblue") +
  geom_text(aes(x = as.Date('2017-05-01'), y= 750, 
                label= paste(
                  "Máximo",
                  ceiling(maxGustatwO), "\n",
                  "Minimo", 
                  round(minGustatwO, 2),  "\n",
                  "Promedio",
                  ceiling(promedioGustatwO), 
                  sep=" "
                )),
            color="steelblue") +
  geom_text(aes(x = as.Date('2018-02-01'), y= 800, 
                label= paste("2017-10-01/2018-06-01")),
            color="darkred") +
  geom_text(aes(x = as.Date('2018-02-01'), y= 750, 
                label= paste(
                  "Pronóstico Promedio",
                  ceiling(promedioGustatwP), "\n",
                  "2018-06-01 ->", gustaTw18,
                  sep=" "
                )),
            color="darkred")+
  geom_vline(xintercept = as.numeric(as.Date("2018-06-01")),
             linetype=4, color="darkred")

dev.off()

############ RETWEETS #####################
RTTWts <- xts(tw$Retweets, as.Date(tw$Fecha))

diferenciaRTTW <- difftime(fechaFinal2, time(last(RTTWts)))
diferenciaRTTW <- floor(as.numeric(diferenciaRTTW)) +1

arimaRTtw <-RTTWts%>%  
  coredata() %>%
  ts %>%
  Arima(order= c(0,1,1), include.drift = T) %>% 
  forecast(h= diferenciaRTTW + 1) 

pronosticoRTtw <- xts(arimaRTtw$mean, 
                         seq.POSIXt(as.POSIXct(last(RTTWts)), 
                                    length.out =  diferenciaRTTW+1, by="day"))

RTTWTS <- cbind(RTTWts, pronosticoRTtw) 
names(RTTWTS)<- c("Observado", "Pronostico")

ggRTTWTS <- data.frame(fecha = time(RTTWTS), data.frame(RTTWTS))

promedioRTtwO <- mean(RTTWTS["2017-01-01/2017-09-24"], na.rm=T)
maxRTtwO <- max(RTTWTS["2017-01-01/2017-09-24"], na.rm=T)
minRTtwO <- min(RTTWTS["2017-01-01/2017-09-24"], na.rm=T)
promedioRTtwP <-mean(RTTWTS, na.rm=T)
RTTw18 <- RTTWTS["2018-06-01"] %>% 
  as.numeric %>% na.omit %>%  ceiling()

png("imagenes/retweetsTW.png", width = 1200, height = 800, res=100)
ggRTTWTS %>% 
  gather(tipo, valor, -fecha) %>% 
  ggplot(aes(x = fecha, y= valor, color=tipo))+
  geom_line()+
  theme_classic()+
  theme(legend.position = "top",
        axis.text.x = element_text(angle=45, hjust=1))+
  scale_color_manual(values=c("steelblue", "darkred"),
                     labels=c("Observado", "Pronóstico"), name="")+
  scale_x_date(date_breaks = "2 months")+
  xlab("")+
  ylab("Retweets")+
  geom_vline(xintercept = as.numeric(as.Date("2017-01-01")),
             linetype=4, color="darkgreen")+
  geom_vline(xintercept = as.numeric(as.Date("2017-09-24")),
             linetype=4, color="darkgreen")+
  geom_text(aes(x = as.Date('2017-05-01'), y= 400, 
                label= paste("2017-01-01/2017-09-24")),
            color="steelblue") +
  geom_text(aes(x = as.Date('2017-05-01'), y= 350, 
                label= paste(
                  "Máximo",
                  ceiling(maxRTtwO), "\n",
                  "Minimo", 
                  round(minRTtwO, 2),  "\n",
                  "Promedio",
                  ceiling(promedioRTtwO), 
                  sep=" "
                )),
            color="steelblue") +
  geom_text(aes(x = as.Date('2018-02-01'), y= 400, 
                label= paste("2017-10-01/2018-06-01")),
            color="darkred") +
  geom_text(aes(x = as.Date('2018-02-01'), y= 350, 
                label= paste(
                  "Pronóstico Promedio",
                  ceiling(promedioRTtwP), "\n",
                  "2018-06-01 ->", RTTw18,
                  sep=" "
                )),
            color="darkred")+
  geom_vline(xintercept = as.numeric(as.Date("2018-06-01")),
             linetype=4, color="darkred")

dev.off()

########### MENTIONS ##
Mentionsts <- xts(tw$Menciones, as.Date(tw$Fecha))
diferenciaMentions <- difftime(fechaFinal2, time(last(Mentionsts)))
diferenciaMentions <- floor(as.numeric(diferenciaMentions)) +1

arimaMentions <- Mentionsts%>%  
  coredata() %>%
  ts %>%
  Arima(order = c(2, 1, 2)) %>% 
  forecast(h= diferenciaMentions + 1) 

pronosticoMentions <- xts(arimaMentions$mean, 
                      seq.POSIXt(as.POSIXct(last(Mentionsts)), 
                                 length.out =  diferenciaMentions+1, by="day"))

mentionsTS <- cbind(Mentionsts, pronosticoMentions) 
names(mentionsTS)<- c("Observado", "Pronostico")

ggmentionsTS <- data.frame(fecha = time(mentionsTS), data.frame(mentionsTS))

promedioMentionsO <- mean(Mentionsts["2017-01-01/2017-09-24"], na.rm=T)
maxMentionsO <- max(Mentionsts["2017-01-01/2017-09-24"], na.rm=T)
minMentionsO <- min(Mentionsts["2017-01-01/2017-09-24"], na.rm=T)
promedioMentionsP <-mean(mentionsTS[,2], na.rm=T)
Mentions18 <- mentionsTS["2018-06-01"] %>% 
  as.numeric %>% na.omit %>%  ceiling()

png("imagenes/mentions.png", width = 1200, height = 800, res=100)
ggmentionsTS %>% 
  gather(tipo, valor, -fecha) %>% 
  ggplot(aes(x = fecha, y= valor, color=tipo))+
  geom_line()+
  theme_classic()+
  theme(legend.position = "top",
        axis.text.x = element_text(angle=45, hjust=1))+
  scale_color_manual(values=c("steelblue", "darkred"),
                     labels=c("Observado", "Pronóstico"), name="")+
  scale_x_date(date_breaks = "2 months")+
  xlab("")+
  ylab("Menciones")+
  geom_vline(xintercept = as.numeric(as.Date("2017-01-01")),
             linetype=4, color="darkgreen")+
  geom_vline(xintercept = as.numeric(as.Date("2017-09-24")),
             linetype=4, color="darkgreen")+
  geom_text(aes(x = as.Date('2017-05-01'), y= 500, 
                label= paste("2017-01-01/2017-09-24")),
            color="steelblue") +
  geom_text(aes(x = as.Date('2017-05-01'), y= 450, 
                label= paste(
                  "Máximo",
                  ceiling(maxMentionsO), "\n",
                  "Minimo", 
                  round(minMentionsO, 2),  "\n",
                  "Promedio",
                  ceiling(promedioMentionsO), 
                  sep=" "
                )),
            color="steelblue") +
  geom_text(aes(x = as.Date('2018-02-01'), y= 500, 
                label= paste("2017-10-01/2018-06-01")),
            color="darkred") +
  geom_text(aes(x = as.Date('2018-02-01'), y= 450, 
                label= paste(
                  "Pronóstico Promedio",
                  ceiling(promedioMentionsP), "\n",
                  # "2018-06-01 ->", Mentions18,
                  sep=" "
                )),
            color="darkred")+
  geom_vline(xintercept = as.numeric(as.Date("2018-06-01")),
             linetype=4, color="darkred")


dev.off()

# Pronósticos Instagram  -----------------------------------------------------
# Modelo exponencial
seguidoresIns <- xts(ins$Seguidores, as.Date(ins$Fecha)) 
fechasIns <- seq(ymd('2014-01-01'),ymd('2017-09-24'), by = 'days')
TiempoIns <- seq_along(seguidoresIns)

diferenciaInsSeguidor <- difftime(fechaFinal, last(fechasIns)) %>% 
  as.numeric

diferenciaInsSeguidor <- floor(diferenciaInsSeguidor)


TIME_InsSeg <- 1:(length(TiempoIns) + diferenciaInsSeguidor)


Model_expIns = nls(coredata(seguidoresIns) ~ P0 * exp(R * TiempoIns), 
          start = list(P0 = 1e3, R = 0.005))

predInsSeg <- predict(Model_expIns, 
                      newdata = list(TiempoIns = TIME_InsSeg))

predInsSeg <- xts(predInsSeg, seq(ymd(20140101), by="day", 
                                  length.out = length(predInsSeg)))





##Modelo logistico
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


### Modelo lineal
seguidoresIns <- seguidoresIns['2016-06-01/']

fechasIns <- seq(ymd('2016-06-01'), ymd('2017-09-24'), by = 'days')
TiempoIns <- seq_along(seguidoresIns)

diferenciaInsSeguidor <- difftime(fechaFinal, last(fechasIns)) %>% 
  as.numeric

diferenciaInsSeguidor <- floor(diferenciaInsSeguidor)

TIME_InsSeg <- 1:(length(TiempoIns) + diferenciaInsSeguidor)

modeloLinealIns <- lm(coredata(seguidoresIns)~TiempoIns)

predInsSeg <- predict(modeloLinealIns, 
                      newdata = list(TiempoIns= TIME_InsSeg))

predInsSeg <- xts(predInsSeg, seq(ymd(20160601), by="day", 
                                  length.out = length(predInsSeg)))

seguidoresInsLineal <- cbind(seguidoresIns, predInsSeg) 
names(seguidoresInsLineal)<- c("Observado", "Pronostico")

ggseguidoresInsLineal <- data.frame(fecha = time(seguidoresInsLineal), 
                                    data.frame(seguidoresInsLineal))

observadoEneroIns <- seguidoresIns["2017-01-01"] %>% 
  as.numeric 

observadoAgoIns <- seguidoresIns["2017-08-01"] %>% 
  as.numeric 

crecimiento1InsObs <- (observadoAgoIns -observadoEneroIns)/observadoEneroIns*100

eneroIns <- predInsSeg["2018-01-01"]  %>% 
  as.numeric %>% 
  floor

crecimiento1Ins <- (eneroIns - observadoEneroIns)/observadoEneroIns*100

junioIns <- predInsSeg["2018-06-01"] %>% 
  as.numeric %>% 
  floor

crecimiento2Ins <- (junioIns -observadoEneroIns)/observadoEneroIns*100

diciembreIns <- predInsSeg["2019-01-01"] %>% 
  as.numeric %>% 
  floor

crecimiento3Ins <- (diciembreIns -observadoEneroIns)/observadoEneroIns*100

png("imagenes/seguidoresInstagram.png", width = 1200, height = 800, res=100)
ggseguidoresInsLineal %>% 
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
  ylab("Seguidores")+
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
  geom_text(aes(x = as.Date('2016-10-01'), y= 6e4, 
                label= paste('Observado', "2017-01-01",
                             "\n",
                             format(observadoEneroIns, big.mark = ","), 
                             sep=" ")),
            color="darkred")+
  geom_text(aes(x = as.Date('2017-05-01'), y= 6e4, 
                label= paste('Observado',  "2017-08-01",
                             "\n",
                             format(observadoAgoIns, big.mark = ","), 
                             sep=" ")), 
            # angle=90,
            color="darkred")+
  geom_text(aes(x = as.Date('2017-05-01'), y= 5.7e4, 
                label= paste( round(crecimiento1InsObs, 2), "%", 
                              "crecimiento", 
                              "\n",
                              format((observadoAgoIns -observadoEneroIns),
                                     big.mark = ","), 
                              "nuevos seguidores","*",
                              sep=" ")), 
            # angle=90,
            color="darkred")+
  geom_text(aes(x = as.Date('2017-10-15'), y= 3.6e4, 
                label= paste('Pronóstico', "\n", "2018-01-01",
                             "\n",
                             format(eneroIns, big.mark = ","), 
                             sep=" ")), 
            color="steelblue")+
  geom_text(aes(x = as.Date('2017-10-15'), y= 3.2e4, 
                label= paste( round(crecimiento1Ins, 2), "%", 
                              "crecimiento", 
                              "\n",
                              format(floor(eneroIns -observadoEneroIns),
                                     big.mark = ","), "\n", 
                              "nuevos seguidores","*",
                              sep=" ")), 
            # angle=90,
            color="steelblue")+
  geom_text(aes(x = as.Date('2018-04-01'), y= 4.4e4, 
                label= paste('Pronóstico', "\n", "2018-06-01",
                             "\n",
                             format(junioIns, big.mark = ","), 
                             sep=" ")), 
            color="steelblue")+
  geom_text(aes(x = as.Date('2018-03-25'), y= 4e4, 
                label= paste( round(crecimiento2Ins, 2), "%", 
                              "crecimiento", 
                              "\n",
                              format(floor(junioIns - observadoEneroIns),
                                     big.mark = ","), "\n", 
                              "nuevos seguidores","*",
                              sep=" ")), 
            # angle=90,
            color="steelblue") +
  geom_text(aes(x = as.Date('2018-09-01'), y= 5.4e4, 
                label= paste('Pronóstico', "\n", "2019-01-01",
                             "\n",
                             format(diciembreIns, big.mark = ","), 
                             sep=" ")), 
            color="steelblue")+
  geom_text(aes(x = as.Date('2018-09-01'), y= 5e4, 
                label= paste( round(crecimiento3Ins, 2), "%", 
                              "crecimiento", 
                              "\n",
                              format(floor(diciembreIns - observadoEneroIns),
                                     big.mark = ","), "\n", 
                              "nuevos seguidores","*",
                              sep=" ")), 
            # angle=90,
            color="steelblue") +
  geom_text(aes(x = as.Date('2016-09-01'), y= 6.4e4,
                label= paste("*", 'Cifras con respecto al 2017-01-01',
                             sep=" ")),
            color="black", size=4)

dev.off()





### profeta
library(prophet)

seguidoresIns <- xts(ins$Seguidores, as.Date(ins$Fecha)) 
fechasIns <- seq(ymd('2014-01-01'),ymd('2017-09-24'), by = 'days')
TiempoIns <- seq_along(seguidoresIns)

diferenciaInsSeguidor <- difftime(fechaFinal, last(fechasIns)) %>% 
  as.numeric

diferenciaInsSeguidor <- floor(diferenciaInsSeguidor)


TIME_InsSeg <- 1:(length(TiempoIns) + diferenciaInsSeguidor)

testIns <- ins %>% 
  select(Fecha, Seguidores) %>% 
  rename(y = Seguidores) %>% 
  rename(ds = Fecha) %>% 
  data.table %>% 
  .[, ds:= as.Date(ds)] %>% 
  data.frame

X <- prophet(testIns)  

futuroIns <- make_future_dataframe(X, periods = diferenciaInsSeguidor)

predictFuturoIns<-predict(X, futuroIns)
max(predictFuturoIns$trend)

plot(X, predictFuturoIns)


##### INTERACCIONES INSTAGRAM ##
InteraccionIns <- xts(ins$Totales, as.Date(ins$Fecha)) %>% 
  na.omit
InteraccionIns <- InteraccionIns["2016-01-01/"]

diferenciaINTins <- difftime(fechaFinal2, time(last(InteraccionIns)))
diferenciaINTins <- floor(as.numeric(diferenciaINTins)) +1

 arimaIntins <-InteraccionIns %>%  
  coredata() %>%
  ts %>%
  Arima(order= c(0,1,2)) %>% 
  forecast(h=diferenciaINTins) 


pronosticoIntins <- xts( arimaIntins$mean, 
                      seq.POSIXt(as.POSIXct(last(InteraccionIns)), 
                                 length.out =  diferenciaINTins, by="day"))


tsInteraccionIns <- cbind(InteraccionIns, pronosticoIntins) 
names(tsInteraccionIns)<- c("Observado", "Pronostico")

ggInteraccionIns <- data.frame(fecha = time(tsInteraccionIns), 
                               data.frame(tsInteraccionIns))

promedioInsO <- mean(tsInteraccionIns["2017-01-01/2017-09-24"], na.rm=T)
maxInsO <- max(tsInteraccionIns["2017-01-01/2017-09-24"], na.rm=T)
minInsO <- min(tsInteraccionIns["2017-01-01/2017-09-24"], na.rm=T)
promedioInsP <-mean(tsInteraccionIns, na.rm=T)
Ins18 <- tsInteraccionIns["2018-06-01"] %>% 
  as.numeric %>% na.omit %>%  ceiling()


png("imagenes/interaccionesInstagram.png", width = 1200, height = 800, res=100)
ggInteraccionIns %>% 
  gather(tipo, valor, -fecha) %>% 
  ggplot(aes(x = fecha, y= valor, color=tipo))+
  geom_line()+
  theme_classic()+
  theme(legend.position = "top",
        axis.text.x = element_text(angle=45, hjust=1))+
  scale_color_manual(values=c("steelblue", "darkred"),
                     labels=c("Observado", "Pronóstico"), name="")+
  scale_x_date(date_breaks = "2 months")+
  xlab("")+
  ylab("Interacciones \n Instagram")+
  geom_vline(xintercept = as.numeric(as.Date("2017-01-01")),
             linetype=4, color="darkgreen")+
  geom_vline(xintercept = as.numeric(as.Date("2017-09-24")),
             linetype=4, color="darkgreen")+
  geom_text(aes(x = as.Date('2017-05-01'), y= 3000, 
                label= paste("2017-01-01/2017-09-24")),
            color="steelblue") +
  geom_text(aes(x = as.Date('2017-05-01'), y= 2800, 
                label= paste(
                  "Máximo",
                  ceiling(maxInsO), "\n",
                  "Minimo", 
                  round(minInsO, 2),  "\n",
                  "Promedio",
                  ceiling(promedioInsO), 
                  sep=" "
                )),
            color="steelblue") +
  geom_text(aes(x = as.Date('2018-02-01'), y= 3000, 
                label= paste("2017-10-01/2018-06-01")),
            color="darkred") +
  geom_text(aes(x = as.Date('2018-02-01'), y= 2800, 
                label= paste(
                  "Pronóstico Promedio" ,
                  ceiling(promedioInsP), #"\n",
                  # "2018-06-01 ->", Ins18,
                  sep=" "
                )),
            color="darkred")+
  geom_vline(xintercept = as.numeric(as.Date("2018-06-01")),
             linetype=4, color="darkred")

dev.off()

  
# Pronósticos YouTube -----------------------------------------------------
crecimientoYT <- Reduce(yt$Subscriptores, f= sum, accumulate = T)
crecimientoYT <- xts(crecimientoYT, as.Date(yt$Fecha)) 
yt$Crecimiento <- coredata(crecimientoYT)

png("imagenes/crecimientoYT.png", width = 1200, height = 800, res=100)
plot(crecimientoYT, type = "n", main="No pronosticable")
lines(crecimientoYT, col="steelblue")
dev.off()

png("imagenes/vistasYT.png", width = 1200, height = 800, res=100)
vistasYT <- xts(yt$Vistas, as.Date(yt$Fecha))
plot(vistasYT, type = "n", main="No pronosticable")
lines(vistasYT, col="steelblue")
dev.off()





##### DEMASIADO EFECTO DE PAUTA
########### NO ES POSIBLE PRONOSTICAR YOUTUBE



# Pronósticos sitio web ---------------------------------------------------
sitioWeb[sitioWeb==0] <-NA
sitioWeb <- sitioWeb %>% 
  data.table %>% 
  .[, Date := as.Date(Date)] %>% 
  filter(Date>"2015-06-30") 

diferenciaWeb <- difftime(fechaFinal2, as.Date(sitioWeb[length(sitioWeb$Date),1]))
diferenciaWeb <- floor(as.numeric(diferenciaWeb)) +2


######### VISITAS ##
visitasTs <- xts(sitioWeb$Visitas, as.Date(sitioWeb$Date)) %>% 
  na.omit 

arimaVisitas <- visitasTs %>% 
  coredata %>% 
  ts %>% 
  Arima(order= c(1,0,2)) %>% 
  forecast(h= diferenciaWeb)

pronosticoVisitas <- xts( arimaVisitas$mean, 
                         seq.POSIXt(as.POSIXct(last(visitasTs)), 
                                    length.out =  diferenciaWeb, by="day"))


tsVisitas <- cbind(visitasTs, pronosticoVisitas) 
names(tsVisitas)<- c("Observado", "Pronostico")

ggtsVisitas <- data.frame(fecha = time(tsVisitas), 
                               data.frame(tsVisitas))

promedioVisitasO <- mean(tsVisitas["2017-01-01/2017-09-24"], na.rm=T)
maxVisitas <- max(tsVisitas["2017-01-01/2017-09-24"], na.rm=T)
minVisitas <- min(tsVisitas["2017-01-01/2017-09-24"], na.rm=T)
promedioVisitas <-mean(pronosticoVisitas, na.rm=T)
Visitas18 <- pronosticoVisitas["2018-06-01"] %>% 
  as.numeric %>% na.omit %>%  ceiling()

png("imagenes/visitasWeb.png", width = 1200, height = 800, res=100)
ggtsVisitas %>% 
  gather(tipo, valor, -fecha) %>% 
  ggplot(aes(x = fecha, y= valor, color=tipo))+
  geom_line()+
  theme_classic()+
  theme(legend.position = "top",
        axis.text.x = element_text(angle=45, hjust=1))+
  scale_color_manual(values=c("steelblue", "darkred"),
                     labels=c("Observado", "Pronóstico"), name="")+
  scale_x_date(date_breaks = "2 months")+
  xlab("")+
  ylab("Visitas \n Sitio Web")+
  geom_vline(xintercept = as.numeric(as.Date("2017-01-01")),
             linetype=4, color="darkgreen")+
  geom_vline(xintercept = as.numeric(as.Date("2017-09-24")),
             linetype=4, color="darkgreen")+
  geom_text(aes(x = as.Date('2017-05-01'), y= 6.5e4, 
                label= paste("2017-01-01/2017-09-24")),
            color="steelblue") +
  geom_text(aes(x = as.Date('2017-05-01'), y= 6e4, 
                label= paste(
                  "Máximo",
                  format(ceiling(maxVisitas), big.mark = ","), "\n",
                  "Minimo", 
                  format(round(minVisitas, 2), big.mark = ","),  "\n",
                  "Promedio",
                  format(ceiling(promedioVisitasO), big.mark = ","), 
                  sep=" "
                )),
            color="steelblue") +
  geom_text(aes(x = as.Date('2018-02-01'), y=  6.5e4, 
                label= paste("2017-10-01/2018-06-01")),
            color="darkred") +
  geom_text(aes(x = as.Date('2018-02-01'), y=  6e4, 
                label= paste(
                  "Pronóstico Promedio" ,
                  format(ceiling(promedioVisitas), big.mark = ","), #"\n",
                  # "2018-06-01 ->", Ins18,
                  sep=" "
                )),
            color="darkred")+
  geom_vline(xintercept = as.numeric(as.Date("2018-06-01")),
             linetype=4, color="darkred")

dev.off()





######### CAR CONFIGURATION ##
configuraTs <- xts(sitioWeb$CarConfiguration, as.Date(sitioWeb$Date)) %>% 
  na.omit()

arimaConfigura <- configuraTs %>% 
  coredata %>% 
  ts %>% 
  Arima(order=c(2,1,1)) %>% 
  forecast(h= diferenciaWeb) 

pronosticoConfigura <- xts( arimaConfigura$mean, 
                          seq.POSIXt(as.POSIXct(last(configuraTs)), 
                                     length.out =  diferenciaWeb, by="day"))


tsConfigura <- cbind(configuraTs, pronosticoConfigura) 
names(tsConfigura)<- c("Observado", "Pronostico")

ggtsConfigura <- data.frame(fecha = time(tsConfigura), 
                          data.frame(tsConfigura))

promedioConfiguraO <- mean(tsConfigura["2017-01-01/2017-09-24"], na.rm=T)
maxConfigura <- max(tsConfigura["2017-01-01/2017-09-24"], na.rm=T)
minConfigura <- min(tsConfigura["2017-01-01/2017-09-24"], na.rm=T)
promedioConfigura <-mean(pronosticoConfigura, na.rm=T)
Configura18 <- pronosticoConfigura["2018-06-01"] %>% 
  as.numeric %>% na.omit %>%  ceiling()

png("imagenes/ConfiguracionesWeb.png", width = 1200, height = 800, res=100)
ggtsConfigura %>% 
  gather(tipo, valor, -fecha) %>% 
  ggplot(aes(x = fecha, y= valor, color=tipo))+
  geom_line()+
  theme_classic()+
  theme(legend.position = "top",
        axis.text.x = element_text(angle=45, hjust=1))+
  scale_color_manual(values=c("steelblue", "darkred"),
                     labels=c("Observado", "Pronóstico"), name="")+
  scale_x_date(date_breaks = "2 months")+
  xlab("")+
  ylab("Configuraciones de automóviles")+
  geom_vline(xintercept = as.numeric(as.Date("2017-01-01")),
             linetype=4, color="darkgreen")+
  geom_vline(xintercept = as.numeric(as.Date("2017-09-24")),
             linetype=4, color="darkgreen")+
  geom_text(aes(x = as.Date('2017-06-01'), y= 6500, 
                label= paste("2017-01-01/2017-09-24")),
            color="steelblue") +
  geom_text(aes(x = as.Date('2017-06-01'), y= 6000, 
                label= paste(
                  "Máximo",
                  format(ceiling(maxConfigura), big.mark = ","), "\n",
                  "Minimo", 
                  format(round(minConfigura, 2), big.mark = ","),  "\n",
                  "Promedio",
                  format(ceiling(promedioConfiguraO), big.mark = ","), 
                  sep=" "
                )),
            color="steelblue") +
  geom_text(aes(x = as.Date('2018-02-01'), y=  6500, 
                label= paste("2017-10-01/2018-06-01")),
            color="darkred") +
  geom_text(aes(x = as.Date('2018-02-01'), y=  6000, 
                label= paste(
                  "Pronóstico Promedio" ,
                  format(ceiling(promedioConfigura), big.mark = ","), #"\n",
                  # "2018-06-01 ->", Ins18,
                  sep=" "
                )),
            color="darkred")+
  geom_vline(xintercept = as.numeric(as.Date("2018-06-01")),
             linetype=4, color="darkred")

dev.off()


######### RETURN VISITS ##
returnVisitsTs <- xts(sitioWeb$ReturnVisits, as.Date(sitioWeb$Date)) %>% 
  na.omit()

arimaReturn <- returnVisitsTs %>% 
  coredata %>% 
  ts %>% 
  Arima(order = c(2,1,2)) %>% 
  forecast(h= diferenciaWeb) 


pronosticoReturn<- xts(arimaReturn$mean, 
                            seq.POSIXt(as.POSIXct(last(returnVisitsTs)), 
                                       length.out =  diferenciaWeb, by="day"))


tsReturn <- cbind(returnVisitsTs, pronosticoReturn) 
names(tsReturn)<- c("Observado", "Pronostico")

ggtsReturn <- data.frame(fecha = time(tsReturn), 
                            data.frame(tsReturn))

promedioReturnO <- mean(tsReturn["2017-01-01/2017-09-24"], na.rm=T)
maxReturn <- max(tsReturn["2017-01-01/2017-09-24"], na.rm=T)
minReturn <- min(tsReturn["2017-01-01/2017-09-24"], na.rm=T)
promedioReturn <-mean(pronosticoReturn, na.rm=T)
Return18 <- pronosticoReturn["2018-06-01"] %>% 
  as.numeric %>% na.omit %>%  ceiling()

png("imagenes/returnvisitsWeb.png", width = 1200, height = 800, res=100)
ggtsReturn %>% 
  gather(tipo, valor, -fecha) %>% 
  ggplot(aes(x = fecha, y= valor, color=tipo))+
  geom_line()+
  theme_classic()+
  theme(legend.position = "top",
        axis.text.x = element_text(angle=45, hjust=1))+
  scale_color_manual(values=c("steelblue", "darkred"),
                     labels=c("Observado", "Pronóstico"), name="")+
  scale_x_date(date_breaks = "2 months")+
  xlab("")+
  ylab('"Return Visits"')+
  geom_vline(xintercept = as.numeric(as.Date("2017-01-01")),
             linetype=4, color="darkgreen")+
  geom_vline(xintercept = as.numeric(as.Date("2017-09-24")),
             linetype=4, color="darkgreen")+
  geom_text(aes(x = as.Date('2017-06-01'), y= 2.5e4, 
                label= paste("2017-01-01/2017-09-24")),
            color="steelblue") +
  geom_text(aes(x = as.Date('2017-06-01'), y= 2.3e4, 
                label= paste(
                  "Máximo",
                  format(ceiling(maxReturn), big.mark = ","), "\n",
                  "Minimo", 
                  format(round(minReturn, 2), big.mark = ","),  "\n",
                  "Promedio",
                  format(ceiling(promedioReturnO), big.mark = ","), 
                  sep=" "
                )),
            color="steelblue") +
  geom_text(aes(x = as.Date('2018-02-01'), y=  2.5e4, 
                label= paste("2017-10-01/2018-06-01")),
            color="darkred") +
  geom_text(aes(x = as.Date('2018-02-01'), y=  2.3e4, 
                label= paste(
                  "Pronóstico Promedio" ,
                  format(ceiling(promedioReturn), big.mark = ","), #"\n",
                  # "2018-06-01 ->", Ins18,
                  sep=" "
                )),
            color="darkred")+
  geom_vline(xintercept = as.numeric(as.Date("2018-06-01")),
             linetype=4, color="darkred")


dev.off()


######### TIME RATE ##
timerateTs <- xts(sitioWeb$TimeRate, as.Date(sitioWeb$Date)) %>% 
  na.omit()

arimaRate <- timerateTs %>% 
  coredata %>% 
  ts %>% 
  Arima(order= c(2,1,2)) %>% 
  forecast(h=diferenciaWeb) 


pronosticoRate <- xts(arimaRate$mean, 
                       seq.POSIXt(as.POSIXct(last(timerateTs)), 
                                  length.out =  diferenciaWeb, by="day"))


tsRate <- cbind(timerateTs, pronosticoRate) 
names(tsRate)<- c("Observado", "Pronostico")

ggtsRate <- data.frame(fecha = time(tsRate), 
                         data.frame(tsRate))

promedioRateO <- mean(tsRate["2017-01-01/2017-09-24"], na.rm=T)
maxRate <- max(tsRate["2017-01-01/2017-09-24"], na.rm=T)
minRate <- min(tsRate["2017-01-01/2017-09-24"], na.rm=T)
promedioRate <-mean(pronosticoRate, na.rm=T)
Rate18 <- pronosticoRate["2018-06-01"] %>% 
  as.numeric %>% na.omit %>%  ceiling()

png("imagenes/timeRateWeb.png", width = 1200, height = 800, res=100)
ggtsRate %>% 
  gather(tipo, valor, -fecha) %>% 
  ggplot(aes(x = fecha, y= valor, color=tipo))+
  geom_line()+
  theme_classic()+
  theme(legend.position = "top",
        axis.text.x = element_text(angle=45, hjust=1))+
  scale_color_manual(values=c("steelblue", "darkred"),
                     labels=c("Observado", "Pronóstico"), name="")+
  scale_x_date(date_breaks = "2 months")+
  xlab("")+
  ylab('"Time Rate"')+
  geom_vline(xintercept = as.numeric(as.Date("2017-01-01")),
             linetype=4, color="darkgreen")+
  geom_vline(xintercept = as.numeric(as.Date("2017-09-24")),
             linetype=4, color="darkgreen")+
  geom_text(aes(x = as.Date('2017-06-01'), y= 2.5e4, 
                label= paste("2017-01-01/2017-09-24")),
            color="steelblue") +
  geom_text(aes(x = as.Date('2017-06-01'), y= 2.3e4, 
                label= paste(
                  "Máximo",
                  format(ceiling(maxRate), big.mark = ","), "\n",
                  "Minimo", 
                  format(round(minRate, 2), big.mark = ","),  "\n",
                  "Promedio",
                  format(ceiling(promedioRateO), big.mark = ","), 
                  sep=" "
                )),
            color="steelblue") +
  geom_text(aes(x = as.Date('2018-02-01'), y=  2.5e4, 
                label= paste("2017-10-01/2018-06-01")),
            color="darkred") +
  geom_text(aes(x = as.Date('2018-02-01'), y=  2.3e4, 
                label= paste(
                  "Pronóstico Promedio" ,
                  format(ceiling(promedioRate), big.mark = ","), #"\n",
                  # "2018-06-01 ->", Ins18,
                  sep=" "
                )),
            color="darkred")+
  geom_vline(xintercept = as.numeric(as.Date("2018-06-01")),
             linetype=4, color="darkred")

dev.off()

#### Impronosticables
testTs <- xts(sitioWeb$TestDrive, as.Date(sitioWeb$Date)) %>% 
  na.omit()

png("imagenes/testDriveWeb.png", width = 1200, height = 800, res=100)
plot(testTs, type = "n", main="No pronosticable")
lines(testTs, col="steelblue")
dev.off()

dealerTs <- xts(sitioWeb$DealerSearch, as.Date(sitioWeb$Date)) %>% 
  na.omit()

png("imagenes/dealersearchWeb.png", width = 1200, height = 800, res=100)
plot(dealerTs, type = "n", main="No pronosticable")
lines(dealerTs, col="steelblue")
dev.off()

