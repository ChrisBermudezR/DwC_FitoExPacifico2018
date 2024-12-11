#Título del script: Gráfica del cambio mareal
#Autores: Christian Bermúdez-Rivas 
#Objetivo: construir la gráfica del cambio mareal en el área de Sanquianga-Gorgona
#Lenguaje: R
#Fecha: Enero 2022
#Notas: No olvidar instalar los paquetes necesarios para correr el script
###############################################################################################################################

####Librería####
if(!require(tidyverse))install.packages("tidyverse")
if(!require(dplyr))install.packages("dplyr")
if(!require(ggrepel))install.packages("ggrepel")
if(!require(scales))install.packages("scales")
if(!require(stringr))install.packages("stringr")
if(!require(cowplot))install.packages("cowplot")


source("ciclo_Mareal.R")

####Entrada de datos####
marea<-read.table("RTT_CAN_201811-201812.txt", header=TRUE, sep = "\t")
names(marea)<-c("fecha_Hora", "altura")
#marea <- dplyr::mutate(marea, fecha_Hora= paste(fecha, hora)) 
marea$fecha_Hora<- as.POSIXct(marea$fecha_Hora, format = "%d/%m/%Y %H:%M:%S")
head(marea)
write.table(marea, "Mareas_Cabo_Manglares_Event_1min.csv", sep = ",", col.names = TRUE, row.names = FALSE)


event<-read.table("../DwC/event.csv", header=TRUE, sep = ",")
estaciones<-as.data.frame(cbind(event$eventDate, event$eventTime,event$fieldNumber))
colnames(estaciones)<-c("fecha", "hora", "evento")
estaciones <- mutate(estaciones, fecha_Hora = paste(fecha, hora)) 
estaciones$fecha_Hora<- as.POSIXct(estaciones$fecha_Hora)

estaciones$estacion<-estaciones$evento

estaciones$estacion<-as.character(str_replace(estaciones$estacion, "BME", ""))
estaciones$estacion<-as.character(str_replace(estaciones$estacion, "PME", ""))
head(estaciones)
#Unión de las tablas de mareas
estaciones<-merge(marea, estaciones)

write.table(estaciones, "Alturas_Mareales_Estaciones.csv", sep = ",", col.names = TRUE,row.names = FALSE)

estaciones$marea<-estaciones$evento
estaciones$marea <- str_replace(estaciones$marea, "BM.*$", "Bajamar")
estaciones$marea <- str_replace(estaciones$marea, "PM.*$", "Pleamar")

Bajamar<-subset(estaciones, marea == "Bajamar")

Pleamar<-subset(estaciones, marea == "Pleamar")

####Construcción de la gráfica####
ciclo<-ciclo_mareal(marea, marea$fecha_Hora, marea$altura, "2018-11-25 -05","2018-12-01 -05", "12 hours" )
ciclo


ciclo_muestras<-ciclo+
  geom_point(data=Pleamar, aes(x=as.POSIXct(fecha_Hora), y=altura,color = "red"), size = 1)+
  geom_point(data=Bajamar, aes(x=as.POSIXct(fecha_Hora), y=altura,color = "blue"), size = 1)+
  geom_text_repel(aes(x=as.POSIXct(estaciones$fecha_Hora), 
                       y=estaciones$altura,
                       label = estaciones$estacion),
                      box.padding   = 0.1, 
                      point.padding = 0.5,
                     size = 1.5,
                   segment.size = 0.1,
                  segment.color = 'darkgrey')+
  theme(legend.position = "none",
        text = element_text(size = 5),
       
        axis.line = element_line(linewidth = 0.1),  # Tamaño de la línea del eje
        axis.ticks = element_line(linewidth = 0.1)  # Tamaño de las líneas de los ticks
        )

ciclo_muestras



fecha_inicio <- as.POSIXct("2018-11-24 00:00:00", tz = "UTC")
fecha_fin <- as.POSIXct("2018-12-01 00:00:00", tz = "UTC")

# Crear el gráfico base
ciclo_muestreo<-ciclo_mareal(marea, marea$fecha_Hora, marea$altura, "2018-11-15 -05","2018-12-15 -05","48 hours" )+
  # Agregar el cuadro gris
  geom_rect(aes(xmin = fecha_inicio, xmax = fecha_fin, ymin = -Inf, ymax = Inf),
            fill = "lightblue", alpha = 0.5) +
  geom_text(data = data.frame(x = as.POSIXct("2018-11-27 00:00:00") , y = 1.7),
            aes(x = x, y = y, label = "Toma de muestras"),
            color = "black", size = 2, vjust = -0.5, hjust = 0.5)+
theme(legend.position = "none",
      text = element_text(size = 5),
        axis.line = element_line(linewidth = 0.1),  # Tamaño de la línea del eje
      axis.ticks = element_line(linewidth = 0.1) 
)
ciclo_muestreo




png("onda_Mareal_CaboManglares.png", width = 14, height = 12, res = 300, units = "cm", pointsize = 5)
cowplot::plot_grid(labels=c("A.","B."),
                   label_size = 10,
                   ciclo_muestreo, 
                   ciclo_muestras, 
                    nrow = 2,
                    ncol=1)
dev.off()
