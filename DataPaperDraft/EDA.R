
if(!require(vegan))install.packages("vegan")
if(!require(tidyverse))install.packages("tidyverse")
if(!require(ggplot2))install.packages("ggplot2")
if(!require(tidyr))install.packages("tidyr")
if(!require(ggpubr))install.packages("ggpubr")

#Carga de datos de DwC

direccion_Datos="../DwC/Occurrence.csv"

Occ_Dwc<-read.table(direccion_Datos, header = TRUE, sep = ",")

colnames(Occ_Dwc)

Occ_Dwc$verbatimIdentification
Occ_Dwc$eventID
Occ_Dwc$organismQuantity
Occ_Dwc$eventID


biolData_lon<-as.data.frame(cbind(Occ_Dwc$recordNumber, Occ_Dwc$decimalLatitude, Occ_Dwc$decimalLongitude, Occ_Dwc$verbatimIdentification,Occ_Dwc$organismQuantity, Occ_Dwc$eventID))
colnames(biolData_lon)<-c("recordNumber", "decimalLatitude", "decimalLongitude", "verbatimIdentification", "organismQuantity", "eventID")
biolData_lon$recordNumber<-as.factor(biolData_lon$recordNumber)
biolData_lon$verbatimIdentification<-as.factor(biolData_lon$verbatimIdentification)
biolData_lon$eventID<-as.factor(biolData_lon$eventID)
biolData_lon$verbatimIdentification<-gsub("\\s+", "_", biolData_lon$verbatimIdentification)
biolData_lon$organismQuantity<-as.numeric(biolData_lon$organismQuantity)
biolData_lon$decimalLatitude<-as.numeric(biolData_lon$decimalLatitude)
biolData_lon$decimalLongitude<-as.numeric(biolData_lon$decimalLongitude)

biolData_lon$Estaciones<-gsub("CCCP:EXP.PACIFICO:2018:", "", biolData_lon$eventID)
biolData_lon$evento_Mareal<-gsub("CCCP:EXP.PACIFICO:2018:|E.*", "", biolData_lon$eventID)


biolData_wide<-biolData_lon%>%
  tidyr::pivot_wider(names_from = verbatimIdentification,
                     values_from = organismQuantity)

dim(biolData_wide)

abundancia_Total<-sort(apply(biolData_wide[,7:53], 2, sum), decreasing = TRUE)


Pleamar_bio<-subset(biolData_wide, evento_Mareal=="PM")
Bajamar_bio<-subset(biolData_wide, evento_Mareal=="BM")

Pleamar_bio_Total<-sort(apply(Pleamar_bio[,7:53], 2, sum), decreasing = TRUE)
Bajamar_bio_Total<-sort(apply(Bajamar_bio[,7:53], 2, sum), decreasing = TRUE)

Pleamar_bio_Total<-subset(Pleamar_bio_Total, Pleamar_bio_Total >0)
Bajamar_bio_Total<-subset(Bajamar_bio_Total, Bajamar_bio_Total >0)


options(scipen=999)




Rango_Plot_Total<- ggplot() +
  geom_point(aes(y = Total_Rank_data[["y"]], x = seq(1:47), shape = "Total"), size=1) +
  scale_y_continuous(trans = 'log10', limits = function(x) {c(min(x), ceiling(1000000))}) +
    labs(subtitle = "", x = "Rango", tag="A.", title="Total",
       y = expression(paste("log(Densidad) [Cel ", L^-1, "]"))) +
  theme_bw()+
  theme(legend.position = "none",
        legend.title = element_text(color = "white", size = 0),
        text = element_text(size = 8),
        axis.line = element_line(linewidth = 0.1),  # Tamaño de la línea del eje
        axis.ticks = element_line(linewidth = 0.1) )+
  annotate("text", x = 4, y = 478540, label = expression(italic("Skeletonema")), size = 3, color = "black") +
  annotate("text", x = 5, y = 104600, label = expression(italic("Chaetoceros")), size = 3, color = "black")+
  annotate("text", x = 6, y = 27908, label = expression(italic("Thalassionema")), size = 3, color = "black")


Rango_Plot_Pleamar<- ggplot() +
   geom_point(aes(y = Pleamar_Rank_data[["y"]], x = seq(1:40), shape = "Pleamar"), size=1) +
    scale_y_continuous(trans = 'log10', limits = function(x) {c(min(x), ceiling(1000000))}) +
    labs(subtitle = "", x = "Rango", tag="B.", title="Pleamar",
       y = expression(paste("log(Densidad) [Cel ", L^-1, "]"))) +
  theme_bw()+
  theme(legend.position = "none",
        legend.title = element_text(color = "white", size = 0),
        text = element_text(size = 8),
        axis.line = element_line(linewidth = 0.1),  # Tamaño de la línea del eje
        axis.ticks = element_line(linewidth = 0.1) )+
  annotate("text", x = 6, y = 195800, label = expression(italic("Skeletonema")), size = 3, color = "black") +
  annotate("text", x = 7, y = 48000, label = expression(italic("Chaetoceros")), size = 3, color = "black")+
  annotate("text", x = 8, y = 9000, label = expression(italic("Odontella")), size = 3, color = "black")


Rango_Plot_Bajamar<- ggplot() +
  geom_point(aes(y = Bajamar_Rank_data[["y"]], x = seq(1:41), shape = "Bajamar"), size=1) +
  scale_y_continuous(trans = 'log10', limits = function(x) {c(min(x), ceiling(1000000))}) +
  
  labs(subtitle = "", x = "Rango", tag="C.", title="Bajamar",
       y = expression(paste("log(Densidad) [Cel ", L^-1, "]"))) +
  theme_bw()+
  theme(legend.position = "none",
        legend.title = element_text(color = "white", size = 0),
        text = element_text(size = 8),
        axis.line = element_line(linewidth = 0.1),  # Tamaño de la línea del eje
        axis.ticks = element_line(linewidth = 0.1) )+
  annotate("text", x = 5, y = 285000, label = expression(italic("Skeletonema")), size = 3, color = "black") +
  annotate("text", x = 6, y = 58000, label = expression(italic("Chaetoceros")), size = 3, color = "black")+
  annotate("text", x = 8, y = 21000, label = expression(italic("Thalassionema")), size = 3, color = "black")






Total_Rank_data<- rad.lognormal(abundancia_Total)
Pleamar_Rank_data<- rad.lognormal(Pleamar_bio_Total)
Bajamar_Rank_data<- rad.lognormal(Bajamar_bio_Total)

Total_Rank_radfit<- radfit(t(abundancia_Total))
Pleamar_Rank_radfit<- radfit(t(Pleamar_bio_Total))
Bajamar_Rank_radfit<- radfit(t(Bajamar_bio_Total))


total_plot<-ggplot()+
  geom_point(aes(y=Total_Rank_radfit[["y"]], x=seq(1:47), color="black"), size=1)+
  geom_line(aes(y= Total_Rank_radfit[["models"]][["Null"]][["fitted.values"]],x=seq(1:47),color="Null" ))+
  geom_line(aes(y= Total_Rank_radfit[["models"]][["Preemption"]][["fitted.values"]],x=seq(1:47),color="Preemption"))+
  geom_line(aes(y= Total_Rank_radfit[["models"]][["Lognormal"]][["fitted.values"]],x=seq(1:47),color="Lognormal" ))+
  geom_line(aes(y= Total_Rank_radfit[["models"]][["Zipf"]][["fitted.values"]],x=seq(1:47),color="Zipf"))+
  geom_line(aes(y= Total_Rank_radfit[["models"]][["Mandelbrot"]][["fitted.values"]],x=seq(1:47),color="Mandelbrot" ))+
  scale_y_continuous(trans = 'log10', limits = function(x) {c(min(x), ceiling(1000000))}) +
  scale_color_manual(values = c("#404040", "#d7191c","#fdae61", "#a6dba0", "#008837", "#2b83ba"),
                     #breaks=c("Jet Chocó", "Null", "Preemption", "Lognormal", "Zipf", "Mandelbrot"),
                     name = " ",
                     labels=c("Rango", "Null", "Preemption", "Lognormal", "Zipf", "Mandelbrot")
  )+
  
  labs(colour = "",  title="Total",  x ="Rango", 
       y = expression(paste("log(Densidad) [Cel ", L^-1, "]")),tag="D.")+
  theme_bw()+
  theme(legend.position = "none",
        legend.title = element_text(color = "white", size = 0),
        text = element_text(size = 8),
        axis.line = element_line(linewidth = 0.1),  # Tamaño de la línea del eje
        axis.ticks = element_line(linewidth = 0.1) )


pleamar_plot<-ggplot()+
  geom_point(aes(y=Pleamar_Rank_radfit[["y"]], x=seq(1:40), color="black"), size=1)+
  geom_line(aes(y= Pleamar_Rank_radfit[["models"]][["Null"]][["fitted.values"]],x=seq(1:40),color="Null" ))+
  geom_line(aes(y= Pleamar_Rank_radfit[["models"]][["Preemption"]][["fitted.values"]],x=seq(1:40),color="Preemption"))+
  geom_line(aes(y= Pleamar_Rank_radfit[["models"]][["Lognormal"]][["fitted.values"]],x=seq(1:40),color="Lognormal" ))+
  geom_line(aes(y= Pleamar_Rank_radfit[["models"]][["Zipf"]][["fitted.values"]],x=seq(1:40),color="Zipf"))+
  geom_line(aes(y= Pleamar_Rank_radfit[["models"]][["Mandelbrot"]][["fitted.values"]],x=seq(1:40),color="Mandelbrot" ))+
  scale_y_continuous(trans = 'log10', limits = function(x) {c(min(x), ceiling(1000000))}) +
  scale_color_manual(values = c("#404040", "#d7191c","#fdae61", "#a6dba0", "#008837", "#2b83ba"),
                     #breaks=c("Jet Chocó", "Null", "Preemption", "Lognormal", "Zipf", "Mandelbrot"),
                     name = " ",
                     labels=c("Rango", "Null", "Preemption", "Lognormal", "Zipf", "Mandelbrot")
  )+
  
  labs(colour = "",  title="Pleamar",  x ="Rango", 
       y = expression(paste("log(Densidad) [Cel ", L^-1, "]")),tag="E.")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_text(color = "white", size = 0),
        text = element_text(size = 8),
        axis.line = element_line(linewidth = 0.1),  # Tamaño de la línea del eje
        axis.ticks = element_line(linewidth = 0.1) )

bajamar_plot<-ggplot()+
  geom_point(aes(y=Bajamar_Rank_radfit[["y"]], x=seq(1:41), color="black"), size=1)+
  geom_line(aes(y= Bajamar_Rank_radfit[["models"]][["Null"]][["fitted.values"]],x=seq(1:41),color="Null" ))+
  geom_line(aes(y= Bajamar_Rank_radfit[["models"]][["Preemption"]][["fitted.values"]],x=seq(1:41),color="Preemption"))+
  geom_line(aes(y= Bajamar_Rank_radfit[["models"]][["Lognormal"]][["fitted.values"]],x=seq(1:41),color="Lognormal" ))+
  geom_line(aes(y= Bajamar_Rank_radfit[["models"]][["Zipf"]][["fitted.values"]],x=seq(1:41),color="Zipf"))+
  geom_line(aes(y= Bajamar_Rank_radfit[["models"]][["Mandelbrot"]][["fitted.values"]],x=seq(1:41),color="Mandelbrot" ))+
  scale_y_continuous(trans = 'log10', limits = function(x) {c(min(x), ceiling(1000000))}) +
  scale_color_manual(values = c("#404040", "#d7191c","#fdae61", "#a6dba0", "#008837", "#2b83ba"),
                     #breaks=c("Jet Chocó", "Null", "Preemption", "Lognormal", "Zipf", "Mandelbrot"),
                     name = " ",
                     labels=c("Bajamar", "Null", "Preemption", "Lognormal", "Zipf", "Mandelbrot")
  )+
  
  labs(colour = "",  title="Bajamar",  x ="Rango", 
       y = expression(paste("log(Densidad) [Cel ", L^-1, "]")),tag="F.")+
  theme_bw()+
  theme(legend.position = "none",
        legend.title = element_text(color = "white", size = 0),
        text = element_text(size = 8),
        axis.line = element_line(linewidth = 0.1),  # Tamaño de la línea del eje
        axis.ticks = element_line(linewidth = 0.1) )


legend <- get_legend(pleamar_plot + theme(legend.position = "bottom"))

library(patchwork)
library(cowplot)
combined_plots <- (Rango_Plot_Total) /
  (Rango_Plot_Pleamar | Rango_Plot_Bajamar) /
  (total_plot | pleamar_plot | bajamar_plot)

final_layout <- plot_grid(combined_plots, legend, ncol = 1, rel_heights = c(1, 0.1))


layout <- (Rango_Plot_Total) /
  (Rango_Plot_Pleamar | Rango_Plot_Bajamar)

# Guardar la imagen
ggsave("Fig_03.png", layout, width = 25, height = 15, units = "cm", dpi = 300)

