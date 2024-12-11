#' Grafica del ciclo mareal.
if(!require(tidyverse))install.packages("tidyverse")
if(!require(dplyr))install.packages("dplyr")
if(!require(ggrepel))install.packages("ggrepel")
if(!require(scales))install.packages("scales")
#' @param datos: Conjunto de datos de donde se obtiene el ciclo mareal.
#' @param fecha_hora: Vector o columna con los datos de la fecha y la hora guardados como as.POSIXct(). Se debe usar el pacquete "scales"
#' @param altura: vector o comna con los datos de la Altura mareal referída al MLWS (m).
#'
#' @return
#' @export
#'
#' @examples
ciclo_mareal<-function(datos, fecha_hora, altura, fecha_inicial, fecha_final, segmentos){
  ggplot()+
    geom_line(data=datos, aes(x=fecha_hora, y=altura),linewidth=0.3, colour="grey")+
    geom_hline(yintercept = -2:2,linetype='dotted', col = 'red', linewidth=0.1)+
    geom_hline(yintercept = 0 ,linetype='dotted', col = 'black', linewidth=0.1)+
    labs(x = "Fecha", y = "Altura mareal referida al MLWS [m]") +
    theme_classic()+
    scale_x_datetime(
      breaks = seq(as.POSIXct(fecha_inicial),
                   as.POSIXct(fecha_final), segmentos),
      labels = date_format("%b-%d\n%H:%M", tz = ""),
      expand = c(0, 0),
      limits = c(
        as.POSIXct(fecha_inicial),
        as.POSIXct(fecha_final)))
}
