if(!require(worrms)){install.packages("worrms")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(tidyr)){install.packages("tidyr")}
if(!require(writexl)){install.packages("writexl")}

source("Listado_Encabezados.R")

#Si desea imprimir cualquier explicación puede usar el la linea de comandos: writeLines(Eventos_encabezados$parentEventID, "parentEventID.txt")
#consultar: https://rs.gbif.org/core/dwc_event_2024-02-19.xml


Datos_Colecta<-readxl::read_excel("Datos_Fito_Final.xlsx", sheet = "Datos_Colecta")
Datos_Colecta$eventDate<-as.Date(Datos_Colecta$eventDate, format="%Y-%m-%d")
Datos_Colecta$eventTime <- as.POSIXct(Datos_Colecta$eventTime)
Datos_Colecta$eventTime <- format(Datos_Colecta$eventTime, "%H:%M:%S")



#parentEventID####
Eventos_encabezados$parentEventID

parentEventID<-as.data.frame(rep("DIMAR:CCCP:EXP.PACIFICO:2018", time=34))
colnames(parentEventID)="parentEventID"

#eventID####
Eventos_encabezados$eventID

eventID<-as.data.frame(paste0("DIMAR:CCCP:EXP.PACIFICO:2018:",Datos_Colecta$fieldNumber))
colnames(eventID)="eventID"

#samplingProtocol####
samplingProtocol<-as.data.frame(rep("Red de 20 um"), time=34)
colnames(samplingProtocol)="samplingProtocol"
  
#sampleSizeValue####  
sampleSizeValue<-as.data.frame(rep("1 L"), time=34)
colnames(sampleSizeValue)="sampleSizeValue"

#sampleSizeUnit ####
sampleSizeUnit<-as.data.frame(rep("L"), time=34)
colnames(sampleSizeUnit)="sampleSizeUnit"

#samplingEffort ####

samplingEffort<-as.data.frame(rep("34 muestras de un litro"), time=34)
colnames(samplingEffort)="samplingEffort"

#eventDate####
eventDate<-as.data.frame(format(Datos_Colecta$eventDate, "%Y-%m-%d"))
colnames(eventDate)<-"eventDate"


#year
year<-as.data.frame(format(Datos_Colecta$eventDate, "%Y"))
colnames(year)="year"



# Extraer el mes
month <- as.data.frame(format(Datos_Colecta$eventDate, "%m"))
colnames(month)="month"

#day
day <- as.data.frame(format(Datos_Colecta$eventDate, "%d"))
colnames(day)="day"


#institutionCode
institutionCode<-as.data.frame(rep("Dirección General Marítima (DIMAR)", time=34))
colnames(institutionCode)="institutionCode"

#eventTime###
eventTime<-as.data.frame(Datos_Colecta$eventTime)
colnames(eventTime)<-"eventTime"

#habitat####
habitat<-as.data.frame(rep("Zona Nerítica", time=34))
colnames(habitat)<-"habitat"

#fieldNumber####
fieldNumber<-as.data.frame(Datos_Colecta$fieldNumber)
colnames(fieldNumber)="fieldNumber"


#eventRemarks####
eventRemarks<-as.data.frame(Datos_Colecta$eventRemarks)
colnames(eventRemarks)="eventRemarks"

#type####
type<-as.data.frame(rep("Event", time=34))
colnames(type)<-"type"

#institutionCode####
institutionID<-as.data.frame(rep("830.027.904-1", time=34))
colnames(institutionID)<-"institutionID"

#datasetName####

datasetName<-as.data.frame(rep("Fitoplancton Expedición Pacífico 2018 - Cabo Manglares", time=34))
colnames(datasetName)<-"datasetName"

#datasetID####
datasetID<-as.data.frame(rep("cecoldo.ide_00010", time=34))
colnames(datasetID)<-"datasetID"

#language####
language<-as.data.frame(rep("es", time=34))
colnames(language)<-"language"

#rightsHolder####
rightsHolder<-as.data.frame(rep("Dirección General Marítima (DIMAR)", time=34))
colnames(rightsHolder)="rightsHolder"

#references####
references<-as.data.frame(rep("https://doi.org/10.26640/cecoldo.ide_00010", time=34))
colnames(references)="references"

#locationID####
locationID<-as.data.frame(rep("http://marineregions.org/mrgid/21972", time=34))
colnames(locationID)="locationID"

#higherGeography####
higherGeography<-as.data.frame(rep("América | Sudamérica | Colombia | Región del Pacífico", time=34))
colnames(higherGeography)="higherGeography"

#continent####
continent<-as.data.frame(rep("América del Sur", time=34))
colnames(continent)="continent"

#stateProvince####
stateProvince<-as.data.frame(rep("Nariño", time=34))
colnames(stateProvince)="stateProvince"

#county####
county<-as.data.frame(rep("San Andrés de Tumaco", time=34))
colnames(county)="county"

#waterBody####

waterBody<-as.data.frame(rep("Océano Pacífico", time=34))
colnames(waterBody)="waterBody"

#country####
country<-as.data.frame(rep("Colombia", time=34))
colnames(country)="country"

#countryCode####
countryCode<-as.data.frame(rep("CO", time=34))
colnames(countryCode)="countryCode"

#locality####

locality<-as.data.frame(rep("Cabo Manglares", time=34))
colnames(locality)="locality"

#minimumDepthInMeters####

minimumDepthInMeters<-as.data.frame(rep("0", time=34))
colnames(minimumDepthInMeters)="minimumDepthInMeters"


#maximumDepthInMeters####

maximumDepthInMeters<-as.data.frame(rep("1", time=34))
colnames(maximumDepthInMeters)="maximumDepthInMeters"

#decimalLatitude####
decimalLatitude<-as.data.frame(Datos_Colecta$decimalLatitude)
colnames(decimalLatitude)="decimalLatitude"


#decimalLongitude####
decimalLongitude<-as.data.frame(Datos_Colecta$decimalLongitude)
colnames(decimalLongitude)="decimalLongitude"

#geodeticDatum####
geodeticDatum<-as.data.frame(rep("WGS84", time=34))
colnames(geodeticDatum)="geodeticDatum"

#georeferencedBy####
georeferencedBy<-as.data.frame(rep("Fredy Albeiro Castrillón Valencia", time=34))
colnames(georeferencedBy)="georeferencedBy"

#Unión Final####

eventFinal<-cbind(parentEventID, 
      eventID,
      samplingProtocol,
      sampleSizeValue,
      sampleSizeUnit,
      samplingEffort,
      eventDate,
      year,
      month,
      day,
      institutionCode,
      eventTime,
      habitat,
      fieldNumber,
      eventRemarks,
      type,
      institutionID,
      datasetName,
      datasetID,
      language,
      rightsHolder,
      references,
      locationID,
      higherGeography,
      continent,
      county,
      stateProvince,
      waterBody,
      country,
      countryCode,
      locality,
      minimumDepthInMeters,
      maximumDepthInMeters,
      decimalLatitude,
      decimalLongitude,
      geodeticDatum,
      georeferencedBy
      )
write.table(eventFinal, "../DwC/event.csv", col.names = TRUE, row.names = FALSE, sep=",", fileEncoding="UTF-8")
