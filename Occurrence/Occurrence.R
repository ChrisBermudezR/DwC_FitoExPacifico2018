if(!require(worrms)){install.packages("worrms")}
if(!require(tidyr)){install.packages("tidyr")}
if(!require(tidyr)){install.packages("dplyr")}
if(!require(writexl)){install.packages("writexl")}

#Consultar: https://rs.gbif.org/core/dwc_occurrence_2024-02-23.xml



wide<-readxl::read_excel("Datos_Fito_Final.xlsx", sheet = "wide")
colnames(wide)

wide_to_long <- tidyr::pivot_longer(wide, 
                                         cols = 11:53,
                                         names_to = "verbatimIdentification",
                                         values_to = "individualCount")

source("calculo_Densidad.R")
calculo_densidad(wide_to_long$individualCount, 
                 wide_to_long$diametro_campo, 
                 wide_to_long$N_campos, 
                 wide_to_long$Vol_Muestra_concentrada, 
                 wide_to_long$no_alicuotas,
                 wide_to_long$Vol_Alicuota,
                 wide_to_long$Vol_Muestreado)

Matriz_Densidad_long<-wide_to_long %>% 
      dplyr::mutate(organismQuantity = densidadCalculada,
                                                    confianza = 2/sqrt(individualCount)*100
                                                    )

eventoTable<-read.csv2("../DwC/event.csv", header = TRUE, sep=",")

tablaMerge<-merge(Matriz_Densidad_long, eventoTable, by="fieldNumber")

#Inicio del OCcurrence####


#occurrenceID####
occurrenceID_secuencia<-as.data.frame(seq(1:1462))
colnames(occurrenceID_secuencia)<-"occurrenceID_secuencia"

bindTabla<-cbind(occurrenceID_secuencia, tablaMerge$eventID)
bindTabla<-bindTabla%>% 
  dplyr::mutate(occurrenceID = paste0(bindTabla$`tablaMerge$eventID`,":",bindTabla$occurrenceID_secuencia))
occurrenceID<-as.data.frame(bindTabla[,3])
colnames(occurrenceID)<-"occurrenceID"

tablaMerge<-cbind(occurrenceID, tablaMerge)

#basisOfRecord####
basisOfRecord<-as.data.frame(rep("HumanObservation", time=1462))
colnames(basisOfRecord)="basisOfRecord"

#type####

type<-as.data.frame(rep("Event", time=1462))
colnames(type)="type"

#institutionCode####

institutionCode<-as.data.frame(tablaMerge$institutionCode)
colnames(institutionCode)="institutionCode"

#institutionID####

institutionID<-as.data.frame(tablaMerge$institutionID)
colnames(institutionID)="institutionID"

#datasetName####

datasetName<-as.data.frame(tablaMerge$datasetName)
colnames(datasetName)="datasetName"

#datasetID####

datasetID<-as.data.frame(tablaMerge$datasetID)
colnames(datasetID)="datasetID"

#language####

language<-as.data.frame(tablaMerge$language)
colnames(language)="language"

#rightsHolder####

rightsHolder<-as.data.frame(tablaMerge$rightsHolder)
colnames(rightsHolder)="rightsHolder"

#rightsHolder####

accessRights<-as.data.frame(rep("Sólo para uso no comercial", time=1462))
colnames(accessRights)="accessRights"

#references####

references<-as.data.frame(tablaMerge$references)
colnames(references)="references"

#ownerInstitutionCode####

ownerInstitutionCode<-as.data.frame(rep("Centro de Investigaciones Oceanográficas e Hidrográficas del Pacífico - Dirección General Marítima (DIMAR)", time=1462))
colnames(ownerInstitutionCode)="ownerInstitutionCode"


#recordedBy####
recordNumber<-as.data.frame(rep("Exp.Pacifico2018", time=1462))
colnames(recordNumber)="recordNumber"

#recordedBy####
recordedBy<-as.data.frame(rep("Fredy Albeiro Castrillón Valencia", time=1462))
colnames(recordedBy)="recordedBy"

#recordedByID####

recordedByID<-as.data.frame(rep("https://orcid.org/0000-0003-2815-6122", time=1462))
colnames(recordedByID)="recordedByID"

#individualCount####

individualCount<-as.data.frame(tablaMerge$individualCount)
colnames(individualCount)="individualCount"


#organismQuantity####

organismQuantity<-as.data.frame(as.integer(round(tablaMerge$organismQuantity)))
colnames(organismQuantity)="organismQuantity"

#organismQuantityType####

organismQuantityType<-as.data.frame(rep("Células por litro", time=1462))
colnames(organismQuantityType)="organismQuantityType"

#occurrenceStatus####

occurrenceStatus<-as.data.frame(individualCount%>%
                                  dplyr::mutate(occurrenceStatus= ifelse(individualCount > 0, "present", "absent")))
occurrenceStatus<-as.data.frame(occurrenceStatus[,2])
colnames(occurrenceStatus)="occurrenceStatus"

#parentEventID####
parentEventID<-as.data.frame(tablaMerge$parentEventID)
colnames(parentEventID)="parentEventID"

#eventID####
eventID<-as.data.frame(tablaMerge$eventID)
colnames(eventID)="eventID"

#samplingProtocol####
samplingProtocol<-as.data.frame(tablaMerge$samplingProtocol)
colnames(samplingProtocol)="samplingProtocol"

#eventType####

eventType<-as.data.frame(rep("Muestra", time=1462))
colnames(eventType)="eventType"

#sampleSizeValue####
sampleSizeValue<-as.data.frame(tablaMerge$sampleSizeValue)
colnames(sampleSizeValue)="sampleSizeValue"

#sampleSizeUnit####
sampleSizeUnit<-as.data.frame(tablaMerge$sampleSizeUnit)
colnames(sampleSizeUnit)="sampleSizeUnit"

#samplingEffort####
samplingEffort<-as.data.frame(tablaMerge$samplingEffort)
colnames(samplingEffort)="samplingEffort"

#eventDate####
eventDate<-as.data.frame(as.Date(tablaMerge$eventDate, format ="%Y-%m-%d"))
colnames(eventDate)="eventDate"


#year####
year<-as.data.frame(tablaMerge$year)
colnames(year)="year"

#month####
month<-as.data.frame(tablaMerge$month)
colnames(month)="month"


#day####
day<-as.data.frame(tablaMerge$day)
colnames(day)="day"



#eventTime####
eventTime<-as.data.frame(tablaMerge$eventTime)
colnames(eventTime)="eventTime"

#habitat####
habitat<-as.data.frame(tablaMerge$habitat)
colnames(habitat)="habitat"

#fieldNumber####
fieldNumber<-as.data.frame(tablaMerge$fieldNumber)
colnames(fieldNumber)="fieldNumber"

#eventRemarks####
eventRemarks<-as.data.frame(tablaMerge$eventRemarks)
colnames(eventRemarks)="eventRemarks"

#locationID####
locationID<-as.data.frame(tablaMerge$locationID)
colnames(locationID)="locationID"

#higherGeography####
higherGeography<-as.data.frame(tablaMerge$higherGeography)
colnames(higherGeography)="higherGeography"

#continent####
continent<-as.data.frame(tablaMerge$continent)
colnames(continent)="continent"

#waterBody####
waterBody<-as.data.frame(tablaMerge$waterBody)
colnames(waterBody)="waterBody"

#country####
country<-as.data.frame(tablaMerge$country)
colnames(country)="country"

#country####
countryCode<-as.data.frame(tablaMerge$countryCode)
colnames(countryCode)="countryCode"

#locality####
locality<-as.data.frame(tablaMerge$locality)
colnames(locality)="locality"


#minimumDepthInMeters####
minimumDepthInMeters<-as.data.frame(tablaMerge$minimumDepthInMeters)
colnames(minimumDepthInMeters)="minimumDepthInMeters"

#maximumDepthInMeters####
maximumDepthInMeters<-as.data.frame(tablaMerge$maximumDepthInMeters)
colnames(maximumDepthInMeters)="maximumDepthInMeters"

#decimalLatitude####
decimalLatitude<-as.data.frame(tablaMerge$decimalLatitude)
colnames(decimalLatitude)="decimalLatitude"

#decimalLongitude####
decimalLongitude<-as.data.frame(tablaMerge$decimalLongitude)
colnames(decimalLongitude)="decimalLongitude"

#geodeticDatum####
geodeticDatum<-as.data.frame(tablaMerge$geodeticDatum)
colnames(geodeticDatum)="geodeticDatum"

#georeferencedBy####
georeferencedBy<-as.data.frame(tablaMerge$georeferencedBy)
colnames(georeferencedBy)="georeferencedBy"


#identifiedBy####
identifiedBy<-as.data.frame(rep("Fredy Albeiro Castrillón Valencia", time=1462))
colnames(identifiedBy)="identifiedBy"

#identifiedByID####

identifiedByID<-as.data.frame(rep("https://orcid.org/0000-0003-2815-6122", time=1462))
colnames(identifiedByID)="identifiedByID"


#dateIdentified####

dateIdentified<-as.data.frame(rep("2019-02-11/2019-04-30", time=1462))
colnames(dateIdentified)="dateIdentified"


#verbatimIdentification####
verbatimIdentification<-as.data.frame(tablaMerge$verbatimIdentification)
colnames(verbatimIdentification)="verbatimIdentification"

#taxonRank####
taxonRank<-as.data.frame(rep("Género", time=1462))
colnames(taxonRank)="taxonRank"


#Tabla Final####

TablaFinal<-cbind(occurrenceID,
              basisOfRecord,
              type,
              institutionCode,
              institutionID,
              datasetName,
              datasetID,
              language,
              rightsHolder,
              accessRights,
              references,
              ownerInstitutionCode,
              recordNumber,
              recordedBy,
              recordedByID,
              individualCount,
              organismQuantity,
              organismQuantityType,
              occurrenceStatus,
              parentEventID,
              eventID,
              eventType,
              samplingProtocol,
              sampleSizeValue,
              sampleSizeUnit,
              samplingEffort,
              eventDate,
              year,
              month,
              day,
              eventTime,
              habitat,
              fieldNumber,
              eventRemarks,
              locationID,
              higherGeography,
              continent,
              waterBody,
              country,
              countryCode,
              locality,
              stateProvince,
              county,
              minimumDepthInMeters,
              maximumDepthInMeters,
              decimalLatitude,
              decimalLongitude,
              geodeticDatum,
              georeferencedBy,
              identifiedBy,
              identifiedByID,
              dateIdentified,
              verbatimIdentification,
              taxonRank
              )






#Taxonomía####
taxonomia<-readxl::read_excel("matched_Total.xlsx", sheet = "Sheet1")
taxonomia<-taxonomia%>%
  dplyr::select(verbatimIdentification,
                   scientificname, 
                   authority, 
                   url, 
                   lsid, 
                   status,
                   kingdom,
                   phylum,
                   class,
                   order,
                   family,
                   genus)

colnames(taxonomia)<-c("verbatimIdentification",
"scientificName",
"scientificNameAuthorship",
"taxonID",
"scientificNameID",
"taxonomicStatus",
"kingdom",
"phylum",
"class",
"order",
"family",
"genus")



taxonomia$verbatimIdentification<-dplyr::recode_factor(taxonomia$verbatimIdentification,
                      'Pseudo-nitzschia' = "Pseudonizschia")

TablaFinal$verbatimIdentification<-dplyr::recode_factor(TablaFinal$verbatimIdentification,
                                                       'Pseudo-nitzschia' = "Pseudonizschia")



DwC<-merge(TablaFinal,taxonomia, by= "verbatimIdentification")



DwC<-DwC%>%
  dplyr::arrange(occurrenceID)

DwC<-DwC%>%
    dplyr::select(
      #Obligatorio para la publicación de los registros biológicos a través del SiB Colombia
      occurrenceID,
      basisOfRecord,
      type,          
      institutionCode,
      institutionID,
      #Obligatorio según el origen de los datos  y recomendado para la documentación de un buen registro biológico
      
      
      datasetName,
      datasetID,             
       language,
       rightsHolder,
       accessRights,
       references,           
      ownerInstitutionCode,
      
      recordNumber,
      recordedBy,
      recordedByID,            
       individualCount,
       organismQuantity,
       organismQuantityType,
      
       occurrenceStatus,  
      
       parentEventID,
       eventID,
      eventType,
      
       samplingProtocol,
       sampleSizeValue,         
       sampleSizeUnit,
       samplingEffort,
       eventDate,
       year,
      month,
       day,
       eventTime,
       habitat,               
       fieldNumber,
       eventRemarks,
       locationID,
       higherGeography,
      continent,
      waterBody,
      country,
      countryCode,
       locality,
      stateProvince,
      county,
      
       minimumDepthInMeters,
       maximumDepthInMeters,
       decimalLatitude,   
       decimalLongitude,
       geodeticDatum,
       georeferencedBy,
      
#Taxonomy

       identifiedBy,            
       identifiedByID,
       dateIdentified,
       verbatimIdentification,
       scientificName,
       scientificNameAuthorship,
       taxonID,
       scientificNameID,
       taxonomicStatus,
       kingdom,
       phylum,
       class,
       order,
       family,                  
       genus,
      taxonRank
    )

write.table(
  DwC, 
  file = "../DwC/Occurrence.csv", 
  col.names = TRUE, 
  row.names = FALSE, 
  sep = ",", 
  fileEncoding = "UTF-8",
  na = "",
  quote = TRUE
)







