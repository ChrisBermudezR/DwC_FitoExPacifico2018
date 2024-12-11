if(!require(worrms)){install.packages("worrms")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(tidyr)){install.packages("tidyr")}
if(!require(writexl)){install.packages("writexl")}

registros<-readxl::read_excel("Plantilla de registros 4.0.xlsx", sheet = "Plantilla")

individualCount<-readxl::read_excel("Datos_Fito_Final.xlsx", sheet = "Conteo")
organismQuantity<-readxl::read_excel("Datos_Fito_Final.xlsx", sheet = "Densidad")
verbatimIdentification<-readxl::read_excel("matched_Total.xlsx", sheet = "Sheet1")


# Convertir de wide a long
individualCount_long <- pivot_longer(individualCount, 
                           cols = 2:48,
                           names_to = "verbatimIdentification",
                           values_to = "individualCount")
organismQuantity_long <- pivot_longer(organismQuantity, 
                                     cols = 2:48,
                                     names_to = "verbatimIdentification",
                                     values_to = "organismQuantity")



union<-cbind(individualCount_long, organismQuantity_long[,3])


prueba<-merge(union, verbatimIdentification, by="verbatimIdentification")

sort(prueba$fieldNumber)



install.packages("validate")
library(validate)

head(DwC)
as.Date(tablaMerge$eventDate, format ="%Y-%m-%d")
as.Date(DwC$eventDate,  format = "%Y-%m-%d")

rules <- validator(occurrenceID= is.character(occurrenceID),
                   verbatimIdentification = is_unique(verbatimIdentification,
                                                      scientificNameID,
                                                      taxonRank,
                                                      kingdom,),
                basisOfRecord= basisOfRecord == "HumanObservation" | 
                    basisOfRecord =="PreservedSpecimen" | 
                    basisOfRecord == "LivingSpecimen"|
                    basisOfRecord == "MachineObservation" |
                    basisOfRecord == "MaterialSample" |
                    basisOfRecord == "FossilSpecimen",
                occurrenceStatus = occurrenceStatus == "absent" | occurrenceStatus == "present"
                    
                   )
out   <- confront(DwC, rules)
summary(out)
plot(out)


violaciones<-violating(DwC, out[1:3])
levels(DwC$verbatimIdentification)


evenDate >= as.Date("2018-11-25",  format = "%Y-%m-%d") & evenDate <= as.Date("2018-11-30",  format = "%Y-%m-%d")
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
                  identificationQualifier
)

DwC$evenDate >= as.Date("2018-11-25",  format = "%Y-%m-%d") & DwC$evenDate <= as.Date("2018-11-30",  format = "%Y-%m-%d")
