
#Paquetes Usados####
if(!require(worrms)){install.packages("worrms")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(purrr)){install.packages("purrr")}
if(!require(writexl)){install.packages("writexl")}
if(!require(algaeClassify)){install.packages("algaeClassify")}
if(!require(rgbif)){install.packages("rgbif")}
if(!require(inborutils)){remotes::install_github("inbo/inborutils")}


#Carga de la tabla inicial####
data<-readxl::read_excel("Datos_Fito_Final.xlsx", sheet = "Formato")

#Selección de los datos
especies<-colnames(data[7:49])
verbatimIdentification_df<-as.data.frame(especies)


taxa_df<-cbind(verbatimIdentification_df, as.data.frame(especies))
colnames(taxa_df)<-c("verbatimIdentification", "scientificName")

#Worms####

Especies<-levels(as.factor(taxa_df$scientificName))


matchedTaxa<-worrms::wm_records_taxamatch(taxa_df$scientificName, marine_only = TRUE, kingdom = "chromista")

lista_principal <- stats::setNames(matchedTaxa, taxa_df$verbatimIdentification)


# Unir los dataframes en uno solo
df <- dplyr::bind_rows(lista_principal)
levels(as.factor(df$phylum))

df<-df %>% filter( phylum== "Cyanobacteria" | 
                    phylum== "Myzozoa" |
                    phylum== "Heterokontophyta" |
                     phylum== "Ochrophyta")

levels(as.factor(df$scientificname))
levels(as.factor(taxa_df$scientificName))

verbatimIdentification<-taxa_df$verbatimIdentification

taxonomia_final<-cbind(df, verbatimIdentification)


write_xlsx(taxonomia_final, "matched_Total.xlsx")
