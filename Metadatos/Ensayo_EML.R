if(!require(EML)){install.packages("EML")}


# para dudas, consulta este link: https://ipt.gbif.org/manual/en/ipt/latest/gbif-metadata-profile#metadata-elements

#Dataset####

delEMLZ<-read_eml("eml-invemar_aves_narino-v2.0.xml")

assign("list", NULL, envir = .GlobalEnv)

# Creadores

source("datasetInfo.R")
source("colaboradoresCCCP.R")
source("project.R")
source("methods.R")
source("coverage.R")
source("abstract.R")
source("keywordSet.R")





my_eml <- list(
  lang="spa",
  '@context'=list(
    
  ),
  '@type'="EML",
  dataset = list(
    #Data Resource
    title = titulo,
    creator = list(Fredy, Christian),
    contact=list(Christian),
    associatedParty=list(),
    metadataProvider=list(Christian),
    abstract=abstract,
    keywordSet=keywordSet,
    intellectualRights=intellectualRights,
    coverage=coverage,
    maintenance=maintenance,
    language=language,
    methods=methods,
    project=project,
    alternateIdentifier="intellectualRights"
    
    
  )

)

EML::write_eml(my_eml, "Prueba_EML.xml")
#> NULL
eml_validate("Prueba_EML.xml")


EML::write_eml(my_eml, "../DwC/EML.xml")
