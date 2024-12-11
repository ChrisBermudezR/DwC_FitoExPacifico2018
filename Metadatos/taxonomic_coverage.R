if(!require(EML)){ install.packages("EML")}

listado_taxonomico<-readxl::read_excel("../Occurrence/matched_Total.xlsx", sheet = "Sheet1")


listado_taxonomico<-listado_taxonomico[19]

taxon_coverage <- set_taxonomicCoverage(listado_taxonomico)
