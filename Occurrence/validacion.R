if(!require(validate)){install.packages("validate")}

if(!require(finch)){install.packages("finch")}


listaespecies<-levels(wide_to_long$verbatimIdentification)
levels(as.factor(taxonomia$class))






rules <- validator(basisOfRecord= basisOfRecord == "HumanObservation",
                occurrenceStatus = occurrenceStatus == "absent" | occurrenceStatus == "present",
               eventDate= in_range(eventDate, min="2018-11-25", max = "2018-11-30"),
                organismQuantity=in_range(organismQuantity, min= 0, max = 97665),
               individualCount = in_range(individualCount, min= 0, max = 4416),
               verbatimIdentification = verbatimIdentification %in% listaespecies,
               kingdom = kingdom %in% c("Bacteria",  "Chromista"),
               phylum = phylum %in% c("Cyanobacteria", "Myzozoa", "Ochrophyta" ),
               class = class %in% c("Bacillariophyceae", "Cyanophyceae", "Dictyochophyceae",  "Dinophyceae")
)
out   <- confront(DwC, rules,  ref=list(
                                listaespecies=lista_especies))
summary(out)
plot(out, main="Retailers check")

violated<-violating(DwC, out[1:6])

df_out <- as.data.frame(out)

