#coverage
source("taxonomic_coverage.R")
coverage<-list(
    geographicCoverage=list(
        geographicDescription="Distrito de Manejo Integrado - Cabo Manglares",
        boundingCoordinates=list(
          westBoundingCoordinate=-79.2,
          eastBoundingCoordinate=-78.9,
          southBoundingCoordinate=1.5,
          northBoundingCoordinate=1.7
            
        )
    ),
    taxonomicCoverage=taxon_coverage
    ,
    temporalCoverage=list(
        list(rangeOfDates=list(
            beginDate=list(
                calendarDate="2018-11-25"
            ),
            endDate=list(
                calendarDate="2018-11-30"
            )
        )
        )
    )
    
)
