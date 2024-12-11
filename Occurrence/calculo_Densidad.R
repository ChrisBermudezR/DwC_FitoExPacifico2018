calculo_densidad<-function(individualCount,
                           diametro_campo, 
                           N_campos, 
                           Vol_Muestra_concentrada, 
                           no_alicuotas, 
                           Vol_alicuota,
                           Vol_Muestreado){
  
  assign("volumenCampo", 
         ((3.1416*(diametro_campo/2)^2)),
         envir = .GlobalEnv
  )
  
  assign("NmL", 
         (individualCount*1000) /(volumenCampo*N_campos),
          envir = .GlobalEnv
         )
  assign("densidadCalculada", 
         (NmL*Vol_Muestra_concentrada) /(Vol_alicuota*Vol_Muestreado),
         envir = .GlobalEnv
  )
  
  
  }

