# función resumenEstadistico para realizar una tabla de salida de resumen estadístico
# partiendo de un objeto data.frame.
#
# Versión 1.0 15 de diciembre de 2023, Dr. Oscar V. De la Torre-Torres

resumenEstadistico=function(tablaAuxiliar){
  
  # Paso 1 se crea la tabla de salida
  # se determina el número de variables que hay en la tabla:
  nVar=ncol(tablaAuxiliar)
  # Extraemos los nombres de las variables (encabezados) de la tabla (data.frame) a analizar:
  variables=colnames(tablaAuxiliar)
  # Creamos la tabla de salida con el resume estadístico:
  tablaResumen=data.frame(
    "Variable"=variables,
    "Media"=NA,
    "Mediana"=NA,
    "Desv. est."=NA,
    "Mínimo"=NA,
    "Máximo"=NA
  )
  # Se hace el resumen estadítico con un operador for:
  for (contador in 1:nVar){
    # Se calcula la media:
    tablaResumen$Media[contador]=mean(tablaAuxiliar[,contador])
    # Se calcula la mediana:
    tablaResumen$Mediana[contador]=median(tablaAuxiliar[,contador]) 
    # Se calcula la Desv. Est.:
    tablaResumen$Desv..est.[contador]=sd(tablaAuxiliar[,contador])   
    # Se calcula el mínimo:
    tablaResumen$Mínimo[contador]=min(tablaAuxiliar[,contador])  
    # Se calcula el máximo:
    tablaResumen$Máximo[contador]=max(tablaAuxiliar[,contador])    
  }
# Agregamos la instrucción "return" para que regrese al environment el objeto tablaResumen  
return(tablaResumen)
}

