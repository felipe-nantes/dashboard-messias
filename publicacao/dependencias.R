# A aplicação consulta agregados; o processamento também lê os DBC oficiais.
DEPENDENCIAS_APP <- c("shiny","bslib","leaflet","DT","sf","spdep","jsonlite","digest","htmltools")
DEPENDENCIAS_PROCESSAMENTO <- unique(c(DEPENDENCIAS_APP,"read.dbc"))
