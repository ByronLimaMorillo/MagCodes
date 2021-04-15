require(RPostgreSQL)
require(dplyr)
library(stringr)

pw <- "us_canamo_r"
drv <- dbDriver("PostgreSQL") 
con <- dbConnect(drv, dbname = "bdc",host = "10.10.1.32", port = 5432,user = "us_canamo_r", password =pw ) 
rm(pw) 
nombres <-  c("Número de licencia","Fecha de emisión de la licencia","Fecha de caducidad","Estado Actual","Estado","Nombre, razón social",
              "RUC","Tipo de licencia (1-7)","Provincia","Cantón","Coordenadas X","Coordenadas Y","Altitud (licencia 3 y 4)","Área Destinada")
datos <- dbGetQuery(con, "SELECT * from sc_canamo.vw_registro") %>% select(1:14) 
Encoding(datos$reg_canton) <- 'UTF-8'

colnames(datos) <- nombres
datos$`Estado Actual` <- ifelse(datos$`Estado Actual`==TRUE,"Activa","Inactiva")
# str(datos)
datos$RUC <- as.numeric(datos$RUC)


datos$`Tipo de licencia (1-7)` <- as.numeric(str_extract(datos$`Tipo de licencia (1-7)`, "[0-9]+"))
datos$`Tipo de licencia (1-7)` <- as.numeric(datos$`Tipo de licencia (1-7)`)







