# Mapa interactivo de la correlación entre las prácticas agropecuarias y los cambios de masa forestal en el municipio de Marqu&eacute;s de Comillas, Chiapas

Applicación R-Shiny para visualizar: 
* Capa de información sobre la producción agropecuaria
* Capa de información sobre los cambios de uso de suelo
* Correlación entre los datos de las dos capas
* Autocorrelación por unidad de área de 200m x 200m

El mapa interactivo se encuentra publicado en www.shinyapps.io en un servidor gratuito. 

https://iskarwaluyo.shinyapps.io/mapa_agricultura_masaforestal_comillas/


## Sobre el origen de los datos

Los datos que se consultaron para construír la aplicación fueron: 

1. Series III y VI del cambio de uso de suelo del INEGI
2. Imágenes satelitales del sistema Landsat
3. Datos de la producción agropecuaria de la actualización Marco Censal Agropecuario del 2016
4. Datos de la producción agropecuaria del Censo Nacional Agropecuario del 2007

## Construcción de los data frames

Los archivos de la applicación son: 

ui.R
server.R
global.R

Un cuarto archivo generate_rdata.R sirve para

1. Descargar la información original almacenada en el repositorio de Github
2. Homologar los encabezados de la información original
3. Calcular tamaños promedio de parcela, sumar las actividades agrícolas y generar data frames de las sumas de las actividades agrícolas por área de controlo y por tipo de cultivo. Aunque todos los datos ya están generados, se provee el código en el repositorio como respaldo y evidencia de como se generaron los datos desde sus orígenes.

Los datos se guardaron en el formato RData nativo de R para facilitar su lectura en la applicación 