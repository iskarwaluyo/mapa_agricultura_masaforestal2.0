# CARGAR LIBRERIAS UTILIZADAS DE R
library(sf)
library(sp)
library(geojsonio)
library(rgdal)
library(rmapshaper)
library(rio)
library(dplyr)
library(plyr)
library(ggplot2)
library(maptools)
library(raster)
library(corrplot)
library(reshape2)
library(ggcorrplot)
library(rgeos)

# LECTURA DE SHAPE BASE DE ÁREAS DE CONTROL DE GITHUB
# FUENTE: ACTUALIZACIÓN DEL MARCO SENSAL AGROPECUARIO 2016

ac_mapa <- readOGR("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/ac_mapa.geojson")
colnames(ac_mapa@data) <- toupper(colnames(ac_mapa@data)) # CONVERTIR TODOS LOS ENCABEZADOS A MAYUSCULAS
ac_mapa@data$NOM_ENT <- toupper(ac_mapa@data$NOM_ENT) # CONVERTIR TODOS LOS NOMBRES DE ENTIDADES A MAYUSCULAS
ac_mapa@data$NOM_MUN <- toupper(ac_mapa@data$NOM_MUN) # CONVERTIR TODOS LOS NOMBRES DE MUNICIPIO A MAYUSCULAS
ac_mapa@data$CVE_CONCAT <- as.factor(paste(ac_mapa@data$CVE_MUN, ac_mapa@data$CVE_AGEB, ac_mapa@data$CVE_MZA, sep="_"))

ac_mapa <- ms_simplify(ac_mapa, keep = 0.05)

autocorr <- readOGR("http://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/autocorrelacion.geojson")
autocorr_ac <- readOGR("http://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/autocorrelacion_ac.geojson")
serie_3 <- readOGR("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/serie_3.geojson")
serie_6 <- readOGR("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/serie_6.geojson")
cambios_usv <- readOGR("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/cambios_usv_final.geojson")
cambios_usv_ac <- readOGR("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/cambios_usv_ac.geojson")

# LECTURA DE DATOS DE LA PRODUCCIÓN PECUARIAS DE GITHUB
# FUENTE: ACTUALIZACIÓN DEL MARCO SENSAL AGROPECUARIO 2016
pecuario <- import("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/16_pecuario.csv")
colnames(pecuario) <- toupper(colnames(pecuario)) # CONVERTIR TODOS LOS ENCABEZADOS A MAYUSCULAS
col_p <- ncol(pecuario)

# LECTURA DE DATOS DE LA PRODUCCIÓN FORESTAL DE GITHUB
# FUENTE: ACTUALIZACIÓN DEL MARCO SENSAL AGROPECUARIO 2016
forestal <- import("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/16_forestal.csv")
colnames(forestal) <- toupper(colnames(forestal)) # CONVERTIR TODOS LOS ENCABEZADOS A MAYUSCULAS
col_f <- ncol(forestal)

# LECTURA DE DATOS DE PRODUCCIÓN AGRÍCOLA DEL 2016 DE GITHUB
# FUENTE: ACTUALIZACIÓN DEL MARCO SENSAL AGROPECUARIO 2016
concentrado16 <- import("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/16_agricola_total.csv")
colnames(concentrado16) <- toupper(colnames(concentrado16)) # CONVERTIR TODOS LOS ENCABEZADOS A MAYUSCULAS

# CAMBIO DE USO DE SUELO EN EL MUNICIPIO DE MARQUÉS DE COMILLAS DE GITHUB
# FUENTE: ELABORACIÓN PROPIA CON DATOS DEL INEGI
datos_cambios <- import("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/cambios_2.csv")
colnames(datos_cambios) <- toupper(colnames(datos_cambios)) # CONVERTIR TODOS LOS ENCABEZADOS A MAYUSCULAS

# PROCESAMIENTO DE LOS DATOS

# CÁLCULO Y CONCATENACIÓN SUMAS POR ÁREA DE CONTROL DE TAMAÑO PROMEDIO DE LOS TERRENOS REGISTRADOS TOTALES Y CON SUPERFICIE SEMBRADA
concentrado16$TERRENO_PROM_TOT <- as.numeric(as.character(concentrado16$SUP_CARTO))/as.numeric(as.character(concentrado16$TERRENOS))
concentrado16$TERRENO_PROM_SEM <- as.numeric(as.character(concentrado16$SUP_SEMB))/as.numeric(as.character(concentrado16$NUM_TERR))
concentrado16$PCT_SUPSEM <- 100*as.numeric(as.character(concentrado16$SUP_SEMB)) / as.numeric(as.character(concentrado16$SUP_TOTAL))
concentrado16$PCT_SUPSEM_ACTIVA <- 100*as.numeric(as.character(concentrado16$PCT_SUPSEM)) / as.numeric(as.character(concentrado16$SUP_TOTAL))
concentrado16$CULTI_ESPE <- gsub('CAFE CIMARRON', 'CAFE', concentrado16$CULTI_ESPE)

colnames(concentrado16) <- paste(colnames(concentrado16), "_16", sep="")
concentrado16$CULTI_ESPE <- laply(concentrado16$CULTI_ESPE_16, toupper) # CONVERTIR TODOS LOS VALORES A MAYUSCULAS
concentrado16$CONCAT_ESPE <- paste(concentrado16$CVE_CONCAT, concentrado16$CULTI_ESPE, sep="_")
concentrado16$PCT_AGRICOLA <- as.numeric(as.character(concentrado16$NUM_TERR_16))/as.numeric(as.character(concentrado16$TERRENOS_16))

esp_sum16 <- ddply(concentrado16[,c(24,23,14,15,16:22)], .(CONCAT_ESPE), numcolwise(sum))
esp_sum16$CVE_CONCAT <- substr(esp_sum16$CONCAT_ESPE, 1, 13)
esp_sum16$CULTI_ESPE <- substr(esp_sum16$CONCAT_ESPE, 15, length(esp_sum16$CVE_CONCAT))

esp_sum16 <- merge(ac_mapa@data, esp_sum16, by = "CVE_CONCAT")

ac_sum16 <- ddply(concentrado16[,c(11,24,23,14,15,16:22)], .(CVE_CONCAT_16), numcolwise(sum))

# CREACIÓN DE DATA FRAME CON LOS DATOS DE PRODUCCIÓN AGRÍCOLA, PECUARIA Y FORESTAL Y ÁREAS DE CONTROL
df_ac_16 <- merge(ac_mapa@data, forestal[,c(10,col_f)], by.x = "CVE_CONCAT", by.y = "CVE_CONCAT", all.y=TRUE, all.x = TRUE)
df_ac_16 <- merge(df_ac_16, pecuario[,c(10,col_p)], by.x = "CVE_CONCAT", by.y = "CVE_CONCAT", all.y=TRUE, all.x = TRUE)
df_ac_16 <- merge(df_ac_16, ac_sum16, by.x = "CVE_CONCAT", by.y = "CVE_CONCAT_16", all.y=TRUE, all.x = TRUE)
df_ac_16[is.na(df_ac_16)] <- 0

# CÁLCULO DE PORCENTAJES DE TERRENOS OCUPADOS PARA LA ACTIVIDAD FORESTAL, AGRÍCOLA Y PECAUARIA Y SU SUMA (PCT_OCUPADO)
df_ac_16$PCT_FORESTAL <- 100*df_ac_16$F_TOTAL/as.numeric(as.character(df_ac_16$TERRENOS))
df_ac_16$PCT_PECUARIO <- 100*df_ac_16$P_TOTAL/as.numeric(as.character(df_ac_16$TERRENOS))
df_ac_16$PCT_AGRICOLA <- 100*df_ac_16$NUM_TERR_16/as.numeric(as.character(df_ac_16$TERRENOS))
df_ac_16$PCT_OCUPADO <- (df_ac_16$PCT_FORESTAL + df_ac_16$PCT_PECUARIO + df_ac_16$PCT_AGRICOLA)
df_ac_16[is.na(df_ac_16)] <- 0

# LECTURA DE DATOS DE LA PRODUCCIÓN AGRÍCOLA DEL 2007 DE GITHUB
# FUENTE: CENSO AGROPECUARIO 2007
concentrado07 <- import("https://raw.githubusercontent.com/iskarwaluyo/mapa_agricultura_masaforestal/master/data/raw_data/07_agricola_total.csv")
colnames(concentrado07) <- toupper(colnames(concentrado07)) # CONVERTIR TODOS LOS ENCABEZADOS A MAYUSCULAS
colnames(concentrado07)[colnames(concentrado07) %in% c("MUN", "CULTIVO")] <- c("NOM_MUN", "CULTI_ESPE")
colnames(concentrado07) <- paste(colnames(concentrado07), "_07", sep="")

# CÁLCULO Y CONCATENACIÓN SUMAS POR ÁREA DE CONTROL DE TAMAÑO PROMEDIO DE LOS TERRENOS REGISTRADOS TOTALES Y CON SUPERFICIE SEMBRADA
concentrado07$UP_PROM_TOTAL <- as.numeric(as.character(concentrado07$SUP_SC_07))/as.numeric(as.character(concentrado07$UP_07))
concentrado07$UP_PROM_SEMB <- as.numeric(as.character(concentrado07$SUP_SEMB_07))/as.numeric(as.character(concentrado07$UP_07))
concentrado07$CULTI_ESPE<- laply(concentrado07$CULTI_ESPE_07, toupper) # CONVERTIR TODOS LOS VALORES A MAYUSCULAS
concentrado07$CONCAT_ESPE <- paste(concentrado07$CVE_CONCAT, concentrado07$CULTI_ESPE, sep="_")
 
esp_sum07 <- ddply(concentrado07, .(CONCAT_ESPE), numcolwise(sum))
esp_sum07$CVE_CONCAT <- substr(esp_sum07$CONCAT_ESPE, 1, 13)
esp_sum07$CULTI_ESPE <- substr(esp_sum07$CONCAT_ESPE, 15, length(esp_sum07$CVE_CONCAT))

esp_sum07 <- merge(ac_mapa@data, esp_sum07, by.x = "CVE_CONCAT", by.y="CVE_CONCAT")

ac_sum07 <- ddply(concentrado07[,c(7, 11:25)], .(CVE_CONCAT_07), numcolwise(sum))

# CREACIÓN DE DATA FRAME CON LOS DATOS DE PRODUCCIÓN AGRÍCOLA DEL 2007
df_ac_07 <- merge(ac_mapa@data, ac_sum07, by.x = "CVE_CONCAT", by.y = "CVE_CONCAT_07")
df_ac_07[is.na(df_ac_07)] <- 0

# CONCATENAR DATOS 2007 Y 2016 PARA ESTIMAR CAMBIOS
comparado_esp <- merge(esp_sum07, esp_sum16, by.x = "CONCAT_ESPE", by.y = "CONCAT_ESPE", all.x = TRUE, all.y = TRUE)

# CONCATENAR CASOS COMPARABLES QUE TENGAN DATOS DE CULTIVO Y ÁREA DE CONTROL QUE COINCIDAN
casos_comparables_esp <- comparado_esp[complete.cases(comparado_esp),]
casos_comparables_esp <- casos_comparables_esp[,c(1:11,27, 16:26, 38:46)]

sum_comparables_esp <- ddply(casos_comparables_esp[,c(1,13:32)], .(CONCAT_ESPE), numcolwise(sum))
sum_comparables_esp <- merge(comparado_esp[,c(1:11)], sum_comparables_esp, by = "CONCAT_ESPE")
colnames(sum_comparables_esp) <- c("CONCAT_ESPE", "CVE_CONCAT", "CONTROL", "CLAVE_ENTIDAD", "NOMBRE_ENTIDAD", "CLAVE_MUNICIPIO", "NOMBRE_MUNICIPIO", "CLAVE_AGEB", "CLAVE_MANZANA", "TERRENOS_TOTALES", "SUPERFICIE_TOTAL", "SUPERFICIE_CARTOGRAFICA_07", "UNIDADES_PRODUCTIVAS_07", "SUPERFICIE_SEMBRADA_07", "SUPERFICIE_COSECHADA_07", "TONELADAS_PRODUCIDAS_07", "TONELADAS/HECTAREA_07", "SUPERFICIE_SEMBRADA_%_07", "PERIMETRO_SHAPE", "AREA_SHAPE", "UNIDAD_PRODUCTIVA_PROMEDIO", "UNIDAD_PRODUCTIVA_SEMBRADA", "TERRENOS_TOTALES", "SUPERFICE_CARTOGRAFICA_16", "SUPERFICIE_SEMBRADA_16", "PERIMETRO_SHAPE", "AREA_SHAPE", "TERRENO_PROMEDIO_16", "TERRENO_PROMEDIO_SEMBRADO_16", "SUPERFICIE_SEBRADA_%_16", "SUPERFICIE_SEMBRADA_ACTIVA_%_16")
sum_comparables_esp$CAMBIO_AGRICOLA <- as.numeric(as.character(sum_comparables_esp$SUPERFICIE_SEMBRADA_16)) - as.numeric(as.character(sum_comparables_esp$SUPERFICIE_SEMBRADA_07))
sum_comparables_esp$CAMBIO_AGRICOLA_PCT <- as.numeric(as.character(sum_comparables_esp$`SUPERFICIE_SEBRADA_%_16`)) - as.numeric(as.character(sum_comparables_esp$`SUPERFICIE_SEMBRADA_%_07`))

# SUMA DE CASOS COMPARABLES POR ÁREA DE CONTROL
# CONCATENAR DATOS 2007 Y 2016 PARA ESTIMAR CAMBIOS

comparado_ac <- merge(ac_sum07, ac_sum16, by.x = "CVE_CONCAT_07", by.y = "CVE_CONCAT_16", all.x = TRUE, all.y = TRUE)
casos_comparables_ac <- merge(ac_mapa@data, comparado_ac, by.x = "CVE_CONCAT", by.y = "CVE_CONCAT_07", all.x = TRUE)
 
sum_comparables_ac <- casos_comparables_ac[complete.cases(casos_comparables_ac),]
colnames(sum_comparables_ac) <- c("CVE_CONCAT", "CONTROL", "CLAVE_ENTIDAD", "NOMBRE_ENTIDAD", "CLAVE_MUNICIPIO", "NOMBRE_MUNICIPIO", "CLAVE_AGEB", "CLAVE_MANZANA", "TERRENOS_TOTALES", "SUPERFICIE_TOTAL", "SUPERFICIE_CARTOGRAFICA_07", "UNIDADES_PRODUCTIVAS_07", "SUPERFICIE_SEMBRADA_07", "SUPERFICIE_COSECHADA_07", "TONELADAS_PRODUCIDAS_07", "TONELADAS/HECTAREA_07", "SUPERFICIE_SEMBRADA_%_07", "PERIMETRO_SHAPE", "AREA_SHAPE", "UNIDAD_PRODUCTIVA_PROMEDIO", "UNIDAD_PRODUCTIVA_SEMBRADA", "TERRENOS_TOTALES", "SUPERFICE_CARTOGRAFICA_16", "SUPERFICIE_SEMBRADA_16", "PERIMETRO_SHAPE", "AREA_SHAPE", "TERRENO_PROMEDIO_16", "TERRENO_PROMEDIO_SEMBRADO_16", "SUPERFICIE_SEBRADA_%_16", "SUPERFICIE_SEMBRADA_ACTIVA_%_16")

sum_comparables_ac$CAMBIO_SUP_SEMB_AGRICOLA <- as.numeric(as.character(sum_comparables_ac$SUPERFICIE_SEMBRADA_16)) - as.numeric(as.character(sum_comparables_ac$SUPERFICIE_SEMBRADA_07))
sum_comparables_ac$CAMBIO_SUP_SEMB_AGRICOLA_PCT <- as.numeric(as.character(sum_comparables_ac$`SUPERFICIE_SEBRADA_%_16`)) - as.numeric(as.character(sum_comparables_ac$`SUPERFICIE_SEMBRADA_%_07`))

ac_mapa_b <- merge(ac_mapa, sum_comparables_ac[,c(1, 9:32)], by = "CVE_CONCAT", all.y=TRUE, all.x = TRUE)
ac_mapa_b <- merge(ac_mapa_b, datos_cambios, by.x = "CONTROL", by.y = "ETIQUETAS DE FILA")
ac_mapa_b <- merge(ac_mapa_b, df_ac_16[,c(2,11,12,14,13,22,23,24,25)], by = "CONTROL")

ac_mapa_mc <- subset(ac_mapa_b, ac_mapa_b$NOM_MUN == "MARQUÉS DE COMILLAS")

########### CONTROL
# ANALISIS EXPLORATORIO DE DATOS
suma_terrenos_municipio_07 <- ddply (df_ac_07, 
                        "NOM_MUN", 
                        summarise, y = sum(as.numeric(TERRENOS)))

suma_terrenos_municipio_16 <- ddply (df_ac_16, 
                                     "NOM_MUN", 
                                     summarise, y = sum(as.numeric(TERRENOS)))

colnames(suma_terrenos_municipio_07) <- c("Municipio", "Terrenos")
colnames(suma_terrenos_municipio_16) <- c("Municipio", "Terrenos")

area_promedio_municipio_07 <- ddply (df_ac_07, 
                                     "NOM_MUN", 
                                     summarise, y = mean(as.numeric(SUP_SEMB_07)))
area_promedio_municipio_16 <- ddply (df_ac_16, 
                                     "NOM_MUN", 
                                     summarise, y = mean(as.numeric(SUP_SEMB_16)))

colnames(area_promedio_municipio_07) <- c("Municipio", "Superficie promedio sembrada")
colnames(area_promedio_municipio_16) <- c("Municipio", "Superficie promedio sembrada")

resumen_07 <- cbind(suma_terrenos_municipio_07, area_promedio_municipio_07$`Superficie promedio sembrada`)
resumen_16 <- cbind(suma_terrenos_municipio_16, area_promedio_municipio_16$`Superficie promedio sembrada`)


########### CONTROL
# CORRELACIÓN DE DATOS
df_correlacion <- as.data.frame(ac_mapa_b)
df_correlacion_mc <- as.data.frame(ac_mapa_mc)
df_correlacion_mc[,c(11:51)] <- lapply(df_correlacion_mc[,c(11:51)], as.numeric)
df_correlacion_mc[is.na(df_correlacion_mc)] <- 0

matriz_correlacion <- df_correlacion_mc[,c(11:51)]

df_correlacion_pearson <- as.data.frame(round(cor(matriz_correlacion, method = "pearson"),2))
df_correlacion_pearson_melt <- melt(cor(matriz_correlacion, method = "pearson"))

df_correlacion_spearman <- as.data.frame(round(cor(matriz_correlacion, method = "spearman"),2))
df_correlacion_spearman_melt <- melt(cor(matriz_correlacion, method = "spearman"))

# CREAR ARCHIVOS TIPO RData PARA ALMACENAR LOS RESULTADOS DEL PROCESAMIENTO DE LOS DATOS
setwd("/media/iskar/archivos/MAPAS/mapasR/mapa_agricultura_masaforestal/data/Rdata/")

save(ac_mapa, ac_mapa_b, ac_mapa_mc, serie_3, serie_6, file = "carto.RData")
save(df_ac_16, df_ac_07, file = "datos.RData")
save(concentrado07, concentrado16, file = "concentrados.RData")
save(comparado_ac, comparado_esp, casos_comparables_ac, casos_comparables_esp, sum_comparables_ac, sum_comparables_esp, file = "comparados.RData")
save(df_correlacion, df_correlacion_mc, matriz_correlacion, df_correlacion_pearson, df_correlacion_pearson_melt, 
     df_correlacion_spearman, df_correlacion_spearman_melt, file = "correlaciones.RData")
save(suma_terrenos_municipio_07, suma_terrenos_municipio_16, area_promedio_municipio_07, area_promedio_municipio_16, 
     resumen_07, resumen_16, file = "exploracion.RData")
save(autocorr, autocorr_ac, file = "autocorrelaciones.RData")
save(cambios_usv, cambios_usv_ac, file = "cambios.RData")

# REGRESAR AL ENTORNO GENERAL LOCAL
setwd("/media/iskar/archivos/MAPAS/mapasR/mapa_agricultura_masaforestal")

ggcorrplot(df_correlacion_pearson,
           hc.order = TRUE,
           type = "lower",
           outline.color = "white")

ggcorrplot(df_correlacion_spearman,
           hc.order = TRUE,
           type = "lower",
           outline.color = "white")

