
###cargo datos###

files <- list.files(path = paste(getwd(),'input','afiliados', sep ='/'))


library(xlsx)
for (i in 1:length(files)){
  assign(files[i], read.xlsx(paste(getwd(),'input','Afiliados',files[i], sep ='/'), 2))
}

###clasifico datos###
IAMC <- list()
for (i in 1:length(files)){
  ifelse(grepl("IAMC",files[i]),IAMC[i]<-list(files[i]),print("false"))
}

# NO SE PARA QUE SIRVE
# IAMC <- IAMC[-which(sapply(IAMC, is.null))]

ASSE <- list()
for (i in 1:length(files)){
  ifelse(grepl("ASSE",files[i]),ASSE[i]<-list(files[i]),print("false"))
}
# ASSE <- ASSE[-which(sapply(ASSE, is.null))]

SP <- list()
for (i in 1:length(files)){
  ifelse(grepl("SP",files[i]),SP[i]<-list(files[i]),print("false"))
}
# SP <- SP[-which(sapply(SP, is.null))]

# ###cod inst IAMC###
# 
# instituciones <-`2017_12_IAMC.xls`[4,]
# instituciones <- t(instituciones[,!is.na(instituciones)])
# attributes(instituciones) <- NULL
# instituciones <- as.data.frame(instituciones)
# names(instituciones)[names(instituciones)=="instituciones"]="instNombre"
# instituciones <- subset(instituciones, !(instituciones$instNombre %in% c("INSTITUCION")))
# 
# 
# instituciones1 <-`2017_12_IAMC.xls`[30,]
# instituciones1 <- t(instituciones1[,!is.na(instituciones1)])
# attributes(instituciones1) <- NULL
# instituciones1 <- as.data.frame(instituciones1)
# names(instituciones1)[names(instituciones1)=="instituciones1"]="instNombre"
# instituciones1 <- subset(instituciones1, !(instituciones1$instNombre %in% c("INSTITUCION")))
# 
# instituciones <- rbind(instituciones,instituciones1)
# rm(instituciones1)
# 
# 
# instituciones$instCod <- 0
# 
# instituciones$instCod <- ifelse(instituciones$instNombre=='EVANGELICA',264,
#                                 ifelse(instituciones$instNombre=='CASA DE GALICIA',567,
#                                        ifelse(instituciones$instNombre=='CASMU',769,
#                                               ifelse(instituciones$instNombre=='CIRCULO CATOLICO',1062,
#                                                      ifelse(instituciones$instNombre=='CUDAM',1163,
#                                                             ifelse(instituciones$instNombre=='COSEM',1210,
#                                                                    ifelse(instituciones$instNombre=='GREMCA',2315,
#                                                                           ifelse(instituciones$instNombre=='MUCAM',3567,
#                                                                                  ifelse(instituciones$instNombre=='SMI',4315,
#                                                                                       ifelse(instituciones$instNombre=='UNIVERSAL',4810,62))))))))))
# instituciones$instCod <- ifelse(instituciones$instNombre=='GREMEDA',5012,
#                                ifelse(instituciones$instNombre=='CAAMEPA',5163,
#                                       ifelse(instituciones$instNombre=='CRAMI',5315,
#                                              ifelse(instituciones$instNombre=='COMECA',5517,
#                                                     ifelse(instituciones$instNombre=='CAMCEL',5810,
#                                                            ifelse(instituciones$instNombre=='COMECEL',5935,
#                                                                   ifelse(instituciones$instNombre=='ORAMECO',5961,
#                                                                          ifelse(instituciones$instNombre=='CAMEC',6012,
#                                                                                 ifelse(instituciones$instNombre=='CAMOC',6062,
#                                                                                        ifelse(instituciones$instNombre=='CAMEDUR',6416,
#                                                                                               ifelse(instituciones$instNombre=='COMEFLO',6517,
#                                                                                                      ifelse(instituciones$instNombre=='COMEF',6618,
#                                                                                                             ifelse(instituciones$instNombre=='CAMDEL',6769,
#                                                                                                                    ifelse(instituciones$instNombre=='AMECOM',6860,
#                                                                                                                           ifelse(instituciones$instNombre=='CRAME',6985,
#                                                                                                                                  ifelse(instituciones$instNombre=='COMEPA',7062,
#                                                                                                                                         ifelse(instituciones$instNombre=='AMEDRIN',7214,
#                                                                                                                                                ifelse(instituciones$instNombre=='CAMY',7264,
#                                                                                                                                                       ifelse(instituciones$instNombre=='CASMER',7466,
#                                                                                                                                                              ifelse(instituciones$instNombre=='COMERI',7553,
#                                                                                                                                                                     ifelse(instituciones$instNombre=='COMERO',7618,
#                                                                                                                                                                            ifelse(instituciones$instNombre=='SMQS',7719,
#                                                                                                                                                                                   ifelse(instituciones$instNombre=='AMSJ',7810,
#                                                                                                                                                                                          ifelse(instituciones$instNombre=='CAMS',8012,
#                                                                                                                                                                                                 ifelse(instituciones$instNombre=='COMTA',8163,
#                                                                                                                                                                                                        ifelse(instituciones$instNombre=='COMETT',8220,
#                                                                                                                                                                                                               ifelse(instituciones$instNombre=='IAC',8264,
#                                                                                                                                                                                                                              instituciones$instCod)))))))))))))))))))))))))))
# ###fecha####
# IAMC <- as.character(IAMC)
# fecha <- as.data.frame(IAMC)
# fecha$IAMC <- substr(fecha$IAMC,1,7)
# fecha$files1 <- substr(fecha$IAMC,1,4)
# fecha$files2 <- substr(fecha$IAMC,6,7)
# fecha$files3<- "/"
# fecha$Fecha <- paste(fecha$files2,fecha$files3,fecha$files1,sep = "")
# fecha <- as.data.frame(fecha[ ,colnames(fecha)=="Fecha"])
# names(fecha)[names(fecha)=="fecha[, colnames(fecha) == \"Fecha\"]"]="Período"
# 
# 
# ###afiliacion###
# afiliacion <- c("FONASA","NO FONASA", "Sin datos")
# afiliacion <- as.data.frame(afiliacion)
# 
# ###sexo###
# sexo<-c("MASCULINO", "FEMENINO", "Sin datos")
# sexo <- as.data.frame(sexo)
# 
# ###edad###
# edad <- as.data.frame(`2017_12_IAMC.xls`[7:15,2])
# names(edad)[names(edad)=="`2017_12_IAMC.xls`[7:15, 2]"]="Edad"
# 
# ###junto todo##
# 
# afiliados <- merge(instituciones,afiliacion)
# afiliados <- merge(afiliados,fecha)
# afiliados <- merge(afiliados,sexo)
# afiliados <- merge(afiliados,edad)




# IAMC <- as.list(IAMC)
# data <- data.frame("")
# 
# for (i in 1:length(IAMC)){
#   data[,i] <- eval(paste('as.data.frame(`',IAMC[i],'`[7:15,3])', sep = ""))
# }
# 
# #data[i]<-paste('`',IAMC[i],'`','[1,1]',sep = "")
# 
# 
# prueba <- as.data.frame(`2014_03_Afiliados_IAMC.xls`[7:15,3])




#creo objetos que voy a usar

IAMC <- as.list(IAMC)
filas <- list(3:20)
lista <- list()


#creo lista con tabla1, tabla2, tabla3.....
for (i in 1:length(IAMC)){
  eval(parse(text=paste('lista','[i]','<- list(','"','tabla',i,'"',')',sep = "")))
}


#creo tablas para rellenar
for (i in 1:length(IAMC)){
  eval(parse(text=paste('tabla',i,' <- data.frame(matrix(0, nrow = 9, ncol = length(IAMC)))', sep = "")))
}




#relleno datos1: española fonasa masculino

for (i in 1:length(IAMC)){
  eval(parse(text=paste('tabla',i,' <- as.data.frame(`',IAMC[i],'`[4:15,3])', sep = "")))
}  
###################################################
#agrego inst
for (i in 1:length(IAMC)){
  eval(parse(text=paste('tabla',i,'$inst',' <- tabla',i,'[1,1]', sep = "")))
}

#agrego afiliacion
for (i in 1:length(IAMC)){
  eval(parse(text=paste('tabla',i,'$afil',' <- tabla',i,'[2,1]', sep = "")))
}

#agrego sexo
for (i in 1:length(IAMC)){
  eval(parse(text=paste('tabla',i,'$sexo',' <- "masculino"', sep = "")))
}

#quito filas de mas
for (i in 1:length(IAMC)){
  eval(parse(text=paste('tabla',i,' <- tabla',i,'[-1,]', sep = "")))
  eval(parse(text=paste('tabla',i,' <- tabla',i,'[-1,]', sep = "")))
  eval(parse(text=paste('tabla',i,' <- tabla',i,'[-1,]', sep = "")))
}

#agrego la fecha
for (i in 1:length(IAMC)){
  eval(parse(text=paste('tabla',i,'$fecha',' <- substr(IAMC[',i,'],1,7)', sep = "")))
}

#agrego la edad
edad <- as.data.frame(`2017_12_IAMC.xls`[7:15,2])
names(edad)[names(edad)=="`2017_12_IAMC.xls`[7:15, 2]"] <- ""
for (i in 1:length(IAMC)){
  eval(parse(text=paste('tabla',i,'$edad <- as.matrix(edad)', sep = "")))
}

# for (i in 1:length(IAMC)){
#   eval(parse(text=paste('tabla',i,'$edad <- c("<1","1 a 4","5 a 14","15 a 19","20 a 44","45 a 64","65 a 74","> 74","s/d)"', sep = "")))
# }
#cambiar aca

#cambio nombre a todas las columnas de dataframes tabla1, tabla2....
for(i in 1:length(lista)){
  eval(parse(text=paste('colnames(',lista,')[1]', '<- "data"',sep = "")))
}

#cambio rownames
# rownames(tabla1) <- seq(1,9)
# tabla1$rn <- seq(1,9)
# 
# for (i in 2:length(lista)){
#   eval(parse(text=paste('tabla',i,'$rn',' <- seq(tabla',i-1,'[9,7]+1,tabla',i-1,'[9,7]+9)', sep = "")))
# }
# 
# for (i in 1:length(IAMC)){
#   eval(parse(text=paste('rownames(tabla',i,') <- as.factor(tabla',i,'$rn)', sep = "")))
# }



#append

prueba <- seq(1,length(lista))
tablas <- as.list(paste("tabla",prueba, sep = ""))
paste0(tablas)
tablas <- do.call("paste",as.list(tablas))
tablas <- gsub(" ",", ",tablas)
paste(tablas)

datos <- eval(parse(text=paste('do.call("rbind",','list(',list(tablas),'))', sep = "")))



