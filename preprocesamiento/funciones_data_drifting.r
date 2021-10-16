#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

setwd("C:/Archivos/maestria/dmeyf/")

datasetA  <- fread( "./datasetsOri/paquete_premium_202009.csv" )
datasetB  <- fread( "./datasetsOri/paquete_premium_202011.csv" )

# FUNCIONES --------------------------------------------------
#funcion densidad
densidades <- function( campo  )
{
  distA  <- quantile(  datasetA[ , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  distB  <- quantile(  datasetB[ , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  
  a1  <- pmin( distA[[1]], distB[[1]] )
  a2  <- pmax( distA[[2]], distB[[2]] )
  
  densidadA  <- density( datasetA[ , get(campo) ] , kernel="gaussian", na.rm=TRUE)
  densidadB  <- density( datasetB[ , get(campo) ] , kernel="gaussian", na.rm=TRUE)
  
  plot(densidadA, 
       col="blue",
       xlim= c( a1, a2),
       main= paste0("Densidades    ",  campo), )
  
  lines(densidadB, col="red", lty=2)
  
  legend(  "topright",  
           legend=c("A", "B"),
           col=c("blue", "red"), lty=c(1,2))
  
}

# funcion histograma
densidades_g <- function( campo , den = T )
{
  sep = data.frame(var =  datasetA[, get(campo)] )
  sep$mes = "sep"
  nov =  data.frame(var =  datasetB[, get(campo)] )
  nov$mes = "nov"
  
  combo <- rbind(sep, nov)
  
  if( den == T){
    ggplot(combo, aes(var, fill = mes))+geom_density(alpha = 0.2)+ labs(title = campo)+xlim(range(combo$var))
  }else{
    ggplot(combo, aes(var, fill = mes))+geom_histogram(alpha = 0.2, position="identity")+ labs(title = campo)
  }
}

# Funcion de summaries
summary_comparado <- function(campo)
{ 
  print("Septiembre \n")
  print(summary(datasetA[, get(campo) ] )  )  
  print("Noviembre \n")
  print(summary(datasetB[, get(campo) ] )  )  
  }
#----------------------------------------------------------------------------------

campos_malos  <-  c("internet", "mactivos_margen", "foto_mes", "tpaquete1","mpayroll",
                    "mcajeros_propios_descuentos", "tmobile_app", "cmobile_app_trx",
                    "mtarjeta_visa_descuentos", "mtarjeta_master_descuentos",
                    "Master_mpagominimo", "matm_other", "Master_madelantodolares" ) 

summary_comparado( "mcajeros_propios_descuentos")

densidades( "mcajeros_propios_descuentos")
densidades( "tmobile_app")
densidades_g("ccajeros_propios_descuentos", den =F)


densidades("Master_Finiciomora")
densidades("Visa_mconsumosdolares")
densidades_g("ccallcenter_transacciones", den =F)


datasetA[, Master_fultimo_cierre := Master_fultimo_cierre+5]
datasetA[, Visa_fultimo_cierre := Visa_fultimo_cierre+5]

datasetA[, Master_Finiciomora := Master_Finiciomora+10]
datasetB[, Master_Finiciomora := Master_Finiciomora-30]

datasetA[, Visa_Finiciomora := Visa_Finiciomora+30]
datasetB[, Visa_Finiciomora := Visa_Finiciomora-30]

# funcioncita de summary

datasetB$Visa_madelantodolares
