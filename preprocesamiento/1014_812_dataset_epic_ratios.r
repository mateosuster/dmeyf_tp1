#source("~/buckets/b1/crudoB/R/812_dataset_epic.r")
#Necesita para correr en Google Cloud
#256 GB de memoria RAM
#300 GB de espacio en el disco local
#8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("Rcpp")
require("rlist")
require("yaml")

require("lightgbm")


#defino la carpeta donde trabajo
# directory.root  <-  "~/Investigaci?n/1. Maestr?a DM/2021Economia"  
directory.root  <-  "C:/Archivos/maestria/dmeyf/"  
setwd( directory.root )

palancas  <- list()  #variable con las palancas para activar/desactivar

palancas$version  <- "v010"   #Muy importante, ir cambiando la version

palancas$variablesdrift  <- c(  "cliente_vip", "internet")  #aqui van las columnas que se quieren eliminar
  

palancas$corregir <-  TRUE    # TRUE o FALSE

palancas$nuevasvars <-  TRUE  #si quiero hacer Feature Engineering manual

palancas$dummiesNA  <-  FALSE #Idea de Santiago Dellachiesa de UAustral

palancas$lag1   <- FALSE    #lag de orden 1
palancas$delta1 <- FALSE    # campo -  lag de orden 1 
palancas$lag2   <- FALSE
palancas$delta2 <- FALSE
palancas$lag3   <- FALSE
palancas$delta3 <- FALSE
palancas$lag4   <- FALSE
palancas$delta4 <- FALSE
palancas$lag5   <- FALSE
palancas$delta5 <- FALSE
palancas$lag6   <- FALSE
palancas$delta6 <- FALSE

palancas$promedio3  <- FALSE  #promedio  de los ultimos 3 meses
palancas$promedio6  <- FALSE

palancas$minimo3  <- FALSE  #minimo de los ultimos 3 meses
palancas$minimo6  <- FALSE

palancas$maximo3  <- FALSE  #maximo de los ultimos 3 meses
palancas$maximo6  <- FALSE

palancas$tendencia6  <- FALSE    #Great power comes with great responsability


palancas$canaritosimportancia  <- FALSE  #si me quedo solo con lo mas importante de canaritosimportancia


#escribo para saber cuales fueron los parametros
write_yaml(  palancas,  paste0( "./work/palanca_",  palancas$version  ,".yaml" ) )

#------------------------------------------------------------------------------

ReportarCampos  <- function( dataset )
{
  cat( "La cantidad de campos es ", ncol(dataset) , "\n" )
}
#------------------------------------------------------------------------------
#Agrega al dataset una variable que va de 1 a 12, el mes, para que el modelo aprenda estacionalidad

AgregarMes  <- function( dataset )
{
  dataset[  , mes := foto_mes %% 100 ]
  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#Elimina las variables que uno supone hace Data Drifting

DriftEliminar  <- function( dataset, variables )
{
  dataset[  , c(variables) := NULL ]
  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#A las variables que tienen nulos, les agrega una nueva variable el dummy de is es nulo o no {0, 1}

DummiesNA  <- function( dataset )
{

  nulos  <- colSums( is.na(dataset[foto_mes==202101]) )  #cuento la cantidad de nulos por columna
  colsconNA  <- names( which(  nulos > 0 ) )

  dataset[ , paste0( colsconNA, "_isNA") :=  lapply( .SD,  is.na ),
             .SDcols= colsconNA]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#Corrige poniendo a NA las variables que en ese mes estan dañadas

Corregir  <- function( dataset )
{
  #acomodo los errores del dataset

  dataset[ foto_mes==201801,  internet   := NA ]
  dataset[ foto_mes==201801,  thomebanking   := NA ]
  dataset[ foto_mes==201801,  chomebanking_transacciones   := NA ]
  dataset[ foto_mes==201801,  tcallcenter   := NA ]
  dataset[ foto_mes==201801,  ccallcenter_transacciones   := NA ]
  dataset[ foto_mes==201801,  cprestamos_personales   := NA ]
  dataset[ foto_mes==201801,  mprestamos_personales   := NA ]
  dataset[ foto_mes==201801,  mprestamos_hipotecarios  := NA ]
  dataset[ foto_mes==201801,  ccajas_transacciones   := NA ]
  dataset[ foto_mes==201801,  ccajas_consultas   := NA ]
  dataset[ foto_mes==201801,  ccajas_depositos   := NA ]
  dataset[ foto_mes==201801,  ccajas_extracciones   := NA ]
  dataset[ foto_mes==201801,  ccajas_otras   := NA ]

  dataset[ foto_mes==201806,  tcallcenter   :=  NA ]
  dataset[ foto_mes==201806,  ccallcenter_transacciones   :=  NA ]

  dataset[ foto_mes==201904,  ctarjeta_visa_debitos_automaticos  :=  NA ]
  dataset[ foto_mes==201904,  mttarjeta_visa_debitos_automaticos := NA ]
  dataset[ foto_mes==201904,  Visa_mfinanciacion_limite := NA ]

  dataset[ foto_mes==201905,  mrentabilidad     := NA ]
  dataset[ foto_mes==201905,  mrentabilidad_annual     := NA ]
  dataset[ foto_mes==201905,  mcomisiones      := NA ]
  dataset[ foto_mes==201905,  mpasivos_margen  := NA ]
  dataset[ foto_mes==201905,  mactivos_margen  := NA ]
  dataset[ foto_mes==201905,  ctarjeta_visa_debitos_automaticos  := NA ]
  dataset[ foto_mes==201905,  ccomisiones_otras := NA ]
  dataset[ foto_mes==201905,  mcomisiones_otras := NA ]

  dataset[ foto_mes==201910,  mpasivos_margen   := NA ]
  dataset[ foto_mes==201910,  mactivos_margen   := NA ]
  dataset[ foto_mes==201910,  ccomisiones_otras := NA ]
  dataset[ foto_mes==201910,  mcomisiones_otras := NA ]
  dataset[ foto_mes==201910,  mcomisiones       := NA ]
  dataset[ foto_mes==201910,  mrentabilidad     := NA ]
  dataset[ foto_mes==201910,  mrentabilidad_annual        := NA ]
  dataset[ foto_mes==201910,  chomebanking_transacciones  := NA ]
  dataset[ foto_mes==201910,  ctarjeta_visa_descuentos    := NA ]
  dataset[ foto_mes==201910,  ctarjeta_master_descuentos  := NA ]
  dataset[ foto_mes==201910,  mtarjeta_visa_descuentos    := NA ]
  dataset[ foto_mes==201910,  mtarjeta_master_descuentos  := NA ]
  dataset[ foto_mes==201910,  ccajeros_propios_descuentos := NA ]
  dataset[ foto_mes==201910,  mcajeros_propios_descuentos := NA ]

  dataset[ foto_mes==202001,  cliente_vip   := NA ]

  dataset[ foto_mes==202006,  active_quarter   := NA ]
  dataset[ foto_mes==202006,  internet   := NA ]
  dataset[ foto_mes==202006,  mrentabilidad   := NA ]
  dataset[ foto_mes==202006,  mrentabilidad_annual   := NA ]
  dataset[ foto_mes==202006,  mcomisiones   := NA ]
  dataset[ foto_mes==202006,  mactivos_margen   := NA ]
  dataset[ foto_mes==202006,  mpasivos_margen   := NA ]
  dataset[ foto_mes==202006,  mcuentas_saldo   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_debito_transacciones   := NA ]
  dataset[ foto_mes==202006,  mautoservicio   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_visa_transacciones   := NA ]
  dataset[ foto_mes==202006,  mtarjeta_visa_consumo   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_master_transacciones   := NA ]
  dataset[ foto_mes==202006,  mtarjeta_master_consumo   := NA ]
  dataset[ foto_mes==202006,  ccomisiones_otras   := NA ]
  dataset[ foto_mes==202006,  mcomisiones_otras   := NA ]
  dataset[ foto_mes==202006,  cextraccion_autoservicio   := NA ]
  dataset[ foto_mes==202006,  mextraccion_autoservicio   := NA ]
  dataset[ foto_mes==202006,  ccheques_depositados   := NA ]
  dataset[ foto_mes==202006,  mcheques_depositados   := NA ]
  dataset[ foto_mes==202006,  ccheques_emitidos   := NA ]
  dataset[ foto_mes==202006,  mcheques_emitidos   := NA ]
  dataset[ foto_mes==202006,  ccheques_depositados_rechazados   := NA ]
  dataset[ foto_mes==202006,  mcheques_depositados_rechazados   := NA ]
  dataset[ foto_mes==202006,  ccheques_emitidos_rechazados   := NA ]
  dataset[ foto_mes==202006,  mcheques_emitidos_rechazados   := NA ]
  dataset[ foto_mes==202006,  tcallcenter   := NA ]
  dataset[ foto_mes==202006,  ccallcenter_transacciones   := NA ]
  dataset[ foto_mes==202006,  thomebanking   := NA ]
  dataset[ foto_mes==202006,  chomebanking_transacciones   := NA ]
  dataset[ foto_mes==202006,  ccajas_transacciones   := NA ]
  dataset[ foto_mes==202006,  ccajas_consultas   := NA ]
  dataset[ foto_mes==202006,  ccajas_depositos   := NA ]
  dataset[ foto_mes==202006,  ccajas_extracciones   := NA ]
  dataset[ foto_mes==202006,  ccajas_otras   := NA ]
  dataset[ foto_mes==202006,  catm_trx   := NA ]
  dataset[ foto_mes==202006,  matm   := NA ]
  dataset[ foto_mes==202006,  catm_trx_other   := NA ]
  dataset[ foto_mes==202006,  matm_other   := NA ]
  dataset[ foto_mes==202006,  ctrx_quarter   := NA ]
  dataset[ foto_mes==202006,  tmobile_app   := NA ]
  dataset[ foto_mes==202006,  cmobile_app_trx   := NA ]


  dataset[ foto_mes==202010,  internet  := NA ]
  dataset[ foto_mes==202011,  internet  := NA ]
  dataset[ foto_mes==202012,  internet  := NA ]
  dataset[ foto_mes==202101,  internet  := NA ]

  dataset[ foto_mes==202009,  tmobile_app  := NA ]
  dataset[ foto_mes==202010,  tmobile_app  := NA ]
  dataset[ foto_mes==202011,  tmobile_app  := NA ]
  dataset[ foto_mes==202012,  tmobile_app  := NA ]
  dataset[ foto_mes==202101,  tmobile_app  := NA ]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#Esta es la parte que los alumnos deben desplegar todo su ingenio

AgregarVariables  <- function( dataset, ms_vars = T )
{
  #INICIO de la seccion donde se deben hacer cambios con variables nuevas
  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
  dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  dataset[ , mv_status02       := Master_status +  Visa_status ]
  dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  dataset[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  dataset[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

  dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
                                          ifelse( is.na(Master_status), 10, Master_status), 
                                          Visa_status)  ]

  dataset[ , mv_status07       := ifelse( is.na(Master_status), 
                                          ifelse( is.na(Visa_status), 10, Visa_status), 
                                          Master_status)  ]


  #combino MasterCard y Visa
  dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

  dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

  #a partir de aqui juego con la suma de Mastercard y Visa
  dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
  dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
  dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
  dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
  dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
  dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
  dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
  dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
  dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
  dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
  dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
  dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]

  #Aqui debe usted agregar sus propias nuevas variables
  
  ### Ya las agregu? con script python
  
  if(ms_vars == TRUE){
  # variables MS
  dataset[ , mv_Fvencimiento_max         := pmax( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ] #MS
  dataset[ , mv_msaldototal_avg := mean( c(Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ), by =numero_de_cliente  ] #MS
  dataset[ , mv_msaldopesos_avg := mean( c(Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ), by =numero_de_cliente  ] #MS
  dataset[ , mv_mconsumostotales     := mv_mconsumosdolares+mv_mconsumospesos  ] #MS
  
  dataset[, mv_tarjeta_consumo := sum(c(mtarjeta_visa_consumo  ,mtarjeta_master_consumo), na.rm = T) ]
  dataset[, mv_saldo_consumo := sum(c(mv_msaldototal , mv_tarjeta_consumo), na.rm = T) ]
  dataset[, consumo_prestamos := sum(c(mv_tarjeta_consumo , mprestamos_prendarios), na.rm = T) ]
  
  dataset[, m_cons_tar_limite := Master_mconsumospesos / Master_mlimitecompra ]
  dataset[, v_cons_tar_limite := Visa_mconsumospesos / Visa_mlimitecompra ]
  # dataset[, mv_cons_tar_limite := mv_mconsumospesos / mv_mlimitecompra ]
  dataset[, mv_cons_tar_limite_2 :=sum(c(Master_mconsumospesos ,Visa_mconsumospesos), na.rm = T) 
          /   sum(c( Master_mlimitecompra, Visa_mlimitecompra), na.rm = T) ]
  dataset[, limite_descubierto := sum(c(mv_mlimitecompra,mdescubierto_preacordado), na.rm = T)]
  dataset[, mv_saldo_tar_limite :=sum(c(Master_msaldototal ,Visa_msaldototal), na.rm = T)/
            sum( c(Master_mlimitecompra, Visa_mlimitecompra), na.rm = T) ]
  
  dataset[, m_saldo_usd_pes := Master_msaldodolares / Master_msaldopesos ]
  dataset[, v_saldo_usd_pes := Visa_msaldodolares / Visa_msaldopesos ]
  dataset[, mv_saldo_usd_pes := sum(c(m_saldo_usd_pes , v_saldo_usd_pes), na.rm = T) ]
  dataset[, mv_saldo_usd_pes_2 := sum(c(Master_msaldodolares , Visa_msaldodolares), na.rm = T)/
            sum(c(Visa_msaldopesos,Visa_msaldodolares), na.rm = T) ]
  
  dataset[, payroll_cons_tot := cpayroll_trx / Master_mconsumototal ]
  dataset[, saldo_antiguedad := mcuentas_saldo / cliente_antiguedad ]
  dataset[, saldo_antiguedad := Visa_msaldototal / cliente_antiguedad ]
  dataset[, edad_nro_cliente := cliente_edad / numero_de_cliente ]
  
  dataset[, rentabilidad_anual := mrentabilidad / mrentabilidad_annual ]
  dataset[, rentabilidad_total := sum(c(mcomisiones , mrentabilidad_annual ,mrentabilidad,mactivos_margen,mpasivos_margen), na.rm = T)]
  dataset[, rentabilidad_total_4 :=  sum(c(mcomisiones , mrentabilidad_annual,mactivos_margen,mpasivos_margen), na.rm = T)/mcuentas_saldo]
  dataset[, rentabilidad_total_productos := rentabilidad_total/cproductos]
  
  dataset[, cuentas_totales := sum(c(tcuentas,ccuenta_corriente, ccaja_ahorro), na.rm = T)]
  dataset[, tpaquete := sum(c(tpaquete1,tpaquete3, tpaquete4), na.rm = T)]
  dataset[, cta_corr_ca := sum(c(mcuenta_corriente,mcaja_ahorro), na.rm = T)]
  dataset[, cta_corr_ca_ad := sum(c(mcaja_ahorro_adicional,mcuenta_corriente_adicional), na.rm = T)]
  dataset[, cta_corr_ca_tot := sum(c(cta_corr_ca_ad,cta_corr_ca), na.rm = T)]
  dataset[, cta_corr_ca_tot_2 := sum(c(cta_corr_ca_tot,mcaja_ahorro_dolares), na.rm = T)]
  
  dataset[, mplazo_fijo := sum(c(mplazo_fijo_dolares, mplazo_fijo_pesos), na.rm = T)]
  dataset[, mplazo_fijo_cant := mplazo_fijo/ cplazo_fijo]
  
  dataset[ ,saldo_limite          := mcuentas_saldo  / mv_mlimitecompra ]
  dataset[ ,saldo_limite_2          := sum(c(mcuentas_saldo  , mv_mlimitecompra), na.rm = T) ]
  
  dataset[ , transacciones_limite          :=  mv_mlimitecompra/ctarjeta_debito_transacciones ]
  dataset[ , monto_transacciones          :=  mautoservicio/ctarjeta_debito_transacciones ]
  dataset[, vm_transacciones := sum(c(ctarjeta_visa_transacciones,ctarjeta_master_transacciones), na.rm = T)]
  dataset[, mv_consumo_trans := mv_tarjeta_consumo /vm_transacciones ]
  dataset[ ,transacciones_limite_2          :=sum(c( ctarjeta_debito_transacciones  , mv_mlimitecompra), na.rm = T) ]
  dataset[ ,cantidad_tarjetas          := sum(c(ctarjeta_debito  , ctarjeta_visa ,ctarjeta_master), na.rm = T) ]
  dataset[ ,monto_debito_tarjetas          := mautoservicio/ sum(c(ctarjeta_debito  , ctarjeta_visa), na.rm = T) ]
  dataset[, vm_transacciones_2 := vm_transacciones *cantidad_tarjetas ]
  dataset[, payroll_total := mpayroll  * mpayroll2 ]
  dataset[, costo_salida := sum(c(cprestamos_personales , cprestamos_hipotecarios , cprestamos_prendarios, 
                                  cantidad_tarjetas,cuentas_totales,cplazo_fijo ,
                                  cinversion1, cproductos,cliente_vip ,cpayroll_trx,cpayroll2_trx,ctarjeta_visa_debitos_automaticos
                                  ,ctarjeta_master_debitos_automaticos ,ccuenta_debitos_automaticos, ccaja_seguridad ,
                                  ccomisiones_mantenimiento, ccomisiones_otras, tcallcenter,thomebanking,
                                  tmobile_app), na.rm = T)]
  
  dataset[, seguros :=sum(c(cseguro_vida , cseguro_auto ,cseguro_vivienda , cseguro_accidentes_personales), na.rm = T) ]
  dataset[, descuentos :=sum(c(ctarjeta_visa_descuentos, ctarjeta_master_descuentos), na.rm = T) ]
  dataset[, comisiones :=sum(c( mcomisiones_mantenimiento, mcomisiones_otras), na.rm = T) ]
  
  dataset[, inversiones := sum(c(minversion1_pesos , minversion1_dolares, minversion2), na.rm = T)]
  dataset[, debitos_automaticos := sum(c(mcuenta_debitos_automaticos , mttarjeta_visa_debitos_automaticos, 
                                         mttarjeta_master_debitos_automaticos), na.rm = T)]
  dataset[, costo_salida_2 := sum(c(mprestamos_personales , mprestamos_prendarios , mprestamos_hipotecarios ,
                                    mdescubierto_preacordado), na.rm = T) ]
  dataset[, costo_salida_3 := sum(c(costo_salida_2, payroll_total), na.rm = T) ]
  dataset[, costo_salida_4 := sum(c(costo_salida_3, inversiones ,mplazo_fijo, debitos_automaticos ), na.rm = T)]
  dataset[, costo_salida_5 := sum(c(costo_salida_4, mv_tarjeta_consumo ), na.rm = T)]
  dataset[, m_servicios := sum(c(mpagodeservicios, mpagomiscuentas ), na.rm = T)]
  dataset[, c_servicios := sum(c(cpagodeservicios, cpagomiscuentas ), na.rm = T)]
  dataset[, c_forex := sum(c(cforex_buy, cforex_sell ), na.rm = T)]
  dataset[, m_forex := sum(c(mforex_buy, mforex_sell ), na.rm = T)]
  dataset[, c_transferencias := sum(c(ctransferencias_recibidas, ctransferencias_emitidas ), na.rm = T)]
  dataset[, m_transferencias := sum(c(mtransferencias_recibidas, mtransferencias_emitidas ), na.rm = T)]
  dataset[, extracciones := cextraccion_autoservicio *mextraccion_autoservicio]
  dataset[, c_cheques := sum(c(ccheques_depositados, ccheques_emitidos)) ]
  dataset[, m_cheques := sum(c(mcheques_depositados, mcheques_emitidos)) ]
  dataset[, c_cheques_rech := sum(c(ccheques_depositados_rechazados, ccheques_emitidos_rechazados), na.rm = T) ]
  dataset[, m_cheques_rech := sum(c(mcheques_depositados_rechazados, mcheques_emitidos_rechazados), na.rm = T) ]
  dataset[, trans_call_caja := sum(c(chomebanking_transacciones,chomebanking_transacciones ), na.rm = T) ]
  dataset[, ccajas := sum(c(ccajas_transacciones,ccajas_consultas, ccajas_depositos, ccajas_extracciones,
                            ccajas_otras,catm_trx), na.rm = T) ] 
  dataset[, catm := sum(c(catm_trx, catm_trx_other), na.rm = T) ]
  dataset[, matm := sum(c(matm, matm_other), na.rm = T) ]
  dataset[, transacciones_2 := cmobile_app_trx *chomebanking_transacciones ]
  
  
  dataset[, financiacion_limit_compra := mv_mfinanciacion_limite + mv_mlimitecompra ]
  dataset[, transferencias_x := mtransferencias_recibidas* ctransferencias_recibidas]
  dataset[, transferencias_x_2 := c_transferencias* m_transferencias]
  dataset[, prestamos_saldo := mprestamos_personales/mcuentas_saldo]
  dataset[, ctrx_x_saldo := ctrx_quarter *mcuentas_saldo]
  dataset[, ctrx_x_ca := ctrx_quarter *mcaja_ahorro]
  dataset[, ctrx_x_edad := ctrx_quarter *cliente_edad]
  dataset[, ctrx_x_antiguedad := ctrx_quarter *cliente_antiguedad]
  dataset[, rentabilidad_x_antiguedad := mrentabilidad_annual *cliente_antiguedad]
  dataset[, ctrx_x_payroll := ctrx_quarter *cpayroll_trx]
  dataset[, ctrx_x_saldo := ctrx_quarter *cpayroll_trx]
  dataset[, trans_x_edad := ctarjeta_visa_transacciones * cliente_edad]
  }
  
  
  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
  infinitos_qty  <- sum( unlist( infinitos) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <- NA
  }


  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
    cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <- 0
  }

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#esta funcion supone que dataset esta ordenado por   <numero_de_cliente, foto_mes>
#calcula el lag y el delta lag

Lags  <- function( dataset, cols, nlag, deltas )
{

  sufijo  <- paste0( "_lag", nlag )

  dataset[ , paste0( cols, sufijo) := shift(.SD, nlag, NA, "lag"), 
             by= numero_de_cliente, 
             .SDcols= cols]

  #agrego los deltas de los lags, con un "for" nada elegante
  if( deltas )
  {
    sufijodelta  <- paste0( "_delta", nlag )

    for( vcol in cols )
    {
     dataset[,  paste0(vcol, sufijodelta) := get( vcol)  - get(paste0( vcol, sufijo))]
    }
  }

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#calcula el promedio de los ultimos  nhistoria meses

Promedios  <- function( dataset, cols, nhistoria )
{

  sufijo  <- paste0( "_avg", nhistoria )
  
  dataset[ , paste0( cols, sufijo) := frollmean(x=.SD, n=nhistoria, na.rm=TRUE, algo="fast", align="right"), 
             by= numero_de_cliente, 
             .SDcols= cols]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#calcula el minimo de los ultimos  nhistoria meses

Minimos  <- function( dataset, cols, nhistoria )
{

  sufijo  <- paste0( "_min", nhistoria )

  dataset[ , paste0( cols, sufijo) := frollapply(x=.SD, FUN="min", n=nhistoria, align="right"), 
             by= numero_de_cliente, 
             .SDcols= cols]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#calcula el maximo de los ultimos  nhistoria meses

Maximos  <- function( dataset, cols, nhistoria )
{

  sufijo  <- paste0( "_max", nhistoria )

  dataset[ , paste0( cols, sufijo) := frollapply(x=.SD, FUN="max", n=nhistoria, na.rm=TRUE, align="right"), 
             by= numero_de_cliente, 
             .SDcols= cols]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------

#se calculan para los 6 meses previos el minimo, maximo y tendencia calculada con cuadrados minimos
#la formual de calculo de la tendencia puede verse en https://stats.libretexts.org/Bookshelves/Introductory_Statistics/Book%3A_Introductory_Statistics_(Shafer_and_Zhang)/10%3A_Correlation_and_Regression/10.04%3A_The_Least_Squares_Regression_Line
#para la maxíma velocidad esta funcion esta escrita en lenguaje C, y no en la porqueria de R o Python

Rcpp::cppFunction('NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde ) 
{
  // [[Rcpp::plugins(openmp)]]
  /* Aqui se cargan los valores para la regresion */
  double  x[100] ;
  double  y[100] ;

  int n = pcolumna.size();
  NumericVector out( n );


  //#if defined(_OPENMP)
  //#pragma omp parallel for
  //#endif
  for(int i = 0; i < n; i++)
  {
    int  libre    = 0 ;
    int  xvalor   = 1 ;

    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;

       if( !R_IsNA( a ) ) 
       {
          y[ libre ]= a ;
          x[ libre ]= xvalor ;
          libre++ ;
       }

       xvalor++ ;
    }

    /* Si hay al menos dos valores */
    if( libre > 1 )
    {
      double  xsum  = x[0] ;
      double  ysum  = y[0] ;
      double  xysum = xsum * ysum ;
      double  xxsum = xsum * xsum ;
      double  vmin  = y[0] ;
      double  vmax  = y[0] ;

      for( int h=1; h<libre; h++)
      { 
        xsum  += x[h] ;
        ysum  += y[h] ; 
        xysum += x[h]*y[h] ;
        xxsum += x[h]*x[h] ;

        if( y[h] < vmin )  vmin = y[h] ;
        if( y[h] > vmax )  vmax = y[h] ;
      }

      out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ;
    }
    else
    {
      out[ i ]  =  NA_REAL ; 
    }
  }

  return  out;
}')


#------------------------------------------------------------------------------
#calcula la tendencia de las variables cols de los ultimos 6 meses
#la tendencia es la pendiente de la recta que ajusta por cuadrados minimos

Tendencia  <- function( dataset, cols )
{
  #Esta es la cantidad de meses que utilizo para la historia
  ventana_regresion  <- 6

  last  <- nrow( dataset )

  #creo el vector_desde que indica cada ventana
  #de esta forma se acelera el procesamiento ya que lo hago una sola vez
  vector_ids   <- dataset$numero_de_cliente

  vector_desde  <- seq( -ventana_regresion+2,  nrow(dataset)-ventana_regresion+1 )
  vector_desde[ 1:ventana_regresion ]  <-  1

  for( i in 2:last )  if( vector_ids[ i-1 ] !=  vector_ids[ i ] ) {  vector_desde[i] <-  i }
  for( i in 2:last )  if( vector_desde[i] < vector_desde[i-1] )  {  vector_desde[i] <-  vector_desde[i-1] }

  for(  campo  in   cols )
  {
    nueva_col     <- fhistC( dataset[ , get(campo) ], vector_desde ) 

    dataset[ , paste0( campo, "_tend") := nueva_col[ (0*last +1):(1*last) ]  ]
  }

}
#------------------------------------------------------------------------------
VPOS_CORTE  <- c()

fganancia_lgbm_meseta  <- function(probs, datos) 
{
  vlabels  <- getinfo(datos, "label")
  vpesos   <- getinfo(datos, "weight")

  #solo sumo 48750 si vpesos > 1, hackeo 
  tbl  <- as.data.table( list( "prob"=probs, "gan"= ifelse( vlabels==1 & vpesos > 1, 48750, -1250 ) ) )

  setorder( tbl, -prob )
  tbl[ , posicion := .I ]
  tbl[ , gan_acum :=  cumsum( gan ) ]
  setorder( tbl, -gan_acum )   #voy por la meseta

  gan  <- mean( tbl[ 1:500,  gan_acum] )  #meseta de tamaño 500

  pos_meseta  <- tbl[ 1:500,  median(posicion)]
  VPOS_CORTE  <<- c( VPOS_CORTE, pos_meseta )

  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#Elimina del dataset las variables que estan por debajo de la capa geologica de canaritos

CanaritosImportancia  <- function( dataset )
{

  gc()
  dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

  for( i  in 1:(ncol(dataset)/5))  dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset))]

  campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01" ) )

  azar  <- runif( nrow(dataset) )
  entrenamiento  <-  dataset[ , foto_mes>= 202001 &  foto_mes<= 202010 &  foto_mes!=202006 & ( clase01==1 | azar < 0.10 ) ]

  dtrain  <- lgb.Dataset( data=    data.matrix(  dataset[ entrenamiento==TRUE, campos_buenos, with=FALSE]),
                          label=   dataset[ entrenamiento==TRUE, clase01],
                          weight=  dataset[ entrenamiento==TRUE, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)] )

  dvalid  <- lgb.Dataset( data=    data.matrix(  dataset[ foto_mes==202011, campos_buenos, with=FALSE]),
                          label=   dataset[ foto_mes==202011, clase01],
                          weight=  dataset[ foto_mes==202011, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)] )


  param <- list( objective= "binary",
                 metric= "custom",
                 first_metric_only= TRUE,
                 boost_from_average= TRUE,
                 feature_pre_filter= FALSE,
                 verbosity= -100,
                 seed= 999983,
                 max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                 min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                 lambda_l1= 0.0,         #por ahora, lo dejo fijo
                 lambda_l2= 0.0,         #por ahora, lo dejo fijo
                 max_bin= 31,            #por ahora, lo dejo fijo
                 num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                 force_row_wise= TRUE,    #para que los alumnos no se atemoricen con tantos warning
                 learning_rate= 0.02, 
                 feature_fraction= 0.50,
                 min_data_in_leaf= 4000,
                 num_leaves= 600,
                 early_stopping_rounds= 200 )

  modelo  <- lgb.train( data= dtrain,
                        valids= list( valid= dvalid ),
                        eval= fganancia_lgbm_meseta,
                        param= param,
                        verbose= -100 )

  tb_importancia  <- lgb.importance( model= modelo )
  tb_importancia[  , pos := .I ]
  
  fwrite( tb_importancia, file="./work/impo.txt", sep="\t" )
  
  umbral  <- tb_importancia[ Feature %like% "canarito", median(pos) - sd(pos) ]
  col_inutiles  <- tb_importancia[ pos >= umbral | Feature %like% "canarito",  Feature ]

  for( col in col_inutiles )
  {
    dataset[  ,  paste0(col) := NULL ]
  }

  rm( dtrain, dvalid )
  gc()

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------

correr_todo  <- function( palancas )
{
  
  
  
  
  #cargo el dataset ORIGINAL  
  dataset1  <- fread( "./datasets/paquete_premium_202009_most_important_features_ratio.csv")    ### METO DATASET ARMADO CON PYTHON
  dataset2  <- fread( "./datasets/paquete_premium_202011_most_important_features_ratio.csv")

  dataset   <- rbind( dataset1, dataset2 )
  rm( dataset1, dataset2 )
  gc()

  setorder(  dataset, numero_de_cliente, foto_mes )  #ordeno el dataset

  AgregarMes( dataset )  #agrego el mes del año

  if( length(palancas$variablesdrift) > 0 )   DriftEliminar( dataset, palancas$variablesdrift )

  if( palancas$dummiesNA )  DummiesNA( dataset )  #esta linea debe ir ANTES de Corregir  !!

  if( palancas$corregir )  Corregir( dataset )  #esta linea debe ir DESPUES de  DummiesNA

  if( palancas$nuevasvars )  AgregarVariables( dataset )

  cols_analiticas  <- setdiff( colnames(dataset),  c("numero_de_cliente","foto_mes","mes","clase_ternaria") )

  if( palancas$lag1 )   Lags( dataset, cols_analiticas, 1, palancas$delta1 )
  if( palancas$lag2 )   Lags( dataset, cols_analiticas, 2, palancas$delta2 )
  if( palancas$lag3 )   Lags( dataset, cols_analiticas, 3, palancas$delta3 )
  if( palancas$lag4 )   Lags( dataset, cols_analiticas, 4, palancas$delta4 )
  if( palancas$lag5 )   Lags( dataset, cols_analiticas, 5, palancas$delta5 )
  if( palancas$lag6 )   Lags( dataset, cols_analiticas, 6, palancas$delta6 )

  if( palancas$promedio3 )  Promedios( dataset, cols_analiticas, 3 )
  if( palancas$promedio6 )  Promedios( dataset, cols_analiticas, 6 )

  if( palancas$minimo3 )  Minimos( dataset, cols_analiticas, 3 )
  if( palancas$minimo6 )  Minimos( dataset, cols_analiticas, 6 )

  if( palancas$maximo3 )  Maximos( dataset, cols_analiticas, 3 )
  if( palancas$maximo6 )  Maximos( dataset, cols_analiticas, 6 )

  if( palancas$tendencia6 )  Tendencia( dataset, cols_analiticas)


  if( palancas$canaritosimportancia )  CanaritosImportancia( dataset )



  #dejo la clase como ultimo campo
  nuevo_orden  <- c( setdiff( colnames( dataset ) , "clase_ternaria" ) , "clase_ternaria" )
  setcolorder( dataset, nuevo_orden )

  #Grabo el dataset
  fwrite( dataset,
          paste0( "./datasets/dataset_epic_simple_", palancas$version, ".csv.gz" ),
          logical01 = TRUE,
          sep= "," )

}
#------------------------------------------------------------------------------

#Aqui empieza el programa


correr_todo( palancas )


quit( save="no" )


