#Feature Engineering
#creo nuevas variables dentro del mismo mes
#Condimentar a gusto con nuevas variables

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")



#Establezco el Working Directory
# setwd( "~/buckets/b1/crudoB" )
setwd("C:/Archivos/maestria/dmeyf/")


EnriquecerDataset <- function( dataset , arch_destino, data_sep  )
{
  columnas_originales <-  copy(colnames( dataset ))

  #INICIO de la seccion donde se deben hacer cambios con variables nuevas
  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
  dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  dataset[ , mv_status02       := sum(c(Master_status ,  Visa_status), na.rm= T) ]
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
  dataset[ , mv_Fvencimiento_min         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , mv_Fvencimiento_max         := pmax( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ] #MS
  
  # dataset[ , mv_Finiciomora_min          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  # dataset[ , mv_Finiciomora_max          := pmax( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ] #MS
  
  dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset[ , mv_msaldototal_avg := mean( c(Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ), by =numero_de_cliente  ] #MS
  
  dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , mv_msaldopesos_avg := mean( c(Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ), by =numero_de_cliente  ] #MS
  
  dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset[ , mv_msaldodolares_avg        := mean( c( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ] # MS
  
  dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumostotales     := mv_mconsumosdolares+mv_mconsumospesos  ] #MS
  dataset[ , mv_mconsumostotales_avg     := mean(c(mv_mconsumosdolares+mv_mconsumospesos), na.rm = T )  ] #MS
  
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
  
  dataset[ , mv_delinquency := rowSums( cbind( Master_delinquency,  Visa_delinquency) , na.rm=TRUE ) ] #MS
  dataset[, suma_cons_limi := sum(c(mv_mconsumostotales, mv_mlimitecompra, mv_mpagado))] # MS
  
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
  
  
  # Otras MS
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
  dataset[, rentabilidad_total_2 := sum(c(mcomisiones , mrentabilidad_annual,mactivos_margen,mpasivos_margen), na.rm = T)]
  dataset[, rentabilidad_total_3 := sum(c(mcuentas_saldo , rentabilidad_total_2), na.rm = T)]
  dataset[, rentabilidad_total_4 :=  rentabilidad_total_2/mcuentas_saldo]
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
  
  # Correción de variables por data drifting
  if(data_sep == TRUE){
    dataset[, Master_fultimo_cierre := Master_fultimo_cierre+5]
    dataset[, Visa_fultimo_cierre := Visa_fultimo_cierre+5]    
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

  #FIN de la seccion donde se deben hacer cambios con variables nuevas

  columnas_extendidas <-  copy( setdiff(  colnames(dataset), columnas_originales ) )

  #grabo con nombre extendido
  fwrite( dataset,
          file=arch_destino,
          sep= "," )
}
#------------------------------------------------------------------------------

dir.create( "./datasets/" )


#lectura rapida del dataset  usando fread  de la libreria  data.table
dataset1  <- fread("./datasetsOri/paquete_premium_202009.csv")
dataset2  <- fread("./datasetsOri/paquete_premium_202011.csv")

EnriquecerDataset( dataset1, "./datasets/paquete_premium_202009_ext.csv" , data_sep=T)
EnriquecerDataset( dataset2, "./datasets/paquete_premium_202011_ext.csv" ,data_sep=F)


quit( save="no")
