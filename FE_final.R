# source( "~/labo/src/FeatureEngineering/z815_FE_final.r")
#Necesita para correr en Google Cloud
# 256 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("Rcpp")

require("ranger")
require("randomForest")  #solo se usa para imputar nulos

require("lightgbm")

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})


#------------------------------------------------------------------------------

CorregirCampoMes  <- function( pcampo, pmeses )
{
  tbl <- dataset[  ,  list( "v1" = shift( get(pcampo), 1, type="lag" ),
                            "v2" = shift( get(pcampo), 1, type="lead" )
                         ), 
                   by=numero_de_cliente ]
  
  tbl[ , numero_de_cliente := NULL ]
  tbl[ , promedio := rowMeans( tbl,  na.rm=TRUE ) ]
  
  dataset[ ,
           paste0(pcampo) := ifelse( !(foto_mes %in% pmeses),
                                     get( pcampo),
                                     tbl$promedio ) ]
}
#------------------------------------------------------------------------------

Corregir_MachineLearning  <- function( dataset )
{
  #gc()
  #acomodo los errores del dataset

  dataset[ , na_count_start_ML := rowSums(is.na(dataset)) ]

  dataset[ foto_mes==201901,  ctransferencias_recibidas  := NA ]
  dataset[ foto_mes==201901,  mtransferencias_recibidas  := NA ]

  dataset[ foto_mes==201902,  ctransferencias_recibidas  := NA ]
  dataset[ foto_mes==201902,  mtransferencias_recibidas  := NA ]

  dataset[ foto_mes==201903,  ctransferencias_recibidas  := NA ]
  dataset[ foto_mes==201903,  mtransferencias_recibidas  := NA ]

  dataset[ foto_mes==201904,  ctransferencias_recibidas  := NA ]
  dataset[ foto_mes==201904,  mtransferencias_recibidas  := NA ]
  dataset[ foto_mes==201904,  ctarjeta_visa_debitos_automaticos  :=  NA ]
  dataset[ foto_mes==201904,  mttarjeta_visa_debitos_automaticos := NA ]
  dataset[ foto_mes==201904,  Visa_mfinanciacion_limite := NA ]

  dataset[ foto_mes==201905,  ctransferencias_recibidas  := NA ]
  dataset[ foto_mes==201905,  mtransferencias_recibidas  := NA ]
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

  #dataset[ foto_mes==202001,  cliente_vip   := NA ]

  dataset[ foto_mes==202006,  active_quarter   := NA ]
  #dataset[ foto_mes==202006,  internet   := NA ]
  dataset[ foto_mes==202006,  mrentabilidad   := NA ]
  dataset[ foto_mes==202006,  mrentabilidad_annual   := NA ]
  dataset[ foto_mes==202006,  mcomisiones   := NA ]
  dataset[ foto_mes==202006,  mactivos_margen   := NA ]
  dataset[ foto_mes==202006,  mpasivos_margen   := NA ]
  dataset[ foto_mes==202006,  mcuentas_saldo   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_debito_transacciones  := NA ]
  dataset[ foto_mes==202006,  mautoservicio   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_visa_transacciones   := NA ]
  dataset[ foto_mes==202006,  mtarjeta_visa_consumo   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_master_transacciones  := NA ]
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
  #dataset[ foto_mes==202006,  tmobile_app   := NA ]
  dataset[ foto_mes==202006,  cmobile_app_trx   := NA ]


  #dataset[ foto_mes==202010,  internet  := NA ]
  #dataset[ foto_mes==202011,  internet  := NA ]
  #dataset[ foto_mes==202012,  internet  := NA ]
  #dataset[ foto_mes==202101,  internet  := NA ]
  #dataset[ foto_mes==202102,  internet  := NA ]
  #dataset[ foto_mes==202103,  internet  := NA ]

  #dataset[ foto_mes==202009,  tmobile_app  := NA ]
  #dataset[ foto_mes==202010,  tmobile_app  := NA ]
  #dataset[ foto_mes==202011,  tmobile_app  := NA ]
  #dataset[ foto_mes==202012,  tmobile_app  := NA ]
  #dataset[ foto_mes==202101,  tmobile_app  := NA ]
  #dataset[ foto_mes==202102,  tmobile_app  := NA ]
  #dataset[ foto_mes==202103,  tmobile_app  := NA ]

  dataset[ , na_count_finish_ML := rowSums(is.na(dataset)) ]

}
#------------------------------------------------------------------------------
#Esta es la parte que los alumnos deben desplegar todo su ingenio

AgregarVariables  <- function( dataset )
{
  #gc()

  dataset[ , na_count_start_agregar_vars := rowSums(is.na(dataset)) ]

  #INICIO de la seccion donde se deben hacer cambios con variables nuevas

  #creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
  dataset[  , ctrx_quarter_normalizado := ctrx_quarter ]
  dataset[ cliente_antiguedad==1 , ctrx_quarter_normalizado := ctrx_quarter * 50 ]
  dataset[ cliente_antiguedad==2 , ctrx_quarter_normalizado := ctrx_quarter * 20 ]
  dataset[ cliente_antiguedad==3 , ctrx_quarter_normalizado := ctrx_quarter * 12 ]

  #variable extraida de una tesis de maestria de Irlanda
  dataset[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]

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

  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  ##infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
  ##infinitos_qty  <- sum( unlist( infinitos) )
  ##if( infinitos_qty > 0 )
  ##{
    ##cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <<- NA
  ##}

  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  
  ##nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
  ##nans_qty  <- sum( unlist( nans) )
  ##if( nans_qty > 0 )
  ##{
  ##  cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
  ##  cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
  dataset[mapply(is.nan, dataset)] <<- 0
  ##}

  dataset[ , na_count_finish_agregar_vars := rowSums(is.na(dataset)) ]

}

#------------------------------------------------------------------------------
#se calculan para los 6 meses previos el minimo, maximo y tendencia calculada con cuadrados minimos
#la formula de calculo de la tendencia puede verse en https://stats.libretexts.org/Bookshelves/Introductory_Statistics/Book%3A_Introductory_Statistics_(Shafer_and_Zhang)/10%3A_Correlation_and_Regression/10.04%3A_The_Least_Squares_Regression_Line
#para la maxíma velocidad esta funcion esta escrita en lenguaje C, y no en la porqueria de R o Python

cppFunction('NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde ) 
{
  /* Aqui se cargan los valores para la regresion */
  double  x[100] ;
  double  y[100] ;
  int n = pcolumna.size();
  NumericVector out( 5*n );
  for(int i = 0; i < n; i++)
  {
    //lag
    if( pdesde[i]-1 < i )  out[ i + 4*n ]  =  pcolumna[i-1] ;
    else                   out[ i + 4*n ]  =  NA_REAL ;
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
      out[ i + n ]    =  vmin ;
      out[ i + 2*n ]  =  vmax ;
      out[ i + 3*n ]  =  ysum / libre ;
    }
    else
    {
      out[ i       ]  =  NA_REAL ; 
      out[ i + n   ]  =  NA_REAL ;
      out[ i + 2*n ]  =  NA_REAL ;
      out[ i + 3*n ]  =  NA_REAL ;
    }
  }
  return  out;
}')

#------------------------------------------------------------------------------
#calcula la tendencia de las variables cols de los ultimos 6 meses
#la tendencia es la pendiente de la recta que ajusta por cuadrados minimos
#La funcionalidad de ratioavg es autoria de  Daiana Sparta,  UAustral  2021

TendenciaYmuchomas  <- function( dataset, cols, ventana=6, tendencia=TRUE, minimo=TRUE, maximo=TRUE, promedio=TRUE, 
                                 ratioavg=FALSE, ratiomax=FALSE)
{
  gc()

  dataset[ , na_count_start_tendencia := rowSums(is.na(dataset)) ]

  #Esta es la cantidad de meses que utilizo para la historia
  ventana_regresion  <- ventana

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

    if(tendencia)  dataset[ , paste0( campo, "_tend", ventana) := nueva_col[ (0*last +1):(1*last) ]  ]
    if(minimo)     dataset[ , paste0( campo, "_min", ventana)  := nueva_col[ (1*last +1):(2*last) ]  ]
    if(maximo)     dataset[ , paste0( campo, "_max", ventana)  := nueva_col[ (2*last +1):(3*last) ]  ]
    if(promedio)   dataset[ , paste0( campo, "_avg", ventana)  := nueva_col[ (3*last +1):(4*last) ]  ]
    if(ratioavg)   dataset[ , paste0( campo, "_ratioavg", ventana)  := get(campo) /nueva_col[ (3*last +1):(4*last) ]  ]
    if(ratiomax)   dataset[ , paste0( campo, "_ratiomax", ventana)  := get(campo) /nueva_col[ (2*last +1):(3*last) ]  ]
  }

  dataset[ , na_count_finish_tendencia := rowSums(is.na(dataset)) ]

}
#------------------------------------------------------------------------------
#agrega al dataset nuevas variables {0,1} que provienen de las hojas de un Random Forest

AgregaVarRandomForest  <- function( num.trees, max.depth, min.node.size, mtry)
{
  gc()
  dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

  campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria" ) )

  dataset_rf  <- copy( dataset[ , campos_buenos, with=FALSE] )
  azar  <- runif( nrow(dataset_rf) )
  dataset_rf[ , entrenamiento := as.integer( foto_mes>= 202009 &  foto_mes<= 202101 & ( clase01==1 | azar < 0.10 )) ]

  #imputo los nulos, ya que ranger no acepta nulos
  #Leo Breiman, ¿por que le temias a los nulos?
  dataset_rf  <- randomForest::na.roughfix( dataset_rf )

  campos_buenos  <- setdiff( colnames(dataset_rf), c("clase_ternaria","entrenamiento" ) )
  modelo  <- ranger( formula= "clase01 ~ .",
                     data=  dataset_rf[ entrenamiento==1L, campos_buenos, with=FALSE  ] ,
                     classification= TRUE,
                     probability=   FALSE,
                     num.trees=     num.trees,
                     max.depth=     max.depth,
                     min.node.size= min.node.size,
                     mtry=          mtry
                   )

  rfhojas  <- predict( object= modelo, 
                       data= dataset_rf[ , campos_buenos, with=FALSE ],
                       predict.all= TRUE,    #entrega la prediccion de cada arbol
                       type= "terminalNodes" #entrega el numero de NODO el arbol
                     )

  for( arbol in 1:num.trees )
  {
    hojas_arbol  <- unique(  rfhojas$predictions[  , arbol  ] )

    for( pos in 1:length(hojas_arbol) )
    {
      nodo_id  <- hojas_arbol[ pos ]  #el numero de nodo de la hoja, estan salteados
      dataset[  ,  paste0( "rf_", sprintf( "%03d", arbol ), "_", sprintf( "%03d", nodo_id ) ) := 0L ]

      dataset[ which( rfhojas$predictions[ , arbol] == nodo_id ,  ), 
               paste0( "rf_", sprintf( "%03d", arbol ), "_", sprintf( "%03d", nodo_id ) ) := 1L ]
    }
  }

  rm( dataset_rf )
  dataset[ , clase01 := NULL ]

  gc()
}
#------------------------------------------------------------------------------
VPOS_CORTE  <- c()

fganancia_lgbm_meseta  <- function(probs, datos) 
{
  vlabels  <- get_field(datos, "label")
  vpesos   <- get_field(datos, "weight")

  tbl  <- as.data.table( list( "prob"=probs, "gan"= ifelse( vlabels==1 & vpesos > 1, 78000, -2000 ) ) )

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
#se llama varias veces, luego de agregar muchas variables nuevas, para ir reduciendo la cantidad de variables
# y así hacer lugar a nuevas variables importantes

GVEZ <- 1 

CanaritosAsesinos  <- function( canaritos_ratio=0.2 )
{
  gc()
  dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

  for( i  in 1:(ncol(dataset)*canaritos_ratio))  dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset))]

  campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", "foto_mes" ) )

  azar  <- runif( nrow(dataset) )
  dataset[ , entrenamiento := foto_mes>= 202008 &  foto_mes<= 202111  & ( clase01==1 | azar < 0.10 ) ]

  dtrain  <- lgb.Dataset( data=    data.matrix(  dataset[ entrenamiento==TRUE, campos_buenos, with=FALSE]),
                          label=   dataset[ entrenamiento==TRUE, clase01],
                          weight=  dataset[ entrenamiento==TRUE, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)],
                          free_raw_data= FALSE
                        )

  dvalid  <- lgb.Dataset( data=    data.matrix(  dataset[ foto_mes==202101, campos_buenos, with=FALSE]),
                          label=   dataset[ foto_mes==202101, clase01],
                          weight=  dataset[ foto_mes==202101, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)],
                          free_raw_data= FALSE
                          )


  param <- list( objective= "binary",
                 metric= "custom",
                 first_metric_only= TRUE,
                 boost_from_average= TRUE,
                 feature_pre_filter= FALSE,
                 verbosity= -100,
                 seed= 2345234,
                 max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                 min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                 lambda_l1= 0.0,         #por ahora, lo dejo fijo
                 lambda_l2= 0.0,         #por ahora, lo dejo fijo
                 max_bin= 31,            #por ahora, lo dejo fijo
                 num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                 force_row_wise= TRUE,    #para que los alumnos no se atemoricen con tantos warning
                 learning_rate= 0.065, 
                 feature_fraction= 1.0,   #lo seteo en 1 para que las primeras variables del dataset no se vean opacadas
                 min_data_in_leaf= 260,
                 num_leaves= 60,
                 early_stopping_rounds= 200 )

  modelo  <- lgb.train( data= dtrain,
                        valids= list( valid= dvalid ),
                        eval= fganancia_lgbm_meseta,
                        param= param,
                        verbose= -100 )

  tb_importancia  <- lgb.importance( model= modelo )
  tb_importancia[  , pos := .I ]

fwrite( tb_importancia, 
        file= paste0( "impo_", GVEZ ,".txt"),
        sep= "\t" )

  GVEZ  <<- GVEZ + 1

  umbral  <- tb_importancia[ Feature %like% "canarito", median(pos) + 2*sd(pos) ]  #Atencion corto en la mediana mas DOS desvios!!

  col_utiles  <- tb_importancia[ pos < umbral & !( Feature %like% "canarito"),  Feature ]
  col_utiles  <-  unique( c( col_utiles,  c("numero_de_cliente","foto_mes","clase_ternaria","mes") ) )
  col_inutiles  <- setdiff( colnames(dataset), col_utiles )

  dataset[  ,  (col_inutiles) := NULL ]

}
#------------------------------------------------------------------------------
#agrega para cada columna de cols una nueva variable _rank  que es un numero entre 0 y 1  del ranking de esa variable ese mes

Rankeador  <- function( cols )
{
  gc()
  sufijo  <- "_rank" 

  for( vcol in cols )
  {
     dataset[ , paste0( vcol, sufijo) := frank( get(vcol), ties.method= "random")/ .N, 
                by= foto_mes ]
  }
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

#cargo el dataset
dataset  <- fread( "./datasets/competencia1_historia_2022.csv.gz" )

#Elimino los campos problematicos
dataset[  , internet := NULL ]
dataset[  , tmobile_app := NULL ]

print("canarito at beginning")
print(ncol( dataset )) # 154
print(nrow( dataset )) # 3887334 con todo  ----> 966008 con foto_mes>= 202010
#CanaritosAsesinos( canaritos_ratio = 0.2 )
print(ncol( dataset )) # 38 (ratio 0.2)
print(nrow( dataset ))

#corrijo los  < foto_mes, campo >  que fueron pisados con cero
Corregir_MachineLearning( dataset )  

print("canarito after Corregir_MachineLearning")
print(ncol( dataset ))
print(nrow( dataset ))
#CanaritosAsesinos( canaritos_ratio = 0.1 )
print(ncol( dataset ))
print(nrow( dataset ))

#Agrego las variables manuales al dataset
AgregarVariables( dataset )

print("canarito after AgregarVariables")
print(ncol( dataset ))
print(nrow( dataset ))
#CanaritosAsesinos( canaritos_ratio = 0.1 )
print(ncol( dataset ))
print(nrow( dataset ))

#--------------------------------------
#estas son las columnas a las que se puede agregar lags o media moviles ( todas menos las obvias )
cols_lagueables  <- copy(  setdiff( colnames(dataset), c("numero_de_cliente", "foto_mes", "clase_ternaria")  ) )

#ordeno el dataset por <numero_de_cliente, foto_mes> para poder hacer lags
#  es MUY  importante esta linea
setorder( dataset, numero_de_cliente, foto_mes )

#creo los campos lags de orden 1
dataset[ , paste0( cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= cols_lagueables ]

#agrego los delta lags
for( vcol in cols_lagueables )
{
  dataset[ , paste0(vcol, "_delta1") := get(vcol)  - get(paste0( vcol, "_lag1"))  ]
}

#ordeno el dataset por <numero_de_cliente, foto_mes> para poder hacer lags
#  es MUY  importante esta linea
setorder( dataset, numero_de_cliente, foto_mes )

#creo los campos lags de orden 1
dataset[ , paste0( cols_lagueables, "_lag2") := shift(.SD, 2, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= cols_lagueables ]

#agrego los delta lags
for( vcol in cols_lagueables )
{
  dataset[ , paste0(vcol, "_delta2") := get(vcol)  - get(paste0( vcol, "_lag2"))  ]
}

#ordeno el dataset por <numero_de_cliente, foto_mes> para poder hacer lags
#  es MUY  importante esta linea
setorder( dataset, numero_de_cliente, foto_mes )

#creo los campos lags de orden 1
dataset[ , paste0( cols_lagueables, "_lag3") := shift(.SD, 3, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= cols_lagueables ]

#agrego los delta lags
for( vcol in cols_lagueables )
{
  dataset[ , paste0(vcol, "_delta3") := get(vcol)  - get(paste0( vcol, "_lag3"))  ]
}

#ordeno el dataset por <numero_de_cliente, foto_mes> para poder hacer lags
#  es MUY  importante esta linea
setorder( dataset, numero_de_cliente, foto_mes )

#creo los campos lags de orden 1
dataset[ , paste0( cols_lagueables, "_lag4") := shift(.SD, 4, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= cols_lagueables ]

#agrego los delta lags
for( vcol in cols_lagueables )
{
  dataset[ , paste0(vcol, "_delta4") := get(vcol)  - get(paste0( vcol, "_lag4"))  ]
}

print("recorte para quedarme con mes >= 202008")
dataset = dataset[dataset$foto_mes>= 202006, ]          
## 3887334 rows --> 966008 rows (202010)
##              --> 1279761 rows (202008)

print("canarito after delta4")
print(ncol( dataset ))
print(nrow( dataset ))
#CanaritosAsesinos( canaritos_ratio = 0.1 )
print(ncol( dataset ))
print(nrow( dataset ))

#------------------------------------------------------------------------------
#Agrego variables a partir de las hojas de un Random Forest

AgregaVarRandomForest( num.trees = 40,
                       max.depth = 5,
                       min.node.size = 500,
                       mtry = 15 )

print("canarito after AgregaVarRandomForest")
print(ncol( dataset ))
print(nrow( dataset ))

#--------------------------------------
#agrego las tendencias

TendenciaYmuchomas( dataset, 
                    cols= cols_lagueables,
                    ventana=   6,      # 6 meses de historia
                    tendencia= TRUE,
                    minimo=    TRUE,
                    maximo=    TRUE,
                    promedio=  TRUE,
                    ratioavg=  TRUE,
                    ratiomax=  TRUE  )

print("canarito after TendenciaYmuchomas")
print(ncol( dataset ))
print(nrow( dataset ))

CanaritosAsesinos( canaritos_ratio = 0.3 )
print("canarito after CanaritosAsesinos( canaritos_ratio = 0.3 )")
print(ncol( dataset ))
print(nrow( dataset ))

for (col_name in colnames(dataset)) {
        dataset[[paste("is_null_", col_name, sep="")]] = is.na(dataset[[col_name]])
}

print("canarito after many nulls")
print(ncol( dataset ))
print(nrow( dataset ))

#######
####### a1

#dataset[[paste("is_null_", "Visa_delinquency", sep="")]] = is.na(dataset[["Visa_delinquency"]])

dataset$a1p1 = dataset$ctrx_quarter < 14 # L
dataset$a1p2 = dataset$mcuentas_saldo <= -1256.1 # LL
dataset$a1p3 = dataset$cprestamos_personales < 2 # LLL
dataset$a1p4 = dataset$mcaja_ahorro < 2601.1 # LRL
dataset$a1p5 = dataset$Visa_status >= 8 # RL
dataset$a1p6 = dataset$is_null_Visa_delinquency = 0 # RLL
dataset$a1p7 = dataset$ctrx_quarter < 38 # RRL

dataset$a1p8 = dataset$a1p1 & dataset$a1p2
dataset$a1p10 = dataset$a1p1 & dataset$a1p2 & dataset$a1p3
dataset$a1p11 = dataset$a1p1 & dataset$a1p2 & !dataset$a1p3
dataset$a1p9 = dataset$a1p1 & !dataset$a1p2 
dataset$a1p12 = dataset$a1p1 & !dataset$a1p2 & dataset$a1p4
dataset$a1p13 = dataset$a1p1 & !dataset$a1p2 & !dataset$a1p4

dataset$a1p14 = !dataset$a1p1
dataset$a1p15 = !dataset$a1p1 & dataset$a1p5
dataset$a1p16 = !dataset$a1p1 & !dataset$a1p5
dataset$a1p17 = !dataset$a1p1 & dataset$a1p5 & dataset$a1p6
dataset$a1p18 = !dataset$a1p1 & dataset$a1p5 & !dataset$a1p6
dataset$a1p19 = !dataset$a1p1 & !dataset$a1p5 & dataset$a1p7
dataset$a1p20 = !dataset$a1p1 & !dataset$a1p5 & !dataset$a1p7

#######
####### a2

dataset$a2p1 = dataset$active_quarter = 0 # L
dataset$a2p2 = dataset$mcuentas_saldo < -1558.3 # LL
dataset$a2p3 = dataset$cdescubierto_preacordado = 0  # LLL
dataset$a2p4 = dataset$cdescubierto_preacordado = 0 # LRL
dataset$a2p5 = dataset$Visa_status >= 8 # RL
dataset$a2p6 = dataset$is_null_Visa_delinquency = 0 # RLL
dataset$a2p7 = dataset$mpasivos_margen < 112.5 # RRL

dataset$a2p8 = dataset$a2p1 & dataset$a2p2
dataset$a2p10 = dataset$a2p1 & dataset$a2p2 & dataset$a2p3
dataset$a2p11 = dataset$a2p1 & dataset$a2p2 & !dataset$a2p3
dataset$a2p9 = dataset$a2p1 & !dataset$a2p2 
dataset$a2p12 = dataset$a2p1 & !dataset$a2p2 & dataset$a2p4
dataset$a2p13 = dataset$a2p1 & !dataset$a2p2 & !dataset$a2p4

dataset$a2p14 = !dataset$a2p1
dataset$a2p15 = !dataset$a2p1 & dataset$a2p5
dataset$a2p16 = !dataset$a2p1 & !dataset$a2p5
dataset$a2p17 = !dataset$a2p1 & dataset$a2p5 & dataset$a2p6
dataset$a2p18 = !dataset$a2p1 & dataset$a2p5 & !dataset$a2p6
dataset$a2p19 = !dataset$a2p1 & !dataset$a2p5 & dataset$a2p7
dataset$a2p20 = !dataset$a2p1 & !dataset$a2p5 & !dataset$a2p7

#######
####### a3

dataset$a3p1 = dataset$mcaja_ahorro < 259.94 # L
dataset$a3p2 = dataset$mtarjeta_visa_consumo < 857.2 # LL
dataset$a3p3 = dataset$mprestamos_personales < 14858  # LLL
dataset$a3p4 = dataset$mcuenta_corriente_adicional < -21950 # LRL
dataset$a3p5 = dataset$Visa_status >= 8 # RL
dataset$a3p6 = dataset$Master_status >= 8 # RLL
dataset$a3p7 = dataset$mpasivos_margen < 118.53 # RRL

dataset$a3p8 = dataset$a3p1 & dataset$a3p2
dataset$a3p10 = dataset$a3p1 & dataset$a3p2 & dataset$a3p3
dataset$a3p11 = dataset$a3p1 & dataset$a3p2 & !dataset$a3p3
dataset$a3p9 = dataset$a3p1 & !dataset$a3p2 
dataset$a3p12 = dataset$a3p1 & !dataset$a3p2 & dataset$a3p4
dataset$a3p13 = dataset$a3p1 & !dataset$a3p2 & !dataset$a3p4

dataset$a3p14 = !dataset$a3p1
dataset$a3p15 = !dataset$a3p1 & dataset$a3p5
dataset$a3p16 = !dataset$a3p1 & !dataset$a3p5
dataset$a3p17 = !dataset$a3p1 & dataset$a3p5 & dataset$a3p6
dataset$a3p18 = !dataset$a3p1 & dataset$a3p5 & !dataset$a3p6
dataset$a3p19 = !dataset$a3p1 & !dataset$a3p5 & dataset$a3p7
dataset$a3p20 = !dataset$a3p1 & !dataset$a3p5 & !dataset$a3p7

#######
#######

dataset$a4p1 = dataset$mpasivos_margen < 49.83 # L
dataset$a4p2 = dataset$mtarjeta_visa_consumo < 3199 # LL
dataset$a4p3 = dataset$mprestamos_personales < 15486  # LLL
dataset$a4p4 = dataset$ctarjeta_debito_transacciones < 1 # LRL
dataset$a4p5 = dataset$Visa_status >= 8 # RL
dataset$a4p6 = dataset$is_null_Visa_delinquency = 0 # RLL
dataset$a4p7 = dataset$mpasivos_margen < 219.76 # RRL

dataset$a4p8 = dataset$a4p1 & dataset$a4p2
dataset$a4p10 = dataset$a4p1 & dataset$a4p2 & dataset$a4p3
dataset$a4p11 = dataset$a4p1 & dataset$a4p2 & !dataset$a4p3
dataset$a4p9 = dataset$a4p1 & !dataset$a4p2 
dataset$a4p12 = dataset$a4p1 & !dataset$a4p2 & dataset$a4p4
dataset$a4p13 = dataset$a4p1 & !dataset$a4p2 & !dataset$a4p4

dataset$a4p14 = !dataset$a4p1
dataset$a4p15 = !dataset$a4p1 & dataset$a4p5
dataset$a4p16 = !dataset$a4p1 & !dataset$a4p5
dataset$a4p17 = !dataset$a4p1 & dataset$a4p5 & dataset$a4p6
dataset$a4p18 = !dataset$a4p1 & dataset$a4p5 & !dataset$a4p6
dataset$a4p19 = !dataset$a4p1 & !dataset$a4p5 & dataset$a4p7
dataset$a4p20 = !dataset$a4p1 & !dataset$a4p5 & !dataset$a4p7

#######
#######

dataset$a5p1 = dataset$mtarjeta_visa_consumo < 699.89 # L
dataset$a5p2 = dataset$cpayroll_trx < 1 # LL
dataset$a5p3 = dataset$mcuentas_saldo < 276.83  # LLL
dataset$a5p4 = dataset$mcuenta_corriente < -577260 # LRL
dataset$a5p5 = dataset$mautoservicio < 703.81 # RL
dataset$a5p6 = dataset$mtarjeta_visa_consumo < 7255.3 # RLL
dataset$a5p7 = dataset$mpayroll < 6805.2 # RRL

dataset$a5p8 = dataset$a5p1 & dataset$a5p2
dataset$a5p10 = dataset$a5p1 & dataset$a5p2 & dataset$a5p3
dataset$a5p11 = dataset$a5p1 & dataset$a5p2 & !dataset$a5p3
dataset$a5p9 = dataset$a5p1 & !dataset$a5p2 
dataset$a5p12 = dataset$a5p1 & !dataset$a5p2 & dataset$a5p4
dataset$a5p13 = dataset$a5p1 & !dataset$a5p2 & !dataset$a5p4

dataset$a5p14 = !dataset$a5p1
dataset$a5p15 = !dataset$a5p1 & dataset$a5p5
dataset$a5p16 = !dataset$a5p1 & !dataset$a5p5
dataset$a5p17 = !dataset$a5p1 & dataset$a5p5 & dataset$a5p6
dataset$a5p18 = !dataset$a5p1 & dataset$a5p5 & !dataset$a5p6
dataset$a5p19 = !dataset$a5p1 & !dataset$a5p5 & dataset$a5p7
dataset$a5p20 = !dataset$a5p1 & !dataset$a5p5 & !dataset$a5p7

#######
#######

dataset$a6p1 = dataset$ctarjeta_visa_transacciones < 1 # L
dataset$a6p2 = dataset$cpayroll_trx < 1 # LL
dataset$a6p3 = dataset$mcuentas_saldo < 319.78  # LLL
dataset$a6p4 = dataset$mcuenta_corriente < -577260 # LRL
dataset$a6p5 = dataset$mautoservicio < 842.4 # RL
dataset$a6p6 = dataset$Visa_msaldototal < 3515.9 # RLL
dataset$a6p7 = dataset$mpayroll < 8222.1 # RRL

dataset$a6p8 = dataset$a6p1 & dataset$a6p2
dataset$a6p10 = dataset$a6p1 & dataset$a6p2 & dataset$a6p3
dataset$a6p11 = dataset$a6p1 & dataset$a6p2 & !dataset$a6p3
dataset$a6p9 = dataset$a6p1 & !dataset$a6p2 
dataset$a6p12 = dataset$a6p1 & !dataset$a6p2 & dataset$a6p4
dataset$a6p13 = dataset$a6p1 & !dataset$a6p2 & !dataset$a6p4

dataset$a6p14 = !dataset$a6p1
dataset$a6p15 = !dataset$a6p1 & dataset$a6p5
dataset$a6p16 = !dataset$a6p1 & !dataset$a6p5
dataset$a6p17 = !dataset$a6p1 & dataset$a6p5 & dataset$a6p6
dataset$a6p18 = !dataset$a6p1 & dataset$a6p5 & !dataset$a6p6
dataset$a6p19 = !dataset$a6p1 & !dataset$a6p5 & dataset$a6p7
dataset$a6p20 = !dataset$a6p1 & !dataset$a6p5 & !dataset$a6p7

######
######

#dataset[[paste("is_null_", "Visa_mconsumospesos", sep="")]] = is.na(dataset[["Visa_mconsumospesos"]])

dataset$a7p1 = dataset$is_null_Visa_mconsumospesos = 1 # L
dataset$a7p2 = dataset$cpayroll_trx < 1 # LL
dataset$a7p3 = dataset$mcuentas_saldo < 158.39  # LLL
dataset$a7p4 = dataset$mcuenta_corriente < -577260 # LRL
dataset$a7p5 = dataset$Visa_status >= 8 # RL
#dataset$a7p6 = dataset # RLL
dataset$a7p7 = dataset$Visa_msaldototal < 3515.4 # RRL

dataset$a7p8 = dataset$a7p1 & dataset$a7p2
dataset$a7p10 = dataset$a7p1 & dataset$a7p2 & dataset$a7p3
dataset$a7p11 = dataset$a7p1 & dataset$a7p2 & !dataset$a7p3
dataset$a7p9 = dataset$a7p1 & !dataset$a7p2 
dataset$a7p12 = dataset$a7p1 & !dataset$a7p2 & dataset$a7p4
dataset$a7p13 = dataset$a7p1 & !dataset$a7p2 & !dataset$a7p4

dataset$a7p14 = !dataset$a7p1
dataset$a7p15 = !dataset$a7p1 & dataset$a7p5
dataset$a7p16 = !dataset$a7p1 & !dataset$a7p5
#dataset$a7p17 = !dataset$a7p1 & dataset$a7p5 & dataset$a7p6
#dataset$a7p18 = !dataset$a7p1 & dataset$a7p5 & !dataset$a7p6
dataset$a7p19 = !dataset$a7p1 & !dataset$a7p5 & dataset$a7p7
dataset$a7p20 = !dataset$a7p1 & !dataset$a7p5 & !dataset$a7p7

######
######

#dataset[[paste("is_null_", "Visa_mconsumosdolares", sep="")]] = is.na(dataset[["Visa_mconsumosdolares"]])

dataset$a8p1 = dataset$is_null_Visa_mconsumosdolares = 1 # L
dataset$a8p2 = dataset$cpayroll_trx < 1 # LL
dataset$a8p3 = dataset$mcuentas_saldo < 158.39  # LLL
dataset$a8p4 = dataset$mcuenta_corriente < -577260 # LRL
dataset$a8p5 = dataset$Visa_status >= 8 # RL
#dataset$a8p6 = dataset # RLL
dataset$a8p7 = dataset$Visa_msaldototal < 3515.4 # RRL

dataset$a8p8 = dataset$a8p1 & dataset$a8p2
dataset$a8p10 = dataset$a8p1 & dataset$a8p2 & dataset$a8p3
dataset$a8p11 = dataset$a8p1 & dataset$a8p2 & !dataset$a8p3
dataset$a8p9 = dataset$a8p1 & !dataset$a8p2 
dataset$a8p12 = dataset$a8p1 & !dataset$a8p2 & dataset$a8p4
dataset$a8p13 = dataset$a8p1 & !dataset$a8p2 & !dataset$a8p4

dataset$a8p14 = !dataset$a8p1
dataset$a8p15 = !dataset$a8p1 & dataset$a8p5
dataset$a8p16 = !dataset$a8p1 & !dataset$a8p5
#dataset$a8p17 = !dataset$a8p1 & dataset$a8p5 & dataset$a8p6
#dataset$a8p18 = !dataset$a8p1 & dataset$a8p5 & !dataset$a8p6
dataset$a8p19 = !dataset$a8p1 & !dataset$a8p5 & dataset$a8p7
dataset$a8p20 = !dataset$a8p1 & !dataset$a8p5 & !dataset$a8p7

#######
######

dataset$a9p1 = dataset$is_null_Visa_mconsumosdolares = 1 # L
dataset$a9p2 = dataset$cpayroll_trx < 1 # LL
dataset$a9p3 = dataset$mcuentas_saldo < 158.39  # LLL
dataset$a9p4 = dataset$mcuenta_corriente < -577260 # LRL
dataset$a9p5 = dataset$Visa_status >= 8 # RL
#dataset$a9p6 = dataset # RLL
dataset$a9p7 = dataset$Visa_msaldototal < 3515.4 # RRL

dataset$a9p8 = dataset$a9p1 & dataset$a9p2
dataset$a9p10 = dataset$a9p1 & dataset$a9p2 & dataset$a9p3
dataset$a9p11 = dataset$a9p1 & dataset$a9p2 & !dataset$a9p3
dataset$a9p9 = dataset$a9p1 & !dataset$a9p2 
dataset$a9p12 = dataset$a9p1 & !dataset$a9p2 & dataset$a9p4
dataset$a9p13 = dataset$a9p1 & !dataset$a9p2 & !dataset$a9p4

dataset$a9p14 = !dataset$a9p1
dataset$a9p15 = !dataset$a9p1 & dataset$a9p5
dataset$a9p16 = !dataset$a9p1 & !dataset$a9p5
#dataset$a9p17 = !dataset$a9p1 & dataset$a9p5 & dataset$a9p6
#dataset$a9p18 = !dataset$a9p1 & dataset$a9p5 & !dataset$a9p6
dataset$a9p19 = !dataset$a9p1 & !dataset$a9p5 & dataset$a9p7
dataset$a9p20 = !dataset$a9p1 & !dataset$a9p5 & !dataset$a9p7

print("canarito after pseude random forest")
print(ncol( dataset ))
print(nrow( dataset ))
#CanaritosAsesinos( canaritos_ratio = 0.1 )
print(ncol( dataset ))
print(nrow( dataset ))

gc()

#------------------------------------------------------------------------------

print(ncol( dataset ))
print(nrow( dataset ))
print("MUCHOS COCIENTES!!")

dataset$mv_fechaalta_delta2_divided_by_mv_fechaalta_tend6 = dataset$mv_fechaalta_delta2 / dataset$mv_fechaalta_tend6
dataset$mv_fechaalta_delta2_divided_by_Visa_fechaalta_delta2 = dataset$mv_fechaalta_delta2 / dataset$Visa_fechaalta_delta2
dataset$mv_fechaalta_delta2_divided_by_cmobile_app_trx_lag4 = dataset$mv_fechaalta_delta2 / dataset$cmobile_app_trx_lag4
dataset$mv_fechaalta_delta2_divided_by_Master_fultimo_cierre = dataset$mv_fechaalta_delta2 / dataset$Master_fultimo_cierre
dataset$mv_fechaalta_delta2_divided_by_a1p20 = dataset$mv_fechaalta_delta2 / dataset$a1p20
dataset$mv_fechaalta_delta2_divided_by_na_count_finish_ML_avg6 = dataset$mv_fechaalta_delta2 / dataset$na_count_finish_ML_avg6
dataset$mv_fechaalta_delta2_divided_by_mv_Fvencimiento_delta2 = dataset$mv_fechaalta_delta2 / dataset$mv_Fvencimiento_delta2
dataset$mv_fechaalta_delta2_divided_by_cmobile_app_trx_lag2 = dataset$mv_fechaalta_delta2 / dataset$cmobile_app_trx_lag2
dataset$mv_fechaalta_delta2_divided_by_Visa_fultimo_cierre = dataset$mv_fechaalta_delta2 / dataset$Visa_fultimo_cierre
dataset$mv_fechaalta_delta2_divided_by_mv_status01_delta2 = dataset$mv_fechaalta_delta2 / dataset$mv_status01_delta2
dataset$mv_fechaalta_delta2_divided_by_a5p8 = dataset$mv_fechaalta_delta2 / dataset$a5p8
dataset$mv_fechaalta_delta2_divided_by_a5p10 = dataset$mv_fechaalta_delta2 / dataset$a5p10
dataset$mv_fechaalta_delta2_divided_by_cmobile_app_trx = dataset$mv_fechaalta_delta2 / dataset$cmobile_app_trx
dataset$mv_fechaalta_delta2_divided_by_na_count_finish_ML_tend6 = dataset$mv_fechaalta_delta2 / dataset$na_count_finish_ML_tend6
dataset$mv_fechaalta_delta2_divided_by_mv_fultimo_cierre_delta2 = dataset$mv_fechaalta_delta2 / dataset$mv_fultimo_cierre_delta2
dataset$mv_fechaalta_delta2_divided_by_a4p10 = dataset$mv_fechaalta_delta2 / dataset$a4p10
dataset$mv_fechaalta_delta2_divided_by_Master_fultimo_cierre_lag1 = dataset$mv_fechaalta_delta2 / dataset$Master_fultimo_cierre_lag1
dataset$mv_fechaalta_delta2_divided_by_ctrx_quarter_normalizado = dataset$mv_fechaalta_delta2 / dataset$ctrx_quarter_normalizado
dataset$mv_fechaalta_delta2_divided_by_cpayroll_trx = dataset$mv_fechaalta_delta2 / dataset$cpayroll_trx
dataset$mv_fechaalta_delta2_divided_by_cmobile_app_trx_lag3 = dataset$mv_fechaalta_delta2 / dataset$cmobile_app_trx_lag3
dataset$mv_fechaalta_delta2_divided_by_Master_fechaalta_delta2 = dataset$mv_fechaalta_delta2 / dataset$Master_fechaalta_delta2
dataset$mv_fechaalta_delta2_divided_by_ccomisiones_mantenimiento_delta2 = dataset$mv_fechaalta_delta2 / dataset$ccomisiones_mantenimiento_delta2
dataset$mv_fechaalta_delta2_divided_by_rf_040_044 = dataset$mv_fechaalta_delta2 / dataset$rf_040_044
dataset$mv_fechaalta_delta2_divided_by_mprestamos_personales = dataset$mv_fechaalta_delta2 / dataset$mprestamos_personales
dataset$mv_fechaalta_delta2_divided_by_Master_fultimo_cierre_avg6 = dataset$mv_fechaalta_delta2 / dataset$Master_fultimo_cierre_avg6
dataset$mv_fechaalta_delta2_divided_by_Visa_fultimo_cierre_avg6 = dataset$mv_fechaalta_delta2 / dataset$Visa_fultimo_cierre_avg6
dataset$mv_fechaalta_delta2_divided_by_mcuentas_saldo = dataset$mv_fechaalta_delta2 / dataset$mcuentas_saldo
dataset$mv_fechaalta_delta2_divided_by_Visa_fultimo_cierre_lag1 = dataset$mv_fechaalta_delta2 / dataset$Visa_fultimo_cierre_lag1
dataset$mv_fechaalta_delta2_divided_by_a5p16 = dataset$mv_fechaalta_delta2 / dataset$a5p16
dataset$mv_fechaalta_delta2_divided_by_a3p10 = dataset$mv_fechaalta_delta2 / dataset$a3p10
dataset$mv_fechaalta_delta2_divided_by_Visa_fechaalta_delta3 = dataset$mv_fechaalta_delta2 / dataset$Visa_fechaalta_delta3
dataset$mv_fechaalta_delta2_divided_by_ctrx_quarter = dataset$mv_fechaalta_delta2 / dataset$ctrx_quarter
dataset$mv_fechaalta_delta2_divided_by_mactivos_margen = dataset$mv_fechaalta_delta2 / dataset$mactivos_margen
dataset$mv_fechaalta_tend6_divided_by_Visa_fechaalta_delta2 = dataset$mv_fechaalta_tend6 / dataset$Visa_fechaalta_delta2
dataset$mv_fechaalta_tend6_divided_by_cmobile_app_trx_lag4 = dataset$mv_fechaalta_tend6 / dataset$cmobile_app_trx_lag4
dataset$mv_fechaalta_tend6_divided_by_Master_fultimo_cierre = dataset$mv_fechaalta_tend6 / dataset$Master_fultimo_cierre
dataset$mv_fechaalta_tend6_divided_by_a1p20 = dataset$mv_fechaalta_tend6 / dataset$a1p20
dataset$mv_fechaalta_tend6_divided_by_na_count_finish_ML_avg6 = dataset$mv_fechaalta_tend6 / dataset$na_count_finish_ML_avg6
dataset$mv_fechaalta_tend6_divided_by_mv_Fvencimiento_delta2 = dataset$mv_fechaalta_tend6 / dataset$mv_Fvencimiento_delta2
dataset$mv_fechaalta_tend6_divided_by_cmobile_app_trx_lag2 = dataset$mv_fechaalta_tend6 / dataset$cmobile_app_trx_lag2
dataset$mv_fechaalta_tend6_divided_by_Visa_fultimo_cierre = dataset$mv_fechaalta_tend6 / dataset$Visa_fultimo_cierre
dataset$mv_fechaalta_tend6_divided_by_mv_status01_delta2 = dataset$mv_fechaalta_tend6 / dataset$mv_status01_delta2
dataset$mv_fechaalta_tend6_divided_by_a5p8 = dataset$mv_fechaalta_tend6 / dataset$a5p8
dataset$mv_fechaalta_tend6_divided_by_a5p10 = dataset$mv_fechaalta_tend6 / dataset$a5p10
dataset$mv_fechaalta_tend6_divided_by_cmobile_app_trx = dataset$mv_fechaalta_tend6 / dataset$cmobile_app_trx
dataset$mv_fechaalta_tend6_divided_by_na_count_finish_ML_tend6 = dataset$mv_fechaalta_tend6 / dataset$na_count_finish_ML_tend6
dataset$mv_fechaalta_tend6_divided_by_mv_fultimo_cierre_delta2 = dataset$mv_fechaalta_tend6 / dataset$mv_fultimo_cierre_delta2
dataset$mv_fechaalta_tend6_divided_by_a4p10 = dataset$mv_fechaalta_tend6 / dataset$a4p10
dataset$mv_fechaalta_tend6_divided_by_Master_fultimo_cierre_lag1 = dataset$mv_fechaalta_tend6 / dataset$Master_fultimo_cierre_lag1
dataset$mv_fechaalta_tend6_divided_by_ctrx_quarter_normalizado = dataset$mv_fechaalta_tend6 / dataset$ctrx_quarter_normalizado
dataset$mv_fechaalta_tend6_divided_by_cpayroll_trx = dataset$mv_fechaalta_tend6 / dataset$cpayroll_trx
dataset$mv_fechaalta_tend6_divided_by_cmobile_app_trx_lag3 = dataset$mv_fechaalta_tend6 / dataset$cmobile_app_trx_lag3
dataset$mv_fechaalta_tend6_divided_by_Master_fechaalta_delta2 = dataset$mv_fechaalta_tend6 / dataset$Master_fechaalta_delta2
dataset$mv_fechaalta_tend6_divided_by_ccomisiones_mantenimiento_delta2 = dataset$mv_fechaalta_tend6 / dataset$ccomisiones_mantenimiento_delta2
dataset$mv_fechaalta_tend6_divided_by_rf_040_044 = dataset$mv_fechaalta_tend6 / dataset$rf_040_044
dataset$mv_fechaalta_tend6_divided_by_mprestamos_personales = dataset$mv_fechaalta_tend6 / dataset$mprestamos_personales
dataset$mv_fechaalta_tend6_divided_by_Master_fultimo_cierre_avg6 = dataset$mv_fechaalta_tend6 / dataset$Master_fultimo_cierre_avg6
dataset$mv_fechaalta_tend6_divided_by_Visa_fultimo_cierre_avg6 = dataset$mv_fechaalta_tend6 / dataset$Visa_fultimo_cierre_avg6
dataset$mv_fechaalta_tend6_divided_by_mcuentas_saldo = dataset$mv_fechaalta_tend6 / dataset$mcuentas_saldo
dataset$mv_fechaalta_tend6_divided_by_Visa_fultimo_cierre_lag1 = dataset$mv_fechaalta_tend6 / dataset$Visa_fultimo_cierre_lag1
dataset$mv_fechaalta_tend6_divided_by_a5p16 = dataset$mv_fechaalta_tend6 / dataset$a5p16
dataset$mv_fechaalta_tend6_divided_by_a3p10 = dataset$mv_fechaalta_tend6 / dataset$a3p10
dataset$mv_fechaalta_tend6_divided_by_Visa_fechaalta_delta3 = dataset$mv_fechaalta_tend6 / dataset$Visa_fechaalta_delta3
dataset$mv_fechaalta_tend6_divided_by_ctrx_quarter = dataset$mv_fechaalta_tend6 / dataset$ctrx_quarter
dataset$mv_fechaalta_tend6_divided_by_mactivos_margen = dataset$mv_fechaalta_tend6 / dataset$mactivos_margen
dataset$Visa_fechaalta_delta2_divided_by_cmobile_app_trx_lag4 = dataset$Visa_fechaalta_delta2 / dataset$cmobile_app_trx_lag4
dataset$Visa_fechaalta_delta2_divided_by_Master_fultimo_cierre = dataset$Visa_fechaalta_delta2 / dataset$Master_fultimo_cierre
dataset$Visa_fechaalta_delta2_divided_by_a1p20 = dataset$Visa_fechaalta_delta2 / dataset$a1p20
dataset$Visa_fechaalta_delta2_divided_by_na_count_finish_ML_avg6 = dataset$Visa_fechaalta_delta2 / dataset$na_count_finish_ML_avg6
dataset$Visa_fechaalta_delta2_divided_by_mv_Fvencimiento_delta2 = dataset$Visa_fechaalta_delta2 / dataset$mv_Fvencimiento_delta2
dataset$Visa_fechaalta_delta2_divided_by_cmobile_app_trx_lag2 = dataset$Visa_fechaalta_delta2 / dataset$cmobile_app_trx_lag2
dataset$Visa_fechaalta_delta2_divided_by_Visa_fultimo_cierre = dataset$Visa_fechaalta_delta2 / dataset$Visa_fultimo_cierre
dataset$Visa_fechaalta_delta2_divided_by_mv_status01_delta2 = dataset$Visa_fechaalta_delta2 / dataset$mv_status01_delta2
dataset$Visa_fechaalta_delta2_divided_by_a5p8 = dataset$Visa_fechaalta_delta2 / dataset$a5p8
dataset$Visa_fechaalta_delta2_divided_by_a5p10 = dataset$Visa_fechaalta_delta2 / dataset$a5p10
dataset$Visa_fechaalta_delta2_divided_by_cmobile_app_trx = dataset$Visa_fechaalta_delta2 / dataset$cmobile_app_trx
dataset$Visa_fechaalta_delta2_divided_by_na_count_finish_ML_tend6 = dataset$Visa_fechaalta_delta2 / dataset$na_count_finish_ML_tend6
dataset$Visa_fechaalta_delta2_divided_by_mv_fultimo_cierre_delta2 = dataset$Visa_fechaalta_delta2 / dataset$mv_fultimo_cierre_delta2
dataset$Visa_fechaalta_delta2_divided_by_a4p10 = dataset$Visa_fechaalta_delta2 / dataset$a4p10
dataset$Visa_fechaalta_delta2_divided_by_Master_fultimo_cierre_lag1 = dataset$Visa_fechaalta_delta2 / dataset$Master_fultimo_cierre_lag1
dataset$Visa_fechaalta_delta2_divided_by_ctrx_quarter_normalizado = dataset$Visa_fechaalta_delta2 / dataset$ctrx_quarter_normalizado
dataset$Visa_fechaalta_delta2_divided_by_cpayroll_trx = dataset$Visa_fechaalta_delta2 / dataset$cpayroll_trx
dataset$Visa_fechaalta_delta2_divided_by_cmobile_app_trx_lag3 = dataset$Visa_fechaalta_delta2 / dataset$cmobile_app_trx_lag3
dataset$Visa_fechaalta_delta2_divided_by_Master_fechaalta_delta2 = dataset$Visa_fechaalta_delta2 / dataset$Master_fechaalta_delta2
dataset$Visa_fechaalta_delta2_divided_by_ccomisiones_mantenimiento_delta2 = dataset$Visa_fechaalta_delta2 / dataset$ccomisiones_mantenimiento_delta2
dataset$Visa_fechaalta_delta2_divided_by_rf_040_044 = dataset$Visa_fechaalta_delta2 / dataset$rf_040_044
dataset$Visa_fechaalta_delta2_divided_by_mprestamos_personales = dataset$Visa_fechaalta_delta2 / dataset$mprestamos_personales
dataset$Visa_fechaalta_delta2_divided_by_Master_fultimo_cierre_avg6 = dataset$Visa_fechaalta_delta2 / dataset$Master_fultimo_cierre_avg6
dataset$Visa_fechaalta_delta2_divided_by_Visa_fultimo_cierre_avg6 = dataset$Visa_fechaalta_delta2 / dataset$Visa_fultimo_cierre_avg6
dataset$Visa_fechaalta_delta2_divided_by_mcuentas_saldo = dataset$Visa_fechaalta_delta2 / dataset$mcuentas_saldo
dataset$Visa_fechaalta_delta2_divided_by_Visa_fultimo_cierre_lag1 = dataset$Visa_fechaalta_delta2 / dataset$Visa_fultimo_cierre_lag1
dataset$Visa_fechaalta_delta2_divided_by_a5p16 = dataset$Visa_fechaalta_delta2 / dataset$a5p16
dataset$Visa_fechaalta_delta2_divided_by_a3p10 = dataset$Visa_fechaalta_delta2 / dataset$a3p10
dataset$Visa_fechaalta_delta2_divided_by_Visa_fechaalta_delta3 = dataset$Visa_fechaalta_delta2 / dataset$Visa_fechaalta_delta3
dataset$Visa_fechaalta_delta2_divided_by_ctrx_quarter = dataset$Visa_fechaalta_delta2 / dataset$ctrx_quarter
dataset$Visa_fechaalta_delta2_divided_by_mactivos_margen = dataset$Visa_fechaalta_delta2 / dataset$mactivos_margen
dataset$cmobile_app_trx_lag4_divided_by_Master_fultimo_cierre = dataset$cmobile_app_trx_lag4 / dataset$Master_fultimo_cierre
dataset$cmobile_app_trx_lag4_divided_by_a1p20 = dataset$cmobile_app_trx_lag4 / dataset$a1p20
dataset$cmobile_app_trx_lag4_divided_by_na_count_finish_ML_avg6 = dataset$cmobile_app_trx_lag4 / dataset$na_count_finish_ML_avg6
dataset$cmobile_app_trx_lag4_divided_by_mv_Fvencimiento_delta2 = dataset$cmobile_app_trx_lag4 / dataset$mv_Fvencimiento_delta2
dataset$cmobile_app_trx_lag4_divided_by_cmobile_app_trx_lag2 = dataset$cmobile_app_trx_lag4 / dataset$cmobile_app_trx_lag2
dataset$cmobile_app_trx_lag4_divided_by_Visa_fultimo_cierre = dataset$cmobile_app_trx_lag4 / dataset$Visa_fultimo_cierre
dataset$cmobile_app_trx_lag4_divided_by_mv_status01_delta2 = dataset$cmobile_app_trx_lag4 / dataset$mv_status01_delta2
dataset$cmobile_app_trx_lag4_divided_by_a5p8 = dataset$cmobile_app_trx_lag4 / dataset$a5p8
dataset$cmobile_app_trx_lag4_divided_by_a5p10 = dataset$cmobile_app_trx_lag4 / dataset$a5p10
dataset$cmobile_app_trx_lag4_divided_by_cmobile_app_trx = dataset$cmobile_app_trx_lag4 / dataset$cmobile_app_trx
dataset$cmobile_app_trx_lag4_divided_by_na_count_finish_ML_tend6 = dataset$cmobile_app_trx_lag4 / dataset$na_count_finish_ML_tend6
dataset$cmobile_app_trx_lag4_divided_by_mv_fultimo_cierre_delta2 = dataset$cmobile_app_trx_lag4 / dataset$mv_fultimo_cierre_delta2
dataset$cmobile_app_trx_lag4_divided_by_a4p10 = dataset$cmobile_app_trx_lag4 / dataset$a4p10
dataset$cmobile_app_trx_lag4_divided_by_Master_fultimo_cierre_lag1 = dataset$cmobile_app_trx_lag4 / dataset$Master_fultimo_cierre_lag1
dataset$cmobile_app_trx_lag4_divided_by_ctrx_quarter_normalizado = dataset$cmobile_app_trx_lag4 / dataset$ctrx_quarter_normalizado
dataset$cmobile_app_trx_lag4_divided_by_cpayroll_trx = dataset$cmobile_app_trx_lag4 / dataset$cpayroll_trx
dataset$cmobile_app_trx_lag4_divided_by_cmobile_app_trx_lag3 = dataset$cmobile_app_trx_lag4 / dataset$cmobile_app_trx_lag3
dataset$cmobile_app_trx_lag4_divided_by_Master_fechaalta_delta2 = dataset$cmobile_app_trx_lag4 / dataset$Master_fechaalta_delta2
dataset$cmobile_app_trx_lag4_divided_by_ccomisiones_mantenimiento_delta2 = dataset$cmobile_app_trx_lag4 / dataset$ccomisiones_mantenimiento_delta2
dataset$cmobile_app_trx_lag4_divided_by_rf_040_044 = dataset$cmobile_app_trx_lag4 / dataset$rf_040_044
dataset$cmobile_app_trx_lag4_divided_by_mprestamos_personales = dataset$cmobile_app_trx_lag4 / dataset$mprestamos_personales
dataset$cmobile_app_trx_lag4_divided_by_Master_fultimo_cierre_avg6 = dataset$cmobile_app_trx_lag4 / dataset$Master_fultimo_cierre_avg6
dataset$cmobile_app_trx_lag4_divided_by_Visa_fultimo_cierre_avg6 = dataset$cmobile_app_trx_lag4 / dataset$Visa_fultimo_cierre_avg6
dataset$cmobile_app_trx_lag4_divided_by_mcuentas_saldo = dataset$cmobile_app_trx_lag4 / dataset$mcuentas_saldo
dataset$cmobile_app_trx_lag4_divided_by_Visa_fultimo_cierre_lag1 = dataset$cmobile_app_trx_lag4 / dataset$Visa_fultimo_cierre_lag1
dataset$cmobile_app_trx_lag4_divided_by_a5p16 = dataset$cmobile_app_trx_lag4 / dataset$a5p16
dataset$cmobile_app_trx_lag4_divided_by_a3p10 = dataset$cmobile_app_trx_lag4 / dataset$a3p10
dataset$cmobile_app_trx_lag4_divided_by_Visa_fechaalta_delta3 = dataset$cmobile_app_trx_lag4 / dataset$Visa_fechaalta_delta3
dataset$cmobile_app_trx_lag4_divided_by_ctrx_quarter = dataset$cmobile_app_trx_lag4 / dataset$ctrx_quarter
dataset$cmobile_app_trx_lag4_divided_by_mactivos_margen = dataset$cmobile_app_trx_lag4 / dataset$mactivos_margen
dataset$Master_fultimo_cierre_divided_by_a1p20 = dataset$Master_fultimo_cierre / dataset$a1p20
dataset$Master_fultimo_cierre_divided_by_na_count_finish_ML_avg6 = dataset$Master_fultimo_cierre / dataset$na_count_finish_ML_avg6
dataset$Master_fultimo_cierre_divided_by_mv_Fvencimiento_delta2 = dataset$Master_fultimo_cierre / dataset$mv_Fvencimiento_delta2
dataset$Master_fultimo_cierre_divided_by_cmobile_app_trx_lag2 = dataset$Master_fultimo_cierre / dataset$cmobile_app_trx_lag2
dataset$Master_fultimo_cierre_divided_by_Visa_fultimo_cierre = dataset$Master_fultimo_cierre / dataset$Visa_fultimo_cierre
dataset$Master_fultimo_cierre_divided_by_mv_status01_delta2 = dataset$Master_fultimo_cierre / dataset$mv_status01_delta2
dataset$Master_fultimo_cierre_divided_by_a5p8 = dataset$Master_fultimo_cierre / dataset$a5p8
dataset$Master_fultimo_cierre_divided_by_a5p10 = dataset$Master_fultimo_cierre / dataset$a5p10
dataset$Master_fultimo_cierre_divided_by_cmobile_app_trx = dataset$Master_fultimo_cierre / dataset$cmobile_app_trx
dataset$Master_fultimo_cierre_divided_by_na_count_finish_ML_tend6 = dataset$Master_fultimo_cierre / dataset$na_count_finish_ML_tend6
dataset$Master_fultimo_cierre_divided_by_mv_fultimo_cierre_delta2 = dataset$Master_fultimo_cierre / dataset$mv_fultimo_cierre_delta2
dataset$Master_fultimo_cierre_divided_by_a4p10 = dataset$Master_fultimo_cierre / dataset$a4p10
dataset$Master_fultimo_cierre_divided_by_Master_fultimo_cierre_lag1 = dataset$Master_fultimo_cierre / dataset$Master_fultimo_cierre_lag1
dataset$Master_fultimo_cierre_divided_by_ctrx_quarter_normalizado = dataset$Master_fultimo_cierre / dataset$ctrx_quarter_normalizado
dataset$Master_fultimo_cierre_divided_by_cpayroll_trx = dataset$Master_fultimo_cierre / dataset$cpayroll_trx
dataset$Master_fultimo_cierre_divided_by_cmobile_app_trx_lag3 = dataset$Master_fultimo_cierre / dataset$cmobile_app_trx_lag3
dataset$Master_fultimo_cierre_divided_by_Master_fechaalta_delta2 = dataset$Master_fultimo_cierre / dataset$Master_fechaalta_delta2
dataset$Master_fultimo_cierre_divided_by_ccomisiones_mantenimiento_delta2 = dataset$Master_fultimo_cierre / dataset$ccomisiones_mantenimiento_delta2
dataset$Master_fultimo_cierre_divided_by_rf_040_044 = dataset$Master_fultimo_cierre / dataset$rf_040_044
dataset$Master_fultimo_cierre_divided_by_mprestamos_personales = dataset$Master_fultimo_cierre / dataset$mprestamos_personales
dataset$Master_fultimo_cierre_divided_by_Master_fultimo_cierre_avg6 = dataset$Master_fultimo_cierre / dataset$Master_fultimo_cierre_avg6
dataset$Master_fultimo_cierre_divided_by_Visa_fultimo_cierre_avg6 = dataset$Master_fultimo_cierre / dataset$Visa_fultimo_cierre_avg6
dataset$Master_fultimo_cierre_divided_by_mcuentas_saldo = dataset$Master_fultimo_cierre / dataset$mcuentas_saldo
dataset$Master_fultimo_cierre_divided_by_Visa_fultimo_cierre_lag1 = dataset$Master_fultimo_cierre / dataset$Visa_fultimo_cierre_lag1
dataset$Master_fultimo_cierre_divided_by_a5p16 = dataset$Master_fultimo_cierre / dataset$a5p16
dataset$Master_fultimo_cierre_divided_by_a3p10 = dataset$Master_fultimo_cierre / dataset$a3p10
dataset$Master_fultimo_cierre_divided_by_Visa_fechaalta_delta3 = dataset$Master_fultimo_cierre / dataset$Visa_fechaalta_delta3
dataset$Master_fultimo_cierre_divided_by_ctrx_quarter = dataset$Master_fultimo_cierre / dataset$ctrx_quarter
dataset$Master_fultimo_cierre_divided_by_mactivos_margen = dataset$Master_fultimo_cierre / dataset$mactivos_margen
dataset$a1p20_divided_by_na_count_finish_ML_avg6 = dataset$a1p20 / dataset$na_count_finish_ML_avg6
dataset$a1p20_divided_by_mv_Fvencimiento_delta2 = dataset$a1p20 / dataset$mv_Fvencimiento_delta2
dataset$a1p20_divided_by_cmobile_app_trx_lag2 = dataset$a1p20 / dataset$cmobile_app_trx_lag2
dataset$a1p20_divided_by_Visa_fultimo_cierre = dataset$a1p20 / dataset$Visa_fultimo_cierre
dataset$a1p20_divided_by_mv_status01_delta2 = dataset$a1p20 / dataset$mv_status01_delta2
dataset$a1p20_divided_by_a5p8 = dataset$a1p20 / dataset$a5p8
dataset$a1p20_divided_by_a5p10 = dataset$a1p20 / dataset$a5p10
dataset$a1p20_divided_by_cmobile_app_trx = dataset$a1p20 / dataset$cmobile_app_trx
dataset$a1p20_divided_by_na_count_finish_ML_tend6 = dataset$a1p20 / dataset$na_count_finish_ML_tend6
dataset$a1p20_divided_by_mv_fultimo_cierre_delta2 = dataset$a1p20 / dataset$mv_fultimo_cierre_delta2
dataset$a1p20_divided_by_a4p10 = dataset$a1p20 / dataset$a4p10
dataset$a1p20_divided_by_Master_fultimo_cierre_lag1 = dataset$a1p20 / dataset$Master_fultimo_cierre_lag1
dataset$a1p20_divided_by_ctrx_quarter_normalizado = dataset$a1p20 / dataset$ctrx_quarter_normalizado
dataset$a1p20_divided_by_cpayroll_trx = dataset$a1p20 / dataset$cpayroll_trx
dataset$a1p20_divided_by_cmobile_app_trx_lag3 = dataset$a1p20 / dataset$cmobile_app_trx_lag3
dataset$a1p20_divided_by_Master_fechaalta_delta2 = dataset$a1p20 / dataset$Master_fechaalta_delta2
dataset$a1p20_divided_by_ccomisiones_mantenimiento_delta2 = dataset$a1p20 / dataset$ccomisiones_mantenimiento_delta2
dataset$a1p20_divided_by_rf_040_044 = dataset$a1p20 / dataset$rf_040_044
dataset$a1p20_divided_by_mprestamos_personales = dataset$a1p20 / dataset$mprestamos_personales
dataset$a1p20_divided_by_Master_fultimo_cierre_avg6 = dataset$a1p20 / dataset$Master_fultimo_cierre_avg6
dataset$a1p20_divided_by_Visa_fultimo_cierre_avg6 = dataset$a1p20 / dataset$Visa_fultimo_cierre_avg6
dataset$a1p20_divided_by_mcuentas_saldo = dataset$a1p20 / dataset$mcuentas_saldo
dataset$a1p20_divided_by_Visa_fultimo_cierre_lag1 = dataset$a1p20 / dataset$Visa_fultimo_cierre_lag1
dataset$a1p20_divided_by_a5p16 = dataset$a1p20 / dataset$a5p16
dataset$a1p20_divided_by_a3p10 = dataset$a1p20 / dataset$a3p10
dataset$a1p20_divided_by_Visa_fechaalta_delta3 = dataset$a1p20 / dataset$Visa_fechaalta_delta3
dataset$a1p20_divided_by_ctrx_quarter = dataset$a1p20 / dataset$ctrx_quarter
dataset$a1p20_divided_by_mactivos_margen = dataset$a1p20 / dataset$mactivos_margen
dataset$na_count_finish_ML_avg6_divided_by_mv_Fvencimiento_delta2 = dataset$na_count_finish_ML_avg6 / dataset$mv_Fvencimiento_delta2
dataset$na_count_finish_ML_avg6_divided_by_cmobile_app_trx_lag2 = dataset$na_count_finish_ML_avg6 / dataset$cmobile_app_trx_lag2
dataset$na_count_finish_ML_avg6_divided_by_Visa_fultimo_cierre = dataset$na_count_finish_ML_avg6 / dataset$Visa_fultimo_cierre
dataset$na_count_finish_ML_avg6_divided_by_mv_status01_delta2 = dataset$na_count_finish_ML_avg6 / dataset$mv_status01_delta2
dataset$na_count_finish_ML_avg6_divided_by_a5p8 = dataset$na_count_finish_ML_avg6 / dataset$a5p8
dataset$na_count_finish_ML_avg6_divided_by_a5p10 = dataset$na_count_finish_ML_avg6 / dataset$a5p10
dataset$na_count_finish_ML_avg6_divided_by_cmobile_app_trx = dataset$na_count_finish_ML_avg6 / dataset$cmobile_app_trx
dataset$na_count_finish_ML_avg6_divided_by_na_count_finish_ML_tend6 = dataset$na_count_finish_ML_avg6 / dataset$na_count_finish_ML_tend6
dataset$na_count_finish_ML_avg6_divided_by_mv_fultimo_cierre_delta2 = dataset$na_count_finish_ML_avg6 / dataset$mv_fultimo_cierre_delta2
dataset$na_count_finish_ML_avg6_divided_by_a4p10 = dataset$na_count_finish_ML_avg6 / dataset$a4p10
dataset$na_count_finish_ML_avg6_divided_by_Master_fultimo_cierre_lag1 = dataset$na_count_finish_ML_avg6 / dataset$Master_fultimo_cierre_lag1
dataset$na_count_finish_ML_avg6_divided_by_ctrx_quarter_normalizado = dataset$na_count_finish_ML_avg6 / dataset$ctrx_quarter_normalizado
dataset$na_count_finish_ML_avg6_divided_by_cpayroll_trx = dataset$na_count_finish_ML_avg6 / dataset$cpayroll_trx
dataset$na_count_finish_ML_avg6_divided_by_cmobile_app_trx_lag3 = dataset$na_count_finish_ML_avg6 / dataset$cmobile_app_trx_lag3
dataset$na_count_finish_ML_avg6_divided_by_Master_fechaalta_delta2 = dataset$na_count_finish_ML_avg6 / dataset$Master_fechaalta_delta2
dataset$na_count_finish_ML_avg6_divided_by_ccomisiones_mantenimiento_delta2 = dataset$na_count_finish_ML_avg6 / dataset$ccomisiones_mantenimiento_delta2
dataset$na_count_finish_ML_avg6_divided_by_rf_040_044 = dataset$na_count_finish_ML_avg6 / dataset$rf_040_044
dataset$na_count_finish_ML_avg6_divided_by_mprestamos_personales = dataset$na_count_finish_ML_avg6 / dataset$mprestamos_personales
dataset$na_count_finish_ML_avg6_divided_by_Master_fultimo_cierre_avg6 = dataset$na_count_finish_ML_avg6 / dataset$Master_fultimo_cierre_avg6
dataset$na_count_finish_ML_avg6_divided_by_Visa_fultimo_cierre_avg6 = dataset$na_count_finish_ML_avg6 / dataset$Visa_fultimo_cierre_avg6
dataset$na_count_finish_ML_avg6_divided_by_mcuentas_saldo = dataset$na_count_finish_ML_avg6 / dataset$mcuentas_saldo
dataset$na_count_finish_ML_avg6_divided_by_Visa_fultimo_cierre_lag1 = dataset$na_count_finish_ML_avg6 / dataset$Visa_fultimo_cierre_lag1
dataset$na_count_finish_ML_avg6_divided_by_a5p16 = dataset$na_count_finish_ML_avg6 / dataset$a5p16
dataset$na_count_finish_ML_avg6_divided_by_a3p10 = dataset$na_count_finish_ML_avg6 / dataset$a3p10
dataset$na_count_finish_ML_avg6_divided_by_Visa_fechaalta_delta3 = dataset$na_count_finish_ML_avg6 / dataset$Visa_fechaalta_delta3
dataset$na_count_finish_ML_avg6_divided_by_ctrx_quarter = dataset$na_count_finish_ML_avg6 / dataset$ctrx_quarter
dataset$na_count_finish_ML_avg6_divided_by_mactivos_margen = dataset$na_count_finish_ML_avg6 / dataset$mactivos_margen
dataset$mv_Fvencimiento_delta2_divided_by_cmobile_app_trx_lag2 = dataset$mv_Fvencimiento_delta2 / dataset$cmobile_app_trx_lag2
dataset$mv_Fvencimiento_delta2_divided_by_Visa_fultimo_cierre = dataset$mv_Fvencimiento_delta2 / dataset$Visa_fultimo_cierre
dataset$mv_Fvencimiento_delta2_divided_by_mv_status01_delta2 = dataset$mv_Fvencimiento_delta2 / dataset$mv_status01_delta2
dataset$mv_Fvencimiento_delta2_divided_by_a5p8 = dataset$mv_Fvencimiento_delta2 / dataset$a5p8
dataset$mv_Fvencimiento_delta2_divided_by_a5p10 = dataset$mv_Fvencimiento_delta2 / dataset$a5p10
dataset$mv_Fvencimiento_delta2_divided_by_cmobile_app_trx = dataset$mv_Fvencimiento_delta2 / dataset$cmobile_app_trx
dataset$mv_Fvencimiento_delta2_divided_by_na_count_finish_ML_tend6 = dataset$mv_Fvencimiento_delta2 / dataset$na_count_finish_ML_tend6
dataset$mv_Fvencimiento_delta2_divided_by_mv_fultimo_cierre_delta2 = dataset$mv_Fvencimiento_delta2 / dataset$mv_fultimo_cierre_delta2
dataset$mv_Fvencimiento_delta2_divided_by_a4p10 = dataset$mv_Fvencimiento_delta2 / dataset$a4p10
dataset$mv_Fvencimiento_delta2_divided_by_Master_fultimo_cierre_lag1 = dataset$mv_Fvencimiento_delta2 / dataset$Master_fultimo_cierre_lag1
dataset$mv_Fvencimiento_delta2_divided_by_ctrx_quarter_normalizado = dataset$mv_Fvencimiento_delta2 / dataset$ctrx_quarter_normalizado
dataset$mv_Fvencimiento_delta2_divided_by_cpayroll_trx = dataset$mv_Fvencimiento_delta2 / dataset$cpayroll_trx
dataset$mv_Fvencimiento_delta2_divided_by_cmobile_app_trx_lag3 = dataset$mv_Fvencimiento_delta2 / dataset$cmobile_app_trx_lag3
dataset$mv_Fvencimiento_delta2_divided_by_Master_fechaalta_delta2 = dataset$mv_Fvencimiento_delta2 / dataset$Master_fechaalta_delta2
dataset$mv_Fvencimiento_delta2_divided_by_ccomisiones_mantenimiento_delta2 = dataset$mv_Fvencimiento_delta2 / dataset$ccomisiones_mantenimiento_delta2
dataset$mv_Fvencimiento_delta2_divided_by_rf_040_044 = dataset$mv_Fvencimiento_delta2 / dataset$rf_040_044
dataset$mv_Fvencimiento_delta2_divided_by_mprestamos_personales = dataset$mv_Fvencimiento_delta2 / dataset$mprestamos_personales
dataset$mv_Fvencimiento_delta2_divided_by_Master_fultimo_cierre_avg6 = dataset$mv_Fvencimiento_delta2 / dataset$Master_fultimo_cierre_avg6
dataset$mv_Fvencimiento_delta2_divided_by_Visa_fultimo_cierre_avg6 = dataset$mv_Fvencimiento_delta2 / dataset$Visa_fultimo_cierre_avg6
dataset$mv_Fvencimiento_delta2_divided_by_mcuentas_saldo = dataset$mv_Fvencimiento_delta2 / dataset$mcuentas_saldo
dataset$mv_Fvencimiento_delta2_divided_by_Visa_fultimo_cierre_lag1 = dataset$mv_Fvencimiento_delta2 / dataset$Visa_fultimo_cierre_lag1
dataset$mv_Fvencimiento_delta2_divided_by_a5p16 = dataset$mv_Fvencimiento_delta2 / dataset$a5p16
dataset$mv_Fvencimiento_delta2_divided_by_a3p10 = dataset$mv_Fvencimiento_delta2 / dataset$a3p10
dataset$mv_Fvencimiento_delta2_divided_by_Visa_fechaalta_delta3 = dataset$mv_Fvencimiento_delta2 / dataset$Visa_fechaalta_delta3
dataset$mv_Fvencimiento_delta2_divided_by_ctrx_quarter = dataset$mv_Fvencimiento_delta2 / dataset$ctrx_quarter
dataset$mv_Fvencimiento_delta2_divided_by_mactivos_margen = dataset$mv_Fvencimiento_delta2 / dataset$mactivos_margen
dataset$cmobile_app_trx_lag2_divided_by_Visa_fultimo_cierre = dataset$cmobile_app_trx_lag2 / dataset$Visa_fultimo_cierre
dataset$cmobile_app_trx_lag2_divided_by_mv_status01_delta2 = dataset$cmobile_app_trx_lag2 / dataset$mv_status01_delta2
dataset$cmobile_app_trx_lag2_divided_by_a5p8 = dataset$cmobile_app_trx_lag2 / dataset$a5p8
dataset$cmobile_app_trx_lag2_divided_by_a5p10 = dataset$cmobile_app_trx_lag2 / dataset$a5p10
dataset$cmobile_app_trx_lag2_divided_by_cmobile_app_trx = dataset$cmobile_app_trx_lag2 / dataset$cmobile_app_trx
dataset$cmobile_app_trx_lag2_divided_by_na_count_finish_ML_tend6 = dataset$cmobile_app_trx_lag2 / dataset$na_count_finish_ML_tend6
dataset$cmobile_app_trx_lag2_divided_by_mv_fultimo_cierre_delta2 = dataset$cmobile_app_trx_lag2 / dataset$mv_fultimo_cierre_delta2
dataset$cmobile_app_trx_lag2_divided_by_a4p10 = dataset$cmobile_app_trx_lag2 / dataset$a4p10
dataset$cmobile_app_trx_lag2_divided_by_Master_fultimo_cierre_lag1 = dataset$cmobile_app_trx_lag2 / dataset$Master_fultimo_cierre_lag1
dataset$cmobile_app_trx_lag2_divided_by_ctrx_quarter_normalizado = dataset$cmobile_app_trx_lag2 / dataset$ctrx_quarter_normalizado
dataset$cmobile_app_trx_lag2_divided_by_cpayroll_trx = dataset$cmobile_app_trx_lag2 / dataset$cpayroll_trx
dataset$cmobile_app_trx_lag2_divided_by_cmobile_app_trx_lag3 = dataset$cmobile_app_trx_lag2 / dataset$cmobile_app_trx_lag3
dataset$cmobile_app_trx_lag2_divided_by_Master_fechaalta_delta2 = dataset$cmobile_app_trx_lag2 / dataset$Master_fechaalta_delta2
dataset$cmobile_app_trx_lag2_divided_by_ccomisiones_mantenimiento_delta2 = dataset$cmobile_app_trx_lag2 / dataset$ccomisiones_mantenimiento_delta2
dataset$cmobile_app_trx_lag2_divided_by_rf_040_044 = dataset$cmobile_app_trx_lag2 / dataset$rf_040_044
dataset$cmobile_app_trx_lag2_divided_by_mprestamos_personales = dataset$cmobile_app_trx_lag2 / dataset$mprestamos_personales
dataset$cmobile_app_trx_lag2_divided_by_Master_fultimo_cierre_avg6 = dataset$cmobile_app_trx_lag2 / dataset$Master_fultimo_cierre_avg6
dataset$cmobile_app_trx_lag2_divided_by_Visa_fultimo_cierre_avg6 = dataset$cmobile_app_trx_lag2 / dataset$Visa_fultimo_cierre_avg6
dataset$cmobile_app_trx_lag2_divided_by_mcuentas_saldo = dataset$cmobile_app_trx_lag2 / dataset$mcuentas_saldo
dataset$cmobile_app_trx_lag2_divided_by_Visa_fultimo_cierre_lag1 = dataset$cmobile_app_trx_lag2 / dataset$Visa_fultimo_cierre_lag1
dataset$cmobile_app_trx_lag2_divided_by_a5p16 = dataset$cmobile_app_trx_lag2 / dataset$a5p16
dataset$cmobile_app_trx_lag2_divided_by_a3p10 = dataset$cmobile_app_trx_lag2 / dataset$a3p10
dataset$cmobile_app_trx_lag2_divided_by_Visa_fechaalta_delta3 = dataset$cmobile_app_trx_lag2 / dataset$Visa_fechaalta_delta3
dataset$cmobile_app_trx_lag2_divided_by_ctrx_quarter = dataset$cmobile_app_trx_lag2 / dataset$ctrx_quarter
dataset$cmobile_app_trx_lag2_divided_by_mactivos_margen = dataset$cmobile_app_trx_lag2 / dataset$mactivos_margen
dataset$Visa_fultimo_cierre_divided_by_mv_status01_delta2 = dataset$Visa_fultimo_cierre / dataset$mv_status01_delta2
dataset$Visa_fultimo_cierre_divided_by_a5p8 = dataset$Visa_fultimo_cierre / dataset$a5p8
dataset$Visa_fultimo_cierre_divided_by_a5p10 = dataset$Visa_fultimo_cierre / dataset$a5p10
dataset$Visa_fultimo_cierre_divided_by_cmobile_app_trx = dataset$Visa_fultimo_cierre / dataset$cmobile_app_trx
dataset$Visa_fultimo_cierre_divided_by_na_count_finish_ML_tend6 = dataset$Visa_fultimo_cierre / dataset$na_count_finish_ML_tend6
dataset$Visa_fultimo_cierre_divided_by_mv_fultimo_cierre_delta2 = dataset$Visa_fultimo_cierre / dataset$mv_fultimo_cierre_delta2
dataset$Visa_fultimo_cierre_divided_by_a4p10 = dataset$Visa_fultimo_cierre / dataset$a4p10
dataset$Visa_fultimo_cierre_divided_by_Master_fultimo_cierre_lag1 = dataset$Visa_fultimo_cierre / dataset$Master_fultimo_cierre_lag1
dataset$Visa_fultimo_cierre_divided_by_ctrx_quarter_normalizado = dataset$Visa_fultimo_cierre / dataset$ctrx_quarter_normalizado
dataset$Visa_fultimo_cierre_divided_by_cpayroll_trx = dataset$Visa_fultimo_cierre / dataset$cpayroll_trx
dataset$Visa_fultimo_cierre_divided_by_cmobile_app_trx_lag3 = dataset$Visa_fultimo_cierre / dataset$cmobile_app_trx_lag3
dataset$Visa_fultimo_cierre_divided_by_Master_fechaalta_delta2 = dataset$Visa_fultimo_cierre / dataset$Master_fechaalta_delta2
dataset$Visa_fultimo_cierre_divided_by_ccomisiones_mantenimiento_delta2 = dataset$Visa_fultimo_cierre / dataset$ccomisiones_mantenimiento_delta2
dataset$Visa_fultimo_cierre_divided_by_rf_040_044 = dataset$Visa_fultimo_cierre / dataset$rf_040_044
dataset$Visa_fultimo_cierre_divided_by_mprestamos_personales = dataset$Visa_fultimo_cierre / dataset$mprestamos_personales
dataset$Visa_fultimo_cierre_divided_by_Master_fultimo_cierre_avg6 = dataset$Visa_fultimo_cierre / dataset$Master_fultimo_cierre_avg6
dataset$Visa_fultimo_cierre_divided_by_Visa_fultimo_cierre_avg6 = dataset$Visa_fultimo_cierre / dataset$Visa_fultimo_cierre_avg6
dataset$Visa_fultimo_cierre_divided_by_mcuentas_saldo = dataset$Visa_fultimo_cierre / dataset$mcuentas_saldo
dataset$Visa_fultimo_cierre_divided_by_Visa_fultimo_cierre_lag1 = dataset$Visa_fultimo_cierre / dataset$Visa_fultimo_cierre_lag1
dataset$Visa_fultimo_cierre_divided_by_a5p16 = dataset$Visa_fultimo_cierre / dataset$a5p16
dataset$Visa_fultimo_cierre_divided_by_a3p10 = dataset$Visa_fultimo_cierre / dataset$a3p10
dataset$Visa_fultimo_cierre_divided_by_Visa_fechaalta_delta3 = dataset$Visa_fultimo_cierre / dataset$Visa_fechaalta_delta3
dataset$Visa_fultimo_cierre_divided_by_ctrx_quarter = dataset$Visa_fultimo_cierre / dataset$ctrx_quarter
dataset$Visa_fultimo_cierre_divided_by_mactivos_margen = dataset$Visa_fultimo_cierre / dataset$mactivos_margen
dataset$mv_status01_delta2_divided_by_a5p8 = dataset$mv_status01_delta2 / dataset$a5p8
dataset$mv_status01_delta2_divided_by_a5p10 = dataset$mv_status01_delta2 / dataset$a5p10
dataset$mv_status01_delta2_divided_by_cmobile_app_trx = dataset$mv_status01_delta2 / dataset$cmobile_app_trx
dataset$mv_status01_delta2_divided_by_na_count_finish_ML_tend6 = dataset$mv_status01_delta2 / dataset$na_count_finish_ML_tend6
dataset$mv_status01_delta2_divided_by_mv_fultimo_cierre_delta2 = dataset$mv_status01_delta2 / dataset$mv_fultimo_cierre_delta2
dataset$mv_status01_delta2_divided_by_a4p10 = dataset$mv_status01_delta2 / dataset$a4p10
dataset$mv_status01_delta2_divided_by_Master_fultimo_cierre_lag1 = dataset$mv_status01_delta2 / dataset$Master_fultimo_cierre_lag1
dataset$mv_status01_delta2_divided_by_ctrx_quarter_normalizado = dataset$mv_status01_delta2 / dataset$ctrx_quarter_normalizado
dataset$mv_status01_delta2_divided_by_cpayroll_trx = dataset$mv_status01_delta2 / dataset$cpayroll_trx
dataset$mv_status01_delta2_divided_by_cmobile_app_trx_lag3 = dataset$mv_status01_delta2 / dataset$cmobile_app_trx_lag3
dataset$mv_status01_delta2_divided_by_Master_fechaalta_delta2 = dataset$mv_status01_delta2 / dataset$Master_fechaalta_delta2
dataset$mv_status01_delta2_divided_by_ccomisiones_mantenimiento_delta2 = dataset$mv_status01_delta2 / dataset$ccomisiones_mantenimiento_delta2
dataset$mv_status01_delta2_divided_by_rf_040_044 = dataset$mv_status01_delta2 / dataset$rf_040_044
dataset$mv_status01_delta2_divided_by_mprestamos_personales = dataset$mv_status01_delta2 / dataset$mprestamos_personales
dataset$mv_status01_delta2_divided_by_Master_fultimo_cierre_avg6 = dataset$mv_status01_delta2 / dataset$Master_fultimo_cierre_avg6
dataset$mv_status01_delta2_divided_by_Visa_fultimo_cierre_avg6 = dataset$mv_status01_delta2 / dataset$Visa_fultimo_cierre_avg6
dataset$mv_status01_delta2_divided_by_mcuentas_saldo = dataset$mv_status01_delta2 / dataset$mcuentas_saldo
dataset$mv_status01_delta2_divided_by_Visa_fultimo_cierre_lag1 = dataset$mv_status01_delta2 / dataset$Visa_fultimo_cierre_lag1
dataset$mv_status01_delta2_divided_by_a5p16 = dataset$mv_status01_delta2 / dataset$a5p16
dataset$mv_status01_delta2_divided_by_a3p10 = dataset$mv_status01_delta2 / dataset$a3p10
dataset$mv_status01_delta2_divided_by_Visa_fechaalta_delta3 = dataset$mv_status01_delta2 / dataset$Visa_fechaalta_delta3
dataset$mv_status01_delta2_divided_by_ctrx_quarter = dataset$mv_status01_delta2 / dataset$ctrx_quarter
dataset$mv_status01_delta2_divided_by_mactivos_margen = dataset$mv_status01_delta2 / dataset$mactivos_margen
dataset$a5p8_divided_by_a5p10 = dataset$a5p8 / dataset$a5p10
dataset$a5p8_divided_by_cmobile_app_trx = dataset$a5p8 / dataset$cmobile_app_trx
dataset$a5p8_divided_by_na_count_finish_ML_tend6 = dataset$a5p8 / dataset$na_count_finish_ML_tend6
dataset$a5p8_divided_by_mv_fultimo_cierre_delta2 = dataset$a5p8 / dataset$mv_fultimo_cierre_delta2
dataset$a5p8_divided_by_a4p10 = dataset$a5p8 / dataset$a4p10
dataset$a5p8_divided_by_Master_fultimo_cierre_lag1 = dataset$a5p8 / dataset$Master_fultimo_cierre_lag1
dataset$a5p8_divided_by_ctrx_quarter_normalizado = dataset$a5p8 / dataset$ctrx_quarter_normalizado
dataset$a5p8_divided_by_cpayroll_trx = dataset$a5p8 / dataset$cpayroll_trx
dataset$a5p8_divided_by_cmobile_app_trx_lag3 = dataset$a5p8 / dataset$cmobile_app_trx_lag3
dataset$a5p8_divided_by_Master_fechaalta_delta2 = dataset$a5p8 / dataset$Master_fechaalta_delta2
dataset$a5p8_divided_by_ccomisiones_mantenimiento_delta2 = dataset$a5p8 / dataset$ccomisiones_mantenimiento_delta2
dataset$a5p8_divided_by_rf_040_044 = dataset$a5p8 / dataset$rf_040_044
dataset$a5p8_divided_by_mprestamos_personales = dataset$a5p8 / dataset$mprestamos_personales
dataset$a5p8_divided_by_Master_fultimo_cierre_avg6 = dataset$a5p8 / dataset$Master_fultimo_cierre_avg6
dataset$a5p8_divided_by_Visa_fultimo_cierre_avg6 = dataset$a5p8 / dataset$Visa_fultimo_cierre_avg6
dataset$a5p8_divided_by_mcuentas_saldo = dataset$a5p8 / dataset$mcuentas_saldo
dataset$a5p8_divided_by_Visa_fultimo_cierre_lag1 = dataset$a5p8 / dataset$Visa_fultimo_cierre_lag1
dataset$a5p8_divided_by_a5p16 = dataset$a5p8 / dataset$a5p16
dataset$a5p8_divided_by_a3p10 = dataset$a5p8 / dataset$a3p10
dataset$a5p8_divided_by_Visa_fechaalta_delta3 = dataset$a5p8 / dataset$Visa_fechaalta_delta3
dataset$a5p8_divided_by_ctrx_quarter = dataset$a5p8 / dataset$ctrx_quarter
dataset$a5p8_divided_by_mactivos_margen = dataset$a5p8 / dataset$mactivos_margen
dataset$a5p10_divided_by_cmobile_app_trx = dataset$a5p10 / dataset$cmobile_app_trx
dataset$a5p10_divided_by_na_count_finish_ML_tend6 = dataset$a5p10 / dataset$na_count_finish_ML_tend6
dataset$a5p10_divided_by_mv_fultimo_cierre_delta2 = dataset$a5p10 / dataset$mv_fultimo_cierre_delta2
dataset$a5p10_divided_by_a4p10 = dataset$a5p10 / dataset$a4p10
dataset$a5p10_divided_by_Master_fultimo_cierre_lag1 = dataset$a5p10 / dataset$Master_fultimo_cierre_lag1
dataset$a5p10_divided_by_ctrx_quarter_normalizado = dataset$a5p10 / dataset$ctrx_quarter_normalizado
dataset$a5p10_divided_by_cpayroll_trx = dataset$a5p10 / dataset$cpayroll_trx
dataset$a5p10_divided_by_cmobile_app_trx_lag3 = dataset$a5p10 / dataset$cmobile_app_trx_lag3
dataset$a5p10_divided_by_Master_fechaalta_delta2 = dataset$a5p10 / dataset$Master_fechaalta_delta2
dataset$a5p10_divided_by_ccomisiones_mantenimiento_delta2 = dataset$a5p10 / dataset$ccomisiones_mantenimiento_delta2
dataset$a5p10_divided_by_rf_040_044 = dataset$a5p10 / dataset$rf_040_044
dataset$a5p10_divided_by_mprestamos_personales = dataset$a5p10 / dataset$mprestamos_personales
dataset$a5p10_divided_by_Master_fultimo_cierre_avg6 = dataset$a5p10 / dataset$Master_fultimo_cierre_avg6
dataset$a5p10_divided_by_Visa_fultimo_cierre_avg6 = dataset$a5p10 / dataset$Visa_fultimo_cierre_avg6
dataset$a5p10_divided_by_mcuentas_saldo = dataset$a5p10 / dataset$mcuentas_saldo
dataset$a5p10_divided_by_Visa_fultimo_cierre_lag1 = dataset$a5p10 / dataset$Visa_fultimo_cierre_lag1
dataset$a5p10_divided_by_a5p16 = dataset$a5p10 / dataset$a5p16
dataset$a5p10_divided_by_a3p10 = dataset$a5p10 / dataset$a3p10
dataset$a5p10_divided_by_Visa_fechaalta_delta3 = dataset$a5p10 / dataset$Visa_fechaalta_delta3
dataset$a5p10_divided_by_ctrx_quarter = dataset$a5p10 / dataset$ctrx_quarter
dataset$a5p10_divided_by_mactivos_margen = dataset$a5p10 / dataset$mactivos_margen
dataset$cmobile_app_trx_divided_by_na_count_finish_ML_tend6 = dataset$cmobile_app_trx / dataset$na_count_finish_ML_tend6
dataset$cmobile_app_trx_divided_by_mv_fultimo_cierre_delta2 = dataset$cmobile_app_trx / dataset$mv_fultimo_cierre_delta2
dataset$cmobile_app_trx_divided_by_a4p10 = dataset$cmobile_app_trx / dataset$a4p10
dataset$cmobile_app_trx_divided_by_Master_fultimo_cierre_lag1 = dataset$cmobile_app_trx / dataset$Master_fultimo_cierre_lag1
dataset$cmobile_app_trx_divided_by_ctrx_quarter_normalizado = dataset$cmobile_app_trx / dataset$ctrx_quarter_normalizado
dataset$cmobile_app_trx_divided_by_cpayroll_trx = dataset$cmobile_app_trx / dataset$cpayroll_trx
dataset$cmobile_app_trx_divided_by_cmobile_app_trx_lag3 = dataset$cmobile_app_trx / dataset$cmobile_app_trx_lag3
dataset$cmobile_app_trx_divided_by_Master_fechaalta_delta2 = dataset$cmobile_app_trx / dataset$Master_fechaalta_delta2
dataset$cmobile_app_trx_divided_by_ccomisiones_mantenimiento_delta2 = dataset$cmobile_app_trx / dataset$ccomisiones_mantenimiento_delta2
dataset$cmobile_app_trx_divided_by_rf_040_044 = dataset$cmobile_app_trx / dataset$rf_040_044
dataset$cmobile_app_trx_divided_by_mprestamos_personales = dataset$cmobile_app_trx / dataset$mprestamos_personales
dataset$cmobile_app_trx_divided_by_Master_fultimo_cierre_avg6 = dataset$cmobile_app_trx / dataset$Master_fultimo_cierre_avg6
dataset$cmobile_app_trx_divided_by_Visa_fultimo_cierre_avg6 = dataset$cmobile_app_trx / dataset$Visa_fultimo_cierre_avg6
dataset$cmobile_app_trx_divided_by_mcuentas_saldo = dataset$cmobile_app_trx / dataset$mcuentas_saldo
dataset$cmobile_app_trx_divided_by_Visa_fultimo_cierre_lag1 = dataset$cmobile_app_trx / dataset$Visa_fultimo_cierre_lag1
dataset$cmobile_app_trx_divided_by_a5p16 = dataset$cmobile_app_trx / dataset$a5p16
dataset$cmobile_app_trx_divided_by_a3p10 = dataset$cmobile_app_trx / dataset$a3p10
dataset$cmobile_app_trx_divided_by_Visa_fechaalta_delta3 = dataset$cmobile_app_trx / dataset$Visa_fechaalta_delta3
dataset$cmobile_app_trx_divided_by_ctrx_quarter = dataset$cmobile_app_trx / dataset$ctrx_quarter
dataset$cmobile_app_trx_divided_by_mactivos_margen = dataset$cmobile_app_trx / dataset$mactivos_margen
dataset$na_count_finish_ML_tend6_divided_by_mv_fultimo_cierre_delta2 = dataset$na_count_finish_ML_tend6 / dataset$mv_fultimo_cierre_delta2
dataset$na_count_finish_ML_tend6_divided_by_a4p10 = dataset$na_count_finish_ML_tend6 / dataset$a4p10
dataset$na_count_finish_ML_tend6_divided_by_Master_fultimo_cierre_lag1 = dataset$na_count_finish_ML_tend6 / dataset$Master_fultimo_cierre_lag1
dataset$na_count_finish_ML_tend6_divided_by_ctrx_quarter_normalizado = dataset$na_count_finish_ML_tend6 / dataset$ctrx_quarter_normalizado
dataset$na_count_finish_ML_tend6_divided_by_cpayroll_trx = dataset$na_count_finish_ML_tend6 / dataset$cpayroll_trx
dataset$na_count_finish_ML_tend6_divided_by_cmobile_app_trx_lag3 = dataset$na_count_finish_ML_tend6 / dataset$cmobile_app_trx_lag3
dataset$na_count_finish_ML_tend6_divided_by_Master_fechaalta_delta2 = dataset$na_count_finish_ML_tend6 / dataset$Master_fechaalta_delta2
dataset$na_count_finish_ML_tend6_divided_by_ccomisiones_mantenimiento_delta2 = dataset$na_count_finish_ML_tend6 / dataset$ccomisiones_mantenimiento_delta2
dataset$na_count_finish_ML_tend6_divided_by_rf_040_044 = dataset$na_count_finish_ML_tend6 / dataset$rf_040_044
dataset$na_count_finish_ML_tend6_divided_by_mprestamos_personales = dataset$na_count_finish_ML_tend6 / dataset$mprestamos_personales
dataset$na_count_finish_ML_tend6_divided_by_Master_fultimo_cierre_avg6 = dataset$na_count_finish_ML_tend6 / dataset$Master_fultimo_cierre_avg6
dataset$na_count_finish_ML_tend6_divided_by_Visa_fultimo_cierre_avg6 = dataset$na_count_finish_ML_tend6 / dataset$Visa_fultimo_cierre_avg6
dataset$na_count_finish_ML_tend6_divided_by_mcuentas_saldo = dataset$na_count_finish_ML_tend6 / dataset$mcuentas_saldo
dataset$na_count_finish_ML_tend6_divided_by_Visa_fultimo_cierre_lag1 = dataset$na_count_finish_ML_tend6 / dataset$Visa_fultimo_cierre_lag1
dataset$na_count_finish_ML_tend6_divided_by_a5p16 = dataset$na_count_finish_ML_tend6 / dataset$a5p16
dataset$na_count_finish_ML_tend6_divided_by_a3p10 = dataset$na_count_finish_ML_tend6 / dataset$a3p10
dataset$na_count_finish_ML_tend6_divided_by_Visa_fechaalta_delta3 = dataset$na_count_finish_ML_tend6 / dataset$Visa_fechaalta_delta3
dataset$na_count_finish_ML_tend6_divided_by_ctrx_quarter = dataset$na_count_finish_ML_tend6 / dataset$ctrx_quarter
dataset$na_count_finish_ML_tend6_divided_by_mactivos_margen = dataset$na_count_finish_ML_tend6 / dataset$mactivos_margen
dataset$mv_fultimo_cierre_delta2_divided_by_a4p10 = dataset$mv_fultimo_cierre_delta2 / dataset$a4p10
dataset$mv_fultimo_cierre_delta2_divided_by_Master_fultimo_cierre_lag1 = dataset$mv_fultimo_cierre_delta2 / dataset$Master_fultimo_cierre_lag1
dataset$mv_fultimo_cierre_delta2_divided_by_ctrx_quarter_normalizado = dataset$mv_fultimo_cierre_delta2 / dataset$ctrx_quarter_normalizado
dataset$mv_fultimo_cierre_delta2_divided_by_cpayroll_trx = dataset$mv_fultimo_cierre_delta2 / dataset$cpayroll_trx
dataset$mv_fultimo_cierre_delta2_divided_by_cmobile_app_trx_lag3 = dataset$mv_fultimo_cierre_delta2 / dataset$cmobile_app_trx_lag3
dataset$mv_fultimo_cierre_delta2_divided_by_Master_fechaalta_delta2 = dataset$mv_fultimo_cierre_delta2 / dataset$Master_fechaalta_delta2
dataset$mv_fultimo_cierre_delta2_divided_by_ccomisiones_mantenimiento_delta2 = dataset$mv_fultimo_cierre_delta2 / dataset$ccomisiones_mantenimiento_delta2
dataset$mv_fultimo_cierre_delta2_divided_by_rf_040_044 = dataset$mv_fultimo_cierre_delta2 / dataset$rf_040_044
dataset$mv_fultimo_cierre_delta2_divided_by_mprestamos_personales = dataset$mv_fultimo_cierre_delta2 / dataset$mprestamos_personales
dataset$mv_fultimo_cierre_delta2_divided_by_Master_fultimo_cierre_avg6 = dataset$mv_fultimo_cierre_delta2 / dataset$Master_fultimo_cierre_avg6
dataset$mv_fultimo_cierre_delta2_divided_by_Visa_fultimo_cierre_avg6 = dataset$mv_fultimo_cierre_delta2 / dataset$Visa_fultimo_cierre_avg6
dataset$mv_fultimo_cierre_delta2_divided_by_mcuentas_saldo = dataset$mv_fultimo_cierre_delta2 / dataset$mcuentas_saldo
dataset$mv_fultimo_cierre_delta2_divided_by_Visa_fultimo_cierre_lag1 = dataset$mv_fultimo_cierre_delta2 / dataset$Visa_fultimo_cierre_lag1
dataset$mv_fultimo_cierre_delta2_divided_by_a5p16 = dataset$mv_fultimo_cierre_delta2 / dataset$a5p16
dataset$mv_fultimo_cierre_delta2_divided_by_a3p10 = dataset$mv_fultimo_cierre_delta2 / dataset$a3p10
dataset$mv_fultimo_cierre_delta2_divided_by_Visa_fechaalta_delta3 = dataset$mv_fultimo_cierre_delta2 / dataset$Visa_fechaalta_delta3
dataset$mv_fultimo_cierre_delta2_divided_by_ctrx_quarter = dataset$mv_fultimo_cierre_delta2 / dataset$ctrx_quarter
dataset$mv_fultimo_cierre_delta2_divided_by_mactivos_margen = dataset$mv_fultimo_cierre_delta2 / dataset$mactivos_margen
dataset$a4p10_divided_by_Master_fultimo_cierre_lag1 = dataset$a4p10 / dataset$Master_fultimo_cierre_lag1
dataset$a4p10_divided_by_ctrx_quarter_normalizado = dataset$a4p10 / dataset$ctrx_quarter_normalizado
dataset$a4p10_divided_by_cpayroll_trx = dataset$a4p10 / dataset$cpayroll_trx
dataset$a4p10_divided_by_cmobile_app_trx_lag3 = dataset$a4p10 / dataset$cmobile_app_trx_lag3
dataset$a4p10_divided_by_Master_fechaalta_delta2 = dataset$a4p10 / dataset$Master_fechaalta_delta2
dataset$a4p10_divided_by_ccomisiones_mantenimiento_delta2 = dataset$a4p10 / dataset$ccomisiones_mantenimiento_delta2
dataset$a4p10_divided_by_rf_040_044 = dataset$a4p10 / dataset$rf_040_044
dataset$a4p10_divided_by_mprestamos_personales = dataset$a4p10 / dataset$mprestamos_personales
dataset$a4p10_divided_by_Master_fultimo_cierre_avg6 = dataset$a4p10 / dataset$Master_fultimo_cierre_avg6
dataset$a4p10_divided_by_Visa_fultimo_cierre_avg6 = dataset$a4p10 / dataset$Visa_fultimo_cierre_avg6
dataset$a4p10_divided_by_mcuentas_saldo = dataset$a4p10 / dataset$mcuentas_saldo
dataset$a4p10_divided_by_Visa_fultimo_cierre_lag1 = dataset$a4p10 / dataset$Visa_fultimo_cierre_lag1
dataset$a4p10_divided_by_a5p16 = dataset$a4p10 / dataset$a5p16
dataset$a4p10_divided_by_a3p10 = dataset$a4p10 / dataset$a3p10
dataset$a4p10_divided_by_Visa_fechaalta_delta3 = dataset$a4p10 / dataset$Visa_fechaalta_delta3
dataset$a4p10_divided_by_ctrx_quarter = dataset$a4p10 / dataset$ctrx_quarter
dataset$a4p10_divided_by_mactivos_margen = dataset$a4p10 / dataset$mactivos_margen
dataset$Master_fultimo_cierre_lag1_divided_by_ctrx_quarter_normalizado = dataset$Master_fultimo_cierre_lag1 / dataset$ctrx_quarter_normalizado
dataset$Master_fultimo_cierre_lag1_divided_by_cpayroll_trx = dataset$Master_fultimo_cierre_lag1 / dataset$cpayroll_trx
dataset$Master_fultimo_cierre_lag1_divided_by_cmobile_app_trx_lag3 = dataset$Master_fultimo_cierre_lag1 / dataset$cmobile_app_trx_lag3
dataset$Master_fultimo_cierre_lag1_divided_by_Master_fechaalta_delta2 = dataset$Master_fultimo_cierre_lag1 / dataset$Master_fechaalta_delta2
dataset$Master_fultimo_cierre_lag1_divided_by_ccomisiones_mantenimiento_delta2 = dataset$Master_fultimo_cierre_lag1 / dataset$ccomisiones_mantenimiento_delta2
dataset$Master_fultimo_cierre_lag1_divided_by_rf_040_044 = dataset$Master_fultimo_cierre_lag1 / dataset$rf_040_044
dataset$Master_fultimo_cierre_lag1_divided_by_mprestamos_personales = dataset$Master_fultimo_cierre_lag1 / dataset$mprestamos_personales
dataset$Master_fultimo_cierre_lag1_divided_by_Master_fultimo_cierre_avg6 = dataset$Master_fultimo_cierre_lag1 / dataset$Master_fultimo_cierre_avg6
dataset$Master_fultimo_cierre_lag1_divided_by_Visa_fultimo_cierre_avg6 = dataset$Master_fultimo_cierre_lag1 / dataset$Visa_fultimo_cierre_avg6
dataset$Master_fultimo_cierre_lag1_divided_by_mcuentas_saldo = dataset$Master_fultimo_cierre_lag1 / dataset$mcuentas_saldo
dataset$Master_fultimo_cierre_lag1_divided_by_Visa_fultimo_cierre_lag1 = dataset$Master_fultimo_cierre_lag1 / dataset$Visa_fultimo_cierre_lag1
dataset$Master_fultimo_cierre_lag1_divided_by_a5p16 = dataset$Master_fultimo_cierre_lag1 / dataset$a5p16
dataset$Master_fultimo_cierre_lag1_divided_by_a3p10 = dataset$Master_fultimo_cierre_lag1 / dataset$a3p10
dataset$Master_fultimo_cierre_lag1_divided_by_Visa_fechaalta_delta3 = dataset$Master_fultimo_cierre_lag1 / dataset$Visa_fechaalta_delta3
dataset$Master_fultimo_cierre_lag1_divided_by_ctrx_quarter = dataset$Master_fultimo_cierre_lag1 / dataset$ctrx_quarter
dataset$Master_fultimo_cierre_lag1_divided_by_mactivos_margen = dataset$Master_fultimo_cierre_lag1 / dataset$mactivos_margen
dataset$ctrx_quarter_normalizado_divided_by_cpayroll_trx = dataset$ctrx_quarter_normalizado / dataset$cpayroll_trx
dataset$ctrx_quarter_normalizado_divided_by_cmobile_app_trx_lag3 = dataset$ctrx_quarter_normalizado / dataset$cmobile_app_trx_lag3
dataset$ctrx_quarter_normalizado_divided_by_Master_fechaalta_delta2 = dataset$ctrx_quarter_normalizado / dataset$Master_fechaalta_delta2
dataset$ctrx_quarter_normalizado_divided_by_ccomisiones_mantenimiento_delta2 = dataset$ctrx_quarter_normalizado / dataset$ccomisiones_mantenimiento_delta2
dataset$ctrx_quarter_normalizado_divided_by_rf_040_044 = dataset$ctrx_quarter_normalizado / dataset$rf_040_044
dataset$ctrx_quarter_normalizado_divided_by_mprestamos_personales = dataset$ctrx_quarter_normalizado / dataset$mprestamos_personales
dataset$ctrx_quarter_normalizado_divided_by_Master_fultimo_cierre_avg6 = dataset$ctrx_quarter_normalizado / dataset$Master_fultimo_cierre_avg6
dataset$ctrx_quarter_normalizado_divided_by_Visa_fultimo_cierre_avg6 = dataset$ctrx_quarter_normalizado / dataset$Visa_fultimo_cierre_avg6
dataset$ctrx_quarter_normalizado_divided_by_mcuentas_saldo = dataset$ctrx_quarter_normalizado / dataset$mcuentas_saldo
dataset$ctrx_quarter_normalizado_divided_by_Visa_fultimo_cierre_lag1 = dataset$ctrx_quarter_normalizado / dataset$Visa_fultimo_cierre_lag1
dataset$ctrx_quarter_normalizado_divided_by_a5p16 = dataset$ctrx_quarter_normalizado / dataset$a5p16
dataset$ctrx_quarter_normalizado_divided_by_a3p10 = dataset$ctrx_quarter_normalizado / dataset$a3p10
dataset$ctrx_quarter_normalizado_divided_by_Visa_fechaalta_delta3 = dataset$ctrx_quarter_normalizado / dataset$Visa_fechaalta_delta3
dataset$ctrx_quarter_normalizado_divided_by_ctrx_quarter = dataset$ctrx_quarter_normalizado / dataset$ctrx_quarter
dataset$ctrx_quarter_normalizado_divided_by_mactivos_margen = dataset$ctrx_quarter_normalizado / dataset$mactivos_margen
dataset$cpayroll_trx_divided_by_cmobile_app_trx_lag3 = dataset$cpayroll_trx / dataset$cmobile_app_trx_lag3
dataset$cpayroll_trx_divided_by_Master_fechaalta_delta2 = dataset$cpayroll_trx / dataset$Master_fechaalta_delta2
dataset$cpayroll_trx_divided_by_ccomisiones_mantenimiento_delta2 = dataset$cpayroll_trx / dataset$ccomisiones_mantenimiento_delta2
dataset$cpayroll_trx_divided_by_rf_040_044 = dataset$cpayroll_trx / dataset$rf_040_044
dataset$cpayroll_trx_divided_by_mprestamos_personales = dataset$cpayroll_trx / dataset$mprestamos_personales
dataset$cpayroll_trx_divided_by_Master_fultimo_cierre_avg6 = dataset$cpayroll_trx / dataset$Master_fultimo_cierre_avg6
dataset$cpayroll_trx_divided_by_Visa_fultimo_cierre_avg6 = dataset$cpayroll_trx / dataset$Visa_fultimo_cierre_avg6
dataset$cpayroll_trx_divided_by_mcuentas_saldo = dataset$cpayroll_trx / dataset$mcuentas_saldo
dataset$cpayroll_trx_divided_by_Visa_fultimo_cierre_lag1 = dataset$cpayroll_trx / dataset$Visa_fultimo_cierre_lag1
dataset$cpayroll_trx_divided_by_a5p16 = dataset$cpayroll_trx / dataset$a5p16
dataset$cpayroll_trx_divided_by_a3p10 = dataset$cpayroll_trx / dataset$a3p10
dataset$cpayroll_trx_divided_by_Visa_fechaalta_delta3 = dataset$cpayroll_trx / dataset$Visa_fechaalta_delta3
dataset$cpayroll_trx_divided_by_ctrx_quarter = dataset$cpayroll_trx / dataset$ctrx_quarter
dataset$cpayroll_trx_divided_by_mactivos_margen = dataset$cpayroll_trx / dataset$mactivos_margen
dataset$cmobile_app_trx_lag3_divided_by_Master_fechaalta_delta2 = dataset$cmobile_app_trx_lag3 / dataset$Master_fechaalta_delta2
dataset$cmobile_app_trx_lag3_divided_by_ccomisiones_mantenimiento_delta2 = dataset$cmobile_app_trx_lag3 / dataset$ccomisiones_mantenimiento_delta2
dataset$cmobile_app_trx_lag3_divided_by_rf_040_044 = dataset$cmobile_app_trx_lag3 / dataset$rf_040_044
dataset$cmobile_app_trx_lag3_divided_by_mprestamos_personales = dataset$cmobile_app_trx_lag3 / dataset$mprestamos_personales
dataset$cmobile_app_trx_lag3_divided_by_Master_fultimo_cierre_avg6 = dataset$cmobile_app_trx_lag3 / dataset$Master_fultimo_cierre_avg6
dataset$cmobile_app_trx_lag3_divided_by_Visa_fultimo_cierre_avg6 = dataset$cmobile_app_trx_lag3 / dataset$Visa_fultimo_cierre_avg6
dataset$cmobile_app_trx_lag3_divided_by_mcuentas_saldo = dataset$cmobile_app_trx_lag3 / dataset$mcuentas_saldo
dataset$cmobile_app_trx_lag3_divided_by_Visa_fultimo_cierre_lag1 = dataset$cmobile_app_trx_lag3 / dataset$Visa_fultimo_cierre_lag1
dataset$cmobile_app_trx_lag3_divided_by_a5p16 = dataset$cmobile_app_trx_lag3 / dataset$a5p16
dataset$cmobile_app_trx_lag3_divided_by_a3p10 = dataset$cmobile_app_trx_lag3 / dataset$a3p10
dataset$cmobile_app_trx_lag3_divided_by_Visa_fechaalta_delta3 = dataset$cmobile_app_trx_lag3 / dataset$Visa_fechaalta_delta3
dataset$cmobile_app_trx_lag3_divided_by_ctrx_quarter = dataset$cmobile_app_trx_lag3 / dataset$ctrx_quarter
dataset$cmobile_app_trx_lag3_divided_by_mactivos_margen = dataset$cmobile_app_trx_lag3 / dataset$mactivos_margen
dataset$Master_fechaalta_delta2_divided_by_ccomisiones_mantenimiento_delta2 = dataset$Master_fechaalta_delta2 / dataset$ccomisiones_mantenimiento_delta2
dataset$Master_fechaalta_delta2_divided_by_rf_040_044 = dataset$Master_fechaalta_delta2 / dataset$rf_040_044
dataset$Master_fechaalta_delta2_divided_by_mprestamos_personales = dataset$Master_fechaalta_delta2 / dataset$mprestamos_personales
dataset$Master_fechaalta_delta2_divided_by_Master_fultimo_cierre_avg6 = dataset$Master_fechaalta_delta2 / dataset$Master_fultimo_cierre_avg6
dataset$Master_fechaalta_delta2_divided_by_Visa_fultimo_cierre_avg6 = dataset$Master_fechaalta_delta2 / dataset$Visa_fultimo_cierre_avg6
dataset$Master_fechaalta_delta2_divided_by_mcuentas_saldo = dataset$Master_fechaalta_delta2 / dataset$mcuentas_saldo
dataset$Master_fechaalta_delta2_divided_by_Visa_fultimo_cierre_lag1 = dataset$Master_fechaalta_delta2 / dataset$Visa_fultimo_cierre_lag1
dataset$Master_fechaalta_delta2_divided_by_a5p16 = dataset$Master_fechaalta_delta2 / dataset$a5p16
dataset$Master_fechaalta_delta2_divided_by_a3p10 = dataset$Master_fechaalta_delta2 / dataset$a3p10
dataset$Master_fechaalta_delta2_divided_by_Visa_fechaalta_delta3 = dataset$Master_fechaalta_delta2 / dataset$Visa_fechaalta_delta3
dataset$Master_fechaalta_delta2_divided_by_ctrx_quarter = dataset$Master_fechaalta_delta2 / dataset$ctrx_quarter
dataset$Master_fechaalta_delta2_divided_by_mactivos_margen = dataset$Master_fechaalta_delta2 / dataset$mactivos_margen
dataset$ccomisiones_mantenimiento_delta2_divided_by_rf_040_044 = dataset$ccomisiones_mantenimiento_delta2 / dataset$rf_040_044
dataset$ccomisiones_mantenimiento_delta2_divided_by_mprestamos_personales = dataset$ccomisiones_mantenimiento_delta2 / dataset$mprestamos_personales
dataset$ccomisiones_mantenimiento_delta2_divided_by_Master_fultimo_cierre_avg6 = dataset$ccomisiones_mantenimiento_delta2 / dataset$Master_fultimo_cierre_avg6
dataset$ccomisiones_mantenimiento_delta2_divided_by_Visa_fultimo_cierre_avg6 = dataset$ccomisiones_mantenimiento_delta2 / dataset$Visa_fultimo_cierre_avg6
dataset$ccomisiones_mantenimiento_delta2_divided_by_mcuentas_saldo = dataset$ccomisiones_mantenimiento_delta2 / dataset$mcuentas_saldo
dataset$ccomisiones_mantenimiento_delta2_divided_by_Visa_fultimo_cierre_lag1 = dataset$ccomisiones_mantenimiento_delta2 / dataset$Visa_fultimo_cierre_lag1
dataset$ccomisiones_mantenimiento_delta2_divided_by_a5p16 = dataset$ccomisiones_mantenimiento_delta2 / dataset$a5p16
dataset$ccomisiones_mantenimiento_delta2_divided_by_a3p10 = dataset$ccomisiones_mantenimiento_delta2 / dataset$a3p10
dataset$ccomisiones_mantenimiento_delta2_divided_by_Visa_fechaalta_delta3 = dataset$ccomisiones_mantenimiento_delta2 / dataset$Visa_fechaalta_delta3
dataset$ccomisiones_mantenimiento_delta2_divided_by_ctrx_quarter = dataset$ccomisiones_mantenimiento_delta2 / dataset$ctrx_quarter
dataset$ccomisiones_mantenimiento_delta2_divided_by_mactivos_margen = dataset$ccomisiones_mantenimiento_delta2 / dataset$mactivos_margen
dataset$rf_040_044_divided_by_mprestamos_personales = dataset$rf_040_044 / dataset$mprestamos_personales
dataset$rf_040_044_divided_by_Master_fultimo_cierre_avg6 = dataset$rf_040_044 / dataset$Master_fultimo_cierre_avg6
dataset$rf_040_044_divided_by_Visa_fultimo_cierre_avg6 = dataset$rf_040_044 / dataset$Visa_fultimo_cierre_avg6
dataset$rf_040_044_divided_by_mcuentas_saldo = dataset$rf_040_044 / dataset$mcuentas_saldo
dataset$rf_040_044_divided_by_Visa_fultimo_cierre_lag1 = dataset$rf_040_044 / dataset$Visa_fultimo_cierre_lag1
dataset$rf_040_044_divided_by_a5p16 = dataset$rf_040_044 / dataset$a5p16
dataset$rf_040_044_divided_by_a3p10 = dataset$rf_040_044 / dataset$a3p10
dataset$rf_040_044_divided_by_Visa_fechaalta_delta3 = dataset$rf_040_044 / dataset$Visa_fechaalta_delta3
dataset$rf_040_044_divided_by_ctrx_quarter = dataset$rf_040_044 / dataset$ctrx_quarter
dataset$rf_040_044_divided_by_mactivos_margen = dataset$rf_040_044 / dataset$mactivos_margen
dataset$mprestamos_personales_divided_by_Master_fultimo_cierre_avg6 = dataset$mprestamos_personales / dataset$Master_fultimo_cierre_avg6
dataset$mprestamos_personales_divided_by_Visa_fultimo_cierre_avg6 = dataset$mprestamos_personales / dataset$Visa_fultimo_cierre_avg6
dataset$mprestamos_personales_divided_by_mcuentas_saldo = dataset$mprestamos_personales / dataset$mcuentas_saldo
dataset$mprestamos_personales_divided_by_Visa_fultimo_cierre_lag1 = dataset$mprestamos_personales / dataset$Visa_fultimo_cierre_lag1
dataset$mprestamos_personales_divided_by_a5p16 = dataset$mprestamos_personales / dataset$a5p16
dataset$mprestamos_personales_divided_by_a3p10 = dataset$mprestamos_personales / dataset$a3p10
dataset$mprestamos_personales_divided_by_Visa_fechaalta_delta3 = dataset$mprestamos_personales / dataset$Visa_fechaalta_delta3
dataset$mprestamos_personales_divided_by_ctrx_quarter = dataset$mprestamos_personales / dataset$ctrx_quarter
dataset$mprestamos_personales_divided_by_mactivos_margen = dataset$mprestamos_personales / dataset$mactivos_margen
dataset$Master_fultimo_cierre_avg6_divided_by_Visa_fultimo_cierre_avg6 = dataset$Master_fultimo_cierre_avg6 / dataset$Visa_fultimo_cierre_avg6
dataset$Master_fultimo_cierre_avg6_divided_by_mcuentas_saldo = dataset$Master_fultimo_cierre_avg6 / dataset$mcuentas_saldo
dataset$Master_fultimo_cierre_avg6_divided_by_Visa_fultimo_cierre_lag1 = dataset$Master_fultimo_cierre_avg6 / dataset$Visa_fultimo_cierre_lag1
dataset$Master_fultimo_cierre_avg6_divided_by_a5p16 = dataset$Master_fultimo_cierre_avg6 / dataset$a5p16
dataset$Master_fultimo_cierre_avg6_divided_by_a3p10 = dataset$Master_fultimo_cierre_avg6 / dataset$a3p10
dataset$Master_fultimo_cierre_avg6_divided_by_Visa_fechaalta_delta3 = dataset$Master_fultimo_cierre_avg6 / dataset$Visa_fechaalta_delta3
dataset$Master_fultimo_cierre_avg6_divided_by_ctrx_quarter = dataset$Master_fultimo_cierre_avg6 / dataset$ctrx_quarter
dataset$Master_fultimo_cierre_avg6_divided_by_mactivos_margen = dataset$Master_fultimo_cierre_avg6 / dataset$mactivos_margen
dataset$Visa_fultimo_cierre_avg6_divided_by_mcuentas_saldo = dataset$Visa_fultimo_cierre_avg6 / dataset$mcuentas_saldo
dataset$Visa_fultimo_cierre_avg6_divided_by_Visa_fultimo_cierre_lag1 = dataset$Visa_fultimo_cierre_avg6 / dataset$Visa_fultimo_cierre_lag1
dataset$Visa_fultimo_cierre_avg6_divided_by_a5p16 = dataset$Visa_fultimo_cierre_avg6 / dataset$a5p16
dataset$Visa_fultimo_cierre_avg6_divided_by_a3p10 = dataset$Visa_fultimo_cierre_avg6 / dataset$a3p10
dataset$Visa_fultimo_cierre_avg6_divided_by_Visa_fechaalta_delta3 = dataset$Visa_fultimo_cierre_avg6 / dataset$Visa_fechaalta_delta3
dataset$Visa_fultimo_cierre_avg6_divided_by_ctrx_quarter = dataset$Visa_fultimo_cierre_avg6 / dataset$ctrx_quarter
dataset$Visa_fultimo_cierre_avg6_divided_by_mactivos_margen = dataset$Visa_fultimo_cierre_avg6 / dataset$mactivos_margen
dataset$mcuentas_saldo_divided_by_Visa_fultimo_cierre_lag1 = dataset$mcuentas_saldo / dataset$Visa_fultimo_cierre_lag1
dataset$mcuentas_saldo_divided_by_a5p16 = dataset$mcuentas_saldo / dataset$a5p16
dataset$mcuentas_saldo_divided_by_a3p10 = dataset$mcuentas_saldo / dataset$a3p10
dataset$mcuentas_saldo_divided_by_Visa_fechaalta_delta3 = dataset$mcuentas_saldo / dataset$Visa_fechaalta_delta3
dataset$mcuentas_saldo_divided_by_ctrx_quarter = dataset$mcuentas_saldo / dataset$ctrx_quarter
dataset$mcuentas_saldo_divided_by_mactivos_margen = dataset$mcuentas_saldo / dataset$mactivos_margen
dataset$Visa_fultimo_cierre_lag1_divided_by_a5p16 = dataset$Visa_fultimo_cierre_lag1 / dataset$a5p16
dataset$Visa_fultimo_cierre_lag1_divided_by_a3p10 = dataset$Visa_fultimo_cierre_lag1 / dataset$a3p10
dataset$Visa_fultimo_cierre_lag1_divided_by_Visa_fechaalta_delta3 = dataset$Visa_fultimo_cierre_lag1 / dataset$Visa_fechaalta_delta3
dataset$Visa_fultimo_cierre_lag1_divided_by_ctrx_quarter = dataset$Visa_fultimo_cierre_lag1 / dataset$ctrx_quarter
dataset$Visa_fultimo_cierre_lag1_divided_by_mactivos_margen = dataset$Visa_fultimo_cierre_lag1 / dataset$mactivos_margen
dataset$a5p16_divided_by_a3p10 = dataset$a5p16 / dataset$a3p10
dataset$a5p16_divided_by_Visa_fechaalta_delta3 = dataset$a5p16 / dataset$Visa_fechaalta_delta3
dataset$a5p16_divided_by_ctrx_quarter = dataset$a5p16 / dataset$ctrx_quarter
dataset$a5p16_divided_by_mactivos_margen = dataset$a5p16 / dataset$mactivos_margen
dataset$a3p10_divided_by_Visa_fechaalta_delta3 = dataset$a3p10 / dataset$Visa_fechaalta_delta3
dataset$a3p10_divided_by_ctrx_quarter = dataset$a3p10 / dataset$ctrx_quarter
dataset$a3p10_divided_by_mactivos_margen = dataset$a3p10 / dataset$mactivos_margen
dataset$Visa_fechaalta_delta3_divided_by_ctrx_quarter = dataset$Visa_fechaalta_delta3 / dataset$ctrx_quarter
dataset$Visa_fechaalta_delta3_divided_by_mactivos_margen = dataset$Visa_fechaalta_delta3 / dataset$mactivos_margen
dataset$ctrx_quarter_divided_by_mactivos_margen = dataset$ctrx_quarter / dataset$mactivos_margen


#------------------------------------------------------------------------------
#Elimino las variables que no son tan importantes en el dataset

print("after many divisions")
print(ncol( dataset ))
print(nrow( dataset ))

#------------------------------------------------------------------------------
#grabo el dataset
fwrite( dataset,
        "dataset2_FE_8_meses.csv.gz",
        logical01= TRUE,
        sep= "," )

CanaritosAsesinos( canaritos_ratio = 0.3 )
print("after CanaritosAsesinos( canaritos_ratio = 0.3 )")
print(ncol( dataset ))
print(nrow( dataset ))

fwrite( dataset,
        "dataset2_FE_8_meses_canarito_03.csv.gz",
        logical01= TRUE,
        sep= "," )
