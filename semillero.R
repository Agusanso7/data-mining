# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM
# 256 GB espacio en disco

# son varios archivos, subirlos INTELIGENTEMENTE a Kaggle

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("primes")
require("lightgbm")

kdataset       <- "/Users/asansone/Desktop/data-mining/dataset2_FE_8_meses_2cocientes.csv.gz" # QUEDA ASI
ksemilla_azar  <- 102191  #Aqui poner la propia semilla
ktraining      <- c( 202101 ) # QUEDA ASI
# hasta 202006
kfuture        <- c( 202103 )   #periodo donde aplico el modelo final


kexperimento   <- "dataset2_FE_8_meses_2cocientes_FINAL_1m"

ksemilla_primos  <-  3245234
ksemillerio  <- 50

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

#genero un vector de una cantidad de PARAM$semillerio  de semillas,  buscando numeros primos al azar
primos  <- generate_primes(min=100000, max=1000000)  #genero TODOS los numeros primos entre 100k y 1M
set.seed( ksemilla_primos ) #seteo la semilla que controla al sample de los primos
ksemillas  <- sample(primos)[ 1:ksemillerio ]   #me quedo con PARAM$semillerio primos al azar

#cargo el dataset donde voy a entrenar
dataset  <- fread(kdataset, stringsAsFactors= TRUE)


#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )

#--------------------------------------


#establezco donde entreno
dataset[ , train  := 0L ]
dataset[ foto_mes %in% ktraining, train  := 1L ]


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01], max_bin=30) # , max_bin=170

dapply  <- dataset[ foto_mes== kfuture ]


tb_prediccion_semillerio  <- dapply[  , list(numero_de_cliente) ]
tb_prediccion_semillerio[ , pred_acumulada := 0L ]

index = -1
ksemillas  <- sample(primos)[ 1:ksemillerio ]
for( semilla  in  ksemillas ) {
  #genero el modelo
  #estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
  index = index + 1
  print(index)
  set.seed( semilla )

  modelo  <- lgb.train( data= dtrain,
                      param= list( objective=          "binary",
                                  force_row_wise= TRUE, 
                                  boost_from_average= TRUE,
                                    num_iterations=     187,
                                    num_leaves=         796,
                                    min_data_in_leaf=   1,
                                    learning_rate=      0.0181,
                                    feature_fraction=   0.396,
                                    seed=               semilla
                                  )
                    )     

  prediccion  <- predict( modelo, data.matrix( dapply[, campos_buenos, with=FALSE ]) )

  #calculo el ranking
  prediccion_semillerio  <- frank( prediccion) #,  ties.method= "random" )

  #acumulo el ranking de la prediccion
  tb_prediccion_semillerio[ , paste0( "pred_", semilla ) :=  prediccion ]
  tb_prediccion_semillerio[ , pred_acumulada := pred_acumulada + prediccion_semillerio ]
}

tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes ) ]
tb_entrega[  , prob := tb_prediccion_semillerio$pred_acumulada ]

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )

#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 8500, 11500, by=500 )
for( envios  in  cortes )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]

  fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0(  kexperimento, "_50_", envios, ".csv" ),
          sep= "," )
}

ksemillas  <- sample(primos)[ 1:ksemillerio ]
for( semilla  in  ksemillas ) {
  #genero el modelo
  #estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
  index = index + 1
  print(index)
  set.seed( semilla )

  modelo  <- lgb.train( data= dtrain,
                      param= list( objective=          "binary",
                                  force_row_wise= TRUE, 
                                  boost_from_average= TRUE,
                                    num_iterations=     187,
                                    num_leaves=         796,
                                    min_data_in_leaf=   1,
                                    learning_rate=      0.0181,
                                    feature_fraction=   0.396,
                                    seed=               semilla
                                  )
                    ) # semillas 100                                        

  prediccion  <- predict( modelo, data.matrix( dapply[, campos_buenos, with=FALSE ]) )

  #calculo el ranking
  prediccion_semillerio  <- frank( prediccion) #,  ties.method= "random" )

  #acumulo el ranking de la prediccion
  tb_prediccion_semillerio[ , paste0( "pred_", semilla ) :=  prediccion ]
  tb_prediccion_semillerio[ , pred_acumulada := pred_acumulada + prediccion_semillerio ]
}

tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes ) ]
tb_entrega[  , prob := tb_prediccion_semillerio$pred_acumulada ]

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )

#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 8500, 11500, by=500 )
for( envios  in  cortes )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]

  fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0(  kexperimento, "_100_", envios, ".csv" ),
          sep= "," )
}


ksemillas  <- sample(primos)[ 1:ksemillerio ]
for( semilla  in  ksemillas ) {
  #genero el modelo
  #estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
  index = index + 1
  print(index)
  set.seed( semilla )

  modelo  <- lgb.train( data= dtrain,
                      param= list( objective=          "binary",
                                  force_row_wise= TRUE, 
                                  boost_from_average= TRUE,
                                    num_iterations=     187,
                                    num_leaves=         796,
                                    min_data_in_leaf=   1,
                                    learning_rate=      0.0181,
                                    feature_fraction=   0.396,
                                    seed=               semilla
                                  )
                    ) # semillas 100                                        

  prediccion  <- predict( modelo, data.matrix( dapply[, campos_buenos, with=FALSE ]) )

  #calculo el ranking
  prediccion_semillerio  <- frank( prediccion) #,  ties.method= "random" )

  #acumulo el ranking de la prediccion
  tb_prediccion_semillerio[ , paste0( "pred_", semilla ) :=  prediccion ]
  tb_prediccion_semillerio[ , pred_acumulada := pred_acumulada + prediccion_semillerio ]
}

tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes ) ]
tb_entrega[  , prob := tb_prediccion_semillerio$pred_acumulada ]

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )

#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 8500, 11500, by=500 )
for( envios  in  cortes )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]

  fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0(  kexperimento, "_150_", envios, ".csv" ),
          sep= "," )
}
