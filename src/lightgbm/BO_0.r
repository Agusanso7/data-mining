# Este script esta pensado para correr en Google Cloud
# Eliminar la vm una vez que termine
#   8 vCPU
#  16 GB memoria RAM
# 256 GB espacio en disco

# Optimizacion Bayesiana de hiperparametros de  lightgbm, con el metodo TRADICIONAL de los hiperparametros originales de lightgbm
# 5-fold cross validation
# la probabilidad de corte es un hiperparametro

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")

require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


kBO_iter  <- 100   #cantidad de iteraciones de la Optimizacion Bayesiana

#Aqui se cargan los hiperparametros
hs <- makeParamSet( 
    #makeNumericParam("feature_fraction", lower=  0.001, upper=1) # lower=  0.01 , upper=    0.3)
         #makeNumericParam("learning_rate", lower=  0.01, upper=0.03), # lower=  0.01 , upper=    0.3),
         #makeNumericParam("feature_fraction", lower=  0.45  , upper=    0.8), # lower=  0.2  , upper=    1.0),
         #makeIntegerParam("min_data_in_leaf", lower=  500    , upper= 4000), # lower=  0    , upper= 8000),
         #makeIntegerParam("num_leaves",       lower= 2500L   , upper= 6000L), # lower= 16L   , upper= 1024L),
         #makeNumericParam("prob_corte",       lower= 1/80  , upper=  1/20) # lower= 1/80  , upper=  1/20) 
                        #esto sera visto en clase en gran detalle ## esto antes era 1/40 !!!!!
         # forbidden = quote( minbucket > 0.5*minsplit ) )
 
            makeNumericParam("learning_rate",       lower= 0.001   , upper= 0.25) 
            #makeIntegerParam("num_leaves",       lower= 1L   , upper= 8000L)
        )

#------------------------------------------------------------------------------
#esta funcion calcula internamente la ganancia de la prediccion probs
fganancia_logistic_lightgbm   <- function( probs, datos) 
{
  vlabels  <- get_field(datos, "label")

  gan  <- sum( (probs > PROB_CORTE  ) *
               ifelse( vlabels== 1, 78000, -2000 ) )


  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales, la semilla del mal ...

EstimarGanancia_lightgbm  <- function( x )
{
  gc()  #libero memoria

  PROB_CORTE <<- 1/40 #x$prob_corte   #asigno la variable global

  kfolds  <- 5   # cantidad de folds para cross validation

    GANANCIAS = c()

    writeLines(toString(x$learning_rate), stderr())

    for(semilla in semillas) {
        param_basicos  <- list( objective= "binary",
                                metric= "custom",
                                first_metric_only= TRUE,
                                boost_from_average= TRUE,
                                feature_pre_filter= FALSE,
                                verbosity= -100,
                                max_depth=  3, #[BO]         # -1 significa no limitar,  por ahora lo dejo fijo [3 candidato]
                                num_iterations = 3000, # default 100

                                #min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                                #lambda_l1= 0.0, #0.0,         #por ahora, lo dejo fijo
                                #lambda_l2= 0.0,         #por ahora, lo dejo fijo
                                #max_bin= 50,            #por ahora, lo dejo fijo
                                #num_iterations= 250,   #un numero muy grande, lo limita early_stopping_rounds
                                force_row_wise= TRUE,   #para que los alumnos no se atemoricen con tantos warning
                                seed= semilla,

                                learning_rate=x$learning_rate,
                                feature_fraction=0.44,
                                min_data_in_leaf=1100
                                #num_leaves=64
                                )

        #el parametro discolo, que depende de otro
        param_variable  <- list(  early_stopping_rounds= as.integer(50 + 5/x$learning_rate) )

        param_completo  <- c( param_basicos, param_variable, x )

        set.seed( semilla )
        modelocv  <- lgb.cv( data= dtrain,
                            verbose= -1, 
                            eval= fganancia_logistic_lightgbm,
                            stratified= TRUE, #sobre el cross validation
                            nfold= kfolds,    #folds del cross validation
                            param= param_completo,
                            )

        #obtengo la ganancia
        ganancia  <- unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ]

        writeLines("best iter:", stderr())
        writeLines(toString(modelocv$best_iter), stderr())
        writeLines("ganancia:", stderr())
        ganancia_normalizada  <-  ganancia* kfolds     #normailizo la ganancia
        writeLines(toString(ganancia_normalizada), stderr())

        GANANCIAS = c(GANANCIAS, ganancia_normalizada )
    }

  attr(ganancia_normalizada ,"extras" )  <- list("num_iterations"= modelocv$best_iter)  #esta es la forma de devolver un parametro extra

  param_completo$num_iterations <- modelocv$best_iter  #asigno el mejor num_iterations
  param_completo["early_stopping_rounds"]  <- NULL     #elimino de la lista el componente  "early_stopping_rounds"

    xx  <- param_completo
    # learning_rate, feature_fraction, min_data_in_leaf, num_leaves, prob_corte, num_iterations, ganancia
  line = paste(c("learning_rate: ", x$learning_rate, 
                    ", feature_fraction: ", x$feature_fraction, 
                    ", min_data_in_leaf: ", xx$min_data_in_leaf, 
                    ", num_leaves: ", xx$num_leaves, 
                    ", prob_corte: ", xx$prob_corte, 
                    ", num_iterations: ", xx$num_iterations, 
                    ", max_depth: ", x$max_depth, 
                    ", ganancia: ", mean(GANANCIAS), ", ganancia_sd: ", sd(GANANCIAS), sep=" "))
  writeLines(toString(line), stderr())

  return( mean(GANANCIAS) )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

#Aqui se debe poner la carpeta de la computadora local
#setwd("~/buckets/b1/")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar el modelo
dataset  <- fread("./datasets/competencia1_2022.csv")

for (col_name in colnames(dataset)) {
        dataset[[paste("is_null_", col_name, sep="")]] = is.na(dataset[[col_name]])
}


#en estos archivos quedan los resultados
kbayesiana  <- "HT5330_learning_rate.RDATA"

semillas = c(1, 2, 3, 4, 5)

#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ foto_mes==202101, clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]


#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ foto_mes==202101, campos_buenos, with=FALSE]),
                        label= dataset[ foto_mes==202101, clase01 ] )


#Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar  <- EstimarGanancia_lightgbm   #la funcion que voy a maximizar

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar, #la funcion que voy a maximizar
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,     #definido al comienzo del programa
              has.simple.signature = FALSE   #paso los parametros en una lista
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)  #se graba cada 600 segundos
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )   #cantidad de iteraciones
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )

#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
run  <- mbo(obj.fun, learner= surr.km, control= ctrl)


quit( save="no" )


# pero nosotros  NO nos vamos a quedar tranquilos sin cuestionar los hiperparametros originales
# min_data_in_leaf  y  num_leaves   estan relacionados entre ellos
