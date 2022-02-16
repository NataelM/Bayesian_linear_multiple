setwd("C:/Users/jonat/Downloads/BAYE/proyecto final RLM")
library(dplyr)
library(psych)
library(GGally)
library(ggplot2)
library(lmtest)
library(corrplot)
library(car)
library(gridExtra)
library(grid)
library(lattice)
library(sqldf)
library(rjags)
library(nortest)
library(lmtest)
library(fpp)
library(MASS)
library(rriskDistributions)

db <- read.csv('DRAGONBALL-LEGENDS.csv', header = T)
options(scipen=999) # quita formato de notacion cientifica
db$X <- NULL
db$X.1 <- NULL

#se agregan tres ceros como cadena para que en el siguiente bloque de codigo
#cuando se sustituya la K por 3 ceros no se afecten aquellos regustron que no
#tienen k
db$POWER <- paste(db$POWER, '000', sep = '')
db$HEALTH <- paste(db$HEALTH, '000', sep = '')
db$STRIKE.ATTACK <- paste(db$STRIKE.ATTACK, '000', sep = '')
db$STRIKE.DEFENCE <- paste(db$STRIKE.DEFENCE, '000', sep = '')
db$BLAST.ATTACK <- paste(db$BLAST.ATTACK, '000', sep = '')
db$BLAST.DEFENCE <- paste(db$BLAST.DEFENCE, '000', sep = '')


#cambiamos las k por 3 ceros, convertimos a numericos los string,
#y dividimos enytre 1000 para quitar los tres ceros del principio
db$POWER <- as.numeric(gsub('K', '000', db$POWER)) * 1000
db$HEALTH <- as.numeric(gsub('K', '000', db$HEALTH)) * 1000
db$STRIKE.ATTACK <- as.numeric(gsub('K', '000', db$STRIKE.ATTACK)) * 1000
db$STRIKE.DEFENCE <- as.numeric(gsub('K', '000', db$STRIKE.DEFENCE)) * 1000
db$BLAST.ATTACK <- as.numeric(gsub('K', '000', db$BLAST.ATTACK)) * 1000
db$BLAST.DEFENCE <- as.numeric(gsub('K', '000', db$BLAST.DEFENCE)) * 1000


#tipos de datos a manejar
str(db)

##### dado que el nombre del personaje se repite muchas veces, a pesar de 
#tener id_Card distinto para trabajar con personajes unicos, lo haremos
#tomando el promedio de los registros por personaje

#cambio de nombres en los encabezados porque en sentencias sql no se 
#aceptan nombres con puntos
names(db) <- c("CHARACTER", "CARD_NUMBER", "POWER", "HEALTH",        
               "STRIKE_ATTACK", "STRIKE_DEFENCE", "BLAST_ATTACK",
               "BLAST_DEFENCE" )

db <- sqldf('select 
            CHARACTER,
            CARD_NUMBER,
            median(POWER) as POWER,
            median(HEALTH) as HEALTH,
            median(STRIKE_ATTACK) as STRIKE_ATTACK,
            median(STRIKE_DEFENCE) as STRIKE_DEFENCE,
            median(BLAST_ATTACK) as BLAST_ATTACK,
            median(BLAST_DEFENCE) as BLAST_DEFENCE
            FROM db group by CHARACTER')


#variables numericas
dball <- db[c(3:8)]
########################
### como primer paso veremos la correlacion que 
# hay entre variables
# agarramos un subconjunto del dataset original
# para trabajarlo
#sacamos las correlaciones
cor(x = dball, method = 'pearson')

## vemos el comportamiento/distribucion de las variables
#par(mar=c(4, 4, 4, 4))
multi.hist(x = dball, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = names(dball) , mar = c(2.5, 2.5, 1, 2))
#las graficas anteriores muestran un histograma de las variables,
#también muestran su densidad (linea punteada con azul), y la linea roja son
#normales ajustadas 


#boxplot
boxplot(dball, col = c(1:5))


######diagrama de dispersion
ggpairs(dball, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

#un analisis preliminar de los resultados mostrados anterioremente muestra que 
# en general todas las variables tienen una correlacion positiva con nuestra 
#variable objetivo, 'POWER', quizá para nuestro modelo de regresion multiple
# sea bueno eliminar la variable 'Blast atack' ya que fue la mostró menor 
# correlacion con las demás variables


###### generacion del modelo
attach(dball)
db_model <- lm(POWER ~ HEALTH+STRIKE_ATTACK+STRIKE_DEFENCE+BLAST_ATTACK+
                 BLAST_DEFENCE)
vif(db_model)
summary(db_model)
confint(db_model)

#se puede mejorar con una apriori lasso en la parte bayesiana

# apesar de que el intercepto no atraviesa el cero,
#presentamos variables colineales 

# podemos observar un R^2 de .9957 para el primer modelo 'db_model'
#lo cual no indica que el power está explicado 
# por el 99.58% del modelo y dado el p-value obtenido, 
#podemos decir que que el modelo está bien explicado sin embargo hay
#multicolinealidad en dos variables

##################### modelos con 3 variables ########################

###
#mod1 STRIKE, R = 0.9553
m1 <- lm(POWER ~ HEALTH + STRIKE_ATTACK + STRIKE_DEFENCE)
vif(m1)
summary(m1)
confint(m1)
#el intercepto atraviesa el cero lo que nos podria causar 
#problemas en el modelo por esta razón este modelo no será
#considerado para aplicarle las pruebas (distribucional,
#var cte y no correlacion)

#mod2 BLAST, R = 0.953
m2 <- lm(POWER ~ HEALTH + BLAST_ATTACK + BLAST_DEFENCE)
vif(m2)
summary(m2)
confint(m2)
#ninguna variable atraviesa el cero 

#mod3 ATTACK sugerido por el VIF R = 0.9692
m3 <- lm(POWER ~ HEALTH + STRIKE_ATTACK + BLAST_ATTACK)
vif(m3)
summary(m3)
confint(m3)
#ninguna de las variables atraviesa el cero

#mod4 DEFENCE R = 0.9052
m4 <- lm(POWER ~ HEALTH + STRIKE_DEFENCE + BLAST_DEFENCE)
vif(m4)
#este modelo se deshecha pues el VIF es en las variables defensivas
#pasan el 10 
summary(m4)
confint(m4)
#aunque en este modelo ninguna variable contiene al cero, el modelo es
#multicolineal

#modelo blast + strike attack
m5 <- lm(formula = POWER ~ HEALTH + BLAST_ATTACK 
          + BLAST_DEFENCE+ STRIKE_ATTACK)
vif(m5)
summary(m5)
confint(m5)
plot(m5)
#ninguna de las variables atraviesa el cero

####################### comprobacion de supuestos #######################
#los residuales distribuyen normal?

#ahora probaremos(cumplimiento de supuestos) los modelos m2 y m3
#ya que db_model y m4 tienen variables multicolineales no probaremos
#los supuestos y para m1 dado que B_0 atraviesa el cero en los 
#intrvalos de confianza
#################### residuales normales #####################
residus <- data.frame(m2$residuals, m3$residuals,m5$residuals )
multi.hist(x = residus, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = names(residus) , mar = c(2.5, 2.5, 1, 2))

#se puede observar que la densidad de los residules (linea punteada en azul)
#al menos graficamente se acerca a la linea roja la cual es una normal ajustada
#a los datos

###### residuales normales
par(mar=c(1.5, 1.5, 1.5, 1.5)) #para los margenes de los graficos
par(mfrow=c(3, 1)) 

qqnorm(m2$residuals, main = 'm2')
qqline(m2$residuals, col = 'red', lwd = 2)

qqnorm(m3$residuals, main = 'm3')
qqline(m3$residuals, col = 'red', lwd = 2)

qqnorm(m5$residuals, main = 'm5')
qqline(m5$residuals, col = 'red', lwd = 2)

#graficamente pareciera que los residuales en los modelos
#sí se distribuyen normal excepto en m5

#pruebas de hipotesis para normalidad 
######################## anderson darling
#H0:LOS DATOS DISTRIBUYEN NORMAL 
#H1: LOS DATOS NO SITRIBUYEN NORMAL 
#SI EL P_VALUE ES > 0.05 SE ACEPTA H0

ad.test(m2$residuals)
#los residuales de m2 si distribuyen de manera normal

ad.test(m3$residuals)
#rechaza la hipotesis nula -> no es normal

ad.test(m5$residuals)
#rechaza la hipotesis nula -> no es normal

lillie.test(m2$residuals)
#nuevamente los residuales distribuyen de manera normal

lillie.test(m3$residuals)
#rechaza la hipotesis nula -> no es normal

lillie.test(m5$residuals)
#rechaza la hipotesis nula -> no es normal

#################### varianza constante
#h_0: VARIANZA CTE
#H1 : VARIANZA NO CTE
# SI P_VALUE > 0.05 NO SE RECHAZA H0
#h_0: varianza cte
bptest(m2)
#varianza no cte
bptest(m3)
#varianza no cte pero es la mas cercana a superar el 0.05
bptest(m5)
#elmodelo 5 si tiene varianza constante

#################### no correlacion
#h_0: no correlacion
dwtest(m2)
dwtest(m3)
dwtest(m5)
#los modelos pasan la prueba de no correlacion

#m2 y m5 fueron los modelos que pasaron más pruebas sin embargo, 
#m5 pasó la prueba de varianza cte por lo que decidiremos modelar con m5
#pues la prueba que no pasó fue la de dristibucion normal en los residuales

###### encontrar una distribucion para los residuales
#intento de ajustar a una distribucion los datos, pero no,
#por lo tanto sólo podemos decir que los residuales no ajustan
#a alguna distribucion

#fit.cont(m5$residuals) 
#m5_res <- fitdistr(m5$residuals, "t")

#ks.test(m5$res, "pt")
#ad.test(m5$residuals)


################################### Extra  ##########################
##### identificacion de posibles valores atipicos e 
#influyentes en el modelo
dball$studentized_residual <- rstudent(m5)
ggplot(data = dball, aes(x = predict(db_model), y = abs(studentized_residual))) +
  geom_hline(yintercept = 3, color = "grey", linetype = "dashed") +
  # se identifican en rojo observaciones con residuos estandarizados absolutos > 3
  geom_point(aes(color = ifelse(abs(studentized_residual) > 3, 'red', 'black'))) +
  scale_color_identity() +
  labs(title = "Distribución de los residuos studentized",
       x = "predicción modelo") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

which(abs(dball$studentized_residual) > 3)

#summary(influence.measures(m5))

#todas las observaciones con * están influenciadas por las variables 
#explicativas
par(mfrow=c(1, 1)) 
par(mar=c(2, 2, 2, 2))
influencePlot(m5)

# el resultado muestra aquellas observaciones que se ven  más influidas 
#por el modelo, una alternativa sería eliminar aquellas observaciones y de nuevo
# correr el modelo y ver el comportamiento del nuevo 


############### Modelo bayesiano ajua ######################

n = nrow(dball)

#lista de datos a atribuir

data <- list(
  Y = POWER,#variable a predeciIR
  X_1 = HEALTH,
  X_2 = BLAST_ATTACK,
  X_3 = BLAST_DEFENCE,
  X_4 = STRIKE_ATTACK,
  n = n,
  #vector de medias
  # agregar tantos ceros como betas(PARAMETROS A ESTIMAR)
  zeros = c(0, 0, 0, 0, 0),
  diagonal = diag(1/1000, 5) 
)                       

param <- c('Beta', 'Sigma_2') #no es necesario declarar todas las betas porque
#se está declarando como vector

#funcion de valores iniciales
inits <- function(){
  list(
    'Beta' = rnorm(5, 0, 1), #distribucion poco infirmativa de las betas
    'Tau' = rgamma(1, 1, 1)
  )
}

modelo_bayes <- 'model{
  #prior
  Beta ~ dmnorm(zeros, diagonal) #cambiar por normales univariadas
  Tau ~ dgamma(0.001, 0.001)      #
  Sigma_2 <- (1/Tau)
  
  #modelo
  for(i in 1:n){
    RLM[i] <- Beta[1] + (Beta[2]*X_1[i]) + 
              (Beta[3]*X_2[i]) +
               (Beta[4]*X_3[i]) + 
              (Beta[5]*X_4[i] )
          
    Y[i] ~ dnorm(RLM[i], Tau)
  }

}'

#fit

fit <- jags.model(textConnection(modelo_bayes),
                  data,
                  inits,
                  n.chains = 3
                  )

update(fit, 1000)

sample <- coda.samples(fit, param, n.iter = 4000, 
                       thin = 1)

#inspeccionar la parte posterior
plot(sample)
##################### la varianza tan grande se debe a la escala de los datos
########e.g dividir todo el data set entre 100
##### no hay problema porque en la traza converge en la sigma_2
####

#residuales bayesiano
#histograma
#matriz diseño
aux = cbind(rep(2, n), HEALTH, BLAST_ATTACK, BLAST_DEFENCE,
            STRIKE_ATTACK)

aux_cade = do.call(rbind, sample)

aux_params = colMeans(aux_cade)


#claculo de los residuales
ajustado = drop(aux %*% aux_params[1:5])
residuales = POWER - ajustado 
hist(residuales, col = 124, breaks = 30, freq = TRUE,
     main = 'Residuales Bayesianos')

#tienen que ver con la kurtosis de los datos
jarque.bera.test(residuales)
lillie.test(residuales)

m5 ;aux_params


######################### testeo #####################
p_1 <- db[1,3]#poder
prueba_1 <- as.numeric(c(1, db[1,c(4, 7, 8, 5)])) #variables predictivas
clasico <- m5$coefficients 

sum(clasico*prueba_1);p_1 #estimacion con el modelo clasico

bayesiano <- aux_params[1:5]

sum(bayesiano*prueba_1);p_1 #estimacion con el modelo bayesiano

