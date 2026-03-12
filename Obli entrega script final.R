install.packages("ranger")
install.packages("DALEX")


library(glmnet)
library(ISLR)
library(MASS)
library(h2o)
library(ranger)
library(DALEX)


data("Hitters")


str(Hitters)
Hitters= na.omit(Hitters)

#visualizamos la data graficamente
boxplot(Hitters$Salary)
boxplot(log(Hitters$Salary))

plot(Hitters$Years,Hitters$Salary)
Model=lm(Hitters$Salary~Hitters$Years)
abline(Model)
legend("topright",legend=paste("R2 is", format(summary(Model)$r.squared,digits=3)))

plot(Hitters$Years,log(Hitters$Salary))
Model=lm(log(Hitters$Salary)~Hitters$Years)
abline(Model)
legend("topright",legend=paste("R2 is", format(summary(Model)$r.squared,digits=3)))

plot(log(Hitters$Years),log(Hitters$Salary))
Model=lm(log(Hitters$Salary)~log(Hitters$Years))
abline(Model)
legend("topright",legend=paste("R2 is", format(summary(Model)$r.squared,digits=3)))


#Creamos nueva variable para diferenciar a quienes tuvieron un contrato y quienes no
Hitters$PrimerContrato=ifelse(Hitters$Years>5,1,0)
boxplot(Hitters$Salary~Hitters$PrimerContrato)
boxplot(log(Hitters$Salary)~Hitters$PrimerContrato)


# Convertir los valores de la columna League y Division a Dummy
Hitters$LeagueDummy <- ifelse(Hitters$League == "A", 1, 0)
Hitters$DivisionDummy <- ifelse(Hitters$Division == "E", 1, 0)

#Convertir la variable Salary (Y) a Logaritmica
Hitters$Log_Salary <- log(Hitters$Salary)
# Eliminar la columna original League, Salary y Division
Hitters$League <- NULL
Hitters$Division <- NULL
Hitters$Salary <-NULL 
Hitters$NewLeague <-NULL

str(Hitters)


Resultados=data.frame(modelo=integer(),Train=integer(),Test=integer())


set.seed(1111)   
train <- sample(nrow(Hitters), nrow(Hitters)*0.7) 
test <- (-train)


reg <- lm(Log_Salary~., data= Hitters, subset=train)
summary(reg)

pred<- predict(reg,Hitters)

ECM.train <- sqrt(sum((pred[train]-Hitters$Log_Salary[train])^2)/(reg$df.residual))
ECM.train  # mismo valor que indica el summary(reg)

ECM.test <- sqrt(sum((pred[test]-Hitters$Log_Salary[test])^2)/(length(Hitters$Log_Salary[test])))
ECM.test

Resultados[nrow(Resultados)+1, ]=c('RL',ECM.train,ECM.test)


x_train <- model.matrix(Log_Salary ~ ., data = Hitters[train, ])[, -1]
y_train <- Hitters[train, "Log_Salary"]

# Ridge

set.seed(123)
cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 0,  
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)
cv_error

plot(cv_error)


reg_ridge.min <- glmnet(
  x      = x_train,
  y      = y_train,
  alpha       = 0,
  lambda      = cv_error$lambda.min,
  standardize = TRUE
)

reg_ridge.1se <- glmnet(
  x      = x_train,
  y      = y_train,
  alpha       = 0,
  lambda      = cv_error$lambda.1se,
  standardize = TRUE
)

coef(reg)
coef(reg_ridge.min)
coef(reg_ridge.1se)

pred_ridge.min <- predict(reg_ridge.min,as.matrix(Hitters[,-20]))
pred_ridge.1se <- predict(reg_ridge.1se,as.matrix(Hitters[,-20]))

# Error Cuadratico Medio
ECM.train <- c(sqrt(mean((pred_ridge.min[train]-Hitters$Log_Salary[train])^2)),
               sqrt(mean((pred_ridge.1se[train]-Hitters$Log_Salary[train])^2)))

ECM.test <- c(sqrt(mean((pred_ridge.min[test]-Hitters$Log_Salary[test])^2)),
              sqrt(mean((pred_ridge.1se[test]-Hitters$Log_Salary[test])^2)))

Resultados=rbind(Resultados,c('Ridge.min',ECM.train[1],ECM.test[1]))
Resultados=rbind(Resultados,c('Ridge.1se',ECM.train[2],ECM.test[2]))

# Lasso

set.seed(123)
cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,  
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)
cv_error

plot(cv_error)


reg_lasso.min <- glmnet(
  x      = x_train,
  y      = y_train,
  alpha       = 1,
  lambda      = cv_error$lambda.min,
  standardize = TRUE
)

reg_lasso.1se <- glmnet(
  x      = x_train,
  y      = y_train,
  alpha       = 1,
  lambda      = cv_error$lambda.1se,
  standardize = TRUE
)

coef(reg)
coef(reg_lasso.min)
coef(reg_lasso.1se)



pred_lasso.min <- predict(reg_lasso.min,as.matrix(Hitters[,-20]))
pred_lasso.1se <- predict(reg_lasso.1se,as.matrix(Hitters[,-20]))



# Error Cuadratico Medio
ECM.train <- c(sqrt(mean((pred_lasso.min[train]-Hitters$Log_Salary[train])^2)),
               sqrt(mean((pred_lasso.1se[train]-Hitters$Log_Salary[train])^2)))

ECM.test <- c(sqrt(mean((pred_lasso.min[test]-Hitters$Log_Salary[test])^2)),
              sqrt(mean((pred_lasso.1se[test]-Hitters$Log_Salary[test])^2)))

Resultados=rbind(Resultados,c('Lasso.min',ECM.train[1],ECM.test[1]))
Resultados=rbind(Resultados,c('Lasso.1se',ECM.train[2],ECM.test[2]))

Resultados


# elastic net

set.seed(123)
cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1/2,  
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)
cv_error

plot(cv_error)


reg_elastic.min <- glmnet(
  x      = x_train,
  y      = y_train,
  alpha       = 1/2,
  lambda      = cv_error$lambda.min,
  standardize = TRUE
)

reg_elastic.1se <- glmnet(
  x      = x_train,
  y      = y_train,
  alpha       = 1/2,
  lambda      = cv_error$lambda.1se,
  standardize = TRUE
)

coef(reg)
coef(reg_elastic.min)
coef(reg_elastic.1se)


pred_elastic.min <- predict(reg_elastic.min,as.matrix(Hitters[,-20]))
pred_elastic.1se <- predict(reg_elastic.1se,as.matrix(Hitters[,-20]))


# Error Cuadratico Medio
ECM.train <- c(sqrt(mean((pred_elastic.min[train]-Hitters$Log_Salary[train])^2)),
               sqrt(mean((pred_elastic.1se[train]-Hitters$Log_Salary[train])^2)))

ECM.test <- c(sqrt(mean((pred_elastic.min[test]-Hitters$Log_Salary[test])^2)),
              sqrt(mean((pred_elastic.1se[test]-Hitters$Log_Salary[test])^2)))

Resultados=rbind(Resultados,c('Elastic.min',ECM.train[1],ECM.test[1]))
Resultados=rbind(Resultados,c('Elastic.1se',ECM.train[2],ECM.test[2]))

Resultados

# Promedio de predicciones

prom_pred=rowMeans(cbind(pred,pred_elastic.1se,pred_elastic.min,pred_lasso.1se,pred_lasso.min,pred_ridge.1se,pred_ridge.min))

ECM.train <- c(sqrt(mean((prom_pred[train]-Hitters$Log_Salary[train])^2)))

ECM.test <- c(sqrt(mean((prom_pred[test]-Hitters$Log_Salary[test])^2)))

Resultados=rbind(Resultados,c('Promedio',ECM.train,ECM.test))

Resultados

# AutoML: Automatic Machine Learning

h2o.init()

train_h = as.h2o(Hitters[train,])
test_h = as.h2o(Hitters[test,])


y = 'Log_Salary'
pred = setdiff(names(Hitters), y)

# Run AutoML for 20 base models
aml = h2o.automl(x = pred, y = y,
                 training_frame = train_h,
                 max_models = 20,
                 seed = 1,
                 exclude_algos='DeepLearning',
                 max_runtime_secs = 30
)

lb <- h2o.get_leaderboard(object = aml)
  lb

m <- h2o.get_best_model(aml)
m


h2o.performance(m, newdata = test_h)



###############################################################################
#Del H2O surge que el modelo con menor ECM es DRF por lo que nos enfocamos en este para continuar con el trabajo.
###RandomForest Ranger

rf_model <- ranger(
  formula = Log_Salary ~ .,  
  data = Hitters[train, ],    
  num.trees = 500
)

# Evaluar el modelo en los datos de prueba
predictions <- predict(rf_model, data = Hitters)$predictions

# Error Cuadratico Medio
ECM.train <- c(sqrt(mean((predictions[train]-Hitters$Log_Salary[train])^2)),
               sqrt(mean((predictions[train]-Hitters$Log_Salary[train])^2)))

ECM.test <- c(sqrt(mean((predictions[test]-Hitters$Log_Salary[test])^2)),
              sqrt(mean((predictions[test]-Hitters$Log_Salary[test])^2)))

Resultados=rbind(Resultados,c('RF.min',ECM.train[1],ECM.test[1]))
Resultados=rbind(Resultados,c('RF.1se',ECM.train[2],ECM.test[2]))


# LINEAS AGREGADAS POSTERIOR A LA ENTREGA A PARTIR DE UN COMENTARIO DEL PROFESOR
rf_model <- ranger(
  formula = Log_Salary ~ .,  
  data = Hitters[train, ],    
  num.trees = 500,
  oob.error = TRUE
)

# Extraer las predicciones OOB del modelo entrenado
oob_predictions <- rf_model$predictions

# Calcular el Error Cuadrático Medio (ECM) en el conjunto de entrenamiento usando las predicciones OOB
ECM_train_oob <- sqrt(mean((oob_predictions - Hitters$Log_Salary[train])^2))
cat("Error Cuadrático Medio en el conjunto de entrenamiento (OOB):", ECM_train_oob, "\n")

# Evaluar el modelo en los datos de prueba
test_predictions <- predict(rf_model, data = Hitters[test, ])$predictions

# Calcular el Error Cuadrático Medio (ECM) en el conjunto de prueba
ECM_test <- sqrt(mean((test_predictions - Hitters$Log_Salary[test])^2))
cat("Error Cuadrático Medio en el conjunto de prueba:", ECM_test, "\n")

# Guardar los resultados
Resultados <- rbind(Resultados, c('RF.OOB', ECM_train_oob, ECM_test))
print(Resultados)

###DALEX

############################
# REGRESION LINEAL
############################

e_reg=explain(reg,
              y=Hitters$Log_Salary[train],
              label = "reg")

# variable importance
vi_reg <- model_parts(e_reg)
plot(vi_reg)

# efecto de la variable

vr_CRuns  <- model_profile(e_reg, variables =  "CRuns")
head(vr_CRuns)
plot(vr_CRuns)

vr_CAtBat  <- variable_profile(e_reg, variables =  "CAtBat")
plot(vr_CAtBat)

# efecto de la prediccion

new_test=Hitters[test,][1,]


pe_reg <- predict_parts(e_reg, new_test)
plot(pe_reg)

mp_reg=model_performance(e_reg)
mp_reg


############################
# RANDOM FOREST
############################

e_RF=explain(rf_model,
             data=Hitters[,-20],
             y=Hitters[,20],
             label = "RF")

# variable importance
vi_RF <- model_parts(e_RF)
plot(vi_RF)

# efecto de la variable

vr_rm  <- model_profile(e_RF, variables =  "CRuns")
head(vr_rm)
plot(vr_rm)

vr_lstat  <- variable_profile(e_RF, variables =  "CAtBat")
head(vr_lstat)
plot(vr_lstat)

# efecto de la prediccion

new_test=Hitters[test,][1,]

#valores estimados por los modelos
#P[test,][1,]

pe_RF <- predict_parts(e_RF, new_test)
plot(pe_RF)

mp_RF=model_performance(e_RF)
mp_RF


#############################
# PERFORMANCE

plot(mp_reg, mp_RF,  geom = "boxplot")
plot(mp_reg, mp_RF, geom = "histogram")




