library(paqueteMOD)
library(dplyr)
data("rotacion")
# Visualización data
glimpse(rotacion)

# Selección de variables
datos<-rotacion[, c(1,2,3,4,5)]
# Arreglo nombre de variables
names(datos) = c("rotacion","edad","viaje.negocios_","departamento_","distancia.casa")

# Convertir en factor la variable dependiente
datos$rotacion<-factor(datos$rotacion)

# Modelo1
modelo1= 
  glm(rotacion ~ edad + viaje.negocios_ + departamento_ + distancia.casa, family = binomial(link = "logit"), data = datos)
summary(modelo1)

# Evaluacion del modelo

# Separacion de muetras
ntrain <- nrow(datos)*0.6
ntest <- nrow(datos)*0.4
# c(ntrain,ntest)

set.seed(123)
index_train<-sample(1:nrow(datos),size = ntrain)
train<-datos[index_train,]  # Muestra de entrenamiento
test<-datos[-index_train,]  # Muestra de prueba

# Matriz de confusión
valor_pronosticado <- predict(modelo1,test,type = "response")
niveles_pronosticados <- ifelse(valor_pronosticado >0.5, "Si","No") %>%
  factor(.)


rendimiento_data<-data.frame(observados=test$rotacion,
                             predicciones= niveles_pronosticados)


Positivos <- sum(rendimiento_data$observados=="Si")
Negativos <- sum(rendimiento_data$observados=="No")
Positivos_pronosticados <- sum(rendimiento_data$predicciones=="Si")
Negativos_pronosticados <- sum(rendimiento_data$predicciones=="No")
Total <- nrow(rendimiento_data)
VP<-sum(rendimiento_data$observados=="Si" & rendimiento_data$predicciones=="Si")
VN<-sum(rendimiento_data$observados=="No" & rendimiento_data$predicciones=="No")
FP<-sum(rendimiento_data$observados=="No" & rendimiento_data$predicciones=="Si")
FN<-sum(rendimiento_data$observados=="Si" & rendimiento_data$predicciones=="No")

matriz_confusion=matrix(c(VP, FP, FN,VN), nrow=2)

rownames(matriz_confusion) = c(" Si ", " No    ")
colnames(matriz_confusion) = c("Si", "No")
matriz_confusion
#-----------------------------------------------------------------------
# Qué hacer cuando la data esta desbalanceada
table(test$rotacion) %>% 
  prop.table()

# Oversampling
train.blc <- ovun.sample(rotacion~., data=train, 
                         p=0.5, seed=1, 
                         method="over")$data

test.blc <- ovun.sample(rotacion~., data=test, 
                         p=0.5, seed=1, 
                         method="over")$data


# Modelo2
modelo2=  glm(rotacion ~ edad + viaje.negocios_ + departamento_ + distancia.casa, 
              family = binomial(link = "logit"), data = train.blc)
summary(modelo2)

# Matriz de confusión
valor_prnt.blc <- predict(modelo2,test.blc,type = "response")
niveles_prnt.blc <- ifelse(valor_prnt.blc >0.5, "Si","No") %>%
  factor(.)

rendimiento_data<-data.frame(observados=test.blc$rotacion,
                             predicciones= niveles_prnt.blc)

#-----------------------

Positivos <- sum(rendimiento_data$observados=="Si")
Negativos <- sum(rendimiento_data$observados=="No")
Positivos_pronosticados <- sum(rendimiento_data$predicciones=="Si")
Negativos_pronosticados <- sum(rendimiento_data$predicciones=="No")
Total <- nrow(rendimiento_data)
VP<-sum(rendimiento_data$observados=="Si" & rendimiento_data$predicciones=="Si")
VN<-sum(rendimiento_data$observados=="No" & rendimiento_data$predicciones=="No")
FP<-sum(rendimiento_data$observados=="No" & rendimiento_data$predicciones=="Si")
FN<-sum(rendimiento_data$observados=="Si" & rendimiento_data$predicciones=="No")

matriz_confusion=matrix(c(VP, FP, FN,VN), nrow=2)

rownames(matriz_confusion) = c(" Si ", " No    ")
colnames(matriz_confusion) = c("Si", "No")
matriz_confusion
#--------------------------------------------------
library(tidyverse)
Exactitud <- (VP+VN)/Total
Tasa_de_Error <- (FP+FN)/Total
Sensibilidad <- VP/Positivos
Especificidad <- VN/Negativos
Precision <- VP/Positivos_pronosticados
Valor_prediccion_negativo <- VN / Negativos_pronosticados

indicadores <- t(data.frame(Exactitud,Tasa_de_Error,Sensibilidad,Especificidad,Precision,Valor_prediccion_negativo))

colnames(indicadores)="indicadores" 
rownames(indicadores) =c("Exactitud ", 
                         "Tasa de Error ", 
                         "Sensibilidad", 
                         "Especificidad", 
                         "Precisión", 
                         "Valor predicción negativo")

indicadores %>% 
  round(.,3) 
#-----------------------------------------------------


