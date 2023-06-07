library(readxl)
conjunto <- read_excel("C:/Users/zulay/OneDrive/Escritorio/conjunto.xlsx")
View(conjunto)
## 3.1. Organizar los datos
x <- conjunto[, c("age", "job", "marital", "education", "default", "balance", "housing", "loan", "contact", "day", "month", "duration", "campaign", "poutcome")]
y <- conjunto$Y

View(conjunto)


## 3.2. Modelo.
################# primer modelo de probit
## modelo 
modelo1=glm(y~x,family=binomial(link="probit"))
summary(modelo1)


## coeficiente del modelo 
coe<-coefficients(modelo1);coe
########################## segundo  modelo de probit

# Ajustar el modelo Probit
library("mlogit")  # Cargar el paquete mlogit
library("dfidx")
# Convertir las variables categóricas en variables ficticias
conjunto_dummy <- mlogit.data(conjunto, shape = "wide", choice = "y")

# Ajustar el modelo Probit
probit_model <- mlogit(y ~ age + job + marital + education + default + balance + housing + loan + contact + day + month + duration + campaign + poutcome, data = conjunto_dummy, method = "probit")

# Ver los resultados del modelo
summary(probit_model)

##################### tercer modelo de probit

# Ajustar el modelo probit
modelo <- glm(Y ~ age + job + marital + education + default + balance + housing + loan + contact + day + month + duration + campaign + poutcome,
              data = conjunto,
              family = binomial(link = "probit"))

# Mostrar los resultados
summary(modelo)

#################### cuarto  modelo de probit
# Cargar el conjunto de datos
conjunto <- read.csv("conjunto.csv")

# Ajustar el modelo Probit
modelo_probit <- glm(y ~ age + job + marital + education + default + balance +
                       housing + loan + contact + day + month + duration +
                       campaign + poutcome, data = conjunto, family = binomial(link = "probit"))

# Resumen del modelo
summary(modelo_probit)

## exportar resultados del modelo 
setwd("C:\\Users\\zulay\\OneDrive\\Escritorio\\probit\\11.xlsx")

## 3.3. Coeficientes del modelo.

coe<-coefficients(modelo_probit);coe
c1<-coe[1]
c2<-coe[2]
exp(coefficients(modelo_probit))

##3.4. Intervalo de confianza de los parámetros.

confint(modelo_probit,level=0.95)

## Gráfico del modelo.

curve(pnorm(c1+c2*x),xlim=c(30000,60000),lwd=2)
## 3.6. Predichos.

pred<-predict(modelo_probit)
pred

exp(pred)

pre1<-predict(modelo_probit,type="response");pre1

## Predicción para ingreso de 38000.

oda<- predict(modelo_probit,data.frame(x=38000))
oda   
library(pROC)
## 3.8. Curva ROC.
roc(y,pre1,plot = TRUE, legacy.axes = TRUE,
    percent = TRUE, xlab = "% Falsos positivos",
    ylab = "% verdaderos postivios", col = "red", lwd = 2,
    print.auc = TRUE)

##3.9. Matriz de confusión.
tabla1<-table(true=y,pred=round(fitted(modelo_probit)))
sum(diag(tabla1))/sum(tabla1)


matriz1<-confusionMatrix(tabla1);matriz1



curve(pnorm(x,0,1),xlim=c(-4,4),lwd=2)

file.choose()


write.csv(coe, file = "C:\\Users\\zulay\\OneDrive\\Escritorio\\probit\\coe.csv")

library(readr)
