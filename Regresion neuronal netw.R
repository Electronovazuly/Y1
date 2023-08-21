file.choose()
ruta_excel <- "C:\\Users\\yrincong\\Desktop\\reg.xlsx"
basereg <-   (ruta_excel)
read_excel(ruta_excel)
#### correlacion de datos sin normalizar###
pairs(reg)
library(psych)
cor.plot(reg[1:6])
cor.plot(reg [1:12])
cor.plot(reg [7:12])

# NORMALIZACIÓN DE DATOS CON MAX Y MIN
rm(list = ls())

summary(reg)
library(caret)
process <- preProcess(as.data.frame(reg), method=c("range"))
norm_scale <- predict(process, as.data.frame(reg))
library(rio)

export(norm_scale, "norm_scale_rio.xlsx")

# CORRELACION DATOS CON NORMALIZDOS ###
corr.test(reg, method = "pearson")
M <- cor(reg)


modeloreg <- lm (reg [1:7])
summary(modeloreg)


modeloregnor <- lm (norm_scale)
summary(modeloregnor)


modeloregnor1 <- lm (norm_scale [1:6])
summary(modeloregnor1)
modeloregnor2 <- lm (norm_scale [7:12])
summary(modeloregnor2)
# CORRELACION CON DATOS NORMALIZADOS 
cor(norm_scale)
cor.plot(norm_scale)
cor.plot(norm_scale [1:6])
cor.plot(norm_scale [7:12])


# RED NEURONAL CON REGRESION VARIABLES mlpkerasdropout perceptron
library(tensorflow)
install_tensorflow()
library(tidyverse)
library(caret)


# partition data into training and test sets
set.seed(1)
inTrain <- createDataPartition(norm_scale$Y1, p = 0.7, list = FALSE)
training <- norm_scale[inTrain, ]
test <- norm_scale[-inTrain, ]

# keras package in caret

# fit regression model with mlpKerasDropout
set.seed(1)
system.time(mlpKerasDropout <- train(Y1 ~ ., data = training, method = 'mlpKerasDropout',
                                     preProcess = list(center = c('X1', 'X2', 'X3', 'X4', 'X5', 'X6'), 
                                                       scale = c('X1', 'X2', 'X3', 'X4', 'X5', 'X6')), 
                                     trControl = trainControl(method = 'cv', number = 10)))
mlpKerasDropout

library(RSNNS)
library(Rcpp)
library(keras)
