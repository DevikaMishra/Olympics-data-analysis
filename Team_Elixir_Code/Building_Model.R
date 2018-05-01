#==================================================================
#PREDICTION
#==================================================================
#Prediction analysis: 
#=================================================

#Libraries needed: 
library("e1071")
library("caret")

library(dplyr)
library(neuralnet)
require(neuralnet)


#data: 
library(readxl)
Medals_country <- read_excel("/Team_Elixir_Dataset/Medals_country.xlsx", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric"))

Testing_data <- read_excel("/Team_Elixir_Dataset/Testing_data.xlsx")

#Code:
mytable<-Medals_country
mytable$IOC_code<-NULL
Medals_country$IOC_code<-NULL


#SVM's:
model_svm <- svm(Medal_Count ~ . ,data = Medals_country)
predict(model_svm,Medals_country)->Pred_svm
as.data.frame(Pred_svm)->Pred_svm
cbind(mytable,Pred_svm)->mytable


#Calculating the error:
error_2 <- mytable$Medal_Count - Pred_svm
svm_error <- sqrt(mean(error_2^2))
svm_error

########################################################################################################


#BUILDING A MULTIPLE REGRESSION MODEL 
model <- lm(Medal_Count~Year+GDP+prev_perf , data = Medals_country)

summary(model)
Pred_Count = predict.lm(model, Testing_data) 

#cOMPUTING ERROR
error_lm <- Testing_data$Medal_Count - Pred_Count
lm_error <- sqrt(mean(error_lm^2))
lm_error


######################################################################################

#Building  a neural network
n <- names(Medals_country)
f <- as.formula(paste("Medal_Count ~", paste(n[!n %in% c("Medal_Count" , "IOC_code","Country")], collapse = " + ")))


nn<-NULL
nn <- neuralnet(f,data=Medals_country,hidden=c(3, 2),linear.output=T, rep = 10)
plot(nn)

pred_nn <- neuralnet::compute(nn,Testing_data[ ,c("Year" , "prev_perf" ,"GDP")])
str(pred_nn)
#pred_nn$net.result


error_nn <- Testing_data$Medal_Count - pred_nn$net.result
nn_error <- sqrt(mean(error_nn^2))
nn_error


#####################################################################################################

