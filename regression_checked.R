rm(list=ls())  
diamonds=read.csv("diamonds.csv", header = TRUE, sep = ",", dec = ".")
str(diamonds)
head(diamonds)
class(diamonds)

#Преименуване на категорните променливи 
levels(diamonds$cut)=c(1,2,5,4,3)
levels(diamonds$color)=c(7,6,5,4,3,2,1)
levels(diamonds$clarity)=c(1,8,3,2,5,4,7,6)

#Превръщане на factor променливите в numeric 
diamonds$cut=as.numeric(diamonds$cut)
diamonds$color=as.numeric(diamonds$color)
diamonds$clarity=as.numeric(diamonds$clarity)

#Премахване колона Х от данните, която отговаря за номерата на редовете,
#защото не ни е необходима в анализа
diamonds$X=remove()

#Преуменуваме променливите, с които ще работим 
names(diamonds)=c('X1','X2','X3','X4','X5','X6','Y','X7','X8','X9')
str(diamonds)

#Разделяме данните на такива, върху които ще тренираме модела, 
#и на такива, върху които ще тестваме дали модела ни се справя добре 
s=dim(diamonds)
set.seed(1)
pred_idx=sample(1:s[1],floor(0.1*s[1]),replace = FALSE)
train=diamonds[-pred_idx, ]
test=diamonds[pred_idx, ]
t=dim(train)

#Правим scatter plot само с една част от данните и всички прооменливи,
#за да придобием обща представа 
plot(train[sample(1:t[1],1000,replace = FALSE),])

#Извеждаме корелационната таблица 
corTable=cor(train)
corTable

#Пускаме базов модел, който ще ни служи за ориентация 
#за бъдещите модели дали са достатъчно добри 
baseline_model=lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9, data=train) 
summary(baseline_model)
plot(baseline_model)

#Правим модел с най-добре корелираната променлива с Y 
model1=lm(Y~X1, data=train)
summary(model1)

#Следващата най-корелирана променлива с Y е X7
#Проверяваме дали е квазимултиколинеарна с предишния ни модел - model1
QMC_X7=lm(X7~X1, data=train)
summary(QMC_X7)
#Х7 е квазимултиколинеарна и затова не я добавяме към модела ни и преминаваме
#към проверка за квазимултиколинеарност на следващата най-добре корелирана променлива с Y

QMC_X8=lm(X8~X1, data=train)
summary(QMC_X8)
#Х8 е квазимултиколинеарна и затова не я добавяме към модела ни и преминаваме
#към проверка за квазимултиколинеарност на следващата най-добре корелирана променлива с Y

QMC_X9=lm(X9~X1, data=train)
summary(QMC_X9)
#Х9 е квазимултиколинеарна и затова не я добавяме към модела ни и преминаваме
#към проверка за квазимултиколинеарност на следващата най-добре корелирана променлива с Y

QMC_X3=lm(X3~X1, data=train)
summary(QMC_X3)
#Х3 не е квазимултиколинеарна и затова я добавяме към модела ни
model2=lm(Y~X1+X3, data=train)
summary(model2)

#Следващата най-корелирана променлива с Y е X6
#Проверяваме дали е квазимултиколинеарна с предишния ни модел - model2
QMC_X6=lm(X6~X1+X3, data=train)
summary(QMC_X6)
#Х6 не е квазимултиколинеарна и затова я добавяме към модела ни
model3=lm(Y~X1+X3+X6, data=train)
summary(model3)

#Следващата най-корелирана променлива с Y е X4
#Проверяваме дали е квазимултиколинеарна с предишния ни модел - model3
QMC_X4=lm(X4~X1+X3+X6, data=train)
summary(QMC_X4)
#Х4 не е квазимултиколинеарна и затова я добавяме към модела ни
model4=lm(Y~X1+X3+X6+X4, data=train)
summary(model4)

#Следващата най-корелирана променлива с Y е X2
#Проверяваме дали е квазимултиколинеарна с предишния ни модел - model4
QMC_X2=lm(X2~X1+X3+X6+X4, data=train)
summary(QMC_X2)
#Х2 не е квазимултиколинеарна и затова я добавяме към модела ни
model5=lm(Y~X1+X3+X6+X4+X2, data=train)
summary(model5)

#Следващата най-корелирана променлива с Y е X5
#Проверяваме дали е квазимултиколинеарна с предишния ни модел - model5
QMC_X5=lm(X5~X1+X3+X6+X4+X2, data=train)
summary(QMC_X5)
#Х5 не е квазимултиколинеарна и затова я добавяме към модела ни
model6=lm(Y~X1+X3+X6+X4+X2+X5, data=train)
summary(model6)
#получваме модел, който е почти толкова добър до базовия, но не го подобрява
plot(model6)

#Провеярваме предположенията за последния ни модел 6
model= lm(Y~X1+X3+X6+X4+X2+X5, data=train)

#Проверка за нормалност на грешките 
install.packages("nortest")
library(nortest)
ad.test(residuals(model))

se=sqrt(deviance(model)/df.residual(model))
se  # standard error
rse=se/mean(train$Y)
rse #relative standard error

predict(model, int="p", newdata=test)
Y_predicted=predict(model, newdata=test)

MAE=mean(abs(test$Y-Y_predicted)) #Mean Absolute Error
MSE=mean((test$Y-Y_predicted)^2) #Mean Squared Error
RMSE=sqrt(MSE) #Root Mean Squared Error
MAE; RMSE

MARE=mean(abs((test$Y-Y_predicted)/test$Y)) #Mean Absolute Relative Error
MSRE=mean(((test$Y-Y_predicted)/test$Y)^2) #Mean Squared Relative Error
RMSRE=sqrt(MSRE) #Root Mean Squared Relative Error
MARE; RMSRE

PMAE=MAE/mean(test$Y) # Percentage Mean Absolute Error
PRMSE=RMSE/mean(test$Y) # Percentage Root Mean Squared Error
PMAE; PRMSE

Theil_coeff=sqrt(mean((test$Y-Y_predicted)^2))/sqrt(mean((test$Y)^2)+mean((Y_predicted)^2))

cor.test(Y_predicted,test$Y)

plot(test$Y, Y_predicted, pch=21,cex=1.5,bg=rgb(1/3,1/3,1/3))
abline(lm(test$Y~Y_predicted), col=rgb(220/255,0,0), lwd=3)
summary(lm(test$Y~Y_predicted))

plot(test$Y, Y_predicted, pch=21,cex=1.5,bg=rgb(1/3,1/3,1/3))
abline(lm(test$Y~Y_predicted-1), col=rgb(220/255,0,0), lwd=3)
summary(lm(test$Y~Y_predicted-1))

#Bartlett test for heteroscedasticity
############

#Опитваме се да намерим модел, който подобрява модел 6 и по възможност базовия 

my_model1=lm(Y~X3+X6+X4+X2+X5+poly(X1, degree=2, raw=TRUE), data=train)
summary(my_model1)
#Моделът е по-добър от модел 6, но не и от базовия 

my_model2=lm(Y~polym(X1, X3, X6, X4, X2, X5, degree=2, raw=TRUE), data=train)
summary(my_model2)
#Mоделът е по-добър от базовия модел 

my_model3=lm(Y~polym(X1, X3, X6, X4, X2, X5, degree=3, raw=TRUE), data=train)
summary(my_model3)
#Моделът е по-добър и от my_ model2
plot(my_model3)

#Проверяваме предположенията за my_model3
model= lm(Y~polym(X1, X3, X6, X4, X2, X5, degree=3, raw=TRUE), data=train)

#Проверяваме предположението за нормалност на грешките
ad.test(residuals(model))

se=sqrt(deviance(model)/df.residual(model))

se  # standard error
rse=se/mean(train$Y)
rse #relative standard error

predict(model, int="p", newdata=test)
Y_predicted=predict(model, newdata=test)

MAE=mean(abs(test$Y-Y_predicted)) #Mean Absolute Error
MSE=mean((test$Y-Y_predicted)^2) #Mean Squared Error
RMSE=sqrt(MSE) #Root Mean Squared Error
MAE; RMSE

MARE=mean(abs((test$Y-Y_predicted)/test$Y))  #Mean Absolute Relative Error
MSRE=mean(((test$Y-Y_predicted)/test$Y)^2)  #Mean Squared Relative Error
RMSRE=sqrt(MSRE) #Root Mean Squared Relative Error
MARE; RMSRE

PMAE=MAE/mean(test$Y) #Percentage Mean Absolute Error
PRMSE=RMSE/mean(test$Y) #Percentage Root Mean Squared Error
PMAE; PRMSE

Theil_coeff=sqrt(mean((test$Y-Y_predicted)^2))/sqrt(mean((test$Y)^2)+mean((Y_predicted)^2))

cor.test(Y_predicted,test$Y)

plot(test$Y, Y_predicted, pch=21,cex=1.5,bg=rgb(1/3,1/3,1/3))
abline(lm(test$Y~Y_predicted), col=rgb(220/255,0,0), lwd=3)
summary(lm(test$Y~Y_predicted))


#######################################################################################################
########################### Bartlett test for heteroscedasticity with respect to Y ###
#######################################################################################################
train[,length(train)+1]=as.factor(train$Y>median(train$Y))
names(train)[length(train)]=paste("Y_Groups")
levels(train[,length(train)])=c("Group 1","Group 2")
str(train)
formula=Y~polym(X1, X3, X6, X4, X2, X5, degree=3, raw=TRUE)
lmg1=lm(formula,data = train, subset = which(train$Y_Groups=="Group 1"))
lmg2=lm(formula,data = train, subset = which(train$Y_Groups=="Group 2"))
bartlett_stat=deviance(lmg1)/deviance(lmg2)
if(bartlett_stat>1){
  bartlett_p.value=pf(bartlett_stat, df.residual(lmg1), df.residual(lmg2), lower.tail = FALSE)
} else{
  bartlett_p.value=pf(1/bartlett_stat, df.residual(lmg2), df.residual(lmg1), lower.tail = FALSE)}
c("Stat"=bartlett_stat,"p.value"=bartlett_p.value)
##################### => Има хетероскедастичност по оношение на Y

#######################################################################################################
########################### Bartlett test for heteroscedasticity with respect to X1 ###################
#######################################################################################################
train[,length(train)+1]=as.factor(train$X1>median(train$X1))
names(train)[length(train)]=paste("X1_Groups")
levels(train[,length(train)])=c("Group 1","Group 2")
str(train)
formula=Y~polym(X1, X3, X6, X4, X2, X5, degree=3, raw=TRUE)
lmg1=lm(formula,data = train, subset = which(train$X1_Groups=="Group 1"))
lmg2=lm(formula,data = train, subset = which(train$X1_Groups=="Group 2"))
bartlett_stat=deviance(lmg1)/deviance(lmg2)
if(bartlett_stat>1){
  bartlett_p.value=pf(bartlett_stat, df.residual(lmg1), df.residual(lmg2), lower.tail = FALSE)
} else{
  bartlett_p.value=pf(1/bartlett_stat, df.residual(lmg2), df.residual(lmg1), lower.tail = FALSE)}
c("Stat"=bartlett_stat,"p.value"=bartlett_p.value)
##################### => Има хетероскедастичност по оношение на X1


#######################################################################################################
########################### PCA #######################################################################
#######################################################################################################
library("pls")
train_data=train[,1:10]
str(train_data)
model7=pcr(Y~., data = train_data, scale = TRUE)
summary(model7) #### Можем да използваме от 2 до 6 компоненти на 9-те предиктора
model7$coefficients # Коефициентите в моделите пред 9-те предиктора
model7$coefficients[,,3] # Коефициентите пред 9-те предиктора за модела базиран на 3 компоненти
model7$residuals[,,3] # Остатъците на модела базиран на 3 компоненти

model7$loadings # Собствените вектори на 9-те компонента. Показват теглото на предикторите във всеки компонент
F1=model7$scores[,1]
model7$loadings[,1] # F1 (comp 1) комбинира основно X1, X7, X8 и X9
F2=model7$scores[,2]
model7$loadings[,2] # F2 (comp 2) комбинира основно X2, X5 и X6
F3=model7$scores[,3]
model7$loadings[,3] # F3 (comp 3) комбинира основно X3 и X4

new_train_data=data.frame(train_data$Y,F1,F2,F3)
str(new_train_data)
names(new_train_data)=c("Y","F1","F2","F3")
plot(new_train_data)
model8=lm(Y~poly(F1,2)+F2+F3,data=new_train_data) #### Може би е по-сложна зависимостта?
summary(model8)
ad.test(residuals(model8))
#### Грешките не са нормално разпределени => и с PCA няма да се получи добър модел
