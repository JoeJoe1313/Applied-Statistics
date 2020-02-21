###################################################
################# Начални стъпки ##################
###################################################
#Изобразяваме наблюденията
observations=cbind(c(1,1,0,5,6,4),c(4,3,4,1,2,0))
plot(observations)

#Произволно присвояваме номер на клъстер на всяко наблюдение
set.seed(2)
clusterLabel=sample(2, size=6, replace=TRUE)
clusterLabel

###################################################
#### Дефинираме функциите, които ще използваме #### 
###################################################
plotCluster=function(x,y){
  plot(x[y==1, 1], x[y==1, 2], col=2, pch=20, cex=1.5, xlim=c(0,6), ylim=c(0,4))
  points(x[y==2, 1], x[y==2, 2], col=11, pch=20, cex=1.5)
}

centre1=function(x,y){
  return(c(mean(x[y==1, 1]), mean(x[y==1, 2])))
}

centre2=function(x,y){
  return(c(mean(x[y==2, 1]), mean(x[y==2, 2])))
}

#Дефинираме функция за Евклидово разстояние ^2
distance=function (x, y){
  return(sqrt((x[1]-y[1])^2 + (x[2]-y[2])^2))                            
}
###################################################
###################################################
###################################################

#Изобразяваме клъстерите с различен цвят 
plotCluster(observations,clusterLabel)

#Изчисляваме центроидите на всеки клъстер 
centroid1=centre1(observations,clusterLabel)
centroid1
centroid2=centre2(observations,clusterLabel)
centroid2

#Изобразяваме и центроидите на всеки клъстер със съответсващ на клъстера цвят
points(centroid1[1], centroid1[2], col=2, pch=2)
points(centroid2[1], centroid2[2], col=11, pch=2)

#Присвояваме всяко наблюдение към клъстера на центроидата, до която е най-близо
for (i in 1:6) {
  ifelse(distance(observations[i,], centroid1)<distance(observations[i,], centroid2),clusterLabel[i]<-1,clusterLabel[i]<-2)
}

#Повтаряме стъпките докато промените спрат 

plotCluster(observations,clusterLabel)

centroid1=centre1(observations,clusterLabel)
centroid1
centroid2=centre2(observations,clusterLabel)
centroid2

points(centroid1[1], centroid1[2], col=2, pch=2)
points(centroid2[1], centroid2[2], col=11, pch=2)

for (i in 1:6) {
  ifelse(distance(observations[i,], centroid1)<distance(observations[i,], centroid2),clusterLabel[i]<-1,clusterLabel[i]<-2)
}

#Изобразяваме клъстерите и центроидите, които сме получили 
plotCluster(observations,clusterLabel)

centroid1=centre1(observations,clusterLabel)
centroid1
centroid2=centre2(observations,clusterLabel)
centroid2

points(centroid1[1], centroid1[2], col=2, pch=2)
points(centroid2[1], centroid2[2], col=11, pch=2)

for (i in 1:6) {
  ifelse(distance(observations[i,], centroid1)<distance(observations[i,], centroid2),clusterLabel[i]<-1,clusterLabel[i]<-2)
}

#Изобразяваме клъстерите и центроидите, които сме получили 
plotCluster(observations,clusterLabel)

centroid1=centre1(observations,clusterLabel)
centroid1
centroid2=centre2(observations,clusterLabel) #Центроидите не се променят
centroid2

points(centroid1[1], centroid1[2], col=2, pch=2)
points(centroid2[1], centroid2[2], col=11, pch=2)
#Нищо не се промени и значи приключваме повтоарянето на горните стъпки 

#In your plot from (a), color the observations according to the cluster labels obtained
plotCluster(observations,clusterLabel)