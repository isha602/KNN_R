### KNN 
library("class")
wbcdata=read.csv("C:/Users/Isha/Downloads/Breast Cancer Wisconsin.csv")
View(wbcdata)
wbcd=wbcdata[-1]
View(wbcd)
class(wbcd$diagnosis)
wbcd$diagnosis
wbcd$diagnosis=factor(wbcd$diagnosis,levels=c("B","M"),labels=c("Benign","Malignant"))
summary(wbcd)
View(wbcd)
dim(wbcd)

wbcdd=as.data.frame(lapply(wbcd[2:31],normalize))

#splitting the training and the test dataset 

trainwbcd=wbcdd[1:469,]             
testwbcd=wbcdd[470:569,]
View(trainwbcd)
trainwbcd_l=wbcd[1:469,1] 
View(trainwbcd_l)
testwbcd_l=wbcd[470:569,1]
#evaluating knn 
#decide the number of k accrding to the dimensions of the training data which is sqrt(469)
set.seed(123)
wbcd_knn=knn(train=trainwbcd,test=testwbcd,cl=trainwbcd_l,k=21)

#
library(gmodels)
table(testwbcd_l,wbcd_knn)
CrossTable(x=testwbcd_l,y=wbcd_knn,prop.chisq = F)

#improving the model by using z normalization i.e. scale()

wbcd_z=as.data.frame(scale(wbcd[-1]))
trainwbcdz=wbcd_z[1:469,]             
testwbcdz=wbcd_z[470:569,]

trainwbcd_lz=wbcd[1:469,1] 
View(trainwbcd_lz)
testwbcd_lz=wbcd[470:569,1]
#evaluating knn 
#decide the number of k accrding to the dimensions of the training data which is sqrt(469)
wbcd_knnz=knn(train=trainwbcdz,test=testwbcdz,cl=trainwbcd_lz,k=21)
wbcd_knn
plot(wbcd_knnz)

#
library(gmodels)
table(testwbcd_lz,wbcd_knnz)
CrossTable(x=testwbcd_lz,y=wbcd_knnz,prop.chisq = F)

#################################
data(iris)
dim(iris)
View(iris)
#Since the iris dataset is sorted by "Species" by default, we will first jumble the data rows and then take subset.
set.seed(99)# required to reproduce the results
rnum=sample(rep(1:150)) # randomly generate numbers from 1 to 150
rnum
iris=iris[rnum,]
#normalizing the data
iris_n=as.data.frame(lapply(iris[1:4],normalize))

#splitting training and testing data
train_iris=iris_n[1:130,]
test_iris=iris_n[131:150,]

#excluding the target variable
train_iris_l=iris[1:130,5]
test_iris_l=iris[131:150,5]

#implementing the knn model on the normalized dataset 

iris_knn=knn(train=train_iris,test=test_iris,cl=train_iris_l,k=11)

#prinitng out the results 
CrossTable(x=test_iris_l,y=iris_knn)
table(test_iris_l,iris_knn)


##################
###try in air quality dataset 
data("airquality")
airquality=airquality[-6]
View(airquality)
#airq=na.omit(airquality)
for (i in 1:nrow(airquality)){
  if(is.na(airquality[i,"Ozone"])){
    airquality[i,"Ozone"]<- mean(airquality[which(airquality[,"Month"]==airquality[i,"Month"]),"Ozone"],na.rm = TRUE)
  }
  
  # Impute monthly mean in Solar.R
  if(is.na(airquality[i,"Solar.R"])){
    airquality[i,"Solar.R"]<- mean(airquality[which(airquality[,"Month"]==airquality[i,"Month"]),"Solar.R"],na.rm = TRUE)
  }
}

class<- data.frame("month"=airquality$Month)
names(class)= "Month"
airquality[,5]<- NULL #remove "Month" from airquality
head(airquality)
airqual_n=data.frame(lapply(airquality[,c(1:4)],normalize))
dim(airquality)
set.seed(123)
rnum=sample(rep(1:153))
class<- as.data.frame(class[rnum,])
airqual=airqual_n[rnum,]
train_air=airqual[1:130,]

test_air=airqual[131:153,]

train_air_l=class[1:130,]


test_air_l=class[131:153,]

sqrt(130)
air_knn=knn(train=train_air,test=test_air,cl=train_air_l,k=11)
table(test_air_l,air_knn)
mean(test_air_l== air_knn)

