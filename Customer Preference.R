#Data exploration & pre-processing
library(readr)
library(ggplot2)
library(binr)
library(C50)
library(caret)
library(mlbench)
summary(CompleteResponses)

# Check for missing values
my_na<-is.na(CompleteResponses)
summary(my_na)

# Converting elevel, zipcode, brand and car to factor variables

CompleteResponses$elevel<-as.factor(CompleteResponses$elevel)
CompleteResponses$car<-as.factor(CompleteResponses$car)
CompleteResponses$zipcode<-as.factor(CompleteResponses$zipcode)
CompleteResponses$brand<-as.factor(CompleteResponses$brand)

#Data Visualization

levels(CompleteResponses$brand) <- c("Acer", "Sony")

ggplot(data = CompleteResponses ,mapping = aes(x = brand,fill=brand))+geom_bar()+
  geom_text(stat="count",aes(label=..count..,y=..count..), vjust=10)

histogram("salary",CompleteResponses)

ggplot(CompleteResponses, aes(x=salary)) + 
  geom_histogram(binwidth=1)

ggplot(CompleteResponses, aes(x=salary)) + 
  geom_histogram()

qplot(CompleteResponses$salary,
      geom="histogram",
      binwidth = 5,  
      main = "Histogram for Salary", 
      xlab = "Salary",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(20,50))

ggplot(CompleteResponses, aes(x=elevel)) + 
  geom_histogram(binwidth=1)

ggplot(CompleteResponses, aes(x=age)) + 
  geom_histogram(binwidth=1)

ggplot(CompleteResponses, aes(x=credit)) + 
  geom_histogram(binwidth=1)

ggplot(data = CompleteResponses ,mapping = aes(x = zipcode,fill=brand))+geom_bar()+
  geom_text(stat="count",aes(label=..count..,y=..count..), vjust=10)

ggplot(data = CompleteResponses ,mapping = aes(x = zipcode,fill=brand))+geom_bar()+
  geom_text(stat="count",aes(label=..count..), vjust=10)

ggplot(data = CompleteResponses ,mapping = aes(x = zipcode,fill=brand))+geom_bar()

ggplot(data = CompleteResponses ,mapping = aes(x = elevel,fill=brand))+geom_bar()

ggplot(data = CompleteResponses ,mapping = aes(x = car,fill=brand))+geom_bar()

ggplot(CompleteResponses, aes(x=salary, y=age, col=brand )) + geom_point()

ggplot(CompleteResponses, aes(x=salary, y=zipcode, col=brand )) + geom_point()

ggplot(CompleteResponses, aes(x=salary, y=elevel, col=brand )) + geom_point()

ggplot(CompleteResponses, aes(x=salary, y=car, col=brand )) + geom_point()

#Boxplot

ggplot(CompleteResponses, aes(x=brand, y=salary,main= "Brand Distribution by Salary")) + geom_boxplot()

ggplot(CompleteResponses, aes(x=brand, y=salary)) + 
  geom_boxplot(fill="gray")+
  labs(title="Brand Distribution by Salary",x="Brand", y = "Salary")

ggplot(CompleteResponses, aes(x=brand, y=age)) + 
  geom_boxplot(fill="gray")+
  labs(title="Brand Distribution by Age",x="Brand", y = "Age")

#Bining the age and salary data
CompleteResponses$age_bin <- cut(CompleteResponses$age, 3)
CompleteResponses$salary_bin <- cut(CompleteResponses$salary, 5)
str(CompleteResponses)

#COnverting into factor data

CompleteResponses$age_bin <- as.factor(CompleteResponses$age_bin)
CompleteResponses$salary_bin <- as.factor(CompleteResponses$salary_bin)

#New Data to train model

features<-c("brand","age_bin","salary_bin")
CR<-CompleteResponses[,features]
head(CR)

#Create Partition

set.seed(107)

inTraining<-createDataPartition(CR$brand, p=.75, list=FALSE)
training<-CR[inTraining,]
testing<-CR[-inTraining,]

#Cross Validation
model1<-trainControl(method = "repeatedcv",number=10, repeats=1)

model1

ctrl<-trainControl(method = "repeatedcv", repeats=3)
model1<-train(brand~.,
              data = training,
              method="C.50",
              tunelength=2,
              trControl=ctrl,
              preProc = c("center", "scale"))

#C.50 model

library(C.50)

ctrl<-trainControl(method = "repeatedcv", repeats=3)

model1<-train(brand~.,
              data = training,
              method="C5.0",
              tunelength=2,
              trControl=ctrl,
              preProc = c("center", "scale"))
model1

plot(model1)

varImp(model1)


#Random forest 

library(randomForest)

rfGrid<-expand.grid(mtry=c(1,2,3,4,5))

model2<-train(brand~.,data = training,method="rf",trControl=ctrl, tuneGrid=rfGrid)

model2


#PREDICTION


read_csv("SurveyIncomplete.csv")

SurvIncomp<-read_csv("SurveyIncomplete.csv")

head(SurvIncomp)

#Bining the age and salary of new data
SurvIncomp$age_bin <- cut(SurvIncomp$age, 3)

SurvIncomp$salary_bin <- cut(SurvIncomp$salary, 5)

str(SurvIncomp)

#COnverting into factor data

SurvIncomp$age_bin <- as.factor(SurvIncomp$age_bin)

SurvIncomp$salary_bin <- as.factor(SurvIncomp$salary_bin)

#Selecting features in data

features2<-c("brand","age_bin","salary_bin")

SurvIncomp2<-SurvIncomp[,features2]

head(SurvIncomp)


Prediction<-predict(model1,SurvIncomp2)

summary(Prediction)

#Testing Prediction

prediction2<-SurvIncomp[1:6]

read_csv("SurveyIncomplete.csv")

SurveyIncomplete<-read_csv("SurveyIncomplete.csv")

prediction2<-SurveyIncomplete[1:6]

prediction2$brand<-Prediction

head(prediction2)

#SCATTERPLOT OF NEW PREDICTION

ggplot(prediction2, aes(x=salary, y=age, col = brand))+geom_point() +
  labs(x="salary", y="age", 
  title="Relation between Age, Salary, and Predicted Brand")

#CUSTOMERS’ PREFERENCE (REAL AND PREDICTED)

summary(prediction)

summary(CompleteResponses$brand)

Total<- summary(Prediction) + summary(CompleteResponses$brand)

survey.total<- rbind(CompleteResponses[1:7],prediction2[,c(1:7)])

levels(survey.total$brand) <- c("Acer", "Sony")

ggplot(data = survey.total ,mapping = aes(x = brand,fill=brand))+geom_bar()+
     geom_text(stat="count",aes(label=..count..,y=..count..), vjust=10)





