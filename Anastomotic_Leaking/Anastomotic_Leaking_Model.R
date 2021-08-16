library(car)
library(boot)
library(stringr)

setwd("C:\\Users\\User\\Desktop\\School\\Math_536\\HW\\HW3")
Data=read.csv("colon2017.csv")

head(Data)

drops=c("Diagnosis.ICD9.Code","CPT.Code","Procedure","Incision.Start","Incision.Close")
Data2=Data[,!(names(Data) %in% drops)]
head(Data2)

Data2$Race=as.character(Data2$Race)
fixes=c("White","white","White ","W")

Data2[Data2$Race %in% fixes,][,7]="W"
unique(Data2$Race)

as.character(temp[1,7])==as.character(temp[2,7])
temp[1,7]==temp[2,7]
drops=c("Anastamotic.Leak")

x=Data2[,!(names(Data2) %in% drops)]
y=Data2[,"Anastamotic.Leak"]

Data2$Operative.Length=Data2$Operative.Length*24

model = glm(Anastamotic.Leak~Gender+BMI+Age+Race+Tobacco+DM+CAD.PAD+Cancer+Albumin..g.dL.+Operative.Length,data=Data2,family="binomial")

summary(model)

#Potential drops: Cancer,CAD.PAD,RaceW
set.seed(1)
folds=cut(seq(1,nrow(Data2)),breaks=j,labels=F)
pred=0
logLike=0
for (i in 1:j)
{
  testIndexes=which(folds==i,arr.ind=T)
  testData=Data2[testIndexes,]
  trainData=Data2[-testIndexes,]
  model = glm(Anastamotic.Leak~Gender+BMI+Age+Race+Tobacco+DM+CAD.PAD+Cancer+Albumin..g.dL.+Operative.Length,data=trainData,family="binomial")
  pred=predict.glm(model,newdata=testData,type="response")
  logLike=logLike + sum((testData$Anastamotic.Leak*log(pred))+((1-testData$Anastamotic.Leak)*log(1-pred)) )
}
print(logLike)

#drops: Cancer
set.seed(1)
folds=cut(seq(1,nrow(Data2)),breaks=j,labels=F)
pred=0
logLike=0
for (i in 1:j)
{
  testIndexes=which(folds==i,arr.ind=T)
  testData=Data2[testIndexes,]
  trainData=Data2[-testIndexes,]
  model = glm(Anastamotic.Leak~Gender+BMI+Age+Race+Tobacco+DM+CAD.PAD+Albumin..g.dL.+Operative.Length,data=trainData,family="binomial")
  pred=predict.glm(model,newdata=testData,type="response")
  logLike=logLike + sum((testData$Anastamotic.Leak*log(pred))+((1-testData$Anastamotic.Leak)*log(1-pred)) )
}
print(logLike)

#Potential drops: Cancer,CAD.PAD
set.seed(1)
folds=cut(seq(1,nrow(Data2)),breaks=j,labels=F)
pred=0
logLike=0
for (i in 1:j)
{
  testIndexes=which(folds==i,arr.ind=T)
  testData=Data2[testIndexes,]
  trainData=Data2[-testIndexes,]
  model = glm(Anastamotic.Leak~Gender+BMI+Age+Race+Tobacco+DM+Albumin..g.dL.+Operative.Length,data=trainData,family="binomial")
  pred=predict.glm(model,newdata=testData,type="response")
  logLike=logLike + sum((testData$Anastamotic.Leak*log(pred))+((1-testData$Anastamotic.Leak)*log(1-pred)) )
}
print(logLike)

#dropping RaceW does not make sense since its only one of a catagory column

model = glm(Anastamotic.Leak~Gender+BMI+Age+Race+Tobacco+DM+Albumin..g.dL.+Operative.Length,data=trainData,family="binomial")

modSum=summary(model)

printTable=function()
{
  print("Variable:             Estimate:    STD Error:    Z-Value    Pr(>|z|)     %Change           Lower Bound     Upper Bound")
  print(paste0("Intercept","     ",modSum$coefficients[1,1],"     ",modSum$coefficients[1,2],"     ",modSum$coefficients[1,3]
        ,"     ",modSum$coefficients[1,4],"     ",round((exp(modSum$coefficients[1,1])-1)*100,8),"     ",
        round((exp(modSum$coefficients[1,1]-1.96*modSum$coefficients[1,2])-1)*100,8),"     ",
        round(((exp(modSum$coefficients[1,1]+1.96*modSum$coefficients[1,2])-1)*100),8)))
  for(i in 1:8)
  {
    print(paste0(str_pad(attr(model$terms , "term.labels")[i],17,"right"),"     ",round(modSum$coefficients[i+1,1],6),"     ",round(modSum$coefficients[i+1,2],6),"     ",
           round(modSum$coefficients[i+1,3],6),"     ",round(modSum$coefficients[i+1,4],6),"     ",round((exp(modSum$coefficients[i+1,1])-1)*100,8),"     ",
           round((exp(modSum$coefficients[i+1,1]-1.96*modSum$coefficients[i+1,2])-1)*100,8),"     ",
           round(((exp(modSum$coefficients[i+1,1]+1.96*modSum$coefficients[i+1,2])-1)*100),8)))
  }
}

printTable()






























