
library(ggplot2)
library(plot3D)
library(dplyr)
library(tidyr)
#esquisse::esquisser(data)

setwd("C:\\Users\\User\\Desktop\\School\\Math_536\\Final")

pitches=read.csv("baseball.csv",stringsAsFactors=FALSE)
pitches$X=as.integer(pitches$X)

head(pitches)

unique(pitches$description)


pitches %>%
  filter(!(pitch_type %in% "") | is.na(pitch_type)) %>%
  ggplot() +
  aes(x = plate_x, y = plate_z, colour = description) +
  geom_point(size = 1L) +
  scale_color_hue() +
  theme_minimal()




library(dplyr)
library(ggplot2)

pitches %>%
 filter(!(pitch_type %in% "") | is.na(pitch_type)) %>%
 ggplot() +
 aes(x = plate_x, y = plate_z, colour = description) +
 geom_point(size = 1L) +
 scale_color_manual(values=c("#aa0000","#0000aa")) +
 theme_minimal()

pitches$plate_x=as.double(pitches$plate_x)
pitches$plate_z=as.double(pitches$plate_z)

pitches=pitches[!(is.na(pitches$plate_x)),]
pitches=pitches[!(is.na(pitches$plate_z)),]

strike=pitches[pitches$description=="called_strike",]
ball=pitches[pitches$description=="ball",]

print("Dim(ball) Prior")
dim(ball)

for (j in 1:dim(ball)[1])
{
  ballTemp=cbind(ball$X,sqrt((ball$plate_x-ball$plate_x[j])^2+(ball$plate_z-ball$plate_z[j])^2))
  if(length(ballTemp[ballTemp[,2]<.1])>0)
  {
    ball=ball[!(ball$X %in% ball$X[j]),]
    j=j-1
  }
}

print("Dim(ball) Post")
dim(ball)

ballDist=matrix(0,dim(ball)[1],5)
strikeDist=matrix(0,dim(strike)[1],5)

for(i in 1:dim(strike)[1])
{
  distance=strike$plate_x-ball$plate_x[i]
  minXTemp=min(abs(distance))
  maxXTemp=max(abs(distance))
  distance=strike$plate_z-ball$plate_z[i]
  minYTemp=min(abs(distance))
  maxYTemp=max(abs(distance))
  ballDist[i,]=c(ball$X[i],minXTemp,maxXTemp,minYTemp,maxYTemp)
}



for(i in 1:dim(strike)[1])
{
  ballTemp=cbind(ball$X,sqrt((ball$plate_x-strike$plate_x[j])^2+(ball$plate_z-strike$plate_z[j])^2))
  ballH=strike$plate_z[i]+.1
  ballL=strike$plate_z[i]-.1
  temp=ball[(ball$plate_z<ballH),]
  temp=temp[temp$plate_z>ballL,]
  if(length(ballTemp[ballTemp[,2]<.1])>0)
  distance=ball[ball$X %in% temp$X,]$plate_x-strike$plate_x[i]
  minXTemp=min(abs(distance))
  maxXTemp=max(abs(distance))
  
  ballH=strike$plate_x[i]+.1
  ballL=strike$plate_x[i]-.1
  temp=ball[(temp$plate_x<ballH),]
  temp=temp[ball$plate_x>ballL,]
  distance=ball[ball$X %in% temp$X,]$plate_z-strike$plate_z[i]
  
  #distance=ball$plate_z-strike$plate_z[i]
  minYTemp=min(abs(distance))
  maxYTemp=max(abs(distance))
  strikeDist[i,]=c(strike$X[i],minXTemp,maxXTemp,minYTemp,maxYTemp)
}


for(i in 1:dim(strike)[1])
{
  ballTemp=cbind(ball$X,sqrt((ball$plate_x-strike$plate_x[j])^2+(ball$plate_z-strike$plate_z[j])^2))
  minTemp=min(abs(ballTemp[,2]))
  strikeDist[i,]=c(strike$X[i],min(abs(ballTemp[,2])))
}
strikeDist=strikeDist[!(strikeDist[,1]==0)]
  
  
ballDist=ballDist[!(ballDist[,1]==0),]
ballDist=ballDist[!(ballDist[,2]==0),]
ballDist=ballDist[!(ballDist[,3]==0),]
ballDist=ballDist[!(ballDist[,4]==0),]

strikeDist=strikeDist[!(strikeDist[,1]==0),]
strikeDist=strikeDist[!(strikeDist[,2]==0),]
strikeDist=strikeDist[!(strikeDist[,3]==0),]
strikeDist=strikeDist[!(strikeDist[,4]==0),]

min(abs(ballDist[,1]))
max(abs(ballDist[,1]))

max(strike$plate_z)

kP1=pitches[pitches$plate_z<max(strike$plate_z),]

kP2=pitches[pitches$plate_z>min(strike$plate_z),]

kP3=pitches[pitches$plate_x<max(strike$plate_x),]
kP4=pitches[pitches$plate_x>min(strike$plate_x),]

kP1Temp=pitches[pitches$X %in% setdiff(pitches$X,kP1$X),]
kP2Temp=pitches[pitches$X %in% setdiff(pitches$X,kP2$X),]
kP3Temp=pitches[pitches$X %in% setdiff(pitches$X,kP3$X),]
kP4Temp=pitches[pitches$X %in% setdiff(pitches$X,kP4$X),]

kP=pitches[pitches$X %in% setdiff(pitches$X,kP1Temp$X),]
kP=kP[kP$X %in% setdiff(kP$X,kP2Temp$X),]
kP=kP[kP$X %in% setdiff(kP$X,kP3Temp$X),]
kP=kP[kP$X %in% setdiff(kP$X,kP4Temp$X),]

plot(kP1Temp$plate_x,kP1Temp$plate_z)
plot(kP$plate_x,kP$plate_z)

#esquisse::esquisser(kP)

ggplot(kP) +
  aes(x = plate_x, y = plate_z, colour = description) +
  geom_point(size = 1L) +
  scale_color_manual(values=c("#FF3333","#000066")) +
  theme_minimal()


temp1=kP[kP$X %in% strikeDist[abs(strikeDist[,2])<.1,1],]
temp2=kP[kP$X %in% strikeDist[abs(strikeDist[,3])<.1,2],]
temp3=kP[kP$X %in% strikeDist[abs(strikeDist[,3])<.1,3],]
temp4=kP[kP$X %in% strikeDist[abs(strikeDist[,3])<.1,4],]

temp=rbind(temp1,temp2,temp3,temp4)

temp=rbind(temp,kP[kP$description=="ball",])
#temp=rbind(temp,ball)

#esquisse::esquisser(temp)

plot(temp$plate_x,temp$plate_z)

ggplot(temp) +
  aes(x = plate_x, y = plate_z, colour = description) +
  geom_point(size = 1L) +
  scale_color_manual(values=c("#FF3333","#000066")) +
  theme_minimal()











