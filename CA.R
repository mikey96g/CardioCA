#Michael Gallagher x00121692

#install.packages("nortest",dependencies = TRUE)
#install.packages("moments",dependencies = TRUE)
#install.packages('corrplot',dependencies = TRUE)
#install.packages("ggplot2")
#install.packages("missForest")

library(nortest)
library(moments)
library(corrplot)
library(ggplot2)
library(missForest)
Cardiology<-read.table("https://drive.google.com/uc?export=download&id=1Pto51euMg7A6-9zKShOCFAfYCjxGEdcC", stringsAsFactors=FALSE, sep =",",
                       header=TRUE)
#Cardiology<-read.table(file="H://github//CardioCA//CardiologyRel.csv",
#stringsAsFactors=FALSE, sep =",", header=TRUE)

#Cardiology<-read.table(file="C://Users//X00121692//Desktop//Github//CardioCA//CardiologyRel.csv",
#                       stringsAsFactors=FALSE, sep =",", header=TRUE)

#Checking Data
summary(Cardiology)
head(Cardiology)
table(Cardiology$age)
table(Cardiology$sex)
table(Cardiology$cp)
table(Cardiology$trestbps)
table(Cardiology$cholesterol)
table(Cardiology$Fasting.blood.sugar...120)
table(Cardiology$restecg)
table(Cardiology$diastbpexerc)
table(Cardiology$thalach)
table(Cardiology$exang)
table(Cardiology$oldpeak)
table(Cardiology$slope)
table(Cardiology$ca)
table(Cardiology$thal)
table(Cardiology$class)

#Moving values to vectors
age<-Cardiology$age
sex<-Cardiology$sex
cp<-Cardiology$cp
trestbps<-Cardiology$trestbps
cholesterol<-Cardiology$cholesterol
bSugar<-Cardiology$Fasting.blood.sugar...120
restecg<-Cardiology$restecg
diastbpexerc<-Cardiology$diastbpexerc
thalach<-Cardiology$thalach
exang<-Cardiology$exang
oldpeak<-Cardiology$oldpeak
slope<-Cardiology$slope
ca<-Cardiology$ca
thal<-Cardiology$thal
class<-Cardiology$class

#Change to factor if needed
class<-factor(class)
cp<-factor(cp)
bSugar<-factor(bSugar)
exang<-factor(exang)
slope<-factor(slope)
ca<-factor(ca)
thal<-factor(thal)
sex<-factor(sex)
restecg<-factor(restecg)

table(sex)
sex[sex == "f"]<-"Female"
sex[sex == "m"]<-"Male"
cp[cp == " Asymptomatic"] <- "Asymptomatic"
table(sex)
table(cp)

Heart<-data.frame(age,
                  sex,
                  cp,
                  trestbps,
                  cholesterol,
                  bSugar,
                  diastbpexerc,
                  thalach,
                  exang,
                  oldpeak,
                  slope,
                  ca,
                  thal,
                  class,
                  restecg)



#Find
#1.The attribute type, e.g. nominal, ordinal, numeric 
class(age)
class(sex)
class(cp)
class(trestbps)
class(cholesterol)
class(bSugar)
class(restecg)
class(diastbpexerc)
class(thalach)
class(exang)
class(oldpeak)
class(slope)
class(ca)
class(thal)
class(class)

#2.Percentage of missing values in the data
table(is.na(Cardiology))
percentageOf_missing_values<-(7/4613)*100

#3.Max, min, mean, mode, median standard deviation
summary(Cardiology)

#Function for the mode
Mode <- function(x)
{
 ux<- unique(x)
 ux[which.max(tabulate(match(x,ux)))]
}
Mode(age)
Mode(sex)
Mode(cp)
Mode(trestbps)
Mode(cholesterol)
Mode(bSugar)
Mode(restecg)
Mode(diastbpexerc)
Mode(thalach)
Mode(exang)
Mode(oldpeak)
Mode(slope)
Mode(ca)
Mode(thal)
Mode(class)

#Stanard Deviation
sd(age)
sd(trestbps)
sd(cholesterol,na.rm = TRUE)
sd(diastbpexerc)
sd(thalach)
sd(oldpeak)
#Remove Scientific Notation.
options(scipen=999)

#4.The type of distribution that the numeric attributes seem to follow (e.g. normal).
ad.test(age)
ad.test(cholesterol)
ad.test(diastbpexerc)
ad.test(oldpeak)
ad.test(thalach)
ad.test(trestbps)

shapiro.test(age)
shapiro.test(cholesterol)
shapiro.test(diastbpexerc)
shapiro.test(oldpeak)
shapiro.test(thalach)
shapiro.test(trestbps)


#plotting for normaility
#normal - maybe
qqnorm(age)
qqline(age,col = "red")
hist(age)

#normal
qqnorm(cholesterol)
qqline(cholesterol,col = "red")
hist(cholesterol)

#not-normal
qqnorm(diastbpexerc)
qqline(diastbpexerc,col = "red")
hist(diastbpexerc)

#not-normal
qqnorm(oldpeak)
qqline(oldpeak,col = "red")
hist(oldpeak)

#not-normal- maybe 
qqnorm(thalach)
qqline(thalach,col = "red")
hist(thalach)

#not-normal
qqnorm(trestbps)
qqline(trestbps,col = "red")
hist(trestbps)



#5.Whether the numeric data is skewed and the type of skewness
skewness(age)
skewness(cholesterol,na.rm = TRUE)
skewness(diastbpexerc)
skewness(oldpeak)
skewness(thalach)
skewness(trestbps)

#6.Correlation
#new dataframe giving interger values to categorical variables to calculate correlation
HeartCorr<-data.frame(age,
                  as.integer(sex),
                  as.integer(cp),
                  trestbps,
                  #cholesterol,
                  as.integer(bSugar),
                  diastbpexerc,
                  thalach,
                  as.integer(exang),
                  oldpeak,
                  as.integer(slope),
                  as.integer(ca),
                  as.integer(thal)
                  #as.integer(restecg)
                  )

M<-cor(HeartCorr)
corrplot(M,type = "upper",method = 'number')
M




#2 Histogram Overlay
HistogramOverlay <- function(nVal,label){
  ggplot(Heart,aes(x=nVal,fill=class))+
  geom_histogram(position = "identity",alpha=0.4,binwidth = 8)+theme_bw()+ xlab(label)
}
#separate call on cholesterol and thalach to fix binwidth
ggplot(Heart,aes(x=cholesterol,fill=class))+
  geom_histogram(position = "identity",alpha=0.4,binwidth = 40)+theme_bw()+ xlab("cholesterol")
ggplot(Heart,aes(x=thalach,fill=class))+
  geom_histogram(position = "identity",alpha=0.4,binwidth = 40)+theme_bw()+ xlab("Max Heart Rate")

HistogramOverlay(age,"Age")
HistogramOverlay(diastbpexerc,"Diastolic blood pressure during excercise")
HistogramOverlay(oldpeak,"oldpeak")
HistogramOverlay(trestbps,"Resting blood pressure")

#3 Barchart overlay
BarChartOverlay<- function(cVal,label){
ggplot(Heart,aes(x=cVal,fill=class))+
  geom_bar(position = "identity",alpha=0.4)+theme_bw()+ xlab(label)
}


BarChartOverlay(cp,"Chest pain type")
BarChartOverlay(bSugar,"Fasting Blood Sugar")
BarChartOverlay(exang,"Exercise induced angina")
BarChartOverlay(slope,"The slope of the peak exercise")
BarChartOverlay(ca,"Number of major vessels")
BarChartOverlay(thal,"thal")
BarChartOverlay(sex,"Gender")
BarChartOverlay(restecg,"Resting electrocardiographic results")

#4 Finding outliers
boxplot(age,main="age")
boxplot(trestbps,main="trestbps")
boxplot(cholesterol,main="cholesterol")
boxplot(diastbpexerc,main="diastbpexerc")
boxplot(thalach,main="thalach")
boxplot(oldpeak,main="oldpeak")


BarChartOutlier<- function(oVal){
  ggplot(Heart,aes(x=oVal))+
    geom_bar()
}

BarChartOutlier(class)
BarChartOutlier(cp)
BarChartOutlier(bSugar)
BarChartOutlier(exang)
BarChartOutlier(slope)
BarChartOutlier(ca)
BarChartOutlier(thal)
BarChartOutlier(sex)
BarChartOutlier(restecg)
#Z-score test
trestbps_sd <- sd(trestbps)
trestbps_mean<-mean(trestbps)
z<-(192-trestbps_mean)/trestbps_sd
z

#IQR Test
quantile(trestbps)
IQR(trestbps)
140+(1.5*20)



#5
# scatter function plot for correlation
scatterPlot<-function(v1,v2,xVal,yVal)
  {
ggplot(Heart,aes(x=v1,v2))+
  geom_point(size=1)+ xlab(xVal) + ylab(yVal)+
  geom_smooth(method = "lm", se = FALSE)
  
}

# scatter plots numeric only need to finish
scatterPlot(trestbps,age,"Resting Blood Pressure","age")
scatterPlot(trestbps,cholesterol,"Resting Blood Pressure","cholesterol")
scatterPlot(trestbps,diastbpexerc,"Resting Blood Pressure","diastbpexerc")
scatterPlot(trestbps,thalach,"Resting Blood Pressure","thalach")
scatterPlot(trestbps,oldpeak,"Resting Blood Pressure","oldpeak")
scatterPlot(age,cholesterol,"age","cholesterol")
scatterPlot(age,diastbpexerc,"age","diastbpexerc")
scatterPlot(age,thalach,"age","thalach")
scatterPlot(age,oldpeak,"age","oldpeak")
scatterPlot(cholesterol,diastbpexerc,"cholesterol","diastbpexerc")
scatterPlot(cholesterol,thalach,"cholesterol","thalach")
scatterPlot(cholesterol,oldpeak,"cholesterol","oldpeak")
scatterPlot(oldpeak,diastbpexerc,"oldpeak","diastbpexerc")
scatterPlot(oldpeak,thalach,"oldpeak","thalach")
scatterPlot(thalach,diastbpexerc,"thalach","diastbpexerc")

#verify correlation
cor(trestbps,diastbpexerc,method = c("pearson"));
cor(trestbps,diastbpexerc,method = c("kendall"));
cor(trestbps,diastbpexerc,method = c("spearman"));



#######PART 2#######  
#install.packages("classInt",dependencies = TRUE)
#library(classInt)
n<-length(age)
#Declare number of bins and bin indicator
nbins<-4
whichbin<-c(rep(0,n)) 
whichbin
#Equal Frequencey(Equal Depth)
freq<-n/nbins
#sort the data
xsorted<-sort(age)
for (i in 1:nbins)
{
  for(j in 1:n)
  {
    if((i-1)*freq<j&&j<=i*freq)
      whichbin[j]<-i
  }
  
}
whichbin
xsorted

#equal-width-4 bins
range.age<-max(age)-min(age)+1
binwidth<-round(range.age/nbins)
for(i in 1:nbins)
{
  for(j in 1:n)
  {
    if((i-1)*binwidth < age[j]&&age[j]<=(i)*binwidth)
      whichbin[j]<-i
    
  }
  
}
whichbin
age

#K-means clustering as a binning strategy where k=4
kmeansclustering<- kmeans(age,centers = nbins)
k.age<-kmeansclustering$cluster
k.age
age

#7.Skewness
#Z-score stanard
skewness(oldpeak)
z.oldpeak<- scale(oldpeak, center = TRUE, scale = TRUE)
skewness(z.oldpeak)

#natural log
natlog.oldpeak<-log(Cardiology$oldpeak)
natlog.oldpeak
skewness(natlog.oldpeak)

#Square Root
sq.oldpeak<-sqrt(Cardiology$oldpeak)
skewness(sq.oldpeak)

#Inverse
invsqrt.oldpeak<-1/sqrt(Cardiology$oldpeak)
invsqrt.oldpeak
skewness(invsqrt.oldpeak)

#https://stackoverflow.com/questions/20254084/plot-normal-left-and-right-skewed-distribution-in-r
#machiene learning 
install.packages("rpart",dependencies = TRUE)
library(rpart)
 
summary(restecg)


cholesterol[is.na(cholesterol)]<-240
summary(cholesterol)
summary(class)
class[is.na(class)]<-"Healthy"
summary(class)
new.restecg <-restecg
restecg

dataImp <- data.frame(k.age,
                   sex,
                   cp,
                   trestbps,
                   cholesterol,
                   bSugar,
                   diastbpexerc,
                   thalach,
                   exang,
                   sq.oldpeak,
                   slope,
                   ca,
                   thal,
                   class,
                   new.restecg)

newHeart <-missForest(dataImp)
newHeart <-missForest(dataImp,ntree = 20)
newHeart <-missForest(dataImp,ntree = 40)


newHeart$ximp
newHeart$OOBerror
  