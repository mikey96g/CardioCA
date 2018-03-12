#Michael Gallagher x00121692
#install.packages("nortest",dependencies = TRUE)
#install.packages("moments",dependencies = TRUE)
#install.packages('corrplot',dependencies = TRUE)
#install.packages("ggplot2")
#install.packages("matlab",dependencies = TRUE)
install.packages("missForest")

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
cholestral<-Cardiology$cholesterol
bSugar<-Cardiology$Fasting.blood.sugar...120
restecg<-Cardiology$restecg
diastbpererc<-Cardiology$diastbpexerc
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
table(Heart$sex)
table(Heart$cp)

Heart<-data.frame(age,
                  sex,
                  cp,
                  trestbps,
                  cholestral,
                  bSugar,
                  diastbpererc,
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
class(cholestral)
class(bSugar)
class(restecg)
class(diastbpererc)
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
Mode(cholestral)
Mode(bSugar)
Mode(restecg)
Mode(diastbpererc)
Mode(thalach)
Mode(exang)
Mode(oldpeak)
Mode(slope)
Mode(ca)
Mode(thal)
Mode(class)

sd(age)
sd(trestbps)
sd(cholestral,na.rm = TRUE)
sd(diastbpererc)
sd(thalach)
sd(oldpeak)



#4.The type of distribution that the numeric attributes seem to follow (e.g. normal).
ad.test(age)
ad.test(cholestral)
ad.test(ca)
ad.test(diastbpererc)
ad.test(oldpeak)
ad.test(thalach)
ad.test(trestbps)

#5.Whether the numeric data is skewed and the type of skewness
skewness(age)
skewness(cholestral,na.rm = TRUE)
skewness(diastbpererc)
skewness(oldpeak)
skewness(thalach)
skewness(trestbps)

#6.The level of correlation among predictor variables. Should there be any action taken?
#What is the correct action?
d <- data.frame(age,
                sex=rnorm(308),
                cp=rnorm(308),
                trestbps,
                cholestral=rnorm(308),
                bSugar=rnorm(308),
                restecg=rnorm(308),
                diastbpererc,
                thalach,
                exang=rnorm(308),
                oldpeak,
                slope=rnorm(308),
                ca=rnorm(308),
                thal=rnorm(308),
                class=rnorm(308))

M<-cor(d)
corrplot(M,method = 'ellipse')
cor(d)

data <- data.frame(age,
                sex,
                cp,
                trestbps,
                cholestral,
                bSugar,
                diastbpererc,
                thalach,
                exang,
                oldpeak,
                slope,
                ca,
                thal,
                class,
                restecg)


#ggplot(data,aes(x=age))+geom_histogram(binwidth = 3) +
#  geom_density(aes(n=class, y=..density..*n,color='cut',))

#2 Histogram Overlay
HistogramOverlay <- function(nVal){
  ggplot(data,aes(x=nVal,fill=class))+
  geom_histogram(position = "identity",alpha=0.4,binwidth = 3)+theme_bw()
}


HistogramOverlay(age)
HistogramOverlay(cholestral)
HistogramOverlay(diastbpererc)
HistogramOverlay(oldpeak)
HistogramOverlay(thalach)
HistogramOverlay(trestbps)

#3 Barchart overlay
BarChartOverlay<- function(cVal){
ggplot(data,aes(x=cVal,fill=class))+
  geom_bar(position = "identity",alpha=0.4)+theme_bw()
}


BarChartOverlay(class)
BarChartOverlay(cp)
BarChartOverlay(bSugar)
BarChartOverlay(exang)
BarChartOverlay(slope)
BarChartOverlay(ca)
BarChartOverlay(thal)
BarChartOverlay(sex)
BarChartOverlay(restecg)

#4 Finding outliers
boxplot(age)
boxplot(trestbps)
boxplot(cholestral)
boxplot(diastbpererc)
boxplot(thalach)
boxplot(oldpeak)
#boxplot(sex)
#boxplot(cp)
#boxplot(bSugar)
#boxplot(restecg)
#boxplot(exang)
#boxplot(slope)
#boxplot(ca)
#boxplot(thal)
#boxplot(class)

OUTLIERhistogram<- function(oVal)
  {
  ggplot(data,aes(x=oVal
                  ))+    
  geom_histogram(binwidth = 3)
}
OUTLIERhistogram(age)
OUTLIERhistogram(trestbps)
OUTLIERhistogram(cholestral)
OUTLIERhistogram(diastbpererc)
OUTLIERhistogram(thalach)
OUTLIERhistogram(oldpeak)
#OUTLIERhistogram(sex)
#OUTLIERhistogram(cp)
#OUTLIERhistogram(bSugar)
#OUTLIERhistogram(restecg)
#OUTLIERhistogram(exang)
#OUTLIERhistogram(slope)
#OUTLIERhistogram(ca)
#OUTLIERhistogram(thal)
#OUTLIERhistogram(class)

BarChartOutlier<- function(oVal){
  ggplot(data,aes(x=oVal))+
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

#5
#install.packages('car', dependencies = TRUE)
#library(car)
# scatter function plot for correlation
scatterPlot<-function(v1,v2)
  {
ggplot(data,aes(x=v1,v2))+
  geom_point(size=1)+
  geom_smooth(method = "lm", se = FALSE)
}

# scatter plots numeric only need to finish
scatterPlot(trestbps,age)

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
which<-kmeansclustering$cluster
whichbin
age

#7.Skewness
#Z-score stanard



#natural log
natlog.oldpeak<-log(Cardiology$oldpeak)
natlog.oldpeak
skewness(natlog.oldpeak)

#Square Root
oldpeak<-sqrt(Cardiology$oldpeak)
skewness(oldpeak)

#Inverse
invsqrt.oldpeak<-1/sqrt(Cardiology$oldpeak)
invsqrt.oldpeak
skewness(invsqrt.oldpeak)

#https://stackoverflow.com/questions/20254084/plot-normal-left-and-right-skewed-distribution-in-r
#machiene learning 
install.packages("rpart",dependencies = TRUE)
library(rpart)
 
#grow tree https://www.analyticsvidhya.com/blog/2016/04/complete-tutorial-tree-based-modeling-scratch-in-python/
#fit rpart(Cardiology ~)
summary(restecg)
missForest(restecg)
summary(d)

cholestral[is.na(cholestral)]<-240
summary(cholestral)
summary(class)
class[is.na(class)]<-"Healthy"
summary(class)


dataImp <- data.frame(age,
                   sex,
                   cp,
                   trestbps,
                   cholestral,
                   bSugar,
                   diastbpererc,
                   thalach,
                   exang,
                   oldpeak,
                   slope,
                   ca,
                   thal,
                   class,
                   restecg)
ooga <-missForest(dataImp)
ooga$ximp
ooga$OOBerror
  