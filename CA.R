#Michael Gallagher x00121692
install.packages("nortest",dependencies = TRUE)
library(nortest)
install.packages("moments",dependencies = TRUE)
library(moments)
install.packages('corrplot',dependencies = TRUE)
library(corrplot)
install.packages("ggplot2")
library(ggplot2)
#ggloop
Cardiology<-read.table("C:/Users/x00121692/Downloads/CardiologyRel.csv", stringsAsFactors=FALSE, sep =",",
                       header=TRUE)

head(Cardiology)
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
thal<-factor(tahl)
sex<-factor(sex)
restecg<-factor(restecg)


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
sd(cholestral)
sd(bSugar)
sd(diastbpererc)
sd(thalach)
sd(exang)
sd(oldpeak)
sd(ca)


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
skewness(cholestral)
skewness(ca)
skewness(diastbpererc)
skewness(oldpeak)
skewness(thalach)
skewness(trestbps)

#6.The level of correlation among predictor variables. Should there be any action taken?
#What is the correct action?
d <- data.frame(age=rnorm(308),
                sex=rnorm(308),
                cp=rnorm(308),
                trestbps=rnorm(308),
                cholestral=rnorm(308),
                bSugar=rnorm(308),
                restecg=rnorm(308),
                diastbpererc=rnorm(308),
                thalach=rnorm(308),
                exang=rnorm(308),
                oldpeak=rnorm(308),
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