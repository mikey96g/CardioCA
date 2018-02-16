install.packages("nortest",dependencies = TRUE)
library(nortest)
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

#6.The level of correlation among predictor variables. Should there be any action taken?
#What is the correct action?
