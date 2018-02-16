#theUrl <- "https://github.com/mikey96g/CardioCA/blob/master/CardiologyRel.csv"
#Cardiology <-read.table(file = theUrl,header = TRUE,sep = ",",stringsAsFactors=FALSE)
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

summary(Cardiology)
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
(7/4613)*100
#3.Max, min, mean, mode, median standard deviation
max(age)
max(sex)
max()
#4.The type of distribution that the numeric attributes seem to follow (e.g. normal).
#5.Whether the numeric data is skewed and the type of skewness
#6.The level of correlation among predictor variables. Should there be any action taken?
#What is the correct action?
