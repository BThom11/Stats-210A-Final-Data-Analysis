#Final Data Analysis, Stats 210a

#Set wd to wherever data is downloaded to
#setwd('C:/Users/rjtho/OneDrive/Desktop/Stats210/FinalAnalysis')

library(ggplot2)
LungDat=read.csv('lung.csv')

attach(LungDat)

#exploratory plot, FEV is dependent measure of lung health (Forced ? Volume), wrap is smoking

ggplot(aes(x=Age,y=FEV,group=Sex,color=Sex),data=LungDat)+ 
   geom_point()+
   facet_wrap(~Smoker)

#standard model, no interaction/transformation. Violations linear data as shown by Residuals on FEV
normmodelAdd=lm(FEV~ Smoker+Sex+Height+Age)
summary(normmodelAdd)
plot(normmodelAdd)

#Log transformed data, largely corrects for non linear pattern. Possible non-normality caused by few outliers,
#but not enough information to justify removing outliers.
modelAdd=lm(log(FEV) ~ Smoker+Sex+Height+Age)
modelAdd$stdres=rstandard(modelAdd)
summary(modelAdd)
plot(modelAdd$fitted.values,modelAdd$stdres)
plot(modelAdd)

#Interaction model
modelInt=lm(log(FEV) ~ Smoker*(Height+Sex+Age))
summary(modelInt)
anova(modelInt)
plot(modelInt)

#Model comparison yielded F=.79 to test whether all interaction coefficients were 0, hence simple additive model
#is adequete

##Basic model validation to test for overfitting.
TIndex=sample(nrow(LungDat),400)
LungDatFit=LungDat[TIndex,]
LungDatTest=LungDat[-TIndex,]

modelAdd=lm(log(FEV) ~ Smoker+Sex+Height+Age+Height,data=LungDatFit)
summary(modelAdd)
anova(modelAdd)

predictions=predict(modelAdd,LungDatTest)
error=(log(LungDatTest$FEV)-predictions)^2
mean(error)





