#remotes::install_github("easystats/report") # You only need to do that once

library(report)
pooled1<-lm(data=LatAm2014,gini100 ~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment)
report(pooled1)
pooled2<-lm(data=LatAm2014,gini100 ~ SimpsonDiversity+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment)
report(pooled2)
pooled3<-glm(data=LatAm2014,gini100 ~ EthnicFractionalizationTS+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment)
report(pooled3)


model <- lm(Sepal.Length ~ Species, data = iris)
report(model)
summary(0.1*LatAm2014$Amerindians)
summary(1.1*LatAm2014$SimpsonDiversity)
summary(3.8*LatAm2014$EthnicFractionalizationTS)
summary(LatAm2014$gini100)
52.72-45.63

summary(-0.62*LatAm2014$socexpgdp)
summary(-0.46*LatAm2014$socexpgdp)
summary(-0.45*LatAm2014$socexpgdp)

summary(1.11*LatAm2014$educexpgdp)
summary(1.56*LatAm2014$educexpgdp)
summary(1.49*LatAm2014$educexpgdp)

summary(0.74*LatAm2014$PresidentPartyLeftRightIndex_mean)
summary(1.1*LatAm2014$PresidentPartyLeftRightIndex_mean)
summary(1.01*LatAm2014$PresidentPartyLeftRightIndex_mean)

summary(0.2*LatAm2014$highlyeducatedlabor)
summary(0.15*LatAm2014$highlyeducatedlabor)
summary(0.16*LatAm2014$highlyeducatedlabor)

summary(0.09*LatAm2014$employment)
summary(-0.16*LatAm2014$employment)
summary(-0.15*LatAm2014$employment)


library(effects)
library(sjPlot)
pooled3$coefficients
plot_model(pooled3,type="pred",
           terms="PresidentPartyLeftRightIndex_mean",
           title = "Predicted values of Gini Income Inequality",
           y)
?grid.arrange


plot_model