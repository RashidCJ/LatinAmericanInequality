
# Setup, load, initial cleaning -------------------------------------------

setwd("/N/slate/rmarcano/LatinAmericapaper")
rm(list = ls( )) 
#load("LatinAmerica14.RData")
#load("LatAmBayesian.RData")
library(readxl)
LatAm2014 <- read_excel("LatAm2014.xlsx", col_types = c("text", "text", "numeric", 
                                                        "numeric", "text", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "text", "text", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "text", "text", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "text", "text"))
attach(LatAm2014)
unique(LatAm2014$country)
LatAm2014$gini100<-gini_equivalized*100

library(stargazer)
library(emmeans)
library(ggeffects)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(bayesplot)
library(ggridges)
library(sjPlot)
library(insight)
library(httr)
library(brms)
library(devtools)
library(tidybayes)
library(brms)
library(cowplot)
install.packages("sjmisc")
library(lme4)
theme_set(theme_tidybayes() + panel_border())
set.seed(42)


# Initial view, summary ---------------------------------------------------

summary(LatAm2014$gini100)
labels(LatAm2014)
set_labels(LatAm2014, c("Year", "Country", "CountryCode","Equivalized Gini Inequality"))
LatAm2014 
   

# Initial model computation -----------------------------------------------

empty.model<-(lmer(gini100~(1|country),LatAm2014)) #null model
empty.model.Bayesian<-brm(data = LatAm2014, family = gaussian,
                          gini100~(1|country),
                          prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                    prior(cauchy(0,2), class = sigma)),
                          control=list(adapt_delta=0.99, max_treedepth=20),
                          iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)
empty.model.Bayesian
Uncond.Growth.Model<-(lmer(gini100~year+(1|country),LatAm2014)) #unconditional growth model
Uncond.Growth.Model.Bayesian<-brm(data = LatAm2014, family = gaussian,
                                  gini100~year+(1|country),
                                  prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                            prior(normal(0, 2), class = b),
                                            prior(cauchy(0,2), class = sigma)),
                                  control=list(adapt_delta=0.99, max_treedepth=20),
                                  iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)
PooledRace<-brm(data = LatAm2014, family = gaussian,
                gini100 ~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment,
                prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                          prior(normal(0, 2), class = b),
                          prior(cauchy(0,5), class = sigma)),
                control=list(adapt_delta=0.99, max_treedepth=20),
                iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)
PooledRace
PooledRace.lag<-brm(data = LatAm2014, family = gaussian,
                        gini100 ~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_meanlag+wage.arrangement+highlyeducatedlabor+logminwage+employment,
                        prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                  prior(normal(0, 2), class = b),
                                  prior(cauchy(0,5), class = sigma)),
                        control=list(adapt_delta=0.99, max_treedepth=20),
                        iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)
PooledRaceArea<-mcmc_areas(
  PooledRace, 
  pars = c("b_Blacks","b_Mulattoes","b_Mestizo","b_Amerindians","b_Asians","b_CreolesEtGafurinas","b_socexpgdp","b_educexpgdp",
           "b_PresidentPartyLeftRightIndex_mean","b_wage.arrangementRegional","b_wage.arrangementSegmented","b_highlyeducatedlabor","b_logminwage","b_employment"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean")
PooledDiversity<-brm(data = LatAm2014, family = gaussian,
                     gini100 ~ SimpsonDiversity+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment,
                     prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                               prior(normal(0, 2), class = b),
                               prior(cauchy(0,5), class = sigma)),
                     control=list(adapt_delta=0.99, max_treedepth=20),
                     iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)
PooledDiversity
PooledDiversity.lag<-brm(data = LatAm2014, family = gaussian,
                             gini100 ~ SimpsonDiversity+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_meanlag+wage.arrangement+highlyeducatedlabor+logminwage+employment,
                             prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                       prior(normal(0, 2), class = b),
                                       prior(cauchy(0,5), class = sigma)),
                             control=list(adapt_delta=0.99, max_treedepth=20),
                             iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)

PooledHeterogeneity<-brm(data = LatAm2014, family = gaussian,
                      gini100 ~ HeterogeneityIndex+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment,
                      prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                prior(normal(0, 2), class = b),
                                prior(cauchy(0,5), class = sigma)),
                      control=list(adapt_delta=0.99, max_treedepth=20),
                      iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)


PooledHeterogeneity

PooledFractionalization<-brm(data = LatAm2014, family = gaussian,
                         gini100 ~ Fractionalization+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment,
                         prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                   prior(normal(0, 2), class = b),
                                   prior(cauchy(0,5), class = sigma)),
                         control=list(adapt_delta=0.99, max_treedepth=20),
                         iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)
PooledFractionalization

PooledFractional.lag<-brm(data = LatAm2014, family = gaussian,
                          gini100 ~ EthnicFractLag1+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_meanlag+wage.arrangement+highlyeducatedlabor+logminwage+employment,
                          prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                    prior(normal(0, 2), class = b),
                                    prior(cauchy(0,5), class = sigma)),
                          control=list(adapt_delta=0.99, max_treedepth=20),
                          iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)

PooledFractional<-brm(data = LatAm2014, family = gaussian,
                       gini100 ~ EthnicFractionalizationTS+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment,
                       prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                 prior(normal(0, 2), class = b),
                                 prior(cauchy(0,5), class = sigma)),
                       control=list(adapt_delta=0.99, max_treedepth=20),
                       iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)

tab_model(PooledRace,PooledHeterogeneity,PooledDiversity,PooledDiversity.lag)

tab_model(PooledFractional,PooledFractional.lag, PooledFractionalization, PooledDiversity)


# Extraction of Models for Table 1 ----------------------------------------


summary(empty.model.Bayesian,robust = TRUE)
summary(Uncond.Growth.Model.Bayesian,robust = TRUE)
round(fixef(empty.model.Bayesian,robust = TRUE),digits = 2)
round(fixef(Uncond.Growth.Model.Bayesian,robust = TRUE),digits = 2)

round(fixef(Uncond.Growth.Model.Bayesian,robust = TRUE),digits = 2)



4.12^2
a<-4.12^2
3.03 ^2
5.98^2
3.13^2
b<-3.13^2
2.93^2
3.36^2
rho<-a/(a+b)
rho # 64% of variation in Income Inequality in LatAm is explained just by country

4.15^2
a1<-4.15^2
3.06 ^2
5.99^2
2.32^2
b1<-2.32^2
2.16^2
2.49^2


#variance of residual changed from null to uncond.growth from 9.8 to 5.38
rho<-a1/(a1+b1)
rho
sigmachange<-(b-b1)/b 
sigmachange
#45.40955%, that is, we may conclude 46% of within-country changes 
#are systematically associated with time, year.

16.9744/(16.9744+4.6656)
9.1809-4.6656
4.5153/9.1809
tab_model(empty.model.Bayesian,show.re.var = TRUE)
tab_model(Uncond.Growth.Model.Bayesian)

tab_model(empty.model.Bayesian,Uncond.Growth.Model.Bayesian,show.re.var = TRUE)

# Extraction of Models for Table 2 ----------------------------------------

PooledRace$model

round(fixef(PooledRace,robust = TRUE),digits = 2)
round(fixef(PooledDiversity,robust = TRUE),digits = 2)
round(fixef(PooledFractional2,robust = TRUE),digits = 2)

tab_model(PooledRace,PooledDiversity,PooledFractional,PooledFractional2, PooledFractional.lag,show.re.var = TRUE)
plot_model(PooledRace,type = "diag")
plot_model(PooledRace, sort.est = TRUE,bpe.style = "dot", show.values = TRUE,bpe.color = "purple",vline.color = "red") + theme_sjplot() + coord_flip()
?plot_model

library(shinystan)
launch_shinystan(PooledRace)
#Bayes-R2 function using modelled (approximate) residual variance
#from https://avehtari.github.io/bayes_R2/bayes_R2.html 
bayes_R2 <- function(fit) {
  mupred <- rstanarm::posterior_epred(fit)
  var_mupred <- apply(mupred, 1, var)
  if (family(fit)$family == "binomial" && NCOL(y) == 1) {
    sigma2 <- apply(mupred*(1-mupred), 1, mean)
  } else {
    sigma2 <- as.matrix(fit, pars = c("sigma"))^2
  }
  var_mupred / (var_mupred + sigma2)
}
#Bayes-R2 function using residuals
bayes_R2_res <- function(fit) {
  y <- rstanarm::get_y(fit)
  ypred <- rstanarm::posterior_epred(fit)
  if (family(fit)$family == "binomial" && NCOL(y) == 2) {
    trials <- rowSums(y)
    y <- y[, 1]
    ypred <- ypred %*% diag(trials)
  }
  e <- -1 * sweep(ypred, 2, y)
  var_ypred <- apply(ypred, 1, var)
  var_e <- apply(e, 1, var)
  var_ypred / (var_ypred + var_e)
}

summary(bayes_R2(PooledRace))
summary(bayes_R2_res(PooledRace))
summary(bayes_R2(PooledDiversity))
summary(bayes_R2_res(PooledDiversity))
summary(bayes_R2(PooledFractional))
summary(bayes_R2(PooledHeterogeneity))
summary(bayes_R2_res(PooledHeterogeneity))
summary(PooledHeterogeneity,robust = TRUE)
3.77^2
summary(bayes_R2(PooledFractional))
summary(bayes_R2_res(PooledFractional))
summary(PooledFractional2,robust = TRUE)

summary(bayes_R2(PooledFractional2))
summary(bayes_R2_res(PooledFractional2))
summary(bayes_R2(PooledFractional.lag))
summary(bayes_R2_res(PooledFractional.lag))
summary(bayes_R2(PooledFractionalization))
summary(bayes_R2_res(PooledFractionalization))
summary(PooledFractionalization,robust = TRUE)
summary(PooledFractional,robust = TRUE)
summary(PooledFractional.lag,robust = TRUE)
3.79^2

summary(RandomSlopes13,robust = TRUE)





performance::variance_decomposition(PooledRace)

print(performance::icc(PooledFractional2),digits=8)
VarianceDec1<-performance::variance_decomposition(RandomSlopes13)
VarianceDec2<-performance::variance_decomposition(RandomSlopes14)
VarianceDec3<-performance::variance_decomposition(RandomSlopes15)
print(VarianceDec1,digits=4)
print(VarianceDec2,digits=4)
print(VarianceDec3,digits=4)


print(performance::icc(RandomSlopes13),digits=8)
print(performance::icc(RandomSlopes14),digits=8)
print(performance::icc(RandomSlopes15),digits=8)


PooledRace<-brm(data = LatAm2014, family = gaussian,
                gini100 ~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,
                prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                          prior(normal(0, 2), class = b),
                          prior(cauchy(0,5), class = sigma)),
                control=list(adapt_delta=0.99, max_treedepth=20),
                iter = 10000, warmup = 2500, chains = 5, cores = 1,seed = 5)
summary(PooledRace)
tab_model(PooledRace)
PooledRaceArea<-mcmc_areas(
  PooledRace, 
  pars = c("b_Blacks","b_Mulattoes","b_Mestizo","b_Amerindians","b_Asians","b_CreolesEtGafurinas","b_socexpgdp","b_educexpgdp",
           "b_left","b_wage.arrangementRegional","b_wage.arrangementSegmented","b_highlyeducatedlabor","b_logminwage","b_employment"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean")

#forest(PooledRace)
summary(LatAm2014$Blacks*100*0.93)
summary(LatAm2014$Mulattoes*100*0.06)
summary(LatAm2014$Mestizo*100*0.04)
summary(LatAm2014$Amerindians*100*0.01)
summary(LatAm2014$CreolesEtGafurinas*100*0.23)
summary(LatAm2014$Asians*100*0.72)
summary(LatAm2014$SimpsonDiversity*0.61)
summary(LatAm2014$socexpgdp*0.25)
summary(LatAm2014$educexpgdp*0.8)
summary(LatAm2014$highlyeducatedlabor*0.07)
summary(LatAm2014$logminwage*0.86)
7.138*(0.86/100)
summary(LatAm2014$employment*0.14)



PooledDiversityArea<-mcmc_areas(
  PooledDiversity, 
  pars = c("b_SimpsonDiversity","b_socexpgdp","b_educexpgdp",
           "b_PresidentPartyLeftRightIndex_mean","b_wage.arrangementRegional","b_wage.arrangementSegmented","b_highlyeducatedlabor","b_logminwage","b_employment"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean")

RandInt.Race<-brm(data = LatAm2014, family = gaussian,
                  gini100 ~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment+year+(1|country),
                  prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                            prior(normal(0, 2), class = b),
                            prior(cauchy(0,5), class = sigma)),
                  control=list(adapt_delta=0.99, max_treedepth=20),
                  iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)

RandInt.Diversity<-brm(data = LatAm2014, family = gaussian,
                       gini100 ~ SimpsonDiversity+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment+year+(1|country),
                       prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                 prior(normal(0, 2), class = b),
                                 prior(cauchy(0,5), class = sigma)),
                       control=list(adapt_delta=0.99, max_treedepth=20),
                       iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)

RandInt.Fractional<-brm(data = LatAm2014, family = gaussian,
                        gini100 ~ EthnicFractLag1+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment+year+(1|country),
                        prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                  prior(normal(0, 2), class = b),
                                  prior(cauchy(0,5), class = sigma)),
                        control=list(adapt_delta=0.99, max_treedepth=20),
                        iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)

RandInt.Fractional2<-brm(data = LatAm2014, family = gaussian,
                         gini100 ~ EthnicFractionalizationTS+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment+year+(1|country),
                         prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                   prior(normal(0, 2), class = b),
                                   prior(cauchy(0,5), class = sigma)),
                         control=list(adapt_delta=0.99, max_treedepth=20),
                         iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)

RandInt.Fractional3<-brm(data = LatAm2014, family = gaussian,
                         gini100 ~ EthnicFractionalizationTS+EthnicFractLag1+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment+year+(1|country),
                         prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                   prior(normal(0, 2), class = b),
                                   prior(cauchy(0,5), class = sigma)),
                         control=list(adapt_delta=0.99, max_treedepth=20),
                         iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)

RandomIntDiversity<-brmstools::forest(RandInt.Diversity,pars = c("Intercept","logminwage", "wage.arrangementregional",
                                                                 "SimpsonDiversity","educexpgdp",
                                                                 "PresidentPartyLeftRightIndex_mean","highlyeducatedlabor","socexpgdp","employment"))

RandomIntRace<-brmstools::forest(RandInt.Race,pars = c("Intercept","logminwage", "wage.arrangementregional",
                                                       "Blacks","Mulattoes","Mestizo","educexpgdp",
                                                       "PresidentPartyLeftRightIndex_mean","highlyeducatedlabor","socexpgdp","employment"))

RandomSlopes1<-brm(data = LatAm2014, family = gaussian,
                   gini100 ~ SimpsonDiversity+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+year+(1+PresidentPartyLeftRightIndex_mean|country),
                   prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                             prior(normal(0, 2), class = b),
                             prior(cauchy(0,2), class = sigma),
                             prior(lkj(2), class = cor)),
                   control=list(adapt_delta=0.999, max_treedepth=30),
                   iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes2<-brm(data = LatAm2014, family = gaussian,
                   gini100 ~ SimpsonDiversity+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+year+(1+PresidentPartyLeftRightIndex_mean|country),
                   prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                             prior(normal(0, 2), class = b),
                             prior(cauchy(0,2), class = sigma),
                             prior(lkj(2), class = cor)),
                   control=list(adapt_delta=0.999, max_treedepth=30),
                   iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes3<-brm(data = LatAm2014, family = gaussian,
                   gini100 ~ SimpsonDiversity+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment+year+(1+PresidentPartyLeftRightIndex_mean|country),
                   prior = c(prior(normal(51.24, 4), class = Intercept),
                             prior(normal(0, 2), class = b),
                             prior(cauchy(0,2), class = sigma),
                             prior(lkj(2), class = cor)),
                   control=list(adapt_delta=0.999, max_treedepth=30),
                   iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes4<-brm(data = LatAm2014, family = gaussian,
                   gini100 ~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+year+(1+PresidentPartyLeftRightIndex_mean|country),
                   prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                             prior(normal(0, 2), class = b),
                             prior(cauchy(0,2), class = sigma),
                             prior(lkj(2), class = cor)),
                   control=list(adapt_delta=0.999, max_treedepth=30),
                   iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes5<-brm(data = LatAm2014, family = gaussian,
                   gini100 ~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+year+(1+PresidentPartyLeftRightIndex_mean|country),
                   prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                             prior(normal(0, 2), class = b),
                             prior(cauchy(0,2), class = sigma),
                             prior(lkj(2), class = cor)),
                   control=list(adapt_delta=0.999, max_treedepth=30),
                   iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes6<-brm(data = LatAm2014, family = gaussian,
                   gini100 ~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment+year+(1+PresidentPartyLeftRightIndex_mean|country),
                   prior = c(prior(normal(51.24, 4), class = Intercept),
                             prior(normal(0, 2), class = b),
                             prior(cauchy(0,2), class = sigma),
                             prior(lkj(2), class = cor)),
                   control=list(adapt_delta=0.999, max_treedepth=30),
                   iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes7<-brm(data = LatAm2014, family = gaussian,
                   gini100 ~ DiverseFractional+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+year+(1+PresidentPartyLeftRightIndex_mean|country),
                   prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                             prior(normal(0, 2), class = b),
                             prior(cauchy(0,2), class = sigma),
                             prior(lkj(2), class = cor)),
                   control=list(adapt_delta=0.999, max_treedepth=30),
                   iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes8<-brm(data = LatAm2014, family = gaussian,
                   gini100 ~ DiverseFractional+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+year+(1+PresidentPartyLeftRightIndex_mean|country),
                   prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                             prior(normal(0, 2), class = b),
                             prior(cauchy(0,2), class = sigma),
                             prior(lkj(2), class = cor)),
                   control=list(adapt_delta=0.999, max_treedepth=30),
                   iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes9<-brm(data = LatAm2014, family = gaussian,
                   gini100 ~ DiverseFractional+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment+year+(1+PresidentPartyLeftRightIndex_mean|country),
                   prior = c(prior(normal(51.24, 4), class = Intercept),
                             prior(normal(0, 2), class = b),
                             prior(cauchy(0,2), class = sigma),
                             prior(lkj(2), class = cor)),
                   control=list(adapt_delta=0.999, max_treedepth=30),
                   iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes10<-brm(data = LatAm2014, family = gaussian,
                    gini100 ~ EthnicFractLag1+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+year+(1+PresidentPartyLeftRightIndex_mean|country),
                    prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                              prior(normal(0, 2), class = b),
                              prior(cauchy(0,2), class = sigma),
                              prior(lkj(2), class = cor)),
                    control=list(adapt_delta=0.999, max_treedepth=30),
                    iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes11<-brm(data = LatAm2014, family = gaussian,
                    gini100 ~ EthnicFractLag1+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+year+(1+PresidentPartyLeftRightIndex_mean|country),
                    prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                              prior(normal(0, 2), class = b),
                              prior(cauchy(0,2), class = sigma),
                              prior(lkj(2), class = cor)),
                    control=list(adapt_delta=0.999, max_treedepth=30),
                    iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes12<-brm(data = LatAm2014, family = gaussian,
                    gini100 ~ EthnicFractLag1+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment+year+(1+PresidentPartyLeftRightIndex_mean|country),
                    prior = c(prior(normal(51.24, 4), class = Intercept),
                              prior(normal(0, 2), class = b),
                              prior(cauchy(0,2), class = sigma),
                              prior(lkj(2), class = cor)),
                    control=list(adapt_delta=0.999, max_treedepth=30),
                    iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes13<-brm(data = LatAm2014, family = gaussian,
                    gini100 ~ EthnicFractionalizationTS+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+year+(1+PresidentPartyLeftRightIndex_mean|country),
                    prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                              prior(normal(0, 2), class = b),
                              prior(cauchy(0,2), class = sigma),
                              prior(lkj(2), class = cor)),
                    control=list(adapt_delta=0.999, max_treedepth=30),
                    iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes14<-brm(data = LatAm2014, family = gaussian,
                    gini100 ~ EthnicFractionalizationTS+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+year+(1+PresidentPartyLeftRightIndex_mean|country),
                    prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                              prior(normal(0, 2), class = b),
                              prior(cauchy(0,2), class = sigma),
                              prior(lkj(2), class = cor)),
                    control=list(adapt_delta=0.999, max_treedepth=30),
                    iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes15<-brm(data = LatAm2014, family = gaussian,
                    gini100 ~ EthnicFractionalizationTS+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment+year+(1+PresidentPartyLeftRightIndex_mean|country),
                    prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                              prior(normal(0, 2), class = b),
                              prior(cauchy(0,2), class = sigma),
                              prior(lkj(2), class = cor)),
                    control=list(adapt_delta=0.999, max_treedepth=30),
                    iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes1.lag<-brm(data = LatAm2014, family = gaussian,
                       gini100 ~ SimpsonDiversity+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_meanlag+year+(1+PresidentPartyLeftRightIndex_meanlag|country),
                       prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                 prior(normal(0, 2), class = b),
                                 prior(cauchy(0,2), class = sigma),
                                 prior(lkj(2), class = cor)),
                       control=list(adapt_delta=0.999, max_treedepth=30),
                       iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes2.lag<-brm(data = LatAm2014, family = gaussian,
                       gini100 ~ SimpsonDiversity+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_meanlag+wage.arrangement+year+(1+PresidentPartyLeftRightIndex_meanlag|country),
                       prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                 prior(normal(0, 2), class = b),
                                 prior(cauchy(0,2), class = sigma),
                                 prior(lkj(2), class = cor)),
                       control=list(adapt_delta=0.999, max_treedepth=30),
                       iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes3.lag<-brm(data = LatAm2014, family = gaussian,
                       gini100 ~ SimpsonDiversity+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_meanlag+wage.arrangement+highlyeducatedlabor+logminwage+employment+year+(1+PresidentPartyLeftRightIndex_meanlag|country),
                       prior = c(prior(normal(51.24, 4), class = Intercept),
                                 prior(normal(0, 2), class = b),
                                 prior(cauchy(0,2), class = sigma),
                                 prior(lkj(2), class = cor)),
                       control=list(adapt_delta=0.999, max_treedepth=30),
                       iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes4.lag<-brm(data = LatAm2014, family = gaussian,
                       gini100 ~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_meanlag+year+(1+PresidentPartyLeftRightIndex_meanlag|country),
                       prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                 prior(normal(0, 2), class = b),
                                 prior(cauchy(0,2), class = sigma),
                                 prior(lkj(2), class = cor)),
                       control=list(adapt_delta=0.999, max_treedepth=30),
                       iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes5.lag<-brm(data = LatAm2014, family = gaussian,
                       gini100 ~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_meanlag+wage.arrangement+year+(1+PresidentPartyLeftRightIndex_meanlag|country),
                       prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                 prior(normal(0, 2), class = b),
                                 prior(cauchy(0,2), class = sigma),
                                 prior(lkj(2), class = cor)),
                       control=list(adapt_delta=0.999, max_treedepth=30),
                       iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes6.lag<-brm(data = LatAm2014, family = gaussian,
                       gini100 ~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_meanlag+wage.arrangement+highlyeducatedlabor+logminwage+employment+year+(1+PresidentPartyLeftRightIndex_meanlag|country),
                       prior = c(prior(normal(51.24, 4), class = Intercept),
                                 prior(normal(0, 2), class = b),
                                 prior(cauchy(0,2), class = sigma),
                                 prior(lkj(2), class = cor)),
                       control=list(adapt_delta=0.999, max_treedepth=30),
                       iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes7.lag<-brm(data = LatAm2014, family = gaussian,
                       gini100 ~ DiverseFractional+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_meanlag+year+(1+PresidentPartyLeftRightIndex_meanlag|country),
                       prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                 prior(normal(0, 2), class = b),
                                 prior(cauchy(0,2), class = sigma),
                                 prior(lkj(2), class = cor)),
                       control=list(adapt_delta=0.999, max_treedepth=30),
                       iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes8.lag<-brm(data = LatAm2014, family = gaussian,
                       gini100 ~ DiverseFractional+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_meanlag+wage.arrangement+year+(1+PresidentPartyLeftRightIndex_meanlag|country),
                       prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                 prior(normal(0, 2), class = b),
                                 prior(cauchy(0,2), class = sigma),
                                 prior(lkj(2), class = cor)),
                       control=list(adapt_delta=0.999, max_treedepth=30),
                       iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes9.lag<-brm(data = LatAm2014, family = gaussian,
                       gini100 ~ DiverseFractional+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_meanlag+wage.arrangement+highlyeducatedlabor+logminwage+employment+year+(1+PresidentPartyLeftRightIndex_meanlag|country),
                       prior = c(prior(normal(51.24, 4), class = Intercept),
                                 prior(normal(0, 2), class = b),
                                 prior(cauchy(0,2), class = sigma),
                                 prior(lkj(2), class = cor)),
                       control=list(adapt_delta=0.999, max_treedepth=30),
                       iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes10.lag<-brm(data = LatAm2014, family = gaussian,
                        gini100 ~ EthnicFractLag1+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_meanlag+year+(1+PresidentPartyLeftRightIndex_meanlag|country),
                        prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                  prior(normal(0, 2), class = b),
                                  prior(cauchy(0,2), class = sigma),
                                  prior(lkj(2), class = cor)),
                        control=list(adapt_delta=0.999, max_treedepth=30),
                        iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes11.lag<-brm(data = LatAm2014, family = gaussian,
                        gini100 ~ EthnicFractLag1+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_meanlag+wage.arrangement+year+(1+PresidentPartyLeftRightIndex_meanlag|country),
                        prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                  prior(normal(0, 2), class = b),
                                  prior(cauchy(0,2), class = sigma),
                                  prior(lkj(2), class = cor)),
                        control=list(adapt_delta=0.999, max_treedepth=30),
                        iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes12.lag<-brm(data = LatAm2014, family = gaussian,
                        gini100 ~ EthnicFractLag1+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_meanlag+wage.arrangement+highlyeducatedlabor+logminwage+employment+year+(1+PresidentPartyLeftRightIndex_meanlag|country),
                        prior = c(prior(normal(51.24, 4), class = Intercept),
                                  prior(normal(0, 2), class = b),
                                  prior(cauchy(0,2), class = sigma),
                                  prior(lkj(2), class = cor)),
                        control=list(adapt_delta=0.999, max_treedepth=30),
                        iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes13.lag<-brm(data = LatAm2014, family = gaussian,
                        gini100 ~ EthnicFractionalizationTS+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_meanlag+year+(1+PresidentPartyLeftRightIndex_meanlag|country),
                        prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                  prior(normal(0, 2), class = b),
                                  prior(cauchy(0,2), class = sigma),
                                  prior(lkj(2), class = cor)),
                        control=list(adapt_delta=0.999, max_treedepth=30),
                        iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes14.lag<-brm(data = LatAm2014, family = gaussian,
                        gini100 ~ EthnicFractionalizationTS+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_meanlag+wage.arrangement+year+(1+PresidentPartyLeftRightIndex_meanlag|country),
                        prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                  prior(normal(0, 2), class = b),
                                  prior(cauchy(0,2), class = sigma),
                                  prior(lkj(2), class = cor)),
                        control=list(adapt_delta=0.999, max_treedepth=30),
                        iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

RandomSlopes15.lag<-brm(data = LatAm2014, family = gaussian,
                        gini100 ~ EthnicFractionalizationTS+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_meanlag+wage.arrangement+highlyeducatedlabor+logminwage+employment+year+(1+PresidentPartyLeftRightIndex_meanlag|country),
                        prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                  prior(normal(0, 2), class = b),
                                  prior(cauchy(0,2), class = sigma),
                                  prior(lkj(2), class = cor)),
                        control=list(adapt_delta=0.999, max_treedepth=30),
                        iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)



save.image("IterationsWithHighlyEducAlternateLeftsPresidentsOnly.RData")
library(shinystan)
launch_shinystan(PooledRace)
#Bayes-R2 function using modelled (approximate) residual variance
#from https://avehtari.github.io/bayes_R2/bayes_R2.html 
bayes_R2 <- function(fit) {
  mupred <- rstanarm::posterior_epred(fit)
  var_mupred <- apply(mupred, 1, var)
  if (family(fit)$family == "binomial" && NCOL(y) == 1) {
    sigma2 <- apply(mupred*(1-mupred), 1, mean)
  } else {
    sigma2 <- as.matrix(fit, pars = c("sigma"))^2
  }
  var_mupred / (var_mupred + sigma2)
}
#Bayes-R2 function using residuals
bayes_R2_res <- function(fit) {
  y <- rstanarm::get_y(fit)
  ypred <- rstanarm::posterior_epred(fit)
  if (family(fit)$family == "binomial" && NCOL(y) == 2) {
    trials <- rowSums(y)
    y <- y[, 1]
    ypred <- ypred %*% diag(trials)
  }
  e <- -1 * sweep(ypred, 2, y)
  var_ypred <- apply(ypred, 1, var)
  var_e <- apply(e, 1, var)
  var_ypred / (var_ypred + var_e)
}

summary(bayes_R2(PooledRace))
summary(bayes_R2_res(PooledRace))
summary(bayes_R2(PooledDiversity))
summary(bayes_R2_res(PooledDiversity))
summary(bayes_R2(PooledFractional2))
summary(bayes_R2_res(PooledFractional2))
summary(RandomSlopes13,robust = TRUE)

#RandomSlopesAllWage<-brm(data = LatAm2014, family = gaussian,
#                   gini100 ~ SimpsonDiversity+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment+year+(1+wage.arrangement|country),
#                   prior = c(prior(normal(51.24, 4), class = Intercept),
#                             prior(normal(0, 2), class = b),
#                             prior(cauchy(0,2), class = sigma),
#                             prior(lkj(2), class = cor)),
#                   control=list(adapt_delta=0.999, max_treedepth=30),
#                   iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)
#RandomSlopesAllDiversity<-brm(data = LatAm2014, family = gaussian,
#                   gini100 ~ SimpsonDiversity+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment+year+(1+SimpsonDiversity|country),
#                   prior = c(prior(normal(51.24, 4), class = Intercept),
#                             prior(normal(0, 2), class = b),
#                             prior(cauchy(0,2), class = sigma),
#                             prior(lkj(2), class = cor)),
#                   control=list(adapt_delta=0.999, max_treedepth=30),
#                   iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

library(effects)

#Subsets for each year
LatAm<-split(LatAm2014,LatAm2014$year)
countryyear<-c()
for (i in 1:22) {
  countryyear[i]<-brm(data = LatAm[[i]], family = gaussian,
                      gini100~SimpsonDiversity+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,
                      prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                prior(normal(0, 2), class = b),
                                prior(cauchy(0,2), class = sigma)),
                      control=list(adapt_delta=0.99, max_treedepth=20),
                      iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
  print(paste0("Datos para el anno  ", i+1992, " listos."))
}

library(broom)


broom.mixed::tidy(M1.all)
str(rstan::extract(M1.all))
posteriors<-c()
for (i in 1:22) {
  posteriors[i]<-posterior_samples(countryyear[i])
  print(paste0("Posteriores para el a?o  ", i+1992, " listos."))
}

post93$year <- rep(1993,times=length(post93))
post94$year <- rep(1994,times=length(post94))
post95$year <- rep(1995,times=length(post95))
post96$year <- rep(1996,times=length(post96))
post97$year <- rep(1997,times=length(post97))
post98$year <- rep(1998,times=length(post98))
post99$year <- rep(1999,times=length(post99))
post00$year <- rep(2000,times=length(post00))
post01$year <- rep(2001,times=length(post01))
post02$year <- rep(2002,times=length(post02))
post03$year <- rep(2003,times=length(post03))
post04$year <- rep(2004,times=length(post04))
post05$year <- rep(2005,times=length(post05))
post06$year <- rep(2006,times=length(post06))
post07$year <- rep(2007,times=length(post07))
post08$year <- rep(2008,times=length(post08))
post09$year <- rep(2009,times=length(post09))
post10$year <- rep(2010,times=length(post10))
post11$year <- rep(2011,times=length(post11))
post12$year <- rep(2012,times=length(post12))
post13$year <- rep(2013,times=length(post13))
post14$year <- rep(2014,times=length(post14))

post93$SimpsonDiversity_mean <- rep(mean(post93$b_SimpsonDiversity),times=30000)
post94$SimpsonDiversity_mean <- rep(mean(post94$b_SimpsonDiversity),times=30000)
post95$SimpsonDiversity_mean <- rep(mean(post95$b_SimpsonDiversity),times=30000)
post96$SimpsonDiversity_mean <- rep(mean(post96$b_SimpsonDiversity),times=30000)
post97$SimpsonDiversity_mean <- rep(mean(post97$b_SimpsonDiversity),times=30000)
post98$SimpsonDiversity_mean <- rep(mean(post98$b_SimpsonDiversity),times=30000)
post99$SimpsonDiversity_mean <- rep(mean(post99$b_SimpsonDiversity),times=30000)
post00$SimpsonDiversity_mean <- rep(mean(post00$b_SimpsonDiversity),times=30000)
post01$SimpsonDiversity_mean <- rep(mean(post01$b_SimpsonDiversity),times=30000)
post02$SimpsonDiversity_mean <- rep(mean(post02$b_SimpsonDiversity),times=30000)
post03$SimpsonDiversity_mean <- rep(mean(post03$b_SimpsonDiversity),times=30000)
post04$SimpsonDiversity_mean <- rep(mean(post04$b_SimpsonDiversity),times=30000)
post05$SimpsonDiversity_mean <- rep(mean(post05$b_SimpsonDiversity),times=30000)
post06$SimpsonDiversity_mean <- rep(mean(post06$b_SimpsonDiversity),times=30000)
post07$SimpsonDiversity_mean <- rep(mean(post07$b_SimpsonDiversity),times=30000)
post08$SimpsonDiversity_mean <- rep(mean(post08$b_SimpsonDiversity),times=30000)
post09$SimpsonDiversity_mean <- rep(mean(post09$b_SimpsonDiversity),times=30000)
post10$SimpsonDiversity_mean <- rep(mean(post10$b_SimpsonDiversity),times=30000)
post11$SimpsonDiversity_mean <- rep(mean(post11$b_SimpsonDiversity),times=30000)
post12$SimpsonDiversity_mean <- rep(mean(post12$b_SimpsonDiversity),times=30000)
post13$SimpsonDiversity_mean <- rep(mean(post13$b_SimpsonDiversity),times=30000)
post14$SimpsonDiversity_mean <- rep(mean(post14$b_SimpsonDiversity),times=30000)

post93$socexpgdp_mean <- rep(mean(post93$b_socexpgdp),times=30000)
post94$socexpgdp_mean <- rep(mean(post94$b_socexpgdp),times=30000)
post95$socexpgdp_mean <- rep(mean(post95$b_socexpgdp),times=30000)
post96$socexpgdp_mean <- rep(mean(post96$b_socexpgdp),times=30000)
post97$socexpgdp_mean <- rep(mean(post97$b_socexpgdp),times=30000)
post98$socexpgdp_mean <- rep(mean(post98$b_socexpgdp),times=30000)
post99$socexpgdp_mean <- rep(mean(post99$b_socexpgdp),times=30000)
post00$socexpgdp_mean <- rep(mean(post00$b_socexpgdp),times=30000)
post01$socexpgdp_mean <- rep(mean(post01$b_socexpgdp),times=30000)
post02$socexpgdp_mean <- rep(mean(post02$b_socexpgdp),times=30000)
post03$socexpgdp_mean <- rep(mean(post03$b_socexpgdp),times=30000)
post04$socexpgdp_mean <- rep(mean(post04$b_socexpgdp),times=30000)
post05$socexpgdp_mean <- rep(mean(post05$b_socexpgdp),times=30000)
post06$socexpgdp_mean <- rep(mean(post06$b_socexpgdp),times=30000)
post07$socexpgdp_mean <- rep(mean(post07$b_socexpgdp),times=30000)
post08$socexpgdp_mean <- rep(mean(post08$b_socexpgdp),times=30000)
post09$socexpgdp_mean <- rep(mean(post09$b_socexpgdp),times=30000)
post10$socexpgdp_mean <- rep(mean(post10$b_socexpgdp),times=30000)
post11$socexpgdp_mean <- rep(mean(post11$b_socexpgdp),times=30000)
post12$socexpgdp_mean <- rep(mean(post12$b_socexpgdp),times=30000)
post13$socexpgdp_mean <- rep(mean(post13$b_socexpgdp),times=30000)
post14$socexpgdp_mean <- rep(mean(post14$b_socexpgdp),times=30000)

post93$educexpgdp_mean <- rep(mean(post93$b_educexpgdp),times=30000)
post94$educexpgdp_mean <- rep(mean(post94$b_educexpgdp),times=30000)
post95$educexpgdp_mean <- rep(mean(post95$b_educexpgdp),times=30000)
post96$educexpgdp_mean <- rep(mean(post96$b_educexpgdp),times=30000)
post97$educexpgdp_mean <- rep(mean(post97$b_educexpgdp),times=30000)
post98$educexpgdp_mean <- rep(mean(post98$b_educexpgdp),times=30000)
post99$educexpgdp_mean <- rep(mean(post99$b_educexpgdp),times=30000)
post00$educexpgdp_mean <- rep(mean(post00$b_educexpgdp),times=30000)
post01$educexpgdp_mean <- rep(mean(post01$b_educexpgdp),times=30000)
post02$educexpgdp_mean <- rep(mean(post02$b_educexpgdp),times=30000)
post03$educexpgdp_mean <- rep(mean(post03$b_educexpgdp),times=30000)
post04$educexpgdp_mean <- rep(mean(post04$b_educexpgdp),times=30000)
post05$educexpgdp_mean <- rep(mean(post05$b_educexpgdp),times=30000)
post06$educexpgdp_mean <- rep(mean(post06$b_educexpgdp),times=30000)
post07$educexpgdp_mean <- rep(mean(post07$b_educexpgdp),times=30000)
post08$educexpgdp_mean <- rep(mean(post08$b_educexpgdp),times=30000)
post09$educexpgdp_mean <- rep(mean(post09$b_educexpgdp),times=30000)
post10$educexpgdp_mean <- rep(mean(post10$b_educexpgdp),times=30000)
post11$educexpgdp_mean <- rep(mean(post11$b_educexpgdp),times=30000)
post12$educexpgdp_mean <- rep(mean(post12$b_educexpgdp),times=30000)
post13$educexpgdp_mean <- rep(mean(post13$b_educexpgdp),times=30000)
post14$educexpgdp_mean <- rep(mean(post14$b_educexpgdp),times=30000)

post93$left_mean <- rep(mean(post93$b_left),times=30000)
post94$left_mean <- rep(mean(post94$b_left),times=30000)
post95$left_mean <- rep(mean(post95$b_left),times=30000)
post96$left_mean <- rep(mean(post96$b_left),times=30000)
post97$left_mean <- rep(mean(post97$b_left),times=30000)
post98$left_mean <- rep(mean(post98$b_left),times=30000)
post99$left_mean <- rep(mean(post99$b_left),times=30000)
post00$left_mean <- rep(mean(post00$b_left),times=30000)
post01$left_mean <- rep(mean(post01$b_left),times=30000)
post02$left_mean <- rep(mean(post02$b_left),times=30000)
post03$left_mean <- rep(mean(post03$b_left),times=30000)
post04$left_mean <- rep(mean(post04$b_left),times=30000)
post05$left_mean <- rep(mean(post05$b_left),times=30000)
post06$left_mean <- rep(mean(post06$b_left),times=30000)
post07$left_mean <- rep(mean(post07$b_left),times=30000)
post08$left_mean <- rep(mean(post08$b_left),times=30000)
post09$left_mean <- rep(mean(post09$b_left),times=30000)
post10$left_mean <- rep(mean(post10$b_left),times=30000)
post11$left_mean <- rep(mean(post11$b_left),times=30000)
post12$left_mean <- rep(mean(post12$b_left),times=30000)
post13$left_mean <- rep(mean(post13$b_left),times=30000)
post14$left_mean <- rep(mean(post14$b_left),times=30000)

post93$wage.arrangementregional_mean <- rep(mean(post93$b_wage.arrangementRegional),times=30000)
post94$wage.arrangementregional_mean <- rep(mean(post94$b_wage.arrangementRegional),times=30000)
post95$wage.arrangementregional_mean <- rep(mean(post95$b_wage.arrangementRegional),times=30000)
post96$wage.arrangementregional_mean <- rep(mean(post96$b_wage.arrangementRegional),times=30000)
post97$wage.arrangementregional_mean <- rep(mean(post97$b_wage.arrangementRegional),times=30000)
post98$wage.arrangementregional_mean <- rep(mean(post98$b_wage.arrangementRegional),times=30000)
post99$wage.arrangementregional_mean <- rep(mean(post99$b_wage.arrangementRegional),times=30000)
post00$wage.arrangementregional_mean <- rep(mean(post00$b_wage.arrangementRegional),times=30000)
post01$wage.arrangementregional_mean <- rep(mean(post01$b_wage.arrangementRegional),times=30000)
post02$wage.arrangementregional_mean <- rep(mean(post02$b_wage.arrangementRegional),times=30000)
post03$wage.arrangementregional_mean <- rep(mean(post03$b_wage.arrangementRegional),times=30000)
post04$wage.arrangementregional_mean <- rep(mean(post04$b_wage.arrangementRegional),times=30000)
post05$wage.arrangementregional_mean <- rep(mean(post05$b_wage.arrangementRegional),times=30000)
post06$wage.arrangementregional_mean <- rep(mean(post06$b_wage.arrangementRegional),times=30000)
post07$wage.arrangementregional_mean <- rep(mean(post07$b_wage.arrangementRegional),times=30000)
post08$wage.arrangementregional_mean <- rep(mean(post08$b_wage.arrangementRegional),times=30000)
post09$wage.arrangementregional_mean <- rep(mean(post09$b_wage.arrangementRegional),times=30000)
post10$wage.arrangementregional_mean <- rep(mean(post10$b_wage.arrangementRegional),times=30000)
post11$wage.arrangementregional_mean <- rep(mean(post11$b_wage.arrangementRegional),times=30000)
post12$wage.arrangementregional_mean <- rep(mean(post12$b_wage.arrangementRegional),times=30000)
post13$wage.arrangementregional_mean <- rep(mean(post13$b_wage.arrangementRegional),times=30000)
post14$wage.arrangementregional_mean <- rep(mean(post14$b_wage.arrangementRegional),times=30000)

post93$wage.arrangementsegmented_mean <- rep(mean(post93$b_wage.arrangementSegmented),times=30000)
post94$wage.arrangementsegmented_mean <- rep(mean(post94$b_wage.arrangementSegmented),times=30000)
post95$wage.arrangementsegmented_mean <- rep(mean(post95$b_wage.arrangementSegmented),times=30000)
post96$wage.arrangementsegmented_mean <- rep(mean(post96$b_wage.arrangementSegmented),times=30000)
post97$wage.arrangementsegmented_mean <- rep(mean(post97$b_wage.arrangementSegmented),times=30000)
post98$wage.arrangementsegmented_mean <- rep(mean(post98$b_wage.arrangementSegmented),times=30000)
post99$wage.arrangementsegmented_mean <- rep(mean(post99$b_wage.arrangementSegmented),times=30000)
post00$wage.arrangementsegmented_mean <- rep(mean(post00$b_wage.arrangementSegmented),times=30000)
post01$wage.arrangementsegmented_mean <- rep(mean(post01$b_wage.arrangementSegmented),times=30000)
post02$wage.arrangementsegmented_mean <- rep(mean(post02$b_wage.arrangementSegmented),times=30000)
post03$wage.arrangementsegmented_mean <- rep(mean(post03$b_wage.arrangementSegmented),times=30000)
post04$wage.arrangementsegmented_mean <- rep(mean(post04$b_wage.arrangementSegmented),times=30000)
post05$wage.arrangementsegmented_mean <- rep(mean(post05$b_wage.arrangementSegmented),times=30000)
post06$wage.arrangementsegmented_mean <- rep(mean(post06$b_wage.arrangementSegmented),times=30000)
post07$wage.arrangementsegmented_mean <- rep(mean(post07$b_wage.arrangementSegmented),times=30000)
post08$wage.arrangementsegmented_mean <- rep(mean(post08$b_wage.arrangementSegmented),times=30000)
post09$wage.arrangementsegmented_mean <- rep(mean(post09$b_wage.arrangementSegmented),times=30000)
post10$wage.arrangementsegmented_mean <- rep(mean(post10$b_wage.arrangementSegmented),times=30000)
post11$wage.arrangementsegmented_mean <- rep(mean(post11$b_wage.arrangementSegmented),times=30000)
post12$wage.arrangementsegmented_mean <- rep(mean(post12$b_wage.arrangementSegmented),times=30000)
post13$wage.arrangementsegmented_mean <- rep(mean(post13$b_wage.arrangementSegmented),times=30000)
post14$wage.arrangementsegmented_mean <- rep(mean(post14$b_wage.arrangementSegmented),times=30000)

post93$highlyeducatedlabor_mean <- rep(mean(post93$b_highlyeducatedlabor),times=30000)
post94$highlyeducatedlabor_mean <- rep(mean(post94$b_highlyeducatedlabor),times=30000)
post95$highlyeducatedlabor_mean <- rep(mean(post95$b_highlyeducatedlabor),times=30000)
post96$highlyeducatedlabor_mean <- rep(mean(post96$b_highlyeducatedlabor),times=30000)
post97$highlyeducatedlabor_mean <- rep(mean(post97$b_highlyeducatedlabor),times=30000)
post98$highlyeducatedlabor_mean <- rep(mean(post98$b_highlyeducatedlabor),times=30000)
post99$highlyeducatedlabor_mean <- rep(mean(post99$b_highlyeducatedlabor),times=30000)
post00$highlyeducatedlabor_mean <- rep(mean(post00$b_highlyeducatedlabor),times=30000)
post01$highlyeducatedlabor_mean <- rep(mean(post01$b_highlyeducatedlabor),times=30000)
post02$highlyeducatedlabor_mean <- rep(mean(post02$b_highlyeducatedlabor),times=30000)
post03$highlyeducatedlabor_mean <- rep(mean(post03$b_highlyeducatedlabor),times=30000)
post04$highlyeducatedlabor_mean <- rep(mean(post04$b_highlyeducatedlabor),times=30000)
post05$highlyeducatedlabor_mean <- rep(mean(post05$b_highlyeducatedlabor),times=30000)
post06$highlyeducatedlabor_mean <- rep(mean(post06$b_highlyeducatedlabor),times=30000)
post07$highlyeducatedlabor_mean <- rep(mean(post07$b_highlyeducatedlabor),times=30000)
post08$highlyeducatedlabor_mean <- rep(mean(post08$b_highlyeducatedlabor),times=30000)
post09$highlyeducatedlabor_mean <- rep(mean(post09$b_highlyeducatedlabor),times=30000)
post10$highlyeducatedlabor_mean <- rep(mean(post10$b_highlyeducatedlabor),times=30000)
post11$highlyeducatedlabor_mean <- rep(mean(post11$b_highlyeducatedlabor),times=30000)
post12$highlyeducatedlabor_mean <- rep(mean(post12$b_highlyeducatedlabor),times=30000)
post13$highlyeducatedlabor_mean <- rep(mean(post13$b_highlyeducatedlabor),times=30000)
post14$highlyeducatedlabor_mean <- rep(mean(post14$b_highlyeducatedlabor),times=30000)

post93$logminwage_mean <- rep(mean(post93$b_logminwage),times=30000)
post94$logminwage_mean <- rep(mean(post94$b_logminwage),times=30000)
post95$logminwage_mean <- rep(mean(post95$b_logminwage),times=30000)
post96$logminwage_mean <- rep(mean(post96$b_logminwage),times=30000)
post97$logminwage_mean <- rep(mean(post97$b_logminwage),times=30000)
post98$logminwage_mean <- rep(mean(post98$b_logminwage),times=30000)
post99$logminwage_mean <- rep(mean(post99$b_logminwage),times=30000)
post00$logminwage_mean <- rep(mean(post00$b_logminwage),times=30000)
post01$logminwage_mean <- rep(mean(post01$b_logminwage),times=30000)
post02$logminwage_mean <- rep(mean(post02$b_logminwage),times=30000)
post03$logminwage_mean <- rep(mean(post03$b_logminwage),times=30000)
post04$logminwage_mean <- rep(mean(post04$b_logminwage),times=30000)
post05$logminwage_mean <- rep(mean(post05$b_logminwage),times=30000)
post06$logminwage_mean <- rep(mean(post06$b_logminwage),times=30000)
post07$logminwage_mean <- rep(mean(post07$b_logminwage),times=30000)
post08$logminwage_mean <- rep(mean(post08$b_logminwage),times=30000)
post09$logminwage_mean <- rep(mean(post09$b_logminwage),times=30000)
post10$logminwage_mean <- rep(mean(post10$b_logminwage),times=30000)
post11$logminwage_mean <- rep(mean(post11$b_logminwage),times=30000)
post12$logminwage_mean <- rep(mean(post12$b_logminwage),times=30000)
post13$logminwage_mean <- rep(mean(post13$b_logminwage),times=30000)
post14$logminwage_mean <- rep(mean(post14$b_logminwage),times=30000)

post93$employment_mean <- rep(mean(post93$b_employment),times=30000)
post94$employment_mean <- rep(mean(post94$b_employment),times=30000)
post95$employment_mean <- rep(mean(post95$b_employment),times=30000)
post96$employment_mean <- rep(mean(post96$b_employment),times=30000)
post97$employment_mean <- rep(mean(post97$b_employment),times=30000)
post98$employment_mean <- rep(mean(post98$b_employment),times=30000)
post99$employment_mean <- rep(mean(post99$b_employment),times=30000)
post00$employment_mean <- rep(mean(post00$b_employment),times=30000)
post01$employment_mean <- rep(mean(post01$b_employment),times=30000)
post02$employment_mean <- rep(mean(post02$b_employment),times=30000)
post03$employment_mean <- rep(mean(post03$b_employment),times=30000)
post04$employment_mean <- rep(mean(post04$b_employment),times=30000)
post05$employment_mean <- rep(mean(post05$b_employment),times=30000)
post06$employment_mean <- rep(mean(post06$b_employment),times=30000)
post07$employment_mean <- rep(mean(post07$b_employment),times=30000)
post08$employment_mean <- rep(mean(post08$b_employment),times=30000)
post09$employment_mean <- rep(mean(post09$b_employment),times=30000)
post10$employment_mean <- rep(mean(post10$b_employment),times=30000)
post11$employment_mean <- rep(mean(post11$b_employment),times=30000)
post12$employment_mean <- rep(mean(post12$b_employment),times=30000)
post13$employment_mean <- rep(mean(post13$b_employment),times=30000)
post14$employment_mean <- rep(mean(post14$b_employment),times=30000)


colnames(fits)
library(plyr)
fits = rbind.fill(post93, post94, post95, post96,post97,post98,post99,post00,
                  post01,post02,post03,post04,post05,post06,post07,post08,post09,
                  post10,post11,post12,post13,post14)
save.image("/geode2/home/u070/rmarcano/Carbonate/Documents/Latin America paper/IterationsWithHighlyEducAlternateLefts.RData")

library(ggdist)
Heterogeneity<-ggplot(fits, aes(y = (as.year), x = SimpsonDiversity_mean)) +
  stat_halfeye(aes(x=b_SimpsonDiversity),
               .width = c(.90, .5),
               slab_fill= "deepskyblue1",
               point_color = "gold1",
               point_fill = "darkslategray",
               interval_color = "gray0") +  
  labs(x="Effect on Inequality from Racial Heterogeneity")+
  geom_vline(xintercept = 0, linetype = "dashed") 
Heterogeneity<-Heterogeneity+ coord_flip()+geom_hline(yintercept = 0,colour="red")+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
Heterogeneity

SocExpGDP<-ggplot(fits, aes(y = (as.year), x = socexpgdp_mean)) +
  stat_halfeye(aes(x=b_socexpgdp),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from Social Expenditure as a share of GDP")+
  geom_vline(xintercept = 0, linetype = "dashed") 
SocExpGDP<-SocExpGDP+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
EmployIneq
SocExpGDP

EduExpGDP<-ggplot(fits, aes(y = (as.year), x = educexpgdp_mean)) +
  stat_halfeye(aes(x=b_educexpgdp),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from Educational Expenditure as a share of GDP")+
  geom_vline(xintercept = 0, linetype = "dashed") 
EduExpGDP<-EduExpGDP+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
EmployIneq
EduExpGDP

LeftIneq<-ggplot(fits, aes(y = (as.year), x = educexpgdp_mean)) +
  stat_halfeye(aes(x=b_educexpgdp),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from Left-of-Center governments")+
  geom_vline(xintercept = 0, linetype = "dashed") 
LeftIneq<-LeftIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
EmployIneq
LeftIneq

TertiaryIneq<-ggplot(fits, aes(y = (as.year), x = highlyeducatedlabor_mean)) +
  stat_halfeye(aes(x=b_highlyeducatedlabor),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from Educational Expenditure as a share of GDP") +
  geom_vline(xintercept = 0, linetype = "dashed") 
TertiaryIneq<-TertiaryIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
EmployIneq
TertiaryIneq

WageRegionallIneq<-ggplot(fits, aes(y = (as.year), x = wage.arrangementregional_mean)) +
  stat_halfeye(aes(x=b_wage.arrangementRegional),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from regional wage arrangements (vs. national)")+
  geom_vline(xintercept = 0, linetype = "dashed") 
WageRegionallIneq<-WageRegionallIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
EmployIneq
WageRegionallIneq

WageSegmlIneq<-ggplot(fits, aes(y = (as.year), x = wage.arrangementsegmented_mean)) +
  stat_halfeye(aes(x=b_wage.arrangementSegmented),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from segmented wage arrangements (vs. national)")+
  geom_vline(xintercept = 0, linetype = "dashed") 
WageSegmlIneq<-WageSegmlIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
EmployIneq
WageSegmlIneq

LogMinWageIneq<-ggplot(fits, aes(y = (as.year), x = logminwage_mean)) +
  stat_halfeye(aes(x=b_logminwage),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from log(minimum wage)")+
  geom_vline(xintercept = 0, linetype = "dashed") 
LogMinWageIneq<-LogMinWageIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
EmployIneq
LogMinWageIneq

EmployIneq<-ggplot(fits, aes(y = (as.year), x = employment_mean)) +
  stat_halfeye(aes(x=b_employment),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from Employment rate")+
  geom_vline(xintercept = 0, linetype = "dashed") 
EmployIneq<-EmployIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
EmployIneq
library(gridExtra)
grid.arrange(Heterogeneity,SocExpGDP,EduExpGDP,LeftIneq,TertiaryIneq,
             WageRegionallIneq,WageSegmlIneq,LogMinWageIneq,EmployIneq,ncol=3)


#Alternative, race based instead of heterogeneity based


M1P.93<-brm(data = yr1993, family = gaussian,
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                      prior(normal(0, 2), class = b),
                      prior(cauchy(0,2), class = sigma)),
            control=list(adapt_delta=0.99, max_treedepth=20),
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1P.94<-brm(data = yr1994, family = gaussian,
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                      prior(normal(0, 2), class = b),
                      prior(cauchy(0,2), class = sigma)),
            control=list(adapt_delta=0.99, max_treedepth=20),
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1P.95<-brm(data = yr1995, family = gaussian,
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                      prior(normal(0, 2), class = b),                    
                      prior(cauchy(0,2), class = sigma)),          
            control=list(adapt_delta=0.99, max_treedepth=20),          
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1P.96<-brm(data = yr1996, family = gaussian,          
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                      prior(normal(0, 2), class = b),                    
                      prior(cauchy(0,2), class = sigma)),          
            control=list(adapt_delta=0.99, max_treedepth=20),          
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)

M1P.97<-brm(data = yr1997, family = gaussian,          
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                      prior(normal(0, 2), class = b),                    
                      prior(cauchy(0,2), class = sigma)),          
            control=list(adapt_delta=0.99, max_treedepth=20),          
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1P.98<-brm(data = yr1998, family = gaussian,          
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                      prior(normal(0, 2), class = b),                    
                      prior(cauchy(0,2), class = sigma)),          
            control=list(adapt_delta=0.99, max_treedepth=20),          
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1P.99<-brm(data = yr1999, family = gaussian,          
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                      prior(normal(0, 2), class = b),                    
                      prior(cauchy(0,2), class = sigma)),          
            control=list(adapt_delta=0.99, max_treedepth=20),          
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1P.00<-brm(data = yr2000, family = gaussian,          
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                      prior(normal(0, 2), class = b),                    
                      prior(cauchy(0,2), class = sigma)),          
            control=list(adapt_delta=0.99, max_treedepth=20),          
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1P.01<-brm(data = yr2001, family = gaussian,          
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                      prior(normal(0, 2), class = b),                    
                      prior(cauchy(0,2), class = sigma)),          
            control=list(adapt_delta=0.99, max_treedepth=20),          
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1P.02<-brm(data = yr2002, family = gaussian,          
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                      prior(normal(0, 2), class = b),                    
                      prior(cauchy(0,2), class = sigma)),          
            control=list(adapt_delta=0.99, max_treedepth=20),          
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1P.03<-brm(data = yr2003, family = gaussian,          
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                      prior(normal(0, 2), class = b),                    
                      prior(cauchy(0,2), class = sigma)),          
            control=list(adapt_delta=0.99, max_treedepth=20),          
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1P.04<-brm(data = yr2004, family = gaussian,          
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                      prior(normal(0, 2), class = b),                    
                      prior(cauchy(0,2), class = sigma)),          
            control=list(adapt_delta=0.99, max_treedepth=20),          
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1P.05<-brm(data = yr2005, family = gaussian,          
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                      prior(normal(0, 2), class = b),                    
                      prior(cauchy(0,2), class = sigma)),          
            control=list(adapt_delta=0.99, max_treedepth=20),          
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1P.06<-brm(data = yr2006, family = gaussian,          
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                      prior(normal(0, 2), class = b),                    
                      prior(cauchy(0,2), class = sigma)),          
            control=list(adapt_delta=0.99, max_treedepth=20),          
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1P.07<-brm(data = yr2007, family = gaussian,          
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                      prior(normal(0, 2), class = b),                    
                      prior(cauchy(0,2), class = sigma)),          
            control=list(adapt_delta=0.99, max_treedepth=20),          
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1P.08<-brm(data = yr2008, family = gaussian,          
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                      prior(normal(0, 2), class = b),                    
                      prior(cauchy(0,2), class = sigma)),          
            control=list(adapt_delta=0.99, max_treedepth=20),          
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1P.09<-brm(data = yr2009, family = gaussian,          
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                      prior(normal(0, 2), class = b),                    
                      prior(cauchy(0,2), class = sigma)),          
            control=list(adapt_delta=0.99, max_treedepth=20),          
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1P.10<-brm(data = yr2010, family = gaussian,          
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                      prior(normal(0, 2), class = b),                    
                      prior(cauchy(0,2), class = sigma)),          
            control=list(adapt_delta=0.99, max_treedepth=20),          
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1P.11<-brm(data = yr2011, family = gaussian,          
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                      prior(normal(0, 2), class = b),                    
                      prior(cauchy(0,2), class = sigma)),          
            control=list(adapt_delta=0.99, max_treedepth=20),          
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1P.12<-brm(data = yr2012, family = gaussian,          
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                      prior(normal(0, 2), class = b),                    
                      prior(cauchy(0,2), class = sigma)),          
            control=list(adapt_delta=0.99, max_treedepth=20),          
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1P.13<-brm(data = yr2013, family = gaussian,          
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                      prior(normal(0, 2), class = b),                    
                      prior(cauchy(0,2), class = sigma)),          
            control=list(adapt_delta=0.99, max_treedepth=20),          
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1P.14<-brm(data = yr2014, family = gaussian,          
            gini100~ Blacks+Mulattoes+Mestizo+Amerindians+Asians+CreolesEtGafurinas+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
            prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                      prior(normal(0, 2), class = b),                    
                      prior(cauchy(0,2), class = sigma)),          
            control=list(adapt_delta=0.99, max_treedepth=20),          
            iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
?posterior_samples
postp93 <- posterior_samples(M1P.93)
postp94 <- posterior_samples(M1P.94)
postp95 <- posterior_samples(M1P.95)
postp96 <- posterior_samples(M1P.96)
postp97 <- posterior_samples(M1P.97)
postp98 <- posterior_samples(M1P.98)
postp99 <- posterior_samples(M1P.99)
postp00 <- posterior_samples(M1P.00)
postp01 <- posterior_samples(M1P.01)
postp02 <- posterior_samples(M1P.02)
postp03 <- posterior_samples(M1P.03)
postp04 <- posterior_samples(M1P.04)
postp05 <- posterior_samples(M1P.05)
postp06 <- posterior_samples(M1P.06)
postp07 <- posterior_samples(M1P.07)
postp08 <- posterior_samples(M1P.08)
postp09 <- posterior_samples(M1P.09)
postp10 <- posterior_samples(M1P.10)
postp11 <- posterior_samples(M1P.11)
postp12 <- posterior_samples(M1P.12)
postp13 <- posterior_samples(M1P.13)
postp14 <- posterior_samples(M1P.14)

postp93$year <- rep(1993,times=30000)
postp94$year <- rep(1994,times=30000)
postp95$year <- rep(1995,times=30000)
postp96$year <- rep(1996,times=30000)
postp97$year <- rep(1997,times=30000)
postp98$year <- rep(1998,times=30000)
postp99$year <- rep(1999,times=30000)
postp00$year <- rep(2000,times=30000)
postp01$year <- rep(2001,times=30000)
postp02$year <- rep(2002,times=30000)
postp03$year <- rep(2003,times=30000)
postp04$year <- rep(2004,times=30000)
postp05$year <- rep(2005,times=30000)
postp06$year <- rep(2006,times=30000)
postp07$year <- rep(2007,times=30000)
postp08$year <- rep(2008,times=30000)
postp09$year <- rep(2009,times=30000)
postp10$year <- rep(2010,times=30000)
postp11$year <- rep(2011,times=30000)
postp12$year <- rep(2012,times=30000)
postp13$year <- rep(2013,times=30000)
postp14$year <- rep(2014,times=30000)

postp93$Blacks_mean <- rep(mean(postp93$b_Blacks),times=30000)
postp94$Blacks_mean <- rep(mean(postp94$b_Blacks),times=30000)
postp95$Blacks_mean <- rep(mean(postp95$b_Blacks),times=30000)
postp96$Blacks_mean <- rep(mean(postp96$b_Blacks),times=30000)
postp97$Blacks_mean <- rep(mean(postp97$b_Blacks),times=30000)
postp98$Blacks_mean <- rep(mean(postp98$b_Blacks),times=30000)
postp99$Blacks_mean <- rep(mean(postp99$b_Blacks),times=30000)
postp00$Blacks_mean <- rep(mean(postp00$b_Blacks),times=30000)
postp01$Blacks_mean <- rep(mean(postp01$b_Blacks),times=30000)
postp02$Blacks_mean <- rep(mean(postp02$b_Blacks),times=30000)
postp03$Blacks_mean <- rep(mean(postp03$b_Blacks),times=30000)
postp04$Blacks_mean <- rep(mean(postp04$b_Blacks),times=30000)
postp05$Blacks_mean <- rep(mean(postp05$b_Blacks),times=30000)
postp06$Blacks_mean <- rep(mean(postp06$b_Blacks),times=30000)
postp07$Blacks_mean <- rep(mean(postp07$b_Blacks),times=30000)
postp08$Blacks_mean <- rep(mean(postp08$b_Blacks),times=30000)
postp09$Blacks_mean <- rep(mean(postp09$b_Blacks),times=30000)
postp10$Blacks_mean <- rep(mean(postp10$b_Blacks),times=30000)
postp11$Blacks_mean <- rep(mean(postp11$b_Blacks),times=30000)
postp12$Blacks_mean <- rep(mean(postp12$b_Blacks),times=30000)
postp13$Blacks_mean <- rep(mean(postp13$b_Blacks),times=30000)
postp14$Blacks_mean <- rep(mean(postp14$b_Blacks),times=30000)

postp93$Mulattoes_mean <- rep(mean(postp93$b_Mulattoes),times=30000)
postp94$Mulattoes_mean <- rep(mean(postp94$b_Mulattoes),times=30000)
postp95$Mulattoes_mean <- rep(mean(postp95$b_Mulattoes),times=30000)
postp96$Mulattoes_mean <- rep(mean(postp96$b_Mulattoes),times=30000)
postp97$Mulattoes_mean <- rep(mean(postp97$b_Mulattoes),times=30000)
postp98$Mulattoes_mean <- rep(mean(postp98$b_Mulattoes),times=30000)
postp99$Mulattoes_mean <- rep(mean(postp99$b_Mulattoes),times=30000)
postp00$Mulattoes_mean <- rep(mean(postp00$b_Mulattoes),times=30000)
postp01$Mulattoes_mean <- rep(mean(postp01$b_Mulattoes),times=30000)
postp02$Mulattoes_mean <- rep(mean(postp02$b_Mulattoes),times=30000)
postp03$Mulattoes_mean <- rep(mean(postp03$b_Mulattoes),times=30000)
postp04$Mulattoes_mean <- rep(mean(postp04$b_Mulattoes),times=30000)
postp05$Mulattoes_mean <- rep(mean(postp05$b_Mulattoes),times=30000)
postp06$Mulattoes_mean <- rep(mean(postp06$b_Mulattoes),times=30000)
postp07$Mulattoes_mean <- rep(mean(postp07$b_Mulattoes),times=30000)
postp08$Mulattoes_mean <- rep(mean(postp08$b_Mulattoes),times=30000)
postp09$Mulattoes_mean <- rep(mean(postp09$b_Mulattoes),times=30000)
postp10$Mulattoes_mean <- rep(mean(postp10$b_Mulattoes),times=30000)
postp11$Mulattoes_mean <- rep(mean(postp11$b_Mulattoes),times=30000)
postp12$Mulattoes_mean <- rep(mean(postp12$b_Mulattoes),times=30000)
postp13$Mulattoes_mean <- rep(mean(postp13$b_Mulattoes),times=30000)
postp14$Mulattoes_mean <- rep(mean(postp14$b_Mulattoes),times=30000)

postp93$Mestizo_mean <- rep(mean(postp93$b_Mestizo),times=30000)
postp94$Mestizo_mean <- rep(mean(postp94$b_Mestizo),times=30000)
postp95$Mestizo_mean <- rep(mean(postp95$b_Mestizo),times=30000)
postp96$Mestizo_mean <- rep(mean(postp96$b_Mestizo),times=30000)
postp97$Mestizo_mean <- rep(mean(postp97$b_Mestizo),times=30000)
postp98$Mestizo_mean <- rep(mean(postp98$b_Mestizo),times=30000)
postp99$Mestizo_mean <- rep(mean(postp99$b_Mestizo),times=30000)
postp00$Mestizo_mean <- rep(mean(postp00$b_Mestizo),times=30000)
postp01$Mestizo_mean <- rep(mean(postp01$b_Mestizo),times=30000)
postp02$Mestizo_mean <- rep(mean(postp02$b_Mestizo),times=30000)
postp03$Mestizo_mean <- rep(mean(postp03$b_Mestizo),times=30000)
postp04$Mestizo_mean <- rep(mean(postp04$b_Mestizo),times=30000)
postp05$Mestizo_mean <- rep(mean(postp05$b_Mestizo),times=30000)
postp06$Mestizo_mean <- rep(mean(postp06$b_Mestizo),times=30000)
postp07$Mestizo_mean <- rep(mean(postp07$b_Mestizo),times=30000)
postp08$Mestizo_mean <- rep(mean(postp08$b_Mestizo),times=30000)
postp09$Mestizo_mean <- rep(mean(postp09$b_Mestizo),times=30000)
postp10$Mestizo_mean <- rep(mean(postp10$b_Mestizo),times=30000)
postp11$Mestizo_mean <- rep(mean(postp11$b_Mestizo),times=30000)
postp12$Mestizo_mean <- rep(mean(postp12$b_Mestizo),times=30000)
postp13$Mestizo_mean <- rep(mean(postp13$b_Mestizo),times=30000)
postp14$Mestizo_mean <- rep(mean(postp14$b_Mestizo),times=30000)

postp93$Amerindians_mean <- rep(mean(postp93$b_Amerindians),times=30000)
postp94$Amerindians_mean <- rep(mean(postp94$b_Amerindians),times=30000)
postp95$Amerindians_mean <- rep(mean(postp95$b_Amerindians),times=30000)
postp96$Amerindians_mean <- rep(mean(postp96$b_Amerindians),times=30000)
postp97$Amerindians_mean <- rep(mean(postp97$b_Amerindians),times=30000)
postp98$Amerindians_mean <- rep(mean(postp98$b_Amerindians),times=30000)
postp99$Amerindians_mean <- rep(mean(postp99$b_Amerindians),times=30000)
postp00$Amerindians_mean <- rep(mean(postp00$b_Amerindians),times=30000)
postp01$Amerindians_mean <- rep(mean(postp01$b_Amerindians),times=30000)
postp02$Amerindians_mean <- rep(mean(postp02$b_Amerindians),times=30000)
postp03$Amerindians_mean <- rep(mean(postp03$b_Amerindians),times=30000)
postp04$Amerindians_mean <- rep(mean(postp04$b_Amerindians),times=30000)
postp05$Amerindians_mean <- rep(mean(postp05$b_Amerindians),times=30000)
postp06$Amerindians_mean <- rep(mean(postp06$b_Amerindians),times=30000)
postp07$Amerindians_mean <- rep(mean(postp07$b_Amerindians),times=30000)
postp08$Amerindians_mean <- rep(mean(postp08$b_Amerindians),times=30000)
postp09$Amerindians_mean <- rep(mean(postp09$b_Amerindians),times=30000)
postp10$Amerindians_mean <- rep(mean(postp10$b_Amerindians),times=30000)
postp11$Amerindians_mean <- rep(mean(postp11$b_Amerindians),times=30000)
postp12$Amerindians_mean <- rep(mean(postp12$b_Amerindians),times=30000)
postp13$Amerindians_mean <- rep(mean(postp13$b_Amerindians),times=30000)
postp14$Amerindians_mean <- rep(mean(postp14$b_Amerindians),times=30000)

postp93$Asians_mean <- rep(mean(postp93$b_Asians),times=30000)
postp94$Asians_mean <- rep(mean(postp94$b_Asians),times=30000)
postp95$Asians_mean <- rep(mean(postp95$b_Asians),times=30000)
postp96$Asians_mean <- rep(mean(postp96$b_Asians),times=30000)
postp97$Asians_mean <- rep(mean(postp97$b_Asians),times=30000)
postp98$Asians_mean <- rep(mean(postp98$b_Asians),times=30000)
postp99$Asians_mean <- rep(mean(postp99$b_Asians),times=30000)
postp00$Asians_mean <- rep(mean(postp00$b_Asians),times=30000)
postp01$Asians_mean <- rep(mean(postp01$b_Asians),times=30000)
postp02$Asians_mean <- rep(mean(postp02$b_Asians),times=30000)
postp03$Asians_mean <- rep(mean(postp03$b_Asians),times=30000)
postp04$Asians_mean <- rep(mean(postp04$b_Asians),times=30000)
postp05$Asians_mean <- rep(mean(postp05$b_Asians),times=30000)
postp06$Asians_mean <- rep(mean(postp06$b_Asians),times=30000)
postp07$Asians_mean <- rep(mean(postp07$b_Asians),times=30000)
postp08$Asians_mean <- rep(mean(postp08$b_Asians),times=30000)
postp09$Asians_mean <- rep(mean(postp09$b_Asians),times=30000)
postp10$Asians_mean <- rep(mean(postp10$b_Asians),times=30000)
postp11$Asians_mean <- rep(mean(postp11$b_Asians),times=30000)
postp12$Asians_mean <- rep(mean(postp12$b_Asians),times=30000)
postp13$Asians_mean <- rep(mean(postp13$b_Asians),times=30000)
postp14$Asians_mean <- rep(mean(postp14$b_Asians),times=30000)

postp93$CreolesEtGafurinas_mean <- rep(mean(postp93$b_CreolesEtGafurinas),times=30000)
postp94$CreolesEtGafurinas_mean <- rep(mean(postp94$b_CreolesEtGafurinas),times=30000)
postp95$CreolesEtGafurinas_mean <- rep(mean(postp95$b_CreolesEtGafurinas),times=30000)
postp96$CreolesEtGafurinas_mean <- rep(mean(postp96$b_CreolesEtGafurinas),times=30000)
postp97$CreolesEtGafurinas_mean <- rep(mean(postp97$b_CreolesEtGafurinas),times=30000)
postp98$CreolesEtGafurinas_mean <- rep(mean(postp98$b_CreolesEtGafurinas),times=30000)
postp99$CreolesEtGafurinas_mean <- rep(mean(postp99$b_CreolesEtGafurinas),times=30000)
postp00$CreolesEtGafurinas_mean <- rep(mean(postp00$b_CreolesEtGafurinas),times=30000)
postp01$CreolesEtGafurinas_mean <- rep(mean(postp01$b_CreolesEtGafurinas),times=30000)
postp02$CreolesEtGafurinas_mean <- rep(mean(postp02$b_CreolesEtGafurinas),times=30000)
postp03$CreolesEtGafurinas_mean <- rep(mean(postp03$b_CreolesEtGafurinas),times=30000)
postp04$CreolesEtGafurinas_mean <- rep(mean(postp04$b_CreolesEtGafurinas),times=30000)
postp05$CreolesEtGafurinas_mean <- rep(mean(postp05$b_CreolesEtGafurinas),times=30000)
postp06$CreolesEtGafurinas_mean <- rep(mean(postp06$b_CreolesEtGafurinas),times=30000)
postp07$CreolesEtGafurinas_mean <- rep(mean(postp07$b_CreolesEtGafurinas),times=30000)
postp08$CreolesEtGafurinas_mean <- rep(mean(postp08$b_CreolesEtGafurinas),times=30000)
postp09$CreolesEtGafurinas_mean <- rep(mean(postp09$b_CreolesEtGafurinas),times=30000)
postp10$CreolesEtGafurinas_mean <- rep(mean(postp10$b_CreolesEtGafurinas),times=30000)
postp11$CreolesEtGafurinas_mean <- rep(mean(postp11$b_CreolesEtGafurinas),times=30000)
postp12$CreolesEtGafurinas_mean <- rep(mean(postp12$b_CreolesEtGafurinas),times=30000)
postp13$CreolesEtGafurinas_mean <- rep(mean(postp13$b_CreolesEtGafurinas),times=30000)
postp14$CreolesEtGafurinas_mean <- rep(mean(postp14$b_CreolesEtGafurinas),times=30000)


postp93$socexpgdp_mean <- rep(mean(postp93$b_socexpgdp),times=30000)
postp94$socexpgdp_mean <- rep(mean(postp94$b_socexpgdp),times=30000)
postp95$socexpgdp_mean <- rep(mean(postp95$b_socexpgdp),times=30000)
postp96$socexpgdp_mean <- rep(mean(postp96$b_socexpgdp),times=30000)
postp97$socexpgdp_mean <- rep(mean(postp97$b_socexpgdp),times=30000)
postp98$socexpgdp_mean <- rep(mean(postp98$b_socexpgdp),times=30000)
postp99$socexpgdp_mean <- rep(mean(postp99$b_socexpgdp),times=30000)
postp00$socexpgdp_mean <- rep(mean(postp00$b_socexpgdp),times=30000)
postp01$socexpgdp_mean <- rep(mean(postp01$b_socexpgdp),times=30000)
postp02$socexpgdp_mean <- rep(mean(postp02$b_socexpgdp),times=30000)
postp03$socexpgdp_mean <- rep(mean(postp03$b_socexpgdp),times=30000)
postp04$socexpgdp_mean <- rep(mean(postp04$b_socexpgdp),times=30000)
postp05$socexpgdp_mean <- rep(mean(postp05$b_socexpgdp),times=30000)
postp06$socexpgdp_mean <- rep(mean(postp06$b_socexpgdp),times=30000)
postp07$socexpgdp_mean <- rep(mean(postp07$b_socexpgdp),times=30000)
postp08$socexpgdp_mean <- rep(mean(postp08$b_socexpgdp),times=30000)
postp09$socexpgdp_mean <- rep(mean(postp09$b_socexpgdp),times=30000)
postp10$socexpgdp_mean <- rep(mean(postp10$b_socexpgdp),times=30000)
postp11$socexpgdp_mean <- rep(mean(postp11$b_socexpgdp),times=30000)
postp12$socexpgdp_mean <- rep(mean(postp12$b_socexpgdp),times=30000)
postp13$socexpgdp_mean <- rep(mean(postp13$b_socexpgdp),times=30000)
postp14$socexpgdp_mean <- rep(mean(postp14$b_socexpgdp),times=30000)

postp93$educexpgdp_mean <- rep(mean(postp93$b_educexpgdp),times=30000)
postp94$educexpgdp_mean <- rep(mean(postp94$b_educexpgdp),times=30000)
postp95$educexpgdp_mean <- rep(mean(postp95$b_educexpgdp),times=30000)
postp96$educexpgdp_mean <- rep(mean(postp96$b_educexpgdp),times=30000)
postp97$educexpgdp_mean <- rep(mean(postp97$b_educexpgdp),times=30000)
postp98$educexpgdp_mean <- rep(mean(postp98$b_educexpgdp),times=30000)
postp99$educexpgdp_mean <- rep(mean(postp99$b_educexpgdp),times=30000)
postp00$educexpgdp_mean <- rep(mean(postp00$b_educexpgdp),times=30000)
postp01$educexpgdp_mean <- rep(mean(postp01$b_educexpgdp),times=30000)
postp02$educexpgdp_mean <- rep(mean(postp02$b_educexpgdp),times=30000)
postp03$educexpgdp_mean <- rep(mean(postp03$b_educexpgdp),times=30000)
postp04$educexpgdp_mean <- rep(mean(postp04$b_educexpgdp),times=30000)
postp05$educexpgdp_mean <- rep(mean(postp05$b_educexpgdp),times=30000)
postp06$educexpgdp_mean <- rep(mean(postp06$b_educexpgdp),times=30000)
postp07$educexpgdp_mean <- rep(mean(postp07$b_educexpgdp),times=30000)
postp08$educexpgdp_mean <- rep(mean(postp08$b_educexpgdp),times=30000)
postp09$educexpgdp_mean <- rep(mean(postp09$b_educexpgdp),times=30000)
postp10$educexpgdp_mean <- rep(mean(postp10$b_educexpgdp),times=30000)
postp11$educexpgdp_mean <- rep(mean(postp11$b_educexpgdp),times=30000)
postp12$educexpgdp_mean <- rep(mean(postp12$b_educexpgdp),times=30000)
postp13$educexpgdp_mean <- rep(mean(postp13$b_educexpgdp),times=30000)
postp14$educexpgdp_mean <- rep(mean(postp14$b_educexpgdp),times=30000)

postp93$left_mean <- rep(mean(postp93$b_left),times=30000)
postp94$left_mean <- rep(mean(postp94$b_left),times=30000)
postp95$left_mean <- rep(mean(postp95$b_left),times=30000)
postp96$left_mean <- rep(mean(postp96$b_left),times=30000)
postp97$left_mean <- rep(mean(postp97$b_left),times=30000)
postp98$left_mean <- rep(mean(postp98$b_left),times=30000)
postp99$left_mean <- rep(mean(postp99$b_left),times=30000)
postp00$left_mean <- rep(mean(postp00$b_left),times=30000)
postp01$left_mean <- rep(mean(postp01$b_left),times=30000)
postp02$left_mean <- rep(mean(postp02$b_left),times=30000)
postp03$left_mean <- rep(mean(postp03$b_left),times=30000)
postp04$left_mean <- rep(mean(postp04$b_left),times=30000)
postp05$left_mean <- rep(mean(postp05$b_left),times=30000)
postp06$left_mean <- rep(mean(postp06$b_left),times=30000)
postp07$left_mean <- rep(mean(postp07$b_left),times=30000)
postp08$left_mean <- rep(mean(postp08$b_left),times=30000)
postp09$left_mean <- rep(mean(postp09$b_left),times=30000)
postp10$left_mean <- rep(mean(postp10$b_left),times=30000)
postp11$left_mean <- rep(mean(postp11$b_left),times=30000)
postp12$left_mean <- rep(mean(postp12$b_left),times=30000)
postp13$left_mean <- rep(mean(postp13$b_left),times=30000)
postp14$left_mean <- rep(mean(postp14$b_left),times=30000)

postp93$wage.arrangementregional_mean <- rep(mean(postp93$b_wage.arrangementRegional),times=30000)
postp94$wage.arrangementregional_mean <- rep(mean(postp94$b_wage.arrangementRegional),times=30000)
postp95$wage.arrangementregional_mean <- rep(mean(postp95$b_wage.arrangementRegional),times=30000)
postp96$wage.arrangementregional_mean <- rep(mean(postp96$b_wage.arrangementRegional),times=30000)
postp97$wage.arrangementregional_mean <- rep(mean(postp97$b_wage.arrangementRegional),times=30000)
postp98$wage.arrangementregional_mean <- rep(mean(postp98$b_wage.arrangementRegional),times=30000)
postp99$wage.arrangementregional_mean <- rep(mean(postp99$b_wage.arrangementRegional),times=30000)
postp00$wage.arrangementregional_mean <- rep(mean(postp00$b_wage.arrangementRegional),times=30000)
postp01$wage.arrangementregional_mean <- rep(mean(postp01$b_wage.arrangementRegional),times=30000)
postp02$wage.arrangementregional_mean <- rep(mean(postp02$b_wage.arrangementRegional),times=30000)
postp03$wage.arrangementregional_mean <- rep(mean(postp03$b_wage.arrangementRegional),times=30000)
postp04$wage.arrangementregional_mean <- rep(mean(postp04$b_wage.arrangementRegional),times=30000)
postp05$wage.arrangementregional_mean <- rep(mean(postp05$b_wage.arrangementRegional),times=30000)
postp06$wage.arrangementregional_mean <- rep(mean(postp06$b_wage.arrangementRegional),times=30000)
postp07$wage.arrangementregional_mean <- rep(mean(postp07$b_wage.arrangementRegional),times=30000)
postp08$wage.arrangementregional_mean <- rep(mean(postp08$b_wage.arrangementRegional),times=30000)
postp09$wage.arrangementregional_mean <- rep(mean(postp09$b_wage.arrangementRegional),times=30000)
postp10$wage.arrangementregional_mean <- rep(mean(postp10$b_wage.arrangementRegional),times=30000)
postp11$wage.arrangementregional_mean <- rep(mean(postp11$b_wage.arrangementRegional),times=30000)
postp12$wage.arrangementregional_mean <- rep(mean(postp12$b_wage.arrangementRegional),times=30000)
postp13$wage.arrangementregional_mean <- rep(mean(postp13$b_wage.arrangementRegional),times=30000)
postp14$wage.arrangementregional_mean <- rep(mean(postp14$b_wage.arrangementRegional),times=30000)

postp93$wage.arrangementsegmented_mean <- rep(mean(postp93$b_wage.arrangementSegmented),times=30000)
postp94$wage.arrangementsegmented_mean <- rep(mean(postp94$b_wage.arrangementSegmented),times=30000)
postp95$wage.arrangementsegmented_mean <- rep(mean(postp95$b_wage.arrangementSegmented),times=30000)
postp96$wage.arrangementsegmented_mean <- rep(mean(postp96$b_wage.arrangementSegmented),times=30000)
postp97$wage.arrangementsegmented_mean <- rep(mean(postp97$b_wage.arrangementSegmented),times=30000)
postp98$wage.arrangementsegmented_mean <- rep(mean(postp98$b_wage.arrangementSegmented),times=30000)
postp99$wage.arrangementsegmented_mean <- rep(mean(postp99$b_wage.arrangementSegmented),times=30000)
postp00$wage.arrangementsegmented_mean <- rep(mean(postp00$b_wage.arrangementSegmented),times=30000)
postp01$wage.arrangementsegmented_mean <- rep(mean(postp01$b_wage.arrangementSegmented),times=30000)
postp02$wage.arrangementsegmented_mean <- rep(mean(postp02$b_wage.arrangementSegmented),times=30000)
postp03$wage.arrangementsegmented_mean <- rep(mean(postp03$b_wage.arrangementSegmented),times=30000)
postp04$wage.arrangementsegmented_mean <- rep(mean(postp04$b_wage.arrangementSegmented),times=30000)
postp05$wage.arrangementsegmented_mean <- rep(mean(postp05$b_wage.arrangementSegmented),times=30000)
postp06$wage.arrangementsegmented_mean <- rep(mean(postp06$b_wage.arrangementSegmented),times=30000)
postp07$wage.arrangementsegmented_mean <- rep(mean(postp07$b_wage.arrangementSegmented),times=30000)
postp08$wage.arrangementsegmented_mean <- rep(mean(postp08$b_wage.arrangementSegmented),times=30000)
postp09$wage.arrangementsegmented_mean <- rep(mean(postp09$b_wage.arrangementSegmented),times=30000)
postp10$wage.arrangementsegmented_mean <- rep(mean(postp10$b_wage.arrangementSegmented),times=30000)
postp11$wage.arrangementsegmented_mean <- rep(mean(postp11$b_wage.arrangementSegmented),times=30000)
postp12$wage.arrangementsegmented_mean <- rep(mean(postp12$b_wage.arrangementSegmented),times=30000)
postp13$wage.arrangementsegmented_mean <- rep(mean(postp13$b_wage.arrangementSegmented),times=30000)
postp14$wage.arrangementsegmented_mean <- rep(mean(postp14$b_wage.arrangementSegmented),times=30000)

postp93$highlyeducatedlabor_mean <- rep(mean(postp93$b_highlyeducatedlabor),times=30000)
postp94$highlyeducatedlabor_mean <- rep(mean(postp94$b_highlyeducatedlabor),times=30000)
postp95$highlyeducatedlabor_mean <- rep(mean(postp95$b_highlyeducatedlabor),times=30000)
postp96$highlyeducatedlabor_mean <- rep(mean(postp96$b_highlyeducatedlabor),times=30000)
postp97$highlyeducatedlabor_mean <- rep(mean(postp97$b_highlyeducatedlabor),times=30000)
postp98$highlyeducatedlabor_mean <- rep(mean(postp98$b_highlyeducatedlabor),times=30000)
postp99$highlyeducatedlabor_mean <- rep(mean(postp99$b_highlyeducatedlabor),times=30000)
postp00$highlyeducatedlabor_mean <- rep(mean(postp00$b_highlyeducatedlabor),times=30000)
postp01$highlyeducatedlabor_mean <- rep(mean(postp01$b_highlyeducatedlabor),times=30000)
postp02$highlyeducatedlabor_mean <- rep(mean(postp02$b_highlyeducatedlabor),times=30000)
postp03$highlyeducatedlabor_mean <- rep(mean(postp03$b_highlyeducatedlabor),times=30000)
postp04$highlyeducatedlabor_mean <- rep(mean(postp04$b_highlyeducatedlabor),times=30000)
postp05$highlyeducatedlabor_mean <- rep(mean(postp05$b_highlyeducatedlabor),times=30000)
postp06$highlyeducatedlabor_mean <- rep(mean(postp06$b_highlyeducatedlabor),times=30000)
postp07$highlyeducatedlabor_mean <- rep(mean(postp07$b_highlyeducatedlabor),times=30000)
postp08$highlyeducatedlabor_mean <- rep(mean(postp08$b_highlyeducatedlabor),times=30000)
postp09$highlyeducatedlabor_mean <- rep(mean(postp09$b_highlyeducatedlabor),times=30000)
postp10$highlyeducatedlabor_mean <- rep(mean(postp10$b_highlyeducatedlabor),times=30000)
postp11$highlyeducatedlabor_mean <- rep(mean(postp11$b_highlyeducatedlabor),times=30000)
postp12$highlyeducatedlabor_mean <- rep(mean(postp12$b_highlyeducatedlabor),times=30000)
postp13$highlyeducatedlabor_mean <- rep(mean(postp13$b_highlyeducatedlabor),times=30000)
postp14$highlyeducatedlabor_mean <- rep(mean(postp14$b_highlyeducatedlabor),times=30000)

postp93$logminwage_mean <- rep(mean(postp93$b_logminwage),times=30000)
postp94$logminwage_mean <- rep(mean(postp94$b_logminwage),times=30000)
postp95$logminwage_mean <- rep(mean(postp95$b_logminwage),times=30000)
postp96$logminwage_mean <- rep(mean(postp96$b_logminwage),times=30000)
postp97$logminwage_mean <- rep(mean(postp97$b_logminwage),times=30000)
postp98$logminwage_mean <- rep(mean(postp98$b_logminwage),times=30000)
postp99$logminwage_mean <- rep(mean(postp99$b_logminwage),times=30000)
postp00$logminwage_mean <- rep(mean(postp00$b_logminwage),times=30000)
postp01$logminwage_mean <- rep(mean(postp01$b_logminwage),times=30000)
postp02$logminwage_mean <- rep(mean(postp02$b_logminwage),times=30000)
postp03$logminwage_mean <- rep(mean(postp03$b_logminwage),times=30000)
postp04$logminwage_mean <- rep(mean(postp04$b_logminwage),times=30000)
postp05$logminwage_mean <- rep(mean(postp05$b_logminwage),times=30000)
postp06$logminwage_mean <- rep(mean(postp06$b_logminwage),times=30000)
postp07$logminwage_mean <- rep(mean(postp07$b_logminwage),times=30000)
postp08$logminwage_mean <- rep(mean(postp08$b_logminwage),times=30000)
postp09$logminwage_mean <- rep(mean(postp09$b_logminwage),times=30000)
postp10$logminwage_mean <- rep(mean(postp10$b_logminwage),times=30000)
postp11$logminwage_mean <- rep(mean(postp11$b_logminwage),times=30000)
postp12$logminwage_mean <- rep(mean(postp12$b_logminwage),times=30000)
postp13$logminwage_mean <- rep(mean(postp13$b_logminwage),times=30000)
postp14$logminwage_mean <- rep(mean(postp14$b_logminwage),times=30000)

postp93$employment_mean <- rep(mean(postp93$b_employment),times=30000)
postp94$employment_mean <- rep(mean(postp94$b_employment),times=30000)
postp95$employment_mean <- rep(mean(postp95$b_employment),times=30000)
postp96$employment_mean <- rep(mean(postp96$b_employment),times=30000)
postp97$employment_mean <- rep(mean(postp97$b_employment),times=30000)
postp98$employment_mean <- rep(mean(postp98$b_employment),times=30000)
postp99$employment_mean <- rep(mean(postp99$b_employment),times=30000)
postp00$employment_mean <- rep(mean(postp00$b_employment),times=30000)
postp01$employment_mean <- rep(mean(postp01$b_employment),times=30000)
postp02$employment_mean <- rep(mean(postp02$b_employment),times=30000)
postp03$employment_mean <- rep(mean(postp03$b_employment),times=30000)
postp04$employment_mean <- rep(mean(postp04$b_employment),times=30000)
postp05$employment_mean <- rep(mean(postp05$b_employment),times=30000)
postp06$employment_mean <- rep(mean(postp06$b_employment),times=30000)
postp07$employment_mean <- rep(mean(postp07$b_employment),times=30000)
postp08$employment_mean <- rep(mean(postp08$b_employment),times=30000)
postp09$employment_mean <- rep(mean(postp09$b_employment),times=30000)
postp10$employment_mean <- rep(mean(postp10$b_employment),times=30000)
postp11$employment_mean <- rep(mean(postp11$b_employment),times=30000)
postp12$employment_mean <- rep(mean(postp12$b_employment),times=30000)
postp13$employment_mean <- rep(mean(postp13$b_employment),times=30000)
postp14$employment_mean <- rep(mean(postp14$b_employment),times=30000)


colnames(fits)
library(plyr)
fitsRace = rbind.fill(postp93, postp94, postp95, postp96,postp97,postp98,postp99,postp00,
                      postp01,postp02,postp03,postp04,postp05,postp06,postp07,postp08,postp09,
                      postp10,postp11,postp12,postp13,postp14)

library(ggdist)
#Replace Races

BlackIneq<-ggplot(fitsRace, aes(y = (as.year), x = Blacks_mean)) +
  stat_halfeye(aes(x=b_Blacks),
               .width = c(.90, .5),
               slab_fill= "chocolate4",
               point_color = "bisque4",
               point_fill = "burlywood4",
               interval_color = "brown4") + 
  labs(x="Effect on Inequality from Black share of population")+
  geom_vline(xintercept = 0, linetype = "dashed") 
BlackIneq<-BlackIneq+ coord_flip()+ylab(NULL)+ theme(legend.position="none")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
BlackIneq

MulattoIneq<-ggplot(fitsRace, aes(y = (as.year), x = Mulattoes_mean)) +
  stat_halfeye(aes(x=b_Mulattoes),
               .width = c(.90, .5),
               slab_fill= "darkkhaki",
               point_color = "brown1",
               point_fill = "brown3",
               interval_color = "brown4") +  labs(x="Effect on Inequality from Mulatto share of population")+
  geom_vline(xintercept = 0, linetype = "dashed") 
MulattoIneq<-MulattoIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
MulattoIneq

MestizoIneq<-ggplot(fitsRace, aes(y = (as.year), x = Mestizo_mean)) +
  stat_halfeye(aes(x=b_Mestizo),
               .width = c(.90, .5),
               slab_fill= "yellow3",
               point_color = "dodgerblue4",
               point_fill = "lightseagreen",
               interval_color = "lightseagreen") +  labs(x="Effect on Inequality from Mestizos share of population")+
  geom_vline(xintercept = 0, linetype = "dashed") 
MestizoIneq<-MestizoIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
MestizoIneq

AmerindianIneq<-ggplot(fitsRace, aes(y = (as.year), x = Amerindians_mean)) +
  stat_halfeye(aes(x=b_Amerindians),
               .width = c(.90, .5),
               slab_fill= "slateblue",
               point_color = "darkorchid4",
               point_fill = "turquoise4",
               interval_color = "turquoise4") +   labs(x="Effect on Inequality from Amerindians share of population")+
  geom_vline(xintercept = 0, linetype = "dashed") 
AmerindianIneq<-AmerindianIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
AmerindianIneq

AsianIneq<-ggplot(fitsRace, aes(y = (as.year), x = Asians_mean)) +
  stat_halfeye(aes(x=b_Asians),
               .width = c(.90, .5),
               slab_fill= "springgreen",
               point_color = "violetred",
               point_fill = "slategray4",
               interval_color = "slategray3") +   labs(x="Effect on Inequality from Asians share of population")+
  geom_vline(xintercept = 0, linetype = "dashed") 
AsianIneq<-AsianIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
AsianIneq

CreolesIneq<-ggplot(fitsRace, aes(y = (as.year), x = CreolesEtGafurinas_mean)) +
  stat_halfeye(aes(x=b_CreolesEtGafurinas),
               .width = c(.90, .5),
               slab_fill= "forestgreen",
               point_color = "darkred",
               point_fill = "darkslategray",
               interval_color = "darkslategray") + 
  labs(x="Effect on Inequality from Creoles and Garífunas share of population")+
  geom_vline(xintercept = 0, linetype = "dashed") 
CreolesIneq<-CreolesIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
CreolesIneq

SocExpGDP<-ggplot(fitsRace, aes(y = (as.year), x = socexpgdp_mean)) +
  stat_halfeye(aes(x=b_socexpgdp),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from Social Expenditure as a share of GDP")+
  geom_vline(xintercept = 0, linetype = "dashed") 
SocExpGDP<-SocExpGDP+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
SocExpGDP

EduExpGDP<-ggplot(fitsRace, aes(y = (as.year), x = educexpgdp_mean)) +
  stat_halfeye(aes(x=b_educexpgdp),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from Educational Expenditure as a share of GDP")+
  geom_vline(xintercept = 0, linetype = "dashed") 
EduExpGDP<-EduExpGDP+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
EduExpGDP

LeftIneq<-ggplot(fitsRace, aes(y = (as.year), x = educexpgdp_mean)) +
  stat_halfeye(aes(x=b_educexpgdp),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from Left-of-Center governments")+
  geom_vline(xintercept = 0, linetype = "dashed") 
LeftIneq<-LeftIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
LeftIneq

TertiaryIneq<-ggplot(fitsRace, aes(y = (as.year), x = highlyeducatedlabor_mean)) +
  stat_halfeye(aes(x=b_highlyeducatedlabor),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from Educational Expenditure as a share of GDP") +
  geom_vline(xintercept = 0, linetype = "dashed") 
TertiaryIneq<-TertiaryIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
TertiaryIneq

WageRegionallIneq<-ggplot(fitsRace, aes(y = (as.year), x = wage.arrangementregional_mean)) +
  stat_halfeye(aes(x=b_wage.arrangementRegional),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from regional wage arrangements (vs. national)")+
  geom_vline(xintercept = 0, linetype = "dashed") 
WageRegionallIneq<-WageRegionallIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
WageRegionallIneq

WageSegmlIneq<-ggplot(fitsRace, aes(y = (as.year), x = wage.arrangementsegmented_mean)) +
  stat_halfeye(aes(x=b_wage.arrangementSegmented),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from segmented wage arrangements (vs. national)")+
  geom_vline(xintercept = 0, linetype = "dashed") 
WageSegmlIneq<-WageSegmlIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
WageSegmlIneq

LogMinWageIneq<-ggplot(fitsRace, aes(y = (as.year), x = logminwage_mean)) +
  stat_halfeye(aes(x=b_logminwage),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from log(minimum wage)")+
  geom_vline(xintercept = 0, linetype = "dashed") 
LogMinWageIneq<-LogMinWageIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
LogMinWageIneq

EmployIneq<-ggplot(fitsRace, aes(y = (as.year), x = employment_mean)) +
  stat_halfeye(aes(x=b_employment),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from Employment rate")+
  geom_vline(xintercept = 0, linetype = "dashed") 
EmployIneq<-EmployIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
EmployIneq
library(gridExtra)
grid.arrange(BlackIneq, MulattoIneq, MestizoIneq, AmerindianIneq, AsianIneq, CreolesIneq,
             SocExpGDP,EduExpGDP,LeftIneq,TertiaryIneq,
             WageRegionallIneq,WageSegmlIneq,LogMinWageIneq,EmployIneq,ncol=3)


## new ones

#Subsets for each year
yr1993<-subset(LatAm2014,year==1993)
yr1994<-subset(LatAm2014,year==1994)
yr1995<-subset(LatAm2014,year==1995)
yr1996<-subset(LatAm2014,year==1996)
yr1997<-subset(LatAm2014,year==1997)
yr1998<-subset(LatAm2014,year==1998)
yr1999<-subset(LatAm2014,year==1999)
yr2000<-subset(LatAm2014,year==2000)
yr2001<-subset(LatAm2014,year==2001)
yr2002<-subset(LatAm2014,year==2002)
yr2003<-subset(LatAm2014,year==2003)
yr2004<-subset(LatAm2014,year==2004)
yr2005<-subset(LatAm2014,year==2005)
yr2006<-subset(LatAm2014,year==2006)
yr2007<-subset(LatAm2014,year==2007)
yr2008<-subset(LatAm2014,year==2008)
yr2009<-subset(LatAm2014,year==2009)
yr2010<-subset(LatAm2014,year==2010)
yr2011<-subset(LatAm2014,year==2011)
yr2012<-subset(LatAm2014,year==2012)
yr2013<-subset(LatAm2014,year==2013)
yr2014<-subset(LatAm2014,year==2014)

M1.93<-brm(data = yr1993, family = gaussian,
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                     prior(normal(0, 2), class = b),
                     prior(cauchy(0,2), class = sigma)),
           control=list(adapt_delta=0.99, max_treedepth=20),
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1.94<-brm(data = yr1994, family = gaussian,
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                     prior(normal(0, 2), class = b),
                     prior(cauchy(0,2), class = sigma)),
           control=list(adapt_delta=0.99, max_treedepth=20),
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1.95<-brm(data = yr1995, family = gaussian,
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                     prior(normal(0, 2), class = b),                    
                     prior(cauchy(0,2), class = sigma)),          
           control=list(adapt_delta=0.99, max_treedepth=20),          
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1.96<-brm(data = yr1996, family = gaussian,          
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                     prior(normal(0, 2), class = b),                    
                     prior(cauchy(0,2), class = sigma)),          
           control=list(adapt_delta=0.99, max_treedepth=20),          
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)

M1.97<-brm(data = yr1997, family = gaussian,          
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                     prior(normal(0, 2), class = b),                    
                     prior(cauchy(0,2), class = sigma)),          
           control=list(adapt_delta=0.99, max_treedepth=20),          
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1.98<-brm(data = yr1998, family = gaussian,          
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                     prior(normal(0, 2), class = b),                    
                     prior(cauchy(0,2), class = sigma)),          
           control=list(adapt_delta=0.99, max_treedepth=20),          
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1.99<-brm(data = yr1999, family = gaussian,          
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                     prior(normal(0, 2), class = b),                    
                     prior(cauchy(0,2), class = sigma)),          
           control=list(adapt_delta=0.99, max_treedepth=20),          
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1.00<-brm(data = yr2000, family = gaussian,          
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                     prior(normal(0, 2), class = b),                    
                     prior(cauchy(0,2), class = sigma)),          
           control=list(adapt_delta=0.99, max_treedepth=20),          
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1.01<-brm(data = yr2001, family = gaussian,          
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                     prior(normal(0, 2), class = b),                    
                     prior(cauchy(0,2), class = sigma)),          
           control=list(adapt_delta=0.99, max_treedepth=20),          
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1.02<-brm(data = yr2002, family = gaussian,          
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                     prior(normal(0, 2), class = b),                    
                     prior(cauchy(0,2), class = sigma)),          
           control=list(adapt_delta=0.99, max_treedepth=20),          
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1.03<-brm(data = yr2003, family = gaussian,          
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                     prior(normal(0, 2), class = b),                    
                     prior(cauchy(0,2), class = sigma)),          
           control=list(adapt_delta=0.99, max_treedepth=20),          
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1.04<-brm(data = yr2004, family = gaussian,          
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                     prior(normal(0, 2), class = b),                    
                     prior(cauchy(0,2), class = sigma)),          
           control=list(adapt_delta=0.99, max_treedepth=20),          
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1.05<-brm(data = yr2005, family = gaussian,          
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                     prior(normal(0, 2), class = b),                    
                     prior(cauchy(0,2), class = sigma)),          
           control=list(adapt_delta=0.99, max_treedepth=20),          
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1.06<-brm(data = yr2006, family = gaussian,          
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                     prior(normal(0, 2), class = b),                    
                     prior(cauchy(0,2), class = sigma)),          
           control=list(adapt_delta=0.99, max_treedepth=20),          
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1.07<-brm(data = yr2007, family = gaussian,          
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                     prior(normal(0, 2), class = b),                    
                     prior(cauchy(0,2), class = sigma)),          
           control=list(adapt_delta=0.99, max_treedepth=20),          
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1.08<-brm(data = yr2008, family = gaussian,          
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                     prior(normal(0, 2), class = b),                    
                     prior(cauchy(0,2), class = sigma)),          
           control=list(adapt_delta=0.99, max_treedepth=20),          
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1.09<-brm(data = yr2009, family = gaussian,          
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                     prior(normal(0, 2), class = b),                    
                     prior(cauchy(0,2), class = sigma)),          
           control=list(adapt_delta=0.99, max_treedepth=20),          
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1.10<-brm(data = yr2010, family = gaussian,          
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                     prior(normal(0, 2), class = b),                    
                     prior(cauchy(0,2), class = sigma)),          
           control=list(adapt_delta=0.99, max_treedepth=20),          
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1.11<-brm(data = yr2011, family = gaussian,          
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                     prior(normal(0, 2), class = b),                    
                     prior(cauchy(0,2), class = sigma)),          
           control=list(adapt_delta=0.99, max_treedepth=20),          
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1.12<-brm(data = yr2012, family = gaussian,          
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                     prior(normal(0, 2), class = b),                    
                     prior(cauchy(0,2), class = sigma)),          
           control=list(adapt_delta=0.99, max_treedepth=20),          
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1.13<-brm(data = yr2013, family = gaussian,          
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                     prior(normal(0, 2), class = b),                    
                     prior(cauchy(0,2), class = sigma)),          
           control=list(adapt_delta=0.99, max_treedepth=20),          
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
M1.14<-brm(data = yr2014, family = gaussian,          
           gini100~EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,          
           prior = c(prior(normal(49.43, 5.018121), class = Intercept),                    
                     prior(normal(0, 2), class = b),                    
                     prior(cauchy(0,2), class = sigma)),          
           control=list(adapt_delta=0.99, max_treedepth=20),          
           iter = 10000, warmup = 2500, chains = 4, cores = 1,seed = 5)
library(broom)


#broom.mixed::tidy(M1.all)
#str(rstan::extract(M1.all))

post93 <- posterior_samples(M1.93)
post94 <- posterior_samples(M1.94)
post95 <- posterior_samples(M1.95)
post96 <- posterior_samples(M1.96)
post97 <- posterior_samples(M1.97)
post98 <- posterior_samples(M1.98)
post99 <- posterior_samples(M1.99)
post00 <- posterior_samples(M1.00)
post01 <- posterior_samples(M1.01)
post02 <- posterior_samples(M1.02)
post03 <- posterior_samples(M1.03)
post04 <- posterior_samples(M1.04)
post05 <- posterior_samples(M1.05)
post06 <- posterior_samples(M1.06)
post07 <- posterior_samples(M1.07)
post08 <- posterior_samples(M1.08)
post09 <- posterior_samples(M1.09)
post10 <- posterior_samples(M1.10)
post11 <- posterior_samples(M1.11)
post12 <- posterior_samples(M1.12)
post13 <- posterior_samples(M1.13)
post14 <- posterior_samples(M1.14)

post93$year <- rep(1993,times=length(post93))
post94$year <- rep(1994,times=length(post94))
post95$year <- rep(1995,times=length(post95))
post96$year <- rep(1996,times=length(post96))
post97$year <- rep(1997,times=length(post97))
post98$year <- rep(1998,times=length(post98))
post99$year <- rep(1999,times=length(post99))
post00$year <- rep(2000,times=length(post00))
post01$year <- rep(2001,times=length(post01))
post02$year <- rep(2002,times=length(post02))
post03$year <- rep(2003,times=length(post03))
post04$year <- rep(2004,times=length(post04))
post05$year <- rep(2005,times=length(post05))
post06$year <- rep(2006,times=length(post06))
post07$year <- rep(2007,times=length(post07))
post08$year <- rep(2008,times=length(post08))
post09$year <- rep(2009,times=length(post09))
post10$year <- rep(2010,times=length(post10))
post11$year <- rep(2011,times=length(post11))
post12$year <- rep(2012,times=length(post12))
post13$year <- rep(2013,times=length(post13))
post14$year <- rep(2014,times=length(post14))

post93$EthnicFractionalizationTS_mean <- rep(mean(post93$b_EthnicFractionalizationTS),times=30000)
post94$EthnicFractionalizationTS_mean <- rep(mean(post94$b_EthnicFractionalizationTS),times=30000)
post95$EthnicFractionalizationTS_mean <- rep(mean(post95$b_EthnicFractionalizationTS),times=30000)
post96$EthnicFractionalizationTS_mean <- rep(mean(post96$b_EthnicFractionalizationTS),times=30000)
post97$EthnicFractionalizationTS_mean <- rep(mean(post97$b_EthnicFractionalizationTS),times=30000)
post98$EthnicFractionalizationTS_mean <- rep(mean(post98$b_EthnicFractionalizationTS),times=30000)
post99$EthnicFractionalizationTS_mean <- rep(mean(post99$b_EthnicFractionalizationTS),times=30000)
post00$EthnicFractionalizationTS_mean <- rep(mean(post00$b_EthnicFractionalizationTS),times=30000)
post01$EthnicFractionalizationTS_mean <- rep(mean(post01$b_EthnicFractionalizationTS),times=30000)
post02$EthnicFractionalizationTS_mean <- rep(mean(post02$b_EthnicFractionalizationTS),times=30000)
post03$EthnicFractionalizationTS_mean <- rep(mean(post03$b_EthnicFractionalizationTS),times=30000)
post04$EthnicFractionalizationTS_mean <- rep(mean(post04$b_EthnicFractionalizationTS),times=30000)
post05$EthnicFractionalizationTS_mean <- rep(mean(post05$b_EthnicFractionalizationTS),times=30000)
post06$EthnicFractionalizationTS_mean <- rep(mean(post06$b_EthnicFractionalizationTS),times=30000)
post07$EthnicFractionalizationTS_mean <- rep(mean(post07$b_EthnicFractionalizationTS),times=30000)
post08$EthnicFractionalizationTS_mean <- rep(mean(post08$b_EthnicFractionalizationTS),times=30000)
post09$EthnicFractionalizationTS_mean <- rep(mean(post09$b_EthnicFractionalizationTS),times=30000)
post10$EthnicFractionalizationTS_mean <- rep(mean(post10$b_EthnicFractionalizationTS),times=30000)
post11$EthnicFractionalizationTS_mean <- rep(mean(post11$b_EthnicFractionalizationTS),times=30000)
post12$EthnicFractionalizationTS_mean <- rep(mean(post12$b_EthnicFractionalizationTS),times=30000)
post13$EthnicFractionalizationTS_mean <- rep(mean(post13$b_EthnicFractionalizationTS),times=30000)
post14$EthnicFractionalizationTS_mean <- rep(mean(post14$b_EthnicFractionalizationTS),times=30000)

post93$socexpgdp_mean <- rep(mean(post93$b_socexpgdp),times=30000)
post94$socexpgdp_mean <- rep(mean(post94$b_socexpgdp),times=30000)
post95$socexpgdp_mean <- rep(mean(post95$b_socexpgdp),times=30000)
post96$socexpgdp_mean <- rep(mean(post96$b_socexpgdp),times=30000)
post97$socexpgdp_mean <- rep(mean(post97$b_socexpgdp),times=30000)
post98$socexpgdp_mean <- rep(mean(post98$b_socexpgdp),times=30000)
post99$socexpgdp_mean <- rep(mean(post99$b_socexpgdp),times=30000)
post00$socexpgdp_mean <- rep(mean(post00$b_socexpgdp),times=30000)
post01$socexpgdp_mean <- rep(mean(post01$b_socexpgdp),times=30000)
post02$socexpgdp_mean <- rep(mean(post02$b_socexpgdp),times=30000)
post03$socexpgdp_mean <- rep(mean(post03$b_socexpgdp),times=30000)
post04$socexpgdp_mean <- rep(mean(post04$b_socexpgdp),times=30000)
post05$socexpgdp_mean <- rep(mean(post05$b_socexpgdp),times=30000)
post06$socexpgdp_mean <- rep(mean(post06$b_socexpgdp),times=30000)
post07$socexpgdp_mean <- rep(mean(post07$b_socexpgdp),times=30000)
post08$socexpgdp_mean <- rep(mean(post08$b_socexpgdp),times=30000)
post09$socexpgdp_mean <- rep(mean(post09$b_socexpgdp),times=30000)
post10$socexpgdp_mean <- rep(mean(post10$b_socexpgdp),times=30000)
post11$socexpgdp_mean <- rep(mean(post11$b_socexpgdp),times=30000)
post12$socexpgdp_mean <- rep(mean(post12$b_socexpgdp),times=30000)
post13$socexpgdp_mean <- rep(mean(post13$b_socexpgdp),times=30000)
post14$socexpgdp_mean <- rep(mean(post14$b_socexpgdp),times=30000)

post93$educexpgdp_mean <- rep(mean(post93$b_educexpgdp),times=30000)
post94$educexpgdp_mean <- rep(mean(post94$b_educexpgdp),times=30000)
post95$educexpgdp_mean <- rep(mean(post95$b_educexpgdp),times=30000)
post96$educexpgdp_mean <- rep(mean(post96$b_educexpgdp),times=30000)
post97$educexpgdp_mean <- rep(mean(post97$b_educexpgdp),times=30000)
post98$educexpgdp_mean <- rep(mean(post98$b_educexpgdp),times=30000)
post99$educexpgdp_mean <- rep(mean(post99$b_educexpgdp),times=30000)
post00$educexpgdp_mean <- rep(mean(post00$b_educexpgdp),times=30000)
post01$educexpgdp_mean <- rep(mean(post01$b_educexpgdp),times=30000)
post02$educexpgdp_mean <- rep(mean(post02$b_educexpgdp),times=30000)
post03$educexpgdp_mean <- rep(mean(post03$b_educexpgdp),times=30000)
post04$educexpgdp_mean <- rep(mean(post04$b_educexpgdp),times=30000)
post05$educexpgdp_mean <- rep(mean(post05$b_educexpgdp),times=30000)
post06$educexpgdp_mean <- rep(mean(post06$b_educexpgdp),times=30000)
post07$educexpgdp_mean <- rep(mean(post07$b_educexpgdp),times=30000)
post08$educexpgdp_mean <- rep(mean(post08$b_educexpgdp),times=30000)
post09$educexpgdp_mean <- rep(mean(post09$b_educexpgdp),times=30000)
post10$educexpgdp_mean <- rep(mean(post10$b_educexpgdp),times=30000)
post11$educexpgdp_mean <- rep(mean(post11$b_educexpgdp),times=30000)
post12$educexpgdp_mean <- rep(mean(post12$b_educexpgdp),times=30000)
post13$educexpgdp_mean <- rep(mean(post13$b_educexpgdp),times=30000)
post14$educexpgdp_mean <- rep(mean(post14$b_educexpgdp),times=30000)

post93$left_mean <- rep(mean(post93$b_left),times=30000)
post94$left_mean <- rep(mean(post94$b_left),times=30000)
post95$left_mean <- rep(mean(post95$b_left),times=30000)
post96$left_mean <- rep(mean(post96$b_left),times=30000)
post97$left_mean <- rep(mean(post97$b_left),times=30000)
post98$left_mean <- rep(mean(post98$b_left),times=30000)
post99$left_mean <- rep(mean(post99$b_left),times=30000)
post00$left_mean <- rep(mean(post00$b_left),times=30000)
post01$left_mean <- rep(mean(post01$b_left),times=30000)
post02$left_mean <- rep(mean(post02$b_left),times=30000)
post03$left_mean <- rep(mean(post03$b_left),times=30000)
post04$left_mean <- rep(mean(post04$b_left),times=30000)
post05$left_mean <- rep(mean(post05$b_left),times=30000)
post06$left_mean <- rep(mean(post06$b_left),times=30000)
post07$left_mean <- rep(mean(post07$b_left),times=30000)
post08$left_mean <- rep(mean(post08$b_left),times=30000)
post09$left_mean <- rep(mean(post09$b_left),times=30000)
post10$left_mean <- rep(mean(post10$b_left),times=30000)
post11$left_mean <- rep(mean(post11$b_left),times=30000)
post12$left_mean <- rep(mean(post12$b_left),times=30000)
post13$left_mean <- rep(mean(post13$b_left),times=30000)
post14$left_mean <- rep(mean(post14$b_left),times=30000)

post93$wage.arrangementregional_mean <- rep(mean(post93$b_wage.arrangementRegional),times=30000)
post94$wage.arrangementregional_mean <- rep(mean(post94$b_wage.arrangementRegional),times=30000)
post95$wage.arrangementregional_mean <- rep(mean(post95$b_wage.arrangementRegional),times=30000)
post96$wage.arrangementregional_mean <- rep(mean(post96$b_wage.arrangementRegional),times=30000)
post97$wage.arrangementregional_mean <- rep(mean(post97$b_wage.arrangementRegional),times=30000)
post98$wage.arrangementregional_mean <- rep(mean(post98$b_wage.arrangementRegional),times=30000)
post99$wage.arrangementregional_mean <- rep(mean(post99$b_wage.arrangementRegional),times=30000)
post00$wage.arrangementregional_mean <- rep(mean(post00$b_wage.arrangementRegional),times=30000)
post01$wage.arrangementregional_mean <- rep(mean(post01$b_wage.arrangementRegional),times=30000)
post02$wage.arrangementregional_mean <- rep(mean(post02$b_wage.arrangementRegional),times=30000)
post03$wage.arrangementregional_mean <- rep(mean(post03$b_wage.arrangementRegional),times=30000)
post04$wage.arrangementregional_mean <- rep(mean(post04$b_wage.arrangementRegional),times=30000)
post05$wage.arrangementregional_mean <- rep(mean(post05$b_wage.arrangementRegional),times=30000)
post06$wage.arrangementregional_mean <- rep(mean(post06$b_wage.arrangementRegional),times=30000)
post07$wage.arrangementregional_mean <- rep(mean(post07$b_wage.arrangementRegional),times=30000)
post08$wage.arrangementregional_mean <- rep(mean(post08$b_wage.arrangementRegional),times=30000)
post09$wage.arrangementregional_mean <- rep(mean(post09$b_wage.arrangementRegional),times=30000)
post10$wage.arrangementregional_mean <- rep(mean(post10$b_wage.arrangementRegional),times=30000)
post11$wage.arrangementregional_mean <- rep(mean(post11$b_wage.arrangementRegional),times=30000)
post12$wage.arrangementregional_mean <- rep(mean(post12$b_wage.arrangementRegional),times=30000)
post13$wage.arrangementregional_mean <- rep(mean(post13$b_wage.arrangementRegional),times=30000)
post14$wage.arrangementregional_mean <- rep(mean(post14$b_wage.arrangementRegional),times=30000)

post93$wage.arrangementsegmented_mean <- rep(mean(post93$b_wage.arrangementSegmented),times=30000)
post94$wage.arrangementsegmented_mean <- rep(mean(post94$b_wage.arrangementSegmented),times=30000)
post95$wage.arrangementsegmented_mean <- rep(mean(post95$b_wage.arrangementSegmented),times=30000)
post96$wage.arrangementsegmented_mean <- rep(mean(post96$b_wage.arrangementSegmented),times=30000)
post97$wage.arrangementsegmented_mean <- rep(mean(post97$b_wage.arrangementSegmented),times=30000)
post98$wage.arrangementsegmented_mean <- rep(mean(post98$b_wage.arrangementSegmented),times=30000)
post99$wage.arrangementsegmented_mean <- rep(mean(post99$b_wage.arrangementSegmented),times=30000)
post00$wage.arrangementsegmented_mean <- rep(mean(post00$b_wage.arrangementSegmented),times=30000)
post01$wage.arrangementsegmented_mean <- rep(mean(post01$b_wage.arrangementSegmented),times=30000)
post02$wage.arrangementsegmented_mean <- rep(mean(post02$b_wage.arrangementSegmented),times=30000)
post03$wage.arrangementsegmented_mean <- rep(mean(post03$b_wage.arrangementSegmented),times=30000)
post04$wage.arrangementsegmented_mean <- rep(mean(post04$b_wage.arrangementSegmented),times=30000)
post05$wage.arrangementsegmented_mean <- rep(mean(post05$b_wage.arrangementSegmented),times=30000)
post06$wage.arrangementsegmented_mean <- rep(mean(post06$b_wage.arrangementSegmented),times=30000)
post07$wage.arrangementsegmented_mean <- rep(mean(post07$b_wage.arrangementSegmented),times=30000)
post08$wage.arrangementsegmented_mean <- rep(mean(post08$b_wage.arrangementSegmented),times=30000)
post09$wage.arrangementsegmented_mean <- rep(mean(post09$b_wage.arrangementSegmented),times=30000)
post10$wage.arrangementsegmented_mean <- rep(mean(post10$b_wage.arrangementSegmented),times=30000)
post11$wage.arrangementsegmented_mean <- rep(mean(post11$b_wage.arrangementSegmented),times=30000)
post12$wage.arrangementsegmented_mean <- rep(mean(post12$b_wage.arrangementSegmented),times=30000)
post13$wage.arrangementsegmented_mean <- rep(mean(post13$b_wage.arrangementSegmented),times=30000)
post14$wage.arrangementsegmented_mean <- rep(mean(post14$b_wage.arrangementSegmented),times=30000)

post93$highlyeducatedlabor_mean <- rep(mean(post93$b_highlyeducatedlabor),times=30000)
post94$highlyeducatedlabor_mean <- rep(mean(post94$b_highlyeducatedlabor),times=30000)
post95$highlyeducatedlabor_mean <- rep(mean(post95$b_highlyeducatedlabor),times=30000)
post96$highlyeducatedlabor_mean <- rep(mean(post96$b_highlyeducatedlabor),times=30000)
post97$highlyeducatedlabor_mean <- rep(mean(post97$b_highlyeducatedlabor),times=30000)
post98$highlyeducatedlabor_mean <- rep(mean(post98$b_highlyeducatedlabor),times=30000)
post99$highlyeducatedlabor_mean <- rep(mean(post99$b_highlyeducatedlabor),times=30000)
post00$highlyeducatedlabor_mean <- rep(mean(post00$b_highlyeducatedlabor),times=30000)
post01$highlyeducatedlabor_mean <- rep(mean(post01$b_highlyeducatedlabor),times=30000)
post02$highlyeducatedlabor_mean <- rep(mean(post02$b_highlyeducatedlabor),times=30000)
post03$highlyeducatedlabor_mean <- rep(mean(post03$b_highlyeducatedlabor),times=30000)
post04$highlyeducatedlabor_mean <- rep(mean(post04$b_highlyeducatedlabor),times=30000)
post05$highlyeducatedlabor_mean <- rep(mean(post05$b_highlyeducatedlabor),times=30000)
post06$highlyeducatedlabor_mean <- rep(mean(post06$b_highlyeducatedlabor),times=30000)
post07$highlyeducatedlabor_mean <- rep(mean(post07$b_highlyeducatedlabor),times=30000)
post08$highlyeducatedlabor_mean <- rep(mean(post08$b_highlyeducatedlabor),times=30000)
post09$highlyeducatedlabor_mean <- rep(mean(post09$b_highlyeducatedlabor),times=30000)
post10$highlyeducatedlabor_mean <- rep(mean(post10$b_highlyeducatedlabor),times=30000)
post11$highlyeducatedlabor_mean <- rep(mean(post11$b_highlyeducatedlabor),times=30000)
post12$highlyeducatedlabor_mean <- rep(mean(post12$b_highlyeducatedlabor),times=30000)
post13$highlyeducatedlabor_mean <- rep(mean(post13$b_highlyeducatedlabor),times=30000)
post14$highlyeducatedlabor_mean <- rep(mean(post14$b_highlyeducatedlabor),times=30000)

post93$logminwage_mean <- rep(mean(post93$b_logminwage),times=30000)
post94$logminwage_mean <- rep(mean(post94$b_logminwage),times=30000)
post95$logminwage_mean <- rep(mean(post95$b_logminwage),times=30000)
post96$logminwage_mean <- rep(mean(post96$b_logminwage),times=30000)
post97$logminwage_mean <- rep(mean(post97$b_logminwage),times=30000)
post98$logminwage_mean <- rep(mean(post98$b_logminwage),times=30000)
post99$logminwage_mean <- rep(mean(post99$b_logminwage),times=30000)
post00$logminwage_mean <- rep(mean(post00$b_logminwage),times=30000)
post01$logminwage_mean <- rep(mean(post01$b_logminwage),times=30000)
post02$logminwage_mean <- rep(mean(post02$b_logminwage),times=30000)
post03$logminwage_mean <- rep(mean(post03$b_logminwage),times=30000)
post04$logminwage_mean <- rep(mean(post04$b_logminwage),times=30000)
post05$logminwage_mean <- rep(mean(post05$b_logminwage),times=30000)
post06$logminwage_mean <- rep(mean(post06$b_logminwage),times=30000)
post07$logminwage_mean <- rep(mean(post07$b_logminwage),times=30000)
post08$logminwage_mean <- rep(mean(post08$b_logminwage),times=30000)
post09$logminwage_mean <- rep(mean(post09$b_logminwage),times=30000)
post10$logminwage_mean <- rep(mean(post10$b_logminwage),times=30000)
post11$logminwage_mean <- rep(mean(post11$b_logminwage),times=30000)
post12$logminwage_mean <- rep(mean(post12$b_logminwage),times=30000)
post13$logminwage_mean <- rep(mean(post13$b_logminwage),times=30000)
post14$logminwage_mean <- rep(mean(post14$b_logminwage),times=30000)

post93$employment_mean <- rep(mean(post93$b_employment),times=30000)
post94$employment_mean <- rep(mean(post94$b_employment),times=30000)
post95$employment_mean <- rep(mean(post95$b_employment),times=30000)
post96$employment_mean <- rep(mean(post96$b_employment),times=30000)
post97$employment_mean <- rep(mean(post97$b_employment),times=30000)
post98$employment_mean <- rep(mean(post98$b_employment),times=30000)
post99$employment_mean <- rep(mean(post99$b_employment),times=30000)
post00$employment_mean <- rep(mean(post00$b_employment),times=30000)
post01$employment_mean <- rep(mean(post01$b_employment),times=30000)
post02$employment_mean <- rep(mean(post02$b_employment),times=30000)
post03$employment_mean <- rep(mean(post03$b_employment),times=30000)
post04$employment_mean <- rep(mean(post04$b_employment),times=30000)
post05$employment_mean <- rep(mean(post05$b_employment),times=30000)
post06$employment_mean <- rep(mean(post06$b_employment),times=30000)
post07$employment_mean <- rep(mean(post07$b_employment),times=30000)
post08$employment_mean <- rep(mean(post08$b_employment),times=30000)
post09$employment_mean <- rep(mean(post09$b_employment),times=30000)
post10$employment_mean <- rep(mean(post10$b_employment),times=30000)
post11$employment_mean <- rep(mean(post11$b_employment),times=30000)
post12$employment_mean <- rep(mean(post12$b_employment),times=30000)
post13$employment_mean <- rep(mean(post13$b_employment),times=30000)
post14$employment_mean <- rep(mean(post14$b_employment),times=30000)


library(plyr)
fitsDiverse = rbind.fill(post93, post94, post95, post96,post97,post98,post99,post00,
                         post01,post02,post03,post04,post05,post06,post07,post08,post09,
                         post10,post11,post12,post13,post14)

library(ggdist)
FractionalDiversity<-ggplot(fitsDiverse, aes(y = (as.year), x = EthnicFractionalizationTS_mean, fill="red")) +
  stat_halfeye(aes(x=b_EthnicFractionalizationTS), .width = c(.90, .5),
               slab_fill= "firebrick1",
               point_color = "gray8",
               point_fill = "gray8",
               interval_color = "forestgreen") +
  labs(x="Effect on Inequality from Ethnic/Racial Fractionalization")+
  geom_vline(xintercept = 0, linetype = "dashed") 
FractionalDiversity<-FractionalDiversity+ coord_flip()+geom_hline(yintercept = 0,colour="red")+ theme(legend.position="none")+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
FractionalDiversity

SocExpGDP<-ggplot(fitsDiverse, aes(y = (as.year), x = socexpgdp_mean)) +
  stat_halfeye(aes(x=b_socexpgdp),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from Social Expenditure as a share of GDP")+
  geom_vline(xintercept = 0, linetype = "dashed") 
SocExpGDP<-SocExpGDP+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
SocExpGDP

EduExpGDP<-ggplot(fitsDiverse, aes(y = (as.year), x = educexpgdp_mean)) +
  stat_halfeye(aes(x=b_educexpgdp),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from Educational Expenditure as a share of GDP")+
  geom_vline(xintercept = 0, linetype = "dashed") 
EduExpGDP<-EduExpGDP+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
EduExpGDP

LeftIneq<-ggplot(fitsDiverse, aes(y = (as.year), x = educexpgdp_mean)) +
  stat_halfeye(aes(x=b_educexpgdp),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from Left-of-Center governments")+
  geom_vline(xintercept = 0, linetype = "dashed") 
LeftIneq<-LeftIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
LeftIneq

TertiaryIneq<-ggplot(fitsDiverse, aes(y = (as.year), x = highlyeducatedlabor_mean)) +
  stat_halfeye(aes(x=b_highlyeducatedlabor),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from Educational Expenditure as a share of GDP") +
  geom_vline(xintercept = 0, linetype = "dashed") 
TertiaryIneq<-TertiaryIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
TertiaryIneq

WageRegionallIneq<-ggplot(fitsDiverse, aes(y = (as.year), x = wage.arrangementregional_mean)) +
  stat_halfeye(aes(x=b_wage.arrangementRegional),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from regional wage arrangements (vs. national)")+
  geom_vline(xintercept = 0, linetype = "dashed") 
WageRegionallIneq<-WageRegionallIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
WageRegionallIneq

WageSegmlIneq<-ggplot(fitsDiverse, aes(y = (as.year), x = wage.arrangementsegmented_mean)) +
  stat_halfeye(aes(x=b_wage.arrangementSegmented),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from segmented wage arrangements (vs. national)")+
  geom_vline(xintercept = 0, linetype = "dashed") 
WageSegmlIneq<-WageSegmlIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
WageSegmlIneq

LogMinWageIneq<-ggplot(fitsDiverse, aes(y = (as.year), x = logminwage_mean)) +
  stat_halfeye(aes(x=b_logminwage),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from log(minimum wage)")+
  geom_vline(xintercept = 0, linetype = "dashed") 
LogMinWageIneq<-LogMinWageIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
LogMinWageIneq

EmployIneq<-ggplot(fitsDiverse, aes(y = (as.year), x = employment_mean)) +
  stat_halfeye(aes(x=b_employment),.width = c(.90, .5)) + 
  labs(x="Effect on Inequality from Employment rate")+
  geom_vline(xintercept = 0, linetype = "dashed") 
EmployIneq<-EmployIneq+ coord_flip()+ylab(NULL)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
EmployIneq
library(gridExtra)
grid.arrange(FractionalDiversity,SocExpGDP,EduExpGDP,LeftIneq,TertiaryIneq,
             WageRegionallIneq,WageSegmlIneq,LogMinWageIneq,EmployIneq,ncol=3)

grid.arrange(Heterogeneity,FractionalDiversity,BlackIneq, 
             MulattoIneq, MestizoIneq, AmerindianIneq, 
             AsianIneq, CreolesIneq,ncol=4)


#Strcutrual Break table of analysis

LatAm1993<-subset(LatAm2014,year<2003)
sd(LatAm1993$gini100)
LatAm2003<-subset(LatAm2014,year>=2003)
sd(LatAm2003$gini100)
library(lme4)
summary(lm(gini100 ~ EthnicFractionalizationTS+socexpgdp+educexpgdp+left+highlyeducatedlabor+logminwage+employment+year,data=LatAm1993))
summary(lm(gini100 ~ EthnicFractionalizationTS+socexpgdp+educexpgdp+left+highlyeducatedlabor+logminwage+employment+year,data=LatAm2003))


PooledFractional93<-brm(data = LatAm1993, family = gaussian,
                        gini100 ~ EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,
                        prior = c(prior(normal(52.89, 4.6), class = Intercept),
                                  prior(normal(0, 2), class = b),
                                  prior(cauchy(0,5), class = sigma)),
                        control=list(adapt_delta=0.99, max_treedepth=20),
                        iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)

PooledFractional03<-brm(data = LatAm2003, family = gaussian,
                        gini100 ~ EthnicFractionalizationTS+socexpgdp+educexpgdp+left+wage.arrangement+highlyeducatedlabor+logminwage+employment,
                        prior = c(prior(normal(49.87, 4.67), class = Intercept),
                                  prior(normal(0, 2), class = b),
                                  prior(cauchy(0,5), class = sigma)),
                        control=list(adapt_delta=0.99, max_treedepth=20),
                        iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)
PooledFractional93
PooledFractional03
tab_model(PooledFractional93,PooledFractional03)
summary(PooledFractional93)
summary(PooledFractional03)

summary(PooledFractional2)
3.91^2

#Map

LatAmMapGini<-read.xlsx("LatAmMap.xlsx", "Sheet1")
head(LatAmMapGini)
#write.xlsx(LatAmMap, "LatAmMap3.xlsx")
#write.xlsx(InequalityLatAm, "LatAmData.xlsx")
LatAm.gini<-ggplot(LatAmMapGini)
#Now for all countries... ugh
#LatAmMap$gini[LatAmMap$gini==Argentina]<-length(grep("Argentina",InequalityLatAm$gini))
#LatAm.gini<-ggplot(LatAm)
latlimits<-c(-60,35)
longlimits<-c(240,335)
LatAm.gini14<-LatAm.gini+geom_polygon(data=LatAmMapGini, aes(x=long, y=lat, group
                                                             = group, fill=LatAmMapGini$gini14), colour="darkgrey") + scale_fill_continuous(low = "white", high = "red", guide="colorbar",limits=c(0.37, 0.602))
LatAm.gini14 #see the map in all its glory

BetterVersion14<-LatAm.gini14+ theme(panel.background = element_rect(fill = "lightsteelblue2", colour = "grey"), panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_blank()) + labs(fill = "Gini", title =
                                                                                                                                                                                                                  "Inequality in selected Latin American countries, 2014", x="", y="")
BetterVersion14
Gini14 <- BetterVersion14 + scale_y_continuous(limits=range(LatAmMapGini$lat)) +
  scale_x_continuous(limits=range(LatAmMapGini$long)) + theme(panel.border =
                                                                element_blank()) + coord_cartesian(xlim = longlimits, ylim = latlimits)
Gini14

#now we replicate for gini95, ginichange, and gini percent change

LatAm.gini93<-LatAm.gini+geom_polygon(data=LatAmMapGini, aes(x=long, y=lat, group
                                                             = group, fill=LatAmMapGini$gini93), colour="darkgrey") + scale_fill_continuous(low = "white", high = "red", guide="colorbar",limits=c(0.37, 0.602))
LatAm.gini93 #see the map in all its glory

BetterVersion93<-LatAm.gini93+ theme(panel.background = element_rect(fill = "lightsteelblue2", colour = "grey"), panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_blank()) + labs(fill = "Gini", title =
                                                                                                                                                                                                                  "Inequality in selected Latin American countries, 1993", x="", y="")
BetterVersion93
Gini93 <- BetterVersion93 + scale_y_continuous(limits=range(LatAmMapGini$lat)) +
  scale_x_continuous(limits=range(LatAmMapGini$long)) + theme(panel.border =
                                                                element_blank()) + coord_cartesian(xlim = longlimits, ylim = latlimits)
Gini93

grid.arrange(Gini93,Gini14)
summary(LatAmMapGini$ginichange)

LatAm.ginichange<-LatAm.gini+geom_polygon(data=LatAmMapGini, aes(x=long, y=lat, group
                                                                 = group, fill=LatAmMapGini$ginichange), colour="darkgrey") + scale_fill_continuous(low = "green", high = "red", guide="colorbar",limits=c(-0.20, 0.0376))
LatAm.ginichange #see the map in all its glory

BetterVersionchange<-LatAm.ginichange+ 
  theme(panel.background = element_rect(fill = "lightsteelblue2", colour = "grey"), 
        panel.grid.major = element_line(colour = "grey90"), 
        panel.grid.minor = element_blank()) + labs(fill = "Income Inequality (Gini)")
BetterVersionchange
Ginichange <- BetterVersionchange + scale_y_continuous(limits=range(LatAmMapGini$lat)) +
  scale_x_continuous(limits=range(LatAmMapGini$long)) + theme(panel.border =
                                                                element_blank()) + coord_cartesian(xlim = longlimits, ylim = latlimits)
Ginichange


LatAm.ginipctchange<-LatAm.gini+geom_polygon(data=LatAmMapGini, aes(x=long, y=lat, group
                                                                    = group, fill=LatAmMapGini$ginipctchange), colour="darkgrey") + scale_fill_continuous(low = "green", high = "red", guide="colorbar")
LatAm.ginipctchange #see the map in all its glory

BetterVersionpctchange<-LatAm.ginipctchange+ theme(panel.background = element_rect(fill = "lightsteelblue2", colour = "grey"), panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_blank()) + labs(fill = "Change (%)", title =
                                                                                                                                                                                                                                "Change in income inequality 1993-2014 (%)", x="", y="")
BetterVersionpctchange
Ginipctchange <- BetterVersionpctchange + scale_y_continuous(limits=range(LatAmMapGini$lat)) +
  scale_x_continuous(limits=range(LatAmMapGini$long)) + theme(panel.border =
                                                                element_blank()) + coord_cartesian(xlim = longlimits, ylim = latlimits)
Ginipctchange

summary(LatAmMapGini$afrodescendants)
LatAm.giniblack<-LatAm.gini+geom_polygon(data=LatAmMapGini, aes(x=long, y=lat, group
                                                                = group, fill=LatAmMapGini$afrodescendants), colour="darkgrey") + scale_fill_continuous(low = "white", high = "darkorange4", guide="colorbar",limits=c(0, 85))
LatAm.giniblack #see the map in all its glory

BetterVersionblack<-LatAm.giniblack+ theme(panel.background = element_rect(fill = "lightsteelblue2", colour = "grey"), panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_blank()) + labs(fill = "%", title =
                                                                                                                                                                                                                        "Afrodescendant share of the population (%)", x="", y="")
BetterVersionblack
Giniblack <- BetterVersionblack + scale_y_continuous(limits=range(LatAmMapGini$lat)) +
  scale_x_continuous(limits=range(LatAmMapGini$long)) + theme(panel.border =
                                                                element_blank()) + coord_cartesian(xlim = longlimits, ylim = latlimits)
Giniblack

LatAm.gininw<-LatAm.gini+geom_polygon(data=LatAmMapGini, aes(x=long, y=lat, group
                                                             = group, fill=LatAmMapGini$nonwhite), colour="darkgrey") + scale_fill_continuous(low = "white", high = "darkgoldenrod4", guide="colorbar",limits=c(0, 100))
LatAm.gininw #see the map in all its glory

BetterVersionnw<-LatAm.gininw+ theme(panel.background = element_rect(fill = "lightsteelblue2", colour = "grey"), panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_blank()) + labs(fill = "%", title =
                                                                                                                                                                                                                  "Nonwhites as a share of the population (%)", x="", y="")
BetterVersionnw
Gininw <- BetterVersionnw + scale_y_continuous(limits=range(LatAmMapGini$lat)) +
  scale_x_continuous(limits=range(LatAmMapGini$long)) + theme(panel.border =
                                                                element_blank()) + coord_cartesian(xlim = longlimits, ylim = latlimits)
Gininw

summary(LatAmMapGini$fractional93)


LatAm.ginifract<-LatAm.gini+geom_polygon(data=LatAmMapGini, aes(x=long, y=lat, group= group, fill=LatAmMapGini$fractional93), colour="darkgrey") + scale_fill_continuous(low = "white", high = "darkorchid4", guide="colorbar",limits=c(0, 0.7))
LatAm.ginifract #see the map in all its glory

BetterVersionfra<-LatAm.ginifract+ theme(panel.background = element_rect(fill = "lightsteelblue2", colour = "grey"), panel.grid.major = element_line(colour = "grey90"), panel.grid.minor = element_blank()) + labs(fill = "Fractionalization", title =
                                                                                                                                                                                                                      "Ethnoracial Fractionalization in 1993", x="", y="")
BetterVersionfra
Ginifra <- BetterVersionfra + scale_y_continuous(limits=range(LatAmMapGini$lat)) +
  scale_x_continuous(limits=range(LatAmMapGini$long)) + theme(panel.border =
                                                                element_blank()) + coord_cartesian(xlim = longlimits, ylim = latlimits)
Ginifra
