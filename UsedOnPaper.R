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
library(lme4)
library(sjmisc)
labels(LatAm2014)

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
library(lme4)
theme_set(theme_tidybayes() + panel_border())
set.seed(42)


# Initial view, summary ---------------------------------------------------

summary(LatAm2014$gini100)


# Correlogram plot --------------------------------------------------------
library(GGally)
library(ggthemes)
names(LatAm2014)
LatAmcorr <- LatAm2014[, c(44, 49, 56,
                           18,19,20,
                           24, 52,5)]
names(LatAmcorr) <- c("Income Inequality"  ,"log Min. Wage","Presidential Ideology",
                      "Educ. Expenditure","Labor Education","Employment Rate",
                      "Social Expenditure", "Ethnic Fractionalization", "Wage institution")
LatAmcorr$`Wage institution`<-as.factor(LatAmcorr$`Wage institution`)
obj<-ggpairs(LatAmcorr, 
             mapping = ggplot2::aes(color=`Wage institution`),
             lower = list(continuous="smooth"),
             upper = list(combo="box"))
obj

objbland<-ggpairs(LatAmcorr, 
             lower = list(continuous="smooth"),
             upper = list(combo="box"))
objbland

LatAmcorrrac <- LatAm2014[, c(44, 26:32)]
names(LatAmcorrrac[1]) <- c("Income Inequality")
ggpairs(LatAmcorrrac, 
        lower = list(continuous="smooth"),
        upper = list(combo="box"))

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

PooledFractional<-brm(data = LatAm2014, family = gaussian,
                      gini100 ~ EthnicFractLag1+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment,
                      prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                prior(normal(0, 2), class = b),
                                prior(cauchy(0,5), class = sigma)),
                      control=list(adapt_delta=0.99, max_treedepth=20),
                      iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)
PooledFractional
PooledFractional.lag<-brm(data = LatAm2014, family = gaussian,
                          gini100 ~ EthnicFractLag1+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_meanlag+wage.arrangement+highlyeducatedlabor+logminwage+employment,
                          prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                    prior(normal(0, 2), class = b),
                                    prior(cauchy(0,5), class = sigma)),
                          control=list(adapt_delta=0.99, max_treedepth=20),
                          iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)

PooledFractional2<-brm(data = LatAm2014, family = gaussian,
                       gini100 ~ EthnicFractionalizationTS+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment,
                       prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                 prior(normal(0, 2), class = b),
                                 prior(cauchy(0,5), class = sigma)),
                       control=list(adapt_delta=0.99, max_treedepth=20),
                       iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)


PooledFractional2.lag<-brm(data = LatAm2014, family = gaussian,
                           gini100 ~ EthnicFractionalizationTS+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_meanlag+wage.arrangement+highlyeducatedlabor+logminwage+employment,
                           prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                     prior(normal(0, 2), class = b),
                                     prior(cauchy(0,5), class = sigma)),
                           control=list(adapt_delta=0.99, max_treedepth=20),
                           iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)


# Extraction of Models for Table 1 ----------------------------------------


summary(empty.model.Bayesian,robust = TRUE)
summary(empty.model.Bayesian,robust = TRUE)
round(fixef(empty.model.Bayesian,robust = TRUE),digits = 2)
round(fixef(Uncond.Growth.Model.Bayesian,robust = TRUE),digits = 2)

# Extraction of Models for Table 2 ----------------------------------------


round(fixef(PooledRace,robust = TRUE),digits = 2)
round(fixef(PooledDiversity,robust = TRUE),digits = 2)
round(fixef(PooledFractional2,robust = TRUE),digits = 2)

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

# Plot for Pooled Models --------------------------------------------------


models<-plot_models(PooledRace,PooledDiversity,PooledFractional2,
                    legend.title = "Model",title = "Pooled models",
                    rm.terms ="b_Intercept", grid=F, show.values=F, vline.color="black",
                    type="est",ci.style="bar",
                    axis.labels = rev(c(
                      "Black share of pop.", "Mulatto share of pop.", "Mestizo share of pop.", 
                      "Amerindian share of pop.", "Asian share of pop.", "Creole and Garifuna  share of pop.",
                      "Social expenditure / GDP", "Educational expenditure / GDP", "Presidential ideology",
                      "Regional wage setting", "Segmented wage setting", "Highly educated labor", "Log of min. wage",
                      "Employment rate", "Static Racial Heterogeneity", "Ethnic Fractionalization"
                    )), m.labels = c("Racial groups", "Static heterogeneity", "Fractionalization"))
models

#models+stat_density_ridges(data=c())


# Random intercept models -------------------------------------------------

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

# Random Slope Models -----------------------------------------------------

# Static Heterogeneity Random Slope Models -----------------------------------------------------

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

# Racial groups Random Slope Models -----------------------------------------------------

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

# Static Fractionalization Random Slope Models -----------------------------------------------------


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

# Fractionalization lagged-1 Random Slope Models -----------------------------------------------------


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

# Ethnic Fractionalization Random Slope Models, table 3 -----------------------------------------------------

plot_model(RandomSlopes13)
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
modelsFract<-plot_models(RandomSlopes13,RandomSlopes14,RandomSlopes15, legend.title = "Model",title = "Fractionalization variable models (intercept+ideology|country)",
                         rm.terms =c("b_Intercept","b_year1994","b_year1995","b_year1996","b_year1997","b_year1998",
                                     "b_year1999","b_year2000","b_year2001","b_year2002","b_year2003","b_year2004",
                                     "b_year2005","b_year2006","b_year2007","b_year2008","b_year2009","b_year2010",
                                     "b_year2011","b_year2012","b_year2013","b_year2014"), 
                         vline.color="black",
                         axis.labels = rev(c(
                           "Ethnic Fractionalization", "Social expenditure / GDP", 
                           "Educational expenditure / GDP", "Presidential ideology",
                           "Regional wage setting", "Segmented wage setting", 
                           "Highly educated labor", "Log of min. wage","Employment rate")),
                         m.labels = c("Model 4", "Model 5", "Model 6"))
modelsFract


# Extraction of Models for Table 3 ----------------------------------------

round(fixef(RandomSlopes13,robust = TRUE),digits = 2)
ranef(RandomSlopes13,robust = TRUE)
RandomSlopes13
round(fixef(RandomSlopes14,robust = TRUE),digits = 2)
round(fixef(RandomSlopes15,robust = TRUE),digits = 2)

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
tab_model(RandomSlopes13,RandomSlopes14,RandomSlopes15)
RandomSlopes13
performance::variance_decomposition(RandomSlopes13)
posterior_summary(as.data.frame(RandomSlopes13)$sigma)^2

RandomSlopes14
performance::variance_decomposition(RandomSlopes14)
posterior_summary(as.data.frame(RandomSlopes14)$sigma)^2

RandomSlopes15
performance::variance_decomposition(RandomSlopes15)
posterior_summary(as.data.frame(RandomSlopes15)$sigma)^2

ranef(RandomSlopes13,robust=TRUE)



sd(ranef(RandomSlopes13)[[2]])
summary(bayes_R2(RandomSlopes13))
summary(bayes_R2_res(RandomSlopes13))
summary(bayes_R2(RandomSlopes14))
summary(bayes_R2_res(RandomSlopes14))
summary(bayes_R2(RandomSlopes15))
summary(bayes_R2_res(RandomSlopes15))

# Structural break table 4 ------------------------------------------------


LatAm1993<-subset(LatAm2014,year<2003)
median(LatAm1993$gini100)
sd(LatAm1993$gini100)
LatAm2003<-subset(LatAm2014,year>=2003)
median(LatAm2003$gini100)
sd(LatAm2003$gini100)
library(lme4)
summary(lm(gini100 ~ EthnicFractionalizationTS+socexpgdp+educexpgdp+left+highlyeducatedlabor+logminwage+employment+year,data=LatAm1993))
summary(lm(gini100 ~ EthnicFractionalizationTS+socexpgdp+educexpgdp+left+highlyeducatedlabor+logminwage+employment+year,data=LatAm2003))


PooledFractional93<-brm(data = LatAm1993, family = gaussian,
                        gini100 ~ EthnicFractionalizationTS+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment,
                        prior = c(prior(normal(51.71068, 4.901302), class = Intercept),
                                  prior(normal(0, 2), class = b),
                                  prior(cauchy(0,5), class = sigma)),
                        control=list(adapt_delta=0.99, max_treedepth=20),
                        iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

PooledFractional03<-brm(data = LatAm2003, family = gaussian,
                        gini100 ~ EthnicFractionalizationTS+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment,
                        prior = c(prior(normal(47.84845, 4.682983), class = Intercept),
                                  prior(normal(0, 2), class = b),
                                  prior(cauchy(0,5), class = sigma)),
                        control=list(adapt_delta=0.99, max_treedepth=20),
                        iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)
PooledFractional93
PooledFractional03
tab_model(PooledFractional93,PooledFractional03,PooledFractional2,robust = T)
save.image("IterationsWithHighlyEducAlternateLeftsPresidentsOnly.RData")
