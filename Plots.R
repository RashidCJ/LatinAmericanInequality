setwd("/N/slate/rmarcano/LatinAmericapaper")
#load("/N/slate/rmarcano/LatinAmericapaper/IterationsWithHighlyEducAlternateLeftsPresidentsOnly.RData")
load("AbridgedData.RData")
library(sjPlot)
library(grid)
library(gridExtra)
library(tidyverse)
library(tidybayes)
library(brms)
library(readxl)
set.seed(42)
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
LatAm2014$gini100<-gini_equivalized*100







plot1<-plot_model(PooledRace,type="pred",
                  terms = "PresidentPartyLeftRightIndex_mean",
                  title="",
                  axis.title =c("Racial groups","Gini Income Inequality"),
                  axis.lim = c(46,54))
plot1


plot2<-plot_model(PooledDiversity,type="pred",
                  terms = "PresidentPartyLeftRightIndex_mean",
                  title="",
                  axis.title =c("Static Heterogeneity",""),
                  axis.lim = c(46,54))
plot3<-plot_model(PooledFractional2,type="pred",
                  terms = "PresidentPartyLeftRightIndex_mean",
                  title="",
                  axis.title =c("Fractionalization",""),
                  axis.lim = c(46,54))

grid.arrange(plot1,plot2,plot3,nrow=1,
             bottom=textGrob("Presidential ideology"),
             top=textGrob("Predicted values for Gini Income Inequality in pooled models",gp=gpar(fontface="bold",cex=1.2)))
# install.packages("devtools")

brmstools::forest(RandomSlopes15)
plot_model(RandomSlopes15,type="re",grid=F)
# Country-specific effects are deviations + average
out_r <- spread_draws(RandomSlopes15, r_country[country,PresidentPartyLeftRightIndex_mean])
# Average effect
out_f <- spread_draws(RandomSlopes15, r_country[country,PresidentPartyLeftRightIndex_mean]) %>% 
  mutate(country = "Average")
# Combine average and study-specific effects' data frames
out_all <- bind_rows(out_r, out_f) %>% 
  ungroup() %>%
  # Ensure that Average effect is on the bottom of the forest plot
  mutate(country = fct_relevel(country, "Average"))
# Data frame of summary numbers
out_all_sum <- group_by(out_all, country) %>% 
  mean_qi(r_country)
#> Warning: unnest() has a new interface. See ?unnest for details.
#> Try `cols = c(.lower, .upper)`, with `mutate()` needed
# Draw plot
plot5<-out_all %>%   
  ggplot(aes(r_country, country)) +
  geom_density_ridges(
    rel_min_height = 0.01, 
    col = NA,
    scale = 1
  ) +
  geom_pointintervalh(
    data = out_all_sum, size = 1
  ) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    # Use glue package to combine strings
    aes(label = glue::glue("{r_country} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward"
  )
#> Picking joint bandwidth of 0.131
plot5+ylab("")+xlab("Presidential ideology effect on inequality")

# new attempt

RandomSlopes15<-brm(data = LatAm2014, family = gaussian,
                    gini100 ~ EthnicFractionalizationTS+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment+year+(1+PresidentPartyLeftRightIndex_mean|country),
                    prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                              prior(normal(0, 2), class = b),
                              prior(cauchy(0,2), class = sigma),
                              prior(lkj(2), class = cor)),
                    control=list(adapt_delta=0.999, max_treedepth=30),
                    iter = 100000, warmup = 20000, chains = 10, cores = 10,seed = 5)

PooledFractional2<-brm(data = LatAm2014, family = gaussian,
                       gini100 ~ EthnicFractionalizationTS+socexpgdp+educexpgdp+PresidentPartyLeftRightIndex_mean+wage.arrangement+highlyeducatedlabor+logminwage+employment,
                       prior = c(prior(normal(49.43, 5.018121), class = Intercept),
                                 prior(normal(0, 2), class = b),
                                 prior(cauchy(0,5), class = sigma)),
                       control=list(adapt_delta=0.99, max_treedepth=20),
                       iter = 100000, warmup = 10000, chains = 10, cores = 10,seed = 5)
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
save.image("AbridgedData.RData")


library(marginaleffects)
pred <- predictions(RandomSlopes15,
                    newdata = datagrid(country = LatAm2014$country,
                                       PresidentPartyLeftRightIndex_mean = c(0,6))) %>%
  posteriordraws()
pred
predictions<-pred %>% 
  group_by(country,PresidentPartyLeftRightIndex_mean) %>%
  summarise(medianGini=median(draw),n=n(),
            medianlow=median(conf.low),
            medianhigh=median(conf.high))

View(predictions)

write.table(predictions, file="PredictedInequalityIdeology.txt",sep = ",",quote = F,row.names = F)

plot6<-ggplot(pred, aes(x = draw, y = country , fill = factor(PresidentPartyLeftRightIndex_mean))) +
  stat_halfeye(slab_alpha = .5) +
  labs(x = "Gini Income Inequality (predicted)",
       y = "Posterior density",
       fill = "President party ideology")
plot6+scale_color_discrete(labels=c("Left", "Right"))+theme(legend.position = "bottom")
TriPartisan<-plot6+scale_color_discrete(labels=c("Left","Center", "Right"))+theme(legend.position = "bottom")


pred2 <- predictions(RandomSlopes15,
                    newdata = datagrid(PresidentPartyLeftRightIndex_mean = seq(0,6,by=0.1))) %>%
  posteriordraws()

ggplot(pred2, aes(x = PresidentPartyLeftRightIndex_mean, y = draw )) +
  stat_lineribbon() +
  scale_fill_brewer(palette="Reds") +
  labs(x = "Presidential Party Ideology from far left to far right",
       y = "Gini Income Inequality (predicted)",
       fill = "")

plot7<-plot_model(PooledFractional93,type="pred",
                  terms = "PresidentPartyLeftRightIndex_mean",
                  title="",
                  axis.title =c("1993-2002","Gini Income Inequality"),
                  axis.lim = c(45,55))
plot7

plot8<-plot_model(PooledFractional03,type="pred",
                  terms = "PresidentPartyLeftRightIndex_mean",
                  title="",
                  axis.title =c("2003-2014",""),
                  axis.lim = c(45,55))
plot8
plot9<-plot_model(PooledFractional2,type="pred",
                  terms = "PresidentPartyLeftRightIndex_mean",
                  title="",
                  axis.title =c("1993-2014",""),
                  axis.lim = c(45,55))

grid.arrange(plot7,plot8,plot9,nrow=1,
             bottom=textGrob("Presidential ideology"),
             top=textGrob("Predicted values for Gini Income Inequality, structural break and original pooled models",gp=gpar(fontface="bold",cex=1.2)))




plot10<-plot_model(PooledFractional93,type="pred",
                   terms = c("PresidentPartyLeftRightIndex_mean","EthnicFractionalizationTS [0,1]"),
                   title="",
                   axis.title =c("1993-2002","Gini Income Inequality"),
                   axis.lim = c(40,60),
                   show.legend = F)
plot11<-plot_model(PooledFractional03,type="pred",
                   terms = c("PresidentPartyLeftRightIndex_mean","EthnicFractionalizationTS [0,1]"),
                   title="",
                   axis.title =c("2003-2014",""),
                   axis.lim = c(40,60),
                   show.legend = F)

legendmine<-plot_model(PooledFractional03,type="pred",
          terms = c("PresidentPartyLeftRightIndex_mean","EthnicFractionalizationTS [0,1]"),
          title="",
          axis.title =c("2003-2014",""),
          axis.lim = c(40,60))+ggplot2::scale_color_discrete(labels=c("Homogeneous society", "Highly and equally fractionalized society"))+theme(legend.position = "right")

extract_legend<-function(ggp){
  step1<-ggplot_gtable(ggplot_build(ggp))
  step2<-which(sapply(step1$grobs,function(x) x$name)=="guide-box")
  step3<-step1$grobs[[step2]]
}

sharedlegend<-extract_legend(legendmine)

grid.arrange(plot10,plot11,sharedlegend,ncol=3,widths=c(1.5,1.5,0.5),heights=c(2.5),
             bottom=textGrob("Ruling party (presidency) ideology")
            # top=textGrob("Predicted values for Gini Income Inequality given societal diversity, structural break pooled models",gp=gpar(fontface="bold",cex=1.2))
             )

                                                                                                                                                                                