rm(list = ls())

# Initial setup -----------------------------------------------------------

library(readr)
library(tidyverse)
if(osVersion=="Windows 10 x64 (build 19044)") {
  setwd("C:/Users/Rashid/OneDrive - Indiana University/General/Doctoral Studies/Research LatAm Inequality/For Publication/Latin American Inequality final/ThinDrive")
  print("Working directory set in accordance to Windows settings. Directorio seteado en config. Windows.")
} else {
  setwd("~/Documents/OneDrive - Indiana University/General/Doctoral Studies/Research LatAm Inequality/For Publication/Latin American Inequality final/ThinDrive")
  print("Working directory set in accordance to Mac/Linux settings. Directorio seteado en config. Mac.")
}

# Load file for analysis --------------------------------------

library(strucchange)
library(readxl)
LatAm2014 <- read_excel("LatAm2014.xlsx", col_types = c("numeric", "text", "numeric", 
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





#LatAm2014$year<-format(LatAm2014$year, format="%Y")
attach(LatAm2014)
#detach(LatAm2014)
LatAm2014$gini100<-gini_equivalized*100

set.seed(42)

# Separate Panel Data into individual country time series ---------------------

country_splits <- split(LatAm2014, LatAm2014$country)    # Split data frame in list
country_splits                # Print list
split_names<-unique(LatAm2014$country)
for (i in 1:length(country_splits)) {        # Run for-loop
  assign(split_names[i], country_splits[[i]])
}
# Conducting Chow Test ---------------------
## Argentina  --------
ArgentinaGini<-ts(Argentina$gini100,start=c(1993),end=c(2014),frequency=1)
ArgentinaIdeo<-ts(Argentina$PresidentPartyLeftRightIndex_mean,start=c(1993),end=c(2014),frequency=1)

ArgentinaTest<-cbind(ArgentinaGini,ArgentinaIdeo)

## initialize the monitoring with the formula interface
Argentina93 <- window(ArgentinaTest,start=c(1993),end=c(2002))

Arg.mefp <- mefp(ArgentinaGini ~ ArgentinaIdeo, type="ME",
                data=Argentina93, alpha=0.05)
plot(Arg.mefp)
## monitor the new observations for the year 1972
Argentina93 <-window(ArgentinaTest,start=c(1993),end=c(2014))
Arg.mefp <- monitor(Arg.mefp)
Arg.mefp

plot(Arg.mefp, functional = NULL)

sctest(ArgentinaGini ~ ArgentinaIdeo,
       data=Argentina93)
?sctest
?Fstats
bpArg<- Fstats(ArgentinaGini ~ ArgentinaIdeo, data = ArgentinaTest)
bpArg$breakpoint
bpArg$par
plot(bpArg)
sctest(bpArg)
#Break detected at observation # 12 

## Bolivia  --------
BoliviaGini<-ts(Bolivia$gini100,start=c(1993),end=c(2014),frequency=1)
BoliviaIdeo<-ts(Bolivia$PresidentPartyLeftRightIndex_mean,start=c(1993),end=c(2014),frequency=1)

BoliviaTest<-cbind(BoliviaGini,BoliviaIdeo)

## initialize the monitoring with the formula interface
Bolivia93 <- window(BoliviaTest,start=c(1993),end=c(2002))

Bol.mefp <- mefp(BoliviaGini ~ BoliviaIdeo, type="ME",
                 data=Bolivia93, alpha=0.05)
plot(Bol.mefp)
## monitor the new observations for the year 1972
Bolivia93 <-window(BoliviaTest,start=c(1993),end=c(2014))
Bol.mefp <- monitor(Bol.mefp)
Bol.mefp

plot(Bol.mefp, functional = NULL)

bpBol<- Fstats(BoliviaGini ~ BoliviaIdeo, data = BoliviaTest)
bpBol$breakpoint
bpBol$par
plot(bpBol)
sctest(bpBol)
#Break detected at observation # 16


## Brazil  --------
BrazilGini<-ts(Brazil$gini100,start=c(1993),end=c(2014),frequency=1)
BrazilIdeo<-ts(Brazil$PresidentPartyLeftRightIndex_mean,start=c(1993),end=c(2014),frequency=1)

BrazilTest<-cbind(BrazilGini,BrazilIdeo)

## initialize the monitoring with the formula interface
Brazil93 <- window(BrazilTest,start=c(1993),end=c(2002))

Brazil.mefp <- mefp(BrazilGini ~ BrazilIdeo, type="ME", rescale=TRUE,
                 data=Brazil93, alpha=0.05)
plot(Brazil.mefp)
## monitor the new observations for the year 1972
Brazil93 <-window(BrazilTest,start=c(1993),end=c(2014))
Brazil.mefp <- monitor(Brazil.mefp)
Brazil.mefp

plot(Brazil.mefp, functional = NULL)
#Break detected at observation # 13

bpBra<- Fstats(BrazilGini ~ BrazilIdeo, data = BrazilTest)
bpBra$breakpoint
plot(bpBra)
sctest(bpBra)
## Chile  --------
ChileGini<-ts(Chile$gini100,start=c(1993),end=c(2014),frequency=1)
ChileIdeo<-ts(Chile$PresidentPartyLeftRightIndex_mean,start=c(1993),end=c(2014),frequency=1)

ChileTest<-cbind(ChileGini,ChileIdeo)

## initialize the monitoring with the formula interface
Chile93 <- window(ChileTest,start=c(1993),end=c(2002))

Chile.mefp <- mefp(ChileGini ~ ChileIdeo, type="ME", rescale=TRUE,
                    data=Chile93, alpha=0.05)
plot(Chile.mefp)
## monitor the new observations for the year 1972
Chile93 <-window(ChileTest,start=c(1993),end=c(2014))
Chile.mefp <- monitor(Chile.mefp)
Chile.mefp

plot(Chile.mefp, functional = NULL)
#Break detected at observation # 14

bpChile<- Fstats(ChileGini ~ ChileIdeo, data = ChileTest)
bpChile$breakpoint
plot(bpChile)
sctest(bpChile)


## Colombia  --------

ColombiaGini<-ts(Colombia$gini100,start=c(1993),end=c(2014),frequency=1)
ColombiaIdeo<-ts(Colombia$PresidentPartyLeftRightIndex_mean,start=c(1993),end=c(2014),frequency=1)

ColombiaTest<-cbind(ColombiaGini,ColombiaIdeo)

## initialize the monitoring with the formula interface
Colombia93 <- window(ColombiaTest,start=c(1993),end=c(2002))

Colombia.mefp <- mefp(ColombiaGini ~ ColombiaIdeo, type="ME",
                    data=Colombia93, alpha=0.05)
plot(Colombia.mefp)
## monitor the new observations for the year 1972
Colombia93 <-window(ColombiaTest,start=c(1993),end=c(2014))
Colombia.mefp <- monitor(Colombia.mefp)
Colombia.mefp

plot(Colombia.mefp, functional = NULL)
#Break detected at observation # 16
bpColombia<- Fstats(ColombiaGini ~ ColombiaIdeo, data = ColombiaTest)
bpColombia$breakpoint
plot(bpColombia)
sctest(bpColombia)
## Costa Rica  --------
CostaRica<-`Costa Rica`
CostaRicaGini<-ts(CostaRica$gini100,start=c(1993),end=c(2014),frequency=1)
CostaRicaIdeo<-ts(CostaRica$PresidentPartyLeftRightIndex_mean,start=c(1993),end=c(2014),frequency=1)

CostaRicaTest<-cbind(CostaRicaGini,CostaRicaIdeo)

## initialize the monitoring with the formula interface
CostaRica93 <- window(CostaRicaTest,start=c(1993),end=c(2002))

CostaRica.mefp <- mefp(CostaRicaGini ~ CostaRicaIdeo, type="OLS-CUSUM",
                    data=CostaRica93, alpha=0.05)
plot(CostaRica.mefp)
## monitor the new observations for the year 1972
CostaRica93 <-window(CostaRicaTest,start=c(1993),end=c(2014))
CostaRica.mefp <- monitor(CostaRica.mefp)
CostaRica.mefp

plot(CostaRica.mefp, functional = NULL)
#Break detected at observation # 20
bpCostaRica<- Fstats(CostaRicaGini ~ CostaRicaIdeo, data = CostaRicaTest)
bpCostaRica$breakpoint
plot(bpCostaRica)
sctest(bpCostaRica)
## Dominican Republic  --------

DominicanRep<-`Dominican Republic`

DominicanRepGini<-ts(DominicanRep$gini100,start=c(1993),end=c(2014),frequency=1)
DominicanRepIdeo<-ts(DominicanRep$PresidentPartyLeftRightIndex_mean,start=c(1993),end=c(2014),frequency=1)

DominicanRepTest<-cbind(DominicanRepGini,DominicanRepIdeo)

## initialize the monitoring with the formula interface
DominicanRep93 <- window(DominicanRepTest,start=c(1993),end=c(2002))

DominicanRep.mefp <- mefp(DominicanRepGini ~ DominicanRepIdeo, type="ME",
                    data=DominicanRep93, alpha=0.05)
plot(DominicanRep.mefp)
## monitor the new observations for the year 1972
DominicanRep93 <-window(DominicanRepTest,start=c(1993),end=c(2014))
DominicanRep.mefp <- monitor(DominicanRep.mefp)
DominicanRep.mefp

plot(DominicanRep.mefp, functional = NULL)
#At 21, bp
bpDominicanRep<- Fstats(DominicanRepGini ~ DominicanRepIdeo, data = DominicanRepTest)
bpDominicanRep$breakpoint
plot(bpDominicanRep)
sctest(bpDominicanRep)
## Ecuador  --------

EcuadorGini<-ts(Ecuador$gini100,start=c(1993),end=c(2014),frequency=1)
EcuadorIdeo<-ts(Ecuador$PresidentPartyLeftRightIndex_mean,start=c(1993),end=c(2014),frequency=1)

EcuadorTest<-cbind(EcuadorGini,EcuadorIdeo)

## initialize the monitoring with the formula interface
Ecuador93 <- window(EcuadorTest,start=c(1993),end=c(2002))

Ecuador.mefp <- mefp(EcuadorGini ~ EcuadorIdeo, type="ME",
                    data=Ecuador93, alpha=0.05)
plot(Ecuador.mefp)
## monitor the new observations for the year 1972
Ecuador93 <-window(EcuadorTest,start=c(1993),end=c(2014))
Ecuador.mefp <- monitor(Ecuador.mefp)
Ecuador.mefp

plot(Ecuador.mefp, functional = NULL)
bpEcuador<- Fstats(EcuadorGini ~ EcuadorIdeo, from=7, data = EcuadorTest)

bpEcuador<- Fstats(EcuadorGini ~ EcuadorIdeo, data = EcuadorTest)
bpEcuador$breakpoint
plot(bpEcuador)
sctest(bpEcuador)
## El Salvador  --------

ElSalvador <- `El Salvador`

ElSalvadorGini<-ts(ElSalvador$gini100,start=c(1993),end=c(2014),frequency=1)
ElSalvadorIdeo<-ts(ElSalvador$PresidentPartyLeftRightIndex_mean,start=c(1993),end=c(2014),frequency=1)

ElSalvadorTest<-cbind(ElSalvadorGini,ElSalvadorIdeo)

## initialize the monitoring with the formula interface
ElSalvador93 <- window(ElSalvadorTest,start=c(1993),end=c(2002))

ElSalvador.mefp <- mefp(ElSalvadorGini ~ ElSalvadorIdeo, type="OLS-CUSUM",
                    data=ElSalvador93, alpha=0.05)
plot(ElSalvador.mefp)
## monitor the new observations for the year 1972
ElSalvador93 <-window(ElSalvadorTest,start=c(1993),end=c(2014))
ElSalvador.mefp <- monitor(ElSalvador.mefp)
ElSalvador.mefp

plot(ElSalvador.mefp, functional = NULL)

#Could not be established
bpElSalvador<- Fstats(ElSalvadorGini ~ ElSalvadorIdeo, data = ElSalvadorTest)
bpElSalvador$breakpoint
plot(bpElSalvador)
sctest(bpElSalvador)
## Guatemala  --------

GuatemalaGini<-ts(Guatemala$gini100,start=c(1993),end=c(2014),frequency=1)
GuatemalaIdeo<-ts(Guatemala$PresidentPartyLeftRightIndex_mean,start=c(1993),end=c(2014),frequency=1)

GuatemalaTest<-cbind(GuatemalaGini,GuatemalaIdeo)

## initialize the monitoring with the formula interface
Guatemala93 <- window(GuatemalaTest,start=c(1993),end=c(2002))

Guatemala.mefp <- mefp(GuatemalaGini ~ GuatemalaIdeo, type="ME",
                    data=Guatemala93, alpha=0.05)
plot(Guatemala.mefp)
## monitor the new observations for the year 1972
Guatemala93 <-window(GuatemalaTest,start=c(1993),end=c(2014))
Guatemala.mefp <- monitor(Guatemala.mefp)
Guatemala.mefp

plot(Guatemala.mefp, functional = NULL)
bpGuatemala<- Fstats(GuatemalaGini ~ GuatemalaIdeo, data = GuatemalaTest)
bpGuatemala$breakpoint
plot(bpGuatemala)
sctest(bpGuatemala)
## Honduras  --------

HondurasGini<-ts(Honduras$gini100,start=c(1993),end=c(2014),frequency=1)
HondurasIdeo<-ts(Honduras$PresidentPartyLeftRightIndex_mean,start=c(1993),end=c(2014),frequency=1)

HondurasTest<-cbind(HondurasGini,HondurasIdeo)

## initialize the monitoring with the formula interface
Honduras93 <- window(HondurasTest,start=c(1993),end=c(2002))

Honduras.mefp <- mefp(HondurasGini ~ HondurasIdeo, type="ME",
                    data=Honduras93, alpha=0.05)
plot(Honduras.mefp)
## monitor the new observations for the year 1972
Honduras93 <-window(HondurasTest,start=c(1993),end=c(2014))
Honduras.mefp <- monitor(Honduras.mefp)
Honduras.mefp
#Break detected at observation # 16 
plot(Honduras.mefp, functional = NULL)
bpHonduras<- Fstats(HondurasGini ~ HondurasIdeo,from=9, data = HondurasTest)
bpHonduras<- Fstats(HondurasGini ~ HondurasIdeo, data = HondurasTest)
bpHonduras$breakpoint
plot(bpHonduras)
sctest(bpHonduras)
## Mexico  --------

MexicoGini<-ts(Mexico$gini100,start=c(1993),end=c(2014),frequency=1)
MexicoIdeo<-ts(Mexico$PresidentPartyLeftRightIndex_mean,start=c(1993),end=c(2014),frequency=1)

MexicoTest<-cbind(MexicoGini,MexicoIdeo)

## initialize the monitoring with the formula interface
Mexico93 <- window(MexicoTest,start=c(1993),end=c(2002))

Mexico.mefp <- mefp(MexicoGini ~ MexicoIdeo, type="ME",
                    data=Mexico93, alpha=0.05)
plot(Mexico.mefp)
## monitor the new observations for the year 1972
Mexico93 <-window(MexicoTest,start=c(1993),end=c(2014))
Mexico.mefp <- monitor(Mexico.mefp)
Mexico.mefp

plot(Mexico.mefp, functional = NULL)
#Break detected at observation # 17 
bpMexico<- Fstats(MexicoGini ~ MexicoIdeo, data = MexicoTest)
bpMexico$breakpoint
plot(bpMexico)
sctest(bpMexico)
## Nicaragua  --------

NicaraguaGini<-ts(Nicaragua$gini100,start=c(1993),end=c(2014),frequency=1)
NicaraguaIdeo<-ts(Nicaragua$PresidentPartyLeftRightIndex_mean,start=c(1993),end=c(2014),frequency=1)

NicaraguaTest<-cbind(NicaraguaGini,NicaraguaIdeo)

## initialize the monitoring with the formula interface
Nicaragua93 <- window(NicaraguaTest,start=c(1993),end=c(2002))

Nicaragua.mefp <- mefp(NicaraguaGini ~ NicaraguaIdeo, type="OLS-CUSUM",
                    data=Nicaragua93, alpha=0.05)
plot(Nicaragua.mefp)
## monitor the new observations for the year 1972
Nicaragua93 <-window(NicaraguaTest,start=c(1993),end=c(2014))
Nicaragua.mefp <- monitor(Nicaragua.mefp)
Nicaragua.mefp

plot(Nicaragua.mefp, functional = NULL)
#No break found in ME form, OLS found it at 13
bpNicaragua<- Fstats(NicaraguaGini ~ NicaraguaIdeo, data = NicaraguaTest)
bpNicaragua$breakpoint
plot(bpNicaragua)
sctest(bpNicaragua)
## Panama  --------

PanamaGini<-ts(Panama$gini100,start=c(1993),end=c(2014),frequency=1)
PanamaIdeo<-ts(Panama$PresidentPartyLeftRightIndex_mean,start=c(1993),end=c(2014),frequency=1)

PanamaTest<-cbind(PanamaGini,PanamaIdeo)

## initialize the monitoring with the formula interface
Panama93 <- window(PanamaTest,start=c(1993),end=c(2002))

Panama.mefp <- mefp(PanamaGini ~ PanamaIdeo, type="ME",
                    data=Panama93, alpha=0.05)
plot(Panama.mefp)
## monitor the new observations for the year 1972
Panama93 <-window(PanamaTest,start=c(1993),end=c(2014))
Panama.mefp <- monitor(Panama.mefp)
Panama.mefp

plot(Panama.mefp, functional = NULL)
#Break detected at observation # 20 
bpPanama<- Fstats(PanamaGini ~ PanamaIdeo, data = PanamaTest)
bpPanama$breakpoint
plot(bpPanama)
sctest(bpPanama)
## Paraguay  --------

ParaguayGini<-ts(Paraguay$gini100,start=c(1993),end=c(2014),frequency=1)
ParaguayIdeo<-ts(Paraguay$PresidentPartyLeftRightIndex_mean,start=c(1993),end=c(2014),frequency=1)
plot(ParaguayIdeo)
ParaguayTest<-cbind(ParaguayGini,ParaguayIdeo)

## initialize the monitoring with the formula interface
Paraguay93 <- window(ParaguayTest,start=c(1993),end=c(2002))

Paraguay.mefp <- mefp(ParaguayGini ~ ParaguayIdeo, type="ME",
                    data=Paraguay93, alpha=0.05)
plot(Paraguay.mefp)
## monitor the new observations for the year 1972
Paraguay93 <-window(ParaguayTest,start=c(1993),end=c(2014))
Paraguay.mefp <- monitor(Paraguay.mefp)
Paraguay.mefp

plot(Paraguay.mefp, functional = NULL)
bpParaguay<- Fstats(ParaguayGini ~ ParaguayIdeo, data = ParaguayTest)
bpParaguay$breakpoint
plot(bpParaguay)
sctest(bpParaguay)
## Peru  --------

PeruGini<-ts(Peru$gini100,start=c(1993),end=c(2014),frequency=1)
PeruIdeo<-ts(Peru$PresidentPartyLeftRightIndex_mean,start=c(1993),end=c(2014),frequency=1)

PeruTest<-cbind(PeruGini,PeruIdeo)

## initialize the monitoring with the formula interface
Peru93 <- window(PeruTest,start=c(1993),end=c(2002))

Peru.mefp <- mefp(PeruGini ~ PeruIdeo, type="ME",
                    data=Peru93, alpha=0.05)
plot(Peru.mefp)
## monitor the new observations for the year 1972
Peru93 <-window(PeruTest,start=c(1993),end=c(2014))
Peru.mefp <- monitor(Peru.mefp)
Peru.mefp

plot(Peru.mefp, functional = NULL)

# Break detected at observation # 19 
bpPeru<- Fstats(PeruGini ~ PeruIdeo, from=10, data = PeruTest)
bpPeru$breakpoint
plot(bpPeru)
sctest(bpPeru)

## Puerto Rico ---------
PuertoRico<-`Puerto Rico`
PuertoRicoGini<-ts(PuertoRico$gini100,start=c(1993),end=c(2014),frequency=1)
PuertoRicoIdeo<-ts(PuertoRico$PresidentPartyLeftRightIndex_mean,start=c(1993),end=c(2014),frequency=1)

PuertoRicoTest<-cbind(PuertoRicoGini,PuertoRicoIdeo)

## initialize the monitoring with the formula interface
PuertoRico93 <- window(PuertoRicoTest,start=c(1993),end=c(2002))

PuertoRico.mefp <- mefp(PuertoRicoGini ~ PuertoRicoIdeo, type="ME",
                    data=PuertoRico93, alpha=0.05)
plot(PuertoRico.mefp)
## monitor the new observations for the year 1972
PuertoRico93 <-window(PuertoRicoTest,start=c(1993),end=c(2014))
PuertoRico.mefp <- monitor(PuertoRico.mefp)
PuertoRico.mefp

plot(PuertoRico.mefp, functional = NULL)
#NO BREAK FOUND FOR PUERTO RICO
bpPuertoRico<- Fstats(PuertoRicoGini ~ PuertoRicoIdeo, from=9, data = PuertoRicoTest)
bpPuertoRico$breakpoint
plot(bpPuertoRico)
sctest(bpPuertoRico) 
## Uruguay  --------
UruguayGini<-ts(Uruguay$gini100,start=c(1993),end=c(2014),frequency=1)
UruguayIdeo<-ts(Uruguay$PresidentPartyLeftRightIndex_mean,start=c(1993),end=c(2014),frequency=1)

UruguayTest<-cbind(UruguayGini,UruguayIdeo)

## initialize the monitoring with the formula interface
Uruguay93 <- window(UruguayTest,start=c(1993),end=c(2002))

Uruguay.mefp <- mefp(UruguayGini ~ UruguayIdeo, type="OLS-CUSUM",
                    data=Uruguay93, alpha=0.05)
plot(Uruguay.mefp)
## monitor the new observations for the year 1972
Uruguay93 <-window(UruguayTest,start=c(1993),end=c(2014))
Uruguay.mefp <- monitor(Uruguay.mefp)
#Break at 14, matrix failed afterwards
Uruguay.mefp

plot(Uruguay.mefp, functional = NULL)

bpUruguay<- Fstats(UruguayGini ~ UruguayIdeo, data = UruguayTest)
bpUruguay$breakpoint
plot(bpUruguay)
sctest(bpUruguay)
## Venezuela  --------
VenezuelaGini<-ts(Venezuela$gini100,start=c(1993),end=c(2014),frequency=1)
VenezuelaIdeo<-ts(Venezuela$PresidentPartyLeftRightIndex_mean,start=c(1993),end=c(2014),frequency=1)

VenezuelaTest<-cbind(VenezuelaGini,VenezuelaIdeo)

## initialize the monitoring with the formula interface
Venezuela93 <- window(VenezuelaTest,start=c(1993),end=c(2002))

Venezuela.mefp <- mefp(VenezuelaGini ~ VenezuelaIdeo, type="ME",
                    data=Venezuela93, alpha=0.05)
plot(Venezuela.mefp)
## monitor the new observations for the year 1972
Venezuela93 <-window(VenezuelaTest,start=c(1993),end=c(2014))
Venezuela.mefp <- monitor(Venezuela.mefp)
Venezuela.mefp$lastcoef

plot(Venezuela.mefp, functional = NULL)
#Break detected at observation # 18 

bpVen<- Fstats(VenezuelaGini ~ VenezuelaIdeo, data = VenezuelaTest)
bpVen$breakpoint
plot(bpVen)
sctest(bpVen)

?mefp
?Fstats
?sctest
# Initial view, summary ---------------------------------------------------

summary(LatAm2014$gini100)
