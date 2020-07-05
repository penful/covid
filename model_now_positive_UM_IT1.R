###################
# Code to reproduce the models estimated in  
# Bartolucci F., Pennoni, F. (2020).
# Alcuni modelli per dati di conteggio 
# con applicazione all’epidemia COVID-19
###################
rm(list = ls())
###################
#### Choose dataUmbria #####
# UMBRIA 
load("dataUmbria.Rdata")
# ITALY
load("dataItaly.Rdata")


#### DEFINE THE COUNTS OF INTEREST ####
tot1Umbria <- dataUmbria$terapia_intensiva
tot2Umbria <- dataUmbria$isolamento_domiciliare
tot3Umbria <- dataUmbria$dimessi_guariti
tot4Umbria <- dataUmbria$ricoverati_con_sintomi
tot5Umbria <- dataUmbria$deceduti
#
#### COUNTS FOR NEW POSITIVE CASES ####
totUmbria <- dataUmbria$terapia_intensiva+
             dataUmbria$isolamento_domiciliare +
             dataUmbria$ricoverati_con_sintomi
#
totItaly <- dataItaly$terapia_intensiva+
  dataItaly$isolamento_domiciliare +
  dataItaly$ricoverati_con_sintomi

#### DEFINE THE COVARIATES ####
## --------------------------------------------------------
# linear and quadratic trend 
regressorsUmbria <- cbind(linearTrend=seq(along=totUmbria), 
                  quadTrend = seq(along=totUmbria)^2/100)
#
regressorsItaly <- cbind(linearTrend=seq(along=totItaly), 
                    quadTrend = seq(along=totItaly)^2/100)

# linear, quadratic and log  trend 
regressors1Umbria <- cbind(linearTrend=seq(along=totUmbria), 
                   quadTrend = seq(along=totUmbria)^2/100,
                   linlogTrend = log(seq(along=totUmbria)))
#
regressors2Umbria <- cbind(linearTrend=seq(along=totUmbria), 
                           linlogTrend = log(seq(along=totUmbria)))

regressors1Italy <- cbind(linearTrend=seq(along=totItaly), 
                           quadTrend = seq(along=totItaly)^2/100,
                           linlogTrend = log(seq(along=totItaly)))
#

#### ESTIMATE THE MODELS ####
## --------------------------------------------------------
# M1 = Poisson, with linear and quadratic trend
# M2 = M1 and a log trend
# M3 = M2 and autoregressive component of first order
# M4 = M3 with a Negative Binomial distribution
##
require(tscount)
M1Umbria <- tsglm(ts=totUmbria, 
              link = "log",
              xreg=regressorsUmbria, 
              distr = "poisson")
#
M2Umbria <- tsglm(ts=totUmbria, 
              link = "log",
              xreg=regressors1Umbria, 
              distr = "poisson")
#

M3Umbria <- tsglm(ts=totUmbria, 
               link = "log",
               model=list(past_obs=1),
               xreg=regressors1Umbria, 
               distr = "poisson")

M4Umbria <- tsglm(ts=totUmbria, 
            link = "log",
            model=list(past_obs=1),
            xreg=regressors1Umbria,
            distr = "nbinom")
#

M1Italy <- tsglm(ts=totItaly, 
                  link = "log",
                  xreg=regressorsItaly, 
                  distr = "poisson")
#
M2Italy <- tsglm(ts=totItaly, 
                  link = "log",
                  xreg=regressors1Italy, 
                  distr = "poisson")
#

M3Italy <- tsglm(ts=totItaly, 
                  link = "log",
                  model=list(past_obs=1),
                  xreg=regressors1Italy, 
                  distr = "poisson")
#
M4Italy <- tsglm(ts=totItaly, 
                  link = "log",
                  model=list(past_obs=1),
                  xreg=regressors1Italy, 
                  distr = "nbinom")

#### MODEL COMPARISON ####
## --------------------------------------------------------
A1Umbria <- summary(M1Umbria)
cbind(A1Umbria$QIC, A2Umbria$QIC,A3Umbria$QIC,A4Umbria$QIC)
cbind(A1Umbria$AIC, A2Umbria$AIC,A3Umbria$AIC,A4Umbria$AIC)

A2Umbria <- summary(M2Umbria)
A3Umbria <- summary(M3Umbria)
A4Umbria <- summary(M4Umbria)
#
A1Italy <- summary(M1Italy)
A2Italy <- summary(M2Italy)
A3Italy  <- summary(M3Italy)
A4Italy <- summary(M4Italy)
#### STANDARD ERROR OVERDISPERSION PARAMETER OBTAINED WITH 
#### PARAMETRIC BOOTSTRAP####
round(M4Umbria$sigmasq,6)
seM4<-se(M4Umbria, B = 200)
seM4$se
round(M4Italy$sigmasq,6)
seM4IT<-se(M4Italy, B = 200)
seM4IT$se
seM4IT$ci
#### PREDICTED VALUES 20 DAYS AHEAD ####
## --------------------------------------------------------
## --------------------------------------------------------
go <- 5
#
TT <- length(totUmbria)
## --------------------------------------------------------
P1Umbria <-predict(M1Umbria, 
              newxreg = data.frame(linearTrend = ((TT+1):(TT+go)), 
                                   quadTrend = ((TT+1):(TT+go))^2/100),
              n.ahead=go)
#
P1Italy <-predict(M1Italy, 
                   newxreg = data.frame(linearTrend = ((TT+1):(TT+go)), 
                                        quadTrend = ((TT+1):(TT+go))^2/100),
                   n.ahead=go)
## --------------------------------------------------------
P2Umbria <-predict(M2Umbria, 
             newxreg = data.frame(linearTrend = ((TT+1):(TT+go)), 
                                  quadTrend = ((TT+1):(TT+go))^2/100,
                                  linlogTrend = log((TT+1):(TT+go))),
             n.ahead=go)
#
P2Italy <-predict(M2Italy, 
                   newxreg = data.frame(linearTrend = ((TT+1):(TT+go)), 
                                        quadTrend = ((TT+1):(TT+go))^2/100,
                                        linlogTrend = log((TT+1):(TT+go))),
                   n.ahead=go)
# --------------------------------------------------------
P3Umbria <-predict(M3Umbria,
             newxreg = data.frame(linearTrend = ((TT+1):(TT+go)),
                                  quadTrend = ((TT+1):(TT+go))^2/100,
                                  linlogTrend = log((TT+1):(TT+go))),
             n.ahead=go)


P3Italy <-predict(M3Italy, 
                   newxreg = data.frame(linearTrend = ((TT+1):(TT+go)), 
                                        quadTrend = ((TT+1):(TT+go))^2/100,
                                        linlogTrend = log((TT+1):(TT+go))),
                   n.ahead=go)
## --------------------------------------------------------
P4Umbria <-predict(M4Umbria, 
             newxreg = data.frame(linearTrend = ((TT+1):(TT+go)), 
                                  quadTrend = ((TT+1):(TT+go))^2/100,
                                  linlogTrend = log((TT+1):(TT+go))),
             n.ahead=go,B=10000)
#
P4Italy <-predict(M4Italy, 
                   newxreg = data.frame(linearTrend = ((TT+1):(TT+go)), 
                                        quadTrend = ((TT+1):(TT+go))^2/100,
                                        linlogTrend = log((TT+1):(TT+go))),
                   n.ahead=go,B=10000)


M4UM<-c(M4Umbria$fitted.values,P4Umbria$pred)

M4IT<-c(M4Italy$fitted.values,P4Italy$pred)

#save.image("Umbria_Italy_model_now_positive.Rdata")
