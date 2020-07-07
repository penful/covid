#### TABLES AND FIGURES ###############
# Code to reproduce tables and plots of the chapter   
# Bartolucci F., Pennoni, F. (2020).
# Alcuni modelli per dati di conteggio 
# con applicazione all’epidemia COVID-19
# BOOK titled: COVID 19 tra emergenza sanitaria ed 
# emergenza economica
##
rm(list = ls())
load("Umbria_Italy_model_now_positive.Rdata")
require(tscount)
#### 
summary(totUmbria)
summary(totItaly)
#### TABLE 1 ####
AIC1Umbria <- rbind(A1Umbria,A2Umbria,A3Umbria,A4Umbria)
AIC1Umbria
#
AIC1Italy <- rbind(A1Italy,A2Italy,A3Italy,
                   A4Italy)
AIC1Italy


#### TABLE 2 ####
summary(M1Umbria)[5]
summary(M2Umbria)[5]
summary(M3Umbria)[5]
summary(M4Umbria)[5]
round(M1Umbria$coefficients,3)
round(M2Umbria$coefficients,3)
round(M3Umbria$coefficients,3)
round(M4Umbria$coefficients,3)
#
summary(M1Italy)[5]
summary(M2Italy)[5]
summary(M3Italy)[5]
summary(M4Italy)[5]
round(M1Italy$coefficients,6)
round(M2Italy$coefficients,6)
round(M3Italy$coefficients,6)
round(M4Italy$coefficients,6)

#### overdispesion s.e.####
summary(M4Italy)
summary(M4Umbria)
#round(seM4$se[6],3)
#round(seM4IT$se[6],3)
### TABLE 3 ####
round(P4Umbria$pred,0)
cbind((P4Umbria$interval[,1]),(P4Umbria$interval[,2]))
#
round(P4Italy$pred,0)
cbind((P4Italy$interval[,1]),(P4Italy$interval[,2]))

#### FIGURE 1 UMBRIA  ####
n<-dim(dataUmbria)[1]
m<-max(totUmbria)
#ytick<-c(0,500,950)
ytick<-c(0,250,500,750,1000)
xtick<-c(1,50,100,133)

plot(totUmbria, 
      ylab=expression("Attualmente positivi in Umbria ("*italic(y[t])*", "*italic(hat(y)[t])*")"),  
      line = 2.5,
      xlab=expression("Giorni ("*italic(t)*")"),
      yaxt="n",
     xaxt="n",
     xlim =c(1,n+5),
     ylim = c(0,1000),
     lwd = 0.5, 
     lty = 1,col ="black" )
abline(v=128, col = "grey")
axis(side=2, at=ytick, labels = FALSE, 
     cex.lab = 0.5, tck=-0.009)
text(par("usr")[1], 
     ytick,  
     labels = ytick, srt = 45, 
     pos = 2, xpd = TRUE, cex.lab = 0.5)

axis(side=1, at=xtick, labels = FALSE, cex=0.5, 
     padj = 5, tck=-0.009)
text(x=xtick,  
     par("usr")[3], 
     labels = xtick, 
     cex.lab = 0.5,
     pos = 1, xpd = TRUE)
lines(c(M1Umbria$fitted.values,P1Umbria$pred), 
      lwd=1, col = 2, lty = 2)
lines(c(M2Umbria$fitted.values,P2Umbria$pred), 
      lwd=1, col = 3, lty = 3)
lines(c(M3Umbria$fitted.values,P3Umbria$pred), 
      lwd=1, col = 4, lty = 4)
lines(c(M4Umbria$fitted.values,P4Umbria$pred), 
      lwd=1, col = 1, lty = 1)

categories<-c(
  expression("Osservati"),
  expression(italic(M)[1]),
  expression(italic(M)[2]),
  expression(italic(M)[3]),
 expression(italic(M)[4]))
col = c(1,2,3,4,1)
legend( 
  98,900,
  pch = c(20,NA,NA,NA,NA),
  lty = c(NA,2,3,4,1),
  legend = categories, 
  col = col, 
  bty = "n", 
  x.intersp = 0.1,
  cex= 1,  pt.cex = .5,
  xpd = TRUE,
  text.width = 0.0001)

#### FIGURE 2 ITALY ####
n<-dim(dataUmbria)[1]
m<-max(totItaly)
#ytick<-c(0,27009,54018,81027,110000)
ytick<-c(0,30000,60000,90000,120000)
xtick<-c(1,50,100,133)
plot(totItaly, 
   line = 2.5,
      ylab=expression("Attualmente positivi in Italia ("*italic(y[t])*", "*italic(hat(y)[t])*")"),
      xlab=expression("Giorni ("*italic(t)*")"),
       yaxt="n",
     xaxt="n",
     xlim =c(1,n+5),
     ylim = c(0,120000),
     lwd = 0.5, 
     lty = 1,col ="black" )
abline(v=128, col = "gray")
axis(side=2, at=ytick, labels = FALSE, 
     cex.lab = 0.5, padj = 2,tck=-0.009)
text(par("usr")[1], 
     ytick,  
     labels = ytick, srt = 45, 
     pos = 2, xpd = TRUE, cex.lab = 0.5)

axis(side=1, at=xtick, 
     labels = FALSE, cex=0.5,tck=-0.009)
text(x=xtick,  
     par("usr")[3], 
     labels = xtick, 
     cex.lab = 0.5,
     pos = 1, xpd = TRUE)
lines(c(M1Italy$fitted.values,P1Italy$pred), 
      lwd=1, col = 2, lty = 2)
lines(c(M2Italy$fitted.values,P2Italy$pred), 
      lwd=1, col = 3, lty = 3)
lines(c(M3Italy$fitted.values,P3Italy$pred), 
      lwd=1, col = 4, lty = 4)
lines(c(M4Italy$fitted.values,P4Italy$pred), 
      lwd=1, col = 1, lty = 1)
legend(98, 108000, 
  pch = c(20,NA,NA,NA,NA),
  lty = c(NA,2,3,4,1),
  legend = categories, 
  col = col, 
  bty = "n", 
  x.intersp = 0.1,
  cex= 1,  pt.cex = .5,
  xpd = TRUE,
  text.width = 0.0001)

#### FIGURE 3 ####
# total population
prevUM<-M4UM/886239
prevIT<-M4IT/60317000
n<-length(prevIT)
# times 1000
plot(prevIT[1:133]*1000, 
     ylab= "Prevalenza dei casi attualmente positivi (x1000)",  
     line = 2.5,
     ylim = c(0,2),
     col = "blue4",
     xlab=expression("Giorni ("*italic(t)*")"),
     yaxt="n",
     xaxt="n",
     type = "l",
     lwd = 1, 
     lty = 1)
lines(prevUM[1:133]*1000, 
      lwd=1, col = "red2", lty = 1)
#ytick<-c(0,0.4, 0.9, 1.3,1.8)
ytick<-c(0, 0.5, 1.0, 1.5, 2)
xtick<-c(1,50,100,133)
abline(v=128, col = "gray")

axis(side=2, at=ytick, labels = FALSE, 
     cex.lab = 0.5,tck=-0.009)
text(par("usr")[1], 
     ytick,  
     labels = c("0.0", "0.5", "1.0","1.5", "2.0"), 
     srt = 45, 
     pos = 2, xpd = TRUE, cex.lab = 0.5)

axis(side=1, at=xtick, labels = FALSE, cex=0.5,tck=-0.009)
text(x=xtick,  
     par("usr")[3], 
     labels = xtick, 
     cex.lab = 0.5,
     pos = 1, xpd = TRUE)

categories<-c(expression("Umbria"),
              expression("Italia"))
col = c("red2","blue4")
legend(98, 1.8,
       pch = c(NA,NA),
       lty = c(1,1),
       legend = categories, 
       col = col, 
       bty = "n", 
       x.intersp = 0.3,
       cex= 1,  pt.cex = .5,
       xpd = TRUE,
       text.width = 0.0001)
