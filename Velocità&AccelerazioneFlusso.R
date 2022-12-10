## vedi lab smoothing ( circa riga 600)

load("C:/Users/enduser/Desktop/ProgettoApplied/Mie prove/Velocità&AccelerazioneFlusso/MatriceSommaFlowMean.Rda")

library(fda)

traffic = matrice
data_W = t(as.matrix(traffic)) #### IMPORTANTE, trasformo come una matrice, poi traspongo
time <- 1:24
m = 3
nn = 7 # numero colonne data_W (giorni della settimana)


basis.1 <- create.bspline.basis(rangeval=c(1,24),nbasis=13, m)

data_W.fd.1 <- Data2fd(y = data_W,argvals = time,basisobj = basis.1)
par(mfrow = c(1,1))
# plot "brutto" smoothed data per dare un'idea
plot(data_W.fd.1)


velocfdUN <- deriv.fd(expr = data_W.fd.1, Lfdobj = 1)
velocmeanfdUN <- mean.fd(velocfdUN)

accelfdUN <- deriv.fd(expr = data_W.fd.1, Lfdobj = 2)
accelmeanfdUN <- mean.fd(accelfdUN)

x11()
par(mfrow=c(2,1),mar=c(6,5,2,1),mex=0.6, mgp=c(2.2,0.7,0),pty="m", font.main=1,font.lab=1, font.axis=1,cex.lab=1.3,cex.axis=1)
plot(data_W.fd.1, xlim=c(1,24), lty=1, lwd=2,
     cex=2, xlab="Hour Of The Day", ylab="Flow (People Moving / Hour)")
plot(velocfdUN, xlim=c(1,24),  lty=1, lwd=2,
     cex=2, xlab="Hour of The Day", ylab="First derivative (People Moving / Hour^2)")

# plotto ora in base a weekend non weekend

col = c(4,4,4,4,7,7,4)


x11()
par(mfrow=c(3,1),mar=c(6,5,2,1),mex=0.6, mgp=c(2.2,0.7,0),pty="m", font.main=1,font.lab=1, font.axis=1,cex.lab=1.3,cex.axis=1)
plot(data_W.fd.1, xlim=c(1,24), lty=1, lwd=2,
     cex=2,col = col, xlab="Hour Of The Day", ylab="Flow ")
plot(velocfdUN, xlim=c(1,24),  lty=1, lwd=2,
     cex=2,col = col, xlab="Hour of The Day", ylab="First derivative ")

plot(accelfdUN, xlim=c(1,24),  lty=1, lwd=2,
     cex=2,col = col, xlab="Hour of The Day", ylab="Second derivative ")






























