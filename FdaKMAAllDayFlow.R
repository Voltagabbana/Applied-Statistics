# FDAKMA sulla base del tde gennaio 2021 prob numero 4:
# ( piu qualche cosa dal laboratorio, vedi kma.compare)

load("C:/Users/enduser/Desktop/ProgettoApplied/Mie prove/FdaKMAAllDayFlow/fda_flowsMio.Rda")

library(fda)
library(fdakma)

#a)

fdat = t(fda_flows) # traspongo semplicemente per avere stesso stile del tde
abscissa <- 1:24
spectra = fdat
spectra1 =as.data.frame( spectra[1,])
Xobs0 <- spectra1$`spectra[1, ]`

plot(abscissa,Xobs0, type = "l")

nbasis <- 6:18
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis(range(abscissa), nbasis[i],norder=3)
  gcv[i] <- smooth.basis(abscissa, Xobs0, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v=nbasis[which.min(gcv)],col='red')

# number of basis = 10

basis <- create.fourier.basis(rangeval=range(abscissa), nbasis=nbasis[which.min(gcv)])
plot(basis)

Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis)
Xsp0bis <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data

plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis ,type="l",col="red",lwd=2)

basismat <- eval.basis(abscissa, basis)
dim(basismat) # number of data x number of basis
head(basismat)

# Fit via LS
help(lsfit)

est_coef = lsfit(basismat, Xobs0, intercept=FALSE)$coef
est_coef

# b)

basis.1 <- create.bspline.basis(rangeval=c(1,24),nbasis=13,norder=4)
data_W.fd.1 <- Data2fd(y = t(spectra),argvals = abscissa,basisobj = basis.1)
plot.fd(data_W.fd.1)

# c)

x = t(as.data.frame(abscissa))
y0 = spectra

# provo a trovare clusters con alignment e d0pearson

set.seed(1)
fdakma_example <- kma(
  x=x, y0=y0, n.clust = 2, 
  warping.method = 'affine', 
  similarity.method = 'd0.pearson',  # similarity computed as the cosine
  # between the first derivatives 
  # (correlation)
  center.method = 'k-means'
  #seeds = c(1,21) # you can give a little help to the algorithm...
)
kma.show.results(fdakma_example)

# provo a trovare clusters con alignment e d0pearson

set.seed(1)
fdakma_example <- kma(
  x=x, y0=y0, n.clust = 2, 
  warping.method = 'NOalignment', 
  similarity.method = 'd0.pearson',  # similarity computed as the cosine
  # between the first derivatives 
  # (correlation)
  center.method = 'k-means'
  #seeds = c(1,21) # you can give a little help to the algorithm...
)
kma.show.results(fdakma_example)

# NB IMPORTANTE:
# RIESCO A TROVARE CLUSTER BEN DEFINITI GIA SOLO DA D0.PEARSON, SENZA CONSIDERARE
# ALIGNMENT (  HA SENSO ANCHE VISIVAMENTE )
# dimostro però questa teoria con il seguente codice: 
# preso da laboratorio

kma.compare_example_3 <- kma.compare (
  x=x, y0=y0, n.clust = 1:3, 
  warping.method = c("NOalignment", "shift", "dilation", "affine"), 
  similarity.method = 'd0.pearson',
  center.method = 'k-means', 
  seeds = c(1,21,30),
  plot.graph=TRUE)

# da questo capisco che no alignment e 2 cluster è ok 

kma.compare_example_3 <- kma.compare (
  x=x, y0=y0, n.clust = 1:3, 
  warping.method = 'NOalignment', 
  similarity.method = 'd0.pearson',
  center.method = 'k-means', 
  seeds = c(1,21,30),
  plot.graph=TRUE)





















