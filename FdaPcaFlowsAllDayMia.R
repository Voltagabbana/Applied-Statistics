# codice per sistemare dati luca
#######
fda_flows <- read.csv(file='fda_flowsAllDayLuca.csv', header=T)

colnames(fda_flows)= c('Day_182','Day_183','Day_184','Day_185','Day_186','Day_187','Day_188','Day_189','Day_190','Day_191','Day_192','Day_193','Day_194','Day_195','Day_196','Day_197','Day_198','Day_199','Day_200','Day_201','Day_202','Day_203','Day_204','Day_205','Day_206','Day_207','Day_208','Day_209','Day_210','Day_211','Day_212','Day_213','Day_214','Day_215','Day_216','Day_217','Day_218','Day_219','Day_220','Day_221','Day_222','Day_223','Day_224','Day_225','Day_226','Day_227','Day_228','Day_229','Day_230','Day_231','Day_232','Day_233','Day_234','Day_235','Day_236','Day_237','Day_238','Day_239','Day_240','Day_241','Day_242','Day_243','Day_244','Day_245','Day_246','Day_247','Day_248','Day_249','Day_250','Day_251','Day_252','Day_253','Day_254','Day_255','Day_256','Day_257','Day_258','Day_259','Day_260','Day_261','Day_262','Day_263','Day_264','Day_265','Day_266','Day_267','Day_268','Day_269','Day_270','Day_271','Day_272','Day_273','Day_274','Day_275','Day_276','Day_277','Day_278','Day_279','Day_280','Day_281','Day_282','Day_283','Day_284','Day_285','Day_286','Day_287','Day_288','Day_289','Day_290','Day_291','Day_292','Day_293','Day_294','Day_295','Day_296','Day_297','Day_298','Day_299','Day_300','Day_301','Day_302','Day_303','Day_304')
rownames(fda_flows) = c('hour1','hour2','hour3','hour4','hour5','hour6','hour7','hour8','hour9',
                                           'hour10','hour11','hour12',
                                           'hour13','hour14','hour15','hour16','hour17','hour18','hour19','hour20',
                                           'hour21','hour22','hour23','hour24')
fda_flows

dim(fda_flows) # 24 * 123 = 2952 = numero file presenti in AllDayHour
head(fda_flows)

save(fda_flows,file="fda_flowsMio.Rda") # salvo matrice per il futuro
#######

# Inizio codice vero e proprio


load("C:/Users/enduser/Desktop/ProgettoApplied/Mie prove/FdaPCAAllDayFlow/fda_flowsMio.Rda")

data_W = fda_flows
time <- 1:24
m = 4
nn = 123 # numero colonne (numero giorni in analisi)

# matplot "brutto" per dare un'idea

matplot(data_W, type = 'l', col =seq_len(nn),lty = 1)
# oss: osservo che il flusso giorno per giorno è dell'ordine delle centinaia, nel file
# mean invece è dell'ordine delle migliaia, ha senso perchè nel file mean si sommano 
# tutti i flow per ogni giorno della settimana (150*123/7=2635.714, per dare un' idea)

# matplot bello per mesi (luglio agosto settembre ottobre)

col = rep(1:4,c(31,31,31,30)) # nero luglio, rosso agosto, verde settembre, blu ottobre

x11()
layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
matplot((as.data.frame(data_W)),col =col,type='l',
        xlab = 'Hour of the day', lwd = 1, lty = 1,
        ylab = 'Total Flow', main = 'Flow for each day from July to October in Maputo')
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
mesi = c('July','August','September','October')
legend("center", mesi,col=col,cex=0.8,fill=seq_len(123))

# oss: osservo che sopratutto ottobre presenta un flusso più basso (probabilmente
# per regioni climatiche infatti l'inverno (la nostra estate), è la stagione più secca
# mentre da ottobre inizia l'estate e la stagione delle piogge da google osservo che mediamente 
# luglio, agosto e settembre presentano 2 giorni al mese di pioggia, ottobre 5)

# matplot bello per stagione con legenda

col = rep(2:3,c(90,33))

x11()
layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
matplot((as.data.frame(data_W)),col =col,type='l',
        xlab = 'Hour of the day', lwd = 1, lty = 1,
        ylab = 'Total Flow', main = 'Flow for each day from July to October in Maputo')
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
mesi = c('Dry Season','Rainy Season')
legend("center", mesi,col=col,cex=0.8,fill=2:3)

# matplot bello per giorni settimana

col = rep(1:7,123) 

x11()
layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
matplot(as.data.frame(data_W),col =col,type = 'l',
        xlab = 'Hour of the day', lwd = 1, lty = 1,
        ylab = 'Total Flow', main = 'Flow for each day of the week in Maputo')
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
mesi = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
legend("center", mesi,col=col,cex=0.8,fill=1:7)

# STOP VISUALIZZAZIONE DATI

# INIZIO SMOOTHING
library(fda)

# creo base
basis.1 <- create.bspline.basis(rangeval=c(1,24),nbasis=13, m)
plot(basis.1)
data_W.fd.1 <- Data2fd(y = as.matrix(data_W),argvals = time,basisobj = basis.1)
par(mfrow = c(1,1))
# plot "brutto" smoothed data per dare un'idea
plot(data_W.fd.1)

# plot per mesi fatto bene
col = rep(1:4,c(31,31,31,30)) # nero luglio, rosso agosto, verde settembre, blu ottobre

x11()
layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot.fd(data_W.fd.1,col =col,
        xlab = 'Hour of the day', lwd = 1, lty = 1,
        ylab = 'Total Flow', main = 'Flow for each day from July to October in Maputo')
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
mesi = c('July','August','September','October')
legend("center", mesi,col=col,cex=0.8,fill=seq_len(123))



# plot bello per stagione con legenda
col = rep(2:3,c(90,33)) # nero luglio, rosso agosto, verde settembre, blu ottobre

x11()
layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot.fd(data_W.fd.1,col =col,
        xlab = 'Hour of the day', lwd = 1, lty = 1,
        ylab = 'Total Flow', main = 'Flow for each day from July to October in Maputo')
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
mesi = c('Dry Season','Rainy Season')
legend("center", mesi,col=col,cex=0.8,fill=2:3)

# plot bello per giorni
col = rep(1:7,123) 

x11()
layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot.fd(data_W.fd.1,col =col,
        xlab = 'Hour of the day', lwd = 1, lty = 1,
        ylab = 'Total Flow', main = 'Flow for each day from July to October in Maputo')
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
mesi = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
legend("center", mesi,col=col,cex=0.8,fill=1:7)



#PCA

pca_W.1 <- pca.fd(data_W.fd.1,nharm=5,centerfns=TRUE)

# scree plot
par(mfrow = c(1,1))
plot(pca_W.1$values[1:35],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)[1:35]/sum(pca_W.1$values),xlab='j',ylab='CPV',ylim=c(0,1))

cumsum(pca_W.1$values)[1:3]/sum(pca_W.1$values) # varianza cumulata prime 3 PC

# 0.7125035 0.8202202 0.8636573



# plot prime 2 eigenfunctions e loro interpretazione
x11()
par(mfrow = c(2,1))
plot(pca_W.1$harmonics[1,],col=2,ylab='FPC1',ylim=c(-0.35,0.55), lwd = 2,
     main = 'First Principal Component', xlab = 'Hour of the day')
# flusso nelle ore di punta
plot(pca_W.1$harmonics[2,],col=4,ylab='FPC2',ylim=c(-0.35,0.55),lwd = 2,
     main = 'Second Principal Component', xlab = 'Hour of the day')
# flusso nelle ore non di punta (notte e mezzogiorno)

# oss: mi sembra ragionevole che dia stessi risultati del file "Mean" essendo le stesse
# variabili solo sommate.


# Plot PCs come perturbazione della media
x11()
par(mfrow=c(1,2))
plot(pca_W.1, nx=150, pointplot=TRUE, harm=c(1,2), expand=0, col = 'blue',
     cycle=FALSE, lwd = 2, xlab = 'Hour of the day')

# interpretazione simile a quella già data dopo il plot delle eigenfunctions.


# # plot degli scores (primi due) in base al giorno (range che va da 182-304)

par(mfrow=c(1,1))
plot(182:304,pca_W.1$scores[,1],xlab="day",ylab="Scores FPC1",lwd=2)

# si vedono evidentemente due cluster


par(mfrow=c(1,1))
plot(182:304,pca_W.1$scores[,2],xlab="day",ylab="Scores FPC2",lwd=2)

# plot scores su piano pc1-pc2

plot(pca_W.1$scores[,1],pca_W.1$scores[,2])

# si vedono evidentemente due cluster

# plot scores su piano pc1-pc2 con scritta identificativa del giorno

x11()
plot(pca_W.1$scores[,1],pca_W.1$scores[,2], type = 'n',xlim = c(-150,100),
     ylim = c(-60,85), xlab = 'PC1',ylab = 'PC2',main = 'Scores along the first two PCs, for each day of the week')
text(pca_W.1$scores[,1],pca_W.1$scores[,2],col = 'blue', dimnames(data_W)[[2]])

# da questo text plot, i cluster non sembrano dipendere ne dalla st

# plot scores su piano pc1-pc2 coloro ina base a:
# a) 7 giorni della settimana
# b) 4 mesi 
# c) stagione pioggia e secca

# a) 7 giorni della settimana

# oss: IMPORTANTE
# 182 --> primo luglio 2019
# 304 --> 31 ottore 2019


plot(pca_W.1$scores[,1],pca_W.1$scores[,2], col = 2:8, pch = 2, lwd = 2)

# stesso plot bello con legenda

col = rep(1:7,123) 

x11()
layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],col =col,
        xlab = 'PC1', lwd = 2, pch = 2,
        ylab = 'PC2', main = 'Scores for each day of the week')
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
mesi = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
legend("center", mesi,col=col,cex=0.8,fill=1:7)


# provo a ingrandire plot e legenda



# sembra molto informativo infatti giallo e viola sono in alto a sinistra, 
# verde, verdeacqua rosso blu e grigio in basso a destra
# potrebbe essere una divisione weekend vs weekdays (!)

# SISTEMO !! --> vedo a quale colore corrisponde quale giorno, legenda ecc.

x11()
plot(pca_W.1$scores[,1],pca_W.1$scores[,2], type = 'n',xlim = c(-150,100),
     ylim = c(-60,85), xlab = 'PC1',ylab = 'PC2',main = 'Scores along the first two PCs, for each day of the week')
text(pca_W.1$scores[,1],pca_W.1$scores[,2],col = 'blue', c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))

# da questo posso capire quali colori corrispondono a quali giorni della settimana:

# rosso - monday
# verde - tuesday
# blu - wednesday
# verde acqua - thursday
# viola - friday
# giallo - saturday
# grigio - sunday

# b) 4 mesi

plot(pca_W.1$scores[,1],pca_W.1$scores[,2], col = rep(1:4,c(31,31,31,30)), pch = 2, lwd = 2)


# poco informativo in termini di cluster, ma informativo in termini di PC2 soprattutto
# SISTEMO LEGENDA


col = rep(1:4,c(31,31,31,30)) # nero luglio, rosso agosto, verde settembre, blu ottobre

x11()
layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],col =col,
     xlab = 'PC1', lwd = 2, pch = 2,
     ylab = 'PC2', main = 'Scores along the first two PCs, for each day of the week')
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
mesi = c('July','August','September','October')
legend("center", mesi,col=col,cex=0.8,fill=1:4)



# c) stagione pioggia e secca

plot(pca_W.1$scores[,1],pca_W.1$scores[,2], col = rep(2:3,c(90,33)), pch = 2, lwd = 2)

# poco informativo in termini di cluster, ma informativo in termini di PC2 soprattutto
# SISTEMO LEGENDA

col = rep(2:3,c(90,33)) # nero luglio, rosso agosto, verde settembre, blu ottobre

x11()
layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],col =col,
     xlab = 'PC1', lwd = 2, pch = 2,
     ylab = 'PC2', main = 'Scores for rainy and dry season')
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
mesi = c('Dry Season','Rainy Season')
legend("center", mesi,col=col,cex=0.8,fill=2:3)

# provo a ingrandire plot e legenda


















# d) giorni progressivi da 182 a 304

col.ramp <- rainbow(123)

plot(pca_W.1$scores[,1],pca_W.1$scores[,2], col =col.ramp, pch = 2, lwd = 2)

plot(1:123,col = col.ramp) # per interpretare grafico precedente

# interpretazione simile a punto b) e c) ma non troppo significativo









