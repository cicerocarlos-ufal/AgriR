#Densing for CRD
#' @author Cícero Carlos de Souza Almeida
#' @export
des.fat2.crd <- function(factor1, factor2, rep, col.1="gray",col.2="white", graph.col=TRUE,fac.names=c('F1','F2'), save=FALSE){

f1=factor1
f2=factor2
r=rep 
#Nomes para os fatores
f1.trt=paste(rep(c("F1-"),f1),rep(1:f1), sep="")
f2.trt=paste(rep(c("F2-"),f2),rep(1:f2), sep="")

#Numero de fatores dentro de fatores
f1.trt=rep(f1.trt,f2)
f2.trt=rep(f2.trt,f1)

f2.trt=f2.trt[order(f2.trt)]

#Combinando os dois fatores
trt=paste(f1.trt,f2.trt,sep="-")

#Obter as cores para o gráfico
coul=colorRampPalette(c(col.1, col.2))(length(trt))

trt.coul=paste(coul,trt,sep="")

densi=sample(rep(trt.coul,r))

mat.col.trt=matrix(densi, nrow = length(trt), ncol=r, byrow = TRUE)

#Retirar informações da matrix
mat.lbl=substring(mat.col.trt,8,last=20)

#Condição de cores
if (graph.col==TRUE){ mat.col=substr(mat.col.trt,1,7)}
else {mat.col=rep(c("white"),length(densi))}

mat = matrix(rep(1:(length(trt)*r),1), nrow = length(trt), ncol=r, byrow = TRUE)

#Obter o gráfico
par(mfcol=c(length(trt),r), mai=c(0.1,0,0,0), omi=c(0,0,0,0))

txt.factors=paste("F1=",fac.names[1],"   |   ","F2=",fac.names[2], sep="")

#Print os labels e cores no gráfico
for (i in 1:length(mat)) { 
barplot(20, xaxt='n', yaxt='n', ann=FALSE, ylim=c(0,20), bty="o",mai=c(0,0,0,0), omi=c(0,0,0,0), col=mat.col[i])
txt=c(mat.lbl[i])
text(1,10,txt,adj=1, cex=1.5, pos=2)
text(1,18, txt.factors, adj=1, cex=1, pos=2)
}

if (save==TRUE){ dev.copy(tiff, "des.fat2.crd.tiff", width=200*r, height=60*f1*f2, res=100)
dev.off() 
}


#Obter outputs.
y.f1=rep(f1.trt, r)
y.f1=y.f1[order(y.f1)]
y.f2=rep(f2.trt, r)
rept=rep(1:r, (f1*f2))

book=data.frame(factor1=y.f1, factor2=y.f2, rep=rept)

print(mat.lbl)
list(Desing=mat.lbl, Book=book)
}

