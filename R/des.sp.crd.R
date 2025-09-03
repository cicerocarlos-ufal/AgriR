#Densing for CRD
des.sp.crd <- function(factor1, factor2, rep, col.1="gray",col.2="white", graph.col=TRUE,fac.names=c('F1','F2'), save=FALSE){

f1=factor1
f2=factor2
r=rep 
#Nomes para os fatores
f1.trt=paste(rep(c("F1-"),f1),rep(1:f1), sep="")
f2.trt=paste(rep(c("F2-"),f2),rep(1:f2), sep="")

coul=colorRampPalette(c(col.1, col.2))(length(f2.trt))

f2.trt.coul=paste(coul,f2.trt,sep="")

parc=sample(rep(f1.trt,r))

subpar=NULL


#subpar=paste(rep(parc[1],length(f2.trt)),sample(f2.trt.coul), sep=" | ")

##
for (i in 1:length(parc)) {
subpar[i]=data.frame(paste(rep(parc[i],length(f2.trt)),sample(f2.trt.coul), sep=" | "))
#names(subpar)=c(parc)
}
#Obter os labels
subpar.lbl=as.data.frame(subpar)
names(subpar.lbl)=c(parc)
subpar.lbl=as.matrix(subpar.lbl)
subpar.lbl=substring(subpar.lbl,15,last=20)

if (graph.col==TRUE){
#Obter as cores
subpar.coul=as.data.frame(subpar)
names(subpar.coul)=c(parc)
subpar.coul=as.matrix(subpar.coul)
subpar.coul=substr(subpar.coul,start=8,stop=14)
}
else {
subpar.coul=NULL

for (i in 1:length(parc)) {
subpar.coul[i]=data.frame(rep(c("white"),f2))
}

subpar.coul=as.data.frame(subpar.coul)
names(subpar.coul)=c("cor")
}

#Grafico

par(mfcol=c(f1,r), mai=c(0.1,0,0,0), omi=c(0,0,0,0))

text.pos=rep(1:f2)*13

txt.factors=paste("F1=",fac.names[1],"   |   ","F2=",fac.names[2], sep="")

my_gr=NULL

for (i in 1:length(parc)){

#my_gr=barplot(as.matrix(table(subpar[i])*15), xaxt='n', yaxt='n', ann=FALSE, bty="o",mai=c(0,0,0,0), omi=c(0,0,0,0), col=subpar.coul[,i], legend=subpar.lbl[,i])

my_gr=barplot(as.matrix(table(subpar[i])*15), xaxt='n', yaxt='n', ann=FALSE, bty="o",mai=c(0,0,0,0), omi=c(0,0,0,0), col=subpar.coul[,i])

#text parcela
text(my_gr,text.pos,rep(parc[i],f2),adj=2, cex=1.5, pos=2)

#text subpar
text(my_gr,text.pos, subpar.lbl[,i],adj=1, cex=1.5, pos=4, col="blue")

#text fac names

text(my_gr,4+f2,txt.factors,adj=0.5, cex=1, pos=1)

}

if (save==TRUE){ dev.copy(tiff, "des.sp.crd.tiff", width=200*r, height=60*f1*f2, res=100)
dev.off() 
}

#Obter outputs.
y.f1=rep(f1.trt, r)
y.f1=y.f1[order(y.f1)]
y.f2=rep(f2.trt, r)
rept=rep(1:r, (f1*f2))

book=data.frame(factor1=y.f1, factor2=y.f2, rep=rept)

print(subpar.lbl)
list(Desing=subpar.lbl, Book=book)
}

