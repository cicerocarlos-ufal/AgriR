#Densing for CRD
des.rbd <- function(trt, rep, col.1="gray",col.2="white", graph.col=TRUE, save=FALSE){

t=trt
r=rep
#-------labes and colors--------------
trt=paste(rep(c("T"),t),rep(1:t), sep="")
coul=colorRampPalette(c(col.1, col.2))(t)
trt.col=paste(coul,trt,sep="")


#-------Samples-----------
mtr <- sample(trt.col, t , replace = FALSE)

block <- c(rep(1, t))

for (y in 2:r) {
block <- c(block, rep(y, t))
mtr <- c(mtr, sample(trt.col, t, replace = FALSE))
}	
#----------------------------------
#Print
mat.col.trt= matrix(mtr, nrow = length(trt), ncol=r, byrow = FALSE)

#Retirar informações da matrix
mat.lbl=substring(mat.col.trt,8,last=20)

#Condição de cores
if (graph.col==TRUE){mat.col=substr(mat.col.trt,1,7)
}
else {mat.col=rep(c("white"),length(mtr))
}

par(mfcol=c(length(trt),r), mai=c(0,0.2,0,0), omi=c(0,0,0,0))

#Print os labels e cores no gráfico
for (i in 1:length(mat.col.trt)) { 
barplot(20, xaxt='n', yaxt='n', ann=FALSE, ylim=c(0,20), bty="o",mai=c(0,0,0,0), omi=c(0,0,0,0), col=mat.col[i])

txt.block = paste(rep(c("block"),block[i]),c(block[i]),sep="-")

txt=c(mat.lbl[i])
text(0.8,10,txt,adj=1, cex=2, pos=2)
text(1,1,txt.block, adj=1, cex=1)
}

if (save==TRUE){ dev.copy(tiff, "des.rdb.tiff", width=200*r, height=200*t, res=100)
dev.off() 
}

#Obter outputs.

yy=c(mat.lbl)
yy=yy[order(yy)]
yyy=rep(1:r,t)

book=data.frame(treat=yy, Block=yyy)

print(mat.lbl)
list(Desing=mat.lbl, Book=book)
}



