#' Densing for One Facgtor Completely Randomized Design
#' @param trt Numeric containing the numbers of treataments
#' @param rep Numeric containing the numbers of replications
#' @param col.1 Allows choosing the color 1
#' @param col.2 Allows choosing the color 2
#' @param graph.col Logic. If TRUE (default), plot the graph
#' @param save Logic. If TRUE (FALSE is default), save the Desing using the file names: "des.crd.tiff"
#' @examples
#' des.crd(trt=4, rep=3)
#' @export
des.crd <- function(trt, rep, col.1="gray",col.2="white", graph.col=TRUE, save=FALSE){

t=trt
r=rep

trt=paste(rep(c("T"),t),rep(1:t), sep="")

coul=colorRampPalette(c(col.1, col.2))(t)


trt.col=paste(coul,trt,sep="")


densi=sample(rep(trt.col,r))

mat.col.trt=matrix(densi, nrow = length(trt), ncol=r, byrow = TRUE)

#Retirar informações da matrix
mat.lbl=substring(mat.col.trt,8,last=20)

#Condição de cores
if (graph.col==TRUE){ mat.col=substr(mat.col.trt,1,7)}
else {mat.col=rep(c("white"),length(densi))}

mat = matrix(rep(1:(length(trt)*r),1), nrow = length(trt), ncol=r, byrow = TRUE)

#Obter o gráfico

par(mfcol=c(length(trt),r), mai=c(0.1,0,0,0), omi=c(0,0,0,0))

#Print os labels e cores no gráfico
for (i in 1:length(mat)) { 
barplot(20, xaxt='n', yaxt='n', ann=FALSE,  bty="o",mai=c(0,0,0,0), omi=c(0,0,0,0), col=mat.col[i])
txt=c(mat.lbl[i])
text(0.8,10,txt,adj=1, cex=2, pos=2)
}

if (save==TRUE){ dev.copy(tiff, "des.crd.tiff", width=200*r, height=200*t, res=100)
dev.off() 
}

#Obter outputs.

yy=c(mat.lbl)
yy=yy[order(yy)]
yyy=rep(1:r,t)

book=data.frame(treat=yy, rep=yyy)

print(mat.lbl)
list(Desing=mat.lbl, Book=book)

}

