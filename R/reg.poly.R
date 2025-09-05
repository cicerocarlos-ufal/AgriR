#' Polinomial Regression
#'
#' \code{reg.poly} Fits sequential regression models until the
#' third power.
#' @param treat Numeric or complex vector containing the
#' treatments.
#' @param resp Numeric or complex vector containing the
#' response variable.
#' @param DFerror Error degrees of freedom.
#' @param SSerror Error sum of squares.
#' @param DFtreat Treatments' dregrees of freedom.
#' @param SStreat Treatments' sum of squares.
#' @return Returns coefficients, significance and ANOVA of the
#' fitted regression models.
#' @references GOMES, F. P. Curso de Estatistica Experimental.
#' 10a ed. Piracicaba: ESALQ/USP. 1982. 430.
#' @author Eric B Ferreira,
#'  \email{eric.ferreira@@unifal-mg.edu.br}
#' @author Denismar Alves Nogueira
#' @author Portya Piscitelli Cavalcanti
#' @seealso \code{\link{graphics}}.
#' @export

reg.poly <-
function(resp, treat, DFerror, SSerror, DFtreat, SStreat, main="", xlab="", ylab="", graph=TRUE, best.fit=FALSE) {

MSerror<-SSerror/DFerror

cat('\nAdjustment of polynomial models of regression\n')

X<-matrix(1,length(treat),4)
X[,2]<-treat
X[,3]<-treat^2
X[,4]<-treat^3


mean.table<-data.frame(tapply(resp,treat,mean))
mean.table$Levels=rownames(mean.table)
colnames(mean.table)<-c('Observed Means','Levels')

#------------------------------------------DAQUI PRA FRENTE OK--------------------------
aov.m1<-anova(lm(resp~treat))
if(dim(mean.table)[1]==2){r2m1<-1}

if(dim(mean.table)[1]>2) {r2m1<-aov.m1[1,2]/SStreat}

#ANOVA of linear regression
nomes1<-c("Linear Effect","Lack of fit","Residuals")
anava1<-data.frame('DF'=c(1,(gld=c(DFtreat-1)),(glr=DFerror)),
                  'SS'=c(round(c(aov.m1[[2]][1],(sqd=c(SStreat-aov.m1[[2]][1])),SSerror),5)),
                  'MS'=c(round(c(aov.m1[[3]][1],(if(gld==0){qmd=0} else{qmd=(sqd/gld)}), MSerror),5)),
                  'Fc'=c(round(c((fcl=aov.m1[[3]][1]/MSerror),(fc=qmd/MSerror)),2),''),
                  'p-value'=c(round(c(pf(fcl,1,glr,lower.tail=FALSE),(if(gld==0){pv=1}else{pv=1-pf(fc,gld,glr)})),5),''))
rownames(anava1)<-nomes1

#--------------------------INICIO DOS GRÁFICOS--------------------------
#if(dim(mean.table)[1]==3){ par(mfrow=c(1,1),font.lab=3, font.main=3)}
#if(dim(mean.table)[1]==4){ par(mfrow=c(1,2),font.lab=3, font.main=3)}
#if(dim(mean.table)[1]>4){ par(mfrow=c(1,3),font.lab=3, font.main=3)}



#Linear equation
lm1=lm(resp~treat)
#Gráfico


pvalue1=pf(fcl,1,glr,lower.tail=FALSE)

if (pvalue1<0.05){
 sig.l=c("*")
 } else {
 sig.l=c("")
 }

if (graph==TRUE){

#lin=curve(lm1$coefficients[1]+lm1$coefficients[2]*x,min(treat), max(treat), ylim=c(min(resp), max(resp)), xlab=xlab, ylab=ylab, col="red", main=main)
xm1=seq.int(min(treat),max(treat),length.out = 100)
ym1=lm1$coefficients[1]+lm1$coefficients[2]*xm1

A=sprintf("%.4f", lm1$coefficients[1])
B=sprintf("%+.4f",lm1$coefficients[2])
r2l=sprintf("%.3f",r2m1)

equation1=bquote(italic("Y" ==.(A)~.(B)~.(sig.l)~ "X  |  R"^2~"="~.(r2l)))

#mtext(equation1,side=3, cex=1)
#points(mean.table[,2],mean.table[,1])
}
#-----------------------------------------linear ok------------------------------------------

###############################################################################


if(dim(mean.table)[1]>3) {

t2<-treat^2
aov.m2<-anova(lm(resp~treat+t2))
if(dim(mean.table)[1]==3){r2m2<-1}
if(dim(mean.table)[1]>3) {r2m2<-(aov.m2[1,2]+aov.m2[2,2])/SStreat}

#ANOVA of quadratic regression
nomes2<-c("Linear Effect","Quadratic Effect","Lack of fit","Residuals")
anava2<-data.frame('DF'=c(aov.m2[[1]][1:2],(gld=c(DFtreat-2)),(glr=DFerror)),
                  'SS'=c(round(c(aov.m2[[2]][1:2],(sqd=c(SStreat-sum(aov.m2[[2]][1:2]))),SSerror),5)),
                  'MS'=c(round(c(aov.m2[[3]][1:2],(if(gld==0){qmd=0} else{qmd=(sqd/gld)}), MSerror),5)),
                  'Fc'=c(round(c((fcl=aov.m2[[3]][1:2]/MSerror),(fc=qmd/MSerror)),2),''),
                  'p-value'=c(round(c(pf(fcl,1,glr,lower.tail=FALSE),(if(gld==0){pv=1}else{pv=1-pf(fc,gld,glr)})),5),''))
rownames(anava2)<-nomes2


#quadratic
lm2=lm(resp~treat+t2)

#Gráfico
pvalue2=pf(fcl,1,glr,lower.tail=FALSE)

if (pvalue2[1]<0.05){
 sig.l=c("*")
 } else {
 sig.l=c("")
}
 
if (pvalue2[2]<0.05){
 sig.q=c("*")
 } else {
 sig.q=c("")
 }

#quad=curve(lm2$coefficients[1]+lm2$coefficients[2]*x + lm2$coefficients[3]*x^2,min(treat), max(treat), ylim=c(min(resp), max(resp)), xlab=xlab, ylab=ylab, col="red", main=main)

xm2=seq.int(min(treat),max(treat),length.out = 100)
ym2=lm2$coefficients[1]+lm2$coefficients[2]*xm2 + lm2$coefficients[3]*xm2^2

#A=paste(round(lm2$coefficients[1],2))
#B=paste(round(lm2$coefficients[2],3))
#C=paste(round(lm2$coefficients[3],3))

D=sprintf("%.4f", lm2$coefficients[1])
E=sprintf("%+.4f", lm2$coefficients[2])
F=sprintf("%+.4f", lm2$coefficients[3])

r2q=sprintf("%.3f",r2m2)

equation2=bquote(italic("Y" == .(D)~.(E)~.(sig.l)~"X"~.(F)~.(sig.q)~"X"^2 ~" |  R"^2~"="~.(r2q)))

#eq=substitute((A + B + C), list(A = A, B=B,C=C))

#mtext(equation2,side=3, cex=1)
#points(mean.table[,2],mean.table[,1])

}



###############################################################################
if(dim(mean.table)[1]>4) {
###############################################################################
#cubic regression
###############################################################################

t3<-treat^3
aov.m3<-anova(lm(resp~treat+t2+t3))
if(dim(mean.table)[1]==4){r2m3<-1}
if(dim(mean.table)[1]>4) {r2m3<-(aov.m3[1,2]+aov.m3[2,2]+aov.m3[3,2])/SStreat}

#ANOVA of cubic regression
nomes3<-c("Linear Effect","Quadratic Effect","Cubic Effect","Lack of fit","Residuals")
anava3<-data.frame('DF'=c(aov.m3[[1]][1:3],(gld=c(DFtreat-3)),(glr=DFerror)),
                  'SS'=c(round(c(aov.m3[[2]][1:3],(sqd=c(SStreat-sum(aov.m3[[2]][1:3]))),SSerror),5)),
                  'MS'=c(round(c(aov.m3[[3]][1:3],(if(gld==0){qmd=0} else{qmd=(sqd/gld)}), MSerror),5)),
                  'Fc'=c(round(c((fcl=aov.m3[[3]][1:3]/MSerror),(fc=qmd/MSerror)),2),''),
                  'p-value'=c(round(c(pf(fcl,1,glr,lower.tail=FALSE),(if(gld==0){pv=1} else {pv=1-pf(fc,gld,glr)})),5),''))
rownames(anava3)<-nomes3



#Cúbica
lm3=lm(resp~treat+t2+t3)

#Gráfico
pvalue3=pf(fcl,1,glr,lower.tail=FALSE)

if (pvalue3[1]<0.05){
 sig.l=c("*")
 } else {
 sig.l=c("")
}
 
if (pvalue3[2]<0.05){
 sig.q=c("*")
 } else {
 sig.q=c("")
 }
if (pvalue3[3]<0.05){
 sig.c=c("*")
 } else {
 sig.c=c("")
 }

#cubi=curve(lm3$coefficients[1]+lm3$coefficients[2]*x + lm3$coefficients[3]*x^2+lm3$coefficients[4]*x^3,min(treat), max(treat), ylim=c(min(resp), max(resp)), xlab=xlab, ylab=ylab, col="red", main=main)

xm3=seq.int(min(treat),max(treat),length.out = 100)
ym3=lm3$coefficients[1]+lm3$coefficients[2]*xm3 + lm3$coefficients[3]*xm3^2+lm3$coefficients[4]*xm3^3

G=sprintf("%.4f", lm3$coefficients[1])
H=sprintf("%+.4f", lm3$coefficients[2])
I=sprintf("%+.4f", lm3$coefficients[3])
K=sprintf("%+.4f", lm3$coefficients[4])

r2c=sprintf("%.3f",r2m3)

equation3=bquote(italic("Y" == .(G)~.(H)~.(sig.l)~"X"~.(I)~.(sig.q)~ "X"^2 ~.(K)~.(sig.c) ~ "X"^3 ~" |  R"^2~"="~.(r2c)))

#eq=substitute((A + B + C), list(A = A, B=B,C=C))

#mtext(equation3,side=3, cex=1)
#points(mean.table[,2],mean.table[,1])
}


#Montar o gr?fico com todos os modelos

if (best.fit==FALSE){
plot(mean.table[,2],mean.table[,1],xlab=xlab, ylab=ylab, pch=16, sub=main)
lines(xm1,ym1, lwd=2)
mtext(equation1, side=3, line=0,cex=0.8)

if(dim(mean.table)[1]>3){
lines(xm2,ym2, col="red", lwd=2)
mtext(equation2, side=3, cex=0.8, line=1, col="red")}

if(dim(mean.table)[1]>4){
  lines(xm3,ym3, col="blue", lwd=2)
  mtext(equation3, side=3, line=2, cex=0.8, col="blue")}
}


#Condições de escolhas

#Para cubibo
if (best.fit==TRUE){
  plot(mean.table[,2],mean.table[,1],xlab=xlab, ylab=ylab, pch=16, sub=main)

if(dim(mean.table)[1]>4) {

  
if (pvalue3[3]<0.05){
		cat("\n----------------------------------------\n")
		cat("\n---------MELHOR MODELO: CÚBICO----------\n")
		cat("\n----------------------------------------\n")
		
		lines(xm3,ym3, col="blue", lwd=2)
		mtext(equation3, side=3, line=0, cex=0.8, col="blue")
		
		 } else {
			if (pvalue2[2]<0.05){
			cat("\n----------------------------------------\n")
			cat("\n-------MELHOR MODELO: QUADRÁTICO--------\n")
			cat("\n----------------------------------------\n")
			lines(xm2,ym2, col="red", lwd=2)
			mtext(equation2, side=3, line=0, cex=0.8,  col="red")
			
			} else {
				if (pvalue1<0.05){
				cat("\n----------------------------------------\n")
				cat("\n---------MELHOR MODELO: LINEAR----------\n")
				cat("\n----------------------------------------\n")
				lines(xm1,ym1, lwd=2)
				mtext(equation1, side=3, line=0,cex=0.8)
				
				 } else {
 					cat("\n----------------------------------------\n")
					cat("\n----------MELHOR MODELO: NENHUM----------\n")
					cat("\n----------------------------------------\n")
					 }
				}
			}
	}

#Para Quadrático
if(dim(mean.table)[1]==4) {

if (pvalue2[2]<0.05){
			cat("\n----------------------------------------\n")
			cat("\n-------MELHOR MODELO: QUADRÁTICO--------\n")
			cat("\n----------------------------------------\n")
			lines(xm2,ym2, col="red", lwd=2)
			mtext(equation2, side=3, cex=0.8, line=0, col="red")
			} else {
				if (pvalue1<0.05){
				cat("\n----------------------------------------\n")
				cat("\n---------MELHOR MODELO: LINEAR----------\n")
				cat("\n----------------------------------------\n")
				lines(xm1,ym1, lwd=2)
				mtext(equation1, side=3, line=0,cex=0.8)
				
				 } else {
 					cat("\n----------------------------------------\n")
					cat("\n----------MELHOR MODELO: NENHUM----------\n")
					cat("\n----------------------------------------\n")
					 }
				}
			}

#Para linear
if(dim(mean.table)[1]==3) {
if (pvalue1<0.05){
		cat("\n----------------------------------------\n")
		cat("\n---------MELHOR MODELO: LINEAR----------\n")
		cat("\n----------------------------------------\n")
		lines(xm1,ym1, lwd=2)
		mtext(equation1, side=3, line=0,cex=0.8)
	 } else {
 		cat("\n----------------------------------------\n")
		cat("\n----------MELHOR MODELO: NENHUM----------\n")
		cat("\n----------------------------------------\n")
	 }
}
}

 if(dim(mean.table)[1]>4) {
return(list(Anova.Linear=anava1,R2.Linear=r2m1,Anova.Quad=anava2, R2.Quad= r2m2, Anova.Cub=anava3, R2.Cub=r2m3))
}
 if(dim(mean.table)[1]==4) {
return(list(Anova.Linear=anava1,R2.Linear=r2m1,Anova.Quad=anava2, R2.Quad= r2m2))
}
if(dim(mean.table)[1]==3) {
return(list(Anova.Linear=anava1,R2.Linear=r2m1))
}

}
