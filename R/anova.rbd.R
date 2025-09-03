#' One factor Completely block Design
#'
#' \code{crd} Analyses balanced experiments in Completely
#' Randomized Design under one single factor, considering a
#' fixed model.
#' @param treat Numeric or complex vector containing the
#' treatments.
#' @param resp Numeric or complex vector containing the
#' response variable.
#' @param block numeric or complex vector containing the block.
#' @param quali Logic. If TRUE (default), the treatments are
#' assumed qualitative, if FALSE, quantitatives.
#' @param mcomp Allows choosing the multiple comparison test;
#' the \emph{default} is the test of Tukey, however, the
#' options are: the LSD test ('lsd'), the LSD test with
#' Bonferroni protection ('lsdb'), the test of Duncan
#' ('duncan'), the test of Student-Newman-Keuls ('snk'),
#' the test of Scott-Knot ('scottkenott').
#' @param sigF The signficance to be used for the F test of
#' ANOVA; the default is 5\%.
#' @return The output contains the ANOVA of the CRD, the
#' Shapiro-Wilk normality test for the residuals of the model,
#' the fitted regression models (when the treatments are
#' quantitative) and/or the multiple comparison tests (when
#' the treatments are qualitative).
#' FERREIRA, E. B.; CAVALCANTI, P. P. Funcao em codigo R para
#' analisar experimentos em DIC simples, em uma so rodada. In:
#' REUNIAO ANUAL DA REGIAO BRASILEIRA DA SOCIEDADE
#' INTERNACIONAL DE BIOMETRIA, 54./SIMPOSIO DE ESTATISTICA
#' APLICADA A EXPERIMENTACAO AGRONOMICA, 13., 2009, Sao Carlos.
#' Programas e resumos... Sao Carlos, SP: UFSCar, 2009. p. 1-5.
#' @author Eric B Ferreira,
#'  \email{eric.ferreira@@unifal-mg.edu.br}
#' @author Denismar Alves Nogueira
#' @author Portya Piscitelli Cavalcanti
#' @seealso \code{\link{fat2.crd}}, \code{\link{fat3.crd}},
#' \code{\link{split2.crd}}, \code{\link{fat2.ad.crd}} and
#' \code{\link{fat3.ad.crd}}.
#' @examples
#' @author Cícero Carlos de Souza Almeida
#' dados=read.table("R/data.rbd.csv", sep=";", header=T, dec=",")
#' attach(dados)
#' anova.rbd(trat,resp,block)
#' @export
anova.rbd <-function(treat, resp, block, quali=TRUE, graph=TRUE, graph.type="barplotb", graph.col="skyblue", xlab="Treatments", ylab="Variable", main="", graph.den=2, mcomp='tukey', best.fit=TRUE, sigF=0.05) 

{

Trat = factor(treat)
block=factor(block)
anova.temp = aov(resp~Trat+block)
tab = summary(anova.temp)

#for trat quantitative

r = as.numeric(table(Trat))

#Replications
t = length(levels(Trat))

#--------------Anova table------------------

colnames(tab[[1]])=c('DF','SS','MS','Fc','Pr>Fc')
#Adicionar o total

tab[[1]]=rbind(tab[[1]],apply(tab[[1]],2,sum))

rownames(tab[[1]]) = c('Treatament','Blocks','Residuals','Total')

tab[[1]][4,3]=NA

cat("\n")
cat("----------Variance Analysis----------------\n")
print(tab[[1]])


#----------------------CV------------------
#Adicionar o CV
cv = round(sqrt(tab[[1]][3,3])/mean(resp)*100, 0)
cv = data.frame(cv,c("%"))
colnames(cv)=c("","")
rownames(cv)=c("Coefficient of Variance")

cat("\n")
cat("----------Coefficient of Variance----------\n")
print(cv)
cat("\n")
#Unir os dados

#-----------------Normality test-------------

p.valor.shapiro<-shapiro.test(anova.temp$residuals)$p.value

if(p.valor.shapiro<0.05){

shap.no=c("WARNING: at 5% of significance, residuals can not be considered normal")}

else{
shap.ok=c("According to Shapiro-Wilk normality test at 5% of significance, residuals can be considered normal")}


#Teste de Tukey
fator=factor(treat)
if(nlevels(fator)==3 && quali==FALSE){ par(mfrow=c(1,1),font.lab=3, font.main=3)}
if(nlevels(fator)==4 && quali==FALSE){ par(mfrow=c(1,2),font.lab=3, font.main=3)}
if(nlevels(fator)>4 && quali==FALSE){ par(mfrow=c(1,3),font.lab=3, font.main=3)}
#-------------------means test----------------

#if(tab[[1]][1,5] < sigF) {

if(quali==TRUE) {


if (mcomp=='tukey') test=tukey(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sigF, graph, graph.type=graph.type, graph.col=graph.col, graph.den=graph.den,xlab=xlab,ylab=ylab) 	#Função ExpDes


if (mcomp=='duncan') test=duncan(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sigF, graph, graph.type=graph.type, graph.col=graph.col, graph.den=graph.den,xlab=xlab,ylab=ylab)

if (mcomp=='scottknott') test=scottknott(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sigF, graph, graph.type=graph.type, graph.col=graph.col, graph.den=graph.den,xlab=xlab,ylab=ylab) 

if (mcomp=='lsd') test=lsd(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sigF, graph, graph.type=graph.type, graph.col=graph.col, graph.den=graph.den,xlab=xlab,ylab=ylab) 

if (mcomp=='snk') test=snk(resp,Trat,tab[[1]][3,1],tab[[1]][3,2],sigF, graph, graph.type=graph.type, graph.col=graph.col, graph.den=graph.den,xlab=xlab,ylab=ylab) 

if (mcomp=='scheffe') test=scheffe(resp,Trat,tab[[1]][3,1],tab[[1]][3,3],tab[[1]][1,4],sigF, graph, graph.type=graph.type, graph.col=graph.col, graph.den=graph.den,xlab=xlab,ylab=ylab)



}


else {reg<-reg.poly(resp, treat, tab[[1]][3,1],tab[[1]][3,2], tab[[1]][1,1], tab[[1]][1,2],xlab=xlab,ylab=ylab, graph=graph, best.fit=best.fit)}

 # }

#else {
#    cat('\nAccording to the F test, the means can not be considered distinct.\n')
#    cat('------------------------------------------------------------------------\n')

#resultado quando não há significância no teste

##medias= tapply(resp,treat, mean)
#medias= data.frame(Treatments=names(medias), Means=medias)
#rownames(medias)=NULL
#test=medias

#cat('------------------------------------------------------------------------\n')
#}

#Result list


if(p.valor.shapiro<0.05) {

		if (quali==TRUE) {
		return(list(Anova=tab[[1]],Shapiro.test=p.valor.shapiro,Shapiro.result=shap.no,CV=cv, Mcomp=test))
		} else {
		return(list(Anova=tab[[1]],Shapiro.test=p.valor.shapiro,Shapiro.result=shap.no,CV=cv, Reg=reg))
		}
		
} else {
		if (quali==TRUE) {
		return(list(Anova=tab[[1]],Shapiro.test=p.valor.shapiro,Shapiro.result=shap.ok,CV=cv, Mcomp=test))
		} else {
		return(list(Anova=tab[[1]],Shapiro.test=p.valor.shapiro,Shapiro.result=shap.ok,CV=cv, Reg=reg))
		}
}
}


