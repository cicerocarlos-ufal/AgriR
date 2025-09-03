#' Split-plots in CRD
#'
#' \code{split2.crd} Analyses experiments in Split-plot scheme
#' in balanced Completely Randomized Design, considering a
#' fixed model.
#' @param factor1 Numeric or complex vector containing the
#' factor 1 levels.
#' @param factor2 Numeric or complex vector containing the
#' factor 2 levels.
#' @param repet Numeric or complex vector containing the
#' replications.
#' @param resp Numeric or complex vector containing the
#' response variable.
#' @param quali Logic. If TRUE (default), the treatments
#' are assumed qualitative, if FALSE, quantitatives.
#' @param mcomp Allows choosing the multiple comparison
#' test; the \emph{default} is the test of Tukey, however,
#' the options are: the LSD test ('lsd'), the LSD test
#' with Bonferroni protection ('lsdb'), the test of Duncan
#' ('duncan'), the test of Student-Newman-Keuls ('snk'),
#' the test of Scott-Knott ('sk'), the Calinski and
#' Corsten test ('ccF') and bootstrap multiple comparison's
#' test ('ccboot').
#' @param fac.names Allows labeling the factors 1 and 2.
#' @param sigT The signficance to be used for the multiple
#' comparison test; the default is 5\%.
#' @param sigF The signficance to be used for the F test
#' of ANOVA; the default is 5\%.
#' @details The arguments sigT and mcomp will be used only
#' when the treatment are qualitative.
#' @return The output contains the ANOVA of the referred
#' CRD, the Shapiro-Wilk normality test for the residuals
#' of the model, the fitted regression models (when the
#' treatments are quantitative) and/or the multiple
#' comparison tests (when the treatments are qualitative).
#' @references BANZATTO, D. A.; KRONKA, S. N.
#' Experimentacao Agricola. 4 ed. Jaboticabal: Funep.
#' 2006. 237 p.
#' @author Eric B Ferreira,
#'  \email{eric.ferreira@@unifal-mg.edu.br}
#' @author Denismar Alves Nogueira
#' @author Portya Piscitelli Cavalcanti
#' @note The \code{\link{graphics}} can be used to
#' construct regression plots and \code{\link{plotres}}
#' for residuals plots.
#' @seealso \code{\link{split2.rbd}} and \code{\link{strip}}.
#' @examples
#' data(ex9)
#' attach(ex9)
#' split2.crd(cobertura, prof, rep, pH, quali = c(TRUE, TRUE),
#' mcomp = "lsd", fac.names = c("Cover", "Depth"), sigT = 0.05,
#' sigF = 0.05)
#' @export

anova.sp2.crd <-
function(factor1, factor2, repet, resp, quali=c(TRUE,TRUE), fac.fix="fac.A", fac.names=c('fac.A','fac.B'),graph=TRUE, graph.type="barplotb", graph.col="skyblue", xlab="Treatments", ylab="Variable", graph.den=2, mcomp='tukey', sigF=0.05) 
{

cat('------------------------------------------------------------------------\nLegend:\n')
cat('FACTOR 1    (plot): ',fac.names[1],'\n')
cat('FACTOR 2 (split-plot): ',fac.names[2],'\n------------------------------------------------------------------------\n\n')

cont<-c(1,3)
Fator1<-factor(factor1)
Fator2<-factor(factor2)
repet<-factor(repet)
nv1<-length(summary(Fator1))   #Diz quantos niveis tem o fator 1.
nv2<-length(summary(Fator2))   #Diz quantos niveis tem o fator 2.

anava<-aov(resp ~ Fator1*Fator2 + (Fator1:repet))
tab1<-summary(anava)
#tab1$'Error: Fator1'[[1]]<-cbind(tab1$'Error: Fator1'[[1]], tab1$'Error: Fator1'[[1]][1,3]/tab1$'Error: Fator1:repet'[[1]][1,3])
#tab1$'Error: Fator1'[[1]]<-cbind(tab1$'Error: Fator1'[[1]], 1-pf(tab1$'Error: Fator1'[[1]][1,4],
#  tab1$'Error: Fator1'[[1]][1,1],tab1$'Error: Fator1:repet'[[1]][1,1]))
tab1[[1]]<-rbind(tab1[[1]][1,],tab1[[1]][4,],tab1[[1]][2,],tab1[[1]][3,],tab1[[1]][5,])
colnames(tab1[[1]])<-c('DF', 'SS', 'MS', 'Fc', 'Pr>Fc')
tab<-tab1[[1]]

#par<- data.frame('DF'=tab1[[1]][1,1]+tab1[[1]][2,1], 'SS'=tab1[[1]][1,2]+tab1[[1]][2,2], 'MS'=NA,'Fc'=NA,'Pr>Fc'=NA)
#rownames(par)=c("Plot")

tab<-rbind(tab,apply(tab,2,sum))

rownames(tab)<-c(fac.names[1],'Error a',fac.names[2],paste(fac.names[1],'*',fac.names[2],sep=''),'Error b','Total')
tab[1,4]<-tab[1,3]/tab[2,3]
tab[1,5]<-1-pf(tab[1,4],tab[1,1],tab[2,1])
tab<-round(tab,6)

tab[6,3:5]<-tab[2,4:5]<-NA

print(tab)
#-------------------CV-------------------
cv1=round(sqrt(tab[2,3])/mean(resp)*100,2)
cv1=data.frame(cv1,c("%"))
colnames(cv1)=c("","")
rownames(cv1)=c("Coefficient of Variance (Plot)")

cv2=round(sqrt(tab[5,3])/mean(resp)*100,2)
cv2=data.frame(cv2,c("%"))
colnames(cv2)=c("","")
rownames(cv2)=c("Coefficient of Variance (SplitPlot)")


#output<-list('Analysis of Variance Table' = tab)
#Print the CV

cat("\n")
cat("----------Coefficient of Variance (Plot)----------\n")
print(cv1)
cat("\n")

cat("----------Coefficient of Variance (SplitPlot)----------\n")
print(cv2)
cat("\n")


fatores<-data.frame('fator 1' = factor1,'fator 2' = factor2)

###############################################################################################################
#Teste de normalidade
#pvalor.shapiro<-shapiro.test(anava$residuals)$p.value
#cat('\n------------------------------------------------------------------------
#Teste de normalidade dos residuos (Shapiro-Wilk)\n')
#cat('p-valor: ',pvalor.shapiro, '\n')
#if(pvalor.shapiro<0.05){cat('ATENCAO: a 5% de significancia, os residuos nao podem ser considerados normais!
#------------------------------------------------------------------------\n')}
#else{cat('De acordo com o teste de Shapiro-Wilk a 5% de significancia, os residuos podem ser considerados normais.
#------------------------------------------------------------------------\n')}
#Teste de normalidade

#p.valor.shapiro<-shapiro.test(anava$residuals)$p.value

#if(p.valor.shapiro<0.05){

#shap.no=c("WARNING: at 5% of significance, residuals can not be considered normal")}

#else{
#shap.ok=c("According to Shapiro-Wilk normality test at 5% of significance, residuals can be considered normal")}


test=NULL
reg=NULL


#Para interacao nao significativa, fazer...
if(as.numeric(tab[4,5])>sigF) {

cat('\nNo significant interaction: analyzing the main effects
------------------------------------------------------------------------\n')


#-----------------divisão dos gráficos-----------------
par(mfcol=c(1,2))
  

for(i in 1:2){

#Para os fatores QUALITATIVOS, teste de medias
if(quali[i]==TRUE) {

    cat(fac.names[i])

    if(mcomp=='tukey'){
#tukey(resp,fatores[,i],as.numeric(tab[cont[i]+i,1]), as.numeric(tab[cont[i]+i,2]),sigT)
test[[i]]=tukey(resp,fatores[,i],as.numeric(tab[cont[i]+i,1]),as.numeric(tab[cont[i]+i,2]),sigF, graph, graph.type=graph.type, graph.col=graph.col, graph.den=graph.den,xlab=xlab,ylab=ylab)
                    }
  if(mcomp=='duncan'){
#duncan(resp,fatores[,i],as.numeric(tab[cont[i]+i,1]),as.numeric(tab[cont[i]+i,2]),sigT)
test[[i]]=duncan(resp,fatores[,i],as.numeric(tab[cont[i]+i,1]),as.numeric(tab[cont[i]+i,2]),sigF, graph, graph.type=graph.type, graph.col=graph.col, graph.den=graph.den,xlab=xlab,ylab=ylab)
                    }
  if(mcomp=='lsd'){
#lsd(resp,fatores[,i],as.numeric(tab[cont[i]+i,1]),as.numeric(tab[cont[i]+i,2]),sigT)
test[[i]]=lsd(resp,fatores[,i],as.numeric(tab[cont[i]+i,1]),as.numeric(tab[cont[i]+i,2]),sigF, graph, graph.type=graph.type, graph.col=graph.col, graph.den=graph.den,xlab=xlab,ylab=ylab)
                    }
  if(mcomp=='scottknott'){
#scottknott(resp,fatores[,i],as.numeric(tab[cont[i]+i,1]),as.numeric(tab[cont[i]+i,2]),sigT)
test[[i]]=scottknott(resp,fatores[,i],as.numeric(tab[cont[i]+i,1]),as.numeric(tab[cont[i]+i,2]),sigF, graph, graph.type=graph.type, graph.col=graph.col, graph.den=graph.den,xlab=xlab,ylab=ylab)  
                  }
  if(mcomp=='snk'){
#snk(resp,fatores[,i],as.numeric(tab[cont[i]+i,1]),as.numeric(tab[cont[i]+i,2]),sigT)
test[[i]]=snk(resp,fatores[,i],as.numeric(tab[cont[i]+i,1]),as.numeric(tab[cont[i]+i,2]),sigF, graph, graph.type=graph.type, graph.col=graph.col, graph.den=graph.den,xlab=xlab,ylab=ylab)  
                  }

  if (mcomp=='scheffe') {
test[[i]]=scheffe(resp,fatores[,i],as.numeric(tab[cont[i]+i,1]),as.numeric(tab[cont[i]+i,3]),as.numeric(tab[cont[i],3]),sigF, graph, graph.type=graph.type, graph.col=graph.col, graph.den=graph.den,xlab=xlab,ylab=ylab)
}
 }

#Para os fatores QUANTITATIVOS, regressao
if(quali[i]==FALSE){
    cat(fac.names[i])
  reg[[i]]=reg.poly(resp, fatores[,i], tab[cont[i]+i,1], as.numeric(tab[cont[i]+i,2]), as.numeric(tab[cont[i],1]), as.numeric(tab[cont[i],2]))
}

cat('\n')
}		#for

}		#if


#-----------------------------------------------------------------------------
#Se a interacao for significativa, desdobrar a interacao

if(as.numeric(tab[4,5])<=sigF) {
cat("\n\n\nSignificant interaction: analyzing the interaction
------------------------------------------------------------------------\n")

if (fac.fix=='fac.A'){
    
    
#Desdobramento de FATOR 1 dentro dos niveis de FATOR 2
cat("\nAnalyzing ", fac.names[1], ' inside of each level of ', fac.names[2], '
------------------------------------------------------------------------\n')
  if (nv2<=3){par(mfrow=c(nv2,1),font.lab=3, font.main=3)}
  if (nv2>3){par(mfrow=c(nv2/2+1,2),font.lab=3, font.main=3)}
  
#Somas de quadrados do fator 1 dentro dos niveis de fator 2
l2<-names(summary(Fator2))

sq<-numeric(0)

for(k in 1:nv2) {
soma<-numeric(0)
for(j in 1:nv1) {
sub<-resp[Fator1==levels(Fator1)[j] & Fator2==levels(Fator2)[k]]
q.som<-length(sub)
soma<-c(soma, sum(sub))
                 }
sq<-c(sq, sum(soma^2)/q.som - sum(soma)^2/(q.som*length(soma)))
                 }
gl.sattert<-(as.numeric(tab[2,3])+(nv2-1)*as.numeric(tab[5,3]))^2/((as.numeric(tab[2,3])^2/as.numeric(tab[2,1])) + (((nv2-1)*as.numeric(tab[5,3]))^2/
as.numeric(tab[5,1])))
gl.f1f2<-c(rep(nv1-1,nv2),gl.sattert)
sq<-c(sq, NA)
qm.f1f2<-sq[1:nv2]/gl.f1f2[1:nv2]
qm.ecomb<-(as.numeric(tab[2,3])+(nv2-1)*as.numeric(tab[5,3]))/nv2
qm.f1f2<-c(qm.f1f2,qm.ecomb)
fc.f1f2<-c(qm.f1f2[1:nv2]/qm.f1f2[nv2+1],NA)
p.f1f2<-c(1-pf(fc.f1f2,gl.f1f2,gl.sattert))
tab.f1f2<-data.frame('DF'=gl.f1f2,'SS'=sq,'MS'=qm.f1f2,'Fc'=fc.f1f2, 'p-value'=p.f1f2)
nome.f1f2<-numeric(0)
for(j in 1:nv2){
nome.f1f2<-c(nome.f1f2, paste(fac.names[1], ' : ', fac.names[2],' ',l2[j],' ',sep=''))
                }
nome.f1f2<-c(nome.f1f2,'Pooled Error')
rownames(tab.f1f2)<-nome.f1f2
tab.f1f2<-round(tab.f1f2,6)
tab.f1f2[nv2+1,2]<-tab.f1f2[nv2+1,3]*tab.f1f2[nv2+1,1]
tab.f1f2[nv2+1,5]<-tab.f1f2[nv2+1,4]<-NA
print(tab.f1f2)
    cat('------------------------------------------------------------------------\n\n')


for(i in 1:nv2) {

    cat('\n',fac.names[1], 'inside of', fac.names[2], l2[i] )
    cat('\n------------------------------------------------------------------------')

  if(quali[1]==TRUE) {

    if(mcomp=='tukey'){
      test[[i]]=tukey(resp[fatores[,2]==l2[i]], fatores[,1][fatores[,2]==l2[i]], as.numeric(tab.f1f2[nv2+1,1]),as.numeric(tab.f1f2[nv2+1,2]),sigF, graph, graph.type=graph.type, graph.col=graph.col, graph.den=graph.den,xlab=xlab,ylab=ylab)
                      }

  if(mcomp=='duncan'){
    test[[i]]=duncan(resp[fatores[,2]==l2[i]],fatores[,1][fatores[,2]==l2[i]],as.numeric(tab.f1f2[nv2+1,1]),as.numeric(tab.f1f2[nv2+1,2]),sigF, graph, graph.type=graph.type, graph.col=graph.col, graph.den=graph.den,xlab=xlab,ylab=ylab)
                    }

  if(mcomp=='lsd'){
    test[[i]]=lsd(resp[fatores[,2]==l2[i]],fatores[,1][fatores[,2]==l2[i]],as.numeric(tab.f1f2[nv2+1,1]),as.numeric(tab.f1f2[nv2+1,2]),sigF, graph, graph.type=graph.type, graph.col=graph.col, graph.den=graph.den,xlab=xlab,ylab=ylab)
                    }

  if(mcomp=='scottknott'){
    test[[i]]=scottknott(resp[fatores[,2]==l2[i]],fatores[,1][fatores[,2]==l2[i]],as.numeric(tab.f1f2[nv2+1,1]),as.numeric(tab.f1f2[nv2+1,2]),sigF, graph, graph.type=graph.type, graph.col=graph.col, graph.den=graph.den,xlab=xlab,ylab=ylab)
                    }

  if(mcomp=='snk'){
    snk(resp[fatores[,2]==l2[i]],fatores[,1][fatores[,2]==l2[i]],as.numeric(tab.f1f2[nv2+1,1]),as.numeric(tab.f1f2[nv2+1,2]),sigF, graph, graph.type=graph.type, graph.col=graph.col, graph.den=graph.den,xlab=xlab,ylab=ylab)
                   }
                   
                   
                   
       }

if(quali[1]==FALSE) {
    reg.poly(resp[fatores[,2]==l2[i]], fatores[,1][fatores[,2]==l2[i]], as.numeric(tab.f1f2[nv2+1,1]),
    as.numeric(tab.f1f2[nv2+1,2]), as.numeric(tab.f1f2[i,1]), as.numeric(tab.f1f2[i,2]))
                                                   }

                        }
}

  if (fac.fix=='fac.B'){
#Desdobramento de FATOR 2 dentro dos niveis de FATOR 1
cat("\n\nAnalyzing ", fac.names[2], ' inside of each level of ', fac.names[1], '
------------------------------------------------------------------------\n')
    if (nv1<=3){par(mfrow=c(nv1,1),font.lab=3, font.main=3)}
    if (nv1>3){par(mfrow=c(nv1/2+1,2),font.lab=3, font.main=3)}
    
#Somas de quadrados do fator 2 dentro dos niveis de fator 1
l1<-names(summary(Fator1))

sq<-numeric(0)

for(k in 1:nv1) {
soma<-numeric(0)
for(j in 1:nv2) {
parc<-resp[Fator1==levels(Fator1)[k] & Fator2==levels(Fator2)[j]]
q.som<-length(parc)
soma<-c(soma, sum(parc))
                 }
sq<-c(sq, sum(soma^2)/q.som - sum(soma)^2/(q.som*length(soma)))
                 }
gl.f2f1<-c(rep(nv2-1,nv1),tab[5,1])
sq<-c(sq, as.numeric(tab[5,2]))
qm.f2f1<-sq/gl.f2f1
fc.f2f1<-c(qm.f2f1[1:nv1]/as.numeric(tab[5,3]),NA)
p.f2f1<-c(1-pf(fc.f2f1,gl.f2f1,as.numeric(tab[5,1])))
tab.f2f1<-data.frame('DF'=gl.f2f1,'SS'=sq,'MS'=qm.f2f1,'Fc'=fc.f2f1, 'p-value'=p.f2f1)
nome.f2f1<-numeric(0)
for(j in 1:nv1){
nome.f2f1<-c(nome.f2f1, paste(fac.names[2], ' : ', fac.names[1],' ',l1[j],' ',sep=''))
                }
nome.f2f1<-c(nome.f2f1,'Error b')
rownames(tab.f2f1)<-nome.f2f1
tab.f2f1<-round(tab.f2f1,6)
tab.f2f1[nv1+1,5]<-tab.f2f1[nv1+1,4]<-NA
print(tab.f2f1)
    cat('------------------------------------------------------------------------\n\n')


for(i in 1:nv1) {

    cat('\n',fac.names[2], 'inside of', fac.names[1], l1[i] )
    cat('\n------------------------------------------------------------------------')


  if(quali[2]==TRUE) {

    if(mcomp=='tukey'){
      test[[i]]=tukey(resp[fatores[,1]==l1[i]], fatores[,2][fatores[,1]==l1[i]], as.numeric(tab.f2f1[nv1+1,1]),as.numeric(tab.f2f1[nv1+1,2]),sigT)
                    }

  if(mcomp=='duncan'){
    test[[i]]=duncan(resp[fatores[,1]==l1[i]],fatores[,2][fatores[,1]==l1[i]],as.numeric(tab.f2f1[nv1+1,1]),as.numeric(tab.f2f1[nv1+1,2]),sigT)
                    }

  if(mcomp=='lsd'){
    test[[i]]=lsd(resp[fatores[,1]==l1[i]],fatores[,2][fatores[,1]==l1[i]],as.numeric(tab.f2f1[nv1+1,1]),as.numeric(tab.f2f1[nv1+1,2]),sigT)
                    }

  if(mcomp=='sk'){
    test[[i]]=scottknott(resp[fatores[,1]==l1[i]],fatores[,2][fatores[,1]==l1[i]],as.numeric(tab.f2f1[nv1+1,1]),as.numeric(tab.f2f1[nv1+1,2]),sigT)
                    }

  if(mcomp=='snk'){
    test[[i]]=snk(resp[fatores[,1]==l1[i]],fatores[,2][fatores[,1]==l1[i]],as.numeric(tab.f2f1[nv1+1,1]),as.numeric(tab.f2f1[nv1+1,2]),sigT)
                   }
  

    cat('------------------------------------------------------------------------\n\n')
                                                      }


  if(quali[2]==FALSE & as.numeric(tab.f2f1[i,5])<sigF){
    reg.poly(resp[fatores[,1]==l1[i]], fatores[,2][fatores[,1]==l1[i]], as.numeric(tab.f2f1[nv1+1,1]),
    as.numeric(tab.f2f1[nv1+1,2]), as.numeric(tab.f2f1[i,1]), as.numeric(tab.f2f1[i,2]))
  }
}
}
}
#----------------Return sem interações----------------------------
if (quali[1]==TRUE && quali[2]==TRUE && tab[4,5]> sigF){return(list(Mcomp=test))}
if (quali[1]==FALSE && quali[2]==TRUE && tab[4,5]> sigF){return(list(Reg=reg[1], Mcomp=test[2]))}
if (quali[1]==TRUE && quali[2]==FALSE && tab[4,5]> sigF){return(list(Mcomp=test[1], Reg=reg[2]))}
if (quali[1]==FALSE && quali[2]==FALSE && tab[4,5] > sigF){return(list(Reg=reg))}
#----------------Return com interações----------------------------
if (quali[1]==TRUE && quali[2]==TRUE && tab[4,5] <=sigF){return(list(Mcomp=test))}
if (quali[1]==FALSE && quali[2]==TRUE && tab[4,5]){return(list(Reg=reg, Mcomp=test))}
if (quali[1]==TRUE && quali[2]==FALSE && tab[4,5]){return(list(Mcomp=test, Reg=reg))}
if (quali[1]==FALSE && quali[2]==FALSE && tab[4,5]){return(list(Reg=reg))}

}
