#' @title Descriptive statistics 
#' @description function for calculation of the Descriptive statistics
#' @return Descriptive statistics
#' @author Cicero Almeida
#' @examples
#' data=c(3,4,4,4,5,5,6,8,8,9,9,10,11,12)
#' desc(data)
#' @export
desc<-function(y,x=1, boxplot=FALSE, xlab="Treatements", ylab="Variable", graph.col="skyblue")
{

x=factor(x)

if (length(x) == 1){
name.y <- c(paste(deparse(substitute(y))))

	if (is.numeric(y)==TRUE){
	cat("\n------------------------------------------------------------\n")
	cat("   Descriptive statistics for",name.y,"variable")
	cat("\n------------------------------------------------------------\n")


	media=mean(y)			#média
	mediana=median(y)		#mediana
	desv.pad=sd(y)			#Desvio Padrão
	erro.medio=sd(y)/sqrt(length(y))	#Erro padrão da média
	cv=paste(round(100*sd(y)/mean(y),2),"%")	#Coeficiente de vaiação
	error <- qt(0.975,df=length(y)-1)*sd(y)/sqrt(length(y))

	#ic95=paste(round(mean(y)-error,3),"-",round(mean(y)+error,3))

	output=data.frame(rbind(round(media,3), round(mediana,3), round(desv.pad,3), round(erro.medio,3),cv,round(error,3)))
	names(output)=c("Statistics")
	rownames(output)=c("Mean","Median","sd","Mean error", "Coefficient of Variation", "Confidence interval - 95%")
	}

	else {

	cat("\n ------------------------------------------------------------\n")
	cat("\n                 No Numeric Variable                         \n")
	cat("\n ------------------------------------------------------------\n")
	}

#---------------------Plotar boxplot-------------------------
if (boxplot==TRUE) { 

boxplot(y, xlab=xlab,ylab=ylab, col=graph.col) }

}

#print(output)
#cat("\n------------------------------------------------------------\n")
#invisible(list(Result=output))
#----------------FIM DOS DADOS SIMPLES----------------------

#---------------INICIO DOS MULTIPLOS---------------------

else { 

data=data.frame(y,x)
name.y <- c(paste(deparse(substitute(y))))
name.x <- c(paste(deparse(substitute(y))))

x.length=levels(x)

dat=NULL
media=NULL
mediana=NULL
desv.pad=NULL
erro.medio=NULL
error=NULL
cv=NULL

	if (is.numeric(y)==TRUE){

	cat("\n------------------------------------------------------------\n")
	cat("   Descriptive statistics for",name.y,"variable")
	cat("\n------------------------------------------------------------\n")


	for (i in 1:length(levels(x))){
	#cat("\n Descriptive statistics for ",levels(x)[i],"treatements\n")

	#cat(levels(x)[i])

	dat[[i]]=data.frame(subset(data, x==levels(x)[i]))

	media[i]=c(mean(dat[[i]][,1]))

	mediana[i]=c(median(dat[[i]][,1]))		#mediana
	desv.pad[i]=c(sd(dat[[i]][,1]))			#Desvio Padrão
	erro.medio[i]=c(sd(dat[[i]][,1])/sqrt(length(dat[[i]][,1])))	#Erro padrão da média

	cv[i]=c(paste(round(100*sd(dat[[i]][,1])/mean(dat[[i]][,1]),2),"%",sep=""))	#Coeficiente de vaiação

	error[i] <- c(qt(0.975,df=length(dat[[i]][,1])-1)*sd(dat[[i]][,1])/sqrt(length(dat[[i]][,1])))

	#ic95[i]=c(paste(round(mean(dat[[i]][,1])-error[i],3),"-",round(mean(dat[[i]][,1])+error[i],3)))

	}

	output=data.frame(rbind(round(media,3), round(mediana,3), round(desv.pad,3), round(erro.medio,3),cv, round(error,3)))
	names(output)=levels(x)
	rownames(output)=c("Mean","Median","sd","Mean error", "Coefficient of Variation", "Confidence interval - 95%")

	}

	else {

	cat("\n ------------------------------------------------------------\n")
	cat("\n                 No Numeric Variable                         \n")
	cat("\n ------------------------------------------------------------\n")

	}
#---------------------Plotar boxplot-------------------------
if (boxplot==TRUE) {boxplot(y~x,xlab=xlab,ylab=ylab, col=graph.col) }

}
#---------------------------FIM DOS MULTIPLOS---------------------

print(output)
cat("\n------------------------------------------------------------\n")
invisible(list(Result=output))

}
