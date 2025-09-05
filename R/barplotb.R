#' @author Cícero Carlos de Souza Almeida
#' @export
barplotb <-function(table.graph, xlab="",ylab="", graph.col="",graph.den=NULL, main=""){

#medias=tapply(resp, list(treat), mean)
#desv.pad=tapply(resp, list(treat), sd)
#se.sup=medias+desv.pad
#se.inf=medias-desv.pad
#name.y <- paste(deparse(substitute(resp)))
#name.t <- paste(deparse(substitute(treat)))
#grafico.tukey<-barplot(medias, beside=T,xpd = FALSE, ylim=c(0,max(resp+1/2*sd(resp))), lwd=2, las = 1)
#arrows(grafico.tukey,se.sup,grafico.tukey,se.inf, code=3,angle=90,length=0.05, col="red", lwd=2)
#tukey=tukey.result
#tukey=tukey[order(rownames(tukey)),]
#letras.tukey=c(tukey$Tukey)
#letras.tukey=gsub(" ","", letras.tukey)	

##tukey=data.frame(anova$Tukey)				#Usados na função tukey.test
##tukey=tukey[order(rownames(tukey)),]		#Usados na função tukey.test
#tukey=cbind(tukey,rownames(tukey))			#usada na funçaõ tukey
#tukey=tukey[order(tukey[3]),]				#usada na funçaõ tukey


#text(grafico.tukey,se.sup+0.3,letras.tukey, cex=1.5)

#--------gráfico usando a tabela--------

table.graph=table.graph[order(rownames(table.graph)),]

ylim=c(0,max(table.graph[,3])+0.3*max(table.graph[,3]))

#ylim=c(0,max(resp+0.5*sd(resp)))

means=table.graph[,1]
names(means)=rownames(table.graph)

grafico.tukey<-barplot(means, beside=T, xpd = FALSE, ylim=ylim, xlab=xlab,ylab=ylab,lwd=2, main=main, las = 1, density=graph.den, col=graph.col)

arrows(grafico.tukey,table.graph[,3],grafico.tukey,table.graph[,4], code=3,angle=90,length=0.05, col="red", lwd=2)

	
letras.tukey=c(table.graph[,5])					#Usado para a função tukey.teste
letras.tukey=gsub(" ","", letras.tukey)



#text(grafico.tukey,table.graph[,3]+0.6*(table.graph[,2]), letras.tukey, cex=1.5)

#text(grafico.tukey,table.graph[,3]+(table.graph[,2])^(1/100), letras.tukey, cex=1.5)

text(grafico.tukey,table.graph[,3]+1/1.5*(mean(table.graph[,2])),letras.tukey, cex=1.5)


}
