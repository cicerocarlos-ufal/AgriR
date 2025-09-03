boxplotb <-function(y,trt,table.graph,xlab="",ylab="", graph.col="", main=""){


#Obtendo grafico box plot modificado
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))

	
letras.tukey=c(table.graph[,5])					#Usado para a função tukey.teste
letras.tukey=gsub(" ","", letras.tukey)

	
grafico.dif = boxplot(y~trt, plot = FALSE)		#Criar o gráfico
grafico.dif$stats[2,] = table.graph[,4]		#Adicionar o desvio inferior
grafico.dif$stats[3,] = table.graph[,1]			#Adicionar a média
grafico.dif$stats[4,] = table.graph[,3]		#Adicionar o desvio superior
bxp(grafico.dif, ylab=ylab, xlab=xlab, boxfill = graph.col)

#text(table.graph[,3]+1/1.5*(mean(table.graph[,2])), letras.tukey, cex=1.5)
text(grafico.dif$stats[2,]+1/1.5*(mean(table.graph[,2])), letras.tukey, cex=1.5)

}
