#' @author Cícero Carlos de Souza Almeida
#' @export
pointplotg <-function(table.graph, xlab="", ylab="",title="", caption="", graph.col="skyblue", barcolor="orange", main=""){

#tukey=c(table.graph$tukey)

letras.tukey=c(table.graph[,5])					#Usado para a função tukey.teste
letras.tukey=gsub(" ","", letras.tukey)

#yl=table.graph[,3]+0.6*(table.graph[,2])
yl=table.graph[,3]+1/2*(mean(table.graph[,2]))

require(ggplot2)
p = ggplot(table.graph,aes(x=rownames(table.graph), y=table.graph[,1])) +
    geom_point(size = 5)+
    geom_errorbar( aes(x=rownames(table.graph), ymin=table.graph[,4], ymax=table.graph[,3]), width=0.2, colour=barcolor, alpha=0.9, size=1.3) +
    labs(title=title, x=xlab, y=ylab, caption=caption) + 
    geom_text(aes(label = letras.tukey, x=rownames(table.graph), y=yl), size=5) + theme_bw(base_size = 12)
#p + theme_bw(base_size = 12)
print(p)
}
