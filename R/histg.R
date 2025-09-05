#' @title Histogram with graph 
#' @description function for calculation of table freq and histogram
#' @return frequency table and histogram graph
#' @author Cicero Almeida
#' @examples
#' data=c(3,4,4,4,5,5,6,8,8,9,9,10,11,12)
#' histg(data)
#' @export
histg <-function(y, xlab="Distribution",ylab="Frequency", graph.col="turquoise", scores="empiric",border=TRUE, freq=TRUE, graph.type="base", main="")
{

df=data.frame(y)

A=diff(range(y, na.rm=TRUE))

bins1=ceiling(sqrt(length(y)))							#empiric
a1=A/bins1
a1f=a1+(1/bins1)*a1
brk1=seq(min(y, na.rm=TRUE)-1/2*a1f,max(y,na.rm=TRUE)+1/2*a1f,a1f)

#brk2=seq(min(y)-1/2*a2,max(y)+1/2*a2,a2+1/8*a2)

bins2=ceiling(1+log2(length(y)))						#Sturges' formula
a2=A/bins2
a2f=a2+(1/bins2)*a2
brk2=seq(min(y, na.rm=TRUE)-1/2*a2f,max(y,na.rm=TRUE)+1/2*a2f,a2f)

bins3=ceiling(2*(length(y)^(1/3)))						#Rice Rule
a3=A/bins3
a3f=a3+(1/bins3)*a3
brk3=seq(min(y,na.rm=TRUE)-1/2*a3f,max(y,na.rm=TRUE)+1/2*a3f,a3f)
									
bins4=ceiling(diff(range(y, na.rm=TRUE))/((3.5*sd(y, na.rm=TRUE))/(length(y)^(1/3))))			#scott
a4=A/bins4
a4f=a4+(1/bins4)*a4
brk4=seq(min(y,na.rm=TRUE)-1/2*a4f,max(y,na.rm=TRUE)+1/2*a4f,a4f)

bins5=ceiling(diff(range(y, na.rm=TRUE)) / (2 * IQR(y, na.rm=TRUE) / length(y)^(1/3)))			#Freedman-Diaconis
a5=A/bins5
a5f=a5+(1/bins5)*a5
brk5=seq(min(y,na.rm=TRUE)-1/2*a5f,max(y,na.rm=TRUE)+1/2*a5f,a5f)


cat("\n------------------------------------------------------------------------\n")
cat("\nHistogram .... ok \n")
cat("\n------------------------------------------------------------------------\n")

#Distribuição de frequência

	if (scores=="empiric"){
		table.y=freqtable(y,brk1)

		if (graph.type=="base") hist(y,	breaks=brk1, border=border, col=graph.col, xlab=xlab,ylab=ylab, freq=freq, main="")
		if	(graph.type=="ggplot2") print (ggplot(df, aes(x=y)) + geom_histogram(bins=bins1, fill="#69b3a2", color="#e9ecef", alpha=0.9))
	
}
	if (scores=="sturges"){
		table.y=freqtable(y,brk2)
		
		if (graph.type=="base") hist(y,	breaks=brk2, border=border, col=graph.col, xlab=xlab,ylab=ylab, freq=freq, main="")
		if	(graph.type=="ggplot2") print (ggplot(df, aes(x=y)) + geom_histogram(bins=bins2, fill="#69b3a2", color="#e9ecef", alpha=0.9))
}
	if (scores=="ricerule"){
		table.y=freqtable(y,brk3)					
		
		if (graph.type=="base") hist(y,	breaks=brk3, border=border, col=graph.col, xlab=xlab,ylab=ylab, freq=freq, main="")
		if	(graph.type=="ggplot2") print (ggplot(df, aes(x=y)) + geom_histogram(bins=bins3, fill="#69b3a2", color="#e9ecef", alpha=0.9))
}
	if (scores=="scott"){
		table.y=freqtable(y,brk4)
		
		if (graph.type=="base") hist(y,	breaks=brk4, border=border, col=graph.col, xlab=xlab,ylab=ylab, freq=freq, main="")
		if	(graph.type=="ggplot2") print (ggplot(df, aes(x=y)) + geom_histogram(bins=bins4, fill="#69b3a2", color="#e9ecef", alpha=0.9))
}
	if (scores=="fd"){
		table.y=freqtable(y,brk5)
		
		if (graph.type=="base") hist(y,	breaks=brk5, border=border, col=graph.col, xlab=xlab,ylab=ylab, freq=freq, main="")
		if	(graph.type=="ggplot2") print (ggplot(df, aes(x=y)) + geom_histogram(bins=bins5, fill="#69b3a2", color="#e9ecef", alpha=0.9))
}

list(Dist.Freq=table.y)

}

#For y discrete

#plot(table(y))
#points(names(table(y)),table(y))

#a=data.frame(table(y))



