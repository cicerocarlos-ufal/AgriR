#' @author Cícero Carlos de Souza Almeida
#' @export
int.confidance <-function(variable, dist="t",alpha=0.05){
  
  #Construi o intervalo de confiança
  
  # delta tukey 
  mean1=mean(variable)
  sd1=sd(variable)
  n1=length(variable)

  x=rnorm(1000,mean1,sd1)
  x=x[order(x)]
  hx <- dnorm(x,mean1,sd1)

 mycol2 <- rgb(0, 0, 255, max = 255, alpha = 80, names = "blue50")
 myhist <- histg(variable, graph.col=mycol2, freq=FALSE)
  
#myhist <- hist(variable, freq=FALSE, col=rgb(1,0,0,0.5), density=4, main=NULL,xlab="Variable X")
  
  #lines(mydensity)
  
  if (dist =="normal"){
    error = qnorm(1-alpha/2)*sd1/sqrt(n1)
    inf = mean1 - error
    sup = mean1 + error
    
    #plot(x, hx, type="n", axes=T)
    
    i <- x >= inf & x <= sup
    
    lines(x, hx)
    mycol <- rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50")
    polygon(c(inf,x[i],sup), c(0,hx[i],0), col=mycol)

  lbl.inf=paste(c("Lower limite"),round(c(inf),2), sep=" - ")
  lbl.sup=paste(c("Upper limite"),round(c(sup),2), sep=" - ")    
  lbl.mean=paste(c("Mean"),round(c(mean1),2), sep=" - ")  
  

  text(inf,max(hx),lbl.inf, col="blue", adj=1)
  text(sup,max(hx),lbl.sup, col="blue", adj=0)
  text(mean1,max(hx)+0.002,lbl.mean, col="red", adj=0.5)
  
  abline(v=mean1, col="blue")
  abline(v=inf, col="blue")
  abline(v=sup, col="blue")
  
}
  

  if (dist =="t"){
    error = qt(1-alpha/2, df=n1-1)*sd1/sqrt(n1)
    inf = mean1 - error
    sup = mean1 + error
    
    #plot(x, hx, type="n", axes=T)
    
    i <- x >= inf & x <= sup
    
    lines(x, hx)
    mycol <- rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50")
    
    polygon(c(inf,x[i],sup), c(0,hx[i],0), col=mycol)
    
    lbl.inf=paste(c("Lower limite"),round(c(inf),2), sep=" - ")
    lbl.sup=paste(c("Upper limite"),round(c(sup),2), sep=" - ")    
    lbl.mean=paste(c("Mean"),round(c(mean1),2), sep=" - ")  
    
    
    text(inf,max(hx),lbl.inf, col="blue", adj=1)
    text(sup,max(hx),lbl.sup, col="blue", adj=0)
    text(mean1,max(hx)+1/5*(max(hx)),lbl.mean, col="red", adj=0.5)
    
    abline(v=mean1, col="blue")
    abline(v=inf, col="blue")
    abline(v=sup, col="blue")
    
  }
 
  
  #Print
  
  cat("--------------------------\n")
  cat("Mean\n")
  print(mean1)
  cat("--------------------------\n")
  
  cat("--------------------------\n")
  cat("Confidance interval\n")
  print(paste(inf,sup, sep="-"))
  cat("--------------------------\n")
  
  shapiro.test(variable)
   
}

