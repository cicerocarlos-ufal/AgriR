aproximate <-function(mean, sd, n, dist="normal",alpha=0.05){
  
  #Construi o intervalo de confiança
  
  # delta tukey 
  
  x=rnorm(1000,mean,sd)
  x=x[order(x)]
  hx <- dnorm(x,mean,sd)
  
  #myhist <- hist(x, freq=F, col=rgb(1,0,0,0.5), density=4,xlim=c(min(x),max(x)),ylim=c(0,max(hx)+0.005), main="Histogram", xlab="Variable X")
  
  myhist <- hist(x, freq=FALSE, col=rgb(1,0,0,0.5), density=4,xlim=c(0,200),ylim=c(0,max(hx)+0.005), main=NULL,xlab="Variable X")
  
  #lines(mydensity)
  
  if (dist =="normal"){
    error = qnorm(1-alpha/2)*sd/sqrt(n)
    inf = mean - error
    sup = mean + error
    
    #plot(x, hx, type="n", axes=T)
    
    i <- x >= inf & x <= sup
    
    lines(x, hx)
    mycol <- rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50")
    polygon(c(inf,x[i],sup), c(0,hx[i],0), col=mycol)

  lbl.inf=paste(c("Lower limite"),round(c(inf),2), sep=" - ")
  lbl.sup=paste(c("Upper limite"),round(c(sup),2), sep=" - ")    
  lbl.mean=paste(c("Mean"),c(mean), sep=" - ")  
  

  text(inf-5,max(hx),lbl.inf, col="blue", adj=1)
  text(sup+5,max(hx),lbl.sup, col="blue", adj=0)
  text(mean,max(hx)+0.002,lbl.mean, col="red", adj=0.5)
  
  abline(v=mean, col="blue")
  abline(v=inf, col="blue")
  abline(v=sup, col="blue")
  
  }
  
  
  if (dist =="t"){
    error = qt(1-alpha/2, df=n-1)*sd/sqrt(n)
    inf = mean - error
    sup = mean + error
    
    #plot(x, hx, type="n", axes=T)
    
    i <- x >= inf & x <= sup
    
    lines(x, hx)
    mycol <- rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50")
    
    polygon(c(inf,x[i],sup), c(0,hx[i],0), col=mycol)
    
    lbl.inf=paste(c("Lower limite"),round(c(inf),2), sep=" - ")
    lbl.sup=paste(c("Upper limite"),round(c(sup),2), sep=" - ")    
    lbl.mean=paste(c("Mean"),c(mean), sep=" - ")  
    
    
    text(inf-5,max(hx),lbl.inf, col="blue", adj=1)
    text(sup+5,max(hx),lbl.sup, col="blue", adj=0)
    text(mean,max(hx)+0.002,lbl.mean, col="red", adj=0.5)
    
    abline(v=mean, col="blue")
    abline(v=inf, col="blue")
    abline(v=sup, col="blue")
    
  }
  
}

