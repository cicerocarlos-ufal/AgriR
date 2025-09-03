means.test <-function(mean1, mean2, sd1, sd2,n1,n2, alpha=0.05){
  
  #Construi o intervalo de confiança
  
  # delta tukey 
  
  x1=rnorm(1000,mean1,sd1)
  x2=rnorm(1000,mean2,sd2)

  x1=x1[order(x1)]
  hx1 <- dnorm(x1,mean1,sd1)
  
  x2=x2[order(x2)]
  hx2 <- dnorm(x2,mean2,sd2)
  
  
  if (max(x1)>=max(x2)){xlima=max(x1)+sd(x1)}
  if (max(x1)<=max(x2)){xlima=max(x2)+sd(x2)}
  if (min(x1)>=min(x2)){xlimi=min(x2)-sd(x2)}
  if (min(x1)<=min(x2)){xlimi=min(x1)-sd(x1)}
  
  #ylimi
  y1=hist(x1, plot=FALSE)$density
  y2=hist(x2, plot=FALSE)$density
  
  if (max(y1)>=max(y2)){ylima=max(y1)+sd(y1)}
  if (max(y1)<=max(y2)){ylima=max(y2)+sd(y2)}
  if (min(y1)>=min(y2)){ylimi=min(y2)-sd(y2)}
  if (min(y1)<=min(y2)){ylimi=min(y1)-sd(y1)}
  
  
  #myhist1 <- hist(x1, freq=FALSE, col=rgb(1,0,0,0.5), density=4,xlim=c(0,200),xlab="",ylim=c(0,max(hx1)+0.02),main=NULL)
  myhist1 <- hist(x1, freq=FALSE, col=rgb(1,0,0,0.5), density=4,xlab="",ylim=c(0,ylima), xlim=c(xlimi,xlima),main=NULL)
  myhist2 <- hist(x2, freq=FALSE,col=rgb(0,0,1,0.5), density=4, add=T)

  #lines(mydensity)
  #Erros padrão para diferenca entre duas médias
  var1=sd1^2
  var2=sd2^2
  var_pod=((n1-1)*var1+(n2-1)*var2)/((n1-1)+(n2-1))
  sdp=sqrt(var_pod*(1/n1+1/n2))
 
   #Mean 1
    error = qt(1-alpha/2, df=n1+n2-2)*sdp
    inf1 = mean1 - error
    sup1 = mean1 + error
    
    #plot(x, hx, type="n", axes=T)
    
    i1 <- x1 >= inf1 & x1 <= sup1
    
    lines(x1, hx1, col="red")
    mycol1 <- rgb(255, 0, 0, max = 255, alpha = 125, names = "red50")
    polygon(c(inf1,x1[i1],sup1), c(0,hx1[i1],0), col=mycol1)
    
    #Mean 2  
    #plot(x, hx, type="n", axes=T)
    inf2 = mean2 - error
    sup2 = mean2 + error
    
    i2 <- x2 >= inf2 & x2 <= sup2
    
    lines(x2, hx2, col="blue")
    mycol2 <- rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50")
    polygon(c(inf2,x2[i2],sup2), c(0,hx2[i2],0), col=mycol2)
    
    #Plotar linhas
    abline(v=mean1, col="red")
    abline(v=inf1, col="red")
    abline(v=sup1, col="red")
    
    abline(v=mean2, col="blue")
    abline(v=inf2, col="blue")
    abline(v=sup2, col="blue")

    #Print DMS
    lbl.mean=paste(round(c(error),2), c("DMS"), sep=" - ")
    lbl.diff=paste(round(c(mean1-mean2),2), c("Mean Difference"), sep=" - ")
    if (abs(mean1-mean2)>error){lbl.test=c("Show statistical significance")
    text(xlimi,ylima-1/4*ylima,lbl.test, col="blue", adj=0)
    }
    if (abs(mean1-mean2)<error){lbl.test=c("Not show statistical significance")
    text(xlimi,ylima-1/4*ylima,lbl.test, col="red", adj=0)
    }
    
    text(xlimi,ylima,c("Teste information"), col="blue", adj=0)
    text(xlimi,ylima-1/10*ylima,lbl.mean, col="blue", adj=0)
    text(xlimi,ylima-1/6*ylima,lbl.diff, col="blue", adj=0)
    
        
}

