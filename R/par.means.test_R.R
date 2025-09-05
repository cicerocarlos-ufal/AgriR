#' @author Cícero Carlos de Souza Almeida
#' @export
par.test <-function(data1, data2, alpha=0.05){
  x1=data1
  x2=data2
  mean1=mean(data1)
  mean2=mean(data2)
  sd1=sd(data1)
  sd2=sd(data2)
  n1=length(data1)
  n2=length(data2)
  var1=var(data1)
  var2=var(data2)
  
  #x-limit
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
  
  
  x11=rnorm(1000,mean1,sd1)
  x11=x11[order(x11)]
  hx11 <- dnorm(x11,mean1,sd1)
  
  x22=rnorm(1000,mean2,sd2)
  x22=x22[order(x22)]
  hx22 <- dnorm(x22,mean2,sd2)
  
  
  myhist1 <- hist(x1, freq=FALSE, col=rgb(1,0,0,0.5), density=4,xlab="",main=NULL, ylim=c(ylimi,ylima), xlim=c(xlimi,xlima))

  
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
    
    i1 <- x11 >= inf1 & x11 <= sup1
    #Obter as curvas
    lines(x11, hx11, col="red")
    mycol1 <- rgb(255, 0, 0, max = 255, alpha = 125, names = "red50")
    polygon(c(inf1,x11[i1],sup1), c(0,hx11[i1],0), col=mycol1)
    
    #Mean 2  
    #plot(x, hx, type="n", axes=T)
    inf2 = mean2 - error
    sup2 = mean2 + error
    
    i2 <- x22 >= inf2 & x22 <= sup2
    
    lines(x22, hx22, col="blue")
    mycol2 <- rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50")
    polygon(c(inf2,x22[i2],sup2), c(0,hx22[i2],0), col=mycol2)
  
    #information
    abline(v=mean1, col="red")
    abline(v=mean2, col="blue")
    
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

