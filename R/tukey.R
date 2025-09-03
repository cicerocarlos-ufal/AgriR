#' Multiple comparison: Tukey's test
#'
#' \code{tukey} Performs the test of Tukey, for multiple
#' comparison of means.
#' @param y Numeric or complex vector containing the response
#' variable.
#' @param trt Numeric or complex vector containing the
#' treatments.
#' @param DFerror Error degrees of freedom.
#' @param SSerror Error sum of squares.
#' @param alpha Significance level.
#' @param group TRUE or FALSE.
#' @param main Title.
#' @details It is necessary first makes a analysis of variance.
#' @return
#' y  Numeric
#' trt  factor
#' DFerror  Numeric
#' MSerror  Numeric
#' alpha  Numeric
#' group Logic
#' main  Text
#' @references Principles and procedures of statistics a
#' biometrical approach Steel and Torry and Dickey. Third
#' Edition 1997
#' @author Eric B Ferreira,
#'  \email{eric.ferreira@@unifal-mg.edu.br}
#' @author Denismar Alves Nogueira
#' @author Portya Piscitelli Cavalcanti
#' (Adapted from Felipe de Mendiburu - GPL)
#' @seealso \code{\link{scottknott}}, \code{\link{duncan}},
#' \code{\link{lsd}}, \code{\link{lsdb}}, \code{\link{ccboot}},
#' \code{\link{snk}}, \code{\link{ccF}}.
#' @examples
#' data(ex1)
#' attach(ex1)
#' crd(trat, ig, quali = TRUE, mcomp = "tukey", sigT = 0.05)
#' @export

tukey <-
function(y, trt, DFerror, SSerror, alpha = 0.05, graph, graph.col, graph.den, graph.type, xlab="", ylab="", group = TRUE,  main = ""){

MSerror<-SSerror/DFerror

    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))

    junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
	
means <- tapply(junto[,1],junto[,2],mean)
sds <- tapply(junto[,1],junto[,2],sd)
nn <- tapply(junto[,1], junto[,2], length)

    #means <- tapply.stat(junto[, 1], junto[, 2], stat = "mean")
    #sds <- tapply.stat(junto[, 1], junto[, 2], stat = "sd")
    #nn <- tapply.stat(junto[, 1], junto[, 2], stat = "length")

means <- data.frame(means, std.err = sds/sqrt(nn),replication = nn)


    names(means)[1:2] <- c(name.t, name.y)			#Alterar os nomes
    ntr <- nrow(means)

    Tprob <- qtukey(1 - alpha, ntr, DFerror)			#Probabilidade de tukey
    nr <- unique(nn)

    nfila <- c("Alpha", "Error Degrees of Freedom", "Error Mean Square",
        "Critical Value of Studentized Range")

    nvalor <- c(alpha, DFerror, MSerror, Tprob)

    xtabla <- data.frame(...... = nvalor)
    row.names(xtabla) <- nfila



    if (group) {
        if (length(nr) == 1) {
            HSD <- Tprob * sqrt(MSerror/nr)			#HSD ---> q . sy
            #cat("\nHonestly Significant Difference", HSD)
        }
        else {
            nr1 <- 1/mean(1/nn)
            HSD <- Tprob * sqrt(MSerror/nr1)
            #cat("\nHonestly Significant Difference", HSD)
            #cat("\nHarmonic Mean of Cell Sizes ", nr1)
            #cat("\n\nDifferent HSD for each comparison")
        }
        cat("\nTukey's test\n------------------------------------------------------------------------")
        cat('\nGroups Treatments Means\n')
        output <- order.group(rownames(means), means[, 1], means[,3], MSerror, Tprob, means[, 2], parameter = 0.5)
        cat('------------------------------------------------------------------------\n')
    }
    if (!group) {
        comb <- combn(ntr, 2)
        nn <- ncol(comb)
        dif <- rep(0, nn)
        pvalue <- rep(0, nn)
        for (k in 1:nn) {
            i <- comb[1, k]
            j <- comb[2, k]
            dif[k] <- abs(means[i, 2] - means[j, 2])
            sdtdif <- sqrt(MSerror * (1/means[i, 4] + 1/means[j,
                4]))
            pvalue[k] <- round(1 - ptukey(dif[k] * sqrt(2)/sdtdif,
                ntr, DFerror), 4)
        }
        tr.i <- comb[1, ]
        tr.j <- comb[2, ]
        #print(data.frame(row.names = NULL, tr.i, tr.j, diff = dif, pvalue = pvalue))
        output <- data.frame(trt = means[, 1], means = means[,2], M = "", N = means[, 3], std.err = means[, 3])
    }
#Fazer o gráfico
resp = y
treat = trt

#Obtendos os dados para gerar o gráfico
medias=data.frame("Medias"=tapply(resp, list(treat), mean))
desv.pad=data.frame("sd"=tapply(resp, list(treat), sd))
se.sup=medias+desv.pad
names(se.sup)=c("se.sup")
se.inf=medias-desv.pad
names(se.inf)=c("se.inf")


#Obter as letras
tukey=data.frame("Tukey"=output[,3])
rownames(tukey)=output[,1]
tukey=tukey[order(rownames(tukey)),]

#Juntar as informações

table.graph=cbind(medias,desv.pad,se.sup,se.inf,tukey)


if(graph==TRUE){

if(graph.type=="boxplotb") boxplotb(y,trt,table.graph,graph.col=graph.col,xlab=xlab,ylab=ylab, main=main)

if(graph.type=="barplotb") barplotb(table.graph, graph.col=graph.col, graph.den=graph.den, xlab=xlab, ylab=ylab, main=main)

if(graph.type=="barplotg") barplotg(table.graph, graph.col=graph.col,xlab=xlab,ylab=ylab, main=main)

if(graph.type=="pointplotg") pointplotg(table.graph,graph.col=graph.col,xlab=xlab,ylab=ylab, main=main)


#barplot.crd(table.graph,graph.col,graph.den)

#barplot.g2(table.graph)

#------------------antigo-------
#output.graph=data.frame("Medias"=output[,2],"Tukey"=output[,3])
#rownames(output.graph)=output[,1]

#barplot.crd(resp,treat,output.graph,graph.col,graph.den)
}
else{

}

return(table.graph)
}
