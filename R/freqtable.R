#' @title frequency table  
#' @description function for frequency table
#' @return frequency table
#' @author Cicero Almeida
#' @examples
#' data=c(3,4,4,4,5,5,6,8,8,9,9,10,11,12)
#' freqtable(data, bins)
#' @export
freqtable<-function(y,bins)
{
Scores= cut(y,bins)

table.y=transform(table(Scores))

rel.freq = round(table.y[2]/sum(table.y[2]), 2)

c.rel.freq = cumsum(round(table.y[2]/sum(table.y[2]), 2))

classes=rownames(table.y)

table.y=cbind(classes, table.y, rel.freq, c.rel.freq)

names(table.y)=c("Cass", "Class limits", "Frequeny","Rel.Freq","C. Rel.Freq")

cat("\nFrequency  Distribution Table .... ok\n")
cat("\n------------------------------------------------------------------------\n")
return(table.y)
}
