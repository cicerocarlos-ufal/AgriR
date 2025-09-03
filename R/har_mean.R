#' @title Calcula a média harmôinca para um conjunto de dados (x > 0) 
#' @description Função para calcular a média harmônica
#' @details Calcular a média geométrica para valores positivos
#' @return Valor da média
#' @author Cicero Almeida
#' @examples
#' data=c(3,4,4,4,5,5,6,8,8,9,9,10,11,12)
#' har_mean(data)
#' @export
har_mean = function(x, na.rm=TRUE){
(length(x))/(sum(1/(x), na.rm=na.rm))
}
