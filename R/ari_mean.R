#' @title Calcula a média aritmétrica para um conjunto de dados 
#' @description Função para calcular a média aritmétrica
#' @details Calcular a média geométrica para valores positivos
#' @return Valor da média
#' @author Cicero Almeida
#' @examples
#' data=c(3,4,4,4,5,5,6,8,8,9,9,10,11,12)
#' ari_mean(data)
#' @export
ari_mean = function(x, na.rm=TRUE){
(sum(x)/length(x))
}
