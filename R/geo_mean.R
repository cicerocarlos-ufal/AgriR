#' @title Calcula a média geométrica para um conjunto de dados (x > 0) 
#' @description Função para calcular a média geométrica
#' @details Calcular a média geométrica para valores positivos
#' @return Valor da média
#' @author Cicero Almeida
#' @examples
#' data=c(3,4,4,4,5,5,6,8,8,9,9,10,11,12)
#' geo_mean(data)
#' @export
geo_mean = function(x, na.rm=TRUE){
  exp(sum(log(x), na.rm=na.rm) / length(x))}
