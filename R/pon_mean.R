#' @title Calcula a média ponderada para um conjunto de dados, com os devidos peso 
#' @description Função para calcular a média ponderada
#' @details Calcular a média ponderada
#' @return Valor da média
#' @author Cicero Almeida
#' @examples
#' data=c(3,4,4,4,5,5,6,8,8,9,9,10,11,12)
#' w=c(5,7,6,5,2,1,5,4,8,3,2,3,8,9)
#' pon_mean(data,w)
#' @export
pon_mean = function(x, w, na.rm=TRUE){
(sum(x*w)/sum(w))}

