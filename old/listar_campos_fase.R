#' @export

listar_campos_fase <- function(id_fase, headers = c()){

query <- sprintf('{phase(id:"%s"){ fields {label}}}', id_fase)

r <- pipefy::requisicao(query, headers)

}
