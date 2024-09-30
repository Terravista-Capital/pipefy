#' @export

buscar_id_pipe <- function(id_card, header = c()){

  query <- "{ card:(id:'%s'){pipe {id}}}"
  r <- pipefy::requisicao(query = query)
  return(r)

}
