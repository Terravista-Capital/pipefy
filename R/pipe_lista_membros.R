#' @export

pipe_lista_membros <- function(pipe_id, header = c()){

  query <- "{ pipe:(id:'%s'){members {role_name user {id name displayName email}}}}"
  r <- pipefy::requisicao(query = query)
  return(r)

}
