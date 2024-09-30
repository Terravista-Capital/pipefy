#' @export

apagar_registro_tabela <- function(id_registro, headers = c()){

  query <- sprintf('mutation {deleteTableRecord (input:{id:"%s"}){success}}', id_registro)

  pipefy::requisicao(query, headers)
}
