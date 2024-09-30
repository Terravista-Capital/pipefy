#' @export

deletar_card <- function(id_card, header = c()){

  query <- sprintf('mutation {deleteCard (input: {id: "%s"}){success}}', id_card)
  pipefy::requisicao(query = query)

}
