#' Prepara o objeto para requisição no Pipefy.
#'
#' @param mock_server Indica se a requisição deve ser feita para um servidor mockado.
#'
#' @return Lista com informações da requisição.

preparar_requisicao <- function(mock_server=FALSE) {
  token <- paste("Bearer", Sys.getenv("PIPEFY_API_TOKEN"))
  headers <- c('Authorization' = token)
  endpoint <- 'https://app.pipefy.com/graphql'
  if (mock_server) {
    endpoint <- 'https://private-a6c28-pipefypipe.apiary-mock.com/queries'
  }

  # Construct a list to hold the "object" data
  obj <- list(token = token, headers = headers, endpoint = endpoint)
  return(obj)
}

