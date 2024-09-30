#' prepara o objeto para requisicao no pipefy
#'
#' @param mock_server A boolean.
#' @returns A list.
#' @export

preparar_requisicao <- function(mock_server=FALSE) {
  #token <- ''
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

