#' @export


requisicao <- function(query, headers=c()){
  obj <- pipefy::preparar_requisicao()

  # Atualizar headers
  headers <- c(obj$headers, headers)

  # Realizar requisicao
  response <- httr::POST(
    url = obj$endpoint,
    body = list(query = query),
    httr::add_headers(.headers = headers),
    encode='json'
  )

  # Pegando resposta JSON da requisicao
  tryCatch(
    {
      response <- httr::content(response, as = "parse", encoding = "UTF-8")
    },
    error = function(e){
      stop(paste("PipefyException:", e$message))
    }
  )

  # Error handling
  if (!is.null(response$error)){
    stop(paste("PipefyException: ", response$error_description, response$error))
  }
  if (!is.null(response$errors)){
    for (error in response$errors){
      stop(paste("PipefyException: ", error$message))
    }
  }

  return(response)
}



