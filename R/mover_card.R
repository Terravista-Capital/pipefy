#' Move um card para uma fase diferente no Pipefy.
#'
#' @param card_id ID do card.
#' @param fase_destino_id ID da fase de destino.
#' @param response_fields Campos a serem retornados pela API.
#' @param headers Headers adicionais para a requisição HTTP.
#'
#' @return Lista com informações do card movido.
#' @export

mover_card <- function(card_id, fase_destino_id, response_fields = NULL, headers = c()){

  if (is.null(response_fields)){
    response_fields <- 'card{ id current_phase { name } }'
  }

  query <- sprintf('mutation {
                    moveCardToPhase(
                      input: {
                        card_id: %s
                        destination_phase_id: %s
                      }
                    ) {%s}
                    }',
                   card_id, fase_destino_id, response_fields)

  response <- pipefy::requisicao(query, headers)
  return(response$data$moveCardToPhase$card)
}
