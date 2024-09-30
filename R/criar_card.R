#' @export

criar_card <- function(pipe_id, atributos, cards_ligados_id = NULL, response_fields = NULL, headers = c()){

  # Set default response fields if not specified
  if (is.null(response_fields)) {
    response_fields <- 'card {
                          id
                          title
                        }'
  }

  if (is.null(cards_ligados_id)) {
    cards_ligados_id <- ''
  }

  # Make query
  query <- sprintf('mutation {
                     createCard(input:{
                      pipe_id: %s
                      fields_attributes: [ %s ]
                      parent_ids: [ %s ]
                     }){
                       %s
                       }}', pipe_id, atributos, cards_ligados_id, response_fields)

  # Make request
  response <- pipefy::requisicao(query, headers)
  # Extract data from response
  if ("data" %in% names(response)) {
    if ("createCard" %in% names(response$data)) {
      return(response$data$createCard$card)
    }
  }

  return(response) # return NULL if the requested data was not found


}
