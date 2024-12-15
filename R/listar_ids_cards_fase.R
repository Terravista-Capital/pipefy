#' Lista os IDs dos cards em uma fase do Pipefy.
#'
#' @param id_fase ID da fase.
#' @param headers Headers adicionais para a requisição HTTP.
#'
#' @return Tibble com os IDs dos cards.
#' @export

listar_ids_cards_fase <- function(id_fase, headers = c()) {
  cards <- tibble::tibble()
  first_query <- TRUE
  has_next_page <- TRUE
  after <- NULL

  while (has_next_page) {

    query_1 <- sprintf('{
        phase(id: "%s") {
          cards(first: 50) {
            edges {
              node {
                id
                title
                fields {
                  name
                  value
                }
              }
            }
            pageInfo {
              endCursor
              hasNextPage
            }
          }
        }
      }
    ', id_fase)



    query <- sprintf('{
        phase(id: "%s") {
          cards(first: 50, after: "%s") {
            edges {
              node {
                id
                title
                fields {
                  name
                  value
                }
              }
            }
            pageInfo {
              endCursor
              hasNextPage
            }
          }
        }
      }
    ', id_fase, after)

    if(first_query == TRUE){
      r <- pipefy::requisicao(query_1, headers)
      first_query <- FALSE
    }else{
      r <- pipefy::requisicao(query, headers)
    }

    # Extrair apenas os IDs dos cards
    ids <- tibble::tibble(id_card = r$data$phase$cards$edges |>
                            purrr::map_chr(~.x$node$id))

    # Adicionar os IDs ao tibble
    cards <- dplyr::bind_rows(cards, ids)

    has_next_page <- r$data$phase$cards$pageInfo$hasNextPage
    after <- r$data$phase$cards$pageInfo$endCursor
  }

  return(cards)
}
