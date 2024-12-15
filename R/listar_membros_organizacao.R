#' Lista os membros da organizacao no Pipefy
#'
#' @param header Headers adicionais para a requisição HTTP.
#'
#' @return Tibble com informações dos membros do pipe.
#' @export

listar_membros_organizacao <- function(header = c()){

  org_id <- Sys.getenv("PIPEFY_ORGANIZATION_ID")

  query <- paste0("{
      organization(id:", org_id ,") {
        members {
          user {
            id
            email
          }
        }
      }
    }
  ")

  r <- pipefy::requisicao(query = query)

  usuarios <- purrr::map_dfr(r$data$organization$members, ~ tibble::as_tibble(.x[1]$user))

  return(usuarios)
}
