#' @export

pipe_lista_membros <- function(header = c()){

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

  usuarios <- r$data$organization$members |>
    purrr::discard(is.null) |>
    unlist() |>
    dplyr::bind_rows() |>
    dplyr::select(email, id)

  tibble_final <- purrr::map_dfr(r$data$organization$members, ~ tibble::as_tibble(.x[1]$user))

  return(usuarios)
}
