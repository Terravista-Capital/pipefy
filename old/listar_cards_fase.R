#' lista os cards em determinada fase do pipefy
#'
#' @param id_fase número da fase para pesquisa no pipefy
#' @param headers parâmetros variáveis para query
#'
#' @return tibble
#' @export
#'
listar_cards_fase <- function(id_fase, headers = c()){


  cont <- 1
  nomes <- "id"
  has_next_page <- TRUE
  first_query <- TRUE

  while(has_next_page == TRUE){

    if(first_query == TRUE){

      query <- sprintf('{phase(id:"%s"){cards {edges { node{id title fields{name value}}} pageInfo {endCursor hasNextPage}}}}', id_fase)

    } else{

      query <- sprintf('{phase(id:"%s",after:"%s"){cards {edges { node{id title fields{name value}}} pageInfo {endCursor hasNextPage}}}}', id_fase, end_cursor)

    }

    r <- pipefy::requisicao(query, headers)
    end_cursor <- r$data$phase$cards$pageInfo$endCursor
    has_next_page <- r$data$phase$cards$pageInfo$hasNextPage
    n_registros <- length(r$data$phase$cards$edges)
    n_campos <- length(r$data$phase$cards$edges[[1]]$node$fields)

    if(first_query == TRUE){

      for(i in 1:n_campos){

        nome <- r$data$phase$cards$edges[[1]]$node$fields[[i]]$name
        nomes <- paste(nomes, nome, sep = ",")
      }

      nomes <- strsplit(nomes, ",") |> unlist()
      tb <- tibble::tibble(!!!nomes, .rows = 0, .name_repair = ~ nomes)
      first_query <- FALSE
    }

    for(i in 1:n_registros){
      tb[cont,"id"] <- r$data$phase$cards$edges[[i]]$node$id
      for(j in 1:n_campos){
        valor <- r$data$phase$cards$edges[[i]]$node$fields[[j]]$value
        if(is.null(valor) == TRUE){valor <- ""}
        tb[cont, r$data$phase$cards$edges[[i]]$node$fields[[j]]$name] <- valor
      }
      cont <- cont + 1
    }
  }


tb$`Ofício Requisitório` <- tb$`Ofício Requisitório` |> jsonlite::fromJSON()
tb$`Ofício Precatório` <- tb$`Ofício Precatório` |> jsonlite::fromJSON()

return(tb)

}
