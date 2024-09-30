#' @export

listar_cards_preenchidos <- function(id_pipe, id_fase, headers = c()){

  cont <- 1
  nomes <- "id"
  has_next_page <- TRUE
  first_query <- TRUE

  aux <- pipefy::listar_campos_formulario_inicial(id_pipe, headers) |> as.data.frame()

  for(i in 1:nrow(aux)){
    nomes <- paste0(nomes, ",", aux[i,1])
  }

  nomes <- strsplit(nomes, ",") |> unlist()
  tb <- tibble::tibble(!!!nomes, .rows = 0, .name_repair = ~ nomes)

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

    for(i in 1:n_registros){
      tb[cont,"id"] <- r$data$phase$cards$edges[[i]]$node$id
      n_campos <- length(r$data$phase$cards$edges[[i]]$node$fields)

      for(j in 1:n_campos){
        nome <- r$data$phase$cards$edges[[i]]$node$fields[[j]]$name
        valor <- r$data$phase$cards$edges[[i]]$node$fields[[j]]$value

        if(stringr::str_detect(stringr::str_c(colnames(tb), collapse = ""), nome) == TRUE){
          tb[i, nome] <- valor
        }
      }

      cont <- cont + 1
    }
}



tb[is.na(tb)] <- ""

tb$`Ofício Requisitório` <- tb$`Ofício Requisitório` |> jsonlite::fromJSON()
tb$`Ofício Precatório` <- tb$`Ofício Precatório` |> jsonlite::fromJSON()

return(tb)

}
