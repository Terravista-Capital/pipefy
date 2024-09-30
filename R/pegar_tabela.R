#' @export

pegar_tabela <- function(id_tabela, headers = c()){

  first_query <- TRUE
  has_next_page <- TRUE
  cont <- 1
  nomes <- "id"

  while(has_next_page == TRUE){

    if(first_query == TRUE){

      query <- sprintf('{table_records(table_id:"%s"){edges { node{id title record_fields{name value}}} pageInfo {endCursor hasNextPage}}}', id_tabela)

    } else{

      query <- sprintf('{table_records(table_id:"%s",after:"%s"){edges { node{id title record_fields{name value}}} pageInfo {endCursor hasNextPage}}}', id_tabela, end_cursor)

    }

    r <- pipefy::requisicao(query, headers)
    end_cursor <- r$data$table_records$pageInfo$endCursor
    has_next_page <- r$data$table_records$pageInfo$hasNextPage
    n_registros <- length(r$data$table_records$edges)
    n_campos <- length(r$data$table_records$edges[[1]]$node$record_fields)

    if(first_query == TRUE){

      for(i in 1:n_campos){

        nome <- r$data$table_records$edges[[1]]$node$record_fields[[i]]$name
        nomes <- paste(nomes, nome, sep = ",")
      }

      nomes <- strsplit(nomes, ",") |> unlist()
      tb <- tibble::tibble(!!!nomes, .rows = 0, .name_repair = ~ nomes)
      first_query <- FALSE
    }

    for(i in 1:n_registros){
      tb[cont,"id"] <- r$data$table_records$edges[[i]]$node$id
      for(j in 1:n_campos){
       valor <- r$data$table_records$edges[[i]]$node$record_fields[[j]]$value
       if(is.null(valor) == TRUE){valor <- ""}
       tb[cont,r$data$table_records$edges[[i]]$node$record_fields[[j]]$name] <- valor
      }
      cont <- cont + 1
    }
  }

  tb$data_criacao <- tb$data_criacao |> lubridate::dmy_hms()
  tb$data_base <- tb$data_base |> lubridate::dmy()
  tb$data_posse <- tb$data_posse |> lubridate::dmy()
  tb$data_expedicao <- tb$data_expedicao |> lubridate::dmy()
  tb$data_prioridade <- tb$data_prioridade |> lubridate::dmy()
  tb$data_prioridade_2 <- tb$data_prioridade_2 |> lubridate::dmy()

  tb$principal <-tb$principal |> as.numeric()
  tb$juros_mora <- tb$juros_mora |> as.numeric()
  tb$desc_prev <- tb$desc_prev |> as.numeric()
  tb$desc_med <- tb$desc_med |> as.numeric()
  tb$juros_compensatorios <- tb$juros_compensatorios |> as.numeric()
  tb$honorarios_sucumbenciais <- tb$honorarios_sucumbenciais |> as.numeric()
  tb$valor_prioridade <- tb$valor_prioridade |> as.numeric()
  tb$valor_prioridade2 <- tb$valor_prioridade2 |> as.numeric()

  return(tb)
}
