baixar_relatorio <- function(id_pipe, id_relatorio, diretorio, headers = c()){

  query1 <- paste0('
    mutation {
      exportPipeReport(input: {pipeId: ', id_pipe, ', pipeReportId: ', id_relatorio, '}) {
        clientMutationId
        pipeReportExport {
          id
          state
        }
      }
    }
  ')

  r1 <- pipefy::requisicao(query1)

  query2 <- paste0('
    query {
      pipeReportExport(id: ', r1$data$exportPipeReport$pipeReportExport$id, ') {
        id
        state
        fileURL
      }
    }
  ')

  r2 <- pipefy::requisicao(query2)

  r2$data$pipeReportExport$state

  while(r2$data$pipeReportExport$state != "done"){

    r2 <- pipefy::requisicao(query2)

  }

  fileURL <- r2$data$pipeReportExport$fileURL
  httr::GET(fileURL,
            httr::authenticate(user = Sys.getenv("PIPEFY_USER"), password = Sys.getenv("PIPEFY_PASSWORD")),
            httr::write_disk(path = paste0(diretorio, "/", id_relatorio, ".xlsx"), overwrite = TRUE))

  }
