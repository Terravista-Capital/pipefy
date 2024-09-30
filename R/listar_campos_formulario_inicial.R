#' @export

listar_campos_formulario_inicial <- function(id_pipe, headers = c()){

  query <- sprintf('{pipe(id:"%s"){ start_form_fields { label}}}', id_pipe)
  r <- requisicao(query, headers)
  return (r$data$pipe$start_form_fields |> unlist())
}
