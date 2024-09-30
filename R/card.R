#' @export

card <- function(id, response_fields=NULL, headers=c()){

  # Set default response fields if not specified
  if (is.null(response_fields)) {
    response_fields <- 'title
                    assignees {
                    id
                    }
                    comments {
                    id
                    }
                    comments_count
                    current_phase {
                    name
                    }
                    done
                    due_date
                    fields {
                    name
                    value
                    }
                    labels {
                    name
                    }
                    phases_history {
                    phase {
                    name
                    }
                    firstTimeIn
                    lastTimeOut
                    }
                    url '

  }

  # Make query
  query <- sprintf('{ card(id: %s) { %s } }',
                   jsonlite::toJSON(id, auto_unbox = TRUE),
                   response_fields)

  # Make request
  response <- pipefy::requisicao(query, headers)
  # Extract data from response
  if ("data" %in% names(response)) {
    if ("card" %in% names(response$data)) {
      return(response$data$card)
    }
  }

  return(NULL) # return NULL if the requested data was not found
}
