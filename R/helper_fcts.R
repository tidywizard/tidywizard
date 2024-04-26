#' Helper functions for the tidywizard app
#'
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_split
#' @importFrom stringr str_replace
#'
#' @export
#'
#' @returns useful functions for tidywizard

logical_condition <- function(input_condition){
  switch(input_condition,
         "equal to" = "==",
         "greater than" = ">",
         "lesser than" = "<",
         "greater or equal to" = ">=",
         "lesser or equal to" = "<=",
         "different from" = "!="
  )
}

escape_special_chars <- function(user_input) {
  # Define um vetor de possíveis caracteres especiais
  special_chars <- c(".", "+", "*", "?", "(", ")", "[", "]", "{", "}", "|", "^", "$")

  # Define um vetor de padrões para verificar
  pattern <- paste0("(\\", paste0(special_chars, collapse = "|\\"), ")")

  # Define qual o padrão que é similar
  matches <- stringr::str_extract_all(user_input, pattern) |> unlist()

  # Caso exista ao menos um match
  if(length(matches) > 0){
    # Altera o user_input para um user_input com o escape "\\" antes do caractere especial
    escaped_input <- stringr::str_replace_all(user_input, pattern = paste0("\\", matches), paste0("\\\\\\\\", matches))

    return(escaped_input)
    }

  # Se não houver um match, retorna o user_input inicial
  return(user_input)
}

paste_cols_args <- function(cols, remove = F){
  if(remove){
    paste_cols <- sprintf("%s", paste(sprintf("-%s", cols), collapse = ", "))
    return(paste_cols)
  }

  paste_cols <- sprintf("%s", paste(sprintf("%s", cols), collapse = ", "))
  return(paste_cols)
}

paste_cols_filtering <- function(class_filter_col, selec_col_filter, condition, filter_value){
  if(length(filter_value) > 1){
    if(condition == "!="){
      selec_col_filter <- paste0("!", selec_col_filter)
    }

    condition <- "%in%"

    filter_value <- sprintf("c(%s)", paste(sprintf("'%s'", filter_value), collapse = ", "))

    paste <- sprintf("%s %s %s", selec_col_filter, condition, filter_value)
    return(paste)
  }

  if(is.numeric(class_filter_col)){
    paste <- sprintf("%s %s %s", selec_col_filter, condition, filter_value)
    return(paste)
  }

  paste <- sprintf("%s %s '%s'", selec_col_filter, condition, filter_value)
  return(paste)
}

code_to_html <- function(r_code) {
  # substituir |> por |></p>
  code_p <- stringr::str_replace_all(r_code, pattern = "\\|>", replacement = "|></p>")

  # split cada "linha" (split em \n)
  code_p_split <- stringr::str_split(code_p, pattern = "\n")[[1]]

  # substituir primeira (funcao) da linha por <p><span>(funcao)</span>
  code_p_span_split <- lapply(code_p_split,
                              \(x) stringr::str_replace(x,
                                                        "\\b(\\w+)\\(",
                                                        "<p class='code_to_html'><span class='code_to_html'>\\1</span>("))

  # colapsa a lista de strings
  code_p_span_collapsed <- paste0(code_p_span_split, collapse = "")

  # adicionando tags p ao início e final
  code_html <- paste0("<p>", code_p_span_collapsed, "</p>")

  return(code_html)
}

