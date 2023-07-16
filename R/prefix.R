prefix <- function(){
  script <- rstudioapi::getSourceEditorContext()$contents
  if (sum(nchar(script), na.rm = TRUE) == 0) {
    warning("It seems there are nothing to prefix...")
    return(invisible())
  }

  script_funs <- get_script_funs(script)

  if (nrow(script_funs) == 0) {
    warning("It seems there are nothing to prefix...")
    return(invisible())
  }

  to_load <- get_unloaded_packages(script = script)

  if (length(to_load) > 0) {
    warning(
      paste0(
        "These packages seems to be used but are not loaded : ",
        paste(to_load, collapse = ", ")
      )
    )
  }

  script_funs$suggested_fun = paste(
    script_funs$package, script_funs$funs, sep = "::"
  )


  script_funs <- script_funs %>%
    dplyr::select(funs, suggested_fun) %>%
    unique()

  activ_doc <- rstudioapi::getSourceEditorContext()

  replacements <- script_funs$suggested_fun
  patterns <- glue::glue("(?<!\\w::)\\b{script_funs$funs}\\b")

  script_ <- stringr::str_replace_all(
    script,
    set_names(replacements, patterns)
  )
  rstudioapi::insertText(
    location = Map(c, Map(c, seq_along(script), 1), Map(c, seq_along(script), nchar(script) + 1)),
    text = script_,
    id = activ_doc$id
  )

}
