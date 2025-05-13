#' @title Get clean code from book repo
#' @description Retrieves clean R code from a book repository based on Rmd files.
#' @param repo_name GitHub repository name (e.g., "username/repo").
#' @param branch_name GitHub repository branch name.
#' @param folder Folder to export files; defaults to 'examples'. If it doesn't exist, it will be created.
#' @param filter_pattern Regex pattern to filter file list; defaults to ".*" (all files).
#' @return A list of output file paths for the generated R scripts.
#' @examples
#' \dontrun{
#' if(interactive()){
#'   # Example usage
#'   get_book_clean_code("tidymodels/TMwR", "main", "examples", "^[01-20]")
#'   get_book_clean_code("RConsortium/S7", "main", "vignettes", "Rmd")
#' }
#' }
#' @seealso
#'  \code{\link[httr]{GET}}, \code{\link[httr]{stop_for_status}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[stringr]{str_subset}}, \code{\link[stringr]{str_replace_all}}
#'  \code{\link[purrr]{map2}}
#'  \code{\link[knitr]{knit}}
#' @rdname get_book_clean_code
#' @export
#' @importFrom httr GET stop_for_status
#' @importFrom glue glue
#' @importFrom stringr str_subset str_replace_all
#' @importFrom purrr map2
#' @importFrom knitr purl
get_book_clean_code <- function(
    repo_name,
    branch_name,
    folder = "examples",
    filter_pattern = ".*") {

  # Validate inputs
  if (missing(repo_name) || repo_name == "") {
    stop("Error: 'repo_name' cannot be empty.")
  }
  if (missing(branch_name) || branch_name == "") {
    stop("Error: 'branch_name' cannot be empty.")
  }
  if (!dir.exists(folder)) {
    message(glue::glue("Creating folder: {folder}"))
    dir.create(folder, recursive = TRUE)
  }

  # Retrieve the file list from the repository
  message("Fetching file list from the repository...")
  req <- httr::GET(
    glue::glue(
      "https://api.github.com/repos/{repo_name}/git/trees/{branch_name}?recursive=1"
    )
  )
  httr::stop_for_status(req)

  content <- httr::content(req)
  if (is.null(content$tree)) {
    stop("Error: No files found in the repository. Check 'repo_name' and 'branch_name'.")
  }

  filelist <- unlist(
    lapply(content$tree, "[", "path"),
    use.names = FALSE
  )

  # Filter files based on the pattern
  rmd_fnames <- filelist %>% stringr::str_subset(filter_pattern)
  if (length(rmd_fnames) == 0) {
    stop("Error: No files matched the provided 'filter_pattern'.")
  }

  ref_rmds <- glue::glue(
    "https://raw.githubusercontent.com/{repo_name}/{branch_name}/{rmd_fnames}"
  )
  out_fnames <- rmd_fnames %>%
    stringr::str_replace_all(".Rmd$", ".R")
  out_rs <- glue::glue("{folder}/{out_fnames}")

  # Convert Rmd files to R scripts and save locally
  message("Converting Rmd files to R scripts...")
  results <- purrr::map2(ref_rmds, out_rs, \(input_url, output_path) {
    tryCatch({
      knitr::purl(input = input_url,
                  output = output_path,
                  documentation = 0)
      message(glue::glue("Converted: {input_url} -> {output_path}"))
      return(output_path)
    }, error = function(e) {
      warning(glue::glue("Failed to convert: {input_url}. Error: {e$message}"))
      return(NULL)
    })
  })

  # Return list of successfully generated files
  results <- unlist(results)
  if (length(results) == 0) {
    warning("No files were successfully processed. Check the input parameters and repository structure.")
  }
  return(results)
}
