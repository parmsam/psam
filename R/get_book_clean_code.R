#' @title Get clean code from book repo
#' @description get_book_clean_code() retrieves clean R code from book repository based on Rmd files
#' @param repo_name Github repository name with org or username included
#' @param branch_name Github repository brach name
#' @param folder folder to export files, defaults to 'examples' folder
#' @param filter_pattern regex pattern to filter file list by
#' @return OUTPUT_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # get_book_clean_code("tidymodels/TMwR", "main", "examples", "^[01-20]")
#'  }
#' }
#' @seealso
#'  \code{\link[httr]{GET}}, \code{\link[httr]{stop_for_status}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[stringr]{str_subset}}, \code{\link[stringr]{str_replace}}
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
    filter_pattern = ".*"){
  # get file list from repo of interest
  req <- httr::GET(
    glue::glue(
      "https://api.github.com/repos/{repo_name}/git/trees/{branch_name}?recursive=1"
    )
  )
  httr::stop_for_status(req)
  filelist <- unlist(
    lapply(httr::content(req)$tree, "[", "path"),
    use.names = F
  )
  # subset to code files
  rmd_fnames <- filelist %>% stringr::str_subset(filter_pattern)
  ref_rmds <- glue::glue(
    "https://raw.githubusercontent.com/{repo_name}/{branch_name}/{rmd_fnames}"
  )
  out_fnames <- rmd_fnames %>%
    stringr::str_replace_all(".Rmd", ".R")
  out_rs <- glue::glue("{folder}/{out_fnames}")
  # convert rmd files from Github into R scripts with local save
  purrr::map2(ref_rmds,
       out_rs, \(a, b) {
         knitr::purl(input = a,
                     output = b,
                     documentation = 0)
       })
}
