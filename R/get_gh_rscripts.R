#' @title Get list of R script Github URLs from Repo
#' @description get_gh_rscripts() retrieves sourceable R code URLs from a Github repo
#' @param repo_name Github repository name with org or username included
#' @param branch_name Github repository brach name
#' @param folder folder to export files, defaults to 'examples' folder
#' @param filter_pattern regex pattern to filter file list by, '*R$' by default for R files
#' @return OUTPUT_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # get_gh_rscripts("tidymodels/TMwR", "main")
#'  }
#' }
#' @seealso
#'  \code{\link[httr]{GET}}, \code{\link[httr]{stop_for_status}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[stringr]{str_subset}}, \code{\link[stringr]{str_replace}}
#' @rdname get_gh_rscripts
#' @export
#' @importFrom httr GET stop_for_status
#' @importFrom glue glue
#' @importFrom stringr str_subset
get_gh_rscripts <- function(
    repo_name,
    branch_name,
    filter_pattern = "\\.R$"
  ){
  # get file list from repo of interest
  req <- httr::GET(
    glue::glue(
      "https://api.github.com/repos/{repo_name}/git/trees/{branch_name}?recursive=1"
    )
  )
  httr::stop_for_status(req)
  filelist <- unlist(
    lapply(
      httr::content(req)$tree, "[", "path"),
    use.names = F
  )
  # subset to code files
  fnames <- filelist %>%
    stringr::str_subset(filter_pattern)
  ref_files <- glue::glue(
    "https://raw.githubusercontent.com/{repo_name}/{branch_name}/{fnames}"
  )
  ref_files <- as.vector(ref_files)
  return(ref_files)
}
