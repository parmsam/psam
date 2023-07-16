#' @title Coerce classes in dataframe based on reference dataframe
#' @description Coerces column classes of a dataframe based on a ref dataframe
#' @param truth truth dataframe to base new classes on
#' @param source source dataframe whose clases should be updated
#' @return source dataframe with classes coerced based on truth dataframe
#' @examples
#' \dontrun{
#' if(interactive()){
#'  df1 <- data.frame(a = sample(1:10, 5), b = as.character(1:5))
#'  df2 <- data.frame(a = as.character(sample(1:10, 5)), b = 1:5)
#'  coerce_df_cols(truth = df1, source = df2)
#'  }
#' }
#' @seealso
#'  \code{\link[janitor]{compare_df_cols}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{arrange}}, \code{\link[dplyr]{filter}}
#'  \code{\link[purrr]{map2}}
#'  \code{\link[tibble]{as_tibble}}
#' @export
#' @importFrom janitor compare_df_cols
#' @importFrom dplyr mutate arrange filter %>%
#' @importFrom purrr map2
#' @importFrom tibble as_tibble

coerce_df_cols <- function(truth, source){
  cdfc <- janitor::compare_df_cols(truth, source)
  col_names_truth <- colnames(truth)
  col_names_source <- colnames(source)

  cdfc_filt <- cdfc %>%
    dplyr::mutate(column_name = factor(column_name, levels = col_names_source)) %>%
    dplyr::arrange(column_name) %>%
    dplyr::filter(!is.na(column_name))

  source %>%
    purrr::map2(
      cdfc_filt$truth,
      \(a,b){
        if(!is.na(b)) as(a,b)
        else a
      }) %>%
    tibble::as_tibble()
}
