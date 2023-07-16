#' Find the position of a word in a script
#'
#' @param script A script (character string)
#' @param word Name of a function
#'
#' @return a \code{data.frame}
#' @noRd
#'
#' @importFrom stringr str_split str_locate_all
#'
#' @examples
#' find_position(
#'  script = "dat <- data.frame(var1 = letters); head(dat)",
#'  word = "head"
#' )
find_position <- function(script, word) {
  script <- stringr::str_split(string = script, pattern = "\\n", simplify = TRUE)
  pos <- stringr::str_locate_all(string = script, pattern = paste0("(?<![[:alnum:]\\._])", word, "(?![[:alnum:]\\._])"))
  num_row <- unlist(lapply(pos, nrow))
  pos <- do.call("rbind", pos)
  pos <- as.data.frame(pos)
  pos$numrow <- rep(which(num_row > 0), num_row[num_row > 0])
  pos$word <- word
  pos$fun_context <- script[pos$numrow]
  pos
}




#' Extract all words of a script and get their position
#'
#' @param script A script (character string)
#'
#' @return a \code{data.frame}
#' @noRd
#'
#' @importFrom stringr str_split str_replace str_replace_all str_extract_all
#'
#' @examples
#' extract_words("dat <- data.frame(var1 = letters); head(dat)")
extract_words <- function(script) {
  script_ <- stringr::str_split(string = script, pattern = "\n")
  script_ <- unlist(script_)
  script_ <- stringr::str_replace(string = script_, pattern = "#.*", replacement = "")
  script_ <- stringr::str_replace(string = script_, pattern = "[[:alnum:]\\._]+[:space:]*<-", replacement = "")
  script_ <- stringr::str_replace(string = script_, pattern = "[[:alnum:]\\._]+[:space:]*=", replacement = "")
  script_ <- stringr::str_replace(string = script_, pattern = "\\$[[:alnum:]\\._]+", replacement = "")
  # script_ <- stringr::str_replace(string = script_, pattern = "(?<=\"|').*(?=\"|')", replacement = "")
  script_ <- stringr::str_replace_all(string = script_, pattern = "([\"'`])(?:(?=(\\\\?))\\2.)*?\\1", replacement = "")
  script_ <- stringr::str_replace_all(string = script_, pattern = "library\\(.*\\)", replacement = "")
  script_ <- stringr::str_replace_all(string = script_, pattern = "[[:alnum:]\\.]+::[[:alnum:]\\._]+", replacement = "")
  funs <- stringr::str_extract_all(string = script_, pattern = "[[:alnum:]\\._]+")
  pos <- lapply(X = unique(unlist(funs)), find_position, script = script)
  do.call("rbind", pos)
}




#' Validate functions in context
#'
#' @param data a \code{data.frame} obtained with \code{\link{get_script_funs}}.
#'
#' @return a \code{data.frame} with a new column \code{valid_context} indicating
#'  if function is used in context suitable to be prefixed.
#' @noRd
#'
#' @importFrom stringr str_sub
#'
#' @examples
#'
#' code <- c("# select variables", "select(iris, Sepal.Length)"
#' data_funs <- get_script_funs(code)
#' validate_context(data_funs)
#'
validate_context <- function(data) {
  data$fun_context_val <- data$fun_context
  for (i in seq_len(nrow(data))) {
    loc <- get_other_occurence(data, data$funs[i], data$numrow[i], data$start[i])
    if (nrow(loc) > 0) {
      # str_sub(string = data$fun_context_val[i], start = loc) <- ""
      for (j in seq_along(nrow(loc))) {
        if (j > 1) {
          loc_ <- loc[j, , drop = FALSE] - nchar(data$funs[1])
        } else {
          loc_ <- loc[j, , drop = FALSE]
        }
        stringr::str_sub(string = data$fun_context_val[i], start = loc_) <- ""
      }
    }
  }
  data$valid_context <- val_cont(data$fun_context_val, data$funs)
  data$fun_context_val <- NULL
  data
}

get_other_occurence <- function(data, fun, numrow, exclude.start) {
  data <- data[data$funs == fun & data$numrow == numrow & data$start != exclude.start, ]
  unique(as.matrix(data[, c("start", "end")]))
}

#' @importFrom stringr str_replace_all str_detect
val_cont <- function(contexts, funs) {
  cont <- stringr::str_replace_all(string = contexts, pattern = "#.*", replacement = "")
  cont <- stringr::str_replace_all(string = cont, pattern = "[[:alnum:]\\._]+[:space:]*<-", replacement = "")
  cont <- stringr::str_replace_all(string = cont, pattern = "[[:alnum:]\\._]+[:space:]*=", replacement = "")
  cont <- stringr::str_replace_all(string = cont, pattern = "[[:alnum:]\\._]+[:space:]*<", replacement = "")
  cont <- stringr::str_replace_all(string = cont, pattern = "[[:alnum:]\\._]+[:space:]*>", replacement = "")
  cont <- stringr::str_replace_all(string = cont, pattern = "[[:alnum:]\\._]+[:space:]*!", replacement = "")
  cont <- stringr::str_replace_all(string = cont, pattern = "[[:alnum:]\\._]+\\[", replacement = "")
  cont <- stringr::str_replace_all(string = cont, pattern = "\\$[[:alnum:]\\._]+", replacement = "")
  cont <- stringr::str_replace_all(string = cont, pattern = "::[[:alnum:]\\._]+", replacement = "")
  # remove anything between quotes
  cont <- stringr::str_replace_all(string = cont, pattern = "([\"'])(?:(?=(\\\\?))\\2.)*?\\1", replacement = "")
  stringr::str_detect(string = cont, pattern = paste0("(?<![[:alnum:]\\._])", funs, "(?![[:alnum:]\\._])"))
}




#' Add quote to multilines strings
#'
#' @param string A character string (script)
#'
#' @return modified string
#' @noRd
#'
#' @importFrom stringr str_split str_count str_c
#'
#' @examples
#'
#' stri <- '
#'
#' x <- 1 + 1
#'
#' y <- "
#' SELECT Sepal.Length
#' FROM iris
#' "
#' print(y)
#'
#' '
#' cat(split_char_string(stri))
#'
split_char_string <- function(string) {
  sc <- stringr::str_split(string = string, pattern = "\n")[[1]]
  add_quote <- function(strings, quote) {
    scc <- stringr::str_count(string = strings, pattern = quote) %% 2
    sccrle <- rle(x = cumsum(scc) %% 2)
    starts <- cumsum(sccrle$lengths) + 1
    starts <- starts[pmax(0, which(sccrle$values == 1) - 1)]
    ends <- cumsum(sccrle$lengths)[sccrle$values == 1] + 1
    middles <- mapply(FUN = seq, from = starts + 1, to = ends - 1, SIMPLIFY = FALSE)
    middles <- unlist(middles)
    sc[starts] <- stringr::str_c(sc[starts], quote)
    sc[middles] <- stringr::str_c(quote, sc[middles], quote)
    sc[ends] <- stringr::str_c(quote, sc[ends])
    sc
  }
  sc <- add_quote(sc, "\"")
  sc <- add_quote(sc, "'")
  stringr::str_c(sc, collapse = "\n")
}






#' Get functions used in a script with their position and others infos
#'
#' @param script A character string
#' @param filter_context Logical, remove function used in an incorrect context,
#'  in a commented line for example.
#' @param highlight Logical, add HTML tag around function in context.
#'
#' @return a \code{data.frame}
#' @noRd
#'
#' @importFrom stats ave
#' @importFrom stringr str_sub str_trim str_sub<-
#'
#' @examples
#'
#' code <- '
#'
#' dat <- read.table("path.txt")
#' str(dat)
#' head(dat)
#'
#' # rnorm & dnorm, dnorm will be ignored since
#' # it only appear in a commented line
#' rnorm(10)
#'
#' '
#'
#' get_script_funs(code)
#' get_script_funs(code, filter_context = FALSE)
#'
get_script_funs <- function(script, filter_context = TRUE, highlight = TRUE) {
  packages_funs <- get_package_funs()
  script_funs <- merge(x = packages_funs, y = extract_words(script = script), by.x = "funs", by.y = "word", all.x = FALSE)
  script_funs$ns_order <- match(x = script_funs$package, table = .packages())
  script_funs <- script_funs[order(script_funs$numrow, script_funs$start, script_funs$ns_order), ]
  # Ignore some packages
  ign_pack <- getOption(x = "prefixer.ignore.package", default = "base")
  script_funs <- script_funs[!script_funs$package %in% ign_pack, ]
  by <- script_funs[c("funs", "package", "numrow")]
  script_funs$occ_row <- ave(x = seq_len(nrow(script_funs)), by = by, FUN = seq_along)
  script_funs$nocc_row <- ave(x = seq_len(nrow(script_funs)), by = by, FUN = length)
  script_funs <- validate_context(script_funs)
  if (filter_context) {
    script_funs <- script_funs[script_funs$valid_context, ]
  }
  if (highlight) {
    hico <- stringr::str_sub(string = script_funs$fun_context, start = script_funs$start, end = script_funs$end)
    hico <- paste0("<b class='highlight-context'>", hico, "</b>")
    stringr::str_sub(string = script_funs$fun_context, start = script_funs$start, end = script_funs$end) <- hico
  }
  script_funs$fun_context <- stringr::str_trim(script_funs$fun_context)
  script_funs
}
