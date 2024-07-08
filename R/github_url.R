# Define the GitHub URL components class
github_url <- function(urls) {
  matches <- regmatches(urls, regexec('http[s]?://github.com/([^/]+)/([^/]+)', urls))
  tmp <- do.call(rbind, matches)
  tmp <- data.frame(tmp, stringsAsFactors = FALSE)
  colnames(tmp) <- c('url', 'user', 'repo')

  tmp$repo <- sub('\\.git$', '', tmp$repo)

  structure(tmp, class = c("data.frame", "github_url"))
}

# Method to print the GitHub URL components
print.github_url <- function(x, ...) {
  print.data.frame(x, ...)
}

# Example usage ----
urls <- c("http://github.com/tidyverse/dplyr")
url_parts <- github_url(urls)
url_parts

url_summary <- function(x, ...) {
  UseMethod("url_summary")
}

url_summary.github_url <- function(x, ...) {
  cat("GitHub URL Parts\n")
  cat("URL: ", x$url, "\n")
  cat("User: ", get_user(x), "\n")
  cat("Repository: ", get_repo_name(x), "\n")
  cat("User/Repository: ", get_user_repo(x), "\n")
  cat("Clone URL: ", gen_clone_url(x), "\n")
  cat("HTTPS URL: ", gen_https_url(x), "\n")
  cat("SSH URL: ", gen_ssh_url(x), "\n")
  cat("Clone Command: ", gen_clone_command(x), "\n")
}

# Extractions functions ----

get_user <- function(x) {
  UseMethod("get_user")
}

get_user.github_url <- function(x) {
  x$user
}

get_repo_name <- function(x) {
  UseMethod("get_repo_name")
}

get_repo_name.github_url <- function(x) {
  x$repo
}

get_user_repo <- function(x) {
  UseMethod("get_user_repo")
}

get_user_repo.github_url <- function(x) {
  paste0(x$user, "/", x$repo)
}

# Validation functions ----

check_domain <- function(x) {
  UseMethod("check_domain")
}

check_domain.github_url <- function(x) {
  all(grepl('github.com', x$url))
}

is_valid <- function(x) {
  UseMethod("is_valid")
}

is_valid.github_url <- function(x) {
  all(!is.na(x$user) & !is.na(x$repo))
}

# Generation functions ----

gen_clone_url <- function(x) {
  UseMethod("gen_clone_url")
}

gen_clone_url.github_url <- function(x) {
  paste0("https://github.com/", x$user, "/", x$repo, ".git")
}

gen_https_url <- function(x) {
  UseMethod("gen_https")
}

gen_https_url.github_url <- function(x) {
  paste0("https://github.com/", x$user, "/", x$repo)
}

gen_ssh_url <- function(x) {
  UseMethod("gen_ssh")
}

gen_ssh.github_url <- function(x) {
  paste0("git@github.com:", x$user, "/", x$repo, ".git")
}

gen_clone_command <- function(x) {
  UseMethod("gen_clone_command")
}

gen_clone_command.github_url <- function(x) {
  paste0("git clone ", gen_clone_url(x))
}
