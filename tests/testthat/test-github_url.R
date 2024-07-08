# Example usage ----
urls <- c("http://github.com/tidyverse/dplyr")
url_parts <- create_github_url(urls)

# get_user(url_parts)
# get_repo_name(url_parts)
# get_user_repo(url_parts)
#
# is_valid(url_parts)

# gen_clone_url(url_parts)
# gen_https_url(url_parts)
# gen_ssh_url(url_parts)

print(url_parts)
url_summary(url_parts)
