
# Required libraries
if (!requireNamespace("rvest", quietly = TRUE)) install.packages("rvest")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
library(rvest)
library(jsonlite)


get_remote_git_tags <- function(repo_url) {
  # Check if Git is available
  if (Sys.which("git") == "") {
    stop("Git is not installed or not found in PATH.")
  }

  # Run git ls-remote --tags
  cmd <- sprintf('git ls-remote --tags %s', repo_url)
  result <- tryCatch(system(cmd, intern = TRUE), error = function(e) return(NULL))

  if (is.null(result)) {
    message("Failed to retrieve tags.")
    return(NULL)
  }

  # Extract tag names from the output
  tag_lines <- grep("refs/tags/", result, value = TRUE)
  tag_names <- sub(".*refs/tags/", "", tag_lines)
  tag_names <- sub("\\^\\{\\}", "", tag_names)  # remove dereferenced annotated tags

  unique(tag_names)
}


get_latest_versions <- function() {
  versions <- list()

  # 1. Latest R version (from GitHub releases mirror)
  try({
    r_releases <- fromJSON("https://api.github.com/repos/r-hub/R/releases")
    versions$R <- r_releases$tag_name[1]
  }, silent = TRUE)

  # 2. Latest RStudio version (scraping Posit release notes)
  try({
    url_rstudio <- "https://docs.posit.co/supported-versions/rstudio.html"
    page <- read_html(url_rstudio)
    header_texts <- page %>% html_nodes("h3") %>% html_text()
    versions$rstudio <- header_texts[1]
  }, silent = TRUE)

  # 3. Latest Git version (from official Git downloads page)
  try({
    tags <- get_remote_git_tags("https://git.kernel.org/pub/scm/git/git.git/")

    cleaner_tags <- gsub("v", "", tags)

    safe_package_version <- purrr::safely(as.package_version)

    git_versions <- purrr::keep(cleaner_tags,
                            function(x) is.package_version(safe_package_version(x)$result))
    versions$git <- as.character(max(as.package_version(git_versions)))
  }, silent = TRUE)

  versions
}

latest <- get_latest_versions()
print(latest)
