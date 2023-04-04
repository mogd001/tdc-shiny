Sys.unsetenv("GITHUB_PAT")
devtools::install_github("rstudio/rsconnect")

library(rsconnect)
rsconnect::deployApp()
