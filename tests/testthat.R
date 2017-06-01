library(testthat)

library(googlesites)
options(googlesites.site_domain = "site")
options(googlesites.site_name = "sitesrtest")

library(googleAuthR)
options(googleAuthR.scopes.selected = "https://sites.google.com/feeds/")
options(googleAuthR.httr_oauth_cache = file.path("testthat", "token.rds"))
token_path <- file.path("testthat", "token.rds")
gar_auth(token=token_path)

if (identical(tolower(Sys.getenv("NOT_CRAN")), "true") & 
    identical(tolower(Sys.getenv("TRAVIS_PULL_REQUEST")), "false")) {
  
  test_check('googlesites')
  
}
