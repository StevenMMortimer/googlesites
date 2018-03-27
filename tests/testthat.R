library(testthat)

library(googlesites)
options(googlesites.site_domain = "site")
options(googlesites.site_name = "sitesrtest")

library(googleAuthR)
options(googleAuthR.client_id = getOption("googlesites.client_id"))
options(googleAuthR.client_secret = getOption("googlesites.client_secret"))
options(googleAuthR.scopes.selected = "https://sites.google.com/feeds/")
this_token <- readRDS(file.path("testthat", "googlesites_token.rds"))
gar_auth(this_token)

if (identical(tolower(Sys.getenv("NOT_CRAN")), "true") & 
    identical(tolower(Sys.getenv("TRAVIS_PULL_REQUEST")), "false")) {
  
  test_check('googlesites')
  
}
