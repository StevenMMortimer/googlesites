library(testthat)

library(googlesites)
options(googlesites.site_domain = "site")
options(googlesites.site_name = "testrgooglsites")

library(googleAuthR)
options(googleAuthR.scopes.selected = "https://sites.google.com/feeds/")
gar_auth()

if (identical(tolower(Sys.getenv("NOT_CRAN")), "true") & 
    identical(tolower(Sys.getenv("TRAVIS_PULL_REQUEST")), "false")) {
  
  test_check('googlesites')
  
}
