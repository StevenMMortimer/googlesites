
googlesites
===========

[![Build Status](https://travis-ci.org/StevenMMortimer/googlesites.svg?branch=master)](https://travis-ci.org/StevenMMortimer/googlesites) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/StevenMMortimer/googlesites?branch=master&svg=true)](https://ci.appveyor.com/project/StevenMMortimer/googlesites) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/googlesites)](http://cran.r-project.org/package=googlesites) [![Coverage Status](https://codecov.io/gh/StevenMMortimer/googlesites/branch/master/graph/badge.svg)](https://codecov.io/gh/StevenMMortimer/googlesites?branch=master)

**googlesites** is the R implementation of the Google Sites API. Using this package assumes you've used the Web UI to create your site. Once you've got a site (and maybe some templates), you can use this package to add more content, add attachments, find content, and delete content. This package allows you to store HTML pages and other static content locally and under version control and push it to your site programmatically whenever you feel like it.

See my blog post on why Google Sites aren't such a bad thing: [Google Sites for Documentation](https://stevenmortimer.com/google-sites-for-documentation/)

### Vignettes

The README below outlines some basic functionality, for more practical scenarios, please refer to the vignettes:

-   [Getting Started with googlesites](https://rawgit.com/StevenMMortimer/googlesites/master/vignettes/getting-started-with-googlesites.html)

### Basic Usage

First, you'll need to install this package from GitHub.

``` r
devtools::install_github("StevenMMortimer/googlesites")
```

Second, you'll need to go through a process of authenticating with Google and specifying the domain and site name so you don't have to refer to it explicitly each time you call a function.

``` r
library(googlesites)
options(googlesites.site_domain = "site") # or your Apps domain
options(googlesites.site_name = "my-site")

library(googleAuthR)
options(googleAuthR.client_id = "012345678901-99thisisatest99.apps.googleusercontent.com")
options(googleAuthR.client_secret = "Th1s1sMyC1ientS3cr3t")
# or you can use the package default client
options(googleAuthR.client_id = getOption("googlesites.client_id"))
options(googleAuthR.client_secret = getOption("googlesites.client_secret"))
options(googleAuthR.scopes.selected = "https://sites.google.com/feeds/")
gar_auth()
```

Third, do stuff to your site! After you've loaded and configured that package, you can take advantage of all the functionality of this package to manage your site. Like this:

#### Add a page from HTML

``` r
test_html <- system.file("extdata", "example-site", "test.html", package="googlesites")
add_html_page(page_xhtml_source = test_html,
              page_title = 'API Test',
              page_custom_url = 'api-test',
              overwrite=TRUE)
#> Auto-refreshing stale OAuth token.
#> Creating New Page
#> Successfully Created Page Skeleton
#> Replacing Links to Local Files
#> Successfully Added All Page Content
#> $page_id
#> [1] "https://sites.google.com/feeds/content/site/sitesrtest/9051074437493288219"
#> 
#> $page_url
#> [1] "https://sites.google.com/site/sitesrtest/api-test"
```

#### Find your Content

``` r
find_content(value_to_match='API Test', 
             field_to_match='title', 
             content_category='webpage')
#>                                                                           id
#> 1 https://sites.google.com/feeds/content/site/sitesrtest/9051074437493288219
#>                                                 url category    title
#> 1 https://sites.google.com/site/sitesrtest/api-test  webpage API Test
#>   parent_page_id content_type
#> 1                       xhtml
```

#### Upload an Attachment to your Page

``` r
pdf_cheatsheet <- 'https://www.rstudio.com/wp-content/uploads/2016/03/rmarkdown-cheatsheet-2.0.pdf'
download.file(pdf_cheatsheet, 'rmarkdown-cheatsheet-2.0.pdf', mode="wb")

upload_file_to_site(local_file_path = "rmarkdown-cheatsheet-2,0.pdf",
                    parent_page_id = find_content(value_to_match='API Test', 
                                                  field_to_match='title')$id)
```

#### Delete your Page

``` r
delete_content(id = find_content(value_to_match='API Test',
                                 field_to_match='title')$id)
#> Content Succesfully Deleted.
```

### Additional Features

This package currently supports the bare minimum for maintaining and uploading content to a Google Site. It's anticipated that other functions to list sites, create sites, update site categories, and manage user permissions will be created and added to this package in the future to expand its capabilities. Thank you for using.
