.onLoad <- function(libname, pkgname) {

  op <- options()
  op.googlesites <- list(
    googlesites.gdata_version = '1.4',
    googlesites.host = 'sites.google.com',
    googlesites.client_id = "857567067257-0njk5c8ibn38k4bohh873pr091h0pt23.apps.googleusercontent.com",
    googlesites.client_secret = "EykGd4dIFcElqtwpYQPwfC-p",
    googlesites.site_domain = 'site',
    googlesites.site_name = NULL
  )
  toset <- !(names(op.googlesites) %in% names(op))
  if(any(toset)) options(op.googlesites[toset])

  invisible()

}
