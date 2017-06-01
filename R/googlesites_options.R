.onLoad <- function(libname, pkgname) {

  op <- options()
  op.googlesites <- list(
    googlesites.gdata_version = '1.4',
    googlesites.host = 'sites.google.com',
    googlesites.site_domain = 'site',
    googlesites.site_name = NULL
  )
  toset <- !(names(op.googlesites) %in% names(op))
  if(any(toset)) options(op.googlesites[toset])

  invisible()

}
