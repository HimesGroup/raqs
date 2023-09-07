.onLoad <- function(libname, pkgname) {
  op <- options()
  op.raqs <- list(raqs.pause = 5)
  if (interactive()) op.raqs$raqs.pause <- 0
  toset <- !(names(op.raqs) %in% names(op))
  if (any(toset)) options(op.raqs[toset])
  invisible()
}
