.onLoad <- function(libname, pkgname) {
  op <- options()
  op.raqs <- list(
    raqs.req_per_min = 10,
    raqs.delay_between_req = 5,
    raqs.delay_fun_exit = 5,
    raqs.return_type = "data.frame"
  )
  if (interactive()) op.raqs$raqs.delay_fun_exit <- 0
  toset <- names(op.raqs) %ni% names(op)
  if (any(toset)) options(op.raqs[toset])
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the raqs package!")
  email <- Sys.getenv("AQS_EMAIL")
  key <- Sys.getenv("AQS_KEY")
  if (!(.is_nonempty_str(email) && .is_nonempty_str(key))) {
    packageStartupMessage(
      "Please set your user credentials first with 'set_aqs_user(email, key)'."
    )
  }
  packageStartupMessage(
    "See ?raqs_options to adjust options for the rate of requests."
  )
}
