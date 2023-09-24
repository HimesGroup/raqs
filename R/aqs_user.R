##' Set your AQS API credentials
##'
##' Set your registered email and key as environmental variables for the current
##' session. Please sign up first using [aqs_signup] if you haven't set up an
##' account on the AQS API. If you want to set your email and key permanently,
##' please add the following lines in your .Renviron file:
##' \itemize{
##'   \item AQS_EMAIL = YOUR REGISTERED EMAIL
##'   \item AQS_KEY = YOUR API KEY
##' }
##'
##' [set_aqs_user] sets your API credentials for the current session.
##' [get_aqs_user], [get_aqs_email], and [get_aqs_key] are helper functions to
##' display saved user values.
##'
##' @param email A string specifying your registered email address
##' @param key A string specifying your API key
##'
##' @return No return value, called to set environmental variables
##'
##' @examples
##' ## Please use your registered email and key
##' set_aqs_user(email = "your@registered.email", key = "your_api_key")
##'
##' ## Show your API credentials
##' get_aqs_user() # return list(email, key)
##' get_aqs_email() # return email
##' get_aqs_key() # return key
##'
##' @seealso See [aqs_signup] to create an account for the AQS API
##'
##' @export
set_aqs_user <- function(email, key) {
  if (!(.is_nonempty_str(email) && .is_nonempty_str(key))) {
    stop("Please enter a valid email and key")
  }
  Sys.setenv(AQS_EMAIL = email, AQS_KEY = key)
}

##' @rdname set_aqs_user
##' @export
get_aqs_user <- function() {
  list(email = get_aqs_email(), key = get_aqs_key())
}

##' @rdname set_aqs_user
##' @export
get_aqs_email <- function() {
  email <- Sys.getenv("AQS_EMAIL")
  if (!.is_nonempty_str(email)) {
    stop("Empty string found! ",
         "Please set your registered email using `set_aqs_user(email, key)`")
  }
  email
}

##' @rdname set_aqs_user
##' @export
get_aqs_key <- function() {
  key <- Sys.getenv("AQS_KEY")
  if (!.is_nonempty_str(key)) {
    stop("Empty string found! ",
         "Please set your registered key using `set_aqs_user(email, key)`")
  }
  key
}

##' Create an account for the AQS API
##'
##' This function helps you create an account or reset a password. Once you
##' execute this function, a verification email will be sent to the email
##' account specified. If the request is made with an email that is already
##' registered, a new key will be issued for that account and emailed to the
##' listed address.
##'
##' @param email A string specifying an email account to register as a user
##'
##' @return No return value, called to sign up for the AQS API
##'
##' @examples
##' \dontrun{
##'
##' ## Please use your email address to create an account
##'
##' aqs_signup(email = "youremail@toregister.com")
##' }
##'
##' @seealso See [set_aqs_user] to set your credentials to send a request to the
##'   AQS API.
##'
##' @export
aqs_signup <- function(email) {
  aqs_url <- .get_api_endpoint(
    aqs_service = "signup", aqs_filter = NULL,
    aqs_variables = list(email = email)
  )
  aqs_resp <- aqs_url |>
    req_error(body = function(resp) {
      resp_body_json(resp, simplifyVector = TRUE)$Header$error[[1]]
      ## unlist(resp_body_json(resp, simplifyVector = TRUE)$Header$error)
    }) |>
    req_perform()
  msg <- resp_body_json(aqs_resp, simplifyVector = TRUE)$Data
  message("Success! ", msg)
  invisible()
}
