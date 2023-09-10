##' Package options
##'
##' The following package options can be set via [options] and queried via
##' [getOption].
##'
##' @section Options to handle the AQS API rate limits:
##'
##' The AQS API recommends not to make more than 10 requests per minute and
##' pause 5 seconds between requests.
##' - `raqs.req_per_min` controls the maximum number of API requests per minute.
##' Default is 10.
##' - `raqs.delay_between_req` controls a delay between API requests sent via a
##' function when your `bdate` and `edate` inputs span multiple years. A value
##' will be rounded to the nearest integer. Default is 5 seconds.
##' - `raqs.delay_fun_exit` controls a delay before a function execution ends. A
##' value will be rounded to the nearest integer. Default is zero if R is being
##' used interactively. Otherwise, it is 5 seconds. This option only applies to
##' functions that send API requests.
##'
##' @section Option to handle the type of data object to return:
##'
##' By default, the parsed data will be returned as a [data.frame] object, but
##' can be adjusted for users' preferences.
##' - `raqs.return_type` controls the type of data object to return. Default is
##' "data.frame" but it can also be set to "tibble" or "data.table".
##'
##'
##' @usage NULL
##' @format NULL
##'
##' @examples
##'
##' ## Change for the duration of the session
##' op <- options(raqs.rep_per_min = 5)
##'
##' ## Change back to the original value
##' options(op)
##'
##' @name raqs_options
NULL
