##' AQS API QA PEP Audits service
##'
##' A collection of functions to fetch data related to PM2.5 monitoring system
##' audits.
##'
##' [aqs_qapepaudits] sends a request to the AQS API based on a user-provided
##' filter using the following underlying functions:
##' - [qapepaudits_bysite] returns PEP Audit data for `param` at `site` in
##' `county`, within `state`, between `bdate` and `edate`.
##' - [qapepaudits_bycounty] returns PEP Audit data for `param` in `county`
##' within `state` between `bdate` and `edate`.
##' - [qapepaudits_bystate] returns PEP Audit data for `param` in `state`
##' between `bdate` and `edate`.
##' - [qapepaudits_bypqao] returns PEP Audit data for `param` in `pqao` between
##' `bdate` and `edate`.
##' - [qapepaudits_byma] returns PEP Audit data for `param` in `agency`
##' (monitoring agency) between `bdate` and `edate`.
##'
##' @param aqs_filter A string specifying one of the service filters. NOT
##'   case-sensitive.
##' @param aqs_variables A named list of variables to fetch data (e.g.,
##'   `state`). Only necessary variables are passed to a specific endpoint
##'   (service/filter) to make a valid request.
##' @param header A logical specifying whether the function returns additional
##'   information from the API header. Default is `FALSE` to return data only.
##' @param param A string or vector of strings specifying the 5-digit AQS
##'   parameter code for data selection. An integer will be coerced to a string.
##'   A maximum of 5 parameter codes may be listed in a single request. A list
##'   of the parameter codes can be obtained via [list_parametersbyclass].
##' @param bdate A string specifying the begin date of data selection in
##'   YYYYMMDD format. Only data on or after this date will be returned.
##' @param edate A string specifying the end date of data selection in YYYYMMDD
##'   format. Only data on or before this date will be returned. If the end date
##'   is not in the same year as the begin date, the function will automatically
##'   split the date range into multiple chunks by year and send requests
##'   sequentially.
##' @param state A string specifying the 2-digit state FIPS code. An integer
##'   will be coerced to a string with a leading zero if necessary (e.g., 1 ->
##'   "01"). A list of the state codes can be obtained via [list_states].
##' @param county A string specifying the 3-digit county FIPS code. An integer
##'   will be coerced to a string with leading zeros if necessary (e.g., 89 ->
##'   "089"). A list of the county codes within each state can be obtained via
##'   [list_countiesbystate].
##' @param site A string specifying the 4-digit AQS site number within the
##'   county. An integer will be coerced to a string with leading zeros if
##'   necessary (e.g., 14 -> "0014"). A list of the site codes within each
##'   county can be obtained via [list_sitesbycounty].
##' @param pqao A string specifying the AQS Primary Quality Assurance
##'   Organization (PQAO) code. A list of the PQAO codes can be obtained via
##'   [list_pqaos].
##' @param agency A string specifying the AQS Monitoring Agency (MA) code. A
##'   list of the MA codes can be obtained via [list_mas]. Here, we named this
##'   input as `agency` instead of "ma" because `agency` is actually used in the
##'   API endpoint URL.
##' @param email A string specifying the email address of the requester. If you
##'   set your email and key with [set_aqs_user], you don't have to specify
##'   this.
##' @param key A string specifying the key matching the email address for the
##'   requester. If you set your email and key with [set_aqs_user], you don't
##'   have to specify this.
##' @param ... Reserved for future use.
##'
##' @return A data.frame containing parsed data or a named list containing
##'   header and data.
##'
##' @examples
##' \dontrun{
##'
##' ## Set your API Key first using set_aqs_user to run the following codes
##'
##' ## Example from the AQS website
##' ## PEP Audit data for FRM PM2.5 in Alabama for 2017
##' aqs_variables <- list(
##'   param = "88101", bdate = "20170101", edate = "20171231",
##'   state = "01"
##' )
##' aqs_qapepaudits(
##'   aqs_filter = "byState", aqs_variables = aqs_variables
##' )
##'
##' ## Equivalent to above; used integers instead of strings
##' qapepaudits_bystate(
##'   param = 88101, bdate = "20170101", edate = "20171231",
##'   state = 1
##' )
##' }
##'
##' @export
aqs_qapepaudits <- function(aqs_filter = c("bySite", "byCounty", "byState",
                                           "byPQAO", "byMA"),
                            aqs_variables = NULL, header = FALSE, ...) {
  aqs_filter <- .match.arg(aqs_filter)
  aqs_variables <- c(.fill_user_to_list(as.list(aqs_variables)), header = header)
  .is_namedlist(aqs_variables)
  switch(
    aqs_filter,
    bySite = do.call(qapepaudits_bysite, aqs_variables),
    byCounty = do.call(qapepaudits_bycounty, aqs_variables),
    byState = do.call(qapepaudits_bystate, aqs_variables),
    byPQAO = do.call(qapepaudits_bypqao, aqs_variables),
    byMA = do.call(qapepaudits_byma, aqs_variables)
  )
}

##' @rdname aqs_qapepaudits
##' @export
qapepaudits_bysite <- function(param, bdate, edate, state, county, site,
                               email = get_aqs_email(), key = get_aqs_key(),
                               header = FALSE, ...) {
  .run_api(
    aqs_service = "qaPepAudits", aqs_filter = "bySite",
    aqs_variables = list(email = email, key = key, param = param,
                         bdate = bdate, edate = edate,
                         state = state, county = county, site = site),
    header = header
  )
}

##' @rdname aqs_qapepaudits
##' @export
qapepaudits_bycounty <- function(param, bdate, edate, state, county,
                                 email = get_aqs_email(), key = get_aqs_key(),
                                 header = FALSE, ...) {
  .run_api(
    aqs_service = "qaPepAudits", aqs_filter = "byCounty",
    aqs_variables = list(email = email, key = key, param = param,
                         bdate = bdate, edate = edate,
                         state = state, county = county),
    header = header
  )
}

##' @rdname aqs_qapepaudits
##' @export
qapepaudits_bystate <- function(param, bdate, edate, state,
                                email = get_aqs_email(), key = get_aqs_key(),
                                header = FALSE, ...) {
  .run_api(
    aqs_service = "qaPepAudits", aqs_filter = "byState",
    aqs_variables = list(email = email, key = key, param = param,
                         bdate = bdate, edate = edate, state = state),
    header = header
  )
}

##' @rdname aqs_qapepaudits
##' @export
qapepaudits_bypqao <- function(param, bdate, edate, pqao,
                               email = get_aqs_email(), key = get_aqs_key(),
                               header = FALSE, ...) {
  .run_api(
    aqs_service = "qaPepAudits", aqs_filter = "byPQAO",
    aqs_variables = list(email = email, key = key, param = param,
                         bdate = bdate, edate = edate, pqao = pqao),
    header = header
  )
}

##' @rdname aqs_qapepaudits
##' @export
qapepaudits_byma <- function(param, bdate, edate, agency,
                             email = get_aqs_email(), key = get_aqs_key(),
                             header = FALSE, ...) {
  .run_api(
    aqs_service = "qaPepAudits", aqs_filter = "byMA",
    aqs_variables = list(email = email, key = key, param = param,
                         bdate = bdate, edate = edate, agency = agency),
    header = header
  )
}
