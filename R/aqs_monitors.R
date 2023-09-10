##' AQS API Monitors service
##'
##' A collection of functions to fetch operational information about the
##' samplers (monitors) used to collect data, including identifying information,
##' operational dates, operating organizations, and etc.
##'
##' [aqs_monitors] sends a request to the AQS API based on a user-provided
##' filter using the following underlying functions:
##' - [monitors_bysite] returns `param` monitors that were operating at `site`
##' in `county`, within `state`, between `bdate` and `edate`.
##' - [monitors_bycounty] returns `param` monitors that were operating in
##' `county` within `state` between `bdate` and `edate`.
##' - [monitors_bystate] returns `param` monitors that were operating in `state`
##' between `bdate` and `edate`.
##' - [monitors_bybox] returns `param` monitors that were operating at a
##' user-provided latitude/longitude bounding box (`minlat`, `maxlat`, `minlon`,
##' `maxlon`) between `bdate` and `edate`.
##' - [monitors_bycbsa] returns `param` monitors that were operating at a
##' user-provided CBSA between `bdate` and `edate`.
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
##'   format. Only data on or before this date will be returned.
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
##' @param minlat A string or numeric value specifying the minimum latitude of a
##'   geographic box. Decimal latitude with north being positive.
##' @param maxlat A string or numeric value specifying the maximum latitude of a
##'   geographic box. Decimal latitude with north being positive.
##' @param minlon A string or numeric value specifying the minimum longitude of
##'   a geographic box. Decimal longitude with east being positive.
##' @param maxlon A string or numeric value specifying the maximum longitude of
##'   a geographic box. Decimal longitude with east being positive.
##' @param cbsa A string specifying the AQS CBSA code. A list of the CBSA codes
##'   can be obtained via [list_cbsas].
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
##' ## SO2 monitors in Hawaii that were operating on May 01, 2015
##' aqs_variables <- list(
##'   param = "42401", bdate = "20150501", edate = "20150502", state = "15"
##' )
##' aqs_monitors(aqs_filter = "bySite", aqs_variables = aqs_variables)
##'
##' ## Equivalent to above; used integers instead of strings
##' monitors_bystate(
##'   param = 42401, bdate = "20150501", edate = "20150502", state = 15
##' )
##' }
##' @export
aqs_monitors <- function(aqs_filter = c("bySite", "byCounty", "byState",
                                        "byBox", "byCBSA"),
                         aqs_variables = NULL, header = FALSE, ...) {
  aqs_filter <- .match.arg(aqs_filter)
  aqs_variables <- c(.fill_user_to_list(as.list(aqs_variables)), header = header)
  .is_namedlist(aqs_variables)
  switch(
    aqs_filter,
    bySite = do.call(monitors_bysite, aqs_variables),
    byCounty = do.call(monitors_bycounty, aqs_variables),
    byState = do.call(monitors_bystate, aqs_variables),
    byBox = do.call(monitors_bybox, aqs_variables),
    byCBSA = do.call(monitors_bycbsa, aqs_variables)
  )
}

##' @rdname aqs_monitors
##' @export
monitors_bysite <- function(param, bdate, edate, state, county, site,
                            email = get_aqs_email(), key = get_aqs_key(),
                            header = FALSE, ...) {
  .run_api(
    aqs_service = "monitors", aqs_filter = "bySite",
    aqs_variables = list(email = email, key = key, param = param,
                         bdate = bdate, edate = edate,
                         state = state, county = county, site = site),
    header = header
  )
}

##' @rdname aqs_monitors
##' @export
monitors_bycounty <- function(param, bdate, edate, state, county,
                              email = get_aqs_email(), key = get_aqs_key(),
                              header = FALSE, ...) {
  .run_api(
    aqs_service = "monitors", aqs_filter = "byCounty",
    aqs_variables = list(email = email, key = key, param = param,
                         bdate = bdate, edate = edate,
                         state = state, county = county),
    header = header
  )
}

##' @rdname aqs_monitors
##' @export
monitors_bystate <- function(param, bdate, edate, state,
                             email = get_aqs_email(), key = get_aqs_key(),
                             header = FALSE, ...) {
  .run_api(
    aqs_service = "monitors", aqs_filter = "byState",
    aqs_variables = list(email = email, key = key, param = param,
                         bdate = bdate, edate = edate,
                         state = state),
    header = header
  )
}

##' @rdname aqs_monitors
##' @export
monitors_bybox <- function(param, bdate, edate,
                           minlat, maxlat, minlon, maxlon,
                           email = get_aqs_email(), key = get_aqs_key(),
                           header = FALSE, ...) {
  .run_api(
    aqs_service = "monitors", aqs_filter = "byBox",
    aqs_variables = list(email = email, key = key, param = param,
                         bdate = bdate, edate = edate,
                         minlat = minlat, maxlat = maxlat,
                         minlon = minlon, maxlon = maxlon),
    header = header
  )
}

##' @rdname aqs_monitors
##' @export
monitors_bycbsa <- function(param, bdate, edate, cbsa,
                            email = get_aqs_email(), key = get_aqs_key(),
                            header = FALSE, ...) {
  .run_api(
    aqs_service = "monitors", aqs_filter = "byCBSA",
    aqs_variables = list(email = email, key = key, param = param,
                         bdate = bdate, edate = edate, cbsa = cbsa),
    header = header
  )
}
