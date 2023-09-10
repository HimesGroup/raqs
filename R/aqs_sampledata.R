##' AQS API Sample Data service
##'
##' A collection of functions to fetch sample data - the finest grain data
##' reported to EPA. Please use a narrow range of dates to adhere to the API's
##' limit imposed on request size.
##'
##' [aqs_sampledata] sends a request to the AQS API based on a user-provided
##' filter using the following underlying functions:
##' - [sampledata_bysite] returns all `param` samples for `site` in `county`,
##' within `state`, between `bdate` and `edate`.
##' - [sampledata_bycounty] returns all `param` samples for `county` in `state`
##' between `bdate` and `edate`.
##' - [sampledata_bystate] returns all `param` samples for `state` between
##' `bdate` and `edate`.
##' - [sampledata_bybox] returns all `param` samples for a user-provided
##' latitude/longitude bounding box (`minlat`, `maxlat`, `minlon`, `maxlon`)
##' between `bdate` and `edate`.
##' - [sampledata_bycbsa] returns all `param` samples for a user-provided CBSA
##' between `bdate` and `edate`.
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
##' @param duration (optional) A string specifying the 1-character AQS sample
##'   duration code. A list of the duration codes can be obtained via
##'   [list_durations]. Only data reported at this sample duration will be
##'   returned.
##' @param cbdate (optional) A string specifying the change begin date in
##'   YYYYMMDD format to subset data based on "date of last change" in database.
##'   Only data that changed on or after this date will be returned.
##' @param cedate (optional) A string specifying the change end date in YYYYMMDD
##'   format to subset data based on "date of last change" in database. Only
##'   data that changed on or before this date will be returned.
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
##' ## FRM/FEM PM2.5 data for Wake County, NC between Jan and Feb 2016
##' aqs_variables <- list(
##'   param = "88101", bdate = "20160101", edate = "20160228",
##'   state = "37", county = "183"
##' )
##' aqs_sampledata(aqs_filter = "byCounty", aqs_variables = aqs_variables)
##'
##' ## Equivalent to above; used integers instead of strings
##' sampledata_bycounty(
##'   param = 88101, bdate = "20160101", edate = "20160228",
##'   state = 37, county = 183
##' )
##' }
##'
##' @export
aqs_sampledata <- function(aqs_filter = c("bySite", "byCounty", "byState",
                                          "byBox", "byCBSA"),
                           aqs_variables = NULL, header = FALSE, ...) {
  aqs_filter <- .match.arg(aqs_filter)
  aqs_variables <- c(.fill_user_to_list(as.list(aqs_variables)), header = header)
  .is_namedlist(aqs_variables)
  switch(
    aqs_filter,
    bySite = do.call(sampledata_bysite, aqs_variables),
    byCounty = do.call(sampledata_bycounty, aqs_variables),
    byState = do.call(sampledata_bystate, aqs_variables),
    byBox = do.call(sampledata_bybox, aqs_variables),
    byCBSA = do.call(sampledata_bycbsa, aqs_variables)
  )
}

##' @rdname aqs_sampledata
##' @export
sampledata_bysite <- function(param, bdate, edate, state, county, site,
                              email = get_aqs_email(), key = get_aqs_key(),
                              duration = NULL, cbdate = NULL, cedate = NULL,
                              header = FALSE, ...) {
  .run_api(
    aqs_service = "sampleData", aqs_filter = "bySite",
    aqs_variables = list(email = email, key = key, param = param,
                         bdate = bdate, edate = edate,
                         state = state, county = county, site = site,
                         duration = duration, cbdate = cbdate, cedate = cedate),
    header = header
  )
}

##' @rdname aqs_sampledata
##' @export
sampledata_bycounty <- function(param, bdate, edate, state, county,
                                email = get_aqs_email(), key = get_aqs_key(),
                                duration = NULL, cbdate = NULL, cedate = NULL,
                                header = FALSE, ...) {
  .run_api(
    aqs_service = "sampleData", aqs_filter = "byCounty",
    aqs_variables = list(email = email, key = key, param = param,
                         bdate = bdate, edate = edate,
                         state = state, county = county,
                         duration = duration, cbdate = cbdate, cedate = cedate),
    header = header
  )
}

##' @rdname aqs_sampledata
##' @export
sampledata_bystate <- function(param, bdate, edate, state,
                               email = get_aqs_email(), key = get_aqs_key(),
                               duration = NULL, cbdate = NULL, cedate = NULL,
                               header = FALSE, ...) {
  .run_api(
    aqs_service = "sampleData", aqs_filter = "byState",
    aqs_variables = list(email = email, key = key, param = param,
                         bdate = bdate, edate = edate, state = state,
                         duration = duration, cbdate = cbdate, cedate = cedate),
    header = header
  )
}

##' @rdname aqs_sampledata
##' @export
sampledata_bybox <- function(param, bdate, edate,
                             minlat, maxlat, minlon, maxlon,
                             email = get_aqs_email(), key = get_aqs_key(),
                             duration = NULL, cbdate = NULL, cedate = NULL,
                             header = FALSE, ...) {
  .run_api(
    aqs_service = "sampleData", aqs_filter = "byBox",
    aqs_variables = list(email = email, key = key, param = param,
                         bdate = bdate, edate = edate,
                         minlat = minlat, maxlat = maxlat,
                         minlon = minlon, maxlon = maxlon,
                         duration = duration, cbdate = cbdate, cedate = cedate),
    header = header
  )
}

##' @rdname aqs_sampledata
##' @export
sampledata_bycbsa <- function(param, bdate, edate, cbsa,
                              email = get_aqs_email(), key = get_aqs_key(),
                              duration = NULL, cbdate = NULL, cedate = NULL,
                              header = FALSE, ...) {
  .run_api(
    aqs_service = "sampleData", aqs_filter = "byCBSA",
    aqs_variables = list(email = email, key = key, param = param,
                         bdate = bdate, edate = edate, cbsa = cbsa,
                         duration = duration, cbdate = cbdate, cedate = cedate),
    header = header
  )
}
