##' AQS API List service
##'
##' A collection of functions to fetch variable values you need to create other
##' service requests. All outputs are a value and the definition of that value.
##'
##' [aqs_list] sends a request to the AQS API based on a user-provided filter
##' using the following underlying functions:
##' - [list_states] returns a list of the states and their FIPS codes.
##' - [list_countiesbystate] returns a list of all counties within a
##' user-provided state.
##' - [list_sitesbycounty] returns a list of all sites within a user-provided
##' county.
##' - [list_cbsas] returns a list of the 5-digit Core Based Statistical Area
##' (CBSA) codes.
##' - [list_classes] returns a list of parameter class codes.
##' - [list_parametersbyclass] returns all parameters in a user-provided
##' parameter class.
##' - [list_pqaos] returns a list of AQS Primary Quality Assurance Organization
##' (PQAO) codes.
##' - [list_mas] returns a list of AQS Monitoring Agency (MA) codes.
##' - [list_durations] returns a list of the 1-character AQS sample duration
##' codes.
##'
##' @param aqs_filter A string specifying one of the service filters. NOT
##'   case-sensitive.
##' @param aqs_variables A named list of variables to fetch data (e.g.,
##'   `state`). Only necessary variables are passed to a specific endpoint
##'   (service/filter) to make a valid request.
##' @param header A logical specifying whether the function returns additional
##'   information from the API header. Default is `FALSE` to return data only.
##' @param state A string specifying the 2-digit state FIPS code. An integer
##'   will be coerced to a string with a leading zero if necessary (e.g., 1 ->
##'   "01"). A list of the state codes can be obtained via [list_states].
##' @param county A string specifying the 3-digit county FIPS code. An integer
##'   will be coerced to a string with leading zeros if necessary (e.g., 89 ->
##'   "089"). A list of the county codes within each state can be obtained via
##'   [list_countiesbystate].
##' @param pc A string specifying the AQS parameter class name. A list of the
##'   class names can be obtained via [list_classes].
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
##' aqs_list(aqs_filter = "states")
##' list_states() # equivalent to above
##'
##' aqs_list("countiesByState", aqs_variables = list(state = "01"))
##' list_countiesbystate(state = "01")
##'
##' aqs_list("sitesByCounty", aqs_variables = list(state = "37", county = "183"))
##' list_sitesbycounty(state = "37", county = "183")
##'
##' aqs_list("cbsas")
##' list_cbsas()
##'
##' aqs_list("classes")
##' list_classes()
##'
##' aqs_list("parametersByClass", list(pc = "CRITERIA")) # Criteria pollutants
##' list_parametersbyclass(pc = "CRITERIA")
##'
##' aqs_list("pqaos")
##' list_pqaos()
##'
##' aqs_list("mas")
##' list_mas()
##'
##' aqs_list("durations")
##' list_durations()
##' }
##'
##' @export
aqs_list <- function(aqs_filter = c("states", "countiesByState",
                                    "sitesByCounty", "cbsas", "classes",
                                    "parametersByClass", "pqaos", "mas",
                                    "durations"),
                     aqs_variables = NULL, header = FALSE, ...) {
  aqs_filter <- .match.arg(aqs_filter)
  aqs_variables <- c(.fill_user_to_list(as.list(aqs_variables)), header = header)
  .is_namedlist(aqs_variables)
  switch(
    aqs_filter,
    states = do.call(list_states, aqs_variables),
    countiesByState = do.call(list_countiesbystate, aqs_variables),
    sitesByCounty = do.call(list_sitesbycounty, aqs_variables),
    cbsas = do.call(list_cbsas, aqs_variables),
    classes = do.call(list_classes, aqs_variables),
    parametersByClass = do.call(list_parametersbyclass, aqs_variables),
    pqaos = do.call(list_pqaos, aqs_variables),
    mas = do.call(list_mas, aqs_variables),
    durations = do.call(list_durations, aqs_variables)
  )
}

##' @rdname aqs_list
##' @export
list_states <- function(email = get_aqs_email(), key = get_aqs_key(),
                        header = FALSE, ...) {
  .run_api(
    aqs_service = "list", aqs_filter = "states",
    aqs_variables = list(email = email, key = key),
    header = header
  )
}

##' @rdname aqs_list
##' @export
list_countiesbystate <- function(state, email = get_aqs_email(),
                                 key = get_aqs_key(),
                                 header = FALSE, ...) {
  .run_api(
    aqs_service = "list", aqs_filter = "countiesByState",
    aqs_variables = list(email = email, key = key, state = state),
    header = header
  )
}

##' @rdname aqs_list
##' @export
list_sitesbycounty <- function(state, county, email = get_aqs_email(),
                               key = get_aqs_key(), header = FALSE, ...) {
  .run_api(
    aqs_service = "list", aqs_filter = "sitesByCounty",
    aqs_variables = list(email = email, key = key,
                         state = state, county = county),
    header = header
  )
}

##' @rdname aqs_list
##' @export
list_cbsas <- function(email = get_aqs_email(), key = get_aqs_key(),
                       header = FALSE, ...) {
  .run_api(
    aqs_service = "list", aqs_filter = "cbsas",
    aqs_variables = list(email = email, key = key),
    header = header
  )
}

##' @rdname aqs_list
##' @export
list_classes <- function(email = get_aqs_email(), key = get_aqs_key(),
                         header = FALSE, ...) {
  .run_api(
    aqs_service = "list", aqs_filter = "classes",
    aqs_variables = list(email = email, key = key),
    header = header
  )
}

##' @rdname aqs_list
##' @export
list_parametersbyclass <- function(pc, email = get_aqs_email(),
                                   key = get_aqs_key(), header = FALSE, ...) {
  .run_api(
    aqs_service = "list", aqs_filter = "parametersByClass",
    aqs_variables = list(email = email, key = key, pc = pc),
    header = header
  )
}

##' @rdname aqs_list
##' @export
list_pqaos <- function(email = get_aqs_email(), key = get_aqs_key(),
                       header = FALSE, ...) {
  .run_api(
    aqs_service = "list", aqs_filter = "pqaos",
    aqs_variables = list(email = email, key = key),
    header = header
  )
}

##' @rdname aqs_list
##' @export
list_mas <- function(email = get_aqs_email(), key = get_aqs_key(),
                     header = FALSE, ...) {
  .run_api(
    aqs_service = "list", aqs_filter = "mas",
    aqs_variables = list(email = email, key = key),
    header = header
  )
}

##' @rdname aqs_list
##' @export
list_durations <- function(email = get_aqs_email(), key = get_aqs_key(),
                           header = FALSE, ...) {
  ## Inconsistency: filter = duration, not durations
  .run_api(
    aqs_service = "list", aqs_filter = "duration",
    aqs_variables = list(email = email, key = key),
    header = header
  )
}
