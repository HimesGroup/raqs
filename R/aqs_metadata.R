##' AQS API Meta Data service
##'
##' A collection of functions to fetch information about the AQS API. The main
##' purpose of this service is to let you know the system is up before you run a
##' long job.
##'
##' [aqs_metadata] sends a request to the AQS API based on a user-provided
##' filter using the following underlying functions:
##' - [metadata_isavailable] checks if the API is up and running.
##' - [metadata_revisionhistory] returns a complete list of revisions to the API
##' in reverse chronological order.
##' - [metadata_fieldsbyservice] returns a list and definitions of fields in a
##' user-provided service.
##' - [metadata_issues] returns a list of any known issues with system
##' functionality or the data.
##'
##' @param aqs_filter A string specifying one of the service filters. NOT
##'   case-sensitive.
##' @param aqs_variables A named list of variables to fetch data (e.g.,
##'   `state`). Only necessary variables are passed to a specific endpoint
##'   (service/filter) to make a valid request.
##' @param header A logical specifying whether the function returns additional
##'   information from the API header. Default is `FALSE` to return data only.
##' @param service A string specifying one of the services available (e.g.,
##'   sampleData)
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
##' aqs_metadata(aqs_filter = "isAvailable")
##' metadata_isavailable() # equivalent to above
##'
##' aqs_metadata("revisionHistory")
##' metadata_revisionhistory()
##'
##' aqs_metadata("fieldsByService", aqs_variables = list(service = "annualData"))
##' metadata_fieldsbyservice(service = "annualData")
##'
##' aqs_metadata("issues")
##' metadata_issues()
##' }
##'
##' @export
aqs_metadata <- function(aqs_filter = c("isAvailable", "revisionHistory",
                                        "fieldsByService", "issues"),
                         aqs_variables = NULL, header = FALSE, ...) {
  aqs_filter <- .match.arg(aqs_filter)
  aqs_variables <- c(.fill_user_to_list(as.list(aqs_variables)), header = header)
  .is_namedlist(aqs_variables)
  switch(
    aqs_filter,
    isAvailable = do.call(metadata_isavailable, aqs_variables),
    revisionHistory = do.call(metadata_revisionhistory, aqs_variables),
    fieldsByService = do.call(metadata_fieldsbyservice, aqs_variables),
    issues = do.call(metadata_issues, aqs_variables)
  )
}

##' @rdname aqs_metadata
##' @export
metadata_isavailable <- function(...) {
  .run_api(
    aqs_service = "metaData", aqs_filter = "isAvailable",
    aqs_variables = NULL, header = TRUE
  )
}
##' @rdname aqs_metadata
##' @export
metadata_revisionhistory <- function(email = get_aqs_email(),
                                     key = get_aqs_key(), header = FALSE, ...) {
  .run_api(
    aqs_service = "metaData", aqs_filter = "revisionHistory",
    aqs_variables = list(email = email, key = key),
    header = header
  )
}

##' @rdname aqs_metadata
##' @export
metadata_fieldsbyservice <- function(service, email = get_aqs_email(),
                                     key = get_aqs_key(), header = FALSE, ...) {
  .run_api(
    aqs_service = "metaData", aqs_filter = "fieldsByService",
    aqs_variables = list(email = email, key = key, service = service),
    header = header
  )
}

##' @rdname aqs_metadata
##' @export
metadata_issues <- function(email = get_aqs_email(), key = get_aqs_key(),
                            header = FALSE, ...) {
  .run_api(
    aqs_service = "metaData", aqs_filter = "issues",
    aqs_variables = list(email = email, key = key),
    header = header
  )
}
