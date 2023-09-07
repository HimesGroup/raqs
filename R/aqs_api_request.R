.check_variables <- function(x, split_multiyear) {
  ## Remove NULL variables (unspecified optional variables)
  x <- Filter(Negate(is.null), x)
  ## Prevent escaping of query parameter with I() for user credentials
  if ("email" %in% names(x)) x$email <- I(x$email)
  if ("key" %in% names(x)) x$key <- I(x$key)
  ## Format multiple parameter codes with comma
  ## Up to 5 parameter codes may be requested
  if (length(x$param) > 5) {
    stop("> 5 parameter codes (param) are not allowed.", call. = FALSE)
  }
  if (length(x$param) > 1) {
    x$param <- I(paste(x$param, collapse = ","))
  }
  ## Type casting with leading zero(s)
  ## If variable is not present, is.numeric(NULL) -> FALSE
  if (is.numeric(x$state)) x$state <- .int_to_str_pad(x$state, width = 2)
  if (is.numeric(x$county)) x$county <- .int_to_str_pad(x$county, width = 3)
  if (is.numeric(x$site)) x$site <- .int_to_str_pad(x$site, width = 4)
  ## Check validity of dates
  if (all(c("bdate", "edate") %in% names(x))) {
    .verify_dates(x$bdate, x$edate)
    ## x[c("bdate", "edate")] <- .process_multiyear(x$bdate, x$edate)
    if (split_multiyear) {
      x <- replace(x, c("bdate", "edate"), .split_multiyear(x$bdate, x$edate))
    }
  }
  if (all(c("cbdate", "cedate") %in% names(x))) {
    .verify_dates(x$cbdate, x$cedate)
  }
  x
}

.make_api_request <- function(aqs_base_url = "https://aqs.epa.gov/data/api",
                              aqs_service = "list",
                              aqs_filter = NULL, aqs_variables = NULL) {
  aqs_url <- request(aqs_base_url) |>
    req_url_path_append(aqs_service)
  if (!is.null(aqs_filter)) {
    aqs_url <- req_url_path_append(aqs_url, aqs_filter)
  }
  if (!is.null(aqs_variables)) {
    aqs_url <- req_url_query(aqs_url, !!!aqs_variables)
  }
  aqs_url
}

.get_api_endpoint <- function(aqs_service = "list",
                              aqs_filter = NULL,
                              aqs_variables = NULL) {
  aqs_url <- request("https://aqs.epa.gov/data/api") |>
    req_url_path_append(aqs_service)
  if (!is.null(aqs_filter)) {
    aqs_url <- req_url_path_append(aqs_url, aqs_filter)
  }
  if (!is.null(aqs_variables)) {
    aqs_url <- req_url_query(aqs_url, !!!aqs_variables)
  }
  aqs_url
}

.mget_api_endpoint <- function(aqs_service = "list",
                               aqs_filter = NULL,
                               aqs_variables = NULL) {

  ## Create variable lists for each (bdate, edate) pair
  aqs_variables <- Map(
    function(x, y) replace(aqs_variables, c("bdate", "edate"), c(x, y)),
    aqs_variables$bdate, aqs_variables$edate, USE.NAMES = FALSE
  )
  ## ## Get df; Each row represents list of variables for a single request
  ## ## It will recycle single-value variables to fill a data.frame up to the
  ## ## length of bdate & edate
  ## aqs_variables <- as.data.frame(do.call(cbind, aqs_variables))
  ## ## Split each row into a list
  ## aqs_variables <- split(aqs_variables, 1:nrow(aqs_variables))
  ## ## Prevent escaping of query parameter again
  ## aqs_variables <- lapply(aqs_variables, function(x) {
  ##   if ("email" %in% names(x)) x$email <- I(x$email)
  ##   if ("key" %in% names(x)) x$key <- I(x$key)
  ##   if ("param" %in% names(x)) x$param <- I(x$param)
  ##   x
  ## })
  ## Get list of AQS endpoints
  aqs_url <- lapply(aqs_variables, function(x) {
    .get_api_endpoint(aqs_service = aqs_service, aqs_filter = aqs_filter,
                      aqs_variables = x)
  })
  aqs_url
}

.get_api_data <- function(aqs_url, header = FALSE,
                          check_type = TRUE, simplifyVector = TRUE, ...) {
  ## browser()
  ## error_body <- function(resp) {
  ##   out <- resp_body_json(resp, simplifyVector = TRUE)
  ##   tryCatch(out$Header$error[[1]],
  ##            error = function(e) out)
  ## }
  aqs_resp <- aqs_url |>
    req_error(body = function(resp) {
      out <- resp_body_json(resp, simplifyVector = TRUE)
      tryCatch(out$Header$error[[1]], error = function(error) out)
      ## resp_body_json(resp, simplifyVector = TRUE)$Header$error[[1]]
    }) |>
    req_perform()
  aqs_data <- resp_body_json(aqs_resp, check_type = check_type,
                             simplifyVector =  simplifyVector, ...)
  ## For Service without data return (e.g., isAvailable)
  if (is.null(aqs_data$Header$rows)) {
    return(aqs_data)
  }
  ## No matched Data
  if (aqs_data$Header$rows == 0) {
    warning(aqs_data$Header$status, call. = FALSE, immediate. = TRUE)
    aqs_data$Data <- NULL
  }
  if (!header) {
    ## if (aqs_data$Header$rows == 0) {
    ##   ## stop(aqs_data$Header$status, call. = FALSE)
    ##   warning(aqs_data$Header$status, call. = FALSE, immediate. = TRUE)
    ##   return(NULL)
    ## }
    aqs_data <- aqs_data$Data
  }
  aqs_data
}

.mget_api_data <- function(aqs_url, bdate, edate, header = FALSE,
                           check_type = TRUE, simplifyVector = TRUE, ...) {
  date_vec <- paste0(.to_ymd(bdate), "-", .to_ymd(edate))
  message("'bdate'-'edate' split in multiple chunks: ",
          paste(date_vec, collapse = ", "), ".\n",
          "Sleep 5 seconds between each request.")
  n_req <- length(aqs_url)
  aqs_data <- Map(function(x, y, z) {
    message("- Requesting: ", y)
    resp <- .get_api_data(
      aqs_url = x, header = header, check_type = check_type,
      simplifyVector = simplifyVector
    )
    if (z != length(aqs_url)) Sys.sleep(5)
    resp
  }, aqs_url, date_vec, seq_along(aqs_url), USE.NAMES = FALSE)
  if (!header) aqs_data <- do.call(rbind, aqs_data)
  aqs_data
}

.run_api <- function(aqs_service, aqs_filter, aqs_variables, header = FALSE,
                     split_multiyear = if (aqs_service != "monitors") TRUE else FALSE,
                     check_type = TRUE, simplifyVector = TRUE, ...) {
  aqs_variables <- .check_variables(aqs_variables, split_multiyear)
  if (length(aqs_variables$bdate) > 1) {
    aqs_url <- .mget_api_endpoint(
      aqs_service = aqs_service, aqs_filter = aqs_filter,
      aqs_variables = aqs_variables
    )
    aqs_data <- .mget_api_data(
      aqs_url, bdate = aqs_variables$bdate, edate = aqs_variables$edate,
      header = header, check_type = check_type,
      simplifyVector = simplifyVector, ...
    )
  } else {
    aqs_url <- .get_api_endpoint(
      aqs_service = aqs_service, aqs_filter = aqs_filter,
      aqs_variables = aqs_variables
    )
    aqs_data <- .get_api_data(
      aqs_url, header = header, check_type = check_type,
      simplifyVector = simplifyVector, ...
    )
  }
  aqs_data
}

.aqs_service_list <- c(
  "metaData", "list", "monitors",
  "sampleData", "dailyData", "quarterlyData", "annualData",
  "qaAnnualPerformanceEvaluations", "qaBlanks", "qaCollocatedAssessments",
  "qaFlowRateVerifications", "qaFlowRateAudits", "qaOnePointQcRawData",
  "qaPepAudits", "transactionsSample",
  "transactionsQaAnnualPerformanceEvaluations"
)

.aqs_filter_list <- c(
  "isAvailable", "revisionHistory", "fieldsByService", "issues",
  "states", "countiesByState", "sitesByCounty", "cbsas", "classes",
  "parametersByClass", "pqaos", "mas",
  "bySite", "byCounty", "byState", "byBox", "byCBSA", "byPQAO", "byMA"
)

.aqs_variable_list <- c(
  "email", "key", "bdate", "edate", "param", "state", "county",
  "site", "duration", "cbsa", "minlat", "maxlat", "minlon", "maxlon",
  "cbdate", "cedate", "pqao", "ma", 'pc'
)
