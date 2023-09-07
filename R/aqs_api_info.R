show_service_info <- function(service = "all") {
  aqs_api_spec <- .get_aqs_api_spec()
  service_info <- .get_service_info(aqs_api_spec)
  if (service == "all") {
    service_info
  } else {
    endpoint_info <- .get_endpoint_info(aqs_api_spec)
    service <- .match.arg(service, choices = c("all", endpoint_info$service))
    cat("Service: ", service, "\n")
    service_match <- service_info$service == service
    cat("Description: ", service_info$description[service_match], "\n")
    filter_match <- endpoint_info$service == service
    cat("Filters: ", paste(endpoint_info$filter[filter_match], collapse = ", "),
        "\n\n")
    endpoint_info <- endpoint_info[endpoint_info$service == service, ]
    subset(endpoint_info, select = -c(service, filter))
  }
}

.get_aqs_api_spec <- function(use_internal = TRUE) {
  if (use_internal) {
    .aqs_api_spec
  } else {
    fromJSON("https://aqs.epa.gov/aqsweb/documents/aqs_api_specification.json")
  }
}

.get_service_info <- function(aqs_api_spec) {
  service_info <- aqs_api_spec$tags
  aqs_service <- sub("^\\/(.*)\\/(.*)", "\\1", names(aqs_api_spec$paths))
  aqs_service <- sub("^\\/", "", aqs_service)
  aqs_tags <- sapply(aqs_api_spec$paths, function(x) x$get$tags)
  names(aqs_service) <- aqs_tags
  tag_recode <- aqs_service[!duplicated(aqs_service)] # unique(aqs_service) would drop name attr
  service_info$name <- tag_recode[service_info$name]
  names(service_info)[names(service_info) == "name"] <- "service"
  service_info
}

.get_endpoint_info <- function(aqs_api_spec) {
  aqs_paths <- names(aqs_api_spec$paths)
  aqs_services <- sub("^\\/(.*)\\/(.*)", "\\1", aqs_paths)
  aqs_services <- sub("^\\/", "", aqs_services)
  aqs_filters <- sub("^\\/(.*)\\/(.*)", "\\2", aqs_paths)
  aqs_filters <- sub("^\\/(.*)", "", aqs_filters)
  variable_recode <- sapply(aqs_api_spec$parameters, function(x) x$name)
  deprecated_variables <- c("units") # appeared to be deprecated; not works
  required_variables <- setdiff(
    unlist(lapply(aqs_api_spec$parameters, function(x) if (x$required) x$name)),
    deprecated_variables
  )
  optional_variables <- setdiff(
    ## duration is an optional variable but not in the spec sheet
    c(variable_recode, "duration"), c(required_variables, deprecated_variables)
  )
  aqs_variables <- lapply(
    aqs_api_spec$paths,
    function(x) {
      params <- sub("^#/parameters/", "", x$get$parameters$`$ref`)
      params <- variable_recode[params]
      if (x$get$tags == "sampleData") params <- c(params, "duration")
      req_params <- intersect(required_variables, params)
      if (anyNA(params)) req_params <- c(req_params, "pc (Parameter class)") # Parameter class
      opt_params <- intersect(optional_variables, params)
      list(required = paste0(req_params, collapse = ", "),
           optional = paste0(opt_params, collapse = ", "))
    }
  )
  aqs_summary <- sapply(aqs_api_spec$paths, function(x) x$get$summary)
  data.frame(service = aqs_services, filter = aqs_filters,
             endpoint = sub("/$", "", paste0(aqs_services, "/", aqs_filters)),
             summary = aqs_summary,
             required_variable = sapply(aqs_variables, function(x) x$required),
             optional_variable = sapply(aqs_variables, function(x) x$optional),
             row.names = NULL)
}

## list_variables <- function() {
##   aqs_api_spec <- .get_aqs_api_spec()
##   .get_variable_info(aqs_api_spec)
## }

## .get_variable_info <- function(aqs_api_spec) {
##   variable_info <- aqs_api_spec$parameters
##   variable_list <- lapply(
##     variable_info,
##     function(x) data.frame(variable = x$name, description = x$description)
##   )
##   names(variable_list) <- NULL
##   do.call(rbind, variable_list)
## }
