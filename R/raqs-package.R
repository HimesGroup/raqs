##' @details
##'
##' The 'raqs' package provides an R interface to the US EPA AQS API that
##' publish data in JSON format. To use this package, you first need to register
##' for the AQS API and get your API key. Please check [aqs_signup] and
##' [set_aqs_user] to set up your API credentials in R.
##'
##' All main functions, for fetching data from the AQS API, were named with the
##' following scheme: `aqs_{service}`
##' - [aqs_metadata] returns information about the API.
##' - [aqs_list] returns variable values you may need to create other service
##' requests.
##' - [aqs_monitors] returns operational information about the monitors used to
##' collect data.
##' - [aqs_sampledata] returns sample data - the finest grain data reported to
##' EPA.
##' - [aqs_dailydata] returns data summarized at the daily level.
##' - [aqs_quarterlydata] returns data summarized at the calendar quarter level.
##' - [aqs_annualdata] returns data summarized at the yearly level
##' - [aqs_qaannualperformanceevaluations] returns pairs of data (known and
##' measured values) at several concentration levels for gaseous criteria
##' pollutants.
##' - [aqs_qablanks] returns concentrations from blank samples.
##' - [aqs_qacollocatedassessments] returns pairs of PM samples collected at the
##' same time and place by different samplers.
##' - [aqs_qaflowrateverifications] returns flow rate checks performed by
##' monitoring agencies.
##' - [aqs_qaflowrateaudits] returns flow rate audits data
##' - [aqs_qaonepointqcrawdata] returns measured versus actual concentration of
##' one point QC checks.
##' - [aqs_qapepaudits] returns data related to PM2.5 monitoring system audits.
##' - [aqs_transactionssample] returns sample data in the transaction format for
##' AQS.
##' - [aqs_transactionsqaannualperformanceevaluations] returns pairs of data QA
##' at several concentration levels in the transaction format for AQS.
##'
##' Each main function has a set of underlying functions that are responsible
##' for sending requests to specific endpoints (service/filter) and were name
##' with the following scheme: `{service}_{filter}`. Please refer to the manual
##' to see how the aforementioned functions work.
##'
##' @importFrom httr2 request req_perform req_url_path_append req_url_query
##'   resp_body_json req_error req_throttle
##' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
##'   pb_bar
"_PACKAGE"
