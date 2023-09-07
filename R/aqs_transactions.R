aqs_transactions <- function(category = c("Sample",
                                          "QaAnnualPerformanceEvaluations"),
                             aqs_filter = c("bySite", "byCounty", "byState",
                                            "byPQAO", "byMA"),
                             aqs_variables = NULL, header = FALSE, ...) {
  category <- .match.arg(category)
  switch(
    category,
    Sample = aqs_transactionssample(
      aqs_filter = aqs_filter, aqs_variables = aqs_variables, header = header
    ),
    QaAnnualPerformanceEvaluations =
      aqs_transactionsqaannualperformanceevaluations(
        aqs_filter = aqs_filter, aqs_variables = aqs_variables, header = header
      )
  )
}
