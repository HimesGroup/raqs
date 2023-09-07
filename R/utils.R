"%ni%" <- function(x, table) match(x, table, nomatch = 0L) == 0L

.is_nonempty_str <- function(x) {
  if (length(x) != 1) {
    return(FALSE)
  }
  if (is.null(x)) {
    return(FALSE)
  }
  if (is.na(x)) {
    return(FALSE)
  }
  if (!is.character(x)) {
    return(FALSE)
  }
  if (!nchar(x)) {
    return(FALSE)
  }
  TRUE
}

.is_namedlist <- function(x) {
  if (!(is.list(x) && length(x) == sum(names(x) !=  "", na.rm = TRUE))) {
    stop("'aqs_variables' should be a named list.", call. = FALSE)
  }
}

.int_to_str_pad <- function(x, width = 2) {
  formatC(x, width = width, format = "d", flag = "0")
}

.verify_dates <- function(begin, end) {
  begin_date <- as.Date(begin, format = "%Y%m%d")
  end_date <- as.Date(end, format = "%Y%m%d")
  if (nchar(begin) != 8 || nchar(end) != 8 | anyNA(c(begin_date, end_date))) {
    stop("Invalid Date Format. ",
         "Please use 'YYYYMMDD' format.")
  }
  if (begin_date > end_date) {
    stop("The begin date must be earlier or the same as the end date.")
  }
}

.split_multiyear <- function(begin, end) {
  begin_date <- as.Date(begin, format = "%Y%m%d")
  end_date <- as.Date(end, format = "%Y%m%d")
  begin_year <- as.integer(format(begin_date, "%Y"))
  end_year <- as.integer(format(end_date, "%Y"))
  if (begin_year < end_year) {
    ## Generate sequence for a multi-year span
    begin_date <- c(
      begin_date,
      seq(as.Date(paste0(begin_year + 1, "-01-01")), end_date, by = "year")
    )
    ## Need unique() to prevent dups when end_date is YYYY-12-31
    end_date <- c(unique(
      c(seq(as.Date(paste0(begin_year, "-12-31")), end_date, by = "year"),
        end_date)
    ))
    ## Re-format to YYYYMMDD
    begin <- format(format(begin_date, "%Y%m%d"))
    end <- format(format(end_date, "%Y%m%d"))
  }
  list(bdate = begin, edate = end)
}

.to_ymd <- function(yyyymmdd) {
  sub("(\\d{4})(\\d{2})(\\d{2})", "\\1/\\2/\\3", yyyymmdd)
}

.fill_user_to_list <- function(x) {
  if ("email" %ni% names(x)) x$email <- get_aqs_email()
  if ("key" %ni% names(x)) x$key <- get_aqs_key()
  x
}

.match.arg <- function(arg, choices, several.ok = FALSE, ignore.case = TRUE) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sysP <- sys.parent()))
    choices <- eval(formal.args[[as.character(substitute(arg))]],
                    envir = sys.frame(sysP))
  }
  if (is.null(arg))
    return(choices[1L])
  else if (!is.character(arg))
    stop("'arg' must be NULL or a character vector")
  if (!several.ok) {
    if (identical(arg, choices))
      return(arg[1L])
    if (length(arg) > 1L)
      stop("'arg' must be of length 1")
  }
  else if (length(arg) == 0L)
    stop("'arg' must be of length >= 1")
  if (ignore.case) {
    arg <- tolower(arg)
    choices_orig <- choices
    choices <- tolower(choices)
  }
  i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
  if (all(i == 0L)) {
    if (ignore.case) {
      stop(sprintf(
        ngettext(length(chs <- unique(choices_orig[nzchar(choices_orig)])),
                 "'arg' should be %s (case-insensitive)",
                 "'arg' should be one of %s (case-insensitive)"),
        paste(dQuote(chs), collapse = ", ")), domain = NA)
    } else {
      stop(sprintf(
        ngettext(length(chs <- unique(choices[nzchar(choices)])),
                 "'arg' should be %s", "'arg' should be one of %s"),
        paste(dQuote(chs), collapse = ", ")), domain = NA)
    }
  }
  i <- i[i > 0L]
  if (!several.ok && length(i) > 1)
    stop("there is more than one match in '.match.arg'")
  if (ignore.case) {
    choices <- choices_orig
  }
  choices[i]
}

