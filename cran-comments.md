## Reviewer's comments

* Please omit the redundant "An R" in your title.
  - We deleted "An R" in the title.

* Please add \value to .Rd files regarding exported methods and explain the
functions results in the documentation (Missing Rd-tags: aqs_signup.Rd: \value).
  - We added \value to aqs_signup.Rd based on the reviewer's suggestions.

* \dontrun{} should only be used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...).

  - All exported functions except for 'set_aqs_user' requires user's API key to
    work. Otherwise, they throw errors. For clarity, we added a comment "Set
    your API Key first using set_aqs_user to run the following codes" at the top
    of the examples with \dontrun{}.

## Test environments

* Local
  - Ubuntu 22.04 LTS: release
* GitHub Actions
  - Ubuntu-latest: release, devel
  - Windows-latest: release, devel
  - Mac-latest: release

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

* Possibly misspelled words in DESCRIPTION:
  - AQS: Air Quality System (AQS) from U.S. Environmental Protection Agency
  - JSON: JSON format
