## Test environments

* local Windows 11 x64, R 4.4.1

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:

> checking for future file timestamps ... NOTE
  unable to verify current time

This appears to be a local environment time-verification issue.

## Resubmission

This is a resubmission. The previous submission produced a Debian pre-test NOTE
because the `estimate_EC50()` help-page examples exceeded 5 seconds. The
examples were reduced to a smaller representative subset while preserving the
same user workflow. Local `R CMD check --as-cran --no-manual` now reports
`checking examples ... OK`.
