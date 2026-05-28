## Submission

This is the initial submission of csemGT. The package estimates the
per-person conditional standard error of measurement under the
persons-by-items single-facet crossed design of Generalizability
Theory, following Brennan (1998) <doi:10.1177/014662169802200401>.

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.
* "Possibly misspelled words in DESCRIPTION: CSEM, CSEMs". CSEM is the
  standard acronym for "conditional standard error of measurement", the
  central psychometric quantity estimated by the package, and CSEMs is
  its plural. The acronym is expanded in full the first time it appears
  in the Description.

## Test environments

* local: Windows 11 x64, R 4.6.0
  (R CMD check --as-cran, _R_CHECK_CRAN_INCOMING_REMOTE_ = TRUE)
* win-builder: R-devel and R-release
* R-hub v2: linux, windows
* GitHub Actions: macOS-latest, Windows-latest, Ubuntu-latest
  (R-devel, R-release, R-oldrel-1)

## Reverse dependencies

This is a new release, so there are no reverse dependencies.
