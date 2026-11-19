## Submission

This is the first submission of AgriR.

## Test environments

* local: R x.y.z (sistema operacional)
* win-builder: R-devel
* GitHub Actions: ubuntu-latest, windows-latest, macos-latest (R release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Notes for the reviewer

* The analysis functions (e.g. anova_crd(), tukey()) are intended to print
  ANOVA tables and grouping results to the console, as is usual for
  teaching software; the results are also returned invisibly.
* Files are written only when the user explicitly asks for it
  (argument save = TRUE in the des.*() functions); nothing is written by
  default, in examples or in tests.
