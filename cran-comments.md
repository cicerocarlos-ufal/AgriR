## Resubmission

This is a resubmission. In this version I have:

* Excluded 'cran-comments.md' from the package build (.Rbuildignore), which
  caused the NOTE "Non-standard file/directory found at top level".
* Rewrote the Description field without the surnames of the authors of the
  tests (Keuls, Knott, Scheffe), which were flagged as possibly misspelled.

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
