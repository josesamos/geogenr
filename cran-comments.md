## Test environments
* local R installation, R 4.0.3
* ubuntu 16.04 (on travis-ci), R 4.0.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

## Resubmission
This is a resubmission. In this version I have:

* Added a link to the used webservices to the description field of the DESCRIPTION file.

* Put functions which download data in \donttest{}.

* Eliminated options(warn=-1) when used.

* Also, I have removed the external data that is not strictly necessary for tests and examples to reduce the size of the package.

