## Release summary

This is a major release that 
* adds the two exported functions `fast_scraper_roster()` as well as `decode_player_ids()`,
* adds a new option to the exported function `update_db()` and
* fixes some minor bugs.

## Test environments

* local R installation, R 4.0.2
* Debian Linux, R-devel, clang, ISO-8859-15 locale
* Debian Linux, R-devel, GCC
* Debian Linux, R-devel, GCC, no long double
* Debian Linux, R-patched, GCC
* Debian Linux, R-release, GCC
* Fedora Linux, R-devel, clang, gfortran
* Fedora Linux, R-devel, GCC
* CentOS 6 with Redhat Developer Toolset, R from EPEL
* macOS 10.13.6 High Sierra, R-release, brew
* macOS 10.13.6 High Sierra, R-release, CRAN's setup
* Ubuntu Linux 16.04 LTS, R-devel, GCC
* Ubuntu Linux 16.04 LTS, R-release, GCC
* Ubuntu Linux 16.04 LTS, R-devel with rchk
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* Windows Server 2008 R2 SP1, R-oldrel, 32/64 bit
* Windows Server 2008 R2 SP1, R-patched, 32/64 bit
* Windows Server 2008 R2 SP1, R-release, 32/64 bit
* Linux (Xenial) (on travis-ci), R Versions	oldrel, release, devel
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* unable to verify current time -> this is a R CMD Check problem
