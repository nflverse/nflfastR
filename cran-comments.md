## Release summary

This is a major release that 
* deprecates function arguments
* adds a new exported function
* cleans the source
* removes models from sysdata
* add new exported data
* fixes bugs

The latest internal test on CRAN were failing due to changes in the infrastructure
the package is depending on. We made sure this won't happen in the future.

## Test environments

* local R installation, R 4.0.3
* Debian Linux, R-devel, clang, ISO-8859-15 locale                       
* Debian Linux, R-devel, GCC                                             
* Debian Linux, R-devel, GCC, no long double                             
* Debian Linux, R-patched, GCC                                           
* Debian Linux, R-release, GCC                                           
* Fedora Linux, R-devel, clang, gfortran                                 
* Fedora Linux, R-devel, GCC                                             
* CentOS 8, stock R from EPEL                                            
* Debian Linux, R-devel, GCC ASAN/UBSAN                                  
* macOS 10.13.6 High Sierra, R-release, brew                             
* macOS 10.13.6 High Sierra, R-release, CRAN's setup                     
* Oracle Solaris 10, x86, 32 bit, R-release                              
* Oracle Solaris 10, x86, 32 bit, R-release, Oracle Developer Studio 12.6
* Ubuntu Linux 20.04.1 LTS, R-devel, GCC                                 
* Ubuntu Linux 20.04.1 LTS, R-release, GCC                               
* Ubuntu Linux 20.04.1 LTS, R-devel with rchk                            
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit                         
* Windows Server 2008 R2 SP1, R-oldrel, 32/64 bit                        
* Windows Server 2008 R2 SP1, R-patched, 32/64 bit                       
* Windows Server 2008 R2 SP1, R-release, 32/64 bit 
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across 
CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
