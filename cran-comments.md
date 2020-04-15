## Test environments
* local OS X 10.13.2 install, R 3.6.2
* ubuntu x86_64-pc-linux-gnu (64-bit) with R 3.6.2 (on travis-ci https://travis-ci.org/MomX/Momocs)

## Before release
Followed devtools::release():
* Including rhub::check_for_cran()
* win-builder (devtools::release) 

## R CMD check results
```
Status: OK
R CMD check succeeded
```
With --as-cran on my machine.


## Reverse dependencies
No reverse dependencies.

