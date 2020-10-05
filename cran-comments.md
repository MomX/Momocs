## Test environments
* local OS X 10.15.6 install, R 4.0.0 with --as-cran on my machine
* ubuntu x86_64-pc-linux-gnu (64-bit) with R 4.0.2 (on travis-ci https://travis-ci.org/MomX/Momocs)

Both succeeded.

## Before release
Followed devtools::release() and statisfied all requests, particularly:
* r-hub check (using `rhub::check_for_cran()` there: https://builder.r-hub.io/status/original/Momocs_1.3.0.tar.gz-0fd354bb55d84e5086950734f1842072 )
* win-builder (using `devtools::release()` there: https://win-builder.r-project.org/iFUhS8KN02WR/00check.log )

## R CMD check results
```
Status: OK
R CMD check succeeded
```
With --as-cran on my machine.

Only the NOTE for archiving on CRAN.

## Reverse dependencies
No reverse dependencies.

## Also

Apologies for letting archive the last version, related to `return` versus `return()`. I have corrected the two reported by r-hub. I also took the opportunity to make a couple of minor changes.

Thanks a lot, dear CRAN maintainers !



