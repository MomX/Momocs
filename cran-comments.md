## Test environments
* local OS X 10.13.2 install, R 3.6.2 with --as-cran on my machine
* ubuntu x86_64-pc-linux-gnu (64-bit) with R 3.6.2 (on travis-ci https://travis-ci.org/MomX/Momocs)

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

Apologies for leeting archive the last version.

I was not able to make required changes at that time since I was busy with this package successor.

Yet I consider it 'retired', I would be happy if it finds its way to CRAN for the sake of reproducibility.

Thanks a lot, dear CRAN maintainers !



