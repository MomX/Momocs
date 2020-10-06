## Test environments
All OK (besides the 'archived package/new submission' note)

* local OS X 10.15.6 install, R 4.0.0 with --as-cran on my machine
* ubuntu x86_64-pc-linux-gnu (64-bit) with R 4.0.2 (on travis-ci https://travis-ci.org/MomX/Momocs)
* Github action on macOS-latest (there https://github.com/MomX/Momocs/actions)
* `devtools::check_rhub()` is OK when passed with `env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always")`

## Before release
Followed devtools::release() and statisfied all requests, particularly:
* r-hub check (using `rhub::check_for_cran()` there: https://builder.r-hub.io/status/Momocs_1.3.2.tar.gz-0a9ee40cb3c4453980b02d051020ed73 )
* win-builder (using `devtools::release()` there: https://win-builder.r-project.org/1lg7KzDr9uQv/00check.log )

## Reverse dependencies
No reverse dependencies.

## Also
Apologies for letting archive the last version, related to `return` versus `return()`. 
Thanks a lot, dear CRAN maintainers !



