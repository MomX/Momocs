## Test environments

* local OS X 10.15.6 install, R 4.0.0 with --as-cran on my machine
* but using Github actions and R-hub on all latest OS/R versions (there https://github.com/MomX/Momocs/actions)

## Before release
Followed devtools::release() and satisfied all requests, particularly:
* r-hub check (using `rhub::check_for_cran()` there: https://builder.r-hub.io/status/Momocs_1.4.0.tar.gz-1bf79d82237140749302b7fee4023cff ; https://builder.r-hub.io/status/Momocs_1.4.0.tar.gz-52339f7f444f4b589e23cebb1ca9baf0 ; https://builder.r-hub.io/status/Momocs_1.4.0.tar.gz-0e6b9162374f405891d4a3d6efd31379 )
* win-builder (using `devtools::release()` there: https://win-builder.r-project.org/AvBb901x8cwO/00install.out )

## Reverse dependencies
No breaking change for 1 dependent package.

## Also
Thanks a lot, dear CRAN maintainers !



