## Test environments

* local OS X 10.15.6 install, R 4.1.3 with --as-cran on my machine (I know it's old but I have a dependency elsewhere and no workaround)
* but using Github actions and (new) rhub on all latest OS/R versions (there https://github.com/MomX/Momocs/actions) 
* also using devtools::check_win_devel() https://win-builder.r-project.org/g67oXFl3c0Xg

## Before release
* Followed devtools::release() and satisfied all requests.
* Still has a XQuartz related note for macos-latest, apparently it's a rgl related bug.

## Reverse dependencies
No breaking change for 1 dependent package.

## Also
Thanks for your patience, dear CRAN maintainers !



