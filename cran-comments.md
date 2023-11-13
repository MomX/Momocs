## Test environments

* local OS X 10.15.6 install, R 4.1.3 with --as-cran on my machine
* but using Github actions and R-hub on all latest OS/R versions (there https://github.com/MomX/Momocs/actions)

## Following Benjamin Altmann email (10 nov. 2023 20:43 )
* I kept only the very necessary \dontrun{} (I really these as examples yet they write files on users' machines).
* I reviewed all files by hand and now they all should have a @return/value{} field

## Before release
* Followed devtools::release() and satisfied all requests.
* Still has a MikTex related note, apparently it's a MikTex related bug.

## Reverse dependencies
No breaking change for 1 dependent package.

## Also
Thanks for your patience, dear CRAN maintainers !



