# bind_db.Coe \<- bind_db.Coo Extracts structure from filenames

If filenames are consistently named with the same character serating
factors, and with every individual including its belonging levels, e.g.:

- `001_speciesI_siteA_ind1_dorsalview`

- `002_speciesI_siteA_ind2_lateralview`

etc., this function returns a
[data.frame](https://rdrr.io/r/base/data.frame.html) from it that can be
passed to [Out](http://momx.github.io/Momocs/reference/Out.md),
[Opn](http://momx.github.io/Momocs/reference/Opn.md),
[Ldk](http://momx.github.io/Momocs/reference/Ldk.md) objects.

## Usage

``` r
lf_structure(lf, names = character(), split = "_", trim.extension = FALSE)
```

## Arguments

- lf:

  a list (its names are used, except if it is a list from
  [import_tps](http://momx.github.io/Momocs/reference/import_tps.md) in
  this case `names(lf$coo)` is used) of a list of filenames, as
  characters, typically such as those obtained with
  [list.files](https://rdrr.io/r/base/list.files.html). Alternatively, a
  path to a folder containing the files. Actually, if lf is of length 1
  (a single character), the function assumes it is a path and do a
  [list.files](https://rdrr.io/r/base/list.files.html) on it.

- names:

  the names of the groups, as a vector of characters which length
  corresponds to the number of groups.

- split:

  character, the spliting factor used for the file names.

- trim.extension:

  logical. Whether to remove the last for characters in filenames,
  typically their extension, e.g. '.jpg'.

## Value

data.frame with, for every individual, the corresponding level for every
group.

## Details

The number of groups must be consistent across filenames.

## Note

This is, to my view, a good practice to 'store' the grouping structure
in filenames, but it is of course not mandatory.

Note also that you can: i) do a
[import_jpg](http://momx.github.io/Momocs/reference/import_jpg.md) and
save is a list, say 'foo'; then ii) pass 'names(foo)' to lf_structure.
See Momocs' vignette for an illustration.

Note this function will be deprecated from Momocs when `Momacs` and
`Momit` will be fully operationnal.

## See also

[import_jpg1](http://momx.github.io/Momocs/reference/import_jpg1.md),
[import_Conte](http://momx.github.io/Momocs/reference/import_Conte.md),
[import_txt](http://momx.github.io/Momocs/reference/import_txt.md),
lf_structure. See also Momocs' vignettes for data import.

Other babel functions:
[`tie_jpg_txt()`](http://momx.github.io/Momocs/reference/tie_jpg_txt.md)
