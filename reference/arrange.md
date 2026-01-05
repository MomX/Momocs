# Arrange rows by variables

Arrange shapes by variables, from the `$fac`. See examples and
[`?dplyr::arrange`](https://dplyr.tidyverse.org/reference/arrange.html).

## Usage

``` r
arrange(.data, ...)
```

## Arguments

- .data:

  a `Coo`, `Coe`, `PCA` object

- ...:

  logical conditions

## Value

a Momocs object of the same class.

## Details

dplyr verbs are maintained.

## See also

Other handling functions:
[`at_least()`](http://momx.github.io/Momocs/reference/at_least.md),
[`chop()`](http://momx.github.io/Momocs/reference/chop.md),
[`combine()`](http://momx.github.io/Momocs/reference/combine.md),
[`dissolve()`](http://momx.github.io/Momocs/reference/dissolve.md),
[`fac_dispatcher()`](http://momx.github.io/Momocs/reference/fac_dispatcher.md),
[`filter()`](http://momx.github.io/Momocs/reference/filter.md),
[`mutate()`](http://momx.github.io/Momocs/reference/mutate.md),
[`rename()`](http://momx.github.io/Momocs/reference/rename.md),
[`rescale()`](http://momx.github.io/Momocs/reference/rescale.md),
[`rm_harm()`](http://momx.github.io/Momocs/reference/rm_harm.md),
[`rm_missing()`](http://momx.github.io/Momocs/reference/rm_missing.md),
[`rm_uncomplete()`](http://momx.github.io/Momocs/reference/rm_uncomplete.md),
[`rw_fac()`](http://momx.github.io/Momocs/reference/rw_fac.md),
[`sample_frac()`](http://momx.github.io/Momocs/reference/sample_frac.md),
[`sample_n()`](http://momx.github.io/Momocs/reference/sample_n.md),
[`select()`](http://momx.github.io/Momocs/reference/select.md),
[`slice()`](http://momx.github.io/Momocs/reference/slice.md),
[`subsetize()`](http://momx.github.io/Momocs/reference/subset.md)

## Examples

``` r
olea
#> Opn (curves)
#>   - 210 curves, 99 +/- 4 coords (in $coo)
#>   - 4 classifiers (in $fac): 
#> # A tibble: 210 × 4
#>   var   domes view  ind  
#>   <fct> <fct> <fct> <fct>
#> 1 Aglan cult  VD    O10  
#> 2 Aglan cult  VL    O10  
#> 3 Aglan cult  VD    O11  
#> 4 Aglan cult  VL    O11  
#> 5 Aglan cult  VD    O12  
#> 6 Aglan cult  VL    O12  
#> # ℹ 204 more rows
#>   - also: $ldk
# we create a new column
olea %>% mutate(id=1:length(.)) %$% fac$id
#>   [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
#>  [19]  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
#>  [37]  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54
#>  [55]  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
#>  [73]  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
#>  [91]  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108
#> [109] 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126
#> [127] 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144
#> [145] 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162
#> [163] 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180
#> [181] 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198
#> [199] 199 200 201 202 203 204 205 206 207 208 209 210
# same but now, shapes are arranged in a desc order, based on id
olea %>% mutate(id=1:length(.)) %>% arrange(desc(id)) %$% fac$id
#>   [1] 210 209 208 207 206 205 204 203 202 201 200 199 198 197 196 195 194 193
#>  [19] 192 191 190 189 188 187 186 185 184 183 182 181 180 179 178 177 176 175
#>  [37] 174 173 172 171 170 169 168 167 166 165 164 163 162 161 160 159 158 157
#>  [55] 156 155 154 153 152 151 150 149 148 147 146 145 144 143 142 141 140 139
#>  [73] 138 137 136 135 134 133 132 131 130 129 128 127 126 125 124 123 122 121
#>  [91] 120 119 118 117 116 115 114 113 112 111 110 109 108 107 106 105 104 103
#> [109] 102 101 100  99  98  97  96  95  94  93  92  91  90  89  88  87  86  85
#> [127]  84  83  82  81  80  79  78  77  76  75  74  73  72  71  70  69  68  67
#> [145]  66  65  64  63  62  61  60  59  58  57  56  55  54  53  52  51  50  49
#> [163]  48  47  46  45  44  43  42  41  40  39  38  37  36  35  34  33  32  31
#> [181]  30  29  28  27  26  25  24  23  22  21  20  19  18  17  16  15  14  13
#> [199]  12  11  10   9   8   7   6   5   4   3   2   1
```
