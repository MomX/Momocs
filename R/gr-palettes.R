# Everything palettes --------------------------------------

#' Color palettes
#'
#' All colorblind friendly RColorBrewer palettes
#' recreated without the number of colors limitation
#' and with transparency support thanks to `pal_alpha` that can be used alone.
#' Also, all viridis palettes (see the [package on CRAN](https://CRAN.R-project.org/package=viridis)),
#' yet color ramps are borrowed and Momocs does not depend on it.
#' Also, `pal_qual_solarized` based on Solarized: \url{http://ethanschoonover.com/solarized}
#' and `pal_seq_grey` only shades of grey from `grey10` to `grey90`.
#'
#' @param n `numeric` number of colors
#' @param cols color(s) as hexadecimal values
#' @param transp `numeric` between 0 and 1 (0, eg opaque, by default)
#' @note RColorBrewer palettes are not happy when `n` is lower than 3 and above
#' a given number for each palette. If this is the case, these functions will
#' create a color palette with [colorRampPalette] and return colors even so.
#'
#' @aliases pal palette
#' @examples
#' pal_div_BrBG(5) %>% barplot(rep(1, 5), col=.)
#' pal_div_BrBG(5, 0.5) %>% barplot(rep(1, 5), col=.)
#' @name palettes
#' @rdname palettes
#' @export
pal_alpha <- function(cols, transp = 0) {
  alpha.int <- as.integer((1 - transp) * 255)
  alpha.hex <- as.character(as.hexmode(alpha.int))
  alpha.hex[nchar(alpha.hex) < 2] <- paste0("0", alpha.hex[nchar(alpha.hex) < 2])
  alpha.hex <- toupper(alpha.hex) # pure cosmetics
  # handle NA
  nas <- is.na(cols)
  cols <- paste0(.rcolors2hex(cols), alpha.hex)
  cols[nas] <- NA
  return(cols)
}

#' @name palettes
#' @rdname palettes
#' @export
pal_manual <- function(cols, transp=0){
  ramp <- colorRamp(cols)
  function(n) {
    x <- ramp(seq.int(0, 1, length.out = n))
    if (ncol(x) == 4L)
      rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255) %>% pal_alpha(transp)
    else rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255) %>% pal_alpha(transp)
  }
}

# RColorBrewer --------
# A function to bypass RColorBrewer limitations in terms of number of colors
# eg it is not happy with less than 3,
# and more than maxcolors as defined in RColorBrewer::brewer.pal.info
# here for less than 3 or more than the corresponding value for each palette,
# we simply create a palette on the fly and return the required colors
# It is used to recreate all RColorBrewer palettes below
.pal_brewer <- function(n, name){
  # RColorBrewer always want at least 3 levels
  if (n<3){
    return(colorRampPalette(RColorBrewer::brewer.pal(3, name))(n))
  }
  # But no more than a given value given in brewer.pal.info
  mc <- RColorBrewer::brewer.pal.info[name, "maxcolors"]
  if (n>mc){
    return(colorRampPalette(RColorBrewer::brewer.pal(mc, name))(n))
  } else {
    # If between 3 and max allowed, simply brewer.pal
    return(RColorBrewer::brewer.pal(n, name))
  }
}

#' @rdname palettes
#' @export
pal_qual_solarized <- function(n, transp=0){
  colorRampPalette(c("#dc322f",
"#d33682",
"#6c71c4",
"#268bd2",
"#2aa198",
"#859900"))(n) %>%
  pal_alpha(transp)
}

#' @rdname palettes
#' @export
pal_seq_grey <- function(n, transp=0){
  colorRampPalette(c("#E5E5E5",
                     "#1A1A1A"))(n) %>%
    pal_alpha(transp)
}

# # below, generated with
# df <- RColorBrewer::brewer.pal.info %>%
#   mutate(name=rownames(.)) %>%
#   filter(colorblind==TRUE) %>%
#   select(category, name)
# paste0("#' @rdname palettes\n",
#        "#' @export\n",
#        "pal", "_", df$category, "_", df$name,
#        " <- function(n, transp=0){
#           .pal_brewer(n, '", df$name, "') %>%
#            pal_alpha(transp=transp)
# }", "\n") %>%
#   cat(sep="\n")

#' @rdname palettes
#' @export
pal_div_BrBG <- function(n, transp=0){
  .pal_brewer(n, 'BrBG') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_div_PiYG <- function(n, transp=0){
  .pal_brewer(n, 'PiYG') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_div_PRGn <- function(n, transp=0){
  .pal_brewer(n, 'PRGn') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_div_PuOr <- function(n, transp=0){
  .pal_brewer(n, 'PuOr') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_div_RdBu <- function(n, transp=0){
  .pal_brewer(n, 'RdBu') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_div_RdYlBu <- function(n, transp=0){
  .pal_brewer(n, 'RdYlBu') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_qual_Dark2 <- function(n, transp=0){
  .pal_brewer(n, 'Dark2') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_qual_Paired <- function(n, transp=0){
  .pal_brewer(n, 'Paired') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_qual_Set2 <- function(n, transp=0){
  .pal_brewer(n, 'Set2') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_Blues <- function(n, transp=0){
  .pal_brewer(n, 'Blues') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_BuGn <- function(n, transp=0){
  .pal_brewer(n, 'BuGn') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_BuPu <- function(n, transp=0){
  .pal_brewer(n, 'BuPu') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_GnBu <- function(n, transp=0){
  .pal_brewer(n, 'GnBu') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_Greens <- function(n, transp=0){
  .pal_brewer(n, 'Greens') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_Greys <- function(n, transp=0){
  .pal_brewer(n, 'Greys') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_Oranges <- function(n, transp=0){
  .pal_brewer(n, 'Oranges') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_OrRd <- function(n, transp=0){
  .pal_brewer(n, 'OrRd') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_PuBu <- function(n, transp=0){
  .pal_brewer(n, 'PuBu') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_PuBuGn <- function(n, transp=0){
  .pal_brewer(n, 'PuBuGn') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_PuRd <- function(n, transp=0){
  .pal_brewer(n, 'PuRd') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_Purples <- function(n, transp=0){
  .pal_brewer(n, 'Purples') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_RdPu <- function(n, transp=0){
  .pal_brewer(n, 'RdPu') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_Reds <- function(n, transp=0){
  .pal_brewer(n, 'Reds') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_YlGn <- function(n, transp=0){
  .pal_brewer(n, 'YlGn') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_YlGnBu <- function(n, transp=0){
  .pal_brewer(n, 'YlGnBu') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_YlOrBr <- function(n, transp=0){
  .pal_brewer(n, 'YlOrBr') %>%
    pal_alpha(transp=transp)
}

#' @rdname palettes
#' @export
pal_seq_YlOrRd <- function(n, transp=0){
  .pal_brewer(n, 'YlOrRd') %>%
    pal_alpha(transp=transp)
}

# viridis ------
# ramps created with:

# vir <- viridisLite::viridis.map
# vir[vir$opt=="A", 1:3] %>% rgb %>%
#   paste0("'", ., "', ") %>% cat(fill=T)
#A=magma, B=inferno, C=plasma, D=viridis

.magma <- c(
  '#000004',  '#010005',  '#010106',  '#010108',  '#020109',
  '#02020B',  '#02020D',  '#03030F',  '#030312',  '#040414',
  '#050416',  '#060518',  '#06051A',  '#07061C',  '#08071E',
  '#090720',  '#0A0822',  '#0B0924',  '#0C0926',  '#0D0A29',
  '#0E0B2B',  '#100B2D',  '#110C2F',  '#120D31',  '#130D34',
  '#140E36',  '#150E38',  '#160F3B',  '#180F3D',  '#19103F',
  '#1A1042',  '#1C1044',  '#1D1147',  '#1E1149',  '#20114B',
  '#21114E',  '#221150',  '#241253',  '#251255',  '#271258',
  '#29115A',  '#2A115C',  '#2C115F',  '#2D1161',  '#2F1163',
  '#311165',  '#331067',  '#341069',  '#36106B',  '#38106C',
  '#390F6E',  '#3B0F70',  '#3D0F71',  '#3F0F72',  '#400F74',
  '#420F75',  '#440F76',  '#451077',  '#471078',  '#491078',
  '#4A1079',  '#4C117A',  '#4E117B',  '#4F127B',  '#51127C',
  '#52137C',  '#54137D',  '#56147D',  '#57157E',  '#59157E',
  '#5A167E',  '#5C167F',  '#5D177F',  '#5F187F',  '#601880',
  '#621980',  '#641A80',  '#651A80',  '#671B80',  '#681C81',
  '#6A1C81',  '#6B1D81',  '#6D1D81',  '#6E1E81',  '#701F81',
  '#721F81',  '#732081',  '#752181',  '#762181',  '#782281',
  '#792282',  '#7B2382',  '#7C2382',  '#7E2482',  '#802582',
  '#812581',  '#832681',  '#842681',  '#862781',  '#882781',
  '#892881',  '#8B2981',  '#8C2981',  '#8E2A81',  '#902A81',
  '#912B81',  '#932B80',  '#942C80',  '#962C80',  '#982D80',
  '#992D80',  '#9B2E7F',  '#9C2E7F',  '#9E2F7F',  '#A02F7F',
  '#A1307E',  '#A3307E',  '#A5317E',  '#A6317D',  '#A8327D',
  '#AA337D',  '#AB337C',  '#AD347C',  '#AE347B',  '#B0357B',
  '#B2357B',  '#B3367A',  '#B5367A',  '#B73779',  '#B83779',
  '#BA3878',  '#BC3978',  '#BD3977',  '#BF3A77',  '#C03A76',
  '#C23B75',  '#C43C75',  '#C53C74',  '#C73D73',  '#C83E73',
  '#CA3E72',  '#CC3F71',  '#CD4071',  '#CF4070',  '#D0416F',
  '#D2426F',  '#D3436E',  '#D5446D',  '#D6456C',  '#D8456C',
  '#D9466B',  '#DB476A',  '#DC4869',  '#DE4968',  '#DF4A68',
  '#E04C67',  '#E24D66',  '#E34E65',  '#E44F64',  '#E55064',
  '#E75263',  '#E85362',  '#E95462',  '#EA5661',  '#EB5760',
  '#EC5860',  '#ED5A5F',  '#EE5B5E',  '#EF5D5E',  '#F05F5E',
  '#F1605D',  '#F2625D',  '#F2645C',  '#F3655C',  '#F4675C',
  '#F4695C',  '#F56B5C',  '#F66C5C',  '#F66E5C',  '#F7705C',
  '#F7725C',  '#F8745C',  '#F8765C',  '#F9785D',  '#F9795D',
  '#F97B5D',  '#FA7D5E',  '#FA7F5E',  '#FA815F',  '#FB835F',
  '#FB8560',  '#FB8761',  '#FC8961',  '#FC8A62',  '#FC8C63',
  '#FC8E64',  '#FC9065',  '#FD9266',  '#FD9467',  '#FD9668',
  '#FD9869',  '#FD9A6A',  '#FD9B6B',  '#FE9D6C',  '#FE9F6D',
  '#FEA16E',  '#FEA36F',  '#FEA571',  '#FEA772',  '#FEA973',
  '#FEAA74',  '#FEAC76',  '#FEAE77',  '#FEB078',  '#FEB27A',
  '#FEB47B',  '#FEB67C',  '#FEB77E',  '#FEB97F',  '#FEBB81',
  '#FEBD82',  '#FEBF84',  '#FEC185',  '#FEC287',  '#FEC488',
  '#FEC68A',  '#FEC88C',  '#FECA8D',  '#FECC8F',  '#FECD90',
  '#FECF92',  '#FED194',  '#FED395',  '#FED597',  '#FED799',
  '#FED89A',  '#FDDA9C',  '#FDDC9E',  '#FDDEA0',  '#FDE0A1',
  '#FDE2A3',  '#FDE3A5',  '#FDE5A7',  '#FDE7A9',  '#FDE9AA',
  '#FDEBAC',  '#FCECAE',  '#FCEEB0',  '#FCF0B2',  '#FCF2B4',
  '#FCF4B6',  '#FCF6B8',  '#FCF7B9',  '#FCF9BB',  '#FCFBBD',
  '#FCFDBF')

.inferno <- c(
  '#000004',  '#010005',  '#010106',  '#010108',  '#02010A',
  '#02020C',  '#02020E',  '#030210',  '#040312',  '#040314',
  '#050417',  '#060419',  '#07051B',  '#08051D',  '#09061F',
  '#0A0722',  '#0B0724',  '#0C0826',  '#0D0829',  '#0E092B',
  '#10092D',  '#110A30',  '#120A32',  '#140B34',  '#150B37',
  '#160B39',  '#180C3C',  '#190C3E',  '#1B0C41',  '#1C0C43',
  '#1E0C45',  '#1F0C48',  '#210C4A',  '#230C4C',  '#240C4F',
  '#260C51',  '#280B53',  '#290B55',  '#2B0B57',  '#2D0B59',
  '#2F0A5B',  '#310A5C',  '#320A5E',  '#340A5F',  '#360961',
  '#380962',  '#390963',  '#3B0964',  '#3D0965',  '#3E0966',
  '#400A67',  '#420A68',  '#440A68',  '#450A69',  '#470B6A',
  '#490B6A',  '#4A0C6B',  '#4C0C6B',  '#4D0D6C',  '#4F0D6C',
  '#510E6C',  '#520E6D',  '#540F6D',  '#550F6D',  '#57106E',
  '#59106E',  '#5A116E',  '#5C126E',  '#5D126E',  '#5F136E',
  '#61136E',  '#62146E',  '#64156E',  '#65156E',  '#67166E',
  '#69166E',  '#6A176E',  '#6C186E',  '#6D186E',  '#6F196E',
  '#71196E',  '#721A6E',  '#741A6E',  '#751B6E',  '#771C6D',
  '#781C6D',  '#7A1D6D',  '#7C1D6D',  '#7D1E6D',  '#7F1E6C',
  '#801F6C',  '#82206C',  '#84206B',  '#85216B',  '#87216B',
  '#88226A',  '#8A226A',  '#8C2369',  '#8D2369',  '#8F2469',
  '#902568',  '#922568',  '#932667',  '#952667',  '#972766',
  '#982766',  '#9A2865',  '#9B2964',  '#9D2964',  '#9F2A63',
  '#A02A63',  '#A22B62',  '#A32C61',  '#A52C60',  '#A62D60',
  '#A82E5F',  '#A92E5E',  '#AB2F5E',  '#AD305D',  '#AE305C',
  '#B0315B',  '#B1325A',  '#B3325A',  '#B43359',  '#B63458',
  '#B73557',  '#B93556',  '#BA3655',  '#BC3754',  '#BD3853',
  '#BF3952',  '#C03A51',  '#C13A50',  '#C33B4F',  '#C43C4E',
  '#C63D4D',  '#C73E4C',  '#C83F4B',  '#CA404A',  '#CB4149',
  '#CC4248',  '#CE4347',  '#CF4446',  '#D04545',  '#D24644',
  '#D34743',  '#D44842',  '#D54A41',  '#D74B3F',  '#D84C3E',
  '#D94D3D',  '#DA4E3C',  '#DB503B',  '#DD513A',  '#DE5238',
  '#DF5337',  '#E05536',  '#E15635',  '#E25734',  '#E35933',
  '#E45A31',  '#E55C30',  '#E65D2F',  '#E75E2E',  '#E8602D',
  '#E9612B',  '#EA632A',  '#EB6429',  '#EB6628',  '#EC6726',
  '#ED6925',  '#EE6A24',  '#EF6C23',  '#EF6E21',  '#F06F20',
  '#F1711F',  '#F1731D',  '#F2741C',  '#F3761B',  '#F37819',
  '#F47918',  '#F57B17',  '#F57D15',  '#F67E14',  '#F68013',
  '#F78212',  '#F78410',  '#F8850F',  '#F8870E',  '#F8890C',
  '#F98B0B',  '#F98C0A',  '#F98E09',  '#FA9008',  '#FA9207',
  '#FA9407',  '#FB9606',  '#FB9706',  '#FB9906',  '#FB9B06',
  '#FB9D07',  '#FC9F07',  '#FCA108',  '#FCA309',  '#FCA50A',
  '#FCA60C',  '#FCA80D',  '#FCAA0F',  '#FCAC11',  '#FCAE12',
  '#FCB014',  '#FCB216',  '#FCB418',  '#FBB61A',  '#FBB81D',
  '#FBBA1F',  '#FBBC21',  '#FBBE23',  '#FAC026',  '#FAC228',
  '#FAC42A',  '#FAC62D',  '#F9C72F',  '#F9C932',  '#F9CB35',
  '#F8CD37',  '#F8CF3A',  '#F7D13D',  '#F7D340',  '#F6D543',
  '#F6D746',  '#F5D949',  '#F5DB4C',  '#F4DD4F',  '#F4DF53',
  '#F4E156',  '#F3E35A',  '#F3E55D',  '#F2E661',  '#F2E865',
  '#F2EA69',  '#F1EC6D',  '#F1ED71',  '#F1EF75',  '#F1F179',
  '#F2F27D',  '#F2F482',  '#F3F586',  '#F3F68A',  '#F4F88E',
  '#F5F992',  '#F6FA96',  '#F8FB9A',  '#F9FC9D',  '#FAFDA1',
  '#FCFFA4'
)

.plasma <- c(
  '#0D0887',  '#100788',  '#130789',  '#16078A',  '#19068C',
  '#1B068D',  '#1D068E',  '#20068F',  '#220690',  '#240691',
  '#260591',  '#280592',  '#2A0593',  '#2C0594',  '#2E0595',
  '#2F0596',  '#310597',  '#330597',  '#350498',  '#370499',
  '#38049A',  '#3A049A',  '#3C049B',  '#3E049C',  '#3F049C',
  '#41049D',  '#43039E',  '#44039E',  '#46039F',  '#48039F',
  '#4903A0',  '#4B03A1',  '#4C02A1',  '#4E02A2',  '#5002A2',
  '#5102A3',  '#5302A3',  '#5502A4',  '#5601A4',  '#5801A4',
  '#5901A5',  '#5B01A5',  '#5C01A6',  '#5E01A6',  '#6001A6',
  '#6100A7',  '#6300A7',  '#6400A7',  '#6600A7',  '#6700A8',
  '#6900A8',  '#6A00A8',  '#6C00A8',  '#6E00A8',  '#6F00A8',
  '#7100A8',  '#7201A8',  '#7401A8',  '#7501A8',  '#7701A8',
  '#7801A8',  '#7A02A8',  '#7B02A8',  '#7D03A8',  '#7E03A8',
  '#8004A8',  '#8104A7',  '#8305A7',  '#8405A7',  '#8606A6',
  '#8707A6',  '#8808A6',  '#8A09A5',  '#8B0AA5',  '#8D0BA5',
  '#8E0CA4',  '#8F0DA4',  '#910EA3',  '#920FA3',  '#9410A2',
  '#9511A1',  '#9613A1',  '#9814A0',  '#99159F',  '#9A169F',
  '#9C179E',  '#9D189D',  '#9E199D',  '#A01A9C',  '#A11B9B',
  '#A21D9A',  '#A31E9A',  '#A51F99',  '#A62098',  '#A72197',
  '#A82296',  '#AA2395',  '#AB2494',  '#AC2694',  '#AD2793',
  '#AE2892',  '#B02991',  '#B12A90',  '#B22B8F',  '#B32C8E',
  '#B42E8D',  '#B52F8C',  '#B6308B',  '#B7318A',  '#B83289',
  '#BA3388',  '#BB3488',  '#BC3587',  '#BD3786',  '#BE3885',
  '#BF3984',  '#C03A83',  '#C13B82',  '#C23C81',  '#C33D80',
  '#C43E7F',  '#C5407E',  '#C6417D',  '#C7427C',  '#C8437B',
  '#C9447A',  '#CA457A',  '#CB4679',  '#CC4778',  '#CC4977',
  '#CD4A76',  '#CE4B75',  '#CF4C74',  '#D04D73',  '#D14E72',
  '#D24F71',  '#D35171',  '#D45270',  '#D5536F',  '#D5546E',
  '#D6556D',  '#D7566C',  '#D8576B',  '#D9586A',  '#DA5A6A',
  '#DA5B69',  '#DB5C68',  '#DC5D67',  '#DD5E66',  '#DE5F65',
  '#DE6164',  '#DF6263',  '#E06363',  '#E16462',  '#E26561',
  '#E26660',  '#E3685F',  '#E4695E',  '#E56A5D',  '#E56B5D',
  '#E66C5C',  '#E76E5B',  '#E76F5A',  '#E87059',  '#E97158',
  '#E97257',  '#EA7457',  '#EB7556',  '#EB7655',  '#EC7754',
  '#ED7953',  '#ED7A52',  '#EE7B51',  '#EF7C51',  '#EF7E50',
  '#F07F4F',  '#F0804E',  '#F1814D',  '#F1834C',  '#F2844B',
  '#F3854B',  '#F3874A',  '#F48849',  '#F48948',  '#F58B47',
  '#F58C46',  '#F68D45',  '#F68F44',  '#F79044',  '#F79143',
  '#F79342',  '#F89441',  '#F89540',  '#F9973F',  '#F9983E',
  '#F99A3E',  '#FA9B3D',  '#FA9C3C',  '#FA9E3B',  '#FB9F3A',
  '#FBA139',  '#FBA238',  '#FCA338',  '#FCA537',  '#FCA636',
  '#FCA835',  '#FCA934',  '#FDAB33',  '#FDAC33',  '#FDAE32',
  '#FDAF31',  '#FDB130',  '#FDB22F',  '#FDB42F',  '#FDB52E',
  '#FEB72D',  '#FEB82C',  '#FEBA2C',  '#FEBB2B',  '#FEBD2A',
  '#FEBE2A',  '#FEC029',  '#FDC229',  '#FDC328',  '#FDC527',
  '#FDC627',  '#FDC827',  '#FDCA26',  '#FDCB26',  '#FCCD25',
  '#FCCE25',  '#FCD025',  '#FCD225',  '#FBD324',  '#FBD524',
  '#FBD724',  '#FAD824',  '#FADA24',  '#F9DC24',  '#F9DD25',
  '#F8DF25',  '#F8E125',  '#F7E225',  '#F7E425',  '#F6E626',
  '#F6E826',  '#F5E926',  '#F5EB27',  '#F4ED27',  '#F3EE27',
  '#F3F027',  '#F2F227',  '#F1F426',  '#F1F525',  '#F0F724',
  '#F0F921'
)

.viridis <- c(
  '#440154',  '#440256',  '#450457',  '#450559',  '#46075A',
  '#46085C',  '#460A5D',  '#460B5E',  '#470D60',  '#470E61',
  '#471063',  '#471164',  '#471365',  '#481467',  '#481668',
  '#481769',  '#48186A',  '#481A6C',  '#481B6D',  '#481C6E',
  '#481D6F',  '#481F70',  '#482071',  '#482173',  '#482374',
  '#482475',  '#482576',  '#482677',  '#482878',  '#482979',
  '#472A7A',  '#472C7A',  '#472D7B',  '#472E7C',  '#472F7D',
  '#46307E',  '#46327E',  '#46337F',  '#463480',  '#453581',
  '#453781',  '#453882',  '#443983',  '#443A83',  '#443B84',
  '#433D84',  '#433E85',  '#423F85',  '#424086',  '#424186',
  '#414287',  '#414487',  '#404588',  '#404688',  '#3F4788',
  '#3F4889',  '#3E4989',  '#3E4A89',  '#3E4C8A',  '#3D4D8A',
  '#3D4E8A',  '#3C4F8A',  '#3C508B',  '#3B518B',  '#3B528B',
  '#3A538B',  '#3A548C',  '#39558C',  '#39568C',  '#38588C',
  '#38598C',  '#375A8C',  '#375B8D',  '#365C8D',  '#365D8D',
  '#355E8D',  '#355F8D',  '#34608D',  '#34618D',  '#33628D',
  '#33638D',  '#32648E',  '#32658E',  '#31668E',  '#31678E',
  '#31688E',  '#30698E',  '#306A8E',  '#2F6B8E',  '#2F6C8E',
  '#2E6D8E',  '#2E6E8E',  '#2E6F8E',  '#2D708E',  '#2D718E',
  '#2C718E',  '#2C728E',  '#2C738E',  '#2B748E',  '#2B758E',
  '#2A768E',  '#2A778E',  '#2A788E',  '#29798E',  '#297A8E',
  '#297B8E',  '#287C8E',  '#287D8E',  '#277E8E',  '#277F8E',
  '#27808E',  '#26818E',  '#26828E',  '#26828E',  '#25838E',
  '#25848E',  '#25858E',  '#24868E',  '#24878E',  '#23888E',
  '#23898E',  '#238A8D',  '#228B8D',  '#228C8D',  '#228D8D',
  '#218E8D',  '#218F8D',  '#21908D',  '#21918C',  '#20928C',
  '#20928C',  '#20938C',  '#1F948C',  '#1F958B',  '#1F968B',
  '#1F978B',  '#1F988B',  '#1F998A',  '#1F9A8A',  '#1E9B8A',
  '#1E9C89',  '#1E9D89',  '#1F9E89',  '#1F9F88',  '#1FA088',
  '#1FA188',  '#1FA187',  '#1FA287',  '#20A386',  '#20A486',
  '#21A585',  '#21A685',  '#22A785',  '#22A884',  '#23A983',
  '#24AA83',  '#25AB82',  '#25AC82',  '#26AD81',  '#27AD81',
  '#28AE80',  '#29AF7F',  '#2AB07F',  '#2CB17E',  '#2DB27D',
  '#2EB37C',  '#2FB47C',  '#31B57B',  '#32B67A',  '#34B679',
  '#35B779',  '#37B878',  '#38B977',  '#3ABA76',  '#3BBB75',
  '#3DBC74',  '#3FBC73',  '#40BD72',  '#42BE71',  '#44BF70',
  '#46C06F',  '#48C16E',  '#4AC16D',  '#4CC26C',  '#4EC36B',
  '#50C46A',  '#52C569',  '#54C568',  '#56C667',  '#58C765',
  '#5AC864',  '#5CC863',  '#5EC962',  '#60CA60',  '#63CB5F',
  '#65CB5E',  '#67CC5C',  '#69CD5B',  '#6CCD5A',  '#6ECE58',
  '#70CF57',  '#73D056',  '#75D054',  '#77D153',  '#7AD151',
  '#7CD250',  '#7FD34E',  '#81D34D',  '#84D44B',  '#86D549',
  '#89D548',  '#8BD646',  '#8ED645',  '#90D743',  '#93D741',
  '#95D840',  '#98D83E',  '#9BD93C',  '#9DD93B',  '#A0DA39',
  '#A2DA37',  '#A5DB36',  '#A8DB34',  '#AADC32',  '#ADDC30',
  '#B0DD2F',  '#B2DD2D',  '#B5DE2B',  '#B8DE29',  '#BADE28',
  '#BDDF26',  '#C0DF25',  '#C2DF23',  '#C5E021',  '#C8E020',
  '#CAE11F',  '#CDE11D',  '#D0E11C',  '#D2E21B',  '#D5E21A',
  '#D8E219',  '#DAE319',  '#DDE318',  '#DFE318',  '#E2E418',
  '#E5E419',  '#E7E419',  '#EAE51A',  '#ECE51B',  '#EFE51C',
  '#F1E51D',  '#F4E61E',  '#F6E620',  '#F8E621',  '#FBE723',
  '#FDE725'
)

#' @rdname palettes
#' @export
pal_seq_magma <- function(n, transp=0){
  ramp <- colorRamp(.magma)
  f <- function(n, transp=0) {
    x <- ramp(seq.int(0, 1, length.out = n))
    if (ncol(x) == 4L)
      rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255) %>% pal_alpha(transp)
    else rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255) %>% pal_alpha(transp)
  }
  f(n, transp)
}

#' @rdname palettes
#' @export
pal_seq_inferno <- function(n, transp=0){
  ramp <- colorRamp(.inferno)
  f <- function(n, transp=0) {
    x <- ramp(seq.int(0, 1, length.out = n))
    if (ncol(x) == 4L)
      rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255) %>% pal_alpha(transp)
    else rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255) %>% pal_alpha(transp)
  }
  f(n, transp)
}

#' @rdname palettes
#' @export
pal_seq_plasma <- function(n, transp=0){
  ramp <- colorRamp(.plasma)
  f <- function(n, transp=0) {
    x <- ramp(seq.int(0, 1, length.out = n))
    if (ncol(x) == 4L)
      rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255) %>% pal_alpha(transp)
    else rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255) %>% pal_alpha(transp)
  }
  f(n, transp)
}

#' @rdname palettes
#' @export
pal_seq_viridis <- function(n, transp=0){
  ramp <- colorRamp(.viridis)
  f <- function(n, transp) {
    x <- ramp(seq.int(0, 1, length.out = n))
    if (ncol(x) == 4L)
      rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255) %>% pal_alpha(transp)
    else rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255) %>% pal_alpha(transp)
  }
  f(n, transp)
}
