# Zap attributes

Modelled on
[`haven::zap_labels()`](https://haven.tidyverse.org/reference/zap_labels.html),
but more encompassing. By default removes the following attributes:
format.spss, format.sas, format.stata, label, labels, na_values,
na_range, display_width

Modelled on
[`haven::zap_labels()`](https://haven.tidyverse.org/reference/zap_labels.html),
but more encompassing. By default removes the following attributes:
format.spss, format.sas, format.stata, label, labels, na_values,
na_range, display_width

## Usage

``` r
zap_attributes(
  x,
  attributes = c("format.spss", "format.sas", "format.stata", "label", "labels",
    "na_values", "na_range", "display_width")
)

zap_attributes(
  x,
  attributes = c("format.spss", "format.sas", "format.stata", "label", "labels",
    "na_values", "na_range", "display_width")
)
```

## Arguments

- x:

  the data frame or variable

- attributes:

  character vector of attributes to zap. NULL if everything (including
  factor levels etc) should be zapped

## Examples

``` r
bfi <- data.frame(matrix(data = rnorm(300), ncol = 3))
names(bfi) <- c("bfi_e1", "bfi_e2R", "bfi_e3")
attributes(bfi$bfi_e1)$label <- "I am outgoing."
attributes(bfi$bfi_e2R)$label <- "I prefer books to people."
attributes(bfi$bfi_e3)$label <- "I love to party."
bfi$bfi_e <- rowMeans(bfi[, c("bfi_e1", "bfi_e2R", "bfi_e3")])
bfi <- detect_scales(bfi, quiet = TRUE) # create attributes
str(zap_attributes(bfi, "label"))
#> 'data.frame':    100 obs. of  4 variables:
#>  $ bfi_e1 : num  -1.534 1.414 -0.218 -0.158 -0.504 ...
#>  $ bfi_e2R: num  -0.513 -0.42 -1.303 -0.742 -0.881 ...
#>  $ bfi_e3 : num  1.281 -0.66 0.467 -0.533 -0.372 ...
#>  $ bfi_e  : num  -0.255 0.112 -0.351 -0.478 -0.586 ...
#>   ..- attr(*, "scale_item_names")= chr [1:3] "bfi_e1" "bfi_e2R" "bfi_e3"
zap_attributes(bfi$bfi_e)
#>   [1] -0.25523913  0.11159753 -0.35125463 -0.47772608 -0.58571094 -0.72702519
#>   [7] -0.09650655 -0.30568227 -0.56180936  0.85717916  0.19498347 -0.06708436
#>  [13]  0.13006760 -0.21337874 -0.09196357  0.56131592 -0.11130615 -0.36103121
#>  [19] -0.24236768  1.27483578 -0.12785163 -0.26391056 -0.27400369 -0.92790128
#>  [25]  1.01780845 -0.88626527 -0.16216249  0.58737656 -0.75410047  0.56609497
#>  [31] -0.22094860  1.02858331 -0.58284008  0.16526207  0.22862913 -0.62937307
#>  [37] -0.83242842 -0.23585387  0.60602911  0.80457805 -0.19381838  0.06590728
#>  [43] -0.45747936 -0.13811728  0.98916758  0.13353981  0.15502826  0.40961592
#>  [49] -0.16241003  0.57449742  0.16420112 -0.88764490  0.54156870  0.49200003
#>  [55] -0.15567229  0.59475641  1.15498492  1.83276946 -0.51828486 -1.35761888
#>  [61]  0.65903688  0.21873076  0.29237630  0.86314832  0.34566035 -0.24986580
#>  [67]  0.32762185 -0.59142172  0.27696730  0.57686346  0.78910808 -0.30224317
#>  [73]  0.19434076  0.21252164 -0.58189959 -0.05866669  1.17880207  0.88545468
#>  [79]  0.29745246 -0.76023167  0.51490443 -0.25448128  0.14577802 -0.01253102
#>  [85] -0.91348910 -0.64427224  0.30046522  0.79656129  0.74657115  0.05693242
#>  [91]  0.65622417 -0.77866602  0.34333199  0.22143575  0.73012162 -0.05203553
#>  [97] -0.63420220  0.27808222 -0.27011602 -1.26098397
bfi <- data.frame(matrix(data = rnorm(300), ncol = 3))
names(bfi) <- c("bfi_e1", "bfi_e2R", "bfi_e3")
attributes(bfi$bfi_e1)$label <- "I am outgoing."
attributes(bfi$bfi_e2R)$label <- "I prefer books to people."
attributes(bfi$bfi_e3)$label <- "I love to party."
bfi$bfi_e <- rowMeans(bfi[, c("bfi_e1", "bfi_e2R", "bfi_e3")])
bfi <- detect_scales(bfi, quiet = TRUE) # create attributes
str(zap_attributes(bfi, "label"))
#> 'data.frame':    100 obs. of  4 variables:
#>  $ bfi_e1 : num  0.386 0.766 1.062 -0.934 -0.291 ...
#>  $ bfi_e2R: num  0.0212 -0.979 0.9274 -0.1695 0.4665 ...
#>  $ bfi_e3 : num  0.234 0.337 -0.513 -0.278 1.681 ...
#>  $ bfi_e  : num  0.2136 0.0413 0.4922 -0.4607 0.6191 ...
#>   ..- attr(*, "scale_item_names")= chr [1:3] "bfi_e1" "bfi_e2R" "bfi_e3"
zap_attributes(bfi$bfi_e)
#>   [1]  0.21361582  0.04126257  0.49223886 -0.46067356  0.61906165  0.73262951
#>   [7] -0.21164239 -0.75676898  0.91920009  0.27443198 -0.02042341  0.10980058
#>  [13] -0.75121945  0.54658096  0.23548549  0.66664103  0.44861597  1.01109739
#>  [19]  0.63343394 -1.23515016  0.36209288 -0.33516627  0.24801681  1.19470958
#>  [25]  0.50257123 -0.41947774 -0.02503102  0.16204421 -0.11097418  0.25277643
#>  [31] -0.75868150 -1.27915782 -0.05982189  0.88571981  0.77558347  0.52929829
#>  [37]  0.39176595 -0.62558657  0.74272472  0.23602041  0.48666970 -1.54324056
#>  [43]  0.33296381  0.83812541 -1.09021140 -0.22464109 -0.20917445  0.35482595
#>  [49] -0.12574700 -0.10971118  0.25483071 -0.57916056  0.53905494 -0.13410289
#>  [55] -0.07406088  0.21712877  0.17208567  0.01295638  1.59417568 -0.78847861
#>  [61]  0.17797955  0.87017085 -0.33034736 -0.53173808  0.07450761 -0.05375663
#>  [67]  0.47715852  1.67225827 -0.86710195 -0.16050488 -0.40387020 -0.47791920
#>  [73] -0.91231069  0.31339042 -1.35665593 -0.26473937 -0.40075686  0.38347550
#>  [79]  0.37477234 -0.51348495 -1.66363840 -0.20473251  0.20372999 -0.76524705
#>  [85] -0.28481408 -0.22470215  0.21171329 -0.21763812  0.24229947 -0.45359643
#>  [91] -0.50263813  0.25599832 -0.40938896  0.93374627  0.34383427  0.03426259
#>  [97] -0.01912929  0.85385950 -0.42217333 -0.76211942
```
