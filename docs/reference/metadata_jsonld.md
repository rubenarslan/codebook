# Metadata as JSON-LD

Echo a list of a metadata, generated using
[`metadata_list()`](https://rubenarslan.github.io/codebook/reference/metadata_list.md)
as JSON-LD in a script tag.

## Usage

``` r
metadata_jsonld(results)
```

## Arguments

- results:

  a data frame, ideally with attributes set on variables

## Examples

``` r
data("bfi")
metadata_jsonld(bfi)
#> No viewer found, probably documenting or testing
#> 
#> 
#> 
#> <script type="application/ld+json">
#> {
#>   "@context": "https://schema.org/",
#>   "@type": "Dataset",
#>   "variableMeasured": [
#>     {
#>       "name": "A1",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "A2",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "A3",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "A4",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "A5",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "C1",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "C2",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "C3",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "C4",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "C5",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "E1",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "E2",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "E3",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "E4",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "E5",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "N1",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "N2",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "N3",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "N4",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "N5",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "O1",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "O2",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "O3",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "O4",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "O5",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "gender",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "education",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "age",
#>       "@type": "propertyValue"
#>     }
#>   ],
#>   "description": "\n\n\n## Table of variables\nThis table contains variable names, labels, and number of missing values.\nSee the complete codebook for more.\n\n|name      |label | n_missing|\n|:---------|:-----|---------:|\n|A1        |NA    |        16|\n|A2        |NA    |        27|\n|A3        |NA    |        26|\n|A4        |NA    |        19|\n|A5        |NA    |        16|\n|C1        |NA    |        21|\n|C2        |NA    |        24|\n|C3        |NA    |        20|\n|C4        |NA    |        26|\n|C5        |NA    |        16|\n|E1        |NA    |        23|\n|E2        |NA    |        16|\n|E3        |NA    |        25|\n|E4        |NA    |         9|\n|E5        |NA    |        21|\n|N1        |NA    |        22|\n|N2        |NA    |        21|\n|N3        |NA    |        11|\n|N4        |NA    |        36|\n|N5        |NA    |        29|\n|O1        |NA    |        22|\n|O2        |NA    |         0|\n|O3        |NA    |        28|\n|O4        |NA    |        14|\n|O5        |NA    |        20|\n|gender    |NA    |         0|\n|education |NA    |       223|\n|age       |NA    |         0|\n\n### Note\nThis dataset was automatically described using the [codebook R package](https://rubenarslan.github.io/codebook/) (version 0.9.7)."
#> }
#> </script>
#> 
#> <details><summary>JSON-LD metadata</summary>
#> The following JSON-LD can be found by search engines, if you share this codebook
#> publicly on the web.
#> 
#> ```json
#> {
#>   "@context": "https://schema.org/",
#>   "@type": "Dataset",
#>   "variableMeasured": [
#>     {
#>       "name": "A1",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "A2",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "A3",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "A4",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "A5",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "C1",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "C2",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "C3",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "C4",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "C5",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "E1",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "E2",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "E3",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "E4",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "E5",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "N1",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "N2",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "N3",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "N4",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "N5",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "O1",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "O2",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "O3",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "O4",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "O5",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "gender",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "education",
#>       "@type": "propertyValue"
#>     },
#>     {
#>       "name": "age",
#>       "@type": "propertyValue"
#>     }
#>   ],
#>   "description": "\n\n\n## Table of variables\nThis table contains variable names, labels, and number of missing values.\nSee the complete codebook for more.\n\n|name      |label | n_missing|\n|:---------|:-----|---------:|\n|A1        |NA    |        16|\n|A2        |NA    |        27|\n|A3        |NA    |        26|\n|A4        |NA    |        19|\n|A5        |NA    |        16|\n|C1        |NA    |        21|\n|C2        |NA    |        24|\n|C3        |NA    |        20|\n|C4        |NA    |        26|\n|C5        |NA    |        16|\n|E1        |NA    |        23|\n|E2        |NA    |        16|\n|E3        |NA    |        25|\n|E4        |NA    |         9|\n|E5        |NA    |        21|\n|N1        |NA    |        22|\n|N2        |NA    |        21|\n|N3        |NA    |        11|\n|N4        |NA    |        36|\n|N5        |NA    |        29|\n|O1        |NA    |        22|\n|O2        |NA    |         0|\n|O3        |NA    |        28|\n|O4        |NA    |        14|\n|O5        |NA    |        20|\n|gender    |NA    |         0|\n|education |NA    |       223|\n|age       |NA    |         0|\n\n### Note\nThis dataset was automatically described using the [codebook R package](https://rubenarslan.github.io/codebook/) (version 0.9.7)."
#> }`
#> ```
#> </details>
```
