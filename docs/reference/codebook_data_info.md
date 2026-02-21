# Codebook data info

A readout of the metadata for this dataset, with some defaults set

## Usage

``` r
codebook_data_info(results, indent = "##")
```

## Arguments

- results:

  a data frame which has the following columns: session, created,
  modified, expired, ended

- indent:

  add \# to this to make the headings in the components lower-level.
  defaults to beginning at h2

## Examples

``` r
# will generate figures in a figure/ subdirectory
data("bfi")
metadata(bfi)$name <- "MOCK Big Five Inventory dataset (German metadata demo)"
metadata(bfi)$description <- "a small mock Big Five Inventory dataset"
metadata(bfi)$citation <- "doi:10.5281/zenodo.1326520"
metadata(bfi)$url <-
   "https://rubenarslan.github.io/codebook/articles/codebook.html"
codebook_data_info(bfi)
#> No viewer found, probably documenting or testing
#> 
#> 
#> 
#> ### Metadata
#> 
#> #### Description
#> __Dataset name__: MOCK Big Five Inventory dataset (German metadata demo)
#> 
#> 
#> a small mock Big Five Inventory dataset
#> 
#> <details>
#> <summary title="Expand this section to see some additional metadata in a structured format that is useful for search engines">Metadata for search engines</summary>
#> 
#> 
#> 
#> - __Citation__: doi:10.5281/zenodo.1326520
#> 
#> - __URL__: [https://rubenarslan.github.io/codebook/articles/codebook.html](https://rubenarslan.github.io/codebook/articles/codebook.html)
#> 
#> 
#> 
#> 
#> 
#> 
#> 
#> 
#> </details>
```
