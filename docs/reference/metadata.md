# Add metadata to a dataset

Use this function to describe a data frame in preparation for JSON-LD
metadata generation using
[`codebook()`](https://rubenarslan.github.io/codebook/reference/codebook.md)
or
[`metadata_list()`](https://rubenarslan.github.io/codebook/reference/metadata_list.md).

## Usage

``` r
metadata(data)

metadata(data) <- value
```

## Arguments

- data:

  the data frame

- value:

  the metadata attribute

## Examples

``` r
data('bfi')
metadata(bfi)$name <- "MOCK Big Five Inventory dataset (German metadata demo)"
metadata(bfi)$description <- "a small mock Big Five Inventory dataset"
metadata(bfi)$identifier <- "doi:10.5281/zenodo.1326520"
metadata(bfi)$datePublished <- "2016-06-01"
metadata(bfi)$creator <- list(
  "@type" = "Person",
  givenName = "Ruben", familyName = "Arslan",
  email = "ruben.arslan@gmail.com",
  affiliation = list("@type" = "Organization",
                     name = "MPI Human Development, Berlin"))
metadata(bfi)$citation <- "Arslan (2016). Mock BFI data."
metadata(bfi)$url <-
  "https://rubenarslan.github.io/codebook/articles/codebook.html"
metadata(bfi)$temporalCoverage <- "2016"
metadata(bfi)$spatialCoverage <- "Goettingen, Germany"
metadata(bfi)$keywords <- c("Personality", "Psychology")
metadata(bfi)
#> $name
#> [1] "MOCK Big Five Inventory dataset (German metadata demo)"
#> 
#> $description
#> [1] "a small mock Big Five Inventory dataset"
#> 
#> $identifier
#> [1] "doi:10.5281/zenodo.1326520"
#> 
#> $datePublished
#> [1] "2016-06-01"
#> 
#> $creator
#> $creator$`@type`
#> [1] "Person"
#> 
#> $creator$givenName
#> [1] "Ruben"
#> 
#> $creator$familyName
#> [1] "Arslan"
#> 
#> $creator$email
#> [1] "ruben.arslan@gmail.com"
#> 
#> $creator$affiliation
#> $creator$affiliation$`@type`
#> [1] "Organization"
#> 
#> $creator$affiliation$name
#> [1] "MPI Human Development, Berlin"
#> 
#> 
#> 
#> $citation
#> [1] "Arslan (2016). Mock BFI data."
#> 
#> $url
#> [1] "https://rubenarslan.github.io/codebook/articles/codebook.html"
#> 
#> $temporalCoverage
#> [1] "2016"
#> 
#> $spatialCoverage
#> [1] "Goettingen, Germany"
#> 
#> $keywords
#> [1] "Personality" "Psychology" 
#> 
```
