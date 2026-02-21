# Create a codebook rmarkdown document

This function will create and open an .Rmd file in the current working
directory. By default, the file is named codebook.Rmd. No files will be
overwritten. The .Rmd file has some useful defaults set for creating
codebooks.

## Usage

``` r
new_codebook_rmd(filename = "codebook", template = "default")
```

## Arguments

- filename:

  under which file name do you want to create a template

- template:

  only "default" exists for now

## Examples

``` r
if (FALSE) { # \dontrun{
new_codebook_rmd()
} # }
```
