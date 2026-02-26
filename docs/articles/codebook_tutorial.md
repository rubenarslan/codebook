# Tutorial

This is the practical part of a tutorial manuscript for this package,
which you can find in full [on
PsyArXiv](https://osf.io/preprints/psyarxiv/5qc6h/) or if you prefer a
copy-edited open access version it is published in [Advances in Methods
and Practices in Psychological
Science](https://doi.org/10.1177/2515245919838783).

You can cite it, if you use the package in a publication:

> Arslan, R. C. (2019). How to automatically document data with the
> codebook package to facilitate data re-use. Advances in Methods and
> Practices in Psychological Science, 2(2), 169–187.
> <https://doi.org/10.1177/2515245919838783>

Using the codebook package locally in RStudio

``` r

knit_by_pkgdown <- !is.null(knitr::opts_chunk$get("fig.retina"))
knitr::opts_chunk$set(
  warning = TRUE, # show warnings during codebook generation
  message = TRUE, # show messages during codebook generation
  error = TRUE, # do not interrupt codebook generation in case of errors,
                # TRUE is usually better for debugging
  echo = TRUE  # show R code
)
ggplot2::theme_set(ggplot2::theme_bw())
```

## Loading data

It is time to load some data. In this Tutorial, I will walk you through
the process by using the “bfi” dataset made available in the psych
package (Goldberg, 1999; Revelle et al., 2016; Revelle, Wilt, &
Rosenthal, 2010). The bfi dataset is already very well documented in the
psych R package, but using the codebook package, we can add
automatically computed reliabilities, graphs, and machine-readable
metadata to the mix. The dataset is available within R, but this will
not usually be the case; I have therefore uploaded it to the OSF, which
also features many other publicly available datasets. A new package in
R, rio (Chan & Leeper, 2018), makes loading data from websites in almost
any format as easy as loading local data. You can import the dataset
directly from the OSF by replacing the line

``` r

library(codebook)
codebook_data <- codebook::bfi
```

with

``` r

codebook_data <- tryCatch(
  rio::import("https://osf.io/download/s87kd", "csv"),
  error = function(e) {
    message(
      "Could not download dataset from https://osf.io/download/s87kd: ",
      conditionMessage(e), "\nUsing bundled data instead."
    )
    codebook::bfi
  }
)
```

on line 34. R Markdown documents have to be reproducible and
self-contained, so it is not enough for a dataset to be loaded locally;
you must load the dataset at the beginning of the document. You can also
use the document interactively, although this will not work seamlessly
for the codebook package. To see how this works, execute the line you
just added by pressing Command + Enter (for a Mac) or Ctrl + Enter (for
other platforms).

RStudio has a convenient data viewer you can use to check whether your
command worked. In the environment tab on the top right, you should see
“codebook_data”. Click that row to open a spreadsheet view of the
dataset in RStudio. As you can see, it is not particularly informative.
Just long columns of numbers with variable names like A4. Are we talking
aggressiveness, agreeableness, or the German industrial norm for paper
size? The lack of useful metadata is palpable. Click “Knit” again to see
what the codebook package can do with this. This time, it will take
longer. Once the result is shown in the viewer tab, scroll through it.
You can see a few warnings stating that the package saw items that might
form part of a scale, but there was no aggregated scale. You will also
see graphs of the distribution for each item and summary statistics.

## Adding and changing metadata

### Variable labels

The last codebook you generated could already be useful if the variables
had meaningful names and self-explanatory values. Unfortunately, this is
rarely the case. Generally, you will need more metadata: labels for
variables and values, a dataset description, and so on. The codebook
package can use metadata that are stored in R attributes. Attributes in
R are most commonly used to store the type of a variable; for instance,
datetime in R is just a number with two attributes (a time zone and a
class). However, they can just as easily store other metadata; the Hmisc
(Harrell, 2018), haven (Wickham & Miller, 2018), and rio (Chan & Leeper,
2018) packages, for example, use attributes to store labels. The benefit
of storing variable metadata in attributes is that even datasets that
are the product of merging and processing raw data retain the necessary
metadata. The haven and rio packages set these attributes when importing
data from SPSS or Stata files. However, it is also easy to add metadata
yourself:

``` r

attributes(codebook_data$C5)$label <- "Waste my time."
```

You have just assigned a new label to a variable. Because it is
inconvenient to do this over and over again, the labelled package
(Larmarange, 2018) adds a few convenience functions. Load the labelled
package by writing the following in your codebook.Rmd

``` r

library(labelled)
```

    ## 
    ## Attaching package: 'labelled'

    ## The following object is masked from 'package:codebook':
    ## 
    ##     to_factor

Now label the C5 item.

``` r

var_label(codebook_data$C5) <- "Waste my time."
```

You can also label values in this manner (label in quotes before the
equal sign, value after):

``` r

val_labels(codebook_data$C1) <- c("Very Inaccurate" = 1, "Very Accurate" = 6)
```

Write these labelling commands after loading the dataset and click
“Knit” again. As you can see in the viewer pane, the graph for the C1
variable now has a label at the top and the lowest and highest values on
the X axis are labelled. If the prospect of adding labels for every
single variable seems tedious, do not fear. Many researchers already
have a codebook in the form of a spreadsheet that they want to import in
order to avoid entering labels one-by-one. The bfi dataset in the psych
package is a good example of this, because it comes with a tabular
dictionary. On the line after loading the bfi data, type the following
to import this data dictionary:

``` r

dict <- tryCatch(
  rio::import("https://osf.io/download/cs678", "csv"),
  error = function(e) NULL
)
if (is.null(dict)) {
  message(
    "Could not download dictionary from https://osf.io/download/cs678\n",
    "The remote resource may be temporarily unavailable. Skipping remainder of this vignette."
  )
  knitr::knit_exit()
}
```

To see what you just loaded, click the “dict” row in the environment tab
in the top right panel. As you can see, the dictionary has information
on the constructs on which this item loads and on the direction with
which it should load on the construct. You can make these metadata
usable through the codebook package. You will often need to work on the
data frames to help you do this; to make this easier, use the dplyr
package (Wickham, François, Henry, & Müller, 2018). Load it by typing
the following

``` r

library(dplyr)
```

Your next goal is to use the variable labels that are already in the
dictionary. Because you want to label many variables at once, you need a
list of variable labels. Instead of assigning one label to one variable
as you just did, you can assign many labels to the whole dataset from a
named list. Here, each element of the list is one item that you want to
label.

``` r

var_label(codebook_data) <- list(
        C5 = "Waste my time.", 
        C1 = "Am exacting in my work."
)
```

There are already a list of variables and labels in your data dictionary
that you can use, so you do not have to perform the tedious task of
writing out the list. You do have to reshape it slightly though, because
it is currently in the form of a rectangular data frame, not a named
list. To do so, use a convenience function from the codebook function
called `dict_to_list`. This function expects to receive a data frame
with two columns: the first should be the variable names, the second the
variable labels. To select these columns, use the select function from
the dplyr package. You will also need to use a special operator, called
a pipe, which looks like this %\>% and allows you to read and write R
code from left to right, almost like an English sentence. First, you
need to take the dict dataset, then select the variable and label
columns, then use the `dict_to_list` function. You also need to assign
the result of this operation to become the variable labels of
codebook_data. You can do all this in a single line using pipes. Add the
following line after importing the dictionary.

``` r

var_label(codebook_data) <- dict %>% select(variable, label) %>% dict_to_list()
```

Click “codebook_data” in the Environment tab again. You should now see
the variable labels below the variable names. If you click “Knit” again,
you will see that your codebook now contains the variable labels. They
are both part of the plots and part of the codebook table at the end.
They are also part of the metadata that can be found using, for example,
Google Dataset Search, but this will not be visible to you.

### Value labels

So far, so good. But you may have noticed that education is shown as a
number. Does this indicate years of education? The average is 3, so that
seems unlikely. In fact, these numbers signify levels of education. In
the dict data frame, you can see that there are are value labels for the
levels of this variable. However, these levels of education are
abbreviated, and you can probably imagine that it would be difficult for
an automated program to understand how these map to the values in your
dataset. You can do better, using another function from the labelled
package: not `var_label` this time, but `val_labels`. Unlike
`var_label`, `val_labels` expects not just one label, but a named
vector, with a name for each value that you want to label. You do not
need to label all values. Named vectors are created using the c()
function. Add the following lines right after the last one.

``` r

val_labels(codebook_data$gender) <- c("male" = 1, "female" = 2)
val_labels(codebook_data$education) <- c("in high school" = 1,
   "finished high school" = 2,
              "some college" = 3, 
               "college graduate" = 4, 
              "graduate degree" = 5)
```

Click the “Knit” button. The bars in the graphs for education and gender
should now be labelled. Now, on to the many Likert items, which all have
the same value labels. You could assign them in the same way you did for
gender and education, entering the lines for each variable over and
over, or you could let a function do the job for you instead. Creating a
function is actually very simple. Just pick a name, ideally one to
remember it by—I chose `add_likert_labels`—and assign the keyword
function followed by two different kinds of brackets. Round brackets
surround the variable x. The x here is a placeholder for the many
variables you will use this function for in the next step. Curly braces
show that you intend to write out what you plan to do with the variable
x. Inside the curly braces, use the `val_labels` function from above and
assign a named vector.

``` r

add_likert_labels <- function(x) {
  val_labels(x) <- c("Very Inaccurate" = 1, 
                  "Moderately Inaccurate" = 2, 
                  "Slightly Inaccurate" = 3,
                  "Slightly Accurate" = 4,
                  "Moderately Accurate" = 5,
                  "Very Accurate" = 6)
  x
}
```

A function is just a tool and does nothing on its own; you have not used
it yet. To use it only on the Likert items, you need a list of them. An
easy way to achieve this is to subset the dict dataframe to only take
those variables that are part of the Big Six. To do so, use the filter
and pull functions from the dplyr package.

``` r

likert_items <- dict %>% filter(Big6 != "") %>% pull(variable)
```

To apply your new function to these items, use another function from the
dplyr package called `mutate_at`. It expects a list of variables and a
function to apply to each. You have both! You can now add value labels
to all Likert items in the codebook_data.

``` r

codebook_data <- codebook_data %>% mutate_at(likert_items,  add_likert_labels)
```

Click “Knit” again. All items should now have value labels. However,
this display is quite repetitive. How about grouping the items by the
factor that they are supposed to load on? And while you are at it, how
can the metadata about keying (or reverse-coded items) in your
dictionary become part of the dataset?

### Adding scales

The codebook package relies on a simple convention to be able to
summarise psychological scales, such as the Big Five dimension
extraversion, which are aggregates across several items. Your next step
will be to assign a new variable, extraversion, to the result of
selecting all extraversion items in the data and passing them to the
`aggregate_and_document_scale` function. This function takes the mean of
its inputs and assigns a label to the result, so that you can still tell
which variables it is an aggregate of.

``` r

codebook_data$extraversion <- codebook_data %>% select(E1:E5) %>% aggregate_and_document_scale()
```

Try knitting now. In the resulting codebook, the items for extraversion
have been grouped in one graph. In addition, several internal
consistency coefficients have been calculated. However, they are oddly
low. You need to reverse items which negatively load on the extraversion
factor, such as “Don’t talk a lot.” To do so, I suggest following a
simple convention early on, when you come up with names for your
items—namely the format scale_numberR (e.g., bfi_extra_1R for a
reverse-coded extraversion item, bfi_neuro_2 for a neuroticism item).
That way, the analyst always knows how an item relates to a scale. This
information is encoded in the data dictionary from the data you just
imported. Rename the reverse-coded items so that you cannot forget about
its direction. First, you need to grab all items with a negative keying
from your dictionary. Add the following three lines above the
aggregate_and_document_scale() line from above.

``` r

reversed_items <- dict %>% filter(Keying == -1) %>% pull(variable)
```

You can see in your Environment tab that names such as A1, C4, and C5
are now stored in the `reversed_items` vector. You can now refer to this
vector using the `rename_at` function, which applies a function to all
variables you list. Use the very simple function `add_R`, which does
exactly what its name indicates.

``` r

codebook_data <- codebook_data %>% 
  rename_at(reversed_items,  add_R)
```

Click “codebook_data” in the Environment tab and you will see that some
variables have been renamed: A1R, C4R, and C5R, and so on. This could
lead to an ambiguity: Does the suffix R means “should be reversed before
aggregation” or “has already been reversed”? With the help of metadata
in the form of labelled values, there is no potential for confusion. You
can reverse the underlying values, but keep the value labels right. So,
if somebody responded “Very accurate,” that remains the case, but the
underlying value will switch from 6 to 1 for a reversed item. The data
you generally import will rarely include labels that remain correct
regardless of whether underlying values are reversed, but the codebook
package makes it easy to bring the data into this shape. A command using
dplyr functions and the `reverse_labelled_values` function can easily
remedy this.

``` r

codebook_data <- codebook_data %>% 
    mutate_at(vars(matches("\\dR$")), reverse_labelled_values)
```

All this statement does is find variable names which end with a number
(\d is the regular expression codeword for a number; a dollar sign
denotes the end of the string) and R and reverse them. Because the
extraversion items have been renamed, we have to amend our scale
aggregation line slightly.

``` r

codebook_data$extraversion <- codebook_data %>% select(E1R:E5) %>% aggregate_and_document_scale()
```

Try knitting again. The reliability for the extraversion scale should be
much higher and all items should load positively. Adding further scales
is easy: Just repeat the above line, changing the names of the scale and
the items. Adding scales that integrate smaller scales is also
straightforward. The data dictionary mentions the Giant Three—try adding
one, Plasticity, which subsumes Extraversion and Openness.

``` r

codebook_data$plasticity <- codebook_data %>% select(E1R:E5, O1:O5R) %>% aggregate_and_document_scale() 
```

Note that writing `E1R:E5` only works if the items are all in order in
your dataset. If you mixed items across constructs, you will need a
different way to select them. One option is to list all items, writing
`select(E1R, E2R, E3, E4, E5)`. This can get tedious when listing many
items. Another solution is to write `select(starts_with("E"))`. Although
this is quite elegant, it will not work in this case because you have
more than one variable that starts with E; this command would include
education items along with the extraversion items you want. This is a
good reason to give items descriptive stems such as extraversion\_ or
bfik_extra. Longer stems not only make confusion less likely, they also
make it possible for you to refer to groups of items by their stems, and
ideally to their aggregates by only the stem. If you have already named
your item too minimally, another solution is to use a regular
expression, as I introduced above for matching reversed items. In this
scenario, `select(matches("^E\\dR?$"))` would work.

## Metadata about the entire dataset

Finally, you might want to sign your work and add a few descriptive
words about the entire dataset. If you simply edit the R Markdown
document to add a description, this information would not become part of
the machine-readable metadata. Metadata (or attributes) of the dataset
as a whole are a lot less persistent than metadata about variables.
Hence, you should add your description right before calling the codebook
function. Adding metadata about the dataset is very simple: Just wrap
the metadata function around codebook_data and assign a value to a
field. The fields “name” and “description” are required. If you do not
edit them, they will be automatically generated based on the data frame
name and its contents. To overwrite them, enter the following lines
above the call codebook(codebook_data):

``` r

metadata(codebook_data)$name <- "25 Personality items representing 5 factors"
metadata(codebook_data)$description <- "25 personality self report items taken from the International Personality Item Pool (ipip.ori.org)[...]"
```

It is good practice to give datasets a canonical identifier. This way,
if a dataset is described in multiple locations, it can still be
identified as the same dataset. For instance, when I did this I did not
want to use the URL of the R package from which I took the package
because URLs can change; instead, I generated a persistent document
object identifier (DOI) on the OSF and specified it here.

``` r

metadata(codebook_data)$identifier <- "https://dx.doi.org/10.17605/OSF.IO/K39BG"
```

In order to let others know who they can contact about the dataset, how
to cite it, and where to find more information, I set the attributes
creator, citation, and URL below.

``` r

metadata(codebook_data)$creator <- "William Revelle"
metadata(codebook_data)$citation <- "Revelle, W., Wilt, J., & Rosenthal, A. (2010). Individual differences in cognition: New methods for examining the personality-cognition link. In A. Gruszka, G. Matthews, & B. Szymura (Eds.), Handbook of individual differences in cognition: Attention, memory, and executive control (pp. 27–49). New York, NY: Springer."
metadata(codebook_data)$url <- "https://CRAN.R-project.org/package=psych"
```

Lastly, it is useful to note when and where the data was collected, as
well as when it was published. Ideally, you would make more specific
information available here, but this is all I know about the BFI
dataset.

``` r

metadata(codebook_data)$datePublished <- "2010-01-01"
metadata(codebook_data)$temporalCoverage <- "Spring 2010" 
metadata(codebook_data)$spatialCoverage <- "Online" 
```

These attributes are documented in more depth on
<https://schema.org/Dataset>. You can also add attributes that are not
documented there, but they will not become part of the machine-readable
metadata. Click “Knit” again. In the viewer tab, you can see that the
metadata section of the codebook has been populated with your additions.

## Exporting and sharing the data with metadata

Having added all the variable-level metadata, you might want to re-use
the marked-up data elsewhere or share it with collaborators or the
public. You can most easily export it using the rio package (Chan &
Leeper, 2018), which permits embedding the variable metadata in the
dataset file for those formats that support it. The only way to keep all
metadata in one file is by staying in R:

``` r

rio::export(codebook_data, "bfi.rds") # to R data structure file
```

The variable-level metadata can also be transferred to SPSS and Stata
files. Please note that this export is based on reverse-engineering the
SPSS and Stata file structure, so the resulting files should be tested
before sharing.

``` r

rio::export(codebook_data, "bfi.sav") # to SPSS file
rio::export(codebook_data, "bfi.dta") # to Stata file
```

## Releasing the codebook publicly

If you want to share your codebook with others, there is a codebook.html
file in the project folder you created at the start. You can email it to
collaborators or upload it to the OSF file storage. However, if you want
Google Dataset Search to index your dataset, this is not sufficient. The
OSF will not render your HTML files for security reasons and Google will
not index the content of your emails (at least not publicly). You need
to post your codebook online. If you are familiar with Github or already
have your own website, uploading the html file to your own website
should be easy. The simplest way I found for publishing the HTML for the
codebook is as follows. First, rename the codebook.html to index.html.
Then create an account on netlify.com. Once you’re signed in, drag and
drop the folder containing the codebook to the Netlify web page (make
sure the folder does not contain anything you do not want to share, such
as raw data). Netlify will upload the files and create a random URL like
estranged-armadillo.netlify.com. You can change this to something more
meaningful, like bfi-study.netlify.com, in the settings. Next, visit the
URL to check that you can see the codebook. The last step is to publicly
share a link to the codebook so that search engines can discover it; for
instance, you could tweet the link with the hashtag \#codebook2—ideally
with a link from the repository where you are sharing the raw data or
the related study’s supplementary material. For instance, I added a link
to the bfi-study codebook on the OSF (<https://osf.io/k39bg/>), where I
had also shared the data. Depending on the speed of the search engine
crawler, the dataset should be findable on Google Dataset Search in
anywhere from three to 21 days.

## The Codebook

### Metadata

#### Description

**Dataset name**: 25 Personality items representing 5 factors

25 personality self report items taken from the International
Personality Item Pool (ipip.ori.org)\[…\]

Metadata for search engines

- **Temporal Coverage**: Spring 2010

- **Spatial Coverage**: Online

- **Citation**: Revelle, W., Wilt, J., & Rosenthal, A. (2010).
  Individual differences in cognition: New methods for examining the
  personality-cognition link. In A. Gruszka, G. Matthews, & B. Szymura
  (Eds.), Handbook of individual differences in cognition: Attention,
  memory, and executive control (pp. 27–49). New York, NY: Springer.

- **URL**: <https://CRAN.R-project.org/package=psych>

- **Identifier**: <https://dx.doi.org/10.17605/OSF.IO/K39BG>

- **Date published**: 2010-01-01

- **Creator**:

| name | value           |
|-----:|:----------------|
|    1 | William Revelle |

| name | value |
|:---|:---|
| keywords | A1R, A2, A3, A4, A5, C1, C2, C3, C4R, C5R, E1R, E2R, E3, E4, E5, N1R, N2R, N3R, N4R, N5R, O1, O2R, O3, O4, O5R, gender, education, age, extraversion, plasticity |

### Variables

#### A1R

Am indifferent to the feelings of others.

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
A1R](codebook_tutorial_files/figure-html/cb_codebook_data_A1R_distribution-115-1.png)

Distribution of values for A1R

16 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | n_value_labels | hist |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|---:|:---|
| A1R | Am indifferent to the feelings of others. | haven_labelled | 16 | 0.9942857 | 1 | 5 | 6 | 4.586566 | 1.407737 | 6 | ▁▂▁▃▃▁▇▇ |

| name                  | value |
|:----------------------|------:|
| Very Inaccurate       |     6 |
| Moderately Inaccurate |     5 |
| Slightly Inaccurate   |     4 |
| Slightly Accurate     |     3 |
| Moderately Accurate   |     2 |
| Very Accurate         |     1 |

Response choices {.table}

#### A2

Inquire about others’ well-being.

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
A2](codebook_tutorial_files/figure-html/cb_codebook_data_A2_distribution-125-1.png)

Distribution of values for A2

27 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | n_value_labels | hist |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|---:|:---|
| A2 | Inquire about others’ well-being. | haven_labelled | 27 | 0.9903571 | 1 | 5 | 6 | 4.80238 | 1.17202 | 6 | ▁▁▁▁▅▁▇▇ |

| name                  | value |
|:----------------------|------:|
| Very Inaccurate       |     1 |
| Moderately Inaccurate |     2 |
| Slightly Inaccurate   |     3 |
| Slightly Accurate     |     4 |
| Moderately Accurate   |     5 |
| Very Accurate         |     6 |

Response choices {.table}

#### A3

Know how to comfort others.

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
A3](codebook_tutorial_files/figure-html/cb_codebook_data_A3_distribution-138-1.png)

Distribution of values for A3

26 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | n_value_labels | hist |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|---:|:---|
| A3 | Know how to comfort others. | haven_labelled | 26 | 0.9907143 | 1 | 5 | 6 | 4.603821 | 1.301834 | 6 | ▁▂▁▂▅▁▇▆ |

| name                  | value |
|:----------------------|------:|
| Very Inaccurate       |     1 |
| Moderately Inaccurate |     2 |
| Slightly Inaccurate   |     3 |
| Slightly Accurate     |     4 |
| Moderately Accurate   |     5 |
| Very Accurate         |     6 |

Response choices {.table}

#### A4

Love children.

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
A4](codebook_tutorial_files/figure-html/cb_codebook_data_A4_distribution-151-1.png)

Distribution of values for A4

19 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | n_value_labels | hist |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|---:|:---|
| A4 | Love children. | haven_labelled | 19 | 0.9932143 | 1 | 5 | 6 | 4.699748 | 1.479633 | 6 | ▁▂▁▁▃▁▅▇ |

| name                  | value |
|:----------------------|------:|
| Very Inaccurate       |     1 |
| Moderately Inaccurate |     2 |
| Slightly Inaccurate   |     3 |
| Slightly Accurate     |     4 |
| Moderately Accurate   |     5 |
| Very Accurate         |     6 |

Response choices {.table}

#### A5

Make people feel at ease.

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
A5](codebook_tutorial_files/figure-html/cb_codebook_data_A5_distribution-164-1.png)

Distribution of values for A5

16 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | n_value_labels | hist |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|---:|:---|
| A5 | Make people feel at ease. | haven_labelled | 16 | 0.9942857 | 1 | 5 | 6 | 4.560345 | 1.258512 | 6 | ▁▂▁▂▅▁▇▆ |

| name                  | value |
|:----------------------|------:|
| Very Inaccurate       |     1 |
| Moderately Inaccurate |     2 |
| Slightly Inaccurate   |     3 |
| Slightly Accurate     |     4 |
| Moderately Accurate   |     5 |
| Very Accurate         |     6 |

Response choices {.table}

#### C1

Am exacting in my work.

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
C1](codebook_tutorial_files/figure-html/cb_codebook_data_C1_distribution-177-1.png)

Distribution of values for C1

21 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | n_value_labels | hist |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|---:|:---|
| C1 | Am exacting in my work. | haven_labelled | 21 | 0.9925 | 1 | 5 | 6 | 4.502339 | 1.241346 | 6 | ▁▁▁▂▅▁▇▅ |

| name                  | value |
|:----------------------|------:|
| Very Inaccurate       |     1 |
| Moderately Inaccurate |     2 |
| Slightly Inaccurate   |     3 |
| Slightly Accurate     |     4 |
| Moderately Accurate   |     5 |
| Very Accurate         |     6 |

Response choices {.table}

#### C2

Continue until everything is perfect.

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
C2](codebook_tutorial_files/figure-html/cb_codebook_data_C2_distribution-190-1.png)

Distribution of values for C2

24 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | n_value_labels | hist |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|---:|:---|
| C2 | Continue until everything is perfect. | haven_labelled | 24 | 0.9914286 | 1 | 5 | 6 | 4.369957 | 1.318347 | 6 | ▁▂▁▂▆▁▇▅ |

| name                  | value |
|:----------------------|------:|
| Very Inaccurate       |     1 |
| Moderately Inaccurate |     2 |
| Slightly Inaccurate   |     3 |
| Slightly Accurate     |     4 |
| Moderately Accurate   |     5 |
| Very Accurate         |     6 |

Response choices {.table}

#### C3

Do things according to a plan.

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
C3](codebook_tutorial_files/figure-html/cb_codebook_data_C3_distribution-203-1.png)

Distribution of values for C3

20 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | n_value_labels | hist |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|---:|:---|
| C3 | Do things according to a plan. | haven_labelled | 20 | 0.9928571 | 1 | 5 | 6 | 4.303957 | 1.288552 | 6 | ▁▂▁▂▆▁▇▅ |

| name                  | value |
|:----------------------|------:|
| Very Inaccurate       |     1 |
| Moderately Inaccurate |     2 |
| Slightly Inaccurate   |     3 |
| Slightly Accurate     |     4 |
| Moderately Accurate   |     5 |
| Very Accurate         |     6 |

Response choices {.table}

#### C4R

Do things in a half-way manner.

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
C4R](codebook_tutorial_files/figure-html/cb_codebook_data_C4R_distribution-216-1.png)

Distribution of values for C4R

26 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | n_value_labels | hist |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|---:|:---|
| C4R | Do things in a half-way manner. | haven_labelled | 26 | 0.9907143 | 1 | 5 | 6 | 4.446647 | 1.375118 | 6 | ▁▂▁▅▅▁▇▇ |

| name                  | value |
|:----------------------|------:|
| Very Inaccurate       |     6 |
| Moderately Inaccurate |     5 |
| Slightly Inaccurate   |     4 |
| Slightly Accurate     |     3 |
| Moderately Accurate   |     2 |
| Very Accurate         |     1 |

Response choices {.table}

#### C5R

Waste my time.

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
C5R](codebook_tutorial_files/figure-html/cb_codebook_data_C5R_distribution-229-1.png)

Distribution of values for C5R

16 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | n_value_labels | hist |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|---:|:---|
| C5R | Waste my time. | haven_labelled | 16 | 0.9942857 | 1 | 4 | 6 | 3.703305 | 1.628542 | 6 | ▃▆▁▇▅▁▇▆ |

| name                  | value |
|:----------------------|------:|
| Very Inaccurate       |     6 |
| Moderately Inaccurate |     5 |
| Slightly Inaccurate   |     4 |
| Slightly Accurate     |     3 |
| Moderately Accurate   |     2 |
| Very Accurate         |     1 |

Response choices {.table}

#### N1R

Get angry easily.

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
N1R](codebook_tutorial_files/figure-html/cb_codebook_data_N1R_distribution-242-1.png)

Distribution of values for N1R

22 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | n_value_labels | hist |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|---:|:---|
| N1R | Get angry easily. | haven_labelled | 22 | 0.9921429 | 1 | 4 | 6 | 4.070914 | 1.570917 | 6 | ▂▅▁▆▅▁▇▇ |

| name                  | value |
|:----------------------|------:|
| Very Inaccurate       |     6 |
| Moderately Inaccurate |     5 |
| Slightly Inaccurate   |     4 |
| Slightly Accurate     |     3 |
| Moderately Accurate   |     2 |
| Very Accurate         |     1 |

Response choices {.table}

#### N2R

Get irritated easily.

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
N2R](codebook_tutorial_files/figure-html/cb_codebook_data_N2R_distribution-255-1.png)

Distribution of values for N2R

21 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | n_value_labels | hist |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|---:|:---|
| N2R | Get irritated easily. | haven_labelled | 21 | 0.9925 | 1 | 3 | 6 | 3.492263 | 1.525944 | 6 | ▃▆▁▇▅▁▆▃ |

| name                  | value |
|:----------------------|------:|
| Very Inaccurate       |     6 |
| Moderately Inaccurate |     5 |
| Slightly Inaccurate   |     4 |
| Slightly Accurate     |     3 |
| Moderately Accurate   |     2 |
| Very Accurate         |     1 |

Response choices {.table}

#### N3R

Have frequent mood swings.

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
N3R](codebook_tutorial_files/figure-html/cb_codebook_data_N3R_distribution-268-1.png)

Distribution of values for N3R

11 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | n_value_labels | hist |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|---:|:---|
| N3R | Have frequent mood swings. | haven_labelled | 11 | 0.9960714 | 1 | 4 | 6 | 3.783435 | 1.602902 | 6 | ▃▆▁▇▅▁▇▆ |

| name                  | value |
|:----------------------|------:|
| Very Inaccurate       |     6 |
| Moderately Inaccurate |     5 |
| Slightly Inaccurate   |     4 |
| Slightly Accurate     |     3 |
| Moderately Accurate   |     2 |
| Very Accurate         |     1 |

Response choices {.table}

#### N4R

Often feel blue.

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
N4R](codebook_tutorial_files/figure-html/cb_codebook_data_N4R_distribution-281-1.png)

Distribution of values for N4R

36 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | n_value_labels | hist |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|---:|:---|
| N4R | Often feel blue. | haven_labelled | 36 | 0.9871429 | 1 | 4 | 6 | 3.814399 | 1.569685 | 6 | ▃▅▁▇▅▁▇▆ |

| name                  | value |
|:----------------------|------:|
| Very Inaccurate       |     6 |
| Moderately Inaccurate |     5 |
| Slightly Inaccurate   |     4 |
| Slightly Accurate     |     3 |
| Moderately Accurate   |     2 |
| Very Accurate         |     1 |

Response choices {.table}

#### N5R

Panic easily.

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
N5R](codebook_tutorial_files/figure-html/cb_codebook_data_N5R_distribution-294-1.png)

Distribution of values for N5R

29 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | n_value_labels | hist |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|---:|:---|
| N5R | Panic easily. | haven_labelled | 29 | 0.9896429 | 1 | 4 | 6 | 4.030314 | 1.618647 | 6 | ▃▃▁▆▅▁▇▇ |

| name                  | value |
|:----------------------|------:|
| Very Inaccurate       |     6 |
| Moderately Inaccurate |     5 |
| Slightly Inaccurate   |     4 |
| Slightly Accurate     |     3 |
| Moderately Accurate   |     2 |
| Very Accurate         |     1 |

Response choices {.table}

#### gender

gender

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
gender](codebook_tutorial_files/figure-html/cb_codebook_data_gender_distribution-307-1.png)

Distribution of values for gender

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | n_value_labels | hist |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|---:|:---|
| gender | gender | haven_labelled | 0 | 1 | 1 | 2 | 2 | 1.671786 | 0.4696471 | 2 | ▃▁▁▁▁▁▁▇ |

| name   | value |
|:-------|------:|
| male   |     1 |
| female |     2 |

Response choices {.table}

#### education

education

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
education](codebook_tutorial_files/figure-html/cb_codebook_data_education_distribution-320-1.png)

Distribution of values for education

223 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | n_value_labels | hist |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|---:|:---|
| education | education | haven_labelled | 223 | 0.9203571 | 1 | 3 | 5 | 3.190144 | 1.107714 | 5 | ▂▂▁▇▁▂▁▃ |

| name                 | value |
|:---------------------|------:|
| in high school       |     1 |
| finished high school |     2 |
| some college         |     3 |
| college graduate     |     4 |
| graduate degree      |     5 |

Response choices {.table}

#### age

age

- Distribution
- Summary statistics

![Distribution of values for
age](codebook_tutorial_files/figure-html/cb_codebook_data_age_distribution-333-1.png)

Distribution of values for age

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|
| age | age | numeric | 0 | 1 | 3 | 26 | 86 | 28.78214 | 11.12755 | ▃▇▂▁▁ |

#### Scale: extraversion

- Overview
- Reliability details
- Summary statistics

**Reliability**: Cronbach’s α \[95% CI\] = 0.76 \[0.75;0.78\].

**Missing**: 87.

    ## Warning: The `size` argument of `element_rect()` is deprecated as of ggplot2 3.4.0.
    ## ℹ Please use the `linewidth` argument instead.
    ## ℹ The deprecated feature was likely used in the likert package.
    ##   Please report the issue at <https://github.com/jbryer/likert/issues>.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![Likert plot of scale extraversion
items](codebook_tutorial_files/figure-html/cb_codebook_data_extraversion_likert-1.png)

Likert plot of scale extraversion items

![Distribution of scale
extraversion](codebook_tutorial_files/figure-html/cb_codebook_data_extraversion_distribution-1.png)

Distribution of scale extraversion

###### Reliability

###### 95% Confidence Interval

|     lower |  estimate |     upper |
|----------:|----------:|----------:|
| 0.7479598 | 0.7617328 | 0.7755058 |

|  | raw_alpha | std.alpha | G6(smc) | average_r | S/N | ase | mean | sd | median_r |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
|  | 0.7617328 | 0.7616173 | 0.7263847 | 0.3898671 | 3.194935 | 0.007027 | 4.145083 | 1.060904 | 0.3815753 |

###### Reliability if an item is dropped:

|  | raw_alpha | std.alpha | G6(smc) | average_r | S/N | alpha se | var.r | med.r |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| E1R | 0.7256547 | 0.7253372 | 0.6729456 | 0.3976655 | 2.640829 | 0.0083696 | 0.0043606 | 0.3815753 |
| E2R | 0.6901804 | 0.6928566 | 0.6339565 | 0.3605943 | 2.255809 | 0.0095088 | 0.0028007 | 0.3542175 |
| E3 | 0.7279142 | 0.7260634 | 0.6734946 | 0.3985396 | 2.650479 | 0.0082409 | 0.0070587 | 0.3962064 |
| E4 | 0.7018885 | 0.7030829 | 0.6462562 | 0.3718537 | 2.367943 | 0.0090732 | 0.0032746 | 0.3769133 |
| E5 | 0.7436327 | 0.7438966 | 0.6910347 | 0.4206822 | 2.904673 | 0.0078236 | 0.0043322 | 0.4187921 |

###### Item statistics

|     |    n |     raw.r |     std.r |     r.cor |    r.drop |     mean |       sd |
|:----|-----:|----------:|----------:|----------:|----------:|---------:|---------:|
| E1R | 2777 | 0.7238448 | 0.7023877 | 0.5877615 | 0.5162729 | 4.025567 | 1.631506 |
| E2R | 2784 | 0.7797002 | 0.7645645 | 0.6933997 | 0.6053688 | 3.858118 | 1.605210 |
| E3  | 2775 | 0.6829944 | 0.7009216 | 0.5824719 | 0.5045725 | 4.000721 | 1.352719 |
| E4  | 2791 | 0.7466939 | 0.7456799 | 0.6621192 | 0.5779940 | 4.422429 | 1.457517 |
| E5  | 2779 | 0.6432360 | 0.6637835 | 0.5231481 | 0.4542446 | 4.416337 | 1.334768 |

###### Non missing response frequency for each item

|     |         1 |         2 |         3 |         4 |         5 |         6 |      miss |
|:----|----------:|----------:|----------:|----------:|----------:|----------:|----------:|
| E1R | 0.0867843 | 0.1321570 | 0.1620454 | 0.1454807 | 0.2347857 | 0.2387468 | 0.0082143 |
| E2R | 0.0912356 | 0.1382902 | 0.2151580 | 0.1232040 | 0.2406609 | 0.1914511 | 0.0057143 |
| E3  | 0.0536937 | 0.1055856 | 0.1484685 | 0.2976577 | 0.2677477 | 0.1268468 | 0.0089286 |
| E4  | 0.0501612 | 0.0938732 | 0.0970978 | 0.1612325 | 0.3375134 | 0.2601218 | 0.0032143 |
| E5  | 0.0341850 | 0.0795250 | 0.1036344 | 0.2227420 | 0.3382512 | 0.2216625 | 0.0075000 |

[TABLE]

#### Scale: plasticity

- Overview
- Reliability details
- Summary statistics

**Reliability**: Cronbach’s α \[95% CI\] = 0.71 \[0.69;0.72\].

**Missing**: 149.

![Likert plot of scale plasticity
items](codebook_tutorial_files/figure-html/cb_codebook_data_plasticity_likert-95-1.png)

Likert plot of scale plasticity items

![Distribution of scale
plasticity](codebook_tutorial_files/figure-html/cb_codebook_data_plasticity_distribution-96-1.png)

Distribution of scale plasticity

###### Reliability

###### 95% Confidence Interval

|     lower |  estimate |     upper |
|----------:|----------:|----------:|
| 0.6923339 | 0.7082754 | 0.7242169 |

|  | raw_alpha | std.alpha | G6(smc) | average_r | S/N | ase | mean | sd | median_r |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
|  | 0.7082754 | 0.7112686 | 0.7379505 | 0.1976525 | 2.463427 | 0.0081334 | 4.366425 | 0.7321724 | 0.2072893 |

###### Reliability if an item is dropped:

|  | raw_alpha | std.alpha | G6(smc) | average_r | S/N | alpha se | var.r | med.r |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| E1R | 0.6774042 | 0.6846206 | 0.7103516 | 0.1943269 | 2.170784 | 0.0091253 | 0.0263123 | 0.2008970 |
| E2R | 0.6667417 | 0.6764014 | 0.6955956 | 0.1884762 | 2.090248 | 0.0094378 | 0.0222839 | 0.2008970 |
| E3 | 0.6589898 | 0.6620819 | 0.6917836 | 0.1787795 | 1.959297 | 0.0095988 | 0.0269164 | 0.1867780 |
| E4 | 0.6806731 | 0.6882334 | 0.7061214 | 0.1969683 | 2.207527 | 0.0090061 | 0.0216484 | 0.2041198 |
| E5 | 0.6706394 | 0.6744523 | 0.7060333 | 0.1871201 | 2.071746 | 0.0092745 | 0.0289699 | 0.1867780 |
| O1 | 0.6807491 | 0.6788495 | 0.7080033 | 0.1901963 | 2.113805 | 0.0089073 | 0.0315324 | 0.2008970 |
| O2R | 0.7165655 | 0.7136577 | 0.7351111 | 0.2168686 | 2.492324 | 0.0078475 | 0.0287371 | 0.2233081 |
| O3 | 0.6629729 | 0.6602215 | 0.6904745 | 0.1775635 | 1.943094 | 0.0093925 | 0.0305469 | 0.1479330 |
| O4 | 0.7321698 | 0.7378481 | 0.7529652 | 0.2382295 | 2.814582 | 0.0075794 | 0.0213805 | 0.2502750 |
| O5R | 0.7004049 | 0.7026973 | 0.7253108 | 0.2079958 | 2.363576 | 0.0083192 | 0.0300768 | 0.2157975 |

###### Item statistics

|     |    n |     raw.r |     std.r |     r.cor |    r.drop |     mean |       sd |
|:----|-----:|----------:|----------:|----------:|----------:|---------:|---------:|
| E1R | 2777 | 0.5903881 | 0.5498606 | 0.4830977 | 0.4146923 | 4.025567 | 1.631506 |
| E2R | 2784 | 0.6300792 | 0.5898162 | 0.5550688 | 0.4669138 | 3.858118 | 1.605210 |
| E3  | 2775 | 0.6526322 | 0.6560372 | 0.6192059 | 0.5259427 | 4.000721 | 1.352719 |
| E4  | 2791 | 0.5581572 | 0.5318226 | 0.4812451 | 0.3965383 | 4.422429 | 1.457517 |
| E5  | 2779 | 0.5975760 | 0.5990775 | 0.5334137 | 0.4597986 | 4.416337 | 1.334768 |
| O1  | 2778 | 0.5380987 | 0.5780694 | 0.5067361 | 0.4142088 | 4.816055 | 1.129530 |
| O2R | 2800 | 0.4047437 | 0.3959198 | 0.2744119 | 0.2041208 | 4.286786 | 1.565152 |
| O3  | 2772 | 0.6371895 | 0.6643414 | 0.6241240 | 0.5218659 | 4.438312 | 1.220901 |
| O4  | 2786 | 0.2084032 | 0.2500424 | 0.0968310 | 0.0414646 | 4.892319 | 1.221250 |
| O5R | 2780 | 0.4399612 | 0.4565139 | 0.3542983 | 0.2762298 | 4.510432 | 1.327959 |

###### Non missing response frequency for each item

|     |         1 |         2 |         3 |         4 |         5 |         6 |      miss |
|:----|----------:|----------:|----------:|----------:|----------:|----------:|----------:|
| E1R | 0.0867843 | 0.1321570 | 0.1620454 | 0.1454807 | 0.2347857 | 0.2387468 | 0.0082143 |
| E2R | 0.0912356 | 0.1382902 | 0.2151580 | 0.1232040 | 0.2406609 | 0.1914511 | 0.0057143 |
| E3  | 0.0536937 | 0.1055856 | 0.1484685 | 0.2976577 | 0.2677477 | 0.1268468 | 0.0089286 |
| E4  | 0.0501612 | 0.0938732 | 0.0970978 | 0.1612325 | 0.3375134 | 0.2601218 | 0.0032143 |
| E5  | 0.0341850 | 0.0795250 | 0.1036344 | 0.2227420 | 0.3382512 | 0.2216625 | 0.0075000 |
| O1  | 0.0079194 | 0.0370770 | 0.0755940 | 0.2181425 | 0.3329734 | 0.3282937 | 0.0078571 |
| O2R | 0.0639286 | 0.0985714 | 0.1553571 | 0.1385714 | 0.2560714 | 0.2875000 | 0.0000000 |
| O3  | 0.0274170 | 0.0523088 | 0.1053391 | 0.2795815 | 0.3401876 | 0.1951659 | 0.0100000 |
| O4  | 0.0197416 | 0.0448672 | 0.0552764 | 0.1726490 | 0.3183776 | 0.3890883 | 0.0050000 |
| O5R | 0.0251799 | 0.0687050 | 0.1309353 | 0.1892086 | 0.3176259 | 0.2683453 | 0.0071429 |

[TABLE]

### Missingness report

### Codebook table

JSON-LD metadata

The following JSON-LD can be found by search engines, if you share this
codebook publicly on the web.

``` json
{
  "name": "25 Personality items representing 5 factors",
  "description": "25 personality self report items taken from the International Personality Item Pool (ipip.ori.org)[...]\n\n\n## Table of variables\nThis table contains variable names, labels, and number of missing values.\nSee the complete codebook for more.\n\n|name         |label                                     | n_missing|\n|:------------|:-----------------------------------------|---------:|\n|A1R          |Am indifferent to the feelings of others. |        16|\n|A2           |Inquire about others' well-being.         |        27|\n|A3           |Know how to comfort others.               |        26|\n|A4           |Love children.                            |        19|\n|A5           |Make people feel at ease.                 |        16|\n|C1           |Am exacting in my work.                   |        21|\n|C2           |Continue until everything is perfect.     |        24|\n|C3           |Do things according to a plan.            |        20|\n|C4R          |Do things in a half-way manner.           |        26|\n|C5R          |Waste my time.                            |        16|\n|E1R          |Don't talk a lot.                         |        23|\n|E2R          |Find it difficult to approach others.     |        16|\n|E3           |Know how to captivate people.             |        25|\n|E4           |Make friends easily.                      |         9|\n|E5           |Take charge.                              |        21|\n|N1R          |Get angry easily.                         |        22|\n|N2R          |Get irritated easily.                     |        21|\n|N3R          |Have frequent mood swings.                |        11|\n|N4R          |Often feel blue.                          |        36|\n|N5R          |Panic easily.                             |        29|\n|O1           |Am full of ideas.                         |        22|\n|O2R          |Avoid difficult reading material.         |         0|\n|O3           |Carry the conversation to a higher level. |        28|\n|O4           |Spend time reflecting on things.          |        14|\n|O5R          |Will not probe deeply into a subject.     |        20|\n|gender       |gender                                    |         0|\n|education    |education                                 |       223|\n|age          |age                                       |         0|\n|extraversion |5 E items aggregated by rowMeans          |        87|\n|plasticity   |10  items aggregated by rowMeans          |       149|\n\n### Note\nThis dataset was automatically described using the [codebook R package](https://rubenarslan.github.io/codebook/) (version 0.10.0).",
  "identifier": "https://dx.doi.org/10.17605/OSF.IO/K39BG",
  "creator": "William Revelle",
  "citation": "Revelle, W., Wilt, J., & Rosenthal, A. (2010). Individual differences in cognition: New methods for examining the personality-cognition link. In A. Gruszka, G. Matthews, & B. Szymura (Eds.), Handbook of individual differences in cognition: Attention, memory, and executive control (pp. 27–49). New York, NY: Springer.",
  "url": "https://CRAN.R-project.org/package=psych",
  "datePublished": "2010-01-01",
  "temporalCoverage": "Spring 2010",
  "spatialCoverage": "Online",
  "keywords": ["A1R", "A2", "A3", "A4", "A5", "C1", "C2", "C3", "C4R", "C5R", "E1R", "E2R", "E3", "E4", "E5", "N1R", "N2R", "N3R", "N4R", "N5R", "O1", "O2R", "O3", "O4", "O5R", "gender", "education", "age", "extraversion", "plasticity"],
  "@context": "https://schema.org/",
  "@type": "Dataset",
  "variableMeasured": [
    {
      "name": "A1R",
      "description": "Am indifferent to the feelings of others.",
      "value": "6. Very Inaccurate,\n5. Moderately Inaccurate,\n4. Slightly Inaccurate,\n3. Slightly Accurate,\n2. Moderately Accurate,\n1. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "A2",
      "description": "Inquire about others' well-being.",
      "value": "1. Very Inaccurate,\n2. Moderately Inaccurate,\n3. Slightly Inaccurate,\n4. Slightly Accurate,\n5. Moderately Accurate,\n6. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "A3",
      "description": "Know how to comfort others.",
      "value": "1. Very Inaccurate,\n2. Moderately Inaccurate,\n3. Slightly Inaccurate,\n4. Slightly Accurate,\n5. Moderately Accurate,\n6. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "A4",
      "description": "Love children.",
      "value": "1. Very Inaccurate,\n2. Moderately Inaccurate,\n3. Slightly Inaccurate,\n4. Slightly Accurate,\n5. Moderately Accurate,\n6. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "A5",
      "description": "Make people feel at ease.",
      "value": "1. Very Inaccurate,\n2. Moderately Inaccurate,\n3. Slightly Inaccurate,\n4. Slightly Accurate,\n5. Moderately Accurate,\n6. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "C1",
      "description": "Am exacting in my work.",
      "value": "1. Very Inaccurate,\n2. Moderately Inaccurate,\n3. Slightly Inaccurate,\n4. Slightly Accurate,\n5. Moderately Accurate,\n6. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "C2",
      "description": "Continue until everything is perfect.",
      "value": "1. Very Inaccurate,\n2. Moderately Inaccurate,\n3. Slightly Inaccurate,\n4. Slightly Accurate,\n5. Moderately Accurate,\n6. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "C3",
      "description": "Do things according to a plan.",
      "value": "1. Very Inaccurate,\n2. Moderately Inaccurate,\n3. Slightly Inaccurate,\n4. Slightly Accurate,\n5. Moderately Accurate,\n6. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "C4R",
      "description": "Do things in a half-way manner.",
      "value": "6. Very Inaccurate,\n5. Moderately Inaccurate,\n4. Slightly Inaccurate,\n3. Slightly Accurate,\n2. Moderately Accurate,\n1. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "C5R",
      "description": "Waste my time.",
      "value": "6. Very Inaccurate,\n5. Moderately Inaccurate,\n4. Slightly Inaccurate,\n3. Slightly Accurate,\n2. Moderately Accurate,\n1. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "E1R",
      "description": "Don't talk a lot.",
      "value": "6. Very Inaccurate,\n5. Moderately Inaccurate,\n4. Slightly Inaccurate,\n3. Slightly Accurate,\n2. Moderately Accurate,\n1. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "E2R",
      "description": "Find it difficult to approach others.",
      "value": "6. Very Inaccurate,\n5. Moderately Inaccurate,\n4. Slightly Inaccurate,\n3. Slightly Accurate,\n2. Moderately Accurate,\n1. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "E3",
      "description": "Know how to captivate people.",
      "value": "1. Very Inaccurate,\n2. Moderately Inaccurate,\n3. Slightly Inaccurate,\n4. Slightly Accurate,\n5. Moderately Accurate,\n6. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "E4",
      "description": "Make friends easily.",
      "value": "1. Very Inaccurate,\n2. Moderately Inaccurate,\n3. Slightly Inaccurate,\n4. Slightly Accurate,\n5. Moderately Accurate,\n6. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "E5",
      "description": "Take charge.",
      "value": "1. Very Inaccurate,\n2. Moderately Inaccurate,\n3. Slightly Inaccurate,\n4. Slightly Accurate,\n5. Moderately Accurate,\n6. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "N1R",
      "description": "Get angry easily.",
      "value": "6. Very Inaccurate,\n5. Moderately Inaccurate,\n4. Slightly Inaccurate,\n3. Slightly Accurate,\n2. Moderately Accurate,\n1. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "N2R",
      "description": "Get irritated easily.",
      "value": "6. Very Inaccurate,\n5. Moderately Inaccurate,\n4. Slightly Inaccurate,\n3. Slightly Accurate,\n2. Moderately Accurate,\n1. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "N3R",
      "description": "Have frequent mood swings.",
      "value": "6. Very Inaccurate,\n5. Moderately Inaccurate,\n4. Slightly Inaccurate,\n3. Slightly Accurate,\n2. Moderately Accurate,\n1. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "N4R",
      "description": "Often feel blue.",
      "value": "6. Very Inaccurate,\n5. Moderately Inaccurate,\n4. Slightly Inaccurate,\n3. Slightly Accurate,\n2. Moderately Accurate,\n1. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "N5R",
      "description": "Panic easily.",
      "value": "6. Very Inaccurate,\n5. Moderately Inaccurate,\n4. Slightly Inaccurate,\n3. Slightly Accurate,\n2. Moderately Accurate,\n1. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "O1",
      "description": "Am full of ideas.",
      "value": "1. Very Inaccurate,\n2. Moderately Inaccurate,\n3. Slightly Inaccurate,\n4. Slightly Accurate,\n5. Moderately Accurate,\n6. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "O2R",
      "description": "Avoid difficult reading material.",
      "value": "6. Very Inaccurate,\n5. Moderately Inaccurate,\n4. Slightly Inaccurate,\n3. Slightly Accurate,\n2. Moderately Accurate,\n1. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "O3",
      "description": "Carry the conversation to a higher level.",
      "value": "1. Very Inaccurate,\n2. Moderately Inaccurate,\n3. Slightly Inaccurate,\n4. Slightly Accurate,\n5. Moderately Accurate,\n6. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "O4",
      "description": "Spend time reflecting on things.",
      "value": "1. Very Inaccurate,\n2. Moderately Inaccurate,\n3. Slightly Inaccurate,\n4. Slightly Accurate,\n5. Moderately Accurate,\n6. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "O5R",
      "description": "Will not probe deeply into a subject.",
      "value": "6. Very Inaccurate,\n5. Moderately Inaccurate,\n4. Slightly Inaccurate,\n3. Slightly Accurate,\n2. Moderately Accurate,\n1. Very Accurate",
      "maxValue": 6,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "gender",
      "description": "gender",
      "value": "1. male,\n2. female",
      "maxValue": 2,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "education",
      "description": "education",
      "value": "1. in high school,\n2. finished high school,\n3. some college,\n4. college graduate,\n5. graduate degree",
      "maxValue": 5,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "age",
      "description": "age",
      "@type": "propertyValue"
    },
    {
      "name": "extraversion",
      "description": "5 E items aggregated by rowMeans",
      "@type": "propertyValue"
    },
    {
      "name": "plasticity",
      "description": "10  items aggregated by rowMeans",
      "@type": "propertyValue"
    }
  ]
}`
```
