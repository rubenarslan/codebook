# Codebook example with SPSS dataset

``` r

knit_by_pkgdown <- !is.null(knitr::opts_chunk$get("fig.retina"))
ggplot2::theme_set(ggplot2::theme_bw())
knitr::opts_chunk$set(warning = TRUE, message = TRUE, error = FALSE, echo = TRUE)
library(dplyr)
library(codebook)
```

In this vignette, you can see how to use the metadata that is often
already stored in SPSS and Stata files. It’s easy. All we need is the
[`rio::import`](http://gesistsa.github.io/rio/reference/import.md)
function. For files with the right file extension, we can automatically
pick the right way to import the data. Here, we’re downloading straight
from the Open Science Framework, so we have to specify the file
extension.

We select a subset of variables, just to keep it short. The data were
shared by Emanuel Jauk in a project called [How alluring are dark
personalities? The Dark Triad and attractiveness in speed
dating](https://osf.io/j4fcb/).

Often, files imported from SPSS or Stata to R will not have their
missings coded properly. Here, that is not the case, but if you find
yourself with such a dataset, the `detect_missing` function makes it
easy to recognise common ways to specify missing data (e.g. negative
values, labelled values, 99/999).

``` r

darktriad <- tryCatch(
  rio::import("https://osf.io/download/j4fcb", format = "sav"),
  error = function(e) NULL
)
if (is.null(darktriad)) {
  message(
    "Could not download dataset from https://osf.io/download/j4fcb\n",
    "The remote resource may be temporarily unavailable. Skipping this vignette."
  )
  knitr::knit_exit()
} else if (!knit_by_pkgdown) {
  darktriad <- darktriad %>%
  select(DG, sex, relStat, education, NPI_avg)
}
```

``` r

metadata(darktriad)$name <- "How alluring are dark personalities? The Dark Triad and attractiveness in speed dating"
metadata(darktriad)$description <- paste0("The data to this speed dating study comes in two different formats: Personwise (one record for each individual) and dyadic (pairwise; one record for each date). The respective SPSS files are named \"DarkTriadDate_person.sav\" and \"DarkTriadDate_dyad.sav\".

### Download link
[Open Science Framework](https://osf.io/download/j4fcb)

### Personwise datafile 
The personwise datafile contains individual differences variables and perceiver and target effects according to the social relations model. These are centered marginal means that were calculated according to the formulae provided by Kenny, Kashy, and Cook (2006). These effects are not (!) based on multilevel analyses.

### Preprocessing
All rating variables (i.e., actual choice, friendship, short-term relationship etc.) were corrected for prior acquaintance, which means that dates wih prior acquaintance were excluded (set to missing) on a dyadic basis.

Variables are labeled in SPSS. 

### A list of important abbreviations, prefixes and suffixes:

* _acq = acquaintance (i.e., variables with this suffix are controlled for prior * acquaintance)
* avg = average
* _rat = rating variable
* _z = z-standardized score
* BC = booty call
* DG = dating group (three groups in this study)
* FIPI = five item personality inventory
* FS = friendship
* FWB = friends-with-benefits
* Int = Intelligence
* Like = Likeability
* LTR = long-term relationship
* MACHIV = mach-iv machiavellianism questionnaire
* N, E, O, A, C = Big5
* NPI = narcissistic personality inventory
* ONS = one night stand
* P = perceiver
* PA = physical attractiveness
* PercEff = perceiver effect
* SD = speed dating
* SRM = social relations model
* SRP = self-report psychopathy scale
* STR = short-term relationship
* T = target
* TargEff = target effect


")
metadata(darktriad)$identifier <- "https://osf.io/jvk3u/"
metadata(darktriad)$datePublished <- "2015-10-07"
metadata(darktriad)$creator <- list(
      "@type" = "Person",
      givenName = "Emanuel", familyName = "Jauk",
      email = "emanuel.jauk@uni‐graz.at", 
      affiliation = list("@type" = "Organization",
        name = "Karl‐Franzens‐Universität Graz, Austria"))
metadata(darktriad)$citation <- "Jauk, E., Neubauer, A. C., Mairunteregger, T., Pemp, S., Sieber, K. P., & Rauthmann, J. F. (2016). How alluring are dark personalities? The Dark Triad and attractiveness in speed dating. European Journal of Personality, 30(2), 125-138."
metadata(darktriad)$url <- "https://osf.io/j4fcb/"
metadata(darktriad)$temporalCoverage <- "2015" 
metadata(darktriad)$spatialCoverage <- "Graz, Austria" 
metadata(darktriad)$distribution = list(
  list("@type" = "DataDownload",
       "requiresSubscription" = "https://schema.org/True",
       "encodingFormat" = "https://www.loc.gov/preservation/digital/formats/fdd/fdd000469.shtml",
       contentUrl = "https://osf.io/download/j4fcb")
)
```

``` r

# We don't want to look at the code in the codebook.
knitr::opts_chunk$set(warning = TRUE, message = TRUE, echo = FALSE)
```

Now, we can immediately generate a codebook.

### Metadata

#### Description

**Dataset name**: How alluring are dark personalities? The Dark Triad
and attractiveness in speed dating

The data to this speed dating study comes in two different formats:
Personwise (one record for each individual) and dyadic (pairwise; one
record for each date). The respective SPSS files are named
“DarkTriadDate_person.sav” and “DarkTriadDate_dyad.sav”.

### Download link

[Open Science Framework](https://osf.io/download/j4fcb)

### Personwise datafile

The personwise datafile contains individual differences variables and
perceiver and target effects according to the social relations model.
These are centered marginal means that were calculated according to the
formulae provided by Kenny, Kashy, and Cook (2006). These effects are
not (!) based on multilevel analyses.

### Preprocessing

All rating variables (i.e., actual choice, friendship, short-term
relationship etc.) were corrected for prior acquaintance, which means
that dates wih prior acquaintance were excluded (set to missing) on a
dyadic basis.

Variables are labeled in SPSS.

### A list of important abbreviations, prefixes and suffixes:

- \_acq = acquaintance (i.e., variables with this suffix are controlled
  for prior \* acquaintance)
- avg = average
- \_rat = rating variable
- \_z = z-standardized score
- BC = booty call
- DG = dating group (three groups in this study)
- FIPI = five item personality inventory
- FS = friendship
- FWB = friends-with-benefits
- Int = Intelligence
- Like = Likeability
- LTR = long-term relationship
- MACHIV = mach-iv machiavellianism questionnaire
- N, E, O, A, C = Big5
- NPI = narcissistic personality inventory
- ONS = one night stand
- P = perceiver
- PA = physical attractiveness
- PercEff = perceiver effect
- SD = speed dating
- SRM = social relations model
- SRP = self-report psychopathy scale
- STR = short-term relationship
- T = target
- TargEff = target effect

Metadata for search engines

- **Temporal Coverage**: 2015

- **Spatial Coverage**: Graz, Austria

- **Citation**: Jauk, E., Neubauer, A. C., Mairunteregger, T., Pemp, S.,
  Sieber, K. P., & Rauthmann, J. F. (2016). How alluring are dark
  personalities? The Dark Triad and attractiveness in speed dating.
  European Journal of Personality, 30(2), 125-138.

- **URL**: <https://osf.io/j4fcb/>

- **Identifier**: <https://osf.io/jvk3u/>

- **Date published**: 2015-10-07

- **Creator**:

| name | value |
|:---|:---|
| @type | Person |
| givenName | Emanuel |
| familyName | Jauk |
| email | <emanuel.jauk@uni>‐graz.at |
| affiliation | @type: Organization, name: Karl‐Franzens‐Universität Graz, Austria |

| name | value |
|:---|:---|
| distribution | : list(`@type` = “DataDownload”, requiresSubscription = “<https://schema.org/True>”, encodingFormat = “<https://www.loc.gov/preservation/digital/formats/fdd/fdd000469.shtml>”, contentUrl = “<https://osf.io/download/j4fcb>”) |
| keywords | SD_Code, DG, DG_size, DG_size_acq, age, sex, height, weight, relStat, relStat_other, education, contracept, date, NPI_avg, SRP_avg, MACHIV_avg, BFI_N_avg, BFI_E_avg, BFI_O_avg, BFI_A_avg, BFI_C_avg, PA_R1, PA_R2, PA_R3, PA_R4, PA_avg, BMI, SOI_R_B_avg, SOI_R_A_avg, SOI_R_D_avg, TargEff\_\_choice_relFrequ_acq, TargEff\_\_FS_avg_acq, TargEff\_\_ONS_avg_acq, TargEff\_\_BC_avg_acq, TargEff\_\_FWB_avg_acq, TargEff\_\_STR_avg_acq, TargEff\_\_LTR_avg_acq, TargEff\_\_PA_avg_acq, TargEff\_\_Like_avg_acq, TargEff\_\_Int_avg_acq, TargEff\_\_FIPI_N_avg_acq, TargEff\_\_FIPI_E_avg_acq, TargEff\_\_FIPI_O_avg_acq, TargEff\_\_FIPI_A_avg_acq, TargEff\_\_FIPI_C_avg_acq, PercEff\_\_choice_relFrequ_acq, PercEff\_\_FS_avg_acq, PercEff\_\_ONS_avg_acq, PercEff\_\_BC_avg_acq, PercEff\_\_FWB_avg_acq, PercEff\_\_STR_avg_acq, PercEff\_\_LTR_avg_acq, PercEff\_\_PA_avg_acq, PercEff\_\_Like_avg_acq, PercEff\_\_Int_avg_acq, PercEff\_\_FIPI_N_avg_acq, PercEff\_\_FIPI_E_avg_acq, PercEff\_\_FIPI_O_avg_acq, PercEff\_\_FIPI_A_avg_acq, PercEff\_\_FIPI_C_avg_acq |

## Variables

### SD_Code

speed dating code

- Distribution
- Summary statistics

![Distribution of values for
SD_Code](codebook_sav_files/figure-html/cb_darktriad_SD_Code_distribution-1.png)

Distribution of values for SD_Code

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|
| SD_Code | speed dating code | numeric | 0 | 1 | 101 | 146 | 246 | 172.6333 | 52.2639 | ▇▅▁▃▇ | F8.0 |

### DG

dating group

- Distribution
- Summary statistics

![Distribution of values for
DG](codebook_sav_files/figure-html/cb_darktriad_DG_distribution-36-1.png)

Distribution of values for DG

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|
| DG | dating group | numeric | 0 | 1 | 1 | 2 | 3 | 2.033333 | 0.7709618 | ▆▁▇▁▆ | F8.0 |

### DG_size

dating group size

- Distribution
- Summary statistics

![Distribution of values for
DG_size](codebook_sav_files/figure-html/cb_darktriad_DG_size_distribution-49-1.png)

Distribution of values for DG_size

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|
| DG_size | dating group size | numeric | 0 | 1 | 11 | 15 | 19 | 15.35556 | 2.88861 | ▃▆▃▁▇ | F8.0 |

### DG_size_acq

dating group size, corrected for prior acquaintance

- Distribution
- Summary statistics

![Distribution of values for
DG_size_acq](codebook_sav_files/figure-html/cb_darktriad_DG_size_acq_distribution-62-1.png)

Distribution of values for DG_size_acq

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| DG_size_acq | dating group size, corrected for prior acquaintance | numeric | 0 | 1 | 7 | 14 | 19 | 14.22222 | 2.936702 | ▁▆▇▆▇ | F8.0 | 11 |

### age

age

- Distribution
- Summary statistics

![Distribution of values for
age](codebook_sav_files/figure-html/cb_darktriad_age_distribution-75-1.png)

Distribution of values for age

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| age | age | numeric | 0 | 1 | 18 | 22 | 32 | 22.86667 | 3.094758 | ▅▇▅▂▁ | F3.0 | 5 |

### sex

sex

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
sex](codebook_sav_files/figure-html/cb_darktriad_sex_distribution-88-1.png)

Distribution of values for sex

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| sex | sex | numeric | 0 | 1 | 1 | 1 | 2 | 1.488889 | 0.502677 | ▇▁▁▁▇ | F1.0 | 5 |

| name   | value |
|:-------|------:|
| female |     1 |
| male   |     2 |

Response choices {.table}

### height

height

- Distribution
- Summary statistics

![Distribution of values for
height](codebook_sav_files/figure-html/cb_darktriad_height_distribution-101-1.png)

Distribution of values for height

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| height | height | numeric | 0 | 1 | 156 | 174 | 196 | 174.5444 | 9.26169 | ▃▇▇▃▃ | F3.0 | 5 |

### weight

weight

- Distribution
- Summary statistics

![Distribution of values for
weight](codebook_sav_files/figure-html/cb_darktriad_weight_distribution-114-1.png)

Distribution of values for weight

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| weight | weight | numeric | 0 | 1 | 46 | 70 | 106 | 68.91111 | 11.28452 | ▃▇▆▂▁ | F3.0 | 5 |

### relStat

relationship status

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
relStat](codebook_sav_files/figure-html/cb_darktriad_relStat_distribution-127-1.png)

Distribution of values for relStat

1 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| relStat | relationship status | numeric | 1 | 0.9888889 | 1 | 1 | 3 | 1.089888 | 0.3247534 | ▇▁▁▁▁ | F8.0 | 10 |

| name                         | value |
|:-----------------------------|------:|
| single                       |     1 |
| in a relationship            |     2 |
| living separately / divorced |     3 |

Response choices {.table}

### relStat_other

other relationship status

- Distribution
- Summary statistics

![Distribution of values for
relStat_other](codebook_sav_files/figure-html/cb_darktriad_relStat_other_distribution-140-1.png)

Distribution of values for relStat_other

0 missing values.

| name | label | data_type | n_missing | complete_rate | n_unique | empty | min | max | whitespace | format.spss | display_width |
|:---|:---|:---|---:|---:|---:|---:|:---|:---|---:|:---|:---|
| relStat_other | other relationship status | character | 0 | 1 | 2 | 89 | 0 | 25 | 0 | A234 | 5 |

### education

highest educational attainment

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
education](codebook_sav_files/figure-html/cb_darktriad_education_distribution-153-1.png)

Distribution of values for education

1 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| education | highest educational attainment | numeric | 1 | 0.9888889 | 4 | 4 | 5 | 4.168539 | 0.3764655 | ▇▁▁▁▂ | F1.0 | 5 |

| name                        | value |
|:----------------------------|------:|
| nine years schooling only   |     1 |
| professional training       |     2 |
| vocational school           |     3 |
| university-entrance diploma |     4 |
| academic degree             |     5 |

Response choices {.table}

### contracept

hormonal contraception

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
contracept](codebook_sav_files/figure-html/cb_darktriad_contracept_distribution-166-1.png)

Distribution of values for contracept

44 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| contracept | hormonal contraception | numeric | 44 | 0.5111111 | 1 | 2 | 2 | 1.652174 | 0.4815434 | ▅▁▁▁▇ | F1.0 | 5 |

| name | value |
|:-----|------:|
| yes  |     1 |
| no   |     2 |

Response choices {.table}

### date

past experience with speed dating

- Distribution
- Summary statistics
- Value labels

![Distribution of values for
date](codebook_sav_files/figure-html/cb_darktriad_date_distribution-179-1.png)

Distribution of values for date

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| date | past experience with speed dating | numeric | 0 | 1 | 1 | 2 | 2 | 1.9 | 0.3016807 | ▁▁▁▁▇ | F1.0 | 5 |

| name | value |
|:-----|------:|
| yes  |     1 |
| no   |     2 |

Response choices {.table}

### NPI_avg

narcissistic personality inventory - average

- Distribution
- Summary statistics

![Distribution of values for
NPI_avg](codebook_sav_files/figure-html/cb_darktriad_NPI_avg_distribution-192-1.png)

Distribution of values for NPI_avg

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| NPI_avg | narcissistic personality inventory - average | numeric | 0 | 1 | 1.7 | 2.6 | 3.6 | 2.611944 | 0.3468807 | ▁▅▇▂▁ | F8.2 | 10 |

### SRP_avg

self-report psychopathy scale - average

- Distribution
- Summary statistics

![Distribution of values for
SRP_avg](codebook_sav_files/figure-html/cb_darktriad_SRP_avg_distribution-205-1.png)

Distribution of values for SRP_avg

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| SRP_avg | self-report psychopathy scale - average | numeric | 0 | 1 | 1.3 | 2.1 | 3.5 | 2.061539 | 0.3653243 | ▅▇▅▁▁ | F8.2 | 10 |

### MACHIV_avg

mach-iv - average

- Distribution
- Summary statistics

![Distribution of values for
MACHIV_avg](codebook_sav_files/figure-html/cb_darktriad_MACHIV_avg_distribution-218-1.png)

Distribution of values for MACHIV_avg

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| MACHIV_avg | mach-iv - average | numeric | 0 | 1 | 1.4 | 2.7 | 4.8 | 2.749383 | 0.6652734 | ▂▇▆▂▁ | F8.2 | 12 |

### BFI_N_avg

big five inventory: neuroticism - average

- Distribution
- Summary statistics

![Distribution of values for
BFI_N_avg](codebook_sav_files/figure-html/cb_darktriad_BFI_N_avg_distribution-231-1.png)

Distribution of values for BFI_N_avg

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| BFI_N_avg | big five inventory: neuroticism - average | numeric | 0 | 1 | 1 | 2.8 | 4.8 | 2.794444 | 0.934346 | ▅▆▇▅▂ | F8.2 | 11 |

### BFI_E_avg

big five inventory: extraversion - average

- Distribution
- Summary statistics

![Distribution of values for
BFI_E_avg](codebook_sav_files/figure-html/cb_darktriad_BFI_E_avg_distribution-244-1.png)

Distribution of values for BFI_E_avg

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| BFI_E_avg | big five inventory: extraversion - average | numeric | 0 | 1 | 1 | 3.8 | 5 | 3.722222 | 0.8032977 | ▁▁▃▇▅ | F8.2 | 11 |

### BFI_O_avg

big five inventory: openness - average

- Distribution
- Summary statistics

![Distribution of values for
BFI_O_avg](codebook_sav_files/figure-html/cb_darktriad_BFI_O_avg_distribution-257-1.png)

Distribution of values for BFI_O_avg

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| BFI_O_avg | big five inventory: openness - average | numeric | 0 | 1 | 1 | 4.2 | 5 | 4.072222 | 0.7599938 | ▁▁▂▅▇ | F8.2 | 11 |

### BFI_A_avg

big five inventory: agreeableness - average

- Distribution
- Summary statistics

![Distribution of values for
BFI_A_avg](codebook_sav_files/figure-html/cb_darktriad_BFI_A_avg_distribution-270-1.png)

Distribution of values for BFI_A_avg

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| BFI_A_avg | big five inventory: agreeableness - average | numeric | 0 | 1 | 1.5 | 3.2 | 4.8 | 3.197222 | 0.6945761 | ▂▅▃▇▁ | F8.2 | 11 |

### BFI_C_avg

big five inventory: conscientiousness - average

- Distribution
- Summary statistics

![Distribution of values for
BFI_C_avg](codebook_sav_files/figure-html/cb_darktriad_BFI_C_avg_distribution-283-1.png)

Distribution of values for BFI_C_avg

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| BFI_C_avg | big five inventory: conscientiousness - average | numeric | 0 | 1 | 1.8 | 3.5 | 4.8 | 3.352778 | 0.7651966 | ▅▃▆▇▃ | F8.2 | 11 |

### PA_R1

physical attractiveness - rater1(f)

- Distribution
- Summary statistics

![Distribution of values for
PA_R1](codebook_sav_files/figure-html/cb_darktriad_PA_R1_distribution-296-1.png)

Distribution of values for PA_R1

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| PA_R1 | physical attractiveness - rater1(f) | numeric | 0 | 1 | 1 | 4 | 7 | 3.633333 | 1.126294 | ▂▆▇▂▁ | F8.2 | 14 |

### PA_R2

physical attractiveness - rater2(f)

- Distribution
- Summary statistics

![Distribution of values for
PA_R2](codebook_sav_files/figure-html/cb_darktriad_PA_R2_distribution-309-1.png)

Distribution of values for PA_R2

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|
| PA_R2 | physical attractiveness - rater2(f) | numeric | 0 | 1 | 2 | 5 | 7 | 4.7 | 1.22199 | ▅▅▇▃▂ | F8.2 |

### PA_R3

physical attractiveness - rater3(m)

- Distribution
- Summary statistics

![Distribution of values for
PA_R3](codebook_sav_files/figure-html/cb_darktriad_PA_R3_distribution-322-1.png)

Distribution of values for PA_R3

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|
| PA_R3 | physical attractiveness - rater3(m) | numeric | 0 | 1 | 1 | 3 | 7 | 3.155556 | 1.505877 | ▇▆▅▃▂ | F8.2 |

### PA_R4

physical attractiveness - rater4(m)

- Distribution
- Summary statistics

![Distribution of values for
PA_R4](codebook_sav_files/figure-html/cb_darktriad_PA_R4_distribution-335-1.png)

Distribution of values for PA_R4

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|
| PA_R4 | physical attractiveness - rater4(m) | numeric | 0 | 1 | 1 | 4 | 7 | 4.144444 | 1.625368 | ▆▇▃▆▇ | F8.2 |

### PA_avg

physical attractiveness - average

- Distribution
- Summary statistics

![Distribution of values for
PA_avg](codebook_sav_files/figure-html/cb_darktriad_PA_avg_distribution-348-1.png)

Distribution of values for PA_avg

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| PA_avg | physical attractiveness - average | numeric | 0 | 1 | 1.8 | 3.8 | 6.5 | 3.908333 | 1.141929 | ▅▇▇▇▂ | F8.2 | 19 |

### BMI

body mass index

- Distribution
- Summary statistics

![Distribution of values for
BMI](codebook_sav_files/figure-html/cb_darktriad_BMI_distribution-361-1.png)

Distribution of values for BMI

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| BMI | body mass index | numeric | 0 | 1 | 18 | 23 | 31 | 22.54685 | 2.753195 | ▆▇▆▃▁ | F8.2 | 10 |

### SOI_R_B_avg

sociosexual orientation inventory revised: behavior - average

- Distribution
- Summary statistics

![Distribution of values for
SOI_R_B_avg](codebook_sav_files/figure-html/cb_darktriad_SOI_R_B_avg_distribution-374-1.png)

Distribution of values for SOI_R_B_avg

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| SOI_R_B_avg | sociosexual orientation inventory revised: behavior - average | numeric | 0 | 1 | 1 | 3 | 8.7 | 3.62963 | 2.111998 | ▇▅▃▂▂ | F8.2 | 16 |

### SOI_R_A_avg

sociosexual orientation inventory revised: attitude - average

- Distribution
- Summary statistics

![Distribution of values for
SOI_R_A_avg](codebook_sav_files/figure-html/cb_darktriad_SOI_R_A_avg_distribution-387-1.png)

Distribution of values for SOI_R_A_avg

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| SOI_R_A_avg | sociosexual orientation inventory revised: attitude - average | numeric | 0 | 1 | 1.7 | 6.8 | 9 | 6.481482 | 2.100539 | ▂▂▅▅▇ | F8.2 | 17 |

### SOI_R_D_avg

sociosexual orientation inventory revised: desire - average

- Distribution
- Summary statistics

![Distribution of values for
SOI_R_D_avg](codebook_sav_files/figure-html/cb_darktriad_SOI_R_D_avg_distribution-400-1.png)

Distribution of values for SOI_R_D_avg

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| SOI_R_D_avg | sociosexual orientation inventory revised: desire - average | numeric | 0 | 1 | 1 | 5.3 | 9 | 5.111111 | 2.092135 | ▃▅▆▇▃ | F8.2 | 15 |

### TargEff\_\_choice_relFrequ_acq {#TargEff\_\_choice_relFrequ_acq .tabset}

SRM target effect: actual choice

#### Distribution

![Distribution of values for
TargEff\_\_choice_relFrequ_acq](codebook_sav_files/figure-html/cb_darktriad_TargEff__choice_relFrequ_acq_distribution-413-1.png)

Distribution of values for TargEff\_\_choice_relFrequ_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| TargEff\_\_choice_relFrequ_acq | SRM target effect: actual choice | numeric | 0 | 1 | -0.39 | -0.0056 | 0.61 | 1.82e-05 | 0.257597 | ▇▇▇▃▂ | F8.2 | 34 |

### TargEff\_\_FS_avg_acq {#TargEff\_\_FS_avg_acq .tabset}

SRM target effect: friendship

#### Distribution

![Distribution of values for
TargEff\_\_FS_avg_acq](codebook_sav_files/figure-html/cb_darktriad_TargEff__FS_avg_acq_distribution-426-1.png)

Distribution of values for TargEff\_\_FS_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| TargEff\_\_FS_avg_acq | SRM target effect: friendship | numeric | 0 | 1 | -1.5 | -0.065 | 1.2 | 2.51e-05 | 0.6740996 | ▂▃▇▅▅ | F8.2 | 29 |

### TargEff\_\_ONS_avg_acq {#TargEff\_\_ONS_avg_acq .tabset}

SRM target effect: one night stand

#### Distribution

![Distribution of values for
TargEff\_\_ONS_avg_acq](codebook_sav_files/figure-html/cb_darktriad_TargEff__ONS_avg_acq_distribution-439-1.png)

Distribution of values for TargEff\_\_ONS_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| TargEff\_\_ONS_avg_acq | SRM target effect: one night stand | numeric | 0 | 1 | -2 | -0.22 | 3.1 | 2.23e-05 | 1.417459 | ▇▅▆▃▃ | F8.2 | 21 |

### TargEff\_\_BC_avg_acq {#TargEff\_\_BC_avg_acq .tabset}

SRM target effect: booty call

#### Distribution

![Distribution of values for
TargEff\_\_BC_avg_acq](codebook_sav_files/figure-html/cb_darktriad_TargEff__BC_avg_acq_distribution-452-1.png)

Distribution of values for TargEff\_\_BC_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| TargEff\_\_BC_avg_acq | SRM target effect: booty call | numeric | 0 | 1 | -1.6 | -0.13 | 3.3 | -4.1e-05 | 1.32644 | ▇▃▃▂▁ | F8.2 | 28 |

### TargEff\_\_FWB_avg_acq {#TargEff\_\_FWB_avg_acq .tabset}

SRM target effect: friends-with-benefits

#### Distribution

![Distribution of values for
TargEff\_\_FWB_avg_acq](codebook_sav_files/figure-html/cb_darktriad_TargEff__FWB_avg_acq_distribution-465-1.png)

Distribution of values for TargEff\_\_FWB_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| TargEff\_\_FWB_avg_acq | SRM target effect: friends-with-benefits | numeric | 0 | 1 | -1.6 | -0.16 | 3.2 | -2.81e-05 | 1.244604 | ▇▆▅▂▁ | F8.2 | 30 |

### TargEff\_\_STR_avg_acq {#TargEff\_\_STR_avg_acq .tabset}

SRM target effect: short-term relationship (aggregated)

#### Distribution

![Distribution of values for
TargEff\_\_STR_avg_acq](codebook_sav_files/figure-html/cb_darktriad_TargEff__STR_avg_acq_distribution-478-1.png)

Distribution of values for TargEff\_\_STR_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| TargEff\_\_STR_avg_acq | SRM target effect: short-term relationship (aggregated) | numeric | 0 | 1 | -1.7 | -0.23 | 3.2 | -1.56e-05 | 1.318697 | ▇▅▅▂▂ | F8.2 | 13 |

### TargEff\_\_LTR_avg_acq {#TargEff\_\_LTR_avg_acq .tabset}

SRM target effect: long-term relationship

#### Distribution

![Distribution of values for
TargEff\_\_LTR_avg_acq](codebook_sav_files/figure-html/cb_darktriad_TargEff__LTR_avg_acq_distribution-491-1.png)

Distribution of values for TargEff\_\_LTR_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| TargEff\_\_LTR_avg_acq | SRM target effect: long-term relationship | numeric | 0 | 1 | -1.4 | -0.066 | 3.3 | -1.73e-05 | 0.9444001 | ▇▇▅▁▁ | F8.2 | 27 |

### TargEff\_\_PA_avg_acq {#TargEff\_\_PA_avg_acq .tabset}

SRM target effect: physical attractiveness

#### Distribution

![Distribution of values for
TargEff\_\_PA_avg_acq](codebook_sav_files/figure-html/cb_darktriad_TargEff__PA_avg_acq_distribution-504-1.png)

Distribution of values for TargEff\_\_PA_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| TargEff\_\_PA_avg_acq | SRM target effect: physical attractiveness | numeric | 0 | 1 | -2.2 | 0.22 | 2.5 | -2.62e-05 | 1.177495 | ▅▅▇▆▂ | F8.2 | 28 |

### TargEff\_\_Like_avg_acq {#TargEff\_\_Like_avg_acq .tabset}

SRM target effect: likeability

#### Distribution

![Distribution of values for
TargEff\_\_Like_avg_acq](codebook_sav_files/figure-html/cb_darktriad_TargEff__Like_avg_acq_distribution-517-1.png)

Distribution of values for TargEff\_\_Like_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| TargEff\_\_Like_avg_acq | SRM target effect: likeability | numeric | 0 | 1 | -1.6 | 0.092 | 1.7 | 4.39e-05 | 0.7112554 | ▃▅▇▆▁ | F8.2 | 31 |

### TargEff\_\_Int_avg_acq {#TargEff\_\_Int_avg_acq .tabset}

SRM target effect: intelligence

#### Distribution

![Distribution of values for
TargEff\_\_Int_avg_acq](codebook_sav_files/figure-html/cb_darktriad_TargEff__Int_avg_acq_distribution-530-1.png)

Distribution of values for TargEff\_\_Int_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| TargEff\_\_Int_avg_acq | SRM target effect: intelligence | numeric | 0 | 1 | -1.2 | -0.0082 | 1 | 2.54e-05 | 0.4567633 | ▁▃▇▆▃ | F8.2 | 29 |

### TargEff\_\_FIPI_N_avg_acq {#TargEff\_\_FIPI_N_avg_acq .tabset}

SRM target effect: neuroticism

#### Distribution

![Distribution of values for
TargEff\_\_FIPI_N_avg_acq](codebook_sav_files/figure-html/cb_darktriad_TargEff__FIPI_N_avg_acq_distribution-543-1.png)

Distribution of values for TargEff\_\_FIPI_N_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| TargEff\_\_FIPI_N_avg_acq | SRM target effect: neuroticism | numeric | 0 | 1 | -1.7 | -0.093 | 1.8 | 1.61e-05 | 0.7246828 | ▂▇▆▆▂ | F8.2 | 28 |

### TargEff\_\_FIPI_E_avg_acq {#TargEff\_\_FIPI_E_avg_acq .tabset}

SRM target effect: extraversion

#### Distribution

![Distribution of values for
TargEff\_\_FIPI_E_avg_acq](codebook_sav_files/figure-html/cb_darktriad_TargEff__FIPI_E_avg_acq_distribution-556-1.png)

Distribution of values for TargEff\_\_FIPI_E_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| TargEff\_\_FIPI_E_avg_acq | SRM target effect: extraversion | numeric | 0 | 1 | -2.4 | 0.00083 | 1.7 | -1.38e-05 | 0.8477838 | ▁▂▇▇▃ | F8.2 | 28 |

### TargEff\_\_FIPI_O_avg_acq {#TargEff\_\_FIPI_O_avg_acq .tabset}

SRM target effect: openness

#### Distribution

![Distribution of values for
TargEff\_\_FIPI_O_avg_acq](codebook_sav_files/figure-html/cb_darktriad_TargEff__FIPI_O_avg_acq_distribution-569-1.png)

Distribution of values for TargEff\_\_FIPI_O_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| TargEff\_\_FIPI_O_avg_acq | SRM target effect: openness | numeric | 0 | 1 | -1.5 | -0.032 | 1.6 | 5.2e-06 | 0.6533151 | ▂▅▇▅▂ | F8.2 | 28 |

### TargEff\_\_FIPI_A_avg_acq {#TargEff\_\_FIPI_A_avg_acq .tabset}

SRM target effect: agreeableness

#### Distribution

![Distribution of values for
TargEff\_\_FIPI_A_avg_acq](codebook_sav_files/figure-html/cb_darktriad_TargEff__FIPI_A_avg_acq_distribution-582-1.png)

Distribution of values for TargEff\_\_FIPI_A_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| TargEff\_\_FIPI_A_avg_acq | SRM target effect: agreeableness | numeric | 0 | 1 | -1.6 | 0.076 | 1.3 | -4.46e-05 | 0.5824646 | ▂▃▇▇▂ | F8.2 | 31 |

### TargEff\_\_FIPI_C_avg_acq {#TargEff\_\_FIPI_C_avg_acq .tabset}

SRM target effect: conscientiousness

#### Distribution

![Distribution of values for
TargEff\_\_FIPI_C_avg_acq](codebook_sav_files/figure-html/cb_darktriad_TargEff__FIPI_C_avg_acq_distribution-595-1.png)

Distribution of values for TargEff\_\_FIPI_C_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| TargEff\_\_FIPI_C_avg_acq | SRM target effect: conscientiousness | numeric | 0 | 1 | -1.8 | 0.03 | 1.4 | -3.87e-05 | 0.5820808 | ▁▂▇▇▂ | F8.2 | 29 |

### PercEff\_\_choice_relFrequ_acq {#PercEff\_\_choice_relFrequ_acq .tabset}

SRM perceiver effect: actual choice

#### Distribution

![Distribution of values for
PercEff\_\_choice_relFrequ_acq](codebook_sav_files/figure-html/cb_darktriad_PercEff__choice_relFrequ_acq_distribution-608-1.png)

Distribution of values for PercEff\_\_choice_relFrequ_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| PercEff\_\_choice_relFrequ_acq | SRM perceiver effect: actual choice | numeric | 0 | 1 | -0.38 | -0.016 | 0.62 | 3.98e-05 | 0.2266172 | ▆▇▆▂▂ | F8.2 | 34 |

### PercEff\_\_FS_avg_acq {#PercEff\_\_FS_avg_acq .tabset}

SRM perceiver effect: friendship

#### Distribution

![Distribution of values for
PercEff\_\_FS_avg_acq](codebook_sav_files/figure-html/cb_darktriad_PercEff__FS_avg_acq_distribution-621-1.png)

Distribution of values for PercEff\_\_FS_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| PercEff\_\_FS_avg_acq | SRM perceiver effect: friendship | numeric | 0 | 1 | -2.7 | 0.096 | 2.5 | -3.72e-05 | 0.9577896 | ▁▃▇▆▁ | F8.2 | 29 |

### PercEff\_\_ONS_avg_acq {#PercEff\_\_ONS_avg_acq .tabset}

SRM perceiver effect: one night stand

#### Distribution

![Distribution of values for
PercEff\_\_ONS_avg_acq](codebook_sav_files/figure-html/cb_darktriad_PercEff__ONS_avg_acq_distribution-634-1.png)

Distribution of values for PercEff\_\_ONS_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| PercEff\_\_ONS_avg_acq | SRM perceiver effect: one night stand | numeric | 0 | 1 | -2 | -0.23 | 3 | 1.57e-05 | 1.377661 | ▇▆▅▅▂ | F8.2 | 21 |

### PercEff\_\_BC_avg_acq {#PercEff\_\_BC_avg_acq .tabset}

SRM perceiver effect: booty call

#### Distribution

![Distribution of values for
PercEff\_\_BC_avg_acq](codebook_sav_files/figure-html/cb_darktriad_PercEff__BC_avg_acq_distribution-647-1.png)

Distribution of values for PercEff\_\_BC_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| PercEff\_\_BC_avg_acq | SRM perceiver effect: booty call | numeric | 0 | 1 | -1.8 | -0.24 | 2.8 | -1.25e-05 | 1.28505 | ▇▆▃▅▃ | F8.2 | 28 |

### PercEff\_\_FWB_avg_acq {#PercEff\_\_FWB_avg_acq .tabset}

SRM perceiver effect: friends-with-benefits

#### Distribution

![Distribution of values for
PercEff\_\_FWB_avg_acq](codebook_sav_files/figure-html/cb_darktriad_PercEff__FWB_avg_acq_distribution-660-1.png)

Distribution of values for PercEff\_\_FWB_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| PercEff\_\_FWB_avg_acq | SRM perceiver effect: friends-with-benefits | numeric | 0 | 1 | -1.8 | -0.24 | 3 | -1.04e-05 | 1.265079 | ▇▇▅▃▂ | F8.2 | 30 |

### PercEff\_\_STR_avg_acq {#PercEff\_\_STR_avg_acq .tabset}

SRM perceiver effect: short-term relationship (aggregated)

#### Distribution

![Distribution of values for
PercEff\_\_STR_avg_acq](codebook_sav_files/figure-html/cb_darktriad_PercEff__STR_avg_acq_distribution-673-1.png)

Distribution of values for PercEff\_\_STR_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| PercEff\_\_STR_avg_acq | SRM perceiver effect: short-term relationship (aggregated) | numeric | 0 | 1 | -1.8 | -0.3 | 2.8 | -2.4e-06 | 1.259888 | ▇▆▃▅▂ | F8.2 | 14 |

### PercEff\_\_LTR_avg_acq {#PercEff\_\_LTR_avg_acq .tabset}

SRM perceiver effect: long-term relationship

#### Distribution

![Distribution of values for
PercEff\_\_LTR_avg_acq](codebook_sav_files/figure-html/cb_darktriad_PercEff__LTR_avg_acq_distribution-686-1.png)

Distribution of values for PercEff\_\_LTR_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| PercEff\_\_LTR_avg_acq | SRM perceiver effect: long-term relationship | numeric | 0 | 1 | -1.6 | -0.16 | 2.7 | -2e-06 | 1.084035 | ▇▆▆▃▂ | F8.2 | 27 |

### PercEff\_\_PA_avg_acq {#PercEff\_\_PA_avg_acq .tabset}

SRM perceiver effect: physical attractiveness

#### Distribution

![Distribution of values for
PercEff\_\_PA_avg_acq](codebook_sav_files/figure-html/cb_darktriad_PercEff__PA_avg_acq_distribution-699-1.png)

Distribution of values for PercEff\_\_PA_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| PercEff\_\_PA_avg_acq | SRM perceiver effect: physical attractiveness | numeric | 0 | 1 | -2.5 | 0.16 | 2.5 | -3.41e-05 | 1.176363 | ▃▅▇▇▂ | F8.2 | 28 |

### PercEff\_\_Like_avg_acq {#PercEff\_\_Like_avg_acq .tabset}

SRM perceiver effect: likeability

#### Distribution

![Distribution of values for
PercEff\_\_Like_avg_acq](codebook_sav_files/figure-html/cb_darktriad_PercEff__Like_avg_acq_distribution-712-1.png)

Distribution of values for PercEff\_\_Like_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| PercEff\_\_Like_avg_acq | SRM perceiver effect: likeability | numeric | 0 | 1 | -2.5 | 0.063 | 2.1 | 3.56e-05 | 0.9432916 | ▂▃▇▆▂ | F8.2 | 31 |

### PercEff\_\_Int_avg_acq {#PercEff\_\_Int_avg_acq .tabset}

SRM perceiver effect: intelligence

#### Distribution

![Distribution of values for
PercEff\_\_Int_avg_acq](codebook_sav_files/figure-html/cb_darktriad_PercEff__Int_avg_acq_distribution-725-1.png)

Distribution of values for PercEff\_\_Int_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| PercEff\_\_Int_avg_acq | SRM perceiver effect: intelligence | numeric | 0 | 1 | -2.4 | 0.043 | 2 | -3.88e-05 | 0.9692282 | ▂▃▇▆▂ | F8.2 | 29 |

### PercEff\_\_FIPI_N_avg_acq {#PercEff\_\_FIPI_N_avg_acq .tabset}

SRM perceiver effect: neuroticism

#### Distribution

![Distribution of values for
PercEff\_\_FIPI_N_avg_acq](codebook_sav_files/figure-html/cb_darktriad_PercEff__FIPI_N_avg_acq_distribution-738-1.png)

Distribution of values for PercEff\_\_FIPI_N_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| PercEff\_\_FIPI_N_avg_acq | SRM perceiver effect: neuroticism | numeric | 0 | 1 | -2.2 | 0.12 | 2.5 | 3.22e-05 | 0.8724864 | ▂▅▇▆▁ | F8.2 | 28 |

### PercEff\_\_FIPI_E_avg_acq {#PercEff\_\_FIPI_E_avg_acq .tabset}

SRM perceiver effect: extraversion

#### Distribution

![Distribution of values for
PercEff\_\_FIPI_E_avg_acq](codebook_sav_files/figure-html/cb_darktriad_PercEff__FIPI_E_avg_acq_distribution-751-1.png)

Distribution of values for PercEff\_\_FIPI_E_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| PercEff\_\_FIPI_E_avg_acq | SRM perceiver effect: extraversion | numeric | 0 | 1 | -1.6 | 0.12 | 1.7 | -4.35e-05 | 0.7303606 | ▃▆▇▇▁ | F8.2 | 28 |

### PercEff\_\_FIPI_O_avg_acq {#PercEff\_\_FIPI_O_avg_acq .tabset}

SRM perceiver effect: openness

#### Distribution

![Distribution of values for
PercEff\_\_FIPI_O_avg_acq](codebook_sav_files/figure-html/cb_darktriad_PercEff__FIPI_O_avg_acq_distribution-764-1.png)

Distribution of values for PercEff\_\_FIPI_O_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| PercEff\_\_FIPI_O_avg_acq | SRM perceiver effect: openness | numeric | 0 | 1 | -2.2 | 0.064 | 1.5 | -1.9e-06 | 0.6694323 | ▁▃▇▇▂ | F8.2 | 28 |

### PercEff\_\_FIPI_A_avg_acq {#PercEff\_\_FIPI_A_avg_acq .tabset}

SRM perceiver effect: agreeableness

#### Distribution

![Distribution of values for
PercEff\_\_FIPI_A_avg_acq](codebook_sav_files/figure-html/cb_darktriad_PercEff__FIPI_A_avg_acq_distribution-777-1.png)

Distribution of values for PercEff\_\_FIPI_A_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| PercEff\_\_FIPI_A_avg_acq | SRM perceiver effect: agreeableness | numeric | 0 | 1 | -2.2 | -0.046 | 1.7 | 3.97e-05 | 0.8562851 | ▁▅▇▆▃ | F8.2 | 31 |

### PercEff\_\_FIPI_C_avg_acq {#PercEff\_\_FIPI_C_avg_acq .tabset}

SRM perceiver effect: conscientiousness

#### Distribution

![Distribution of values for
PercEff\_\_FIPI_C_avg_acq](codebook_sav_files/figure-html/cb_darktriad_PercEff__FIPI_C_avg_acq_distribution-790-1.png)

Distribution of values for PercEff\_\_FIPI_C_avg_acq

0 missing values.

#### Summary statistics

| name | label | data_type | n_missing | complete_rate | min | median | max | mean | sd | hist | format.spss | display_width |
|:---|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|:---|:---|
| PercEff\_\_FIPI_C_avg_acq | SRM perceiver effect: conscientiousness | numeric | 0 | 1 | -2.2 | -0.0029 | 1.6 | 4.34e-05 | 0.7016471 | ▁▂▇▇▂ | F8.2 | 29 |

## Missingness report

## Codebook table

JSON-LD metadata

The following JSON-LD can be found by search engines, if you share this
codebook publicly on the web.

``` json
{
  "name": "How alluring are dark personalities? The Dark Triad and attractiveness in speed dating",
  "description": "The data to this speed dating study comes in two different formats: Personwise (one record for each individual) and dyadic (pairwise; one record for each date). The respective SPSS files are named \"DarkTriadDate_person.sav\" and \"DarkTriadDate_dyad.sav\".\n\n### Download link\n[Open Science Framework](https://osf.io/download/j4fcb)\n\n### Personwise datafile \nThe personwise datafile contains individual differences variables and perceiver and target effects according to the social relations model. These are centered marginal means that were calculated according to the formulae provided by Kenny, Kashy, and Cook (2006). These effects are not (!) based on multilevel analyses.\n\n### Preprocessing\nAll rating variables (i.e., actual choice, friendship, short-term relationship etc.) were corrected for prior acquaintance, which means that dates wih prior acquaintance were excluded (set to missing) on a dyadic basis.\n\nVariables are labeled in SPSS. \n\n### A list of important abbreviations, prefixes and suffixes:\n\n* _acq = acquaintance (i.e., variables with this suffix are controlled for prior * acquaintance)\n* avg = average\n* _rat = rating variable\n* _z = z-standardized score\n* BC = booty call\n* DG = dating group (three groups in this study)\n* FIPI = five item personality inventory\n* FS = friendship\n* FWB = friends-with-benefits\n* Int = Intelligence\n* Like = Likeability\n* LTR = long-term relationship\n* MACHIV = mach-iv machiavellianism questionnaire\n* N, E, O, A, C = Big5\n* NPI = narcissistic personality inventory\n* ONS = one night stand\n* P = perceiver\n* PA = physical attractiveness\n* PercEff = perceiver effect\n* SD = speed dating\n* SRM = social relations model\n* SRP = self-report psychopathy scale\n* STR = short-term relationship\n* T = target\n* TargEff = target effect\n\n\n\n\n\n## Table of variables\nThis table contains variable names, labels, and number of missing values.\nSee the complete codebook for more.\n\n[truncated]\n\n### Note\nThis dataset was automatically described using the [codebook R package](https://rubenarslan.github.io/codebook/) (version 0.10.0).",
  "identifier": "https://osf.io/jvk3u/",
  "datePublished": "2015-10-07",
  "creator": {
    "@type": "Person",
    "givenName": "Emanuel",
    "familyName": "Jauk",
    "email": "emanuel.jauk@uni‐graz.at",
    "affiliation": {
      "@type": "Organization",
      "name": "Karl‐Franzens‐Universität Graz, Austria"
    }
  },
  "citation": "Jauk, E., Neubauer, A. C., Mairunteregger, T., Pemp, S., Sieber, K. P., & Rauthmann, J. F. (2016). How alluring are dark personalities? The Dark Triad and attractiveness in speed dating. European Journal of Personality, 30(2), 125-138.",
  "url": "https://osf.io/j4fcb/",
  "temporalCoverage": "2015",
  "spatialCoverage": "Graz, Austria",
  "distribution": [
    {
      "@type": "DataDownload",
      "requiresSubscription": "https://schema.org/True",
      "encodingFormat": "https://www.loc.gov/preservation/digital/formats/fdd/fdd000469.shtml",
      "contentUrl": "https://osf.io/download/j4fcb"
    }
  ],
  "keywords": ["SD_Code", "DG", "DG_size", "DG_size_acq", "age", "sex", "height", "weight", "relStat", "relStat_other", "education", "contracept", "date", "NPI_avg", "SRP_avg", "MACHIV_avg", "BFI_N_avg", "BFI_E_avg", "BFI_O_avg", "BFI_A_avg", "BFI_C_avg", "PA_R1", "PA_R2", "PA_R3", "PA_R4", "PA_avg", "BMI", "SOI_R_B_avg", "SOI_R_A_avg", "SOI_R_D_avg", "TargEff__choice_relFrequ_acq", "TargEff__FS_avg_acq", "TargEff__ONS_avg_acq", "TargEff__BC_avg_acq", "TargEff__FWB_avg_acq", "TargEff__STR_avg_acq", "TargEff__LTR_avg_acq", "TargEff__PA_avg_acq", "TargEff__Like_avg_acq", "TargEff__Int_avg_acq", "TargEff__FIPI_N_avg_acq", "TargEff__FIPI_E_avg_acq", "TargEff__FIPI_O_avg_acq", "TargEff__FIPI_A_avg_acq", "TargEff__FIPI_C_avg_acq", "PercEff__choice_relFrequ_acq", "PercEff__FS_avg_acq", "PercEff__ONS_avg_acq", "PercEff__BC_avg_acq", "PercEff__FWB_avg_acq", "PercEff__STR_avg_acq", "PercEff__LTR_avg_acq", "PercEff__PA_avg_acq", "PercEff__Like_avg_acq", "PercEff__Int_avg_acq", "PercEff__FIPI_N_avg_acq", "PercEff__FIPI_E_avg_acq", "PercEff__FIPI_O_avg_acq", "PercEff__FIPI_A_avg_acq", "PercEff__FIPI_C_avg_acq"],
  "@context": "https://schema.org/",
  "@type": "Dataset",
  "variableMeasured": [
    {
      "name": "SD_Code",
      "description": "speed dating code",
      "@type": "propertyValue"
    },
    {
      "name": "DG",
      "description": "dating group",
      "@type": "propertyValue"
    },
    {
      "name": "DG_size",
      "description": "dating group size",
      "@type": "propertyValue"
    },
    {
      "name": "DG_size_acq",
      "description": "dating group size, corrected for prior acquaintance",
      "@type": "propertyValue"
    },
    {
      "name": "age",
      "description": "age",
      "@type": "propertyValue"
    },
    {
      "name": "sex",
      "description": "sex",
      "value": "1. female,\n2. male",
      "maxValue": 2,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "height",
      "description": "height",
      "@type": "propertyValue"
    },
    {
      "name": "weight",
      "description": "weight",
      "@type": "propertyValue"
    },
    {
      "name": "relStat",
      "description": "relationship status",
      "value": "1. single,\n2. in a relationship,\n3. living separately / divorced",
      "maxValue": 3,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "relStat_other",
      "description": "other relationship status",
      "@type": "propertyValue"
    },
    {
      "name": "education",
      "description": "highest educational attainment",
      "value": "1. nine years schooling only,\n2. professional training,\n3. vocational school,\n4. university-entrance diploma,\n5. academic degree",
      "maxValue": 5,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "contracept",
      "description": "hormonal contraception",
      "value": "1. yes,\n2. no",
      "maxValue": 2,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "date",
      "description": "past experience with speed dating",
      "value": "1. yes,\n2. no",
      "maxValue": 2,
      "minValue": 1,
      "@type": "propertyValue"
    },
    {
      "name": "NPI_avg",
      "description": "narcissistic personality inventory - average",
      "@type": "propertyValue"
    },
    {
      "name": "SRP_avg",
      "description": "self-report psychopathy scale - average",
      "@type": "propertyValue"
    },
    {
      "name": "MACHIV_avg",
      "description": "mach-iv - average",
      "@type": "propertyValue"
    },
    {
      "name": "BFI_N_avg",
      "description": "big five inventory: neuroticism - average",
      "@type": "propertyValue"
    },
    {
      "name": "BFI_E_avg",
      "description": "big five inventory: extraversion - average",
      "@type": "propertyValue"
    },
    {
      "name": "BFI_O_avg",
      "description": "big five inventory: openness - average",
      "@type": "propertyValue"
    },
    {
      "name": "BFI_A_avg",
      "description": "big five inventory: agreeableness - average",
      "@type": "propertyValue"
    },
    {
      "name": "BFI_C_avg",
      "description": "big five inventory: conscientiousness - average",
      "@type": "propertyValue"
    },
    {
      "name": "PA_R1",
      "description": "physical attractiveness - rater1(f)",
      "@type": "propertyValue"
    },
    {
      "name": "PA_R2",
      "description": "physical attractiveness - rater2(f)",
      "@type": "propertyValue"
    },
    {
      "name": "PA_R3",
      "description": "physical attractiveness - rater3(m)",
      "@type": "propertyValue"
    },
    {
      "name": "PA_R4",
      "description": "physical attractiveness - rater4(m)",
      "@type": "propertyValue"
    },
    {
      "name": "PA_avg",
      "description": "physical attractiveness - average",
      "@type": "propertyValue"
    },
    {
      "name": "BMI",
      "description": "body mass index",
      "@type": "propertyValue"
    },
    {
      "name": "SOI_R_B_avg",
      "description": "sociosexual orientation inventory revised: behavior - average",
      "@type": "propertyValue"
    },
    {
      "name": "SOI_R_A_avg",
      "description": "sociosexual orientation inventory revised: attitude - average",
      "@type": "propertyValue"
    },
    {
      "name": "SOI_R_D_avg",
      "description": "sociosexual orientation inventory revised: desire - average",
      "@type": "propertyValue"
    },
    {
      "name": "TargEff__choice_relFrequ_acq",
      "description": "SRM target effect: actual choice",
      "@type": "propertyValue"
    },
    {
      "name": "TargEff__FS_avg_acq",
      "description": "SRM target effect: friendship",
      "@type": "propertyValue"
    },
    {
      "name": "TargEff__ONS_avg_acq",
      "description": "SRM target effect: one night stand",
      "@type": "propertyValue"
    },
    {
      "name": "TargEff__BC_avg_acq",
      "description": "SRM target effect: booty call",
      "@type": "propertyValue"
    },
    {
      "name": "TargEff__FWB_avg_acq",
      "description": "SRM target effect: friends-with-benefits",
      "@type": "propertyValue"
    },
    {
      "name": "TargEff__STR_avg_acq",
      "description": "SRM target effect: short-term relationship (aggregated)",
      "@type": "propertyValue"
    },
    {
      "name": "TargEff__LTR_avg_acq",
      "description": "SRM target effect: long-term relationship",
      "@type": "propertyValue"
    },
    {
      "name": "TargEff__PA_avg_acq",
      "description": "SRM target effect: physical attractiveness",
      "@type": "propertyValue"
    },
    {
      "name": "TargEff__Like_avg_acq",
      "description": "SRM target effect: likeability",
      "@type": "propertyValue"
    },
    {
      "name": "TargEff__Int_avg_acq",
      "description": "SRM target effect: intelligence",
      "@type": "propertyValue"
    },
    {
      "name": "TargEff__FIPI_N_avg_acq",
      "description": "SRM target effect: neuroticism",
      "@type": "propertyValue"
    },
    {
      "name": "TargEff__FIPI_E_avg_acq",
      "description": "SRM target effect: extraversion",
      "@type": "propertyValue"
    },
    {
      "name": "TargEff__FIPI_O_avg_acq",
      "description": "SRM target effect: openness",
      "@type": "propertyValue"
    },
    {
      "name": "TargEff__FIPI_A_avg_acq",
      "description": "SRM target effect: agreeableness",
      "@type": "propertyValue"
    },
    {
      "name": "TargEff__FIPI_C_avg_acq",
      "description": "SRM target effect: conscientiousness",
      "@type": "propertyValue"
    },
    {
      "name": "PercEff__choice_relFrequ_acq",
      "description": "SRM perceiver effect: actual choice",
      "@type": "propertyValue"
    },
    {
      "name": "PercEff__FS_avg_acq",
      "description": "SRM perceiver effect: friendship",
      "@type": "propertyValue"
    },
    {
      "name": "PercEff__ONS_avg_acq",
      "description": "SRM perceiver effect: one night stand",
      "@type": "propertyValue"
    },
    {
      "name": "PercEff__BC_avg_acq",
      "description": "SRM perceiver effect: booty call",
      "@type": "propertyValue"
    },
    {
      "name": "PercEff__FWB_avg_acq",
      "description": "SRM perceiver effect: friends-with-benefits",
      "@type": "propertyValue"
    },
    {
      "name": "PercEff__STR_avg_acq",
      "description": "SRM perceiver effect: short-term relationship (aggregated)",
      "@type": "propertyValue"
    },
    {
      "name": "PercEff__LTR_avg_acq",
      "description": "SRM perceiver effect: long-term relationship",
      "@type": "propertyValue"
    },
    {
      "name": "PercEff__PA_avg_acq",
      "description": "SRM perceiver effect: physical attractiveness",
      "@type": "propertyValue"
    },
    {
      "name": "PercEff__Like_avg_acq",
      "description": "SRM perceiver effect: likeability",
      "@type": "propertyValue"
    },
    {
      "name": "PercEff__Int_avg_acq",
      "description": "SRM perceiver effect: intelligence",
      "@type": "propertyValue"
    },
    {
      "name": "PercEff__FIPI_N_avg_acq",
      "description": "SRM perceiver effect: neuroticism",
      "@type": "propertyValue"
    },
    {
      "name": "PercEff__FIPI_E_avg_acq",
      "description": "SRM perceiver effect: extraversion",
      "@type": "propertyValue"
    },
    {
      "name": "PercEff__FIPI_O_avg_acq",
      "description": "SRM perceiver effect: openness",
      "@type": "propertyValue"
    },
    {
      "name": "PercEff__FIPI_A_avg_acq",
      "description": "SRM perceiver effect: agreeableness",
      "@type": "propertyValue"
    },
    {
      "name": "PercEff__FIPI_C_avg_acq",
      "description": "SRM perceiver effect: conscientiousness",
      "@type": "propertyValue"
    }
  ]
}`
```
