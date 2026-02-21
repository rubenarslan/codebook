# Codebook metadata table

will generate a table combining metadata from variable attributes with
data summaries generated using
[`skimr::skim()`](https://docs.ropensci.org/skimr/reference/skim.html)

## Usage

``` r
codebook_table(results)
```

## Arguments

- results:

  a data frame, ideally with attributes set on variables

## Examples

``` r
data("bfi")
codebook_table(bfi)
#>             name
#> 1        session
#> 2        created
#> 3       modified
#> 4          ended
#> 5        expired
#> 6    BFIK_open_2
#> 7  BFIK_agree_4R
#> 8   BFIK_extra_2
#> 9  BFIK_agree_1R
#> 10   BFIK_open_1
#> 11 BFIK_neuro_2R
#> 12  BFIK_consc_3
#> 13  BFIK_consc_4
#> 14 BFIK_consc_2R
#> 15 BFIK_agree_3R
#> 16 BFIK_extra_3R
#> 17  BFIK_neuro_3
#> 18  BFIK_neuro_4
#> 19  BFIK_agree_2
#> 20  BFIK_consc_1
#> 21   BFIK_open_4
#> 22  BFIK_extra_4
#> 23 BFIK_extra_1R
#> 24   BFIK_open_3
#> 25    BFIK_agree
#> 26     BFIK_open
#> 27    BFIK_consc
#> 28    BFIK_extra
#> 29    BFIK_neuro
#>                                                                         label
#> 1                                                                        <NA>
#> 2                                                    user first opened survey
#> 3                                                     user last edited survey
#> 4                                                        user finished survey
#> 5                                                                        <NA>
#> 6                       __Ich bin tiefsinnig, denke gerne über Sachen nach.__
#> 7        __Ich kann mich schroff und abweisend anderen gegenüber verhalten.__
#> 8            __Ich bin begeisterungsfähig und kann andere leicht mitreißen.__
#> 9                                  __Ich neige dazu, andere zu kritisieren.__
#> 10                                       __Ich bin vielseitig interessiert.__
#> 11 __Ich bin entspannt, lasse mich durch Stress nicht aus der Ruhe bringen.__
#> 12                                     __Ich bin tüchtig und arbeite flott.__
#> 13                              __Ich mache Pläne und führe sie auch durch.__
#> 14                                    __Ich bin bequem, neige zur Faulheit.__
#> 15                          __Ich kann mich kalt und distanziert verhalten.__
#> 16                               __Ich bin eher der "stille Typ", wortkarg.__
#> 17                                            __Ich mache mir viele Sorgen.__
#> 18                                  __Ich werde leicht nervös und unsicher.__
#> 19  __Ich schenke anderen leicht Vertrauen, glaube an das Gute im Menschen.__
#> 20                                       __Ich erledige Aufgaben gründlich.__
#> 21                   __Ich schätze künstlerische und ästhetische Eindrücke.__
#> 22                                 __Ich gehe aus mir heraus, bin gesellig.__
#> 23                                __Ich bin eher zurückhaltend, reserviert.__
#> 24             __Ich habe eine aktive Vorstellungskraft, bin phantasievoll.__
#> 25                      4 BFIK_agree items aggregated by aggregation_function
#> 26                       4 BFIK_open items aggregated by aggregation_function
#> 27                      4 BFIK_consc items aggregated by aggregation_function
#> 28                      4 BFIK_extra items aggregated by aggregation_function
#> 29                      3 BFIK_neuro items aggregated by aggregation_function
#>             type type_options      data_type
#> 1           <NA>         <NA>      character
#> 2           <NA>         <NA>        POSIXct
#> 3           <NA>         <NA>        POSIXct
#> 4           <NA>         <NA>        POSIXct
#> 5           <NA>         <NA>        logical
#> 6  rating_button            5 haven_labelled
#> 7  rating_button            5 haven_labelled
#> 8  rating_button            5 haven_labelled
#> 9  rating_button            5 haven_labelled
#> 10 rating_button            5 haven_labelled
#> 11 rating_button            5 haven_labelled
#> 12 rating_button            5 haven_labelled
#> 13 rating_button            5 haven_labelled
#> 14 rating_button            5 haven_labelled
#> 15 rating_button            5 haven_labelled
#> 16 rating_button            5 haven_labelled
#> 17 rating_button            5 haven_labelled
#> 18 rating_button            5 haven_labelled
#> 19 rating_button            5 haven_labelled
#> 20 rating_button            5 haven_labelled
#> 21 rating_button            5 haven_labelled
#> 22 rating_button            5 haven_labelled
#> 23 rating_button            5 haven_labelled
#> 24 rating_button            5 haven_labelled
#> 25          <NA>         <NA>        numeric
#> 26          <NA>         <NA>        numeric
#> 27          <NA>         <NA>        numeric
#> 28          <NA>         <NA>        numeric
#> 29          <NA>         <NA>        numeric
#>                                                                                                                         value_labels
#> 1                                                                                                                               <NA>
#> 2                                                                                                                               <NA>
#> 3                                                                                                                               <NA>
#> 4                                                                                                                               <NA>
#> 5                                                                                                                               <NA>
#> 6  1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.
#> 7  5. 1: Trifft überhaupt nicht zu,\n4. 2,\n3. 3,\n2. 4,\n1. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.
#> 8  1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.
#> 9  5. 1: Trifft überhaupt nicht zu,\n4. 2,\n3. 3,\n2. 4,\n1. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.
#> 10 1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.
#> 11 5. 1: Trifft überhaupt nicht zu,\n4. 2,\n3. 3,\n2. 4,\n1. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.
#> 12 1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.
#> 13 1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.
#> 14 5. 1: Trifft überhaupt nicht zu,\n4. 2,\n3. 3,\n2. 4,\n1. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.
#> 15 5. 1: Trifft überhaupt nicht zu,\n4. 2,\n3. 3,\n2. 4,\n1. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.
#> 16 5. 1: Trifft überhaupt nicht zu,\n4. 2,\n3. 3,\n2. 4,\n1. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.
#> 17 1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.
#> 18 1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.
#> 19 1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.
#> 20 1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.
#> 21 1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.
#> 22 1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.
#> 23 5. 1: Trifft überhaupt nicht zu,\n4. 2,\n3. 3,\n2. 4,\n1. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.
#> 24 1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.
#> 25                                                                                                                              <NA>
#> 26                                                                                                                              <NA>
#> 27                                                                                                                              <NA>
#> 28                                                                                                                              <NA>
#> 29                                                                                                                              <NA>
#>    optional                                          scale_item_names
#> 1      <NA>                                                      <NA>
#> 2      <NA>                                                      <NA>
#> 3      <NA>                                                      <NA>
#> 4      <NA>                                                      <NA>
#> 5      <NA>                                                      <NA>
#> 6         0                                                      <NA>
#> 7         0                                                      <NA>
#> 8         0                                                      <NA>
#> 9         0                                                      <NA>
#> 10        0                                                      <NA>
#> 11        0                                                      <NA>
#> 12        0                                                      <NA>
#> 13        0                                                      <NA>
#> 14        0                                                      <NA>
#> 15        0                                                      <NA>
#> 16        0                                                      <NA>
#> 17        0                                                      <NA>
#> 18        0                                                      <NA>
#> 19        0                                                      <NA>
#> 20        0                                                      <NA>
#> 21        0                                                      <NA>
#> 22        0                                                      <NA>
#> 23        0                                                      <NA>
#> 24        0                                                      <NA>
#> 25     <NA> BFIK_agree_4R, BFIK_agree_1R, BFIK_agree_3R, BFIK_agree_2
#> 26     <NA>        BFIK_open_2, BFIK_open_1, BFIK_open_4, BFIK_open_3
#> 27     <NA>   BFIK_consc_3, BFIK_consc_4, BFIK_consc_2R, BFIK_consc_1
#> 28     <NA>  BFIK_extra_2, BFIK_extra_3R, BFIK_extra_4, BFIK_extra_1R
#> 29     <NA>                 BFIK_neuro_2R, BFIK_neuro_3, BFIK_neuro_4
#>    item_order n_missing complete_rate n_unique empty count                 min
#> 1        <NA>         0             1       28     0  <NA>                  64
#> 2        <NA>         0             1       28    NA  <NA> 2016-07-08 09:54:16
#> 3        <NA>         0             1       28    NA  <NA> 2016-07-08 09:55:43
#> 4        <NA>         0             1       28    NA  <NA> 2016-07-08 09:55:43
#> 5        <NA>        28             0       NA    NA    :                 <NA>
#> 6           4         0             1       NA    NA  <NA>                   2
#> 7           5         0             1       NA    NA  <NA>                   1
#> 8           6         0             1       NA    NA  <NA>                   1
#> 9           7         0             1       NA    NA  <NA>                   2
#> 10          8         0             1       NA    NA  <NA>                   2
#> 11          9         0             1       NA    NA  <NA>                   2
#> 12         10         0             1       NA    NA  <NA>                   1
#> 13         11         0             1       NA    NA  <NA>                   2
#> 14         12         0             1       NA    NA  <NA>                   1
#> 15         13         0             1       NA    NA  <NA>                   1
#> 16         14         0             1       NA    NA  <NA>                   1
#> 17         15         0             1       NA    NA  <NA>                   1
#> 18         16         0             1       NA    NA  <NA>                   1
#> 19         17         0             1       NA    NA  <NA>                   1
#> 20         18         0             1       NA    NA  <NA>                   2
#> 21         19         0             1       NA    NA  <NA>                   1
#> 22         20         0             1       NA    NA  <NA>                   1
#> 23         21         0             1       NA    NA  <NA>                   1
#> 24         22         0             1       NA    NA  <NA>                   2
#> 25       <NA>         0             1       NA    NA  <NA>                 1.5
#> 26       <NA>         0             1       NA    NA  <NA>                 3.0
#> 27       <NA>         0             1       NA    NA  <NA>                 2.0
#> 28       <NA>         0             1       NA    NA  <NA>                 1.5
#> 29       <NA>         0             1       NA    NA  <NA>                 1.3
#>                   median                 max     mean        sd whitespace
#> 1                   <NA>                  64       NA        NA          0
#> 2  2016-07-08 12:47:07.5 2016-11-02 21:19:50       NA        NA         NA
#> 3  2016-07-08 14:23:22.5 2016-11-02 21:21:53       NA        NA         NA
#> 4  2016-07-08 14:23:22.5 2016-11-02 21:21:53       NA        NA         NA
#> 5                   <NA>                <NA>      NaN        NA         NA
#> 6                    4.0                   5 4.214286 0.7382232         NA
#> 7                    3.0                   5 2.928571 1.1841100         NA
#> 8                    4.0                   5 4.178571 1.0904831         NA
#> 9                    3.0                   5 3.000000 0.9428090         NA
#> 10                   5.0                   5 4.392857 0.8317445         NA
#> 11                   3.0                   5 3.107143 0.8751417         NA
#> 12                   4.0                   5 3.500000 1.0363755         NA
#> 13                   4.0                   5 3.857143 0.7559289         NA
#> 14                   4.0                   5 3.178571 1.3067792         NA
#> 15                   3.0                   5 3.035714 1.2904820         NA
#> 16                   4.0                   5 3.750000 1.2056964         NA
#> 17                   3.0                   5 3.071429 1.2744954         NA
#> 18                   2.0                   4 2.500000 1.2018504         NA
#> 19                   4.0                   5 3.500000 1.2619796         NA
#> 20                   4.0                   5 4.071429 0.8997354         NA
#> 21                   4.0                   5 4.214286 0.9567361         NA
#> 22                   4.0                   5 3.857143 1.1126973         NA
#> 23                   4.0                   5 3.607143 1.1968875         NA
#> 24                   4.5                   5 4.214286 0.9567361         NA
#> 25                   3.0                 4.8 3.116071 0.9316506         NA
#> 26                   4.2                 5.0 4.258929 0.5630692         NA
#> 27                   3.8                 5.0 3.651786 0.7915622         NA
#> 28                   4.2                 5.0 3.848214 1.0099947         NA
#> 29                   2.8                 4.3 2.892857 0.9254231         NA
#>    n_value_labels     hist
#> 1              NA     <NA>
#> 2              NA     <NA>
#> 3              NA     <NA>
#> 4              NA     <NA>
#> 5              NA     <NA>
#> 6               6 ▁▁▁▁▁▇▁▅
#> 7               6 ▂▇▁▃▁▅▁▂
#> 8               6 ▁▁▁▁▁▇▁▇
#> 9               6 ▇▁▅▁▁▆▁▁
#> 10              6 ▁▁▂▁▁▃▁▇
#> 11              6 ▆▁▇▁▁▇▁▁
#> 12              6 ▁▂▁▅▁▇▁▂
#> 13              6 ▁▁▃▁▁▇▁▂
#> 14              6 ▃▂▁▃▁▇▁▂
#> 15              6 ▂▇▁▃▁▇▁▃
#> 16              6 ▂▂▁▅▁▇▁▇
#> 17              6 ▃▇▁▇▁▅▁▅
#> 18              6 ▆▁▇▁▁▂▁▇
#> 19              6 ▂▅▁▅▁▇▁▆
#> 20              6 ▁▁▂▁▁▇▁▇
#> 21              6 ▁▁▁▂▁▆▁▇
#> 22              6 ▁▂▁▃▁▇▁▆
#> 23              6 ▁▅▁▆▁▇▁▇
#> 24              6 ▁▁▂▁▁▅▁▇
#> 25             NA    ▂▇▅▅▃
#> 26             NA    ▂▃▁▇▇
#> 27             NA    ▂▃▇▇▃
#> 28             NA    ▂▂▃▅▇
#> 29             NA    ▅▇▇▆▇
```
