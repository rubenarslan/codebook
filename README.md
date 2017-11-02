# Codebook Cookbook
## Generate markdown codebooks from the attributes of the variables in your data frame

Integrates with formr ([formr.org](https://formr.org)), an online survey framework and especially the data frames produced and marked up by the [formr R package](https://github.com/rubenarslan/formr). However, it should work with un-annotated datasets too, and you can also manually add the required attributes to make it fancier.

The codebook processes single items, but also "scales", i.e. psychological questionnaires that are aggregated to extract a construct. For scales, the appropriate reliability coefficients (internal consistencies for single measurements, retest reliabilities for repeated measurements, multilevel reliability for multilevel data) are computed.

For items and scales, the distributions are summarised graphically and numerically.

The result are markdown _partials_, that can be integrated in a larger markdown document and then translated to HTML or PDF format.


  install.packages("devtools")
  devtools::install_github("rubenarslan/codebook")
