library(codebook)
data("bfi", package = "codebook")
codebook_component_single_item(bfi$BFIK_neuro_2R)

library(dplyr)
rels <- compute_reliabilities(bfi %>% select(starts_with("BFIK_consc")))
codebook_component_scale(bfi$BFIK_consc, "BFIK_consc", items = bfi)
codebook_component_scale(bfi$BFIK_consc, "BFIK_consc", items = bfi,
                         reliabilities = rels$BFIK_consc)
