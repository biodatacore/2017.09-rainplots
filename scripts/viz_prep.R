# Library -----------------------------------------------------------------
library(magrittr)
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
source('scripts/utils.R')
# Import ------------------------------------------------------------------
dat <-
    readr::read_tsv('data/all_models_linear_output.tsv')


# Transform Response and Control ------------------------------------------------------

dat %<>%
    mutate(response = var_substitute(response))

dat$control %<>%
    str_split(', ') %>%
    map(var_substitute) %>%
    map_chr(str_c, collapse = ', ')



# Cap pvalue --------------------------------------------------------------
dat %<>%
    mutate(neg_log10_pvalue_15cap = ifelse(neg_log10_pvalue >= 15, 15, neg_log10_pvalue)) %>%
    mutate(neg_log10_pvalue = round(neg_log10_pvalue, 2))


# Filtering ---------------------------------------------------------------
dat %<>%
    filter(grepl('^mzid', term))

top_mzid <-
    dat %>%
    group_by(term) %>%
    summarise(mn = mean(neg_log10_pvalue_15cap)) %>%
    arrange(desc(mn)) %>%
    # slice(1:50) %>%
    pull(term)

top50_dat <-
    dat %>%
    filter(term %in% top_mzid[1:50])

top25_dat <-
    dat %>%
    filter(term %in% top_mzid[1:25])

# Modify labels -----------------------------------------------------------
viz_prep <- function(dat) {
    dat %<>%
        mutate(mzid = purrr::map_chr(stringr::str_split(control, ', '), 1),
               mz = as.numeric(stringr::str_extract(mzid, '[0-9]{3}\\.[0-9]{6}')),
               rt = as.numeric(stringr::str_extract(mzid, '[0-9]{1}\\.[0-9]{4}$'))) %>%
        mutate(control = gsub('\\_[0-9]*\\.[0-9]*\\_[0-9]*\\.[0-9]*', '', control)) %>%
        mutate(control = gsub(', ', ' + ', control)) %>%
        mutate(model_specification = paste(response, '~', control),
               size_label = paste("p =", neg_log10_pvalue),
               colour_label = paste("e =", round(estimate, 2)))


    # Label Ordering ----------------------------------------------------------
    model_levels <-
        dat %>%
        arrange(model_name) %>%
        distinct(response, model_specification)

    dat %<>%
        mutate(term = factor(term, levels = rev(top_mzid)),
               model_specification = factor(model_specification, levels = model_levels$model_specification),
               response = factor(response, levels = model_levels$response))

    return(dat)
}

dat %<>% viz_prep()
top50_dat %<>% viz_prep()
top25_dat %<>% viz_prep()

saveRDS(dat, 'data/plotdat.rds')
saveRDS(top50_dat, 'data/top50_plotdat.rds')
saveRDS(top25_dat, 'data/top25_plotdat.rds')
