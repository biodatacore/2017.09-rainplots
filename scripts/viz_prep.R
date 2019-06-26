# Library -----------------------------------------------------------------
library(magrittr)
library(dplyr)
library(stringr)
library(purrr)
# Import ------------------------------------------------------------------
model_results <-
    readr::read_tsv('data/all_models_linear_output.tsv')



# Only Metabolite Terms ---------------------------------------------------

model_results %<>%
    filter(grepl('^mzid', term))


# Full Variable Names -----------------------------------------------------

full_name <- function(var) {
    case_when(
        var == 'AGE8' ~ 'Age',
        var == 'BMI8' ~ 'Body Mass Index',
        var == 'curr_diab8' ~ 'Prevalent Diabetes',
        var == 'points' ~ 'Framingham Risk Score',
        var == 'sex' ~ 'Female Sex',
        var == 'MetS' ~ 'Metabolic Syndrome',
        var == 'curr_diab9' ~ 'Incident Diabetes',
        var == 'cvd' ~ 'Incident CVD',
        var == 'gluco8' ~ 'Fasting Glucose',
        TRUE ~ var
    )
}

model_results %<>%
    mutate(response = full_name(response))

model_results$control %<>%
    str_split(', ') %>%
    map(full_name) %>%
    map_chr(str_c, collapse = ' + ')


# P-value Ranking ---------------------------------------------------------

mzid_rank <-
    model_results %>%
    group_by(term) %>%
    summarise(mn = median(p.value)) %>%
    arrange(mn) %>%
    mutate(rank = 1:n())

model_results %<>%
    select(-rank) %>%
    left_join(mzid_rank)

saveRDS(model_results, 'data/plot_data.rds')


# Tutorial Data -----------------------------------------------------------

model_results <- model_results %>% filter(rank %in% seq(1, 121, 6))

model_results <-
    model_results %>%
    select(response, term, estimate, p.value) %>%
    mutate(estimate = round(estimate, 1))

saveRDS(model_results, 'data/tutorial_plot_data.rds')
