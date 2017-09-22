
# Library -----------------------------------------------------------------

library(magrittr)
library(biodatacoreSmallMolecules)
library(dplyr)


# Import ------------------------------------------------------------------

dat <- import_and_edit_fhs()


# Randomly select 1500 respondents ----------------------------------------
set.seed(12345)

dat$merged %<>%
    sample_n(1500)

# Filter to Eicosanoids ---------------------------------------------------
non_eicosanoid_ids <-
    dat$metabolites$info %>%
    filter(metabolite_type != 'Eicosanoids') %>%
    pull(mzid)

dat$merged %<>%
    select(-one_of(non_eicosanoid_ids))

# Model specification -----------------------------------------------------

rc <-
    list(
        model_01 = list(response = 'AGE8', control = c('mzid', 'sex')),
        model_02 = list(response = 'sex', control = c('mzid', 'AGE8')),
        model_03 = list(response = 'BMI8', control = c('mzid', 'AGE8', 'sex')),
        model_04 = list(response = 'MetS', control = c('mzid', 'AGE8', 'sex')),
        model_05 = list(response = 'curr_diab8', control = c('mzid', 'AGE8', 'sex')),
        model_06 = list(response = 'curr_diab9', control = c('mzid', 'AGE8', 'sex', 'BMI8', 'gluco8')),
        model_07 = list(response = 'points', control = c('mzid')),
        model_08 = list(response = 'cvd', control = c('mzid', 'points'))
    )

model_dat <- modeling_prep(dat$merged, rc)


# Linear Modeling ---------------------------------------------------------

model_results <- lm_rank(model_dat, scale_pvalue_by_model = FALSE)

readr::write_tsv(model_results, 'data/all_models_linear_output.tsv')
