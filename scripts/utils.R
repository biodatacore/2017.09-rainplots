import_plotdat <- function() {
    readRDS('data/plotdat.rds')
}

import_top50_plotdat <- function() {
    readRDS('data/top50_plotdat.rds')
}

import_top25_plotdat <- function() {
    readRDS('data/top25_plotdat.rds')
}

var_substitute <- function(var) {
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

