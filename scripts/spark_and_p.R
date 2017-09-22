
# Library -----------------------------------------------------------------
library(magrittr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)
source('scripts/utils.R')
# Import ------------------------------------------------------------------

dat <-
    import_plotdat()

top50_dat <-
    import_top50_plotdat()

# Sparkplot ---------------------------------------------------------------


sparkplot <- function(dat) {

    # Scale pvalues to estimates so that max/min pvalue ~ max/min estimate
    dat %<>%
        group_by(response) %>%
        mutate(rescaled_neg_log10_pvalue = scales::rescale(neg_log10_pvalue,
                                                           to = c(min(estimate), max(estimate)))
        ) %>%
        ungroup()

    px <-
        dat %>%
        group_by(response) %>%
        # distinct(neg_log10_pvalue) %>%
        arrange(desc(neg_log10_pvalue)) %>%
        mutate(px = 1:n()) %>%
        select(response, term, px) %>%
        ungroup()

    dat %<>%
        left_join(px, by = c('response', 'term'))

    plots <-
        unique(sort(dat$response)) %>%
        as.character() %>%
        purrr::set_names() %>%
        purrr::map(~.spark_plot(., dat))

    # do.call(grid.arrange, c(plots, ncol = 2))
    # grid.arrange(do.call(arrangeGrob, c(plots, ncol = 2, padding = unit(10, 'cm'))))

    #plots w/ spaces
    blank <- rectGrob(gp = gpar(col="white")) # make a white spacer grob
    do.call(plot_grid, c(plots, ncol = 2))
}

# Have to manually extract scaling process from rescale
estimate_to_pvalue <- function(values, dat) {
    scales::rescale(
        values,
        to = range(dat$neg_log10_pvalue, na.rm = TRUE, finite = TRUE),
        from = range(dat$estimate, na.rm = TRUE, finite = TRUE))

}

# Cannot use facet wrap due to need for individual second axis tscale and ransform for each response
.spark_plot <- function(resp, dat) {
    model_dat <-
        dat %>%
        filter(as.character(response) == resp)

    nmetabs <-
        length(unique(model_dat$term))

    if (nmetabs == 539) {
        brks <- seq(0, 500, 100)
    } else {
        brks <- seq(0, 50, 10)
    }
    p <-
        model_dat %>%
        ggplot() +
        geom_col(aes(x = px, y = estimate), fill = 'blue') +
        geom_point(aes(x = px, y = rescaled_neg_log10_pvalue), colour = 'orange')  +
        scale_y_continuous('Beta Coefficient',
                           position = 'right',
                           sec.axis = sec_axis(~estimate_to_pvalue(., model_dat), name = "Neg Log 10 P Value")
        ) +
        scale_x_continuous('Analytes Rank Ordered by Significance',
                           breaks = brks) +
        ggtitle(resp) +
        theme(
            panel.grid = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title = element_text(size = 9, face = 'bold'),
            axis.line = element_blank(),
            axis.text.y.right = element_text(hjust = 1)
        )

    return(p)
}
# # Output ------------------------------------------------------------------


pdf('plots/spark_and_pvalue.pdf', width = 10, height = 7.5)
sparkplot(dat)
sparkplot(top50_dat)
dev.off()

