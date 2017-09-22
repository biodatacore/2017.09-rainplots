
# Library -----------------------------------------------------------------
library(magrittr)
library(dplyr)
library(ggplot2)
library(cowplot)
source('scripts/utils.R')
# Import ------------------------------------------------------------------

dat <-
    import_plotdat()

top50_dat <-
    import_top50_plotdat()

manhattan <- function(dat) {
    # Manhattan Modification --------------------------------------------------
    mzx <-
        dat %>%
        distinct(mz) %>%
        arrange(mz) %>%
        mutate(mzx = 1:n())

    colour_groups <-
        dat %>%
        arrange(model_name) %>%
        distinct(response) %>%
        mutate(colour_group = as.factor(rep_len(c(0, 1), length.out = nrow(.))))

    high_p <-
        dat %>%
        distinct(neg_log10_pvalue) %>%
        top_n(10, neg_log10_pvalue) %>%
        pull(neg_log10_pvalue)

    dat %<>%
        left_join(mzx, by = 'mz') %>%
        left_join(colour_groups, by = 'response') %>%
        mutate(high_p = ifelse(neg_log10_pvalue >= min(high_p), TRUE, FALSE),
               text = ifelse(high_p, mzid, NA)) %>%
        mutate(text = gsub('^mzid_', '', text))



    # Manhattan Plots -----------------------------------------------------------
    thm <-
        theme(
            title = element_text(face = 'bold'),
            strip.text = element_text(face = 'bold'),
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            # axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = 'white'),
            plot.background = element_rect(fill = 'white'),
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            strip.background = element_blank()
        )

    base_manhattan_plot <-
        dat %>%
        ggplot(aes(x = mzx, y = neg_log10_pvalue, colour = colour_group)) +
        geom_point() +
        scale_color_discrete(guide = FALSE) +
        scale_x_continuous('M/Z') +
        scale_y_continuous('Neg Log 10 P Value') +
        thm

    side_by_side_manhattan_plot <-
        base_manhattan_plot +
        facet_grid(~response, scales = 'free_x', switch = 'x') +
        theme(strip.text.x = element_text(angle = 90, vjust = 1))

    individual_manhattan_plot <-
        base_manhattan_plot +
        geom_point(colour = 'black') +
        facet_wrap(~response, scales = 'free', ncol = 2)

    print(side_by_side_manhattan_plot)
    print(individual_manhattan_plot)
}


# Output ------------------------------------------------------------------


pdf('plots/manhattan.pdf', width = 10, height = 7.5)
manhattan(dat)
manhattan(top50_dat)
dev.off()

