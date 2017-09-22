
# Library -----------------------------------------------------------------
library(magrittr)
library(dplyr)
library(ggplot2)
library(viridis)
library(gridExtra)
source('scripts/utils.R')
# Import ------------------------------------------------------------------

dat <-
    import_plotdat()

top50_dat <-
    import_top50_plotdat()

# Volcano Plots -----------------------------------------------------------
volcano <- function(dat) {

    p <-
        dat %>%
        ggplot(aes(x = estimate, y = neg_log10_pvalue,
                   group = model_specification)
        ) +
        geom_point() +
        geom_hline(yintercept = 15, colour = 'lightblue') +# +
        facet_wrap(~model_specification, scales = 'free_x') +
        scale_x_continuous('Beta') +
        scale_y_continuous('-1*log_10(P Value)') +
        theme(
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = 'white'),
            plot.background = element_rect(fill = 'white'),
            axis.line = element_blank()# ,
            #axis.text.x.top  = element_text(colour = 'black', angle = 90, hjust = 0),
            #axis.text.y = element_text(colour = 'black'),
            #legend.background = element_rect(fill = 'white'),
            # legend.text = element_text(colour = 'black'),
            # legend.title = element_text(colour = 'black'),
            # legend.key = element_blank()
        )
    print(p)
}


# Output ------------------------------------------------------------------


pdf('plots/volcano.pdf', width = 15, height = 15)
volcano(dat)
volcano(top50_dat)
dev.off()

