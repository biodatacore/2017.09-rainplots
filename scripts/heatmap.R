
# Library -----------------------------------------------------------------
library(magrittr)
library(dplyr)
library(ggplot2)
library(viridis)
library(gridExtra)
library(tidyr)
library(ggdendro)
library(RColorBrewer)
source('scripts/utils.R')
# Import ------------------------------------------------------------------

dat <-
    import_plotdat()

top50_dat <-
    import_top50_plotdat()


# Heatmap -----------------------------------------------------------------
heatmap <- function(data, x, y, beta, p_value, y_labs = TRUE,
                    x_order = c('none', 'p_value', 'beta', 'alphabetical'),
                    y_order = c('none', 'p_value', 'beta', 'alphabetical', 'clustered'),
                    dendrogram = FALSE, dendrogram_antialiasing = 0.1,
                    beta_palette = NULL, p_value_palette = NULL,
                    beta_legend_title = waiver(), p_value_legend_title = waiver(),
                    n_beta_breaks = NULL,
                    beta_breaks = waiver(), p_value_breaks = waiver(),
                    beta_labels = waiver(), p_value_labels = waiver(),
                    beta_limits = NULL, p_value_limits = NULL,
                    p_value_label = NULL, beta_label = NULL) {

    if (missing(beta_palette)) {
        beta_palette <-
            c("#053061",
              "#313695",
              "#4575b4",
              "#74add1",
              "#abd9e9",
              "#e0f3f8",
              # "#ffffbf",
              "#fee090",
              "#fdae61",
              "#f46d43",
              "#d73027",
              "#a50026",
              '#67001f')
    }

    if (missing(p_value_palette)) {
        p_value_palette <- viridis(20)
    }

    thme <-
        theme(
            line = element_blank(),
            axis.text.x.top = element_text(angle = 90, hjust = 0, size = 20),
            legend.position = "bottom",
            legend.justification = 'center',
            axis.title = element_blank(),
            # axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()
        )

    if (!y_labs) {
        thme <-
            thme + theme(axis.text.y = element_blank())
    }

    ## pvalue colours
    p_value_fill_params <-
        list(
            colours = p_value_palette,
            name = p_value_legend_title,
            breaks = p_value_breaks,
            labels = p_value_labels,
            limits = p_value_limits,
            guide = guide_colourbar(title.position = 'top')
        )

    p_value_fill_scale <- do.call(scale_fill_gradientn, p_value_fill_params)

    ## beta fill colour
    if (!missing(n_beta_breaks)) {
        stopifnot(is.numeric(n_beta_breaks))

        max_clr <-  max((data[[beta]]))
        min_clr <-  min((data[[beta]]))

        vals <-
        beta_breaks <- c(min_clr, 0, seq(-1 * max_clr, max_clr, length.out = n_beta_breaks))

        beta_values <-
            scales::rescale(seq(-1 * max_clr, max_clr, length.out = length(beta_palette)))

        if (missing(beta_limits)) {
            beta_limits <- c((-1 * max_clr) - 0.1, max_clr + 0.1)
        }

    }

    beta_fill_params <-
        list(
            colours = beta_palette,
            name = beta_legend_title,
            breaks = beta_breaks,
            labels = beta_labels,
            limits = beta_limits,
            values = beta_values,
            guide = guide_colourbar(title.position = 'top',
                                    barwidth = 10)
            )

    beta_fill_scale <-
        do.call(scale_fill_gradientn, beta_fill_params)

    # Ordering and clustering
    clust <- NA


    .order <- function(data, ordr, order_col,
                       x = x, y = y, beta = beta, p_value = p_value, ...) {
        if (first(ordr == 'none')) {
            # do nothing
        } else if (first(ordr == 'p_value')) {
            lvls <-
                data %>%
                group_by(!!as.name(order_col)) %>%
                summarise(p_value = mean(!!as.name(p_value))) %>%
                arrange(p_value) %>%
                pull(!!as.name(order_col)) %>%
                as.character()

            data[[order_col]] %<>% factor(levels = lvls)
        } else if (first(ordr == 'beta')) {
            data[[order_col]] %<>% factor(levels = order(data[[beta]]))
        } else if (first(ordr == 'alphabetical')) {
            data[[order_col]] %<>% as.character()
        } else if (first(ordr == 'clustered')) { # This clustering assumes y-axis clustering only for now
            c_data <-
                data %>%
                select(!!y, !!x, !!beta) %>%
                spread(!!x, !!beta)

            c_mat <- as.matrix(c_data[, -1])
            rownames(c_mat) <- c_data[[1]]

            clust <- hclust(dist(c_mat), method = 'ward.D2')

            data[[order_col]] %<>% factor(levels = clust$labels[clust$order])

            # For dendrograms
            clust <<- clust

        } else {
            stop('invalid .order-ing')
        }

        return(data)
    }



    data %<>%
        .order(x_order, x, x, y, beta, p_value) %>%
        .order(y_order, y, x, y, beta, p_value)

    base_heatmap <-
        data %>%
        ggplot(aes_(x = as.name(x), y = as.name(y))) +
        scale_x_discrete(position = "top") +
        thme

    beta_heatmap <-
        base_heatmap +
        geom_tile(aes_(fill = as.name(beta))) +
        beta_fill_scale

    p_value_heatmap <-
        base_heatmap +
        geom_tile(aes_(fill = as.name(p_value))) +
        p_value_fill_scale


    if (dendrogram & first(y_order == 'clustered')) {

        thme_dendro <- theme(
            legend.position = "bottom", # maintain spacing, hide labels
            legend.text = element_text(colour = 'white'), # maintain spacing, hide labels
            axis.title = element_blank(),
            #axis.title.x = element_blank(),
            #axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 0, colour = 'white', size = 20), # maintain spacing, hide labels
            axis.text.y = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_line(colour = 'white'),# maintain spacing, hide labels
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank() # pushes dendrogram out to plot edges
        )

        # Extract dendrogram data
        dendro_dat <- segment(dendro_data(clust))

        # segment draws the dendrograms starting at the middle of each line segemnt.
        # This can expose corners where line segments dont match up exactly, especailly
        # at higher resolutions. This hack attempts to 'anti-alias' that a bit
        dendro_dat %<>%
            mutate(y = ifelse(y != yend, y + dendrogram_antialiasing, y))

        spoof_labs <- unique(as.character(data[[x]])) # spoof labels for spacing

        dendro <-
            ggplot(dendro_dat) +
            geom_segment(aes(x = x, y = y, xend=xend, yend=yend, colour = yend)) + # One segment draws the guide for spacing,
            geom_segment(aes(x = x, y = y, xend=xend, yend=yend), colour = 'black') + # the other draws black on top of it
            scale_x_continuous(expand = c(0, 0.5)) + # pushes dendrogram out to plot edges, with small border so lines are more centered on tiles
            scale_y_reverse(breaks = seq(min(c(dendro_dat$y, dendro_dat$yend)),
                                         max(c(dendro_dat$y, dendro_dat$yend)),
                                         length.out = length(spoof_labs)),
                            labels = spoof_labs,
                            position = 'top') +
            scale_colour_gradient(' ',
                                  low = 'white', high = 'white',
                                  guide = guide_colourbar(
                                      title.position = 'top',
                                      title.hjust = 0.5
                                  )
            ) +
            coord_flip() +
            thme_dendro

        cowplot::plot_grid(dendro, beta_heatmap, p_value_heatmap, ncol = 3, rel_widths = c(1/3, 1, 1))

    } else {
        grid.arrange(beta_heatmap, p_value_heatmap, ncol = 2)
    }


}


# plottr ------------------------------------------------------------------
plottr <- function(data) {

    .heatmap <- purrr::partial(heatmap,
                               data = data,
                               x = 'response',
                               y = 'term',
                               beta = 'estimate',
                               p_value = 'neg_log10_pvalue_15cap',
                               y_labs = FALSE,
                               n_beta_breaks = 5,
                               dendrogram_antialiasing = 0.018,
                               beta_legend_title = 'Beta Coefficient',
                               p_value_legend_title = 'Neg Log10 P Value',
                               # beta_breaks = c(-2, -1, 0, 1),
                               # beta_limits = c(min(data$estimate) - 0.1, round(max(data$estimate) + 0.1)),
                               p_value_limits = c(-0.1, 15.1),
                               p_value_breaks = c(0, 5, 10, 15),
                               p_value_labels = c(0, 5, 10, '>=15'))

    data$estimate %<>% round(1)

    .heatmap(
        y_order = 'p_value'
    )

    .heatmap(
        y_order = 'alphabetical'
    )

    .heatmap(
        y_order = 'clustered'
    )

    .heatmap(
        y_order = 'clustered',
        dendrogram = TRUE
    )
}

# # Output ------------------------------------------------------------------
#
#
#pdf('plots/heatmaps.pdf', width = 15, height = 20)
plottr(dat)
plottr(top50_dat)
#dev.off()

