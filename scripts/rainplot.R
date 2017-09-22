
# Library -----------------------------------------------------------------
library(magrittr)
library(dplyr)
library(ggplot2)
library(viridis)
library(RColorBrewer)
source('scripts/utils.R')
# Import ------------------------------------------------------------------
rainplot_prep <- function(data) {
    data %>%
        mutate(estimate = round(estimate, 1)) %>%
        mutate(
            p_value_label = paste('-log10(p) = ', round(neg_log10_pvalue, 2)),
            beta_label = paste('b = ', estimate)
        )
}

dat <-
    import_plotdat() %>%
    rainplot_prep()

top50_dat <-
    import_top50_plotdat() %>%
    rainplot_prep()

top25_dat <-
    import_top25_plotdat() %>%
    rainplot_prep()




# Rainplot Function -------------------------------------------------------
rainplot <- function(
    data, x, y, beta, p_value, max_size = 16, outline = FALSE, labels = FALSE,
    y_labs = TRUE, narrow = FALSE,
    x_order = c('none', 'p_value', 'beta', 'alphabetical'),
    y_order = c('none', 'p_value', 'beta', 'alphabetical', 'clustered'),
    dendrogram = FALSE, dendrogram_antialiasing = 0.1,
    palette = NULL, thme = 'white',
    beta_legend_title = waiver(), p_value_legend_title = waiver(),
    n_beta_breaks = NULL,
    beta_breaks = waiver(), p_value_breaks = waiver(),
    beta_labels = waiver(), p_value_labels = waiver(),
    beta_limits = NULL, p_value_limits = NULL,
    p_value_label = NULL, beta_label = NULL,
    label_size = 3, label_alpha = 0.33, label_text_colour = 'black',
    label_nudge_x = 0.4, label_nudge_y = 0.2, legend_bubble_colour = NULL,
    plot_elements = list()
) {

    ## checks
    stopifnot(is.character(x))
    stopifnot(is.character(y))
    stopifnot(is.character(beta))
    stopifnot(is.character(p_value))

    stopifnot(is.numeric(max_size))
    stopifnot(max_size > 0)

    stopifnot(is.logical(outline))
    stopifnot(is.logical(labels))

    stopifnot(is.character(data[[x]]) | is.factor(data[[x]]) | is.integer(data[[x]]))
    stopifnot(is.character(data[[y]]) | is.factor(data[[y]]) | is.integer(data[[y]]))
    stopifnot(is.numeric(data[[beta]]))
    stopifnot(is.numeric(data[[p_value]]))

    ## palette
    if (missing(palette) | all(palette == 'light')) {
        palette <-
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
    } else if (all(palette == 'dark')) {
        palette <-
            rev(c('#67001f',
                  '#b2182b',
                  '#d6604d',
                  '#f4a582',
                  '#fddbc7',
                  '#ffffbf',
                  '#d1e5f0',
                  '#92c5de',
                  '#4393c3',
                  '#2166ac',
                  '#053061'
            ))
    } else {
        stopifnot(is.character(palette))
    }

    ## theme
    base_theme <-
        theme(
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_blank(),
            legend.key = element_blank(),
            axis.text.x.top  = element_text(angle = 90, hjust = 0)
        )

    if (!y_labs) {
        base_theme <-
            base_theme + theme(axis.text.y = element_blank())
    }

    black_theme <-
        base_theme + theme(
            text = element_text(colour = 'white'),
            axis.text = element_text(colour = 'white'),
            panel.background = element_rect(fill = '#454545'),
            plot.background = element_rect(fill = '#333333'),
            legend.background = element_rect(fill = '#333333')

        )

    white_theme <-
        base_theme + theme(
            text = element_text(colour = 'black'),
            axis.text = element_text(colour = 'black'),
            panel.background = element_rect(fill = 'white'),
            plot.background = element_rect(fill = 'white'),
            legend.background = element_rect(fill = 'white')
        )


    if (all(class(thme) %in% c("theme", "gg"))) {
        thme <- thme
    } else if (first(thme == 'white')) {
        thme <- white_theme
    } else if (first(thme == 'black')) {
        thme <- black_theme
    } else {
        stop('Invalid thme specification')
    }

    ## size scale
    if (!missing(legend_bubble_colour)) {
        stopifnot(is.character(legend_bubble_colour))
        bubble_colour <- legend_bubble_colour
    } else if (is.null(thme$legend.text$colour) & is.null(thme$axis.text$colour) & is.null(thme$text$colour)) {
        bubble_colour <- 'black'
    } else if (!is.null(thme$legend.text$colour)) {
        bubble_colour <- thme$legend.text$colour
    } else if (!is.null(thme$axis.text$colour)) {
        bubble_colour <- thme$axis.text$colour
    } else if (!is.null(thme$text$colour)) {
        bubble_colour <- thme$text$colour
    } else {
        stop('Cannot figure out legend bubble colour')
    }

    size_params <-
        list(
            name = p_value_legend_title,
            guide = guide_legend(override.aes = list(shape = 21, colour = bubble_colour)),
            breaks = p_value_breaks,
            labels = p_value_labels,
            limits = p_value_limits,
            max_size = max_size
        )

    size_scale <- do.call(scale_size_area, size_params)

    ##colour and fill scale

    if (!missing(n_beta_breaks)) {
        stopifnot(is.numeric(n_beta_breaks))

        max_clr <-  max(abs(data[[beta]]))
        beta_breaks <- seq(-1*max_clr, max_clr, length.out = n_beta_breaks)

        if (missing(beta_limits)) {
            beta_limits <- c(-1*max_clr - 0.1, max_clr + 0.1)
        }

    }



    colour_params <-
        list(
            colours = palette,
            name = beta_legend_title,
            breaks = beta_breaks,
            labels = beta_labels,
            limits = beta_limits)

    colour_scale <-
        do.call(scale_colour_gradientn, colour_params)

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

    rainplot <-
        data %>%
        ggplot(aes_(x = as.name(x), y = as.name(y))) +
        geom_point(aes_(colour = as.name(beta), size = as.name(p_value)))  +
        colour_scale +
        size_scale +
        scale_x_discrete(position = "top") +
        thme


    if (labels) {
        base_label_params = list(
            size = label_size,
            alpha = label_alpha,
            colour = label_text_colour,
            nudge_x = label_nudge_x
        )

        if (missing(p_value_label)) {
            p_value_label <- p_value
        }

        if (missing(beta_label)) {
            beta_label <- beta
        }

        stopifnot(is.character(p_value_label))
        stopifnot(is.character(beta_label))

        p_value_label_params <-
            c(
                base_label_params,
                list(
                    mapping = aes_(label = as.name(p_value_label), fill = as.name(beta)),
                    nudge_y = label_nudge_y
                )
            )

        beta_label_params <-
            c(
                base_label_params,
                list(
                    mapping = aes_(label = as.name(beta_label), fill = as.name(beta)),
                    nudge_y = -1*label_nudge_y
                )
            )


        p_value_label <-
            do.call(geom_label, p_value_label_params)

        beta_label <-
            do.call(geom_label, beta_label_params)

        fill_scale <-
            do.call(scale_fill_gradientn, colour_params)

        rainplot <-
            rainplot +
            p_value_label +
            beta_label +
            fill_scale

    }

    if (outline) {
        point_outline <-
            geom_point(aes_(size = as.name(p_value)), shape = 21, colour = bubble_colour)

        rainplot <-
            rainplot +
            point_outline
    }

    if (narrow) {
        rainplot <-
            rainplot +
            coord_fixed()
    }

    rainplot <-
        purrr::reduce(list(rainplot, plot_elements), `+`)

    if (dendrogram & first(y_order == 'clustered')) {

        thme_dendro <- theme(
            legend.position = "right", # maintain spacing, hide labels
            legend.text = element_text(colour = 'white'), # maintain spacing, hide labels
            axis.title = element_blank(),
            #axis.title.x = element_blank(),
            #axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 0, colour = 'white'), # maintain spacing, hide labels
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

        dendro <-
            purrr::reduce(list(dendro, plot_elements), `+`)

        cowplot::plot_grid(dendro, rainplot, ncol = 2, rel_widths = c(1/3, 1))

    } else {
        invisible(rainplot)
    }

}

# Partial Evaluation ------------------------------------------------------
rainplot_ <- purrr::partial(rainplot,
                            x = 'response',
                            y = 'term',
                            beta = 'estimate',
                            p_value = 'neg_log10_pvalue_15cap',
                            y_labs = FALSE,
                            max_size = 16,
                            palette = 'light',
                            beta_legend_title = 'Beta Coefficient\n',
                            p_value_legend_title = 'Neg Log10 P Value',
                            n_beta_breaks = 5,
                            dendrogram_antialiasing = 0.012,
                            p_value_breaks = c(5, 10, 15),
                            p_value_labels = c('5', '10', '>= 15'),
                            p_value_label = 'p_value_label',
                            beta_label = 'beta_label',
                            label_size = 3,
                            label_alpha = 0.33,
                            label_text_colour = 'black',
                            label_nudge_x = 0.4,
                            label_nudge_y = 0.2)

# Creating Plots ----------------------------------------------------------

plottr <- function(data) {
    black_labeled_rainplot <- rainplot_(data,
                                        outline = FALSE,
                                        labels = TRUE,
                                        thme = 'black')

    white_labeled_rainplot <- rainplot_(data,
                                        outline = FALSE,
                                        labels = TRUE,
                                        thme = 'white')

    white_outlined_labeled_rainplot <- rainplot_(data,
                                                 outline = TRUE,
                                                 labels = TRUE,
                                                 thme = 'white')

    narrow_black_unlabeled_rainplot <-
        rainplot_(data,
                  outline = FALSE,
                  labels = FALSE,
                  narrow = TRUE,
                  thme = 'black') +
        coord_fixed()

    narrow_white_unlabeled_rainplot <-
        rainplot_(data,
                  outline = FALSE,
                  labels = FALSE,
                  narrow = TRUE,
                  thme = 'white') +
        theme(text = element_text(size = 20))

    narrow_white_outlined_unlabeled_rainplot <-
        rainplot_(data,
                  outline = TRUE,
                  labels = FALSE,
                  narrow = TRUE,
                  thme = 'white') +
        theme(text = element_text(size = 20))

    narrow_white_outlined_unlabeled_rainplot_alphabetical <-
        rainplot_(data,
                  outline = TRUE,
                  labels = FALSE,
                  narrow = TRUE,
                  y_order = 'alphabetical',
                  thme = 'white') +
        theme(text = element_text(size = 20))

    narrow_white_outlined_unlabeled_rainplot_clustered <-
        rainplot_(data,
                  outline = TRUE,
                  labels = FALSE,
                  narrow = TRUE,
                  y_order = 'clustered',
                  thme = 'white') +
        theme(text = element_text(size = 20))

    narrow_white_outlined_unlabeled_rainplot_clustered_dendro <-
        rainplot_(data,
                  outline = TRUE,
                  labels = FALSE,
                  # narrow = TRUE,
                  y_order = 'clustered',
                  dendrogram = TRUE,
                  thme = 'white',
                  plot_elements = list(theme(text = element_text(size = 20)))
                  )

    # print(black_labeled_rainplot)
    # print(white_labeled_rainplot)
    # print(white_outlined_labeled_rainplot)
    # print(narrow_black_unlabeled_rainplot)
    # print(narrow_white_unlabeled_rainplot)
    print(narrow_white_outlined_unlabeled_rainplot)
    print(narrow_white_outlined_unlabeled_rainplot_alphabetical)
    print(narrow_white_outlined_unlabeled_rainplot_clustered)
    print(narrow_white_outlined_unlabeled_rainplot_clustered_dendro)
}



# Output ------------------------------------------------------------------

pdf('plots/top25_rainplot.pdf', width = 10, height = 20)
plottr(top25_dat)
dev.off()


pdf('plots/top50_rainplot.pdf', width = 10, height = 30)
plottr(top50_dat)
dev.off()


pdf('plots/rainplot.pdf', width = 10, height = 300)
plottr(dat)
dev.off()
