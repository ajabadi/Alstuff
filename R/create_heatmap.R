#' Create heatmap
#'
#' Creates a heatmap from a matrix, potentially with NA's
#'
#' @param mat A numeric matrix with features as rows
#' @param low Color for lower gradient bound
#' @param high Color for higher gradient bound
#' @param seg.color Color of cell diagonal line for missing value tiles
#' @param NA_color Color for missing value tiles
#' @param seg.shape One of 'line', 'cross', or 'none'
#' @param title Plot title
#' @param xtitle X title
#' @param ytitle Y title
#' @param show.legend show.legend
#' @param legend.title legend.title
#'
#' @return a ggplot object
#' @import ggplot2
#' @export
#'
#' @examples
#' create_heatmap(title = 'Example')
#' create_heatmap(low = 'green', high = 'purple')
#' create_heatmap(low = 'yellow', high = 'grey50')
#' create_heatmap(low = 'yellow', high = 'darkgreen')
#' create_heatmap(low = '#6193f2', high = '0042bf')
create_heatmap <-
  function(mat = matrix(rnorm(100), nrow = 20),
           low = '#a4eaff',
           high = "#1C28A3",
           seg.color = 'grey40',
           NA_color = 'grey70',
           seg.shape = c('none', 'cross', 'line'),
           title = NULL,
           xtitle = NULL,
           ytitle = NULL,
           show.legend = TRUE,
           legend.title = 'Expression') {
    require(ggplot2)

    ## ------ create the long matrix of form:
    ## FEATURE     CELL   EXPRESSION
    ## ENSG00001     A01       2.30
    p_name <- 'FEATURE'
    n_name <- 'CELL'
    add_dimnames <- function(mat, rows = p_name, cols = n_name) {
      rownames(mat) <- paste0(p_name, '_', seq_len(dim(mat)[1]))
      colnames(mat) <- paste0(n_name, '_', seq_len(dim(mat)[2]))
      mat
    }
    mat <- add_dimnames(mat)
    value_name <- 'EXPRESSION'
    df <- as.data.frame(mat) %>% tibble::rownames_to_column(var = p_name)
    df <- tidyr::pivot_longer(df, values_to = value_name, cols = seq_along(df)[-1], names_to = n_name) %>% as.data.frame()
    ## ------ ggplot it
    p <-
      ggplot(df, aes_string(x = p_name, y = n_name)) +
      geom_tile(aes_string(fill = value_name), show.legend = show.legend) +
      scale_fill_gradient(low=low, high=high, na.value = NA_color) +
      theme_void() +
      labs(title = title, x = xtitle, y = ytitle)

    if (show.legend) {
      p <- p + guides(fill = guide_legend(title = legend.title))
    }

    ## ------ titles
    if (!is.null(xtitle)) {
      p <- p + theme(axis.title.x = element_text(colour = 'black'))
    }
    if (!is.null(ytitle)) {
      p <- p + theme(axis.title.y = element_text(colour = 'black'))
    }
    if (!is.null(title)) {
      p <- p + theme(axis.title = element_text(colour = 'black'))
    }

    gb <- ggplot_build(p)
    plot_df <- gb$data[[1]]

    ## ------ NA cells
    if (any(is.na(mat))) {

      seg.shape <- match.arg(seg.shape)

      if (! (seg.shape == 'none' && any(plot_df$fill == NA_color))) {
        p <- p + geom_segment(data=plot_df[plot_df$fill == NA_color, ],
                              aes(x=xmin, xend=xmax, y=ymin, yend=ymax),
                              color=seg.color)
        if (seg.shape == 'cross') {
          p <- p +  geom_segment(data=plot_df[plot_df$fill == NA_color, ],
                                 aes(x=xmin, xend=xmax, y=ymax, yend=ymin),
                                 color=seg.color)
        }
      }
    }
    p
  }

