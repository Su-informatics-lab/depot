#' specific for evaluation.R
#' 
#' visualize cells in 2D-dimensional space
#' @param labels cell labels
#' @param cell_coords 2D embedding coordinates of cells
#' @param color.by the name of the variable in pData, defining cell groups, cells are colored based on the labels
#' @param labels.order defining the factor level of cell groups
#' @param colors.use defining the color for each cell group
#' @param brewer.use use RColorBrewer palette instead of default ggplot2 color
#' @param xlabel label of x-axis
#' @param ylabel label of y-axis
#' @param title main title of the plot
#' @param label.size font size of the legend
#' @param cell.size size of the dots
#' @param font.size font size
#' @param do.label label the cluster in 2D space
#' @param show.legend whether show the legend
#' @param show.axes whether show the axes
#' @return ggplot2 object with 2D plot

VisualizeCell <- function(labels, cell_coords, labels.order = NULL,
                             colors.use = NULL, brewer.use = FALSE,
                              xlabel = "UMAP1", ylabel = "UMAP2", title = NULL,
                              label.size = 4, cell.size = 0.3, font.size = 10,
                              do.label = F, show.legend = T, show.axes = T) {

    if (is.null(labels.order) == FALSE) {
        labels <- factor(labels, levels = labels.order)
    } else if (class(labels) != "factor") {
        labels <- as.factor(labels)
    }

    df <- data.frame(x = cell_coords[, 1], y = cell_coords[, 2], group = labels)

    gg <- ggplot(data = df, aes(x, y)) +
        geom_point(aes(colour = labels), size = cell.size) + scGCN_theme_opts() +
        theme(text = element_text(size = 12)) +
        labs(title = title, x = xlabel, y = ylabel) +
        guides(colour = guide_legend(override.aes = list(size = label.size))) +
        theme(legend.title = element_blank())

    numCluster = length(unique((labels)))
    
    if (is.null(colors.use)) {
        colors <- scPalette(numCluster)
        names(colors) <- levels(labels)
        gg <- gg + scale_color_manual(values = colors)
        if (brewer.use) {
            if (numCluster < 9) {
                colors <- RColorBrewer::brewer.pal(numCluster, "Set1")
            } else {
                colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(numCluster)
            }
            names(colors) <- levels(labels)
            gg <- gg + scale_color_manual(values = colors)
        }
    } else {
        gg <- gg + scale_color_manual(values = colors.use)
    }

    if (do.label) {
        centers <- df %>% dplyr::group_by(group) %>% dplyr::summarize(x = median(x = x), y = median(x = y))
        gg <- gg + ggrepel::geom_text_repel(data = centers, mapping = aes(x, y, label = group), size = label.size)
    }

    if (!show.legend) {
        gg <- gg + theme(legend.position = "none")
    }
    
    if (!show.axes) {
        gg <- gg + theme_void()
    }
    gg
}

#' Generate colors from a customed color palette
#' @param n number of colors
#' @return A color palette for plotting
scPalette <- function(n) {
    colorSpace <- c(
        '#E41A1C','#377EB8','#4DAF4A','#F29403','#F781BF','#BC9DCC',
        '#984EA3','#A65628','#54B0E4','#222F75','#1B9E77','#B2DF8A',
        '#E3BE00','#FB9A99','#E7298A','#910241','#00CDD1','#A6CEE3',
        '#CE1261','#8CA77B','#00441B','#5E4FA2','#DEDC00','#B3DE69',
        '#8DD3C7','#999999')
    if (n <= length(colorSpace)) {colors <- colorSpace[1:n]
    } else {
        colors <- grDevices::colorRampPalette(colorSpace)(n) }
    return(colors) }


#' ggplot theme in scGCN
#' @return
#' @export
#' @importFrom ggplot2 theme_classic element_rect theme element_blank element_line element_text
library(ggpubr)
ckd_theme_opts <- function() {
  theme(strip.background = element_rect(colour = "white", fill = "white")) +
      theme_light(base_size=15) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_blank(),
            panel.border = element_rect(colour = "black"))+
      theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))+
      theme(text = element_text(size = 12))+
      theme(
          legend.background = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA)
      )
}

