#' Creates color palettes for figures.
#'
#' Creates color palettes for figures using the the RColorBrewer, or the default
#' color palette I made. The first 8 colors of the palette are from B. Wong's 2011
#' publication. The additional colors I added and may change as time goes by.
#' @useDynLib phylosmith
#' @usage create_palette(color_count, colors = 'default')
#' @param color_count Number of colors to choose for palette.
#' @param colors Name of a color set from the
#' \link[=RColorBrewer]{RColorBrewer} package or a vector palete of R-accepted
#' colors.
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @importFrom grDevices colorRampPalette
#' @return palette
#' @examples
#' #create_palette(8, 'Dark2')

create_palette <- function(color_count, colors = 'default') {
  options(warn = -1)
  mycolors <- c(
    "#757575", "#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
    "#9EDA8F", "#DE9861","#565656",  "#A6CBE0",
    "#B275D8", "#82BB47", "#e0503a", "#F5E56C",
    "#949696", "#4989DE", "#E2E2E2",
    "#F7B04C", "#696bb2")
  # image(1:length(mycolors), 1, as.matrix(1:length(mycolors)), 
  # col=mycolors, xlab="", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  if (any(colors == 'default')) {
    colors <- mycolors
    if (color_count <= length(colors)) {
      return(colors[seq(color_count)])
    }
  }
  if (any(colors %in% 'rev') | any(colors %in% 'reverse')) {
    colors <- rev(mycolors)
    if (color_count <= length(mycolors)) {
      return(rev(mycolors[seq(color_count)]))
    }
  }
  if (any(!(colors %in% colors()))) {
    if (any(colors %in% rownames(RColorBrewer::brewer.pal.info))) {
      getPalette <- colorRampPalette(brewer.pal(min(c(
        color_count,
        brewer.pal.info[rownames(brewer.pal.info) == colors, 1]
      )), colors))
    } else {
      getPalette <- colorRampPalette(colors)
    }
  } else {
    getPalette <- colorRampPalette(colors)
  }
  return(getPalette(color_count))
}