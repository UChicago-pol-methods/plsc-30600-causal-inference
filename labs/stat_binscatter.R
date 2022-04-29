StatBinscatter <- ggplot2::ggproto(
  "StatBinscatter", 
  Stat,
  compute_group = function(data, scales, scalefactor = 1e-4, scalepoints = F, bins = 10) {
    bins     <- min(floor(nrow(data)/10), bins)
    x_bin    <- ggplot2::cut_number(data$x + 1e-12*runif(nrow(data)), bins)
    x_means  <- stats::ave(data$x, x_bin, FUN = mean)
    y_means  <- stats::ave(data$y, x_bin, FUN = mean)
    y_se     <- stats::ave(data$y, x_bin, FUN = sd)
    y_obs    <- stats::ave(data$y, x_bin, FUN = length)
    if (scalepoints == T){
      result   <- data.frame(x    = x_means, 
                             y    = y_means, 
                             ymax = y_means + 1.96*y_se/sqrt(y_obs),
                             ymin = y_means - 1.96*y_se/sqrt(y_obs),
                             size = scalefactor*y_obs)
    }else{
      result   <- data.frame(x    = x_means, 
                             y    = y_means, 
                             ymax = y_means + 1.96*y_se/sqrt(y_obs),
                             ymin = y_means - 1.96*y_se/sqrt(y_obs))
    }
    result   <- unique(result)
    return(result)
  },
  required_aes = c("x", "y")
)

#' Binscatter
#' 
#' Group variable on the horizontal axis into equal-sized bins and calculate group means
#' for each bin
#' @param bins Number of bins (defaults to 10)
#' @param geom Which geom to plot (defaults to "point", other options include "pointrange" or 
#' "line")
#' @examples 
#' 
#' ggplot(mpg, aes(x = displ, y = hwy)) + 
#' geom_point(alpha = .1) + 
#' stat_binscatter(color = "red")
#' 
#' ggplot(mpg, aes(x = displ, y = hwy)) + 
#' geom_point(alpha = .1) + 
#' stat_binscatter(color = "red", geom = "pointrange")
#' 
#' ggplot(diamonds, aes(x = carat, y = price, color = cut)) + 
#' stat_binscatter(bins = 20, geom = "pointrange") +
#' stat_binscatter(bins = 20, geom = "line")
#' 
stat_binscatter <- function(mapping = NULL, data = NULL, geom = "point",
                            position = "identity", na.rm = FALSE, show.legend = NA, 
                            inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatBinscatter, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}