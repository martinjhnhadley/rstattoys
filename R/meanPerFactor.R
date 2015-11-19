#' Calculate the mean value per factor in a data.frame
#'
#' @param data A data.frame.
#' @param fact A string.
#' @param param A number.
#' @return The mean of \code{param} split by the factor, \code{fact}, in \code{data}.
#' @section Warning:
#' This function was written to learn about package namespaces.
#' @examples
#' mean_per_factor(data.df, "species")
#' add(10, 1)

mean_per_factor <- 2


meanPerFactor <- function(factor) {
  uniqueFactors <- levels(desktopItems.df[,factor])
  meanVec <- sapply(uniqueFactors, function(x) {
    selection <- desktopItems.df[desktopItems.df[,factor] == x,]
    mean(selection$Desktop.Items)
  })
  df <- data.frame(uniqueFactor = uniqueFactors, values = meanVec)
  df$uniqueFactor <-
    factor(df$uniqueFactor, levels = as.vector(df$uniqueFactor)[order(df$values)])
  df
}
