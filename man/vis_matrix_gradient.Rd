% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vis.R
\name{vis_matrix_gradient}
\alias{vis_matrix_gradient}
\title{Visualize continuous values in a matrix}
\usage{
vis_matrix_gradient(
  mat,
  scale_fxn = "linear",
  qclip = c(0, 1),
  low_color = "white",
  high_color = "navy"
)
}
\arguments{
\item{mat}{the matrix whose values will be visualized}

\item{scale_fxn}{the function used to map the values in mat to the range [0,1].
Can be either 'ecdf' for empirical distribution values or 'linear'}

\item{qclip}{two-element vector specifying a range in [0,1] of quantiles used to clamp values.
Values < lower quantile are reset to lower quantile, and values > high quantile are reset to the high quantile.
Optional, default is [0,1].}

\item{low_color}{Optional, specifies the color used for the low end of the color ramp.
Defaults to 'white'.}

\item{high_color}{Optional, specifies the color used for the high end of the color ramp.
Defaults to 'navy'}
}
\value{
nothing, visualization is produced
}
\description{
This is a heatmap of matrix values
}
