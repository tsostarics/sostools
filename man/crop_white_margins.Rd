% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_plot.R
\name{crop_white_margins}
\alias{crop_white_margins}
\title{A function to crop white margins of a PNG image}
\usage{
crop_white_margins(x, new_margin = 15, overwrite = FALSE)
}
\arguments{
\item{x}{path to the PNG image}

\item{new_margin}{number of white pixels lines to keep}

\item{overwrite}{Logical, default FALSE, whether to overwrite the original
file. If FALSE, a new file with _crop appended to the filename will be saved.}
}
\description{
This is taken with some slight modifications from here:
https://github.com/statnmap/SDMSelect/blob/master/R/ToolFunctions.R
}
