% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/png_watermarking.R
\name{scan_watermarked_directory}
\alias{scan_watermarked_directory}
\title{Scan png files in a directory}
\usage{
scan_watermarked_directory(dir, recursive = TRUE)
}
\arguments{
\item{dir}{Directory path}

\item{recursive}{Logical, default `TRUE`, see `list.files`}
}
\value{
Invisibly returns the watermarks for the files.
}
\description{
Read the watermarks of all the png files in a directory and optionally
its subdirectories and displays diagnostics about the watermarks.
}
