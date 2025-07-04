% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/png_watermarking.R
\name{read_watermarked_png}
\alias{read_watermarked_png}
\title{Read a watermarked png}
\usage{
read_watermarked_png(path, nbits = 8L)
}
\arguments{
\item{path}{Path to png file}

\item{nbits}{number of bits, nearly always 8}
}
\value{
String encoded in the png file
}
\description{
Loads a png file and converts the top left corner's binary representation
(assuming pure white vs off-white encoding from `encode_png`) to a string
}
