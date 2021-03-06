% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/khoros_io.R
\name{write_khoros_header}
\alias{write_khoros_header}
\title{Writing a Khoros viff header}
\usage{
write_khoros_header(
  fid,
  imgE,
  imgW,
  imgH,
  imgB,
  imgT = "double",
  comment = " "
)
}
\arguments{
\item{fid}{connection to binary file for writing}

\item{imgE}{string, the endian-ness to write in.  Deprecated -- all files are written in little endian}

\item{imgW}{integer, width of image cube in pixels}

\item{imgH}{integer, height of image cube in pixels}

\item{imgB}{integer, number of image bands}

\item{imgT}{string, type of data to write.  Can be 'double','numeric' (float),'integer' (16-bit signed integer)}

\item{comment}{string, any comments about the data to include in the header}
}
\value{
None

Special note:  The data matrix is assumed to be ordered such that the first row represents the NW pixel and last row represents the SE pixel
}
\description{
Khoros data cubes contain a 1024 byte proprietary header.
}
