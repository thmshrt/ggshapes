% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pts_unit_viewport.R
\name{pts_unit_viewport}
\alias{pts_unit_viewport}
\title{Points for unit viewport}
\usage{
pts_unit_viewport()
}
\value{
\link{tibble} with columns
\itemize{
\item \code{x} x coordinate of point
\item \code{y} y coordinate of point
\item \code{z} z coordinate of point
\item \code{face} is face front, back, top, bottom, left or right
\item \code{tb} is the point on top, bottom or middle of face
\item \code{rl} is the point on left, right or middle of face
\item \code{point_order} points draw order
\item \code{face_order}  face draw order for default faces
}
}
\description{
Points for unit viewport
}
