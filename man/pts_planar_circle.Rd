% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pts_planar_circle.R
\name{pts_planar_circle}
\alias{pts_planar_circle}
\title{Pts for planar circle}
\usage{
pts_planar_circle(
  c1 = 0,
  c2 = 0,
  p = 0,
  r = 1,
  c1_var = x,
  c2_var = y,
  p_var = z,
  n = 50
)
}
\arguments{
\item{c1}{[numeric], length 1, axis 1 coordinate for circle center}

\item{c2}{[numeric], length 1, axis 2 coordinate for circle center}

\item{p}{[numeric], length 1, third axis value for circle}

\item{r}{[numeric], length 1, radius of circle}

\item{c1_var}{[symbol], length 1, axis 1 variable for tibble}

\item{c2_var}{[symbol], length 1, axis 2 variable for tibble}

\item{p_var}{[symbol], length 1, planar axis variable for tibble}

\item{n}{[numeric], length 1, points to use to approximate cirlce}
}
\value{
\link{tibble} default
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
Pts for planar circle
}
