
<!-- README.md is generated from README.Rmd. Please edit that file -->

# [https://thmshrt.github.io/ggshapes/index.html][ggshapes]

<!-- badges: start -->

<!-- R CMD check badge -->

<!-- code coverage badge -->

<!-- cran badge -->

<!-- badges: end -->

## TLDR

1.  Install using `devtools::install_github('thmshrt/ggshapes')`
2.  Open `examples/cookbook.html`
    <!-- and `examples/visual-reference.html` -->
3.  See if the visual examples fit your use case

## Summary

The goal of ggshapes is to provide a set of geoms for adding lite
(without shading) 3 dimensional shapes into ggplots for the purpose of
creating tidy figures for publications and presentations, while
retaining the feel of the tidyverse workflow. There are three general
naming conventions `geom_unit_<shape>`, `geom_planar_<shape>`, and
`geom_<shape>`. `geom_unit_<shape>`s have fixed side lengths (e.g. for
`geom_unit_cube` side length is 1). `geom_planar_<shape>`s have fixed z
values (e.g. for `geom_planar_circle` z values must all be the same).
`geom_<shape>`s have a variety of parameters which are detailed in the
documentation.

**Implemented geoms:**

  - [x] `geom_unit_cube` for drawing a unit cube
  - [x] `geom_cube` for drawing a cube with sidelength `l`
  - [x] `geom_cuboid` for drawing a cuboid with sidelengths `lx`, `ly`,
    `lz`
      - [x] `geom_cuboid_slice` for drawing a slice of a cuboid using
        `sx`,`sxend`, `sy`,`syend`, `sz`, `szend`
      - [x] `geom_cuboid_face` for drawing a face of cuboid `face`
  - [x] `geom_planar_points` for drawing points with coordinates `x`,
    `y`, and fixed `z`
  - [x] `geom_planar_pointgon` for drawing a polygon from user provided
    coordinates `x`, `y`, and fixed `z`
  - [x] `geom_planar_ngon` for drawing a polygon with coordinates `xc`,
    `yc`, `r`, `n`, and fixed `z`
      - [x] `geom_planar_circle` for drawing a circle with center `xc`,
        `yc`, radius `r`, fixed `z`
      - [x] `geom_planar_ellipse` for drawing an ellipse with center
        `xc`, `yc`, `a`, `b`, fixed `zc`
      - [x] `geom_planar_hexagon` for drawing a hexagon with center
        `xc`, `yc`, fixed `zc`
  - [x] `geom_viewport` for drawing a quasi viewport `xc`, `yc`, `w`,
    `h`

**To be implemented:**

  - [ ] `geom_axes` for drawing axes `x`, `xarrow`, `flipx`
  - [ ] `geom_axis_labels` for drawing axis labels `xlabel`, `ylabel`,
    `zlabel`
  - [ ] `geom_ellipsoid` for drawing an ellipsoid
  - [ ] `geom_planar_path` draw a path `x` `y`, fixed `z`
      - [ ] `geom_planar_arc` draw an arc with center `xc`, `yc`, fixed
        `zc`

## Installation

This package has been submitted to [CRAN](https://CRAN.R-project.org)
and is expected to be available soon using:

``` r
install.packages("ggshapes")
```

For now, the package can be installed using

``` r
devtools::install_github('thmshrt/ggshapes')
```

## Examples

For detailed use cases, please see `examples/` or use `R`’s internal
documentation using the command `?geom_unit_cube` and then navigating to
the **Examples** section (scroll to the bottom).

## Extensions

There are several planned extensions to `ggshapes`.

**Release stage** (stable):

  - none

**Developmental stage** (public in alpha/beta):

  - `ggshapes` (this package)

  - `ggmatrixshapes` (currently part of this `ggshapes`) for drawing 2d,
    3d matrices with various annotations
    
      - [x] `geom_unit_matrix` for drawing a unit matrix (1x1x1)
      - [x] `geom_cube_matrix` for drawing a (`l`x`l`x`l`) matrix
      - [x] `geom_matrix` for drawing a (`lx`x`ly`x`lz`) matrix
          - [x] `geom_matrix_face` for drawing a face of matrix
          - [x] `geom_matrix_axes` axes
          - [x] `geom_matrix_label` labels
      - [x] `geom_matrix_slice` for drawing a slice a matrix
        `ix`,`ixend`, `iy`,`iyend`, `iz`, `izend`
          - [x] `geom_matrix_slice_face` for drawing a face of matrix
          - [x] `geom_matrix_slice_axes` axes and labels
          - [x] `geom_matrix_slice_labels` axes and labels
      - [ ] `geom_sparse_matrix` for drawing
          - [ ] `geom_sparse_matrix_face` for drawing a sparse matrix
            face
          - [ ] `geom_sparse_matrix_axes` axes
          - [ ] `geom_sparse_matrix_labels` labels
      - [ ] `geom_value_scalar` for drawing a matrix with value
      - [ ] `geom_value_matrix` for drawing a matrix with value on each
        face

**Experimental stage** (not yet public) :

  - `ggbioshapes` for drawing biological shapes as planar 3d
  - `ggchemshapes` for drawing chemical molecules as planar 3d

<!-- ## Future -->

<!--   - `grid3js` a reimplementation of `grid` for 3d purposes using `webgl` -->

<!--   - `gg3js` a reimplementation of `gg` for 3d visualization using `webgl` -->

## Alternatives

If you are looking for `tidyverse` 3d plotting, you may try `gg3d`,
however the last commit was quite a while ago.

If you are looking for non-`tidyverse` 3d plotting in `R`, `rgl` is a
strong choice due to its active development. There is also `plotly`,
which has `shiny` -like dashboard creating abilities and provides
interfaces for several languages including `R`, `python`, `js`, and
`scala`.

## Table Summary

**`ggshapes`**

| geom                   | implemented          | documented  | unit tested |
| ---------------------- | -------------------- | ----------- | ----------- |
|                        |                      | `@examples` | `testthat`  |
| `geom_unit_cube`       | :white\_check\_mark: | :x:         | :x:         |
| `geom_cube`            | :white\_check\_mark: | :x:         | :x:         |
| `geom_cuboid`          | :white\_check\_mark: | :x:         | :x:         |
| `geom_cuboid_slice`    | :white\_check\_mark: | :x:         | :x:         |
| `geom_cuboid_face`     | :white\_check\_mark: | :x:         | :x:         |
| `geom_planar_points`   | :white\_check\_mark: | :x:         | :x:         |
| `geom_planar_pointgon` | :white\_check\_mark: | :x:         | :x:         |
| `geom_planar_ngon`     | :white\_check\_mark: | :x:         | :x:         |
| `geom_planar_circle`   | :white\_check\_mark: | :x:         | :x:         |
| `geom_planar_ellipse`  | :white\_check\_mark: | :x:         | :x:         |
| `geom_planar_hexagon`  | :white\_check\_mark: | :x:         | :x:         |
| `geom_viewport`        | :white\_check\_mark: | :x:         | :x:         |

**`ggmatrixshapes` as part of `ggshapes`**

| geom                       | implemented          | documented  | unit tested |
| -------------------------- | -------------------- | ----------- | ----------- |
|                            |                      | `@examples` | `testthat`  |
| `geom_unit_matrix`         | :white\_check\_mark: | :x:         | :x:         |
| `geom_cube_matrix`         | :white\_check\_mark: | :x:         | :x:         |
| `geom_matrix`              | :white\_check\_mark: | :x:         | :x:         |
| `geom_matrix_face`         | :white\_check\_mark: | :x:         | :x:         |
| `geom_matrix_axes`         | :white\_check\_mark: | :x:         | :x:         |
| `geom_matrix_label`        | :white\_check\_mark: | :x:         | :x:         |
| `geom_matrix_slice`        | :white\_check\_mark: | :x:         | :x:         |
| `geom_matrix_slice_face`   | :white\_check\_mark: | :x:         | :x:         |
| `geom_matrix_slice_axes`   | :white\_check\_mark: | :x:         | :x:         |
| `geom_matrix_slice_labels` | :white\_check\_mark: | :x:         | :x:         |
