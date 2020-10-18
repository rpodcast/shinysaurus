
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinysaurus

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

ShinySaurus is an interactive application built with the R statistical
computing language that lets you dive in to a very important issue in
statistics with a fun web interface\! Each of the data sets present in
this application come from the [Datasaurus
Dozen](http://www.thefunctionalart.com/2016/08/download-datasaurus-never-trust-summary.html)
collection authored by [Alberto Cairo](http://albertocairo.com), in
which traditional summary statistics such as the average, standard
deviation, and correlation are **very** similar across the data sets,
even with each giving a very different picture in the form of a
scatterplot\!

Within the application you can explore these data sets invidually and
see how the aforementioned summary statistics are impacted by selection
of data points. For some fun, you can visit the animate tab to produce
detailed transition data sets between the different Datasaurus sets
powered by the [{metamer}](https://eliocamp.github.io/metamer/) and
[{plotly}](https://plotly-r.com/) packages.

A detailed walk-through of the application can be found in episode 32 of
the [TidyX](https://www.youtube.com/watch?v=c7dZqyhd4a4) Screencast
series\!

To view the deployed version of the app, visit
[rpodcast.shinyapps.io/shinysaurus](https://rpodcast.shinyapps.io/shinysaurus)

## References

Here is a collection of links I used as I created the application

  - <https://eliocamp.github.io/codigo-r/en/2019/01/statistical-metamerism/>
  - <https://eliocamp.github.io/metamer/>
  - <https://itsalocke.com/datasaurus/>
  - <https://talks.cpsievert.me/20191115/#1>
  - <https://plotly-r.com/index.html>
  - <https://github.com/RinteRface/bs4Dash>

## Installation

You can install the development version of the package using the
following command:

``` r
# install.packages("remotes")
remotes::install_github("rpodcast/shinysaurus")
```

## Code of Conduct

Please note that the shinysaurus project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
