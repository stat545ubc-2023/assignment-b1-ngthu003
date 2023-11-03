Assignment B1
================
Thu Nguyen
2023-11-02

``` r
library(tidyverse)
library(testthat)
library(MASS)
```

## Exercise 1: Make a Function (25 points)

## Exercise 2: Document your Function (20 points)

``` r
#' @title Summarize a metric variable grouped by a categorical variable
#' @details Given a data frame, a categorical (group) variable and a numeric (metric) variable, 
#'          compute the Count, Sum, and Mean of the metric variable grouped by the group variable
#'
#' @param df a data frame object
#' @param group a string of non-numeric variable to be grouped by
#' @param metric a string of numeric metric on which statistics to be computed
#' @param ... not used.
#' @return a data frame with 4 columns: Group - Count - Sum - Mean
#'
#' @examples
#' fn.get.Mean.by.Group(Insurance, 'District', 'Claims')

fn.Summarize.by.Group <- function(df, group, metric, ...) {
  if(!(is.data.frame(df))) {
    stop('df must be a data frame.')
  }
  if(!(group %in% colnames(df))) {
    stop(paste0(group, ' is not found in the data.'))
  }
  if(!(metric %in% colnames(df))) {
    stop(paste0(metric, ' is not found in the data.'))
  }
  if(is.numeric(unlist(df[group]))) {
    stop(paste0('The group (', group, ') must not be of numeric type.'))
  }
  if(!is.numeric(unlist(df[metric]))) {
    stop(paste0('The metric (', metric, ') must be of numeric type.'))
  }
  result <- df %>% 
    group_by(get(group)) %>% 
    summarize(
      # Holders = sum(Holders),
      Count = sum(!is.na(get(metric))),
      Metric_Sum = sum(get(metric), na.rm = T),
      Metric_Mean = round(mean(get(metric), na.rm = T),2)
    ) %>% 
    ungroup() %>% 
    as.data.frame()
  
  return(result)
}

fn.Summarize.by.Group(Insurance, 'District', 'Claims')
```

    ##   get(group) Count Metric_Sum Metric_Mean
    ## 1          1    16       1381       86.31
    ## 2          2    16        891       55.69
    ## 3          3    16        553       34.56
    ## 4          4    16        326       20.38

``` r
fn.Summarize.by.Group(Insurance, 'District', 'Holders')
```

    ##   get(group) Count Metric_Sum Metric_Mean
    ## 1          1    16      10545      659.06
    ## 2          2    16       6653      415.81
    ## 3          3    16       4167      260.44
    ## 4          4    16       1994      124.62

## Exercise 3: Include examples (15 points)

Examples of correct use of the function.

``` r
fn.Summarize.by.Group(Insurance, 'District', 'Claims')
```

    ##   get(group) Count Metric_Sum Metric_Mean
    ## 1          1    16       1381       86.31
    ## 2          2    16        891       55.69
    ## 3          3    16        553       34.56
    ## 4          4    16        326       20.38

``` r
fn.Summarize.by.Group(Insurance, 'Age', 'Holders')
```

    ##   get(group) Count Metric_Sum Metric_Mean
    ## 1        <25    16       1138       71.12
    ## 2      25-29    16       2336      146.00
    ## 3      30-35    16       3007      187.94
    ## 4        >35    16      16878     1054.88

``` r
fn.Summarize.by.Group(iris, 'Species', 'Petal.Width')
```

    ##   get(group) Count Metric_Sum Metric_Mean
    ## 1     setosa    50       12.3        0.25
    ## 2 versicolor    50       66.3        1.33
    ## 3  virginica    50      101.3        2.03

``` r
fn.Summarize.by.Group(iris, 'Species', 'Petal.Length')
```

    ##   get(group) Count Metric_Sum Metric_Mean
    ## 1     setosa    50       73.1        1.46
    ## 2 versicolor    50      213.0        4.26
    ## 3  virginica    50      277.6        5.55

Examples of incorrect use of the function.

``` r
fn.Summarize.by.Group(Insurance, 'Districts', 'Claims')
```

    ## Error in fn.Summarize.by.Group(Insurance, "Districts", "Claims"): Districts is not found in the data.

``` r
fn.Summarize.by.Group(Insurance, 'District', 'Age')
```

    ## Error in fn.Summarize.by.Group(Insurance, "District", "Age"): The metric (Age) must be of numeric type.

``` r
fn.Summarize.by.Group(Insurance, 'Holders', 'Claims')
```

    ## Error in fn.Summarize.by.Group(Insurance, "Holders", "Claims"): The group (Holders) must not be of numeric type.

## Exercise 4: Test the Function (25 points)

1)  Numeric column has no NA’s

``` r
test_that('No NA', {
  df1 <- data.frame(
    Grp = c('A', 'A', 'B'),
    Val = c(1,2,10)
  )
  expect_equal(fn.Summarize.by.Group(df1, 'Grp', 'Val')[1,2], 2)
  expect_equal(fn.Summarize.by.Group(df1, 'Grp', 'Val')[1,3], 3)
}
)
```

    ## Test passed 🎉

2)  Numeric column has NA’s

``` r
test_that('With NA', {
  df1 <- data.frame(
    Grp = c('A', 'A', 'B'),
    Val = c(1,NA,10)
  )
  expect_equal(fn.Summarize.by.Group(df1, 'Grp', 'Val')[1,2], 1)
  expect_equal(fn.Summarize.by.Group(df1, 'Grp', 'Val')[1,3], 1)
})
```

    ## Test passed 🎉

3)  Numeric column has some string values

``` r
test_that('Numeric column mixed with strings', {
  df1 <- data.frame(
    Grp = c('A', 'A', 'B'),
    Val = c(1,'String',10)
  )
  expect_error(fn.Summarize.by.Group(df1, 'Grp', 'Val'))
})
```

    ## Test passed 🎊

4)  Empty data frame

``` r
test_that('Empty data frame', {
  df1 <- data.frame(
    Grp = NA,
    Val = NA
  )
  expect_error(fn.Summarize.by.Group(df1, 'Grp', 'Val'))
})
```

    ## Test passed 🎉

5)  Group/Metric arguments not found in the data frame

``` r
test_that('Group/Metric not found in df', {
  df1 <- data.frame(
    Grp = c('A', 'A', 'B'),
    Val = c(1,2,10)
  )
  expect_error(fn.Summarize.by.Group(df1, 'Group', 'Val'))
})
```

    ## Test passed 😸
