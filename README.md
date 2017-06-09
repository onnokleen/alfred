[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/alfred)](https://cran.r-project.org/package=alfred)
[![Travis-CI Build Status](https://travis-ci.org/onnokleen/alfred.svg?branch=master)](https://travis-ci.org/onnokleen/alfred)
[![Coverage Status](https://img.shields.io/coveralls/onnokleen/alfred.svg)](https://coveralls.io/r/onnokleen/alfred?branch=master)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
# alfred
An R-package for obtaining vintage data from ALFRED: https://alfred.stlouisfed.org. Note that this product uses the FRED&copy; API but is not endorsed or certified by the Federal Reserve Bank of St. Louis.

## Highlights
  - Simple-to-use function for downloading real-time data from ALFRED at different points in time.
  - Returns tidy data frames for further analysis, see https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html.
  - Contrary to similar R-packages, you don't have to get an API-key yourself.
  
## Usage
A more detailed example can be found in my vignette: https://cran.r-project.org/web/packages/alfred/vignettes/alfred.pdf

```r
library(alfred)
# Download industrial production index releases from March 2015 for 2013.
get_alfred_series("INDPRO", "test",
                  observation_start = "2013-03-01", observation_end = "2013-03-30",
                  realtime_start = "2015-02-02", realtime_end = "2015-02-02")
# Wrapper for getting only most recent releases 
get_fred_series("INDPRO", "indpro", observation_start = "2009-03-01", observation_end = "2009-03-01")
```

## Installation

Development version (GitHub):
```r
#install.packages("devtools")
library(devtools)
install_github("onnokleen/alfred")
```

### Terms of use

When using the FRED%copy; API, you are agreeing to be bound by the FRED® API Terms of Use, see https://research.stlouisfed.org/docs/api/terms_of_use.html.

## History
- 10.06.2017: Bug fixes
- 08.06.2017: First release on CRAN
- 2017: First stable release on github.
