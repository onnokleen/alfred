# alfred
An R-package for obtaining vintage data from ALFRED: https://alfred.stlouisfed.org.

## Highlights
  - Simple-to-use function for downloading real-time data from ALFRED at different points in time.
  - Returns tidy data frames for further analysis, see https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html.
  
## Usage
```r
library(alfred)
# Download industrial production index releases from March 2015 for 2013.
get_alfred_series("INDPRO", "test",
                  observation_start = "2013-03-01", observation_end = "2013-06-30",
                  real_time_start = "2015-02-02", real_time_end = "2015-02-02")
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

## History
  - May 2017: First stable release on github.
