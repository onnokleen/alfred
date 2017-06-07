## ---- eval=FALSE---------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("onnokleen/alfred")

## ------------------------------------------------------------------------
library(alfred)
df <- get_fred_series("INDPRO", "indpro")

## ------------------------------------------------------------------------
df

## ---- fig.show = "hold"--------------------------------------------------
library(ggplot2)
ggplot(df) +
  geom_line(aes(x = date, y = indpro))

## ------------------------------------------------------------------------
df_vintages <- 
  get_alfred_series("GDPC1", "rgdp",
                    observation_start = "2007-05-31",
                    real_time_start = "2008-05-31", real_time_end = "2009-03-30")
df_vintages

## ---- fig.show = "hold", fig.width = 6-----------------------------------
library(ggplot2)

ggplot(df_vintages) +
  geom_line(aes(x = date, y = rgdp, colour = as.factor(realtime_period))) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

