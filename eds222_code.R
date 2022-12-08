library(sf)
library(feasts)
library(raster)
library(tmap)
library(tidyverse)
library(raster)
library(terra)
library(dplyr)
library(lubridate)
library(plotly)
library(ggplot2)
library(gtsummary)

rootdir <- "/Users/mallorygiesie/Desktop/MEDS/eds-222/final_project"

files <-
  dir(
    "/Users/mallorygiesie/Desktop/MEDS/eds-222/final_project/ship_data",
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.shp$"
  )
as.list(files)


# Function to read in all shape files and name them the month and year of when they were collected
read_in <- function(filepath) {
  date <- str_sub(filepath,-11,-5)
  file <- st_read(filepath) |> select(nMMSI_A)
  assign(date, file,  envir = parent.frame())
}
# Reading through list of file paths and applying the function defined above
for (i in files) {
  read_in(i)
}

df_geom <- Filter(function(x)
  is(x, "data.frame"), mget(ls()))

`2015-01` <- `2015-01` |>  mutate_all(~ replace(., is.na(.), 0))

`2015-04` <- `2015-04` |> mutate_all(~ replace(., is.na(.), 0))

`2015-07` <- `2015-07` |> mutate_all(~ replace(., is.na(.), 0))

`2015-10` <- `2015-10` |>  mutate_all(~ replace(., is.na(.), 0))


jan <- tm_shape(`2015-01`) +
  tm_polygons(
    col = "nMMSI_A",
    palette = "viridis",
    title = "Number of Unique Ships",
    breaks = c(0, 1, 100, 200, 300, 400, 500)
  ) + tm_layout(legend.outside = TRUE, title = "January 2015")


apr <- tm_shape(`2015-04`) +
  tm_polygons(
    col = "nMMSI_A",
    palette = "viridis",
    title = "Number of Unique Ships",
    breaks = c(0, 1, 100, 200, 300, 400, 500)
  ) + tm_layout(legend.outside = TRUE, title = "April 2015")

july  <- tm_shape(`2015-07`) +
  tm_polygons(
    col = "nMMSI_A",
    palette = "viridis",
    title = "Number of Unique Ships",
    breaks = c(0, 1, 100, 200, 300, 400, 500)
  ) + tm_layout(legend.outside = TRUE, title = "July 2015")

oct <- tm_shape(`2015-10`) +
  tm_polygons(
    col = "nMMSI_A",
    palette = "viridis",
    title = "Number of Unique Ships",
    breaks = c(0, 1, 100, 200, 300, 400, 500)
  ) + tm_layout(legend.outside = TRUE, title = "October 2015")

all_years <- do.call(rbind, df_geom) |>
  tibble::rownames_to_column("Cycle") |>
  mutate(hexID = str_sub(Cycle, 9)) |>
  mutate(Cycle = str_sub(Cycle, 1, 7)) |>
  select(-hexID) |>
  pivot_wider(names_from = 'Cycle',
              values_from = 'nMMSI_A') |>
  mutate_all(~ replace(., is.na(.), 0))

south <- all_years |> slice(0:3500)
north <- all_years |> slice(3501:6553)
south <- st_drop_geometry(south)
yearly_totals <- colSums(south, na.rm = T)
traffic_ts_south <- ts(yearly_totals,
                       frequency = 4,
                       start = c(2015, 1))

#plot.ts(traffic_ts_south)

traffic_ts_south <- as_tsibble(traffic_ts_south)

south_time <- traffic_ts_south  %>%
  model(classical_decomposition(value, type = "additive")) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposition of Ship Traffic in the Arctic: Bering sea from 2015-2021")

north <- st_drop_geometry(north)
yearly_totals <- colSums(north, na.rm = T)
traffic_ts_north <- ts(yearly_totals,
                       frequency = 4,
                       start = c(2015, 1))
#plot.ts(traffic_ts_north)

traffic_ts_north <- as_tsibble(traffic_ts_north)

north_time <- traffic_ts_north  %>%
  model(classical_decomposition(value, type = "additive")) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposition of Ship Traffic in the Arctic: Northern Bering and Southern Chukchi and Beaufort Seas from 2015-2021")

july_anomaly <-
  read.csv(file.path(rootdir, "july_anomaly.csv")) |> slice(37:42) |> dplyr::select(Date, X.SIA.anomaly)
oct_anomaly <-
  read.csv(file.path(rootdir, "oct_anomaly.csv")) |> slice(37:42) |> dplyr::select(Date, X.SIA.anomaly)
january_anomaly <-
  read.csv(file.path(rootdir, "january_anomaly.csv")) |> slice(37:42) |> dplyr::select(Date, X.SIA.anomaly)
april_anomaly <-
  read.csv(file.path(rootdir, "april_anomaly.csv")) |> slice(37:42) |> dplyr::select(Date, X.SIA.anomaly)

anomaly_south <-
  rbind(july_anomaly, oct_anomaly, april_anomaly, january_anomaly) |>
  arrange(Date) |>
  cbind(traffic_ts_south) |>
  dplyr::select(-index) |>
  dplyr::rename(Extent = X.SIA.anomaly, Value = value) |>
  mutate(Date = lubridate::ym(Date)) |>
  mutate(Year = lubridate::year(Date), Month = lubridate::month(Date)) |>
  mutate(Month = as.factor(Month)) |>
  mutate(Season = ifelse(Month == 1 |
                           Month == 4, "First_Half", "Last_half")) |> mutate(Season = as.factor(Season))

anomaly_north <-
  rbind(july_anomaly, oct_anomaly, april_anomaly, january_anomaly) |>
  arrange(Date) |>
  cbind(traffic_ts_north) |>
  dplyr::select(-index) |>
  dplyr::rename(Extent = X.SIA.anomaly, Value = value) |>
  mutate(Date = lubridate::ym(Date)) |>
  mutate(Year = lubridate::year(Date), Month = lubridate::month(Date)) |>
  mutate(Month = as.factor(Month)) |>
  mutate(Season = ifelse(Month == 1 |
                           Month == 4, "First_Half", "Last_half")) |> mutate(Season = as.factor(Season))

mod1 <-
  lm(Value ~ abs(Extent) + Year + Season, data = anomaly_north)
mod1 %>%
  tbl_regression(intercept = TRUE) %>%
  as_gt() %>%
  gt::tab_source_note(
    gt::md(
      "*Kelly Kapsar, Benjamin Sullender, and Aaron Poe. 2022. North Pacific and Arctic Marine Vessel Traffic Dataset (2015-2020); Hexagon Data.*"
    )
  )

anomaly_south <- anomaly_south |> mutate(Region = "south")
anomaly_north <-  anomaly_north |> mutate(Region = "north")
all_anomaly_dat <-
  rbind(anomaly_south, anomaly_north) |> mutate(Region = as.factor(Region))

mod3 <-
  lm(Value ~ Region + Season + Region:Season, data = all_anomaly_dat)

mod3 %>%
  tbl_regression(intercept = TRUE) %>%
  as_gt() %>%
  gt::tab_source_note(
    gt::md(
      "*Kelly Kapsar, Benjamin Sullender, and Aaron Poe. 2022. North Pacific and Arctic Marine Vessel Traffic Dataset (2015-2020); Hexagon Data.*"
    )
  )

mod4 <-
  lm(log(Value) ~ Extent + Year + Region + Season + Region:Season, data = all_anomaly_dat)

mod4 %>%
  tbl_regression(intercept = TRUE) %>%
  as_gt() %>%
  gt::tab_source_note(
    gt::md(
      "*Kelly Kapsar, Benjamin Sullender, and Aaron Poe. 2022. North Pacific and Arctic Marine Vessel Traffic Dataset (2015-2020); Hexagon Data.*"
    )
  )

anomaly <- all_anomaly_dat |> select(Extent)
anomaly_ts <- ts(anomaly, frequency = 4, start = c(2015, 1))

anomaly_ts <- as_tsibble(anomaly_ts)

anomaly_ts  %>%
  model(classical_decomposition(value, type = "additive")) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposition of Sea Ice Extent Anomalies in the Arctic 2015-2021")

mod <- lm(Value ~ Year, data = anomaly_south)
mod %>%
  tbl_regression(intercept = TRUE) %>%
  as_gt() %>%
  gt::tab_source_note(
    gt::md(
      "*Kelly Kapsar, Benjamin Sullender, and Aaron Poe. 2022. North Pacific and Arctic Marine Vessel Traffic Dataset (2015-2020); Hexagon Data.*"
    )
  )

mod2 <-
  lm(Value ~ Season + Year + Region + Extent, data = all_anomaly_dat)
mod2 %>%
  tbl_regression(intercept = TRUE) %>%
  as_gt() %>%
  gt::tab_source_note(
    gt::md(
      "*Kelly Kapsar, Benjamin Sullender, and Aaron Poe. 2022. North Pacific and Arctic Marine Vessel Traffic Dataset (2015-2020); Hexagon Data.*"
    )
  )

mod <- lm(Value ~ Year, data = anomaly_south)

res <- resid(mod)

interaction_errors <- ggplot(data = mod, aes(sample = res)) +
  geom_qq() +
  geom_qq_line() +
  ggtitle("Southern region residuals")

interaction_errors

mod3 <-
  lm(Value ~ Region + Season + Region:Season, data = all_anomaly_dat)

res <- resid(mod3)

interaction_errors <- ggplot(data = mod3, aes(sample = res)) +
  geom_qq() +
  geom_qq_line() +
  ggtitle("All data: season * region residuals")

interaction_errors

mod2 <-
  lm(Value ~ Season + Year + Region + Extent, data = all_anomaly_dat)

res <- resid(mod2)

interaction_errors <- ggplot(data = mod2, aes(sample = res)) +
  geom_qq() +
  geom_qq_line() +
  ggtitle("All data: all predictors residuals")

interaction_errors

mod1 <-
  lm(Value ~ abs(Extent) + Year + Season, data = anomaly_north)
res <- resid(mod1)

interaction_errors <- ggplot(data = mod1, aes(sample = res)) +
  geom_qq() +
  geom_qq_line() +
  ggtitle("Northern region residuals")

interaction_errors

mod4 <-
  lm(log(Value) ~ Extent + Year + Region + Season + Region:Season, data = all_anomaly_dat)
res <- resid(mod4)

interaction_errors <- ggplot(data = mod4, aes(sample = res)) +
  geom_qq() +
  geom_qq_line() +
  ggtitle("All Data: All predictors + region * season residuals")

interaction_errors
