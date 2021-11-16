---
title: "veg_data_cleanup"
author: "Jacob Weverka"
date: "7/12/2021"
output: html_document
---



Read in data    

```r
veg = read_csv(here("data/veg/TOKA_RMNData_2018_RMN_Plants.csv"), skip = 3, col_types = cols(`Annual Forb` = col_double())) %>% 
  janitor::clean_names() %>% 
  drop_na(annual_forb)
```

```
## Warning: 10 parsing failures.
## row         col expected    actual                                                                                    file
##   1 Annual Forb a double NA: Scrub 'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/TOKA_RMNData_2018_RMN_Plants.csv'
##   7 Annual Forb a double NA: Scrub 'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/TOKA_RMNData_2018_RMN_Plants.csv'
##  12 Annual Forb a double NA: Scrub 'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/TOKA_RMNData_2018_RMN_Plants.csv'
##  14 Annual Forb a double NA: Scrub 'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/TOKA_RMNData_2018_RMN_Plants.csv'
##  22 Annual Forb a double NA: Scrub 'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/TOKA_RMNData_2018_RMN_Plants.csv'
## ... ........... ........ ......... .......................................................................................
## See problems(...) for more details.
```

```r
newveg = veg[,-1]
points = veg[,1]
```



```r
lpi_raw = toka_lpi = read_csv(here("data/veg/toka_lpi.csv"))
```

```
## 
## -- Column specification -----------------------------------------------------------------------------------------------------------
## cols(
##   .default = col_character(),
##   Date = col_date(format = ""),
##   `Point Index` = col_double(),
##   Direction = col_double(),
##   Height = col_double(),
##   Lower7 = col_logical(),
##   Lower8 = col_logical(),
##   Lower9 = col_logical(),
##   Lower10 = col_logical(),
##   `Thatch Indices Lower` = col_number(),
##   `Thatch Top Layer` = col_double()
## )
## i Use `spec()` for the full column specifications.
```

```
## Warning: 6 parsing failures.
##  row    col           expected actual                                                                file
## 3323 Lower7 1/0/T/F/TRUE/FALSE  GAAP2 'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## 3323 Lower8 1/0/T/F/TRUE/FALSE  2GA   'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## 3336 Lower7 1/0/T/F/TRUE/FALSE  L     'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## 3344 Lower7 1/0/T/F/TRUE/FALSE  2GA   'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## 3344 Lower8 1/0/T/F/TRUE/FALSE  L     'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## .... ...... .................. ...... ...................................................................
## See problems(...) for more details.
```

```r
lpi_count = toka_lpi = read_csv(here("data/veg/toka_lpi.csv")) %>% 
  janitor::clean_names() %>% 
  filter(!is.na(soil_surface)) %>% 
  group_by(point_id, date) %>% 
  count() %>% 
  mutate(year = year(date))
```

```
## 
## -- Column specification -----------------------------------------------------------------------------------------------------------
## cols(
##   .default = col_character(),
##   Date = col_date(format = ""),
##   `Point Index` = col_double(),
##   Direction = col_double(),
##   Height = col_double(),
##   Lower7 = col_logical(),
##   Lower8 = col_logical(),
##   Lower9 = col_logical(),
##   Lower10 = col_logical(),
##   `Thatch Indices Lower` = col_number(),
##   `Thatch Top Layer` = col_double()
## )
## i Use `spec()` for the full column specifications.
```

```
## Warning: 6 parsing failures.
##  row    col           expected actual                                                                file
## 3323 Lower7 1/0/T/F/TRUE/FALSE  GAAP2 'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## 3323 Lower8 1/0/T/F/TRUE/FALSE  2GA   'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## 3336 Lower7 1/0/T/F/TRUE/FALSE  L     'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## 3344 Lower7 1/0/T/F/TRUE/FALSE  2GA   'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## 3344 Lower8 1/0/T/F/TRUE/FALSE  L     'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## .... ...... .................. ...... ...................................................................
## See problems(...) for more details.
```

```r
toka_lpi = read_csv(here("data/veg/toka_lpi.csv")) %>% 
  janitor::clean_names() %>% 
  filter(!is.na(soil_surface)) %>% 
  mutate(date = lubridate::ymd(date),
         year = lubridate::year(date)) %>% 
  select(point_id, year, top_layer, starts_with("lower")) %>% 
  janitor::remove_empty() %>% 
  pivot_longer(c(top_layer, starts_with("lower")))
```

```
## value for "which" not specified, defaulting to c("rows", "cols")
## 
## -- Column specification -----------------------------------------------------------------------------------------------------------
## cols(
##   .default = col_character(),
##   Date = col_date(format = ""),
##   `Point Index` = col_double(),
##   Direction = col_double(),
##   Height = col_double(),
##   Lower7 = col_logical(),
##   Lower8 = col_logical(),
##   Lower9 = col_logical(),
##   Lower10 = col_logical(),
##   `Thatch Indices Lower` = col_number(),
##   `Thatch Top Layer` = col_double()
## )
## i Use `spec()` for the full column specifications.
```

```
## Warning: 6 parsing failures.
##  row    col           expected actual                                                                file
## 3323 Lower7 1/0/T/F/TRUE/FALSE  GAAP2 'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## 3323 Lower8 1/0/T/F/TRUE/FALSE  2GA   'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## 3336 Lower7 1/0/T/F/TRUE/FALSE  L     'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## 3344 Lower7 1/0/T/F/TRUE/FALSE  2GA   'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## 3344 Lower8 1/0/T/F/TRUE/FALSE  L     'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## .... ...... .................. ...... ...................................................................
## See problems(...) for more details.
```

```r
toka_canopy = read_csv(here("data/veg/toka_lpi.csv")) %>% 
  janitor::clean_names() %>% 
  filter(!is.na(soil_surface)) %>% 
  mutate(date = lubridate::ymd(date),
         year = lubridate::year(date)) %>% 
  select(point_id, point_index, direction, year, starts_with("canopy")) %>% 
  janitor::remove_empty() %>% 
  filter(year == 2018) %>% 
  pivot_longer(c(starts_with("canopy")))
```

```
## value for "which" not specified, defaulting to c("rows", "cols")
## 
## -- Column specification -----------------------------------------------------------------------------------------------------------
## cols(
##   .default = col_character(),
##   Date = col_date(format = ""),
##   `Point Index` = col_double(),
##   Direction = col_double(),
##   Height = col_double(),
##   Lower7 = col_logical(),
##   Lower8 = col_logical(),
##   Lower9 = col_logical(),
##   Lower10 = col_logical(),
##   `Thatch Indices Lower` = col_number(),
##   `Thatch Top Layer` = col_double()
## )
## i Use `spec()` for the full column specifications.
```

```
## Warning: 6 parsing failures.
##  row    col           expected actual                                                                file
## 3323 Lower7 1/0/T/F/TRUE/FALSE  GAAP2 'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## 3323 Lower8 1/0/T/F/TRUE/FALSE  2GA   'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## 3336 Lower7 1/0/T/F/TRUE/FALSE  L     'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## 3344 Lower7 1/0/T/F/TRUE/FALSE  2GA   'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## 3344 Lower8 1/0/T/F/TRUE/FALSE  L     'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## .... ...... .................. ...... ...................................................................
## See problems(...) for more details.
```

```r
canopy_cover = toka_canopy %>% 
  filter(name == "canopy1",
         !is.na(value)) %>% 
  group_by(point_id, year) %>% 
  count() %>% 
  left_join(lpi_count, by = c("year", "point_id")) %>% 
  mutate(percent_cover = n.x/n.y) %>% 
  select(point_id, year, percent_cover)

canopy_sum = toka_canopy %>% 
  group_by(point_id, year) %>% 
  count(value) %>% 
  filter(!is.na(value)) %>% 
  left_join(lpi_count, by = c("year", "point_id")) %>% 
  mutate(percent_cover = n.x/n.y) %>% 
  pivot_wider(id_cols = c(point_id, year), names_from = value, values_from = percent_cover) %>% 
  mutate_all(~ replace_na(.x, 0))
```

```
## `mutate_all()` ignored the following grouping variables:
## Columns `point_id`, `year`
## Use `mutate_at(df, vars(-group_cols()), myoperation)` to silence the message.
```

```r
lpi_sum = toka_lpi %>%   
  group_by(point_id, year) %>% 
  count(value)

com_table = lpi_sum %>% 
  pivot_wider(id_cols = c(point_id, year), names_from = value, values_from = n) %>% 
  mutate_all(~ replace_na(.x, 0)) %>% 
  select(-`NA`, -L, - WL, - NOPLANT)
```

```
## `mutate_all()` ignored the following grouping variables:
## Columns `point_id`, `year`
## Use `mutate_at(df, vars(-group_cols()), myoperation)` to silence the message.
```

```r
load(here("data/veg/CAPlantsv2.RData"))

fungrps = CAPlantsv2 %>% 
  janitor::clean_names() %>% 
  select(symbol, fun_grp)

lpi_fun_sum = lpi_sum %>% 
  left_join(fungrps, by = c("value" = "symbol"))


lpi_fun_sum$fun_grp[lpi_fun_sum$value == "2GA"] = "Annual Grass"
lpi_fun_sum$fun_grp[lpi_fun_sum$value == "2FA"] = "Annual Forb"
lpi_fun_sum$fun_grp[lpi_fun_sum$value == "2GP"] = "Perennial Grass"

lpi_fun = lpi_fun_sum %>% 
  group_by(point_id, year, fun_grp) %>% 
  summarise(n = sum(n))
```

```
## `summarise()` has grouped output by 'point_id', 'year'. You can override using the `.groups` argument.
```

```r
com_table_fun = lpi_fun %>% 
  pivot_wider(id_cols = c(point_id, year), names_from = fun_grp, values_from = n) %>% 
  mutate_all(~ replace_na(.x, 0)) %>% 
  select(-`NA`) 
```

```
## `mutate_all()` ignored the following grouping variables:
## Columns `point_id`, `year`
## Use `mutate_at(df, vars(-group_cols()), myoperation)` to silence the message.
```


```r
releve = read_csv(here("data/veg/toka_releve.csv")) %>% 
  clean_names()
```

```
## 
## -- Column specification -----------------------------------------------------------------------------------------------------------
## cols(
##   `Study Area` = col_character(),
##   `Transect Name` = col_character(),
##   `Point Id` = col_character(),
##   `Event Date` = col_date(format = ""),
##   `Vegetation Type` = col_character(),
##   `USDA Code` = col_character(),
##   `Percent Cover` = col_double(),
##   Height = col_double()
## )
```

```r
woody_cover = releve %>% 
  filter(vegetation_type == "trees" | vegetation_type == "shrubs") %>% 
  mutate(year = year(event_date)) %>% 
  select(point_id, vegetation_type, usda_code, year, percent_cover) %>% 
  group_by(point_id, year, usda_code) %>% 
  summarise(percent_cover = sum(percent_cover)) %>% 
  pivot_wider(id_cols = c(point_id, year), names_from = usda_code, values_from = percent_cover) %>% 
  mutate_all(~ replace_na(.x, 0))
```

```
## `summarise()` has grouped output by 'point_id', 'year'. You can override using the `.groups` argument.
```

```
## `mutate_all()` ignored the following grouping variables:
## Columns `point_id`, `year`
## Use `mutate_at(df, vars(-group_cols()), myoperation)` to silence the message.
```

```r
woody_total = releve %>% 
  filter(vegetation_type == "trees" | vegetation_type == "shrubs") %>% 
  mutate(year = year(event_date)) %>% 
  select(point_id, vegetation_type, usda_code, year, percent_cover) %>% 
  group_by(point_id, year) %>% 
  summarise(percent_cover = sum(percent_cover))
```

```
## `summarise()` has grouped output by 'point_id'. You can override using the `.groups` argument.
```

```r
tree_cover = releve %>% 
  filter(vegetation_type == "trees") %>% 
  mutate(year = year(event_date)) %>% 
  select(point_id, vegetation_type, usda_code, year, percent_cover) %>% 
  group_by(point_id, year, usda_code) %>% 
  summarise(percent_cover = sum(percent_cover)) %>% 
  pivot_wider(id_cols = c(point_id, year), names_from = usda_code, values_from = percent_cover) %>% 
  mutate_all(~ replace_na(.x, 0))
```

```
## `summarise()` has grouped output by 'point_id', 'year'. You can override using the `.groups` argument.
## `mutate_all()` ignored the following grouping variables:
## Columns `point_id`, `year`
## Use `mutate_at(df, vars(-group_cols()), myoperation)` to silence the message.
```

```r
tree_total = releve %>% 
  filter(vegetation_type == "trees") %>% 
  mutate(year = year(event_date)) %>% 
  select(point_id, vegetation_type, usda_code, year, percent_cover) %>% 
  group_by(point_id, year) %>% 
  summarise(percent_cover = sum(percent_cover))
```

```
## `summarise()` has grouped output by 'point_id'. You can override using the `.groups` argument.
```

```r
shrub_cover = releve %>% 
  filter( vegetation_type == "shrubs") %>% 
  mutate(year = year(event_date)) %>% 
  select(point_id, vegetation_type, usda_code, year, percent_cover) %>% 
  group_by(point_id, year, usda_code) %>% 
  summarise(percent_cover = sum(percent_cover)) %>% 
  pivot_wider(id_cols = c(point_id, year), names_from = usda_code, values_from = percent_cover) %>% 
  mutate_all(~ replace_na(.x, 0))
```

```
## `summarise()` has grouped output by 'point_id', 'year'. You can override using the `.groups` argument.
## `mutate_all()` ignored the following grouping variables:
## Columns `point_id`, `year`
## Use `mutate_at(df, vars(-group_cols()), myoperation)` to silence the message.
```

```r
shrub_total = releve %>% 
  filter(vegetation_type == "shrub") %>% 
  mutate(year = year(event_date)) %>% 
  select(point_id, vegetation_type, usda_code, year, percent_cover) %>% 
  group_by(point_id, year) %>% 
  summarise(percent_cover = sum(percent_cover))
```

```
## `summarise()` has grouped output by 'point_id'. You can override using the `.groups` argument.
```







```r
my.rda = rda(newveg)
```

```r
knitr::knit(here("code/soil_data_cleanup.Rmd"))
```

```
## 
## 
## processing file: G:/My Drive/UCSB/Research/tomkat_c_analysis/code/soil_data_cleanup.Rmd
```

```
## Error in parse_block(g[-1], g[1], params.src, markdown_mode): Duplicate chunk label 'setup', which has been used for the chunk:
## knitr::opts_chunk$set(echo = TRUE)
## library(tidyverse)
## library(here)
## library(vegan)
## library(lubridate)
```
Generate the PCA

```r
data_18 = com_table %>% 
  filter(year == 2018)

points_18 = data_18$point_id

new_com_table = data_18 %>% 
  ungroup() %>% 
  mutate(id = paste(point_id, "10", sep = "-")) %>% 
  column_to_rownames("id") %>% 
  select(-point_id, -year) %>% 
  decostand(method = "normalize")

new_com_table10 = data_18 %>% 
  ungroup() %>% 
  mutate(id = paste(point_id, "10", sep = "-")) %>% 
  column_to_rownames("id") %>% 
  select(-point_id, -year) %>% 
  decostand(method = "normalize")

new_com_table40 = data_18 %>% 
  ungroup() %>% 
  mutate(id = paste(point_id, "40", sep = "-")) %>% 
  column_to_rownames("id") %>% 
  select(-point_id, -year) %>% 
  decostand(method = "normalize")

com.rda = rda(new_com_table)
com_scores = scores(com.rda, display = "sites", choices = c(1,2,3,4)) %>% 
  as.data.frame() %>% 
  add_column(point_id = points_18) %>% 
  left_join(change, by = "point_id")

com_surface = com_scores %>% 
  filter(depth_cm == "0 to 10")

com_depth = com_scores %>% 
  filter(depth_cm == "10 to 40")

com_vect = scores(com.rda, display = "species", choices = c(1,2,3)) %>% 
  as.data.frame() %>% 
  mutate(mag = sqrt(PC1^2 + PC2^2))

top_com = com_vect %>% 
  slice_max(n = 10, order_by = mag)

# ggplot(z, aes(x = PC1, y = PC2)) +
#   geom_smooth(method = "lm") +
#   geom_point(aes(color = cn_change), size = 3) +
#   geom_segment(data = com_vect, aes (x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.2, "cm"))) +
#   geom_text(data = top_com, aes(x = PC1, y = PC2, label = rownames(top_com)))
```
Do PCA axes correlate to C change or to C status in surface soils?



```r
com_change_table = com_table %>% 
  filter(year == 2018 | year == 2021) %>% 
  group_by(point_id) %>% 
  filter(n() > 1) %>% 
  mutate(id = paste(point_id, year, sep = "-")) %>% 
  column_to_rownames("id") %>% 
  ungroup() %>% 
  select(-point_id, - year) %>% 
  decostand(method = "total")

year_dist = vegdist(com_change_table, method = "bray") %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(str_detect(rowname, "2018")) %>% 
  column_to_rownames("rowname") %>% 
  select(contains("2021")) %>% 
  as.matrix()

veg_point_distance = data_frame(distance = diag(year_dist)) %>% 
  mutate(points = rownames(year_dist))
```


```r
com_change_table18 = com_table %>% 
  filter(year == 2018 | year < 2018) %>% 
  group_by(point_id) %>% 
  filter(n() > 1) %>% 
  mutate(id = paste(point_id, year, sep = "-")) %>% 
  column_to_rownames("id") %>% 
  ungroup() %>% 
  select(-point_id, - year) %>% 
  decostand(method = "total")

year_dist18 = vegdist(com_change_table18, method = "bray") %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(str_detect(rowname, "2016")) %>% 
  column_to_rownames("rowname") %>% 
  select(contains("2018")) %>% 
  as.matrix()

veg_point_distance18 = data_frame(distance = diag(year_dist18)) %>% 
  mutate(points = rownames(year_dist))
```

```
## Error: Problem with `mutate()` input `points`.
## x Input `points` can't be recycled to size 26.
## i Input `points` is `rownames(year_dist)`.
## i Input `points` must be size 26 or 1, not 15.
```

