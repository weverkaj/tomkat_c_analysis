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
toka_lpi = read_csv(here("data/veg/toka_lpi.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(date = lubridate::ymd(date),
         year = lubridate::year(date)) %>% 
  select(point_id, year, top_layer, starts_with("lower")) %>% 
  janitor::remove_empty() %>% 
  pivot_longer(c(top_layer, starts_with("lower")))
```

```
## value for "which" not specified, defaulting to c("rows", "cols")
```

```
## 
## -- Column specification -------------------------------------------------------------------------------------------------------------------------
## cols(
##   .default = col_character(),
##   Date = col_date(format = ""),
##   `Point Index` = col_double(),
##   Direction = col_double(),
##   Height = col_double(),
##   Lower6 = col_logical(),
##   Lower7 = col_logical(),
##   Lower8 = col_logical(),
##   Lower9 = col_logical(),
##   Lower10 = col_logical(),
##   `Thatch Indices Lower` = col_double(),
##   `Thatch Top Layer` = col_double()
## )
## [36mi[39m Use `spec()` for the full column specifications.
```

```
## Warning: 35 parsing failures.
##  row                  col               expected actual                                                                file
## 1103 Lower6               1/0/T/F/TRUE/FALSE        L   'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## 2080 Thatch Indices Lower no trailing characters    2,3 'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## 2082 Thatch Indices Lower no trailing characters    2,3 'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## 2221 Lower6               1/0/T/F/TRUE/FALSE        L   'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## 2268 Lower6               1/0/T/F/TRUE/FALSE        L   'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/veg/toka_lpi.csv'
## .... .................... ...................... ...... ...................................................................
## See problems(...) for more details.
```

```r
lpi_sum = toka_lpi %>%   
  group_by(point_id, year) %>% 
  count(value)

com_table = lpi_sum %>% 
  pivot_wider(id_cols = c(point_id, year), names_from = value, values_from = n) %>% 
  mutate_all(~ replace_na(.x, 0)) %>% 
  select(-`NA`, -L, - WL, - NOPLANT) %>% 
  filter(point_id != "TOKA-013",
         point_id != "TOKA-006")
```

```
## `mutate_all()` ignored the following grouping variables:
## Columns `point_id`, `year`
## Use `mutate_at(df, vars(-group_cols()), myoperation)` to silence the message.
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
```
Generate the PCA

```r
data_18 = com_table %>% 
  filter(year == 2018)

points_18 = data_18$point_id

new_com_table = data_18 %>% 
  ungroup() %>% 
  select(-point_id, -year) %>% 
  decostand(method = "normalize")

com.rda = rda(new_com_table)
com_scores = scores(com.rda, display = "sites", choices = c(1,2,3,4)) %>% 
  as.data.frame() %>% 
  add_column(point_id = points_18) %>% 
  left_join(change, by = "point_id")
```

```
## Error in is.data.frame(y): object 'change' not found
```

```r
com_surface = com_scores %>% 
  filter(depth_cm == "0 to 10")
```

```
## Error in filter(., depth_cm == "0 to 10"): object 'com_scores' not found
```

```r
com_depth = com_scores %>% 
  filter(depth_cm == "10 to 40")
```

```
## Error in filter(., depth_cm == "10 to 40"): object 'com_scores' not found
```

```r
com_vect = scores(com.rda, display = "species", choices = c(1,2,3)) %>% 
  as.data.frame() %>% 
  mutate(mag = sqrt(PC1^2 + PC2^2))

top_com = com_vect %>% 
  slice_max(n = 10, order_by = mag)

ggplot(z, aes(x = PC1, y = PC2)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = cn_change), size = 3) +
  geom_segment(data = com_vect, aes (x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.2, "cm"))) +
  geom_text(data = top_com, aes(x = PC1, y = PC2, label = rownames(top_com)))
```

```
## Error in ggplot(z, aes(x = PC1, y = PC2)): object 'z' not found
```
Do PCA axes correlate to C change or to C status in surface soils?

```r
#drop outlier
z = com_surface %>% 
  filter(point_id != "TOKA-089")
```

```
## Error in filter(., point_id != "TOKA-089"): object 'com_surface' not found
```

```r
#including outlier - change
m_change = lm(c_change ~ PC1 + PC2 + PC3, data = com_surface)
```

```
## Error in is.data.frame(data): object 'com_surface' not found
```

```r
summary(m_change)
```

```
## Error in summary(m_change): object 'm_change' not found
```

```r
plot(m_change)
```

```
## Error in plot(m_change): object 'm_change' not found
```

```r
#including outlier - status
m_status = lm(c_2018 ~ PC1 + PC2 + PC3, data = com_surface)
```

```
## Error in is.data.frame(data): object 'com_surface' not found
```

```r
summary(m_status)
```

```
## Error in summary(m_status): object 'm_status' not found
```


```r
cca(new_com_table ~ c_change + c_2018, data = com_surface)
```

```
## Error in eval(match.call()$data, environment(formula), enclos = .GlobalEnv): object 'com_surface' not found
```

```r
cca(new_com_table ~ c_change + c_2018, data = com_depth)
```

```
## Error in eval(match.call()$data, environment(formula), enclos = .GlobalEnv): object 'com_depth' not found
```

```r
ca = cca(new_com_table ~ com_surface$c_change + com_surface$c_2018 + com_depth$c_change + com_depth$c_2018)
```

```
## Error in eval(predvars, data, env): object 'com_surface' not found
```

```r
anova(ca)
```

```
## Error in anova(ca): object 'ca' not found
```

Same question, but with deep soils

```r
#including outlier - change
m_change = lm(c_change ~ PC1 + PC2 + PC3, data = com_depth)
```

```
## Error in is.data.frame(data): object 'com_depth' not found
```

```r
summary(m_change)
```

```
## Error in summary(m_change): object 'm_change' not found
```

```r
plot(m_change)
```

```
## Error in plot(m_change): object 'm_change' not found
```

```r
#including outlier - status
m_status = lm(c_2018 ~ PC1 + PC2 + PC3, data = com_depth)
```

```
## Error in is.data.frame(data): object 'com_depth' not found
```

```r
summary(m_status)
```

```
## Error in summary(m_status): object 'm_status' not found
```

```r
plot(m_status)
```

```
## Error in plot(m_status): object 'm_status' not found
```



```r
veg_st = decostand(newveg, "total")
st_PCA = rda(veg_st)
veg_scores = scores(st_PCA, display = "sites") %>% 
  as.data.frame() %>% 
  add_column(points) %>% 
  left_join(change, by = "point_id")
```

```
## Error in is.data.frame(y): object 'change' not found
```

```r
PCAvect = scores(st_PCA, display = "species") %>% 
  as.data.frame()
```






```r
surface = veg_scores %>% 
  filter(depth_cm == "0 to 10")
```

```
## Error in filter(., depth_cm == "0 to 10"): object 'veg_scores' not found
```

```r
depth = veg_scores %>% 
  filter(depth_cm == "10 to 40")
```

```
## Error in filter(., depth_cm == "10 to 40"): object 'veg_scores' not found
```

```r
ggplot(surface, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = cn_change), size = 2) +
  geom_segment(data = PCAvect, aes (x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.2, "cm"))) +
  geom_text(data = PCAvect, aes(x = PC1, y = PC2, label = rownames(PCAvect))) + 
  scale_color_viridis_c()
```

```
## Error in ggplot(surface, aes(x = PC1, y = PC2)): object 'surface' not found
```

```r
ggplot(depth, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = cn_change), size = 2) +
  geom_segment(data = PCAvect, aes (x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.2, "cm"))) +
  geom_text(data = PCAvect, aes(x = PC1, y = PC2, label = rownames(PCAvect))) + 
  scale_color_brewer(palette = "RdBu") + 
  scale_color_viridis_c()
```

```
## Error in ggplot(depth, aes(x = PC1, y = PC2)): object 'depth' not found
```

```r
m1 = lm(c_pc_change ~ PC1 + PC2, data = surface)
```

```
## Error in is.data.frame(data): object 'surface' not found
```

```r
summary(m1)
```

```
## Error in summary(m1): object 'm1' not found
```

```r
m2 = lm(c_pc_change ~ PC1 + PC2 + clay, data = depth)
```

```
## Error in is.data.frame(data): object 'depth' not found
```

```r
summary(m2)
```

```
## Error in summary(m2): object 'm2' not found
```


