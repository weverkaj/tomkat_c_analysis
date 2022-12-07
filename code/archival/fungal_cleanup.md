---
title: "fungus"
output: html_document
---






```r
asv_tab_forward = read_csv(here("data/asv_tab_forward_long.csv")) #asv table
```

```
## New names:
## * `` -> ...1
```

```
## Rows: 42601 Columns: 95
## -- Column specification -----------------------------------------------------------------------------------------------------
## Delimiter: ","
## chr  (1): ...1
## dbl (94): Bioreactor, MESO.04, MESO.11, MESO.13, MESO.16, MESO.23, MESO.24, MESO.36, MESO.40, MESO.43, MESO.44, Old.steri...
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
asv_tab_unite = read_csv(here("data/asv_unite_forward..csv")) #taxon assignments in same order as OTUs
```

```
## New names:
## * `` -> ...1
## Rows: 42601 Columns: 8-- Column specification -----------------------------------------------------------------------------------------------------
## Delimiter: ","
## chr (8): ...1, Kingdom, Phylum, Class, Order, Family, Genus, Species
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
asv_guilds = read_csv(here("data/asv_unite_forward_ff.guilds.csv")) %>% clean_names() #functional assignments
```

```
## Rows: 42601 Columns: 11
## -- Column specification -----------------------------------------------------------------------------------------------------
## Delimiter: ","
## chr (11): sequence, taxonomy, Taxon, Taxon Level, Trophic Mode, Guild, Growth Morphology, Trait, Confidence Ranking, Note...
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

Rarefy to 45316 which is the toka-057-40 depth




```r
asv_table = asv_tab_forward %>% 
  select(contains("TOKA")) %>% 
  clean_names(case = "all_caps") %>% 
  select(-TOKA_099_10) %>% 
  t() %>% 
  rrarefy(sample = min(rowSums(.))) %>% 
  t() %>% 
  as.data.frame()



colnames(asv_table) = str_replace_all(colnames(asv_table), "_", "-")
```



```r
asv_table_standard = asv_table %>% 
  t() %>% 
  decostand(method = "total") %>% 
  t() %>% 
  as.data.frame()

asv_table_guilds = asv_table_standard %>% 
  rownames_to_column() %>% 
  left_join(asv_guilds %>% rownames_to_column(), by = "rowname")
```


```r
fungal_func_sum = asv_table_guilds %>% 
  select(-trait, -sequence, -taxonomy, -taxon, - taxon_level, -notes, -citation_source, - rowname, - guild, -growth_morphology) %>% 
  filter(confidence_ranking %in% c("Probable", "Highly Probable")) %>% 
  select(-confidence_ranking) %>% 
  pivot_longer(-trophic_mode) %>% 
  group_by(name, trophic_mode) %>% 
  summarize(value = sum(value)) %>% 
  pivot_wider(names_from = trophic_mode, values_from = value) %>% 
  column_to_rownames("name")
```

```
## `summarise()` has grouped output by 'name'. You can override using the `.groups` argument.
```


```r
asv_all = asv_table_standard %>% 
  t() %>% 
  as.data.frame() %>% 
  select(where(~ any(. != 0)))

asv_10 = asv_table_standard %>% 
  select(ends_with("-10")) %>% 
  t() %>% 
  as.data.frame() %>% 
  select(where(~ any(. != 0)))

asv_40 = asv_table_standard %>% 
  select(ends_with("-40")) %>% 
  t() %>% 
  as.data.frame() %>% 
  select(where(~ any(. != 0)))
```

