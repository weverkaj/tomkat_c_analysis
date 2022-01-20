---
title: "16S Data"
author: "Jacob Weverka"
date: "8/4/2021"
output: html_document
---




```r
otu_table = read_table2(here("data/16S/table-w-taxonomy.txt"), skip = 1) %>% 
  select(-`TOKA-099-10`)
```

```
## Warning: `read_table2()` was deprecated in readr 2.0.0.
## Please use `read_table()` instead.
```

```
## Warning: 1732 parsing failures.
## row col   expected     actual                                                                        file
##   8  -- 68 columns 66 columns 'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/16S/table-w-taxonomy.txt'
##  15  -- 68 columns 69 columns 'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/16S/table-w-taxonomy.txt'
##  31  -- 68 columns 62 columns 'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/16S/table-w-taxonomy.txt'
##  37  -- 68 columns 62 columns 'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/16S/table-w-taxonomy.txt'
##  38  -- 68 columns 69 columns 'G:/My Drive/UCSB/Research/tomkat_c_analysis/data/16S/table-w-taxonomy.txt'
## ... ... .......... .......... ...........................................................................
## See problems(...) for more details.
```

```r
braycurtis = read_tsv(here("data/16S/braycurtis.tsv"))
jaccard = read_tsv(here("data/16S/jaccard.tsv"))
shannon = read_tsv(here("data/16S/shannon.tsv"))
```





```r
otu_straight = otu_table %>% 
  column_to_rownames("#OTU_ID") %>% 
  select(starts_with("TOKA")) %>% 
  t() %>% 
  as.data.frame() %>% 
  select_if(colSums(.) != 0)

otu_standard = decostand(otu_straight, method = "total")
```


```r
phylum_key = otu_table %>% 
  select(`#OTU_ID`, p)

class_key = otu_table %>% 
  select(`#OTU_ID`, c)


family_table = otu_straight %>% 
  rownames_to_column("point_id") %>% 
  pivot_longer(-point_id) %>% 
  left_join(phylum_key, by = c("name" = "#OTU_ID")) %>% 
  group_by(point_id, p) %>% 
  mutate(p = str_remove(p, ";")) %>% 
  summarise(family_count = sum(value)) %>% 
  pivot_wider(id_cols = point_id, names_from = p, values_from = family_count)
```

```
## `summarise()` has grouped output by 'point_id'. You can override using the `.groups` argument.
```

```r
class_table = otu_straight %>% 
  rownames_to_column("point_id") %>% 
  pivot_longer(-point_id) %>% 
  left_join(class_key, by = c("name" = "#OTU_ID")) %>% 
  group_by(point_id, c) %>% 
  mutate(c = str_remove(c, ";")) %>% 
  summarise(class_count = sum(value)) %>% 
  pivot_wider(id_cols = point_id, names_from = c, values_from = class_count)
```

```
## `summarise()` has grouped output by 'point_id'. You can override using the `.groups` argument.
```

```r
family_standard = family_table %>% 
  column_to_rownames("point_id") %>% 
  decostand(method = "total")

class_standard = class_table %>% 
  column_to_rownames("point_id") %>% 
  decostand(method = "total")
```



```r
# 
# bc = vegdist(otu_standard, method = "bray") %>% 
#   as.matrix(labels = T)
# bcns = vegdist(otu_straight)
# 
# NMS16S = otu_standard %>% 
#   filter(row.names(.) != "TOKA-099-10") %>% 
#   metaMDS( distance = "bray", k = 2, maxit = 999, trymax = 500, wascores = TRUE)
# 
# NMS_family = family_standard %>% 
#   filter(row.names(.) != "TOKA-099-10") %>% 
#   metaMDS(distance = "bray", k = 2, maxit = 999, trymax = 500, wascores = TRUE)
```


```r
# stressplot(NMS16S)
```


```r
# otu_scores = scores(NMS16S) %>% 
#   as.data.frame() %>% 
#   rownames_to_column("point_id")
# 
# family_scores = scores(NMS_family) %>% 
#   as.data.frame() %>% 
#   rownames_to_column("point_id")
```



```r
# rda.fam = family_standard %>% 
#   filter(row.names(.) != "TOKA-099-10") %>% 
#   rda()
# fam_scores = scores(rda.fam, display = "sites", choices = c(1,2,3,4)) %>% 
#   as.data.frame() %>% 
#   rownames_to_column("point_id")
# 
# rda.otu = rda(otu_standard)
# otu_scores = scores(rda.otu, display = "sites", choices = c(1,2,3,4)) %>% 
#   as.data.frame() %>% 
#   rownames_to_column("point_id")
```



```r
# 
# ggplot(fam_scores, aes(x = PC1, y = PC2)) +
#   geom_point()
# 
# ggplot(otu_scores, aes(x = PC2, y = PC3)) +
#   geom_point()
```

