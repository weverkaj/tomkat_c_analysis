---
title: "16S Data"
author: "Jacob Weverka"
date: "8/4/2021"
output: html_document
---




```r
otu_table = read_table2(here("data/16S/table-w-taxonomy.txt"), skip = 1)
```

```
## Warning: 1733 parsing failures.
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
```

```
## Warning: Missing column names filled in: 'X1' [1]
```

```r
jaccard = read_tsv(here("data/16S/jaccard.tsv"))
```

```
## Warning: Missing column names filled in: 'X1' [1]
```

```r
shannon = read_tsv(here("data/16S/shannon.tsv"))
```

```
## Warning: Missing column names filled in: 'X1' [1]
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
bc = vegdist(otu_standard, method = "bray") %>% 
  as.matrix(labels = T)
bcns = vegdist(otu_straight)

NMS16S = otu_standard %>% 
  filter(row.names(.) != "TOKA-099-10") %>% 
  metaMDS( distance = "bray", k = 2, maxit = 999, trymax = 500, wascores = TRUE)
```

```
## Run 0 stress 0.1279303 
## Run 1 stress 0.1861233 
## Run 2 stress 0.1871379 
## Run 3 stress 0.206412 
## Run 4 stress 0.1536593 
## Run 5 stress 0.1695999 
## Run 6 stress 0.1696001 
## Run 7 stress 0.2010266 
## Run 8 stress 0.2041867 
## Run 9 stress 0.1864826 
## Run 10 stress 0.1422837 
## Run 11 stress 0.1280475 
## ... Procrustes: rmse 0.005993764  max resid 0.04294532 
## Run 12 stress 0.1885064 
## Run 13 stress 0.1279303 
## ... New best solution
## ... Procrustes: rmse 1.927364e-06  max resid 1.011548e-05 
## ... Similar to previous best
## Run 14 stress 0.2047662 
## Run 15 stress 0.1279815 
## ... Procrustes: rmse 0.001998834  max resid 0.01263114 
## Run 16 stress 0.2112495 
## Run 17 stress 0.1279815 
## ... Procrustes: rmse 0.001993421  max resid 0.01260567 
## Run 18 stress 0.2049726 
## Run 19 stress 0.174757 
## Run 20 stress 0.1279815 
## ... Procrustes: rmse 0.001997902  max resid 0.01262488 
## *** Solution reached
```

```r
NMS_family = family_standard %>% 
  filter(row.names(.) != "TOKA-099-10") %>% 
  metaMDS(distance = "bray", k = 2, maxit = 999, trymax = 500, wascores = TRUE)
```

```
## Run 0 stress 0.1005324 
## Run 1 stress 0.1201376 
## Run 2 stress 0.1406344 
## Run 3 stress 0.1018873 
## Run 4 stress 0.1110483 
## Run 5 stress 0.1240256 
## Run 6 stress 0.1231681 
## Run 7 stress 0.110121 
## Run 8 stress 0.1211276 
## Run 9 stress 0.1012403 
## Run 10 stress 0.1267726 
## Run 11 stress 0.1110451 
## Run 12 stress 0.1234851 
## Run 13 stress 0.1248639 
## Run 14 stress 0.1110451 
## Run 15 stress 0.1153637 
## Run 16 stress 0.1215081 
## Run 17 stress 0.1257988 
## Run 18 stress 0.1493078 
## Run 19 stress 0.1137831 
## Run 20 stress 0.1283199 
## Run 21 stress 0.1247381 
## Run 22 stress 0.1153637 
## Run 23 stress 0.1406278 
## Run 24 stress 0.1137831 
## Run 25 stress 0.1402166 
## Run 26 stress 0.1115533 
## Run 27 stress 0.1263732 
## Run 28 stress 0.1347966 
## Run 29 stress 0.1227932 
## Run 30 stress 0.1350512 
## Run 31 stress 0.1213935 
## Run 32 stress 0.1101302 
## Run 33 stress 0.110121 
## Run 34 stress 0.1101302 
## Run 35 stress 0.1274942 
## Run 36 stress 0.1329765 
## Run 37 stress 0.1248482 
## Run 38 stress 0.1354099 
## Run 39 stress 0.1141733 
## Run 40 stress 0.1287333 
## Run 41 stress 0.1241423 
## Run 42 stress 0.1215211 
## Run 43 stress 0.1239922 
## Run 44 stress 0.130519 
## Run 45 stress 0.1301061 
## Run 46 stress 0.1259027 
## Run 47 stress 0.1262079 
## Run 48 stress 0.1268133 
## Run 49 stress 0.1137831 
## Run 50 stress 0.1257253 
## Run 51 stress 0.122105 
## Run 52 stress 0.1397112 
## Run 53 stress 0.1101302 
## Run 54 stress 0.1363275 
## Run 55 stress 0.1262819 
## Run 56 stress 0.1290504 
## Run 57 stress 0.1010488 
## Run 58 stress 0.1110451 
## Run 59 stress 0.1232665 
## Run 60 stress 0.1211276 
## Run 61 stress 0.1110483 
## Run 62 stress 0.1227579 
## Run 63 stress 0.1400157 
## Run 64 stress 0.1232749 
## Run 65 stress 0.1220046 
## Run 66 stress 0.1166684 
## Run 67 stress 0.124635 
## Run 68 stress 0.1230145 
## Run 69 stress 0.1010488 
## Run 70 stress 0.1005074 
## ... New best solution
## ... Procrustes: rmse 0.02220878  max resid 0.1190231 
## Run 71 stress 0.10049 
## ... New best solution
## ... Procrustes: rmse 0.0014585  max resid 0.008763055 
## ... Similar to previous best
## *** Solution reached
```


```r
stressplot(NMS16S)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)


```r
otu_scores = scores(NMS16S) %>% 
  as.data.frame() %>% 
  rownames_to_column("point_id")

family_scores = scores(NMS_family) %>% 
  as.data.frame() %>% 
  rownames_to_column("point_id")
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

