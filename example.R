library(tidyverse)
library(janitor)

# small ----
# pull example from DJFMP data (2003-2022)
set.seed(2344)
(temp1 <- slice_sample(dt2[,c(4,29,48)], n = 5))
# why is dt2 (and thus ex) a list? 
dput(temp1) # to examine example data structure

# pivot wider & summarise
temp2 <- as.tibble(temp1) %>% 
  group_by(CommonName, SampleDate) %>% 
  summarise(Count = sum(Count)) %>% 
  pivot_wider(id_cols = c(SampleDate),
              names_from = CommonName,
              values_from = Count,
              values_fill = 0) %>% 
  clean_names()

# transpose
temp3 <-
  temp2 %>% 
  t() %>% 
  row_to_names(1) %>% 
  as.data.frame()

(ex <- ifelse(temp3>=1,1,0))

# large -----
set.seed(2344)
(temp1 <- slice_sample(dt2[,c(4,29,48)], n = 50))

# pivot wider & summarise
temp2 <- as.tibble(temp1) %>% 
  filter(CommonName != "No catch") %>% # remove sample efforts with no catch
  group_by(CommonName, SampleDate) %>% 
  summarise(Count = sum(Count)) %>% 
  pivot_wider(id_cols = c(SampleDate),
              names_from = CommonName,
              values_from = Count,
              values_fill = 0) %>% 
  clean_names()

# transpose
(temp3 <-
  temp2 %>% 
  t() %>% 
  row_to_names(1) %>% 
  as.data.frame())

(ex <- ifelse(temp3>=1,1,0))

# very large ----
set.seed(808)
(temp1 <- slice_sample(dt2[,c(4,29,48)], n = 150))

# pivot wider & summarise
temp2 <- as.tibble(temp1) %>% 
  filter(CommonName != "No catch") %>% # remove sample efforts with no catch
  group_by(CommonName, SampleDate) %>% 
  summarise(Count = sum(Count)) %>% 
  pivot_wider(id_cols = c(SampleDate),
              names_from = CommonName,
              values_from = Count,
              values_fill = 0) %>% 
  clean_names()

# transpose
(temp3 <-
    temp2 %>% 
    t() %>% 
    row_to_names(1) %>% 
    as.data.frame())

(ex <- ifelse(temp3>=1,1,0))

# spp cooccurrence -----
co <- cooccur(ex, spp_names = T)
plot(co)
