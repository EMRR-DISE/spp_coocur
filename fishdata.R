# Pete Nelson
# EMRR-DISE
# Department of Water Resources
# created: 27 March 2023
# purpose: Delta spp co-occurrence visualization
# requires:
# modified:

# DJFMP data from EDI ----


library(tidyverse)
library(lubridate)
library(janitor)

## FMWT ----
### 1976-2001-----
# dt1, midwater trawl: Location=Chipps Island & Benicia
dt1 <- readRDS("data/dt1.rds") # problems don't affect subsequent analyses

exclude <- c("Aurelia labiata", "Blackfordia virginica", "Chinese mitten crab", "Chrysaora fuscescens", "comb jelly", "Crangon spp.", "Crangon Spp.", "Dock Shrimp", "egg yolk jelly", "Heptacarpus spp.", "lamprey unknown", "Maeotias marginata", "Moerisia sp.", "moon jelly", "No catch", "oriental shrimp", "Palaemonetes spp.", "Palaemonetes Spp.", "penicillate jellyfish", "penicillate jellyfish other", "river lamprey", "Scrippsia pacifica", "shrimp unknown", "Siberian prawn", "unid fish")

# limit data to Fall, and locations Chipps Island and Benicia; limit to identified bony fishes (no inverts, lampreys or unidentified fishes)
fmwt1 <-
  as_tibble(dt1) %>% 
  mutate(month = month(SampleDate)) %>% 
  filter(month >= 9 & month <= 12 & 
           MethodCode == "MWTR" &
           Location == "Chipps Island" | Location == "Benicia") %>% 
  select(c(3:4, 26, 28:29,48)) %>% 
  filter(!CommonName %in% exclude) %>% 
  filter(!str_detect(CommonName, "unknown"))

# save FMWT 1967-2001 data
saveRDS(fmwt1, file = "data/fmwt1.rds")

### 2002-2022 ----
# dt2, midwater trawl: Location=Chipps Island & Benicia
dt2 <- readRDS("data/dt2.rds")

exclude <- c("Aurelia labiata", "Blackfordia virginica", "Chinese mitten crab", "Chrysaora fuscescens", "comb jelly", "Crangon spp.", "Crangon Spp.", "Dock Shrimp", "egg yolk jelly", "Heptacarpus spp.", "lamprey unknown", "Maeotias marginata", "Moerisia sp.", "moon jelly", "No catch", "oriental shrimp", "Palaemonetes spp.", "Palaemonetes Spp.", "penicillate jellyfish", "penicillate jellyfish other", "river lamprey", "Scrippsia pacifica", "shrimp unknown", "Siberian prawn", "unid fish")

# limit data to Fall, and locations Chipps Island and Benicia; limit to identified bony fishes (no inverts, lampreys or unidentified fishes)
fmwt2 <-
  as_tibble(dt2) %>% 
  mutate(month = month(SampleDate)) %>% 
  filter(month >= 9 & month <= 12 & 
           MethodCode == "MWTR" &
           Location == "Chipps Island" | Location == "Benicia") %>% 
  select(c(3:4, 26, 28:29,48)) %>% 
  filter(!CommonName %in% exclude) %>% 
  filter(!str_detect(CommonName, "unknown"))

# save FMWT 2002-2022 data
saveRDS(fmwt2, file = "data/fmwt2.rds")

### complete fmwt ----
fmwt <- fmwt1 %>% full_join(fmwt2)
saveRDS(fmwt, file = "data/fmwt.rds")

## Sac R seine ----
# dt3, beach seine: Region designation for the station; 1= Lower Sacramento River, 2= North Delta, 3= Central Delta, 4= South Delta, 5= San Joaquin River, 6= Bay Area, 7= Sacramento River
# Isolate Lower Sacramento River beach seine data and remove problematic data.
# MethodCode, SiteDisturbance...no other factors seem relevant. GearConditionCode refers to how the fish were caught in the net.

dt3 <- readRDS("data/dt3.rds")
### wrangling ----

exclude <- c("Aurelia labiata", "Blackfordia virginica", "Chinese mitten crab", "Chrysaora fuscescens", "comb jelly", "Crangon spp.", "Crangon Spp.", "Dock Shrimp", "egg yolk jelly", "Heptacarpus spp.", "lamprey unknown", "Maeotias marginata", "Moerisia sp.", "moon jelly", "No catch", "oriental shrimp", "Palaemonetes spp.", "Palaemonetes Spp.", "penicillate jellyfish", "penicillate jellyfish other", "river lamprey", "Scrippsia pacifica", "shrimp unknown", "Siberian prawn", "unid fish")

### spatial factors ----
dt3 %>% group_by(RegionCode, Location) %>% summarise(n = n()) %>% print(n = Inf)
# region 2 looks promising w 12 locations...

### Fall -----
lsrf <-
  as_tibble(dt3) %>% # n = 896,018     
  filter(RegionCode == 1) %>% # limit to Lower Sac R, all entries for site disturbance, n = 206,591
  select(c(3:4,28:29,48)) %>% 
  filter(!CommonName %in% exclude) %>% # remove catch records for inverts and lampreys (and "unid fish"), n = 204,301
  filter(!str_detect(CommonName, "unknown")) %>% # remove records with uncertain identification, n = 203,963
  mutate(month = month(SampleDate)) %>% 
  filter(month >= 9 & month <= 12) %>% # n = 62,820
  select(-month)

lsrf %>% group_by(CommonName) %>% summarise(n = n()) %>% print(n = Inf)
head(lsrf)
glimpse(lsrf)
dim(lsrf) 

# save Fall lsr data
saveRDS(lsrf, file = "data/lsrf.rds")

### Spring -----
lsrs <-
  as_tibble(dt3) %>% # n = 896,018     
  filter(RegionCode == 1) %>% # limit to Lower Sac R, all entries for site disturbance, n = 206,591
  select(c(3:4,28:29,48)) %>% 
  filter(!CommonName %in% exclude) %>% # remove catch records for inverts and lampreys (and "unid fish"), n = 204,301
  filter(!str_detect(CommonName, "unknown")) %>% # remove records with uncertain identification, n = 203,963
  mutate(month = month(SampleDate)) %>% 
  filter(month >= 3 & month <= 6) %>% # n = 70,081
  select(-month)

lsrs %>% group_by(CommonName) %>% summarise(n = n()) %>% print(n = Inf)
head(lsrs)
glimpse(lsrs)
dim(lsrs) 

# save Fall lsr data
saveRDS(lsrs, file = "data/lsrs.rds")

# Sam's deltafish package ----

library(deltafish)
library(tictoc)

tic() # start clock
create_fish_db() # creates cache
toc() # stop clock (27 March 2023: 416.22 sec elapsed)

create_fish_db(update=TRUE)

surv <- open_survey()
fish <- open_fish()

# filter for sources and taxa of interest
surv_FMWT <- surv %>% 
  filter(Source == "FMWT") %>% 
  select(SampleID, Date)

fish_smelt <- fish %>% 
  filter(Taxa %in% c("Dorosoma petenense", "Morone saxatilis", "Spirinchus thaleichthys"))

# do a join and collect the resulting data frame
# collect executes the sql query and gives you a table
df <- left_join(surv_FMWT, fish_smelt) %>% 
  collect() 

# SCRATCH #####################################
# Examples of IN operator

# Check value in a Vector
67 %in% c(2,5,8,23,67,34)
45 %in% c(2,5,8,23,67,34)

# Check values from one vector present in another vector
# Compare two vector
vec1 <- c(2,5,8,23,67,34)
vec2 <- c(1,2,8,34) 
vec2 %in% vec1
vec1[vec2 %in% vec1]  

# Sequence of characters
x <- LETTERS[5:10]
y <- LETTERS[2:7]
y %in% x

## Check if any value from vector present in another vector
x <- 1:10
y <- 5:20
any(x %in% y)


## Check if all values from vector present in another vector
x <- 1:5
y <- 1:20
all(x %in% y)

# Check values from one vector present in another vector
# Return Index 
a <- c('A','B','C','D','E')
b <- c('C','D')
which(a %in% b)

# Create emp Data Frame
df=data.frame(
  emp_id=c(1,2,3),
  name=c("Smith","Rose","Williams"),
  dept_id=c(10,20,10)
)
df

# Filter DataFrame
df2 <- df[df$name %in% c('Rose'), ]
df2

# Filter by multiple values
df2 <- df[df$name %in% c('Smith','Rose'), ]
df2

# Using %in% with if_else
# Create new column
library(dplyr)
df$dept_state <- if_else(df$dept_id %in% c(10,50),'NY','CA')
df

# check if 'Smith' is present in name
'Smith' %in% df$name

# to check if any value in column name is Smith or Rose
df$name %in% c('Smith','Rose')

# Select columns using %in% operator
df[ ,(names(df) %in% "emp_id")]
df[ ,(names(df) %in% c("emp_id", 'name'))]
df %>% select_if(names(.) %in% c('emp_id', 'name'))


# Drop column using %in% operator
df[, !(colnames(df) %in% c("emp_id"))]