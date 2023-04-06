# Pete Nelson
# EMRR-DISE
# Department of Water Resources
# created: 27 March 2023
# purpose: Delta spp co-occurrence visualization
# requires:
# modified:

# spp co-occurrence
# based on Veech, J. A. (2013). A probabilistic model for analysing species co-occurrence. Global Ecology and Biogeography, 22(2), 252-260. doi:10.1111/j.1466-8238.2012.00789.x

library(tidyverse)
library(lubridate)
library(janitor)
library(cooccur)
library(visNetwork)
library(tictoc) # use to measure the time required to run code

# FMWT 1967-2002 -----
# limited to Chipps Island and Benicia

# fmwt1 data has to be summarised, pivoted, and transposed
fmwt1 <- readRDS(file = "data/fmwt1.rds")

temp <-
  fmwt1 %>% 
  filter(IEPFishCode != "NOCATC") %>% # originally retained for the zeros but not useful here
  group_by(SampleDate, StationCode, IEPFishCode) %>% 
  summarise(Count = sum(Count)) %>% 
  arrange(SampleDate, StationCode) %>% 
  pivot_wider(id_cols = c(SampleDate, StationCode), 
              names_from = IEPFishCode, 
              values_from = Count, 
              values_fill = 0) %>% 
  ungroup() %>% 
  group_by(StationCode) %>% 
  summarise(across(c(AMESHA:SHOGOB), sum))

# transpose & convert to presence/absence
fmwt1w <-
  temp %>% 
  t() %>% 
  row_to_names(1) %>% 
  as.data.frame() %>% 
  type_convert # otherwise t() converts numeric to character

(fmwt1w <- 
    ifelse(fmwt1w>=1,1,0))

# co-occur fmwt 67-02 -----
# fish co-occurrence from Fall Midwater Trawls (1967-2002)
co_fmwt1 <- cooccur(fmwt1w, 
                    type = "spp_site", 
                    thresh = T, 
                    spp_names = T)
# try the same thing w/ thresh = F; what's different about the output?
summary(co_fmwt1)
(co_fmwt1_table <- prob.table(co_fmwt1))

## interaction plot ----
plot(co_fmwt1)

## network ----

### nodes ----
nodes <- data.frame(id = 1:nrow(fmwt1),
                    label = rownames(fmwt1),
                    color = "#606482",
                    shadow = T)

### edges -----
# edges df from the significant pairwise co-occurrences
# modified code to use data from table() output; test with finch data!
edges <- data.frame(from = co_fmwt1_table$sp1, to = co_fmwt1_table$sp2,
                    color = ifelse(co_fmwt1_table$p_lt <= 0.05, "red", "#3C3F51"),
                    dashes = ifelse(co_fmwt1_table$p_lt <= 0.05, TRUE, FALSE))

### plot -----
# now visualize the thing
visNetwork(nodes = nodes, edges = edges) %>%
  visIgraphLayout(layout = "layout_with_kk")

# To save the network
cooc_fmwt1 <-
  visNetwork(nodes = nodes, edges = edges) %>%
  visIgraphLayout(layout = "layout_with_kk") %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE,
             manipulation = TRUE) %>% visLegend()
cooc_fmwt1 %>%
  visSave(file = "output/cooc_fmwt1.html")

# FMWT 2003-2022 -----
# limited to Chipps Island and Benicia

# fmwt2 data has to be summarised, pivoted, and transposed
fmwt2 <- readRDS(file = "data/fmwt2.rds")

temp <-
  fmwt2 %>% 
  filter(IEPFishCode != "NOCATC") %>% # originally retained for the zeros but not useful here
  group_by(SampleDate, StationCode, IEPFishCode) %>% 
  summarise(Count = sum(Count)) %>% 
  arrange(SampleDate, StationCode) %>% 
  pivot_wider(id_cols = c(SampleDate, StationCode), 
              names_from = IEPFishCode, 
              values_from = Count, 
              values_fill = 0) %>% 
  ungroup() %>% 
  group_by(StationCode) %>% 
  summarise(across(c(AMESHA:SHOGOB), sum))

# transpose & convert to presence/absence
fmwt2w <-
  temp %>% 
  t() %>% 
  row_to_names(1) %>% 
  as.data.frame() %>% 
  type_convert # otherwise t() converts numeric to character

(fmwt2w <- 
    ifelse(fmwt2w>=1,1,0))

# co-occur fmwt 67-02 -----
# fish co-occurrence from Fall Midwater Trawls (1967-2002)
co_fmwt2 <- cooccur(fmwt2w, type = "spp_site", thresh = T, spp_names = T)
summary(co_fmwt2)
(co_fmwt2_table <- prob.table(co_fmwt2))

## interaction plot ----
plot(co_fmwt2)

## network ----

### nodes ----
nodes <- data.frame(id = 1:nrow(fmwt2),
                    label = rownames(fmwt2),
                    color = "#606482",
                    shadow = T)

### edges -----
# edges df from the significant pairwise co-occurrences
# modified code to use data from table() output; test with finch data!
edges <- data.frame(from = co_fmwt2_table$sp1, to = co_fmwt2_table$sp2,
                    color = ifelse(co_fmwt2_table$p_lt <= 0.05, "red", "#3C3F51"),
                    dashes = ifelse(co_fmwt2_table$p_lt <= 0.05, TRUE, FALSE))

### plot -----
# now visualize the thing
visNetwork(nodes = nodes, edges = edges) %>%
  visIgraphLayout(layout = "layout_with_kk")

# To save the network
cooc_fmwt2 <-
  visNetwork(nodes = nodes, edges = edges) %>%
  visIgraphLayout(layout = "layout_with_kk") %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE,
             manipulation = TRUE) %>% visLegend()
cooc_fmwt2 %>%
  visSave(file = "output/cooc_fmwt2.html")

##################################

# Sac R Fall beach seine data -----
# lsrf data has to be summarised, pivoted, and transposed
lsrf <- readRDS(file = "data/lsrf.rds")

temp <-
  lsrf %>% 
  mutate(month = month(SampleDate, label = T)) %>% 
  filter(IEPFishCode != "NOCATC" & # originally retained for the zeros but not useful here
           month >= "Sep" & month <= "Dec") %>% # limit to Fall months
  group_by(SampleDate, StationCode, IEPFishCode) %>% 
  summarise(Count = sum(Count)) %>% 
  arrange(SampleDate, StationCode) %>% 
  pivot_wider(id_cols = c(SampleDate, StationCode), 
              names_from = IEPFishCode, 
              values_from = Count, 
              values_fill = 0) %>% 
  ungroup() %>% 
  mutate(month = month(SampleDate, label = T)) %>% 
  group_by(StationCode) %>% 
  summarise(across(c(SACPIK:SHIGOB), sum))

# transpose & convert to presence/absence
lsrfw <-
  temp %>% 
  t() %>% 
  row_to_names(1) %>% 
  as.data.frame() %>% 
  type_convert()
  
(lsrfw <- 
  ifelse(lsrfw>=1,1,0))

# species names (may/may not be needed)
(sp_names <- tibble(row_id = c(1:length(lsrfw[,1])), species = row.names(lsrfw)))

# co-occur fall beach -----
# fish co-occurrence from Fall beach seines
co_fall1 <- cooccur(lsrfw, type = "spp_site", thresh = T, spp_names = T)
summary(co_fall1)
(co_fall1_table <- prob.table(co_fall1))

## interaction plot ----
plot(co_fall1)

## network ----

### nodes ----
nodes <- data.frame(id = 1:nrow(lsrfw),
                    label = rownames(lsrfw),
                    color = "#606482",
                    shadow = T)

# The edges df needs at least 2 columns, from and to, which should correspond with the ids from the nodes df. Since we used cooccur's numeric labels as our ids, we can use sp1 as our 'from' col and 'sp2' as our to column. (Since our network isn't directed, we could reverse these and it wouldn't matter.) We'll then add some color so our edges match our nodes such that we'll have a lighter color for co-occurrences that occur at a lower frequency than expected and a darker color for co-occurrences that occur at a hither frequency. To make the distinction between 'hither' and 'lower' even more obvious, we'll also specify that co-occurrences tha are lower than expected have a dashed line.

### edges -----
# edges df from the significant pairwise co-occurrences
# modified code to use data from table() output; test with finch data!
edges <- data.frame(from = co_fall1_table$sp1, to = co_fall1_table$sp2,
                    color = ifelse(co_fall1_table$p_lt <= 0.05, "red", "#3C3F51"),
                    dashes = ifelse(co_fall1_table$p_lt <= 0.05, TRUE, FALSE))

# edges <- data.frame(from = co_fall1$sp1, to = co_fall1$sp2,
                    color = ifelse(co_fall1$p_lt <= 0.05, "red", "#3C3F51"),
                    dashes = ifelse(co_fall1$p_lt <= 0.05, TRUE, FALSE))


### plot -----
# now visualize the thing
visNetwork(nodes = nodes, edges = edges) %>%
  visIgraphLayout(layout = "layout_with_kk")

# To save the network
cooc_fall <-
  visNetwork(nodes = nodes, edges = edges) %>%
  visIgraphLayout(layout = "layout_with_kk") %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE,
             manipulation = TRUE) %>% visLegend()
cooc_fall %>%
  visSave(file = "output/cooc_fall.html")

# Sac R Spring beach seine data ----
# lsrs data has to be summarised, pivoted, and transposed
readRDS(file = "data/lsrs.rds")

temp <-
  lsrs %>% 
  mutate(month = month(SampleDate, label = T)) %>% 
  filter(IEPFishCode != "NOCATC" & # originally retained for the zeros but not useful here
           month >= "Mar" & month <= "Jun") %>% # limit to spring months
  group_by(SampleDate, StationCode, IEPFishCode) %>% 
  summarise(Count = sum(Count)) %>% 
  arrange(SampleDate, StationCode) %>% 
  pivot_wider(id_cols = c(SampleDate, StationCode), 
              names_from = IEPFishCode, 
              values_from = Count, 
              values_fill = 0) %>% 
  ungroup() %>% 
  mutate(month = month(SampleDate, label = T)) %>% 
  group_by(StationCode) %>% 
  summarise(across(c(CHISAL:RAIKIL), sum))

# transpose & convert to presence/absence
lsrsw <-
  temp %>% 
  t() %>% 
  row_to_names(1) %>% 
  as.data.frame() %>% 
  type_convert()

(lsrsw <- 
    ifelse(lsrsw>=1,1,0))

# species names (may/may not be needed)
(sp_names <- tibble(row_id = c(1:length(lsrsw[,1])), species = row.names(lsrsw)))

# co-occur spring beach -----
# fish co-occurrence from spring beach seines
co_spring1 <- cooccur(lsrsw, type = "spp_site", thresh = T, spp_names = T)
summary(co_spring1)
(co_spring1_table <- prob.table(co_spring1))

## interaction plot ----
plot(co_spring1)

## network ----

### nodes ----
nodes <- data.frame(id = 1:nrow(lsrsw),
                    label = rownames(lsrsw),
                    color = "#606482",
                    shadow = T)

# The edges df needs at least 2 columns, from and to, which should correspond with the ids from the nodes df. Since we used cooccur's numeric labels as our ids, we can use sp1 as our 'from' col and 'sp2' as our to column. (Since our network isn't directed, we could reverse these and it wouldn't matter.) We'll then add some color so our edges match our nodes such that we'll have a lighter color for co-occurrences that occur at a lower frequency than expected and a darker color for co-occurrences that occur at a hither frequency. To make the distinction between 'hither' and 'lower' even more obvious, we'll also specify that co-occurrences tha are lower than expected have a dashed line.

### edges -----
# edges df from the significant pairwise co-occurrences
# modified code to use data from table() output; test with finch data!
edges <- data.frame(from = co_fall1_table$sp1, to = co_fall1_table$sp2,
                    color = ifelse(co_fall1_table$p_lt <= 0.05, "red", "#3C3F51"),
                    dashes = ifelse(co_fall1_table$p_lt <= 0.05, TRUE, FALSE))

# edges <- data.frame(from = co_fall1$sp1, to = co_fall1$sp2, color = ifelse(co_fall1$p_lt <= 0.05, "red", "#3C3F51"), dashes = ifelse(co_fall1$p_lt <= 0.05, TRUE, FALSE))


### plot -----
# now visualize the thing
visNetwork(nodes = nodes, edges = edges) %>%
  visIgraphLayout(layout = "layout_with_kk")

# To save the network
cooc_spring <-
  visNetwork(nodes = nodes, edges = edges) %>%
  visIgraphLayout(layout = "layout_with_kk") %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE,
             manipulation = TRUE) %>% visLegend()
cooc_spring %>%
  visSave(file = "output/cooc_spring.html")

# SCRATCH ############################

# example w 2 spp -----
# https://thatdarndata.com/understanding-species-co-occurrence/
# define number of sites
N <- 4
# define number of sites occupied by sp 1
n1 <- 2
# define number of sites occupied by sp 2
n2 <- 2
# number of sites spp 1 and 2 co-occur
j <- 1

# prob that spp 1 and 2 occur at exactly 1 site
choose(N, j) * choose(N - j, n2 - j) * choose(N - n2, n1 - j)/(choose(N, n2) * choose(N, n1))

# assessing species co-occurrence significance
# define number of sites
N <- 30
# define number of sites occupied by sp 1
n1 <- 10
# define number of sites occupied by sp 2
n2 <- 25
# number of sites spp 1 and 2 co-occur
j <- max(0, n1 + n2 - N):min(n1, n2)

# prob that spp 1 and 2 occur at exactly j sites
pj <- choose(N, j) * choose(N - j, n2 - j) * choose(N - n2, n1 - j)/(choose(N, n2) * choose(N, n1))
# show table for j, pj, and the cumulative distribution
round(data.frame(j, pj, sumPj = cumsum(pj)), 4)

# expected number of co-occurrences
sum(pj * j)

# The probability of the two lizard species randomly co-occurring at 6 sites or less is 0.0312 (p5 + p6). Assuming a significance level of 0.05, we can conclude the two lizard species occur less frequently than expected by chance. On the other hand, the probability of the two lizard species co-occurring at 6 sites or more is 0.9982 (p6 + p7 + p8 + p9 + p10 or 1 – p5). Additionally, the expected co-occurrence is 8 sites.

# how do you extend this to multiple pairs of spp? use package cooccur
# see https://thatdarndata.com/how-to-create-co-occurrence-networks-with-the-r-packages-cooccur-and-visnetwork/

# finches ----
# begins w presence-absence data...no abundance needed
# example uses the finches data set from the 'cooccur' package

# load data
data(finches)
head(finches)
dim(finches)
cooccur.finches <- cooccur(mat=finches,
                           type="spp_site",
                           thresh=TRUE,
                           spp_names = TRUE)
obs.v.exp(cooccur.finches)
plot(cooccur.finches)

# find significant pairwise co-occurrences
library(tictoc)
tic()
co <- print(cooccur(finches, thresh = T, spp_names = T))
toc()
# interpreting the output: each row represents a significant interaction. In the first column, sp1, we see the numeric label associated with the first species (species 1) of each interaction. This numeric label aligns with the spp order in the data frame. For ex, since the first row in the finches data set is "Geospiza magnirostris", its numeric label is 1.
# We can match the numeric label with the sp name

rownames(finches)[co$sp1]
# Check sp1_name matches numeric label for species.
co[, 'sp1_name'] == rownames(finches)[co$sp1]
co[, 'sp2_name'] == rownames(finches)[co$sp2]

# Columns p_lt and p_gt give us our p-values. If p_lt < 0.05, then spp pair co-occurs at a frequency lower than we would expect to find by chance. If p_gt <- 0.05, the pair co-occurs at a rate hither than we would expect to find by chance. Since we've stored only significant interactions, either p-lt or p_gt will be less than 0.05 for each row.

# How do you visualize this? Plot the co-occurrence networks using 'visNetwork.' 'visNetwork' creates interactive network visualizations using two arguments: a dataframe describing the nodes in the network, and a dataframe describing the edges in the network.

# Start with the nodes data frame. At a min, the nodes df needs an id col to id each node. We'll set our ids to match the numeric labels returned with cooccur (1-13 finch spp). In addition, we'll label our nodes by spp name, specify a color, and add shadow for depth.

# create the df of the nodes in the network
nodes <- data.frame(id = 1:nrow(finches),
                    label = rownames(finches),
                    color = "#606482",
                    shadow = T)

# The edges df needs at least 2 columns, from and to, which should correspond with the ids from the nodes df. Since we used cooccur's numeric labels as our ids, we can use sp1 as our 'from' col and 'sp2' as our to column. (Since our network isn't directed, we could reverse these and it wouldn't matter.) We'll then add some color so our edges match our nodes such that we'll have a lighter color for co-occurrences that occur at a lower frequency than expected and a darker color for co-occurrences that occur at a higher frequency. To make the distinction between 'higher' and 'lower' even more obvious, we'll also specify that co-occurrences that are lower than expected have a dashed line.

# edges df from the significant pairwise co0-occurrences
edges <- data.frame(from = co$sp1, to = co$sp2,
                    color = ifelse(co$p_lt <= 0.05, "lightgreen", "#3C3F51"),
                    dashes = ifelse(co$p_lt <= 0.05, TRUE, FALSE))

# now plot the thing
visNetwork(nodes = nodes, edges = edges) %>%
  visIgraphLayout(layout = "layout_with_kk")

# experiments ----
# what are the min req for cooc in terms of num samples?
# can it handle all 0 samples or absent spp?
print(cooccur(tbl[,-1], spp_names = T))

(tbl <-
   tibble(spp = LETTERS[1:4],
          t1 = c(1,1,0,0),
          t2 = c(1,0,0,0),
          t3 = c(0,1,0,1),
          t4 = c(1,1,1,0))) # error: max <= min

(tbl <-
    tibble(spp = LETTERS[1:4],
           t1 = 1,
           t2 = 1,
           t3 = 1,
           t4 = 1)) # no error but meaningless

(tbl <-
    tibble(spp = LETTERS[1:4],
           t1 = 1,
           t2 = 1,
           t3 = 0,
           t4 = 1)) # no error but meaningless

(tbl <-
    tibble(spp = LETTERS[1:4],
           t1 = 1,
           t2 = 1,
           t3 = 5,
           t4 = 1)) # error: NaNs produced

(tbl <-
    tibble(spp = LETTERS[1:4],
           t1 = 1,
           t2 = 1,
           t3 = c(0,1,0,1),
           t4 = c(1,1,1,0))) # no error but meaningless

(tbl <-
    tibble(spp = LETTERS[1:10],
           t1 = c(0,rep(1,9)),
           t2 = c(0,0,rep(1,8)),
           t3 = sample(c(0,1),10,replace=T),
           t4 = sample(c(0,1),10,replace=T),
           t5 = sample(c(0,1),10,replace=T),
           t6 = sample(c(0,1),10,replace=T),
           t7 = sample(c(0,1),10,replace=T),
           t8 = sample(c(0,1),10,replace=T),
           t9 = sample(c(0,1),10,replace=T),
           t10 = sample(c(0,1),10,replace=T))) # worked: possibly, there's some min number of samples

(tbl <-
    tibble(spp = LETTERS[1:5],
           t1 = c(0,rep(1,4)),
           t2 = c(0,0,rep(1,3)),
           t3 = sample(c(0,1),5,replace=T),
           t4 = sample(c(0,1),5,replace=T),
           t5 = sample(c(0,1),5,replace=T),
           t6 = sample(c(0,1),5,replace=T),
           t7 = sample(c(0,1),5,replace=T),
           t8 = sample(c(0,1),5,replace=T),
           t9 = sample(c(0,1),5,replace=T),
           t10 = sample(c(0,1),5,replace=T))) # didn't work: possibly, there's some min number of samples x spp

(tbl <-
    tibble(spp = LETTERS[1:20],
           t1 = c(0,rep(1,19)),
           t2 = c(0,0,rep(1,18)),
           t3 = sample(c(0,1),20,replace=T),
           t4 = sample(c(0,1),20,replace=T),
           t5 = sample(c(0,1),20,replace=T))) # no error but meaningless

(tbl <-
    tibble(spp = c(paste(LETTERS[1:20],rev(LETTERS[1:20])), LETTERS[1:20]),
           t1 = c(0,rep(1,39)),
           t2 = c(0,0,rep(1,38)),
           t3 = sample(c(0,1),40,replace=T),
           t4 = sample(c(0,1),40,replace=T),
           t5 = sample(c(0,1),40,replace=T))) # no error but meaningless; less about num spp, more about num samples?

# this tbl (10 x 11) worked as originally constructed; now tweaking it...
(tbl <-
    tibble(spp = LETTERS[1:10],
           t1 = c(0,rep(1,9)),
           t2 = c(0,0,rep(1,8)),
           t3 = sample(c(0,1),10,replace=T),
           t4 = sample(c(0,1),10,replace=T),
           t5 = sample(c(0,1),10,replace=T),
           t6 = sample(c(0,1),10,replace=T),
           t7 = sample(c(0,1),10,replace=T),
           t8 = sample(c(0,1),10,replace=T),
           t9 = sample(c(0,1),10,replace=T),
           t10 = sample(c(0,1),10,replace=T)))
# works with 1-3 transect totals=0

set.seed(45678)
(tbl <-
    tibble(spp = LETTERS[1:10],
           t1 = c(0,rep(1,9)),
           t2 = c(0,0,rep(1,8)),
           t3 = c(0,sample(c(0,1),9,replace=T)),
           t4 = c(0,sample(c(0,1),9,replace=T)),
           t5 = c(0,sample(c(0,1),9,replace=T)),
           t6 = c(0,sample(c(0,1),9,replace=T)),
           t7 = c(0,sample(c(0,1),9,replace=T)),
           t8 = c(5,sample(c(0,1),9,replace=T)),
           t9 = c(0,sample(c(0,1),9,replace=T)),
           t10 = c(0,sample(c(0,1),9,replace=T)))) # works...patterns driven largely by sp1 (=A); due to n=5 for t8?

set.seed(45678)
(tbl <-
    tibble(spp = LETTERS[1:10],
           t1 = c(0,rep(1,9)),
           t2 = c(0,0,rep(1,8)),
           t3 = c(0,sample(c(0,1),9,replace=T)),
           t4 = c(0,sample(c(0,1),9,replace=T)),
           t5 = c(0,sample(c(0,1),9,replace=T)),
           t6 = c(0,sample(c(0,1),9,replace=T)),
           t7 = c(0,sample(c(0,1),9,replace=T)),
           t8 = c(1,sample(c(0,1),9,replace=T)), # note change
           t9 = c(0,sample(c(0,1),9,replace=T)),
           t10 = c(0,sample(c(0,1),9,replace=T)))) # works again...but what a change in the cooc table! sp1 now irrelevant

set.seed(7789)
(tbl <-
    tibble(spp = LETTERS[1:5],
           t1 = c(0,rep(1,4)),
           t2 = c(0,0,rep(1,3)),
           t3 = c(0,sample(c(0,1),4,replace=T)),
           t4 = c(0,sample(c(0,1),4,replace=T)),
           t5 = c(0,sample(c(0,1),4,replace=T)),
           t6 = c(sample(c(0,1),5,replace=T)),
           t7 = c(sample(c(0,1),5,replace=T)),
           t8 = c(sample(c(0,1),5,replace=T)),
           t9 = c(sample(c(0,1),5,replace=T)),
           t10 = c(sample(c(0,1),5,replace=T))))

print(cooccur(tbl[,-1], spp_names = T))
co1 <- cooccur(tbl[,-1], spp_names = T)
prob.table(co1)
print(cooccur(tbl[-1,-1], spp_names = T))

set.seed(345)
(df <-
    data.frame(row.names = LETTERS[1:5],
           t1 = c(0,rep(1,4)),
           t2 = c(0,0,rep(1,3)),
           t3 = c(0,sample(c(0,1),4,replace=T)),
           t4 = c(0,sample(c(0,1),4,replace=T)),
           t5 = c(0,sample(c(0,1),4,replace=T)),
           t6 = c(sample(c(0,1),5,replace=T)),
           t7 = c(sample(c(0,1),5,replace=T)),
           t8 = c(sample(c(0,1),5,replace=T)),
           t9 = c(sample(c(0,1),5,replace=T)),
           t10 = c(sample(c(0,1),5,replace=T))))

## example -----
(df <-
    data.frame(row.names=LETTERS[1:10],
               t1=c(0,0,0,0,0,1,0,0,0,1),
               t2=c(1,0,0,0,0,1,1,0,0,1),
               t3=c(0,1,1,1,1,0,1,1,1,0),
               t4=c(0,1,1,1,1,0,0,1,1,0),
               t5=c(1,1,0,1,0,1,0,1,1,0),
               t6=c(1,1,0,0,1,0,1,0,0,1),
               t7=c(1,1,0,0,1,0,1,0,0,1),
               t8=c(1,0,0,0,0,1,1,0,0,1),
               t9=c(1,0,0,0,0,1,1,0,0,1),
               t10=c(1,1,0,0,1,0,1,0,0,1)))


print(cooccur(df, spp_names = T))
ex.co <- cooccur(df, spp_names = T)
ex.table <- prob.table(ex.co)
ex.nodes <- data.frame(id = 1:nrow(df),
           label = rownames(df),
           color = "#606482",
           shadow = T)

# calls data from ex.co (cooccur() output); is there a way to make this available?
ex.edges <- data.frame(from = ex.co$sp1, to = ex.co$sp2,
                            color = ifelse(ex.co$p_lt <= 0.05, "red", "#3C3F51"),
                            dashes = ifelse(ex.co$p_lt <= 0.05, TRUE, FALSE))
ex.edges <- data.frame(from = ex.table$sp1, to = ex.table$sp2,
                       color = ifelse(ex.table$p_lt <= 0.05, "red", "#3C3F51"),
                       dashes = ifelse(ex.table$p_lt <= 0.05, TRUE, FALSE))

# now plot the thing
visNetwork(nodes = ex.nodes, edges = ex.edges) %>%
  visIgraphLayout(layout = "layout_with_kk")


cooccur.finches <- cooccur(finches,
                           type = "spp_site",
                           thresh = T,
                           spp_names = T)
summary(cooccur.finches)
prob.table(cooccur.finches)

# addl references ----
# Almende, B.V., Benoit Thieurmel, Benoit & Titouan Robert (2019). visNetwork: Network Visualization using ‘vis.js’ Library. R package version 2.0.8. https://CRAN.R-project.org/package=visNetwork

# Griffith, Daniel M., Veech, Joseph A., & Marsh, Charles J. (2016). cooccur: Probabilistic Species Co-Occurrence Analysis in R. Journal of Statistical Software, 69(2), 1-17. doi:10.18637/jss.v069.c02

usethis::create_from_github(
  "https://github.com/Pete-Nelson/ulithi_fishes",
  destdir = "G:/My Drive/GitHub Gdrive Projects/ulithi_fishes")
