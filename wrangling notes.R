# Pete Nelson
# EMRR-DISE
# Department of Water Resources
# created: 27 March 2023
# purpose: explore ways of filtering data
# requires:
# modified: 2023-03-28

# filter 1 variable for multiple values ----
# Remove data from a tibble where a variable includes a targeted set of characters, here "unknown". First lines create an example tibble with 3 variables. Variable to be filtered is "sp" (ie species), some of which could not be identified to the lowest possible taxon. Six different coding solutions follow.

## characters ----
# example data
(dat <- tibble(sample = LETTERS[1:4], 
              sp = c("eel unknown","egg yolk jelly","English sole", "bass unknown"),
              count = c(2, 16, 5, 1)))

# solutions
dat |> filter(!grepl("unknown", sp))
filter(dat, !grepl("unknown", sp))
dat %>% filter(!grepl("unknown", sp))
dat |> filter(!str_detect(sp, "unknown"))
filter(dat, !str_detect(sp, "unknown"))
dat %>% filter(!str_detect(sp, "unknown"))

## many terms ----
# Can I filter for multiple terms simultaneously?

(dat <- tibble(sample = LETTERS[1:6], 
               sp = c("eel unknown","egg yolk jelly","English sole", "bass unknown", "Heptacarpus spp.", "unid fish"),
               count = c(2, 16, 5, 1, 0, 1)))

dat %>% filter_any(!str_detect(sp, c("unknown", "spp", "unid fish")))
dat %>% filter(!str_detect(sp, if_any("unknown" | "spp" | "unid fish")))
dat %>% filter(!str_detect(sp, if_any(c("unknown", "spp", "unid fish"))))
dat %>% filter(!str_detect(if_any(sp, c("unknown", "spp", "unid fish"))))
dat %>% filter(sp %in% c("unknown", "spp", "unid fish"))
filter(dat, sp %in% c("unknown", "spp", "unid fish")) # only gives the "unid fish" row
filter(dat, sp != c("unknown", "spp", "unid fish")) # only excludes the "unid fish" row
filter(dat, sp != "unknown", "spp", "unid fish")
filter(dat, sp != "unknown" | "spp" | "unid fish")

# stack terms in a single object
exclude <- c("bass unknown", "Heptacarpus spp.")

# from Rosie
dat %>% filter(!sp %in% exclude)

## Stackoverflow question -----
# First, I *do *have a solution to this, thanks to my colleague, Rosie Hartman, but I'd like to ask the question of the broader community to see if there are additional solutions and because I expect that others have struggled with the same or a similar problem. I was unable to find a solution here or elsewhere online.  

# Here's the problem: I'd like to be able to filter a variable based on multiple character values. I could do this with multiple filter() commands, but surely there's a more efficient way to do this. Here's some example data and a successful approach from RH:

# example data
(dat <- tibble(sample = LETTERS[1:6], 
               sp = c("eel unknown","egg yolk jelly","English sole", "bass unknown", "Heptacarpus spp.", "unid fish"),
               count = c(2, 16, 5, 1, 0, 1)))

# For the sake of the example, I want to exclude "bass unknown" and "Heptacarpus spp."
exclude <- c("bass unknown", "Heptacarpus spp.")

# One (to me, thoroughly unintuitive) solution:
dat %>% filter(!sp %in% exclude)

# Why doesn't something like this work?
filter(dat, sp != exclude)
# or
filter(dat, sp != c("bass unknown", "Heptacarpus spp."))
# or
dat %>% filter(!str_detect(sp, if_any(c("bass unknown", "Heptacarpus spp."))))

# Because I actually have many spp names that I need to exclude from the data set, multiple, successive filters would be not be efficient. I.e.,
dat %>% filter(sp != "bass unknown" & sp != "Heptacarpus spp.")
### end ----

# SCRATCH ########################################
dataset <-  # small subset of your data, rows 1-4 should match but not 5
  tribble(
    ~GENENAME,    ~Tissue1,     ~Tissue2,     ~Tissue3,
    "Gene1", "CellType_AA", "CellType_BB", "CellType_G",
    "Gene2", "CellType_AA", "CellType_BB", NA,
    "Gene3", "CellType_AA", NA, NA,
    "Gene4", "CellType_AA", "CellType_BB", "CellType_G",
    "Gene5", NA, NA, "CellType_G"
  )

desired_pattern <- "_AA"
dataset %>% 
  select(all_of(c("GENENAME", "Tissue1", "Tissue2", "Tissue3"))) %>% 
  filter(if_any(
    .cols = all_of(c("Tissue1", "Tissue2", "Tissue3")),
         .fns = ~ stringr::str_detect(.x, pattern = desired_pattern)
  ))

desired_pattern <- "CellType_G"
dataset %>% 
  filter(if_any(
    .cols = Tissue3,
    .fns = ~ stringr::str_detect(.x, pattern = desired_pattern)
  ))

dataset <-  # small subset of your data, rows 1-4 should match but not 5
  tribble(
    ~GENENAME,    ~Tissue1,     ~Tissue2,     ~Tissue3,
    "Gene1", "CellType_AA", "CellType_BB", "CellType_G",
    "Gene2", "CellType_AA", "CellType_BB", NA,
    "Gene3", "CellType_AA", NA, NA,
    "Gene4", "CellType_AA", "CellType_BB", "Cell_unknown",
    "Gene5", NA, NA, "CellType_G"
  )

desired_pattern <- "unknown"
dataset %>% 
  filter(if_any(
    .cols = Tissue3,
    .fns = ~ stringr::str_detect(.x, pattern = desired_pattern)
  ))
