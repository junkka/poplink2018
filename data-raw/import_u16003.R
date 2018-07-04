# import_u16003

library(readr)
library(assertthat)
library(dplyr)
source("R/dates.R")

mor_person_r <- read_csv("data-raw/data/polink3/mor_person.csv")
partner_person_r <- read_csv("data-raw/data/polink3/partner_person.csv")

recode_na <- function(x){
  ifelse(is.na(x), 0, x)
}

colnames(mor_person_r) <- tolower(colnames(mor_person_r))
mor_person2 <- mor_person_r %>% 
  mutate(
    mfoddat = as_date(mfoddat),
    mdoddat = as_date(mdoddat),
    mstartdat = as_date(mstartdat),
    mslutdat = as_date(mslutdat),
    nykt = recode_na(nykt),
    nyktar = recode_na(nyktar),
    fakf = recode_na(fakf),
    fakfar = recode_na(fakfar),
    regel = recode_na(regel)
  )

assert_that(nrow(filter(mor_person2, is.na(mfoddat))) == nrow(filter(mor_person_r, mfoddat == 0)))
assert_that(nrow(filter(mor_person2, is.na(mdoddat))) == nrow(filter(mor_person_r, mdoddat == 0)))
assert_that(nrow(filter(mor_person2, is.na(mstartdat))) == nrow(filter(mor_person_r, mstartdat == 0)))
assert_that(nrow(filter(mor_person2, is.na(mslutdat))) == nrow(filter(mor_person_r, mslutdat == 0)))

load("data/mor_person.rda")

m1 <- mor_person  %>% mutate(d1 = TRUE) %>% distinct(mddbid, d1)
m2 <- mor_person2 %>% mutate(d2 = TRUE) %>% distinct(mddbid, d2)

m3 <- full_join(m1, m2)

res <- count(m3, d1, d2)

assert_that(all(unique(mor_person$mddbid) %in% unique(mor_person2$mddbid)))
assert_that(all(unique(mor_person2$mddbid) %in% unique(mor_person$mddbid)))

mor_person <- mor_person2

save(mor_person, file = "data/mor_person.rda")

# Partner person -----------------------------

colnames(partner_person_r) <- tolower(colnames(partner_person_r))

partner_person2 <- partner_person_r %>% 
  mutate(
    pfoddat = as_date(pfoddat),
    pdoddat = as_date(pdoddat),
    pstartdat = as_date(pstartdat),
    pslutdat = as_date(pslutdat),
    nykt = recode_na(nykt),
    nyktar = recode_na(nyktar),
    fakf = recode_na(fakf),
    fakfar = recode_na(fakfar),
    regel = recode_na(regel)
  )

assert_that(nrow(filter(partner_person2, is.na(pfoddat))) == nrow(filter(partner_person_r, pfoddat == 0)))
assert_that(nrow(filter(partner_person2, is.na(pdoddat))) == nrow(filter(partner_person_r, pdoddat == 0)))
assert_that(nrow(filter(partner_person2, is.na(pstartdat))) == nrow(filter(partner_person_r, pstartdat == 0)))
assert_that(nrow(filter(partner_person2, is.na(pslutdat))) == nrow(filter(partner_person_r, pslutdat == 0)))


partner_person2 <- partner_person2 %>% 
  select(pddbid, pfodfrs, pfodhfrs, pfoddat, pdoddat, pyrkebast_kod, pyrkebast_txt,nykt, nyktar, fakf, fakfar, regel) %>% 
  distinct()

load("data/partner_person.rda")

m1 <- partner_person  %>% mutate(d1 = TRUE) %>% distinct(pddbid, d1)
m2 <- partner_person2 %>% mutate(d2 = TRUE) %>% distinct(pddbid, d2)

m3 <- full_join(m1, m2)

res <- count(m3, d1, d2)

assert_that(all(unique(partner_person$pddbid) %in% unique(partner_person2$pddbid)))
assert_that(all(unique(partner_person2$pddbid) %in% unique(partner_person$pddbid)))

partner_person <- partner_person2

save(partner_person, file = "data/partner_person.rda")

# Partner ---------------------

# check how many have dual codes 
# for each pddbid count number of unique
res <- partner_person %>% group_by(pddbid) %>% 
  summarise(
    pfodfrs = length(unique(pfodfrs[pfodfrs > 0])), 
    pfodhfrs = length(unique(pfodhfrs[pfodhfrs > 0])), 
    pfoddat = length(unique(pfoddat[pfoddat > 0])), 
    pdoddat = length(unique(pdoddat[pdoddat > 0]))
  ) %>% filter(pfodfrs > 1 | pfodhfrs > 1, pfoddat > 1, pdoddat > 1)
assert_that(nrow(res) == 0)

partner <- partner_person %>% group_by(pddbid) %>% 
  summarise(
    pfodfrs = max(pfodfrs), 
    pfodhfrs = max(pfodhfrs), 
    pfoddat = max(pfoddat), 
    pdoddat = max(pdoddat),
    nykt = max(nykt),
    nyktar = min(nyktar),
    fakf = max(fakf),
    fakfar = min(fakfar),
    regel = min(regel)
  ) %>% ungroup()


save(partner, file = "data/partner.rda")