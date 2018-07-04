# import_u16006.R

library(readr)
library(assertthat)
library(dplyr)
source("R/dates.R")
source('data-raw/passwd.R')
# read data ------------------------

exdir = tempdir()
filename = "data-raw/data/U16006/U16006_datauttag.zip"

system(sprintf("unzip -P%s %s -d %s", passw, filename, exdir))

fnames = list.files(exdir, ".csv$")

raw_files = sapply(fnames, function(a) read_delim(file.path(exdir, a), ";", locale = locale("sv", encoding = "latin1")))

# barn ------------------------

barn_r = raw_files$FIL2_BARN.csv

colnames(barn_r) <- tolower(colnames(barn_r))
barn <- barn_r %>% 
  mutate(
    foddat = as_date(foddat),
    doddat = as_date(doddat)
  )

assert_that(nrow(filter(barn, is.na(foddat))) == length(barn_r$foddat[barn_r$foddat == 0]))
assert_that(nrow(filter(barn, is.na(doddat))) == length(barn_r$doddat[barn_r$doddat == 0]))

save(barn, file = "data/barn.rda")

# mor_person ------------------------

mor_person_r <- raw_files$FIL1_MOR_PERSON.csv

colnames(mor_person_r) <- tolower(colnames(mor_person_r))
mor_person <- mor_person_r %>% 
  mutate(
    mfoddat = as_date(mfoddat),
    mdoddat = as_date(mdoddat),
    mstartdat = as_date(mstartdat),
    mslutdat = as_date(mslutdat)
  )

assert_that(nrow(filter(mor_person, is.na(mfoddat))) == nrow(filter(mor_person_r, mfoddat == 0)))
assert_that(nrow(filter(mor_person, is.na(mdoddat))) == nrow(filter(mor_person_r, mdoddat == 0)))
assert_that(nrow(filter(mor_person, is.na(mstartdat))) == nrow(filter(mor_person_r, mstartdat == 0)))
assert_that(nrow(filter(mor_person, is.na(mslutdat))) == nrow(filter(mor_person_r, mslutdat == 0)))


save(mor_person, file = "data/mor_person.rda")

# Närvaro ----------------------------

narvaro_r <- raw_files$FIL3_NARVARO.csv

colnames(narvaro_r) <- tolower(colnames(narvaro_r))

narvaro <- narvaro_r %>% 
  mutate(
    startdat = as_date(startdat),
    slutdat = as_date(slutdat)
  )

assert_that(nrow(filter(narvaro, is.na(startdat))) == nrow(filter(narvaro_r, startdat == 0)))
assert_that(nrow(filter(narvaro, is.na(slutdat))) == nrow(filter(narvaro_r, slutdat == 0)))

save(narvaro, file = "data/narvaro.rda")

# Vigdel ---------------------------

vigsel_r <- raw_files$FIL4_VIGSEL.csv

colnames(vigsel_r) <- tolower(colnames(vigsel_r))
vigsel <- vigsel_r %>% 
  mutate(
    vigdat = as_date(vigdat),
    uppldat = as_date(uppldat)
  )

assert_that(all(is.na(vigsel$vigdat) == (vigsel_r$vigdat == 0)))
assert_that(all(is.na(vigsel$uppldat) == (vigsel_r$uppldat == 0)))

save(vigsel, file = "data/vigsel.rda")

# Flytt ------------------------


flytt_r <- raw_files$FIL6_FLYTT.csv


colnames(flytt_r) <- tolower(colnames(flytt_r))
flytt <- flytt_r %>% 
  mutate(
    frndat    = as_date(frndat),
    frndatmin = as_date(frndatmin),
    frndatmax = as_date(frndatmax),
    tildat    = as_date(tildat),
    tildatmin = as_date(tildatmin),
    tildatmax = as_date(tildatmax)
  )

save(flytt, file = "data/flytt.rda")


# Partner person ---------------------------

partner_person_r <- raw_files$FIL5_PARTNER_PERSON.csv

colnames(partner_person_r) <- tolower(colnames(partner_person_r))

partner_person <- partner_person_r %>% 
  mutate(
    pfoddat = as_date(pfoddat),
    pdoddat = as_date(pdoddat),
    pstartdat = as_date(pstartdat),
    pslutdat = as_date(pslutdat)
  )

assert_that(nrow(filter(partner_person, is.na(pfoddat))) == nrow(filter(partner_person_r, pfoddat == 0)))
assert_that(nrow(filter(partner_person, is.na(pdoddat))) == nrow(filter(partner_person_r, pdoddat == 0)))
assert_that(nrow(filter(partner_person, is.na(pstartdat))) == nrow(filter(partner_person_r, pstartdat == 0)))
assert_that(nrow(filter(partner_person, is.na(pslutdat))) == nrow(filter(partner_person_r, pslutdat == 0)))

# Check flytt ortkod against source ortkod

geo_d = read_delim("data-raw/data/nyaortkoder.csv", ";", col_names = c("n", "ar", "temp_ortkod", "nyortkod"), skip = 1)

g_codes = unique(geo_d$nyortkod)
f_codes = na.omit(unique(c(flytt$frnortkod, flytt$tilortkod)))

assert_that(all(f_codes %in% g_codes))

save(flytt, file = "data/flytt.rda")

# Make one file for static event for partner 

partner_person <- partner_person %>% 
  select(pddbid, pfodfrs, pfodhfrs, pfoddat, pdoddat, pyrkebast_kod, pyrkebast_txt) %>% 
  distinct()

save(partner_person, file = "data/partner_person.rda")

# Make partner -----------------

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
    pdoddat = max(pdoddat)
  ) %>% ungroup()

save(partner, file = "data/partner.rda")
