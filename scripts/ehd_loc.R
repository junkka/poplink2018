# Add location


library(lubridate)
library(tidyverse)
library(multidplyr)
library(assertr) 
source("R/dates.R")

load(".cache/ehd_va.rda")
load(".cache/moves.rda")

# Create and migration event data and add to ehd base data.

moves2 <- moves %>% 
	transmute(
		mid = mddbid,
		date = datmin,
		offset = ifelse(weight == 2, 1.5, 0.5),
		type = ifelse(type == "in", "inloc", "outloc"),
		place = ortkod,
		parish = frs
	) %>% 
	filter(mid %in% unique(ehd_va$mid))

moves3 <- ehd_va %>% filter(!is.na(bdate)) %>% 
	distinct(mid, bdate) %>% 
	left_join(moves2) %>% 
	mutate(age = diff_years(date, bdate)) %>% 
	filter(age >= 0, age < 125)

d <- ehd_va %>% 
	filter(mid %in% unique(moves$mddbid)) %>% 
	bind_rows(moves3) %>% 
	arrange(mid, date, offset)


# Fill migration values, first "down", chronologically, then "up". 
# The order of filling is assured by arrange emigration events before immigration
# as a immigration event would set the place of all following events we fill down the events, conologically
# And as a emigration events would  set the place of all previous events we then fill it "up" conologically


fill_vals <- function(d){
	x <- fill(d, place)
	x
}

fill_vals_down <- function(d){
	x <- fill(d, place, .direction = "up")
	x
}

d3 <- d %>% 
	mutate(con_ind = ifelse(type %in% c("cons", "birth"), type, NA)) %>% 
	nest(-mid) %>% 
	partition(mid) %>%
	cluster_library("tidyverse") %>%
	cluster_assign_value("fill_vals", fill_vals) %>%
	cluster_assign_value("fill_vals_down", fill_vals_down) %>%
	mutate(data = map(data, fill_vals), data = map(data, fill_vals_down)) %>% 
	collect() %>% 
	select(mid, data) %>% 
	unnest() %>% ungroup()


# However, some migration events occur after end of observation or before observations starts. 
# 
# drop events before first cons/birth  
# drop events after last non move


d5 <- d3 %>% 
	group_by(mid) %>% 
	mutate(
		mindates = ifelse(!type %in% c('in', 'inloc', 'out', 'outloc'), date, NA),
		mindate = as.Date(min(mindates, na.rm = T), origin = "1970-01-01")
	) %>% 
	ungroup() %>% 
	select(-mindate, -mindates)

# add location coordinates


coln <- c("nr", "ar", "regprefix", "frsprefix", "nofrs", "temp_ortkod",
					"befolkning", "ortnmn", "x_koordinat", "y_koordinat",
					"nyortkod", "nybefolkning", "approx", "manual")

dd <- read_delim(
	"data-raw/data/ortkod_geokodat.csv", ";",
	skip = 1,
	col_names = coln,
	locale = locale("sv", encoding = "latin1"))

dd2 <- dd %>%
	group_by(nyortkod) %>%
	summarise(
		x = mean(x_koordinat, na.rm = T),
		y = mean(y_koordinat, na.rm = T)
	) %>% ungroup() %>%
	rename(place = nyortkod)

d6 <- left_join(d5, dd2)


ehd_loc <- d6 %>% ungroup() 

# Save data


save(ehd_loc, file = "data/ehd_loc.rda")
