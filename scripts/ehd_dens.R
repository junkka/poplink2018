# Add population density and migration rate
# 
# number of migrants by population by year
# 
# a migrant is from another parish

library(survival)
library(tidyverse)
library(rgeos)

load(".cache/ehd_mig.rda")
load("data/person.rda")
load("data/voroni_spdf.rda")
load(".cache/locations_v.rda")
load(".cache/moves.rda")

# need to add place to event history data


moves2 <- moves %>% 
	filter(type == "in") %>% 
	transmute(
		mid = mddbid,
		date = datmin,
		type = type,
		place = ortkod,
		parish = frs
	) %>% 
	filter(mid %in% unique(ehd_mig$mid))

d0 <- bind_rows(ehd_mig, moves2) %>% 
	arrange(mid, date)

d02 <- d0 %>% 
	group_by(mid) %>% 
	filter(type != "in") %>% 
	summarise(mind = min(date), maxd = max(date))

d03 <- left_join(d0, d02) %>% 
	filter(date <= maxd, date >= mind)


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


d04 <- left_join(d03, dd2)
d05 <- locations_v %>% 
	distinct(x,y,voro) %>% 
	left_join(d04, .) %>% 
	filter(!is.na(voro))

d <- d05 %>% 
	group_by(mid) %>% 
	mutate(stopdate = lead(date)) %>% 
	ungroup() %>% 
	filter(!is.na(stopdate))

d1 <- d %>% 
	mutate(
		start = lubridate::decimal_date(date),
		stop  = lubridate::decimal_date(stopdate),
		event = 0
	) %>% 
	filter(start < stop)

# split by year
d2 <- survSplit(Surv(start, stop, event) ~1, data = d1, cut = seq(1860, 1961), episode = "year", id = "iid")

d3 <- d1 %>% 
	mutate(iid = row_number()) %>% 
	select(-start, -stop, -event) %>% 
	left_join(d2, .) %>% as_tibble()


d4 <- d3 %>% filter(start >= 1860) %>% 
	mutate(
		m_mig = m_mig != "stayer",
		p_mig = p_mig != "stayer",
		durr = stop - start, 
		year = year + 1859,
		n_mig = m_mig + p_mig
	) %>% 
	filter(!is.na(durr), !is.na(n_mig)) %>% 
	group_by(year, voro) %>% 
	summarise(
		time = sum(durr)*2,
		migr = sum(durr*p_mig),
		mig_prop = migr/time
	)

area <- gArea(voroni_spdf, TRUE)
voroni_spdf$area <- area

v_d <- voroni_spdf@data %>% 
	rename(voro = id) %>% 
	as_tibble()

d5 <- left_join(d4, v_d) 

d6 <- d5 %>% 
	mutate(density = time/(area/100000))

d7 <- select(d6, year, voro, mig_prop, density, population = time)


ehd_mig2 <- ehd_mig %>%
	mutate(year = lubridate::year(date)) %>% 
	left_join(d7) 

ehd_dens <- ehd_mig2

# Save
save(ehd_dens, file = ".cache/ehd_dens.rda")



