
# Add exposure to voluntary association

library(movement)
library(histmaps)
library(lubridate)
library(rgeos)
library(sp)
library(raster)
library(forcats)
library(assertthat)
library(tidyverse)

# Extract base map of parishes
parishes <- hist_boundaries(1900, "parish", "sp")

skell_ids <- c(241771000, 248204004, 248209000, 248207001)
skell <- subset(parishes, nadkod %in% skell_ids)
skell_df <- sp_to_ggplot(skell)

# View parish boundaries
if (interactive()){
	ggplot(skell_df, aes(long, lat, group = group)) +
		geom_path() +
		coord_equal() +
		theme_void()
}

# Extract coordinates of local voluntary association groups
skell_m <- tbl_df(movement) %>% 
	filter(forkod %in% c(unique(skell_df$forkod), 241702), year < 1951) %>% 
	distinct(orgtypn, lon, lat)

# View distribution of associations
if (interactive()){
	ggplot(skell_df) +
		geom_path(aes(long, lat, group = group)) +
		geom_point(data = skell_m, aes(lon, lat), alpha = 0.5) +
		coord_equal() +
		facet_wrap(~orgtypn) +
		theme_void()
}

# Load spatial neigbourhood data
load("data/voroni_spdf.rda")
# buffer neigbourhoods to assure that adjecent polygons will overlapp
vsp <- gBuffer(voroni_spdf, TRUE, width = 10)

# Calculate adjecency matrix
sp_rel <- gOverlaps(vsp, byid = TRUE)
sp_rel_d <- sp_rel %>% as_data_frame() %>% 
	mutate(voro = rownames(sp_rel)) %>% 
	gather(group, ind, -voro) %>% 
	filter(ind)

voroni_df <- sp_to_ggplot(voroni_spdf)

if (interactive()){
	ggplot(voroni_df) +
		geom_path(aes(long, lat, group=group)) +
		geom_point(data = skell_m, aes(lon, lat), color = "red", alpha = 0.8) +
		coord_equal() +
		facet_wrap(~orgtypn) +
		theme_void()
}

ms <- skell_m %>% filter(!is.na(lon))

# Get neigbourhood for each local voluntary association by calculating wheter the coordinates are within neigbourhood boundaries
ms_sp <- SpatialPointsDataFrame(ms[ ,c("lon", "lat")], data = ms, proj4string = CRS(proj4string(voroni_spdf)))

x <- over(voroni_spdf, ms_sp, returnList = TRUE)

a <- lapply(1:length(x), function(y){
	z <- x[[y]]
	if (nrow(z) < 1) return(NULL)
	z$group = names(x)[y]
	z
}) %>% bind_rows() 
a

# for each year aggregate size by neigbourhood and association type

skell_moves <- movement %>% 
	tbl_df() %>% 
	left_join(a) %>% 
	filter(!is.na(group))

period_l <- 1

voro_move <- skell_moves %>% 
	mutate(year = (year %/% period_l) * period_l) %>% 
	group_by(group, year, orgtypn) %>% 
	summarise(membs = sum(medl, na.rm=T)/period_l) %>% 
	ungroup() %>% 
	filter(!is.na(membs)) %>% 
	mutate(
		memb_g = cut(membs, quantile(membs))
	)

voro_t <- full_join(sp_rel_d, voro_move, by = "group") %>% 
	filter(!is.na(year)) %>% 
	replace_na(list(membs = 0)) %>% 
	group_by(voro, year, orgtypn) %>% 
	summarise(neighbour = sum(membs)) %>% 
	ungroup() %>% 
	rename(group = voro)

voro_move2 <- full_join(voro_move, voro_t) %>% 
	replace_na(list(membs = 0, neighbour = 0)) %>% 
	filter(membs > 0 | neighbour > 0)

# add to ehd data as contextual variable  
# need to connect place to voronoi sub-regions

load("data/ehd_loc.rda")
load(".cache/locations_v.rda")

# add voro to ehd, join by x and y

ehd_1 <- locations_v %>% 
	distinct(x, y, voro) %>% 
	left_join(ehd_loc, .)

# Locations without a sub-region assignment

tmp <- ehd_1 %>% 
	filter(is.na(voro)) %>% 
	distinct(x, y)


if(interactive()){
	ggplot(voroni_df) +
		geom_path(aes(long, lat, group=group)) +
		geom_point(data = tmp, aes(x, y), color = "red", alpha = 0.8) +
		coord_equal() +
		theme_void()
}

# Calculate organisation size by year and sub-region


voro_m12 <- voro_move2 %>% 
	select(voro = group, year, orgtypn, membs, neighbour) %>% 
	filter(orgtypn %in% c("fackf", "nykt")) 

voro_m13 <- voro_m12 %>% 
	select(-neighbour) %>% 
	spread(orgtypn, membs) %>% 
	replace_na(list(fackf = 0, nykt = 0))

voro_m14 <- voro_m12 %>% 
	select(-membs) %>% 
	spread(orgtypn, neighbour) %>% 
	replace_na(list(fackf = 0, nykt = 0)) %>% 
	rename(fackf_nei = fackf, nykt_nei = nykt)

voro_m2 <- left_join(voro_m13, voro_m14)


# Add a time dependent variable. A new variable for each voro memb change. Create a new event for each indiv
# 
# select one row per mid and voro

d <- ehd_1 %>% 
	arrange(mid, date, offset) %>% 
	group_by(mid) %>% 
	mutate(
		move = ifelse(lag(voro, default = "0") == voro, 0, 1),
		move = ifelse(is.na(move), 0, move),
		move = cumsum(move)
	) %>% 
	ungroup() %>% 
	group_by(mid, voro, move) %>% 
	mutate(end_date = max(date), mindate = min(date)) %>% 
	ungroup() %>% 
	distinct(mid, move, voro, mindate, end_date)

d2 <- left_join(d, voro_m2, by="voro")

# keep if voro year + 1 is less than end_date 

d3 <- d2 %>% 
	filter(!is.na(year)) %>% 
	mutate(year_d = as.Date(paste0(year + 1, "-01-01"))) %>% 
	filter(year_d < end_date) %>% 
	filter((year_d + years(1)) > mindate)

# Add to event data.

d4 <- d3 %>% 
	transmute(
		mid, 
		date = year_d, 
		offset = 0.1, 
		type = "va_exp",
		fackf_size = fackf, 
		nykt_size = nykt,
		fackf_nei,
		nykt_nei
	)

d5 <- bind_rows(ehd_1, d4)

ehd_exp <- d5

# Save


save(ehd_exp, file = ".cache/ehd_exp.rda")
