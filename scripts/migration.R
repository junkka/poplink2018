# Make migration data
# 
# The source file on migration has a number entries of ilogical in and outdates, as they sometimes overlapp. This is fixed by setting out date to the indate of the following spell, or in the case of full overlapp, by dropping the shorter spell.

library(tidyverse)
load("data/narvaro.rda")

nrow(narvaro)

# Overlapping spells is detected by checkecking if the difference in lag between the enddate of one spell and the indate of the next spell. If the lag is less tha 0 they overlapp

b <- narvaro %>% 
	arrange(ddbid, startdat) %>% 
	group_by(ddbid) %>% 
	mutate(
		next_start = lead(startdat, order_by = ddbid, default = 0),
		next_end = lead(slutdat, order_by = ddbid, default = 0)) 

org <- "1970-01-01"

bb <- b %>% 
	ungroup() %>% 
	mutate(
		next_start = as.Date(ifelse(next_start == org, NA, next_start), origin = org),
		next_end = as.Date(ifelse(next_end == org, NA, next_end), origin = org)
	)

b2 <- bb %>% 
	mutate(
		ind = ifelse(slutdat <= next_start, 0, 
								 ifelse(slutdat >= next_end, 3, 2)
		)
	)
b2 %>% count(ind)

# indicator
# 
# 0 = no overlapp
# 2 = overlapp
# 3 = full overlapp
# NA = no overlapp
# 
# Next the overlapping spells are updated.
# 
# * If ind == 0, NA (no overlapp), the endate and startdate are not changed
# * If ind == 2 (overlapp), the enddate is set to the minimum of the spells enddate and the next start date, the start date is set to the minimum of the previous spells enddate and the spells start date.
# * If ind == 3 (full overlapp), The spell is dropped

b3 <- b2 %>% 
	mutate(ind = ifelse(is.na(ind), 0, ind)) %>% 
	mutate(
		slutdat = as.Date(ifelse(ind == 0, 
														 slutdat,
														 ifelse(
														 	ind == 2, 
														 	pmin(next_start, slutdat), 
														 	slutdat)), 
											origin = org),
		startdat = as.Date(ifelse(lag(ind) == 0,
															startdat,
															ifelse(
																lag(ind) == 2, 
																pmin(lag(next_start), 
																		 lag(slutdat)), 
																startdat)), 
											 origin = org)
	)

migration <- b3 %>% 
	mutate(l_ind = lag(ind), l_ind = ifelse(is.na(l_ind), 0, l_ind)) %>% 
	filter(l_ind != 3) %>% 
	select(-ind, -l_ind, -next_start, -next_end)

nrow(migration)


# Saving 

save(migration, file = "data/migration.rda")

