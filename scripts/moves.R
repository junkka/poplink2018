# Make moves data
# 
# Create a migration event data that containes infomation on all in migrations and first out migration. 

library(tibble)
library(dplyr)
load("data/flytt.rda")

# Drop stayers. A stayer has the same location code in and out. If any is NA they are migrants. 

flytt2 <- flytt %>% mutate(same = frnortkod == tilortkod, same = ifelse(is.na(same), FALSE, same)) %>% 
	filter(!same)

# Assign, event order number by id.


flytt3 <- flytt2 %>% group_by(mddbid) %>% 
	mutate(nr = row_number()) %>% 
	ungroup()

# Extract outdates and indates, and bind to one dataset


outdates <- flytt3 %>% 
	select(
		mddbid, nr, frs = frnfrs, ortkod = frnortkod, 
		dat = frndat, datmin = frndatmin, datmax = frndatmax
	) %>% 
	mutate(type = "out", weight = 1)
indates <- flytt3 %>% 
	select(
		mddbid, nr, frs = tilfrs, ortkod = tilortkod, 
		dat = tildat, datmin = tildatmin, datmax = tildatmax
	) %>% 
	mutate(type = "in", weight = 2)
d3 <- bind_rows(indates, outdates) %>% filter(!is.na(datmin)) %>% arrange(mddbid, datmin, nr, weight)

# keep all first rows and all other in


firstouts <- d3 %>% 
	group_by(mddbid) %>% 
	filter(row_number() == 1) %>% 
	ungroup() %>% 
	filter(type == "out")

flytt4 <- bind_rows(firstouts, indates) %>% 
	filter(!is.na(datmin)) %>% 
	arrange(mddbid, datmin, nr)

# Keep only last in date by mddbid and date


flytt5 <- flytt4 %>% 
	group_by(mddbid, type, datmin) %>% 
	filter(row_number() == max(row_number())) %>% 
	ungroup() %>% 
	select(-nr)


# add stayers


intern_mov <- flytt %>% 
	filter(!mddbid %in% unique(flytt5$mddbid)) %>% 
	select(
		mddbid, frs = tilfrs, ortkod = tilortkod, 
		dat = tildat, datmin = tildatmin, datmax = tildatmax
	) %>% 
	mutate(weight = 2, type = "in")

moves <- bind_rows(intern_mov, flytt5)

save(moves, file = ".cache/moves.rda")

