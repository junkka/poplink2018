# Add occupational class

library(hisco)
library(tidyr)
library(assertr)
library(dplyr)
library(multidplyr)

source("R/dates.R")
load(".cache/ehd.rda")

# get all occupational changes


load("data/children.rda")

ch <- children %>% 
	filter(bddbid %in% unique(ehd$cid), !is.na(ffyrke_kod)) %>% 
	select(cid = bddbid, occu = ffyrke_kod)
message(length(unique(ch$occu)), " unique occupations")

if (interactive()) ch %>% count(occu, sort = T)

# The occupational codes are linked to hisco codes using a linking table.

load("data-raw/ddb_to_hisco.rda")

dh <- ddb_to_hisco %>% 
	select(occu = ddb_kod, hisco, relation, status)

if (interactive()) head(dh)

# Then we can add hisco codes to the ehd dataset and then link hico and socpo codes to the dataset.


ehd2 <- left_join(ehd, ch)

d3 <- left_join(dh, hisco, by = c("hisco", "status", "relation")) %>% 
	left_join(ehd2, ., by = "occu") %>% 
	select(mid:hisco, socpo_label,  hisclass)

# Some of the classes are combined into condensed classification.


new_class <- data_frame(
	ses_label = c(rep("Elite", 2), rep("Middle class", 4), rep("Farmers",1), rep("Farm workers",2), rep("Skilled workers", 2), rep("Unskilled workers", 2)),
	hisclass  = c(1,2,3,4,5,6,  8,  10,12,  7,9,11,13)
)

d4 <- left_join(d3, new_class)


d6 <- d4 %>% 
	mutate(
		socpo_label = as.character(socpo_label),
		ses = ifelse(stringr::str_detect(socpo_label, "Unskilled"), "Unskilled",
								 ifelse(socpo_label %in% c("Semi skilled workers", "Skilled workers"), "Skilled", 
								 			 ifelse(socpo_label %in% c("Middle Class", "Elite"), "Middle class", as.character(socpo_label))))
		
	)

ehd_class <- d6 %>% ungroup()

# Visual check ------------
# 
# Check combinations of parish and class unique mids 

if (interactive()) {
		
	
	library(histmaps)
	
	prs <- hist_parish@data %>% select(parish = dedik, socken) %>% distinct() %>%
		group_by(parish) %>% filter(row_number() == 1)
	
	library(ggplot2)
	
	d <- ehd_class %>% select(mid, parish, ses) %>% distinct() %>%
		filter(!is.na(ses)) %>%
		group_by(parish) %>%
		mutate(tot = n()) %>% ungroup() %>%
		group_by(parish, ses) %>%
		summarise(n = n(), prop = n/mean(tot))
	
	
	da <- d %>% filter(parish == 82980) %>%
		arrange(n)
	
	d2 <- d %>% mutate(socpo = factor(ses, levels = as.character(da$ses))) %>%
		left_join(prs)
	
	
	ggplot(d2, aes(socpo, prop, fill = socpo)) + geom_bar(stat = "identity") + coord_flip() + facet_wrap(~socken)
}

# Saving data


save(ehd_class, file = ".cache/ehd_class.rda")


