# Make children
# 
# The source file on child births can aslo contain multiple records per child, this document creates a file with static infomation on the child and the birth.

library(lubridate)
library(assertthat)
library(tidyverse)
source("R/dates.R")
# For each variable and child, I select the first and earliest recorded non NA value.


load("data/barn.rda")

b <- barn %>% 
	select(bddbid) %>% 
	distinct()

# Recode 0 as NA and select first non NA value
get_var <- function(x){
	
	if (all(is.Date(as.data.frame(barn)[ ,x]))){
		mutate_call = lazyeval::interp(~ as.Date(d), d = as.name(x))
	} else {
		mutate_call = lazyeval::interp(~ ifelse(d == 0, NA, d), d = as.name(x))
	}
	
	a <- barn %>% 
		select_("bddbid", x) %>% 
		mutate_(.dots = setNames(list(mutate_call), x)) %>% 
		filter_(paste0("!is.na(", x, ")")) %>% 
		group_by(bddbid) %>% 
		filter(row_number() == 1)
	
	d <- left_join(b, a, by = "bddbid")
	return(d)
}

for (i in colnames(barn)){
	b <<- get_var(i)
}

children <- b %>% distinct(bddbid, nofrs, kon, foddat, rbes, fodfrs, fodhfrs, dopfrs, doddat, dodfrs,
													 dodhfrs, mddbid, fddbid, utald,mpf, mgpf, fsyskf, fpf, fgpf, mmf, mmfnarvaro,
													 ffyrke_kod, ffyrke_txt, fmyrke_kod, fmyrke_txt)

children <- children %>% 
	mutate(
		diff = diff_years(doddat, foddat),
		doddat = as.Date(ifelse(diff < 0, foddat, doddat), origin = "1970-01-01")
	) %>% 
	select(-diff)

assert_that(nrow(children) == nrow(b))
assert_that(length(unique(b$bddbid)) == nrow(children))


save(children, file = "data/children.rda")


