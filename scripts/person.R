# This script creates a person file, with one row per individual containing static infomation. The file is created from the mother and partner datafiles, thus, combing all static individual infomation into one dataset.
# 
# Each individual has a unique id, however, each individual can have multiple rows in the source files. We need to select only one entry per inidividual.
# 
# ## Load libraries

library(lubridate)
library(assertthat)
library(tidyverse)

# ## Women
# 
# First, load the source file.

load("data/mor_person.rda")

# As seen, there are a number of women who have multiple rows.


dups <- mor_person %>% group_by(mddbid) %>% 
	mutate(n = n()) %>% 
	filter(n > 1)

dup_ids <- unique(dups$mddbid)
length(dup_ids)


# This occures as a women have several start and end dates in the region, sometimes static infomation is missing during one of the spells
# To begin I create a list of all unique ids and assert that there are no missing values

m1 <- mor_person %>% select(mddbid) %>% distinct()

m <- m1

# No missing values
assert_that(length(m$mddbid[is.na(m$mddbid)]) == 0) 

# A entry is select individually for each variable, according to the following rules
# 
# 1. Not, NA. Select only records were there is infomration.
# 2. Earliest start date. Each individual can be registrered multiple times, thus we select the first recorded information.
# 3. First recorded row. Sometimes there are multiple registrations with the same date, then we select the first one. 
# 
# Variables:
# 	
# 	* mfodfrs = Birth parish
# * mfodhfrs = Birth home parish
# * mfoddat = Birth date
# * mdoddat = Death date
# * myrkebast_kod = Highest occupational code, over the course of their life
# * mykrebast_kod = The occupation in plain text


f <- function(x){
	# Get first recorded variable that is not na
	# if value is na add a na record
	message(x)
	
	if (all(is.Date(as.data.frame(mor_person)[ ,x]))){
		the_filter <- paste0("!is.na(", x, ")")
	} else {
		the_filter <- paste0(x, " > 0")
	}
	
	a <- mor_person %>% select_("mddbid", x, "mstartdat") %>% 
		filter_(the_filter) %>% 
		distinct() %>% 
		group_by(mddbid) %>% 
		filter(mstartdat == min(mstartdat)) %>% 
		ungroup() %>%  
		group_by(mddbid) %>% 
		filter(row_number() == 1) %>% 
		ungroup() %>% 
		select(-mstartdat)
	a2 <- a %>% count(mddbid)
	
	assert_that(max(a2$n) == 1)
	aa <- a %>% left_join(m, ., by = "mddbid")
	return(aa)
}


# add mfodfrs birth parish select first observed

for (i in c("mfodfrs", "mfodhfrs", "mfoddat", "mdoddat", "myrkebast_kod", "myrkebast_txt")){
	m <<- f(i)
}

# The dataset `m` now containes unique static infomation for each women, selected accoriding to the previous mentioned rules. 
# 
# Now we set the gender variable to female (1) and add more static infomation which has only one entry per women.
# 
# Added static variables:
# 	
# 	* nykt = Indicator for temperance association membership
# * nyktar = Year of entry into temperance association
# * fakf = Indicator for union membership
# * fackfar = Year of entry into union
# * regel = Linking rule used
# 
# Then I assert that each women has only one row and set standardised columnnames.

m2 <- m %>% mutate(gender = 1)

women <- mor_person %>% distinct(mddbid, nykt, nyktar, fakf, fakfar, regel) %>% 
	left_join(m2, ., by = "mddbid")

assert_that(nrow(m2) == nrow(women))
assert_that(length(unique(m2$mddbid)) == nrow(women))


colnames(women) <- c("id", "b_parish", "bh_parish", "bdate", "ddate", "occu", "occu_txt", "gender", 
										 "nykt", "nyktar", "fakf", "fakfar", "regel")

# ## Men
# 
# The procedure is repeated for the men. 

load("data/partner_person.rda")

p <- partner_person %>% select(pddbid) %>% distinct()

assert_that(length(p$pddbid[is.na(p$pddbid)]) == 0) 

# Once again only one entry is selected for each indivdidual. This time the procedure is only needed for 
# 
# * pfodfrs = Birth parish
# * pfodhfrs = Birth home parish
# * pfoddat = Birth date
# * pdoddat = Death date
# * pyrkebast_kod = Highest occupational code, over the course of their life
# * pykrebast_kod = The occupation in plain text

fp <- function(x){
	# Get first recorded variable that is not na
	# if value is na add a na record
	if (all(is.Date(as.data.frame(partner_person)[,x]))){
		the_filter <- paste0("!is.na(", x, ")")
	} else {
		the_filter <- paste0(x, " > 0")
	}
	
	a <- partner_person %>% select_("pddbid", x) %>% 
		filter_(the_filter) %>% 
		distinct() %>% 
		group_by(pddbid) %>% 
		filter(row_number() == 1) %>% 
		ungroup()
	a2 <- a %>% count(pddbid)
	
	assert_that(max(a2$n) == 1)
	
	aa <- a %>% left_join(p, .)
	return(aa)
}

for (i in c("pfodfrs", "pfodhfrs", "pfoddat", "pdoddat")){
	p <<- fp(i)
}


f2 <- function(x){
	# Get first recorded variable that is not na
	# if value is na add a na record
	
	if (all(is.Date(as.data.frame(partner_person[ ,x])))){
		the_filter <- paste0("!is.na(", x, ")")
	} else {
		the_filter <- paste0(x, " > 0")
	}
	
	a <- partner_person %>% select_("pddbid", x) %>% 
		filter_(the_filter) %>% 
		distinct() %>% 
		group_by(pddbid) %>% 
		filter(row_number() == 1) %>% 
		ungroup()
	a2 <- a %>% count(pddbid)
	
	assert_that(max(a2$n) == 1)
	return(a)
}

# myrkebast_kod

yrkebast_kod <- f2("pyrkebast_kod")

p <- yrkebast_kod %>% select(pddbid, pyrkebast_kod) %>% left_join(p, .)

# myrkebast_txt

yrkebast_txt <- f2("pyrkebast_txt")

p <- yrkebast_txt %>% select(pddbid, pyrkebast_txt) %>% left_join(p, .)


# Gender is set to male (2), the voluntary association data is added, I assert that there is only one row per unique individual and I set standardised variable names. 


p <- p %>% mutate(gender = 2) %>% 
	rename(occu = pyrkebast_kod, occu_txt = pyrkebast_txt)


pp <- partner_person %>% distinct(pddbid, nykt, nyktar, fakf, fakfar, regel) %>% 
	left_join(p, .)


assert_that(nrow(p) == nrow(pp))
assert_that(length(unique(p$pddbid)) == nrow(pp))


colnames(pp) <- c("id", "b_parish", "bh_parish", "bdate", "ddate", "occu", "occu_txt", "gender",
									"nykt", "nyktar", "fakf", "fakfar", "regel")

# The datasets for women and men can now be combined, and I then assert that all individuals in the source files also exists in the final person dataset. 

person <- rbind(women, pp)

assert_that(all(partner_person$pddbid %in% person$id))
assert_that(all(mor_person$mddbid %in% person$id))

# The individual with id 1880, now have only one row.

person %>% filter(id == 1880) %>% select(id, ddate)


# Save dataset for further processing

save(person, file = "data/person.rda")

