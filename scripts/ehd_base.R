# Create the base dataset for event history analysis
# 
# The base dataset for the analysis consists of data on the timing of events in each women's life. The dataset is constructed by combing infomation on marriage,in-migration, out-migration, births and deaths. 
# 
# The events of a individual wil be ordered cronologically, however, some events occur at the same time thus, each event has an offset, which decides the order when dates are tied, based upon the type of event. 
# 
# Offset:
# 
# 1. out migration
# 2. in migration
# 3. marriage
# 4. birth
# 5. death
# 6. end of reproductive life course


library(lubridate)
library(stringr)
library(assertthat)
library(assertr)
library(tidyverse)
library(rcustom)

source("R/dates.R")


load("data/person.rda") 
load("data/vigsel.rda")
load("data/migration.rda")

org <- "1970-01-01"

knitr::opts_chunk$set(cache = FALSE)

# ## Marriages ------------------------
# 
# First I select all marriages, and assert that I have only one row per marriage (combination of the womens id and date of marriage)

v <- vigsel %>% 
	group_by(mddbid, pddbid, vigdat) %>% 
	filter(row_number() == 1) %>% 
	ungroup() %>% 
	select(mddbid, pddbid, vigdat, vigfrs) %>% 
	distinct() %>% 
	arrange(vigdat) %>% 
	group_by(mddbid) %>% 
	filter(vigdat == min(vigdat)) %>% 
	ungroup() %>% 
	transmute(mid = mddbid, pid = pddbid, date = vigdat, 
	type = "marriage", offset = 3, parish = NA) %>% 
	mutate(id = row_number())

# Check unique marriages as unique combinations of mid and date
tmp <- v %>% distinct(mid, date) %>% count(mid) %>% summarise(max = max(n))
assert_that(tmp$max == 1)

# If a women has more than one pid (partner id) per marriage date I select the one where pid is not 0. Count number of pid == 0 male and filter out thouse where count is more than 0

dupp_mars <- v %>% 
	group_by(mid) %>% 
	mutate(
	mars = n(),
	pid0s = sum(ifelse(pid == 0, 1, 0))
	) %>% 
	ungroup() %>% 
	filter(mars > 1 & pid0s > 0)

excess_marr <- dupp_mars %>% filter(pid == 0)

message( nrow(excess_marr), " excess marriages")

# remove excess marriages

v2 <- v %>% filter(!id %in% excess_marr$id)

# For all other duplicates find info on marriage location and remove thouse without marriage location information.

dupp_mars <- v2 %>% 
	group_by(mid) %>% 
	mutate(
	mars = n()
	) %>% 
	ungroup() %>% 
	filter(mars > 1) %>% 
	left_join(vigsel, by = c("mid" = "mddbid", "pid" = "pddbid"))

no_frs = dupp_mars %>% filter(vigfrs == 0)

message(nrow(no_frs), " duplicate marriages without parish notation")

# Remove these duplicates

v3 <- v2 %>% filter(!id %in% no_frs$id) %>% 
	filter(!pid %in% c(357017, 884360)) # qualified selections

# check unique marriage by unique combinations of pid, mid and date to unique mid and date

assert_that(nrow(distinct(v3, mid, pid, date)) == nrow(distinct(v3, mid, date)))

# Add birth date of women, keep only women who married between ages 15 and 50

m <- person %>% filter(gender == 1) %>% 
transmute(mid = id, bdate = bdate)

# first observed birth date and first observed marriage date

d1a <- v3 %>% left_join(m) %>% 
filter(!is.na(bdate), !is.na(date)) %>% 
mutate(m_age = diff_years(date, bdate)) %>% 
filter(m_age >= 15, m_age < 50) %>%
select(mid:parish, bdate)

# check unique marriage by unique combinations of pid, mid and date to unique mid and date


assert_that(nrow(distinct(d1a, mid, pid, date)) == nrow(distinct(d1a, mid, date)))
assert_that(nrow(filter(d1a, is.na(date))) == 0)



if (interactive()) hist(d1a$date, breaks = "year")

a <- d1a %>% assert(not_na, bdate, date) %>% 
	mutate(age = diff_years(date, bdate)) 

if (interactive()) hist(a$age, breaks = 15:50)
if (interactive()) range(a$age)


## Add observation end ---------------------------

# observation end is first of 
# 
# * migration out (longer than 1 year)
# * death


ddates <- person %>% 
	filter(id %in% unique(d1a$mid)) %>% 
	transmute(mid = id, ddate = ddate)

load("data/mor_person.rda")

last_obs_date <- mor_person %>% 
	filter(mddbid %in% unique(d1a$mid)) %>% 
	group_by(mddbid) %>% 
	summarise(mig_date = max(mslutdat, na.rm = T)) %>% 
	rename(mid = mddbid)

e_dates <- ddates %>% left_join(last_obs_date, by = "mid") %>% 
	rowwise() %>% 
	mutate(last = min(c(ddate, mig_date), na.rm = T)) %>% 
	ungroup()

e_dates <- e_dates %>% 
	mutate(event = ifelse(last == ddate, "end_death", "end_mig")) %>% 
	replace_na(list(event = "end_mig")) %>% 
	filter(mid != 992829) %>% 
	assert(not_na, event, last) %>% 
	verify(mid %in% unique(d1a$mid))

d1b <- e_dates %>% 
	transmute(date = last, mid = mid, type = event, offset = 9) %>% 
	bind_rows(d1a) %>% 
	filter(mid != 992829)

start_and_enddates <- d1b %>% 
	group_by(mid) %>%
	mutate(
	sdate = first(bdate[!is.na(bdate)]),
	edate = max(date[type %in% c("end_mig", "end_death")])
	) %>%
	ungroup() %>% 
	select(mid, sdate, edate) %>% 
	distinct(mid, sdate, edate) %>% 
	assert(not_na, sdate, edate)


## Add births --------------------
# 
# add births of women in the dataset

load("data/children.rda")

ch <- children %>% 
	select(mid = mddbid, cid = bddbid, date = foddat) %>% 
	filter(mid %in% unique(d1a$mid)) %>% 
	mutate(date = date, type = "birth", offset = 4)

# only one birthdate per child
assert_that(nrow(distinct(ch, cid, date)) == nrow(ch))
# only unique births
assert_that(nrow(distinct(ch, cid)) == nrow(ch))

message(nrow(ch), " births")

a <-d1a %>% distinct(mid, bdate) %>% filter(!is.na(bdate)) %>% 
	left_join(ch, .) %>% 
	mutate(age = diff_years(date, bdate)) 

illogic_age <- a %>% filter(age < 14 | age > 50)
if (interactive()) hist(illogic_age$age, breaks = c(0:70))

# Add births to the dataset


ch <- a %>% filter(age >= 14, age < 50) %>% 
	select(-bdate, -age)
	d2 <- d1b %>% 
	bind_rows(ch) %>% 
	arrange(mid, date, offset) %>% 
	assert(not_na, mid, date:offset)

d2a <- d2 %>%
	left_join(start_and_enddates) %>% 
	assert(not_na, sdate, edate) %>% 
	filter(date >= sdate, date <= edate) %>% 
	verify(date >= sdate) %>% 
	verify(date <= edate) %>% 
	select(-sdate, -edate)

## Add migration -------------
# 
# Immigration

n <- migration %>% 
	select(mid = ddbid, date = startdat, parish = nofrs) %>% 
	filter(mid %in% unique(d2a$mid)) %>% 
	mutate(type = "in", offset = 2) %>% 
	distinct() %>% 
	filter(!is.na(date)) %>% 
	left_join(start_and_enddates) %>% 
	mutate(
	diff = diff_years(date, sdate),
	date = as.Date(ifelse(diff < 0 & diff > -1, sdate, date), origin = origin)
	) %>% 
	filter(diff > -1, !is.na(date), date <= edate) %>% 
	select(-sdate,-edate,-diff)

d3 <- d2a %>% bind_rows(n)

vtemp <- d3 %>%
	left_join(start_and_enddates) %>% 
	assert(not_na, sdate, edate) %>% 
	verify(date >= sdate) %>%
	verify(date <= edate)

# emigration

n <- migration %>% 
	filter(!is.na(slutdat)) %>%  
	select(mid = ddbid, date = slutdat, parish = nofrs) %>% 
	filter(mid %in% unique(d3$mid)) %>% 
	mutate(type = "out", offset = 1) %>% 
	distinct() %>% 
	left_join(start_and_enddates) %>% 
	mutate(
	diff = diff_years(date, sdate),
	date = as.Date(ifelse(diff < 0 & diff > -1, sdate, date), origin = origin)
	) %>% 
	filter(diff > -1, !is.na(date), date <= edate) %>% 
	select(-sdate,-edate,-diff)

d4 <- d3  %>% bind_rows(n) %>% filter(!is.na(date))


vtemp <- d4 %>%
	left_join(start_and_enddates) %>% 
	assert(not_na, sdate, edate) %>% 
	verify(date >= sdate) %>% 
	verify(date <= edate)

## Filter marriages ----------------------
# 
# Select first marriages wich were under observation, which means that marriage date is inbetween first start date and enddate +- 2 years. 

unob_mars <- d4 %>% 
	select(mid, type, date) %>% 
	filter(type %in% c("marriage", "in")) %>% 
	group_by(mid) %>% 
	mutate(first_in = min(date[type == "in"], na.rm = T)) %>% 
	ungroup() %>% 
	filter(type == "marriage") %>% 
	filter(date < (first_in - years(2)))

if (interactive()) unob_mars %>% glimpse()

# Check distribution of unobserved years


d <- unob_mars %>% 
	mutate(diff = decimal_date(date)- decimal_date(first_in))

if (interactive()) hist(as.integer(d$diff))

# Remove unobserved women from data


d5 <- d4 %>% 
	filter(!mid %in% unob_mars$mid)

ids <- unique(d5$mid)

mid_evs <- distinct(d5, mid, type)

marrs <- filter(d5, stringr::str_detect(type, "marriage")) %>% 
	verify(mid %in% ids) %>% 
	verify(ids %in% mid)


marrs <- filter(d5, stringr::str_detect(type, "end_")) %>% 
	verify(mid %in% ids) %>% 
	verify(ids %in% mid)


marrs <- filter(d5, stringr::str_detect(type, "end_")) %>% 
	count(mid) %>% 
	verify(n == 1)

## Construct end date -------------------
# 
# Create a object with all possible enddates for a couple, the first date == end date
# 
# *Marriage end
# * partner death 
# * women death
# * out migration
# * 50 years
# 
# Select unique marriages


dd <- d5 %>% 
	select(mid, pid) %>% 
	filter(!is.na(pid)) %>% 
	distinct()

# add partner death date


p <- person %>% 
	filter(gender == 2, id %in% unique(d5$pid)) %>% 
	mutate(ddate = as.Date(ifelse(ddate == bdate, NA, ddate), origin = "1970-01-01")) %>% 
	select(pid = id, pdate = ddate) %>% 
	distinct() %>% 
	left_join(dd, .)

if (interactive()) nas_c(p)


# add women death date


p1 <- person %>% 
	filter(gender == 1, id %in% unique(d5$mid)) %>% 
	select(mid = id, mdate = ddate, bdate) %>% 
	distinct() %>% 
	left_join(p, .)
if (interactive()) nas_c(p1)

# Find last migration date

n <- migration %>% 
	filter(slutdat != 0) %>% 
	arrange(ddbid, slutdat) %>% 
	group_by(ddbid) %>% 
	mutate(nextin = lead(startdat, default = 0)) %>% 
	ungroup() %>% 
	mutate(nextin = as.integer(nextin - slutdat)/365.25 )

if (interactive()) nas_c(n)

# select first entry after marriage with less than 2 year absence
# 
# get marriage date
tmp <- d5 %>% filter(type == "marriage") %>% 
transmute(ddbid = mid, mdate = date)

nn <- n %>% 
	# mark rows with a absence of more than 2 year
	mutate(ind = ifelse(nextin > 2 | is.na(nextin), 1, 2)) %>% 
	arrange(ddbid, startdat) %>% 
	select(ddbid, startdat, slutdat, nextin, ind) %>% 
	left_join(tmp) %>% 
	filter(!is.na(mdate)) %>% 
	filter(slutdat > (mdate - dyears(2))) %>%  
	filter(ind == 1) %>% 
	group_by(ddbid) %>% 
	filter(row_number() == 1) %>%
	ungroup %>%  
	transmute(mid = ddbid, enddate = slutdat) 

p2 <- left_join(p1, nn) %>% 
	mutate(date50 = bdate + dyears(50)) %>% 
	left_join(start_and_enddates) %>% 
	mutate(enddate = as.Date(ifelse(enddate > edate, edate, enddate), origin = origin)) %>% 
	select(-edate, -sdate)

if (interactive()) glimpse(p2)

# check ages


tmp2 <- p2 %>% 
	mutate(
		age_md = diff_years(mdate, bdate),
		age_pd = diff_years(pdate, bdate),
		age_ed = diff_years(enddate, bdate)
	) %>% 
	assert(within_bounds(15,125), age_md) %>% 
	assert(within_bounds(15,125), age_pd) %>% 
	assert(within_bounds(14,125), age_ed) 

# Select minimum of all possible end dates.

p3a <- p2 %>% 
	mutate(end = pmin(pdate, mdate, enddate, date50, na.rm = T)) %>% 
	mutate(
		b_diff = diff_years(end, bdate)
	)

tmp3 <- tmp %>% 
	rename(mid = ddbid, marrdate = mdate)

p3b <- p3a %>% 
	verify(b_diff <= 50) %>% 
	assert(within_bounds(14.5,50), b_diff) %>% 
	verify(bdate < end) %>% 
	left_join(tmp3) %>% 
	filter(end > marrdate)


p3 <- p3a %>% 
	transmute(mid, pid, date = end, type = "end", offset = 6)

# assert all marriages have ends
tmp <- filter(d5, type == "marriage")

assert_that(nrow(filter(tmp, !mid %in% d3$mid)) == 0)
assert_that(nrow(filter(p3, !mid %in% tmp$mid)) == 0)

# assert no duplicate ends by marriage
assert_that(nrow(p3) == nrow(tmp))

d6 <- bind_rows(d5, p3)

tmp1 <- d6 %>%
	left_join(start_and_enddates) %>% 
	assert(not_na, sdate, edate) %>% 
	verify(date >= sdate) %>% 
	verify(date <= edate) 


ends <- d6 %>% filter(type == "end") %>% 
	distinct(mid) %>% pluck("mid")

marrs <- d6 %>% filter(type == "marriage") %>% 
	distinct(mid) %>% pluck("mid")

tmp <- d6 %>% 
	verify(mid %in% ends) %>% 
	verify(ends %in% mid) %>% 
	verify(mid %in% marrs) %>%
	verify(marrs %in% mid) 

mids <- unique(d6$mid)

ttemp <- d6 %>% 
	filter(type == "marriage") %>% 
	count(mid) %>% 
	verify(n == 1) %>% 
	verify(mid %in% mids) %>% 
	verify(mids %in% mid)


ttemp <- d6 %>% 
	filter(type == "end") %>% 
	count(mid) %>% 
	verify(n == 1) %>% 
	verify(mid %in% mids) %>% 
	verify(mids %in% mid)

## Child deaths ----------------
# 
# Add child deaths

c1 <- children %>%
	filter(!is.na(doddat)) %>%  
	transmute(mid = mddbid, date = doddat, cid = bddbid, type = "death",
	offset = 5) %>% 
	filter(mid %in% unique(d6$mid))

d7a <- bind_rows(d6, c1) %>% 
	filter(!is.na(date)) %>% 
	arrange(mid, date, offset)


vtemp <- d7a %>%
	left_join(start_and_enddates) %>% 
	assert(not_na, sdate, edate) %>% 
	filter(date >= sdate, date <= edate) %>% 
	verify(date >= sdate) %>% 
	verify(date <= edate)

## Multiple births -------------
# 
# Detect and set indicator for multiplebirths
# 
# mulitples are all births where date is <> 60 days. Set date to min of set of multiples dates. 
# These are detected by checking difference between succeceuent dates of births of the same mother.


d7 <- d7a %>% arrange(mid, date, offset) %>% 
mutate(nr = row_number())

# mulitples are all where date is <> 60 days
# set date to min of set of multiples dates

# Find links between lag dates
close_b <- d7 %>% 
	filter(type == "birth") %>%
	select(mid, nr, date) %>% 
	group_by(mid) %>% 
	mutate(
		next_b = lead(date, default = 0),
		prev_b = lag(date, default = 0)
	) %>% 
	ungroup() %>% 
	mutate(
		next_b2 = ifelse(
			next_b < (date+60) & next_b > (date-60), 
			lead(nr), 
			nr
	),
		prev_b2 = ifelse(
			prev_b < (date+60) & prev_b > (date-60), 
			lag(nr), 
			nr
		)
	)

twins <- close_b %>% 
	filter(next_b2 != prev_b2) %>% 
	mutate(mult_id = histmaps::create_block(next_b2, prev_b2)) %>% 
	group_by(mult_id) %>% 
	mutate(
		multiple = n(),
		mult_date = min(date)
	) %>% 
	select(nr, mult_id, multiple, mult_date)

d8 <- d7 %>% 
	left_join(twins)

d9 <- d8 %>% 
	mutate(
		date = as.Date(ifelse(!is.na(multiple), mult_date, date), origin = "1970-01-01"),
		multiple = ifelse(type == "birth" & is.na(multiple), 1, multiple)
	) %>% 
	select(-mult_date)

tmp <- d9 %>%
	filter(multiple > 1) %>% 
	count(mid, date)

assert_that(min(tmp$n) > 1)

# Check distribution of multiplebirth size
if (interactive()) tmp %>% count(n)

## Remove excess events -----------------------
# 
# First check one marriage date and one end date per women

tmp <- d9 %>% 
	group_by(mid) %>% 
	summarise(mars = sum(ifelse(type == "marriage", 1, 0)),
	ends = sum(ifelse(type == "end", 1, 0)))

assert_that(all(tmp$mars < 2))
assert_that(all(tmp$ends < 2))

# get all with no marr dates
nomars <- tmp %>% filter(mars == 0 | ends == 0)
d10 <- d9 %>% filter(!mid %in% nomars$mid)

tmp <- d10 %>% 
group_by(mid) %>% 
summarise(mars = sum(ifelse(type == "marriage", 1, 0)),
ends = sum(ifelse(type == "end", 1, 0)))

assert_that(all(tmp$mars == 1))
assert_that(all(tmp$ends == 1))

# fill parish and cid
fill_vals <- function(d){
	x <- fill(d, pid, cid, bdate)
	x
}

library(multidplyr)


d10a <- d10 %>% 
	arrange(mid, date, offset) %>% 
	nest(-mid) %>% 
	partition(mid) %>%
	cluster_library("tidyverse") %>%
	cluster_assign_value("fill_vals", fill_vals) %>%
	mutate(data = map(data, fill_vals)) %>% 
	collect()


d10a <- d10a %>% 
	select(mid, data) %>% 
	unnest() %>% ungroup()

## Add gender ---------------------------
# 
# Add gender and calculate number children alive by gender. This is done by setting a birth as a 1 and a death as -1, 
# then cumulativly adding up number of children by marriage.


d12 <- children %>% 
	select(bddbid, gender = kon) %>% 
	left_join(d10a, ., by = c("cid" = "bddbid"))

d13 <- d12 %>% 
	mutate(
		children = ifelse(type == "birth", 1, 0),
		boys = ifelse(type == "birth" & gender == 1, 1, 
		ifelse(type == "death" & gender == 1, -1, 0)),
		girls = ifelse(type == "birth" & gender == 2, 1, 
		ifelse(type == "death" & gender == 2, -1, 0))
	) %>% 
	group_by(mid) %>% 
	mutate(
		children = cumsum(children),
		boys = cumsum(boys),
		girls = cumsum(girls)
	) %>% ungroup()

## Simultainious events --------------------------------
# 
# Remove one of simultainious events, for example twins, out and end


# find all simultainious events
dups <- d13 %>% 
	select(mid, nr, date, offset) %>% 
	distinct() %>% 
	group_by(mid, date) %>%
	# select highest offset event
	filter(offset == max(offset)) %>% 
	# select last of remaining simultainious events
	filter(row_number() == max(row_number())) %>% 
	ungroup()

# remove dups
d13b <- d13 %>% 
	filter(nr %in% dups$nr)

## Remove illogical births ----------------------
# 
# Some birhts occur very shortly after another birth. Births with an intervall shorter tan < 8 months after previous are removed.

# get all births calculate difference to prev
illogical_births <- d13 %>%
	filter(type == "birth") %>% 
	arrange(mid, date) %>% 
	group_by(mid) %>% 
	mutate(prev_b = lag(date, default = 0)) %>% 
	ungroup() %>% 
	mutate(prev_b = as.Date(ifelse(prev_b == org, NA, prev_b), origin = org)) %>% 
	filter(diff_months(date,  prev_b) < 8) 

message(nrow(illogical_births), " illogical births")

d14 <- d13 %>% 
	filter(!nr %in% illogical_births$nr)

if (interactive()) {
	
illogical_births %>% mutate(diff = diff_months(date,  prev_b)) %>% 
	ggplot(aes(diff)) + 
	geom_histogram(binwidth = 1) +
	labs(x = "birth interaval in months")
}

## Conception date ------------------
# 
# Create a conception date, assumed to occur 9 months before birth, or at time of previous birth, which ever is last.


conse <- d14 %>% 
	select(mid, date, type, nr, cid) %>%  
	filter(type == "birth") %>% 
	mutate(c_date = date - 266)

# check if before previous birth
pre_cons <- conse %>% 
	arrange(mid, date) %>% 
	group_by(mid) %>% 
	mutate(p_date = lag(date)) %>%
	ungroup() %>% 
	filter(c_date < p_date)

if (interactive()) {
	pre_cons %>% mutate(diff = c_date - p_date) %>% 
		count(diff) %>% knitr::kable(caption = "Difference between assumed conseption date and date of previous birth where difference is < 0", bookdown = T)
}

# set cons at prev birth + 1 day
new_pre_cons <- pre_cons %>% 
	transmute(nr, new_c_date = p_date + 1) 

conse2 <- left_join(conse, new_pre_cons) %>% 
	mutate(c_date = as.Date(ifelse(!is.na(new_c_date), new_c_date, c_date), origin = "1970-01-01")) %>% 
	transmute(
		mid, pid = NA, date = c_date, type = "cons", offset = 3.5, 
		cid = cid, parish = NA, nr = NA, mult_id = NA, multiple = NA, 
		gender = NA, children = NA, 
		boys = NA, girls = NA, bdate = NA
	) 

d15 <- bind_rows(d14, conse2) %>%
	arrange(mid, date, offset) %>% 
	mutate(nr = row_number())

## Prev_birth & birth event parity -----------
# 
# prev_id = id of previously born child  
# if conseption date set prev_id = cid, the cid will be filled down in a later stage

d16a <- d15 %>% 
	arrange(mid, date, offset) %>% 
	mutate(cid = ifelse(type == "cons", cid, NA)) 

# Calculate birth parity by setting all birth events to 1 then cummulativly summing the parity 
# for each couple. As multiple births have been combined to one event, the parity refers to number of 
# birth events rather than number of children.
# 
# if after marriage set to 1 and cumsum
# if before marriage set to -1 and cumsum in reverse


marr_date <- d16a %>% 
	filter(type == "marriage") %>% 
	transmute(mid = mid, marr_date = date)

d16 <- d16a %>% 
	left_join(marr_date) %>% 
	arrange(mid, date, offset) %>%
	mutate(parity = ifelse(type == "birth" & date >= marr_date, 1, 0)) %>% 
	group_by(mid) %>% 
	mutate(
		parity = cumsum(parity),
		parity = ifelse(date < marr_date, -1, parity)
	) %>% 
	ungroup()

vtemp <- d16 %>%
left_join(start_and_enddates) %>% 
assert(not_na, sdate, edate) %>% 
filter(date >= sdate, date <= edate) %>% 
verify(date >= sdate) %>% 
verify(date <= edate)


# ## Fill missing  ------------------------------
# 
# After a variable is set, for example gender of previously born child, the state of that variable should 
# remain until it changes, thus I fill missing infomation, chronologically until the variable changes. This is 
# done using multiprocessing as the procedure is time consuming.


fill_vals <- function(d){
	x <- fill(d, pid, parish, nr, multiple, gender, children, 
	boys, girls, con_ind, bdate)
	x
}


fill_vals_down <- function(d){
	x <- fill(d, pid, parish, bdate, .direction = "up")
	x
}

d17 <- d16 %>% 
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

## Verfy and save data ----------------

d20 <- d17 %>% mutate(parish = ifelse(parish == 0, NA, parish))

ehd <- d20 %>%
	left_join(start_and_enddates) %>% 
	assert(not_na, sdate, edate) %>% 
	filter(date >= sdate, date <= edate) %>% 
	verify(date >= sdate) %>% 
	verify(date <= edate) 

mids <- unique(d20$mid)

ttemp <- ehd %>% 
	filter(type == "marriage") %>% 
	count(mid) %>% 
	verify(n == 1) %>% 
	verify(mid %in% mids) %>% 
	verify(mids %in% mid)


ttemp <- ehd %>% 
	filter(type == "end") %>% 
	count(mid) %>% 
	verify(n == 1) %>% 
	verify(mid %in% mids) %>% 
	verify(mids %in% mid)


ttemp <- ehd %>% 
	filter(str_detect(type, "end_")) %>% 
	count(mid) %>% 
	verify(n == 1) %>% 
	verify(mid %in% mids) %>% 
	verify(mids %in% mid)

save(ehd, file = ".cache/ehd.rda")
