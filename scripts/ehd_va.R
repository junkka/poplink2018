# Add voluntary association membership

library(assertr)
library(tidyverse)
library(lubridate)

source("R/dates.R")

load(".cache/ehd_class.rda")
load("data/person.rda")

person0 <- person %>% 
	ungroup() %>% 
	mutate(
		fack_date = as.Date(paste0(fakfar, "-01-01"))
	) %>% 
	filter(!is.na(as.numeric(bdate)), !is.na(as.numeric(fack_date))) %>% 
	mutate(
		age = diff_years(fack_date, bdate),
		fakfar = ifelse(age < 18 & fakfar > 0, fakfar + as.integer(18 - age), fakfar)) %>% 
	select(-fack_date, -age)

person2 <- person %>% filter(!id %in% person0$id) %>% 
	bind_rows(person0)

# I add two types of variables, one indicator for membership, set as a static variable and another set as an event. First, I add the indicator.

tmp <- person2 %>% 
	select(id, nykt, nyktar, fakf, fakfar, regel) 

colnames(tmp) <- c("mid", "m_nykt", "m_nykt_ar", "m_fack", "m_fack_ar", "m_regel")
ehd2 <- left_join(ehd_class, tmp)

colnames(tmp) <- c("pid", "p_nykt", "p_nykt_ar", "p_fack", "p_fack_ar", "p_regel")
ehd3 <- left_join(ehd2, tmp)

# Set variable states.


ehd_va <- ehd3 %>% 
	mutate(
		p_nykt = ifelse(is.na(p_nykt), 0, p_nykt),
		m_nykt = ifelse(is.na(m_nykt), 0, m_nykt),
		p_fack = ifelse(is.na(p_fack), 0, p_fack),
		m_fack = ifelse(is.na(m_fack), 0, m_fack),
		nykt = ifelse(p_nykt == 1 | m_nykt == 1, 1, 0),
		fack = ifelse(p_fack == 1 | m_fack == 1, 1, 0)
	) %>% 
	group_by(mid, pid) %>% 
	mutate(
		nykt_ar = min(c(p_nykt_ar, m_nykt_ar), na.rm = T),
		fack_ar = p_fack_ar
	) %>% ungroup()

# Create events. Three types of events,
# 
# * Man joins a union
# * Man joins a temperance lodge
# * Women joins a temperance lodge


fack <- person2 %>% 
	filter(fakfar != 0) %>% 
	select(id, fakfar) %>% 
	rename(pid = id) %>% 
	mutate(fakfar = as.Date(paste0(fakfar, "-01-01")), fack_ind = 1)

tmp2 <- ehd_va %>% 
	filter(!is.na(pid)) %>% 
	group_by(mid, pid) %>% 
	arrange(date, offset) %>% 
	slice(1) %>% ungroup()

tmp3 <- left_join(fack, tmp2) %>% 
	mutate(date = fakfar) %>% 
	select(mid, pid, date, type, marr_date, fack_ind) %>% 
	mutate(
		type = "fack"
	)

va_1 <- bind_rows(tmp3, ehd_va) %>% 
	arrange(mid, date, offset)

fack <- person2 %>% 
	filter(nyktar != 0) %>% 
	select(id, nyktar) %>% 
	rename(pid = id) %>% 
	mutate(nyktar = as.Date(paste0(nyktar, "-01-01")), p_nykt_ind = 1)

tmp2 <- ehd_va %>% 
	filter(!is.na(pid)) %>% 
	group_by(mid, pid) %>% 
	arrange(date, offset) %>% 
	slice(1) %>% ungroup()

tmp3 <- left_join(fack, tmp2) %>% 
	mutate(date = nyktar) %>% 
	select(mid, pid, date, type, marr_date, p_nykt_ind) %>% 
	mutate(
		type = "p_nykt"
	)

va_2 <- bind_rows(tmp3, va_1) %>% 
	arrange(mid, date, offset)



fack <- person2 %>% 
	filter(nyktar != 0) %>% 
	select(id, nyktar) %>% 
	rename(mid = id) %>% 
	mutate(nyktar = as.Date(paste0(nyktar, "-01-01")), m_nykt_ind = 1)

tmp2 <- ehd_va %>% 
	filter(!is.na(pid)) %>% 
	group_by(mid, pid) %>% 
	arrange(date, offset) %>% 
	slice(1) %>% ungroup()

tmp3 <- left_join(fack, tmp2) %>% 
	mutate(date = nyktar) %>% 
	select(mid, pid, date, type, marr_date, m_nykt_ind) %>% 
	mutate(
		type = "m_nykt"
	)

va_3 <- bind_rows(tmp3, va_2) %>% 
	arrange(mid, date, offset)

va_5 <- va_3 %>% 
	filter(!is.na(marr_date)) %>% 
	group_by(mid) %>% 
	mutate(
		mindates = ifelse(!type %in% c('fack', 'm_nykt', 'p_nykt'), date, NA),
		mindate = min(mindates, na.rm = T)
	) %>% 
	ungroup()

ehd_va <- va_5 %>% ungroup()
# Visual inspection

if (interactive()){
		
	
	library(ggplot2)
	library(gridExtra)
	library(tidyr)
	
	dd <- ehd_va %>%
		replace_na(list(p_nykt = 0, p_fack = 0, m_nykt = 0, m_fack = 0)) %>% 
		mutate(
			year = lubridate::year(marr_date)
		) %>% 
		distinct( mid, year, p_nykt, p_fack, m_nykt, m_fack) %>%
		gather(var, val, p_nykt, p_fack, m_nykt, m_fack) %>% 
		mutate(val = factor(val, labels = c("Non-member", "member")))
	
	ehd_va %>%
		replace_na(list(p_nykt = 0, p_fack = 0, m_nykt = 0, m_fack = 0)) %>% 
		mutate(
			year = lubridate::year(marr_date)
		) %>%
		distinct( mid, year, p_nykt, p_fack, m_nykt, m_fack) %>%
		group_by( mid) %>%
		summarise(
			nykt = max(p_nykt + m_nykt),
			fack = max(p_fack + m_fack)
		) %>% ungroup() %>%
		count(nykt)
	
	f <- function(x){
		ggplot(x, aes(year)) +
			geom_histogram() +
			facet_wrap(~val,scales = "free_y", ncol = 1) +
			labs(title = x$var[1]) +
			theme_minimal()
	}
	
	pss <- dd %>% group_by(var) %>%
		do(pl  = f(.))
	
	do.call("grid.arrange", pss$pl)
	
}

# Save 


save(ehd_va, file=".cache/ehd_va.rda")

