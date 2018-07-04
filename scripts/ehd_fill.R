# Fill values 

library(tidyverse)
library(multidplyr)

load(".cache/ehd_dens.rda")

fill_vals <- function(d){
	x <- fill(d, pid, marr_date:population)
	x
}


fill_vals_down <- function(d){
	x <- fill(d, parish, marr_date, bdate, hisco, socpo_label, hisclass, 
						ses, ses_label, m_mig, p_mig, mig_prop,density, population, place,
						x, y, mid_b_parish, pid_b_parish, parish,
						.direction = "up")
	x
}


d <- ehd_dens %>% 
	arrange(mid, date, offset) %>% 
	nest(-mid) %>% 
	partition(mid) %>%
	cluster_library("tidyverse") %>%
	cluster_assign_value("fill_vals", fill_vals) %>%
	cluster_assign_value("fill_vals_down", fill_vals_down) %>%
	mutate(data = map(data, fill_vals), data = map(data, fill_vals_down)) %>% 
	collect() %>% 
	select(mid, data) %>% 
	unnest() %>% ungroup()

eh_data <- d %>%  
	replace_na(list(fackf_size = 0, nykt_size = 0, fackf_nei = 0, nykt_nei = 0))

save(eh_data, file = "data/eh_data.rda", compress = "xz")


