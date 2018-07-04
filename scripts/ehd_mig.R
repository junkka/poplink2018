# Add migration
library(tidyverse)

load(".cache/ehd_exp.rda")
load("data/person.rda")

# add pid (Partner ID)

pids <- ehd_exp %>% distinct(mid, pid) %>% filter(!is.na(pid))
ehd_exp0 <- ehd_exp %>% 
	select(-pid) %>% 
	left_join(pids)

# Add birth parish

mid_b <- person %>% 
	mutate(b_par = ifelse(is.na(bh_parish), b_parish, bh_parish)) %>% 
	select(mid = id, mid_b_parish = b_par) %>% 
	distinct()

pid_b <- person %>% 
	mutate(b_par = ifelse(is.na(bh_parish), b_parish, bh_parish)) %>% 
	select(pid = id, pid_b_parish = b_par) %>% 
	distinct()

ehd_mig01 <- ehd_exp0 %>% 
	left_join(mid_b) %>% 
	left_join(pid_b)

# Set migration indicator
# 
# Parish == birth parish = stayer
# parish != birth parish = migrant
# birth parish in any of parishes in region = internal migrant


set_migration <- function(x, y){
	ifelse(
		is.na(y),
		"migrant",
		ifelse(
			x == y,
			"stayer",
			ifelse(
				x %in% unique(ehd_mig01$parish),
				"internal",
				"migrant"
			)
		)  
	)
	
}

ehd_mig3 <- ehd_mig01 %>% 
	mutate(
		m_mig = set_migration(parish, mid_b_parish),
		p_mig = set_migration(parish, pid_b_parish)
	)

if (interactive()){
	ehd_mig3 %>% 
		count(p_mig)
	
	ehd_mig3 %>% 
		count(m_mig)
}

# save


ehd_mig <- ehd_mig3

save(ehd_mig, file = ".cache/ehd_mig.rda")

