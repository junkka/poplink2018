#' Transfrom ehd_data to fertility data
#' 
#' Starts 1 year before marriage
#' ends at end
#' remove all events between conseption and birth
#' 
#' @param x ehd_data
#' @export

to_fert_data <- function(x){
	y <- x %>% 
		group_by(mid) %>% 
		mutate(
			m_sdate = date[type == "marriage"],
			m_edate = date[type == "end"]
		) %>% 
		ungroup() %>% 
		mutate(m_sdate = m_sdate - years(1)) %>% 
		filter(date >= m_sdate, date <= m_edate)
	
	# date inbetween cons and birth 
	# if date > cid cons date and
	# if date < cid birth date
	cids <- y %>% 
		filter(type == "cons") %>% 
		transmute(cid = cid, con_date = date) %>% 
		distinct()
	
	births <- y %>% 
		filter(type == "birth") %>% 
		transmute(cid = cid, birth_date = date) %>% 
		distinct()
	
	z <- y %>% 
		left_join(cids) %>% 
		left_join(births) %>% 
		filter(date <= con_date | date >= birth_date | is.na(con_date) | is.na(birth_date))
	
	z
}