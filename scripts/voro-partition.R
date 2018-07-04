# voro-partition.R

library(sp)
library(maptools)
library(rgeos)
library(histmaps)
library(survival)
library(deldir)
library(broom)
library(tidyverse)

load('data/ehd_loc.rda')


cluster_points <- function(xy, ratio=0.15){
	# Cluster points 
	# N centers is set to avreage yearly population in parish *ratio
	set.seed(123)
	
	av_obs <- xy %>% group_by(year) %>% summarise(pop = sum(pop)) %>% ungroup() %>% summarise(av = mean(pop))%>% pluck("av")
	xy <- xy %>% select(x, y) %>% distinct()
	clusts <- as.data.frame(xy) %>% 
		kmeans(round(av_obs * ratio), 500)
	augment(clusts, as.data.frame(xy))
}


extract_tiles <- function(voronoi, crs){
	# extract tiles from voronoi object
	tiles <- tile.list(voronoi)
	
	PPS <- list()
	for ( i in 1:length(tiles)){
		t1 <- tiles[[i]]
		pp <- Polygon(coordinates(cbind(t1$x, t1$y)))
		PPS <- c(PPS, Polygons(list(pp), as.character(i)))
	}
	
	SpatialPolygons(PPS, proj4string = CRS(crs))
	
}

cut_to_region <- function(voro, polys){
	# Cut voros to boundaries of parish
	gIntersection(voro, polys, byid =T, drop_lower_td = T)
}


subset_points <- function(points, polys){
	# subset points within the boundaries of a parish
	crs <- proj4string(polys)
	xy_coo <- SpatialPointsDataFrame(coordinates(as.data.frame(points[ ,c("x", "y")])), data= as.data.frame(points[ ,c("year", "pop")]), proj4string = CRS(crs))
	containes <- gContains(polys, xy_coo, byid = TRUE)
	xy_coo[as.vector(containes), ]
}

# Extract parish boundary data
crs <- proj4string(histmaps::hist_parish)
nofrs <-  c(82980,83010,83000,82983,82990,82988,82981, 82984, 82986)
prs <- subset(hist_parish, dedik %in% c(nofrs) & from <= 1935 & tom >= 1935)


# Get location data and caculate yearly population for each location
xy1 <- ehd_loc %>% 
	filter(
		!is.na(x)
	) %>% 
	# mutate(year = lubridate::year(date)) %>% 
	distinct(mid, date, x, y) %>% 
	filter(date >= "1880-01-01", date < "1950-01-01") %>% 
	arrange(mid, date)

library(survival)
xy2 <- xy1 %>% 
	group_by(mid) %>% 
	mutate(end_date = lead(date)) %>% 
	ungroup() %>% 
	filter(!is.na(end_date)) %>% 
	mutate(date = lubridate::decimal_date(date),end_date = lubridate::decimal_date(end_date)) %>% 
	filter(date < end_date)

xy3 <- survSplit(Surv(date, end_date, rep(1, nrow(xy2)))~., cut = 1880:1950, episode = "theid", data= xy2) %>% as_tibble()

xy <- xy3 %>% 
	mutate(durr = end_date - date, year = theid + 1878) %>% 
	group_by(x, y, year) %>% 
	summarise(pop = sum(durr))


# Wrapper function
voroni_region <- function(reg, points, ratio){
	p_sub <- subset_points(points, reg)
	xy <- as.data.frame(p_sub)
	bounds <-  as.vector(t(bbox(reg)))  
	xy_un <- distinct(xy, x, y)
	voronoi <- deldir(xy_un$x, xy_un$y, rw = bounds)
	spp <- extract_tiles(voronoi, proj4string(reg))
	spp <- cut_to_region(spp, reg)
	
	clust <- cluster_points(xy, ratio)
	clust_u <- distinct(clust[,c("x", "y", ".cluster")])
	
	spp_u <- unionSpatialPolygons(spp, paste(reg@data$geomid, clust_u$.cluster))
	clust <- mutate(clust, .cluster = paste(reg@data$geomid, .cluster))
	list(poly = spp_u, clusts = clust)
}


# Iterate over parishes and run partioning function
r <- 0.015
res <- lapply(prs@data$geomid, function(x){
	s_prs <- subset(prs, geomid == x)
	ratio <- r
	if (s_prs@data$geomid == 139)
		ratio <- 0.002
	voroni_region(s_prs, xy, ratio)
})

voros <- res[[1]]$poly
for (i in 2:length(res)){
	voros <- spRbind(voros, res[[i]]$poly)
}

# Save place-neighbourhood assignements

assigns <- plyr::ldply(res, function(x){
	x$clusts
}) %>% tbl_df()

locations_v <- assigns %>% rename(voro = .cluster)
save(locations_v, file = ".cache/locations_v.rda")


# save neigbourhood spatial boundary data

vdf <- assigns %>% count(.cluster) %>% 
	rename(id = .cluster)
df <- data.frame(id = sapply(slot(voros, "polygons"), slot, "ID"))
df <- left_join(df, vdf)
areas <- gArea(voros, byid = TRUE)
df$area <- areas
df <- df %>% 
	mutate(
		area = area/1000,
		dens = n/area
	)
rownames(df) <- df$id
v_spdf <- SpatialPolygonsDataFrame(voros, df)


voroni_spdf <- v_spdf
save(voroni_spdf, file = "data/voroni_spdf.rda")

# 
# # for each voro calculate avreage population
# 
# get_pop <- function(poly, points){
# 	p_sub <- subset_points(points, poly)
# 	p_d <- as.data.frame(p_sub)
# 	av_obs <- p_d %>% group_by(year) %>% summarise(pop = sum(pop)) %>% ungroup() %>% summarise(av = mean(pop))%>% pluck("av")
# 	# max(av_obs$pop)
# 	av_obs
# }
# 
# popus <- plyr::ldply(names(voros), function(x){
# 	data_frame(id = x, pop = get_pop(voros[x], xy))
# })
# 
# popus <- popus %>% 
# 	mutate(
# 		pop_g = cut(pop, c(0,10, 50, 250, 5000))
# 	)
# 
# rownames(popus) <- sapply(slot(voros, "polygons"), slot, "ID")
# v_pop <- SpatialPolygonsDataFrame(voros, popus)
# 
# v_df <- sp_to_ggplot(v_pop)
# 
# 
# 
# p1 <- ggplot(v_df, aes(long, lat, group = group, fill =  pop_g)) +
# 	geom_polygon(color = "white") +
# 	scale_fill_grey()+
# 	coord_equal() +
# 	theme_void()
# 
# library(gridExtra)
# 
# grid.arrange(p1, p2, nrow = 1)
# 
