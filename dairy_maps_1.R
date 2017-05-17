library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(scales)

setwd("/Users/kota/Dropbox/R_projects/dairy_maps")

# ---- county and state shape map ----------
# library(tigris)
# library(stringi)
# shape_counties  <- counties(cb=TRUE, resolution = "20m")
# shape_counties$FIPS <- shape_counties$GEOID
# shape_counties$id   <- rownames(shape_counties@data)
# shape_counties@data$NAME <- stri_encode(shape_counties@data$NAME,"","UTF-8")
# 
# shape_states  <- states(cb=TRUE, resolution = "20m")
# shape_states$FIPS <- shape_states$GEOID
# shape_states$id   <- rownames(shape_states@data)
# shape_states@data$NAME <- stri_encode(shape_states@data$NAME,"","UTF-8")
# 
# save(shape_counties, shape_states,  file = "data/shape_counties.RData")
# 

# --------------------
# geographic shape data 

cnty <- map_data("county") # using ggplot2
states <- map_data("state") # using ggplot2

load("data/shape_counties.RData")
df_shape_counties <- tidy(shape_counties) # convert to data frame
df_shape_counties <- df_shape_counties %>%
  inner_join(shape_counties@data, by="id") 
df_shape_counties$region <- df_shape_counties$FIPS

df_shape_states <- tidy(shape_states) # convert to data frame
df_shape_states <- df_shape_states %>%
  inner_join(shape_states@data, by="id") 
df_shape_states $region <- df_shape_states$FIPS

load("data/ag_census_07.RData")
load("data/ag_census_12.RData")
load("data/labor_stats.RData")

# ---------------------
# functions for mapping 

range_to <- function(x) {
  gsub("\\(", "", gsub(",", " to ", gsub("]", "", x)))
}

my_ordered_levels <- function(breaks, large_limits=FALSE, comma=TRUE) {
  if (comma) breaks <- formatcomma(breaks) 
  rlt <- paste(lag(breaks), "to", breaks)[-1] 
  if (large_limits) {
    rlt[1] <- paste("<", breaks[2])
    rlt[length(rlt)] <- paste(">", breaks[(length(breaks)-1)])
  }
  rlt
}

formatcomma <- function(x, digits=NULL, dollar=FALSE) {
  if (length(x)==0) { return(NA) }
  if (is.null(digits)) {
    xFormat <- format(x, big.mark=",", scientific=FALSE) 
  } else {
    xFormat <- format(round(x,digits), big.mark=",", scientific=FALSE) 
  }
  if (dollar) { xFormat <- paste0("$", xFormat) }
  return(xFormat)
} 


map_dairy_cnty <- function(df, var, title = "", legend_fill = "", 
                           st_region_keep, 
                           st_fips_keep, map_level = "county", rm.na =TRUE) {
  
  if (map_level == "county") {
    df_shape <- df_shape_counties
    map_id <- "FIPS"
    cnty_in_state <- st_region_keep
  } else {
    df_shape <- df_shape_states
    map_id <- "state"
    cnty_in_state <- c()
  }
  
  if (rm.na) df <- df[!is.na(df[[var]]),]
  
  df %>%
    ggplot() +  
    geom_polygon(data =  subset(cnty, region %in% cnty_in_state),
                 mapping = aes(x = long, y = lat, group = group),
                 colour ="gray", fill = NA) +
    geom_map(aes_(map_id = as.name(map_id), fill = as.name(var)),
             map = subset(df_shape, STATEFP %in% st_fips_keep)) +
    geom_polygon(data = subset(states, region %in% st_region_keep),
                 mapping = aes(x = long, y = lat, group = group),
                 colour ="gray40", fill = NA) +
    coord_quickmap() + #xlim(-105, - 87) + ylim( 40, 50) +
    # scale_fill_brewer(palette = "RdYlBu") +
    # scale_fill_gradientn(colours =  brewer.pal(9,"RdYlBu")[-(1:4)]) +
    # scale_fill_gradient(low = "#132B43", high = "#56B1F7") +
    labs(title = title,
         y = NULL, x = NULL, fill = legend_fill) +
    theme_void() +
    theme(legend.position = "bottom", legend.margin = margin(t = -1, unit = 'cm'))
}

  
map_add_footnote  <- function(plot1, footnote, save.as) {

  g <- arrangeGrob(plot1, bottom = textGrob(footnote, x=0, hjust = -0.1, vjust = 0.4,
                                            gp = gpar(fontface="italic", fontsize=11)))
  grid.draw(g)
  if (length(save.as)>0) { 
    ggsave(paste0(save.as,".png"), g ) #dpi = 600)
    dev.off()
  }
}


# -------------------------------
# dairy_jobs
# -------------------------------

# MN, WI, ND, SD, IA
st_fips_keep1 <- c("27", "55", "38", "46", "19")
st_region_keep1 <- c("minnesota", "wisconsin", "north dakota", "south dakota", "iowa")


df_dairy_farming <- df_dairy_farming %>% 
  mutate( 
    FIPS = area_fips,
    ST_FIPS = state %>% as.numeric()
  ) 

df_dairy_farming <- df_dairy_farming %>% 
  arrange(FIPS, year) %>%
  group_by(FIPS) %>%
  mutate(
    emplvl_lag3 = dplyr::lag(avg_emplvl, 3),
    emplvl_diff3 = avg_emplvl - emplvl_lag3
  ) %>% ungroup()


my_breaks_change_jobs_1 <- c(-500, -300, -200, -100, 0, 100, 200, 300, 500)


df_dairy_farming <- df_dairy_farming %>% 
  mutate( 
    change_jobs_gr = cut(emplvl_diff3, breaks = my_breaks_change_jobs_1)
    %>% range_to() %>%
      ordered(levels =  c("-300 to -200", "-200 to -100", "-100 to 0", "",  # added dummy ""
                          "0 to 100", "100 to 200", "200 to 300", "300 to 500", NA))
  )

df_dairy_farming$change_jobs_gr[df_dairy_farming$emplvl_diff3==0] <- NA

df_dairy_farming$change_jobs_gr %>% table()

df_dairy_farming %>% filter(year==2012) %>% 
  group_by(year, ST_FIPS) %>%
  summarise(sum(avg_emplvl)) %>% print(n=60)



loc_colors <- brewer.pal(8, "RdYlBu")[-1]
loc_colors <- c(loc_colors[1:3], "white", loc_colors[4:8])

( map_dairy_cnty(df_dairy_farming  %>%  
                  filter(state %in% st_fips_keep1, year ==2015), 
                var ="change_jobs_gr", 
                title= paste("Change in Employment in Dairy Cattle and Milk Production, 2012 to 2015"), 
                legend_fill="Change (jobs):", 
                st_region_keep=st_region_keep1, 
                st_fips_keep= st_fips_keep1) + 
    scale_fill_manual(values = loc_colors, drop=FALSE) +
    guides(fill=guide_legend(ncol=4, byrow=TRUE)) 
) %>%
  map_add_footnote(footnote="Data Source: Bureau of Labor Statistics, NAICS 112120.",
                   save.as ="img/dairy_farming_jobs_change")


for (yr in c(2012, 2015)) {
  ( map_dairy_cnty(df_dairy_farming  %>%  
                    filter(state %in% st_fips_keep1, year ==yr, avg_emplvl>0), 
                  var = "avg_emplvl", 
                  title= paste("Employment Dairy Cattle and Milk Production,", yr), 
                  legend_fill="Jobs:",
                  st_region_keep=st_region_keep1, 
                  st_fips_keep= st_fips_keep1) + 
      scale_fill_gradientn(colours =  brewer.pal(9,"YlGnBu")[-(1:4)]) 
  ) %>%
    map_add_footnote(footnote="Data Source: Bureau of Labor Statistics, NAICS 112120.",
                     save.as =paste0("img/dairy_farming_jobs_", yr))
}



# -------------------------------
# dairy_manufacturing_jobs
# -------------------------------

# MN, WI, IA
st_fips_keep2 <- c("27", "55", "19")
st_region_keep2 <- c("minnesota", "wisconsin", "iowa")


df_dairy_food_mf <- df_dairy_food_mf %>% 
  mutate( 
    FIPS = area_fips,
    ST_FIPS = state %>% as.numeric()
  ) 

df_dairy_food_mf <- df_dairy_food_mf %>% 
  arrange(FIPS, year) %>%
  group_by(FIPS) %>%
  mutate(
    emplvl_lag3 = dplyr::lag(avg_emplvl, 3),
    emplvl_diff3 = avg_emplvl - emplvl_lag3
  ) %>% ungroup()


my_breaks_change_jobs_2 <- c(-2000, -300, -200, -100, 0, 100, 200, 300, 2000)

df_dairy_food_mf <- df_dairy_food_mf %>% 
  mutate( 
    change_jobs_gr = cut(emplvl_diff3, breaks = my_breaks_change_jobs_2,
                         ordered_result =TRUE, 
                         labels = my_ordered_levels(my_breaks_change_jobs_2,
                                                    large_limits=TRUE))
  )

df_dairy_food_mf$change_jobs_gr[df_dairy_food_mf$emplvl_diff3==0] <- NA

df_dairy_food_mf$emplvl_diff3 %>% summary()
df_dairy_food_mf$change_jobs_gr %>% table()

df_dairy_food_mf %>%  
  filter(state %in% st_fips_keep1) %>% print(n=30)


( map_dairy_cnty(df_dairy_food_mf  %>%  
                  filter(state %in% st_fips_keep2, year ==2015), 
                var ="change_jobs_gr", 
                title= paste("Change in Employment in Dairy Product Manufacturing, 2012 to 2015"), 
                legend_fill="Change (jobs):",
                st_region_keep=st_region_keep2, 
                st_fips_keep= st_fips_keep2) + 
    scale_fill_manual(values =  brewer.pal(8, "RdYlBu"), drop=FALSE) +
    guides(fill=guide_legend(ncol=4, byrow=TRUE)) 
) %>%
  map_add_footnote(footnote="Data Source: Bureau of Labor Statistics, NAICS 3115.",
                   save.as ="img/dairy_food_mf_jobs_change")


for (yr in c(2012, 2015)) {
  ( map_dairy_cnty(df_dairy_food_mf  %>%  
                    filter(state %in% st_fips_keep2, year ==yr, avg_emplvl>0), 
                  var = "avg_emplvl", 
                  title= paste("Employment in Dairy Product Manufacturing,", yr), 
                  legend_fill="Jobs:",
                  st_region_keep=st_region_keep2, 
                  st_fips_keep= st_fips_keep2) + 
      scale_fill_gradientn(colours =  brewer.pal(9,"YlGnBu")[-(1:4)]) 
  ) %>%
    map_add_footnote(footnote="Data Source: Bureau of Labor Statistics, NAICS 3115.",
                     save.as =paste0("img/dairy_food_mf_jobs_", yr))
}


# National level

st_dairy_food_mf <- st_dairy_food_mf %>% 
  mutate( 
    FIPS = area_fips,
    ST_FIPS = state %>% as.numeric()
  ) 

st_dairy_food_mf <- st_dairy_food_mf %>% 
  arrange(FIPS, year) %>%
  group_by(FIPS) %>%
  mutate(
    emplvl_lag3 = dplyr::lag(avg_emplvl, 3),
    emplvl_diff3 = avg_emplvl - emplvl_lag3
  ) %>% ungroup()


my_breaks_change_jobs_3 <- c(-2000, -500, -300, -100, 0, 100, 300, 500, 2000)

st_dairy_food_mf <- st_dairy_food_mf %>% 
  mutate( 
    change_jobs_gr = cut(emplvl_diff3, breaks = my_breaks_change_jobs_3,
                         ordered_result =TRUE, 
                         labels = my_ordered_levels(my_breaks_change_jobs_3,
                                                    large_limits=TRUE))
  )

st_dairy_food_mf$change_jobs_gr[st_dairy_food_mf$emplvl_diff3==0] <- NA

st_dairy_food_mf$emplvl_diff3 %>% summary()
st_dairy_food_mf$change_jobs_gr %>% table()

st_dairy_food_mf %>% print(n=30)


# exclude alaska, hawaii, puerto rico,
all_stfips <- shape_states$FIPS 
all_regions <- states$region %>% unique()

keep_lower_states <- all_stfips[!(all_stfips %in% c("02","15","72"))]
keep_lower_resions <- all_regions[!(all_regions %in% c(""))]


( map_dairy_cnty(st_dairy_food_mf  %>%  
                  filter(state %in% keep_lower_states, year ==2015), 
                var ="change_jobs_gr", 
                title= paste("Change in Employment in Dairy Product Manufacturing, 2012 to 2015"), 
                legend_fill="Change (jobs):",
                st_region_keep =keep_lower_resions, 
                st_fips_keep = keep_lower_states, map_level = "states")  +
    scale_fill_manual(values =  brewer.pal(8, "RdYlBu"), drop=FALSE) +
    guides(fill=guide_legend(ncol=4,  byrow=TRUE))
  ) %>%
  map_add_footnote(footnote="Data Source: Bureau of Labor Statistics, NAICS 3115.",
                   save.as ="img/dairy_food_mf_jobs_change_st")

for (yr in c(2012, 2015)) {
  ( map_dairy_cnty(df_dairy_food_mf  %>%  
                    filter(state %in% keep_lower_states, year ==yr, avg_emplvl>0), 
                  var = "avg_emplvl", 
                  title= paste("Employment in Dairy Product Manufacturing,", yr), 
                  legend_fill="Jobs:",
                  st_region_keep =keep_lower_resions, 
                  st_fips_keep = keep_lower_states, map_level = "states") + 
      scale_fill_gradientn(colours =  brewer.pal(9,"YlGnBu")[-(1:4)]) 
  ) %>%
    map_add_footnote(footnote="Data Source: Bureau of Labor Statistics, NAICS 3115.",
                     save.as =paste0("img/dairy_food_mf_jobs_st_", yr))
}



# ------------------------------------
# ag census dairy cow inventory
# ------------------------------------

df_ag_census <- ag_census_07 %>% 
                filter(LEVEL==1) %>%
                select(FIPS, cows_20_up) %>% 
  inner_join( ag_census_12 %>% 
                filter(LEVEL==1) %>%
                select(FIPS, NAME, STATEFIP, cows_20_up), by = "FIPS") %>% 
  mutate(
    cows_20_up_07 = cows_20_up.x,
    cows_20_up_12 = cows_20_up.y,
    change_cows = cows_20_up_12 - cows_20_up_07,
    change_cows_pct = ifelse(cows_20_up_07==0 & cows_20_up_12==0, 0, 
                             change_cows/cows_20_up_07)
  )

head(df_ag_census)

df_ag_census %>% filter(STATEFIP %in% st_fips_keep1) %>% 
  group_by(STATEFIP) %>%
  summarise(
    cows_2007 = sum(cows_20_up_07),
    cows_2012 = sum(cows_20_up_12),
    cows_change = cows_2012 - cows_2007,
    cows_change_pct = cows_change/cows_2007
  )


my_breaks_change_cows <- c(-10, -3, -2, -1, 0, 1, 2, 3, 10)
my_breaks_change_cows_pct <- c(-1, -.5, -.25, -.10, 0, .10, .25, .5, 1)
my_breaks_cows <- c(0, 10, 20, 30, 40, 50, 60, 100)


df_ag_census <- df_ag_census %>%
  mutate(
    STATEFP = STATEFIP,
    change_cows_gr2 = cut(change_cows, breaks = my_breaks_change_cows*1000,
                         ordered_result =TRUE, 
                         labels = my_ordered_levels(my_breaks_change_cows*1000, 
                                                    large_limits=TRUE)), 
    change_cows_gr = cut(change_cows/1000, breaks = my_breaks_change_cows,
                         ordered_result =TRUE, 
                         labels = my_ordered_levels(my_breaks_change_cows, 
                                                    large_limits=TRUE)), 
    change_cows_pct_gr = cut(change_cows_pct*100, breaks = my_breaks_change_cows_pct*100,
                             ordered_result =TRUE, 
                             labels = my_ordered_levels(my_breaks_change_cows_pct)),
    cows_20_up_07_gr =  cut(cows_20_up_07/1000, breaks = my_breaks_cows,
                            ordered_result =TRUE, 
                            labels = my_ordered_levels(my_breaks_cows, 
                                                       large_limits=TRUE)),
    cows_20_up_12_gr = cut(cows_20_up_12/1000, breaks = my_breaks_cows, 
                           ordered_result =TRUE, 
                           labels = my_ordered_levels(my_breaks_cows, 
                                                      large_limits=TRUE))
  )

df_ag_census$change_cows_gr[df_ag_census$change_cows == 0] <- NA
df_ag_census$change_cows_gr2[df_ag_census$change_cows == 0] <- NA

head(df_ag_census)



(  map_dairy_cnty(df_ag_census  %>%  
                  filter(STATEFIP %in% st_fips_keep), 
                var ="change_cows_gr2", 
                title= "Change in Milk Cow Inventory by County, 2007 to 2012", 
                legend_fill="Change (heads):",
                st_fips_keep = st_fips_keep1,
                st_region_keep = st_region_keep1) +
    scale_fill_manual(values =  brewer.pal(8, "RdYlBu"), drop=FALSE) +
    guides(fill=guide_legend(ncol=4, byrow=TRUE))
) %>%
  map_add_footnote(footnote="Data Source: US Agricultural Census.",
                   save.as ="img/change_cow_inventory_2007_2012")


for (yr in c(2007, 2012)) {
  var <- ifelse(yr==2007, "cows_20_up_07_gr", "cows_20_up_12_gr")
  ( map_dairy_cnty(df_ag_census  %>%  
                    filter(STATEFIP %in% st_fips_keep), 
                  var = var, 
                  title= paste("Milk Cow Inventory by County,", yr), 
                  legend_fill="1,000 heads:",
                  st_region_keep=st_region_keep1, 
                  st_fips_keep= st_fips_keep1) + 
      # scale_fill_gradientn(colours =  brewer.pal(9,"YlGnBu")[-(1:4)]) 
      scale_fill_brewer(palette = "YlGnBu")
  ) %>%
    map_add_footnote(footnote="Data Source: US Agricultural Census.",
                     save.as =paste0("img/cow_inventory_", yr))
}





# national map  2002 and 2012
load("data/ag_census_02.RData")


df_ag_census2 <- ag_census_02 %>% 
  filter(LEVEL==1) %>%
  select(FIPS, cows_20_up, farms_size_20_up) %>% 
  inner_join( ag_census_12 %>% 
                filter(LEVEL==1) %>%
                select(FIPS, NAME, STATEFIP, cows_20_up, farms_size_20_up), by = "FIPS") %>% 
  mutate(
    cows_20_up_02 = cows_20_up.x,
    cows_20_up_12 = cows_20_up.y,
    change_cows = cows_20_up_12 - cows_20_up_02,
    change_cows_pct = ifelse(cows_20_up_02==0 & cows_20_up_12==0, 0, 
                             change_cows/cows_20_up_02),
    farms_size_20_up_02 = farms_size_20_up.x,
    farms_size_20_up_12 = farms_size_20_up.y,
    change_farms = farms_size_20_up_12 - farms_size_20_up_02,
    avg_size_02 = cows_20_up_02/farms_size_20_up_02,
    avg_size_12 = cows_20_up_12/farms_size_20_up_12,
    change_size = avg_size_12 - avg_size_02
  )

head(df_ag_census2)

## some discrepancy in the data of milk cows (?): farms>0 but cows=0 in some size category
## ag_census_12 %>% filter(cows_20_up>0) %>% mutate(avg = cows_20_up/farms_size_20_up) %>% filter(avg<20) 

df_ag_census2 %>% # filter(STATEFIP %in% st_fips_keep1) %>% 
  group_by(STATEFIP) %>%
  summarise(
    cows_2002 = sum(cows_20_up_02),
    cows_2012 = sum(cows_20_up_12),
    cows_change = cows_2012 - cows_2002,
    cows_change_pct = cows_change/cows_2002
  )


my_breaks_change_cows <- c(-Inf, -20, -10, -5, 0, 5, 10, 20, Inf)
my_breaks_change_cows_pct <- c(-1, -.5, -.25, -.10, 0, .10, .25, .5, 1)
my_breaks_cows <- c(0, 10, 20, 30, 40, 50, 60, 100)

my_breaks_change_farms <- c(-Inf, -50, -30, -10, 0, 10, 30, 50, Inf)
my_breaks_change_size <- c(-Inf, -100, -50, -30, 0, 30, 50, 100, Inf)
my_breaks_size <- c(0, 50, 100, 150, 200, 250, 300, Inf)


df_ag_census2 <- df_ag_census2 %>%
  mutate(
    STATEFP = STATEFIP,
    change_cows_gr2 = cut(change_cows, breaks = my_breaks_change_cows*1000,
                          ordered_result =TRUE, 
                          labels = my_ordered_levels(my_breaks_change_cows*1000, 
                                                     large_limits=TRUE)), 
    change_cows_gr = cut(change_cows/1000, breaks = my_breaks_change_cows,
                         ordered_result =TRUE, 
                         labels = my_ordered_levels(my_breaks_change_cows, 
                                                    large_limits=TRUE)), 
    change_cows_pct_gr = cut(change_cows_pct*100, breaks = my_breaks_change_cows_pct*100,
                             ordered_result =TRUE, 
                             labels = my_ordered_levels(my_breaks_change_cows_pct)),
    cows_20_up_02_gr =  cut(cows_20_up_02/1000, breaks = my_breaks_cows,
                            ordered_result =TRUE, 
                            labels = my_ordered_levels(my_breaks_cows, 
                                                       large_limits=TRUE)),
    cows_20_up_12_gr = cut(cows_20_up_12/1000, breaks = my_breaks_cows, 
                           ordered_result =TRUE, 
                           labels = my_ordered_levels(my_breaks_cows, 
                                                      large_limits=TRUE)),
    change_farms_gr = cut(change_farms, breaks = my_breaks_change_farms,
                          ordered_result =TRUE, 
                          labels = my_ordered_levels(my_breaks_change_farms, 
                                                     large_limits=TRUE)),
    change_size_gr = cut(change_size, breaks = my_breaks_change_size,
                          ordered_result =TRUE, 
                          labels = my_ordered_levels(my_breaks_change_size, 
                                                     large_limits=TRUE)),
    avg_size_02_gr = cut(avg_size_02, breaks = my_breaks_size, 
                           ordered_result =TRUE, 
                           labels = my_ordered_levels(my_breaks_size, 
                                                      large_limits=TRUE)),
    avg_size_12_gr = cut(avg_size_12, breaks = my_breaks_size, 
                         ordered_result =TRUE, 
                         labels = my_ordered_levels(my_breaks_size, 
                                                    large_limits=TRUE))
  )

df_ag_census2$change_cows_gr[df_ag_census2$change_cows == 0] <- NA
df_ag_census2$change_cows_gr2[df_ag_census2$change_cows == 0] <- NA
df_ag_census2$change_farms_gr[df_ag_census2$change_farm == 0] <- NA
df_ag_census2$change_size_gr[df_ag_census2$change_size == 0] <- NA
df_ag_census2$avg_size_02_gr[df_ag_census2$avg_size_02 == 0] <- NA
df_ag_census2$avg_size_12_gr[df_ag_census2$avg_size_12== 0] <- NA

head(df_ag_census2)





map_add_footnote  <- function(plot1, footnote, save.as) {
  
  g <- arrangeGrob(plot1, bottom = textGrob(footnote, x=0, hjust = -0.1, vjust = -4,
                                            gp = gpar(fontface="italic", fontsize=12)))
  grid.draw(g)
  if (length(save.as)>0) { 
    ggsave(paste0(save.as,".png"), g ) #dpi = 600)
    dev.off()
  }
}


st_fips_keep_all <-  df_shape_counties$STATEFP %>% unique()
st_region_keep_all  <- states$region %>% unique()

(  map_dairy_cnty(df_ag_census2,  # %>%
                    # filter(STATEFIP %in% st_fips_keep), 
                  var ="change_cows_gr2", 
                  title= "Change in Milk Cow Inventory by County, 2002 to 2012", 
                  legend_fill="Change (1,000 heads):",
                  st_fips_keep = st_fips_keep_all,
                  st_region_keep = st_region_keep_all
                  ) +
    scale_fill_manual(values =  brewer.pal(8, "RdYlBu"), drop=FALSE) +
    guides(fill=guide_legend(ncol=4, byrow=TRUE))
) %>%
  map_add_footnote(footnote="Data Source: US Agricultural Census.",
                   save.as ="img/change_cow_inventory_2002_2012")


(  map_dairy_cnty(df_ag_census2, 
                  var ="change_farms_gr", 
                  title= "Change in Dairy Farms by County, 2002 to 2012", 
                  legend_fill="Change (farms):",
                  st_fips_keep = st_fips_keep_all,
                  st_region_keep = st_region_keep_all
) +
    scale_fill_manual(values =  brewer.pal(8, "RdYlBu"), drop=FALSE) +
    guides(fill=guide_legend(ncol=4, byrow=TRUE))
) %>%
  map_add_footnote(footnote=paste("Data Source: US Agricultural Census.",
                                  "Data include all dairy farms with 20 cows or more."),
                   save.as ="img/change_farms_2002_2012")


(  map_dairy_cnty(df_ag_census2, 
                  var ="change_size_gr", 
                  title= "Change in Average Dairy Herd Size by County, 2002 to 2012", 
                  legend_fill="Change (cows):",
                  st_fips_keep = st_fips_keep_all,
                  st_region_keep = st_region_keep_all
) +
    scale_fill_manual(values =  brewer.pal(8, "RdYlBu"), drop=FALSE) +
    guides(fill=guide_legend(ncol=4, byrow=TRUE))
) %>%
  map_add_footnote(footnote=paste("Data Source: US Agricultural Census.",
                                  "Data include all dairy farms with 20 cows or more."),
                   save.as ="img/change_size_2002_2012")



for (yr in c(2002, 2012)) {
  var <- ifelse(yr==2002, "avg_size_02_gr", "avg_size_12_gr")
  ( map_dairy_cnty(df_ag_census2,
                   var = var, 
                   title= paste("Average Dairy Herd Size by County,", yr), 
                   legend_fill="heads:",
                   st_region_keep=st_region_keep_all, 
                   st_fips_keep= st_fips_keep_all) + 
      # scale_fill_gradientn(colours =  brewer.pal(9,"YlGnBu")[-(1:4)]) 
      scale_fill_brewer(palette = "YlGnBu")
  ) %>%
    map_add_footnote(footnote=paste("Data Source: US Agricultural Census.",
                                     "Data include all dairy farms with 20 cows or more."),
                     save.as =paste0("img/farm_size_", yr))
}



# ------- hispanic population ------

load("data/hispanic.RData")

hispanic_90_15 <- hispanic_90_15 %>%
  mutate(
    FIPS = FIPS %>% as.character(),
    FIPS = ifelse(nchar(FIPS)==4,
                  paste0("0", FIPS), FIPS)
  )

hispanic_90_15 <- hispanic_90_15 %>%
  group_by(FIPS) %>% arrange(FIPS, year) %>%
  mutate(
    pop_all_l10 = dplyr::lag(pop_all, 10),    
    pop_hisp_l10 = dplyr::lag(pop_hisp, 10),
    hisp_change = pop_hisp - pop_hisp_l10,
    growth_all = (pop_all - pop_all_l10)/pop_all_l10*100/10,
    growth_hisp = (pop_hisp - pop_hisp_l10)/pop_hisp_l10*100/10
  ) %>% ungroup()

hispanic_90_15 %>% filter(FIPS=="27053") %>% "["(1:20,)

my_breaks_change_hispanic <- c(-Inf, -5000, -3000, -1000, 0, 1000, 3000, 5000, Inf)

hispanic_90_15 <- hispanic_90_15 %>%
  mutate(
    hisp_change_gr = cut(hisp_change, breaks = my_breaks_change_hispanic, 
                     ordered_result =TRUE, 
                     labels = my_ordered_levels(my_breaks_change_hispanic, 
                                                large_limits=TRUE))
)

hispanic_90_15$hisp_change_gr[hispanic_90_15$pop_hisp == 0] <- NA


(  map_dairy_cnty(hispanic_90_15  %>% filter(year==2012), 
                  var ="hisp_change_gr", 
                  title= "Change in Hispanic Population by County, 2002 to 2012", 
                  legend_fill="Change (persons):",
                  st_fips_keep = st_fips_keep_all,
                  st_region_keep = st_region_keep_all
) +
    scale_fill_manual(values =  brewer.pal(8, "RdYlBu"), drop=FALSE) +
    guides(fill=guide_legend(ncol=4, byrow=TRUE))
) %>%
  map_add_footnote(footnote=paste("Data Source: NIH National Cancer Institute."),
                   save.as ="img/change_hisp_2002_2012")

  

