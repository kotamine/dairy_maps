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
                  filter(state %in% st_fips_keep, year ==2015), 
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
                    filter(state %in% st_fips_keep, year ==yr, avg_emplvl>0), 
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
  filter(state %in% st_fips_keep) %>% print(n=30)


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



# 
# # --------------------------------------------------
# # leaflet
# # --------------------------------------------------
# 
# library(tigris)
# # library(acs)
# library(stringr)
# library(stringi)
# library(leaflet)
# library(htmlwidgets)
# 
# firstcap <- function(x) {
#   lapply(1:length(x), function(i) {
#     xi <- x[i]
#     if(grepl(" ", xi)) {
#            tmp <- gregexpr(" ", xi)[[1]][1] + 1
#            substr(xi,  tmp,  tmp) <- toupper(substr(xi, tmp, tmp))
#     }
#     substr(xi, 1, 1) <- toupper(substr(xi, 1, 1))
#     xi
#   }) %>% unlist()
# }
# 
# formatcomma <- function(x, digits=NULL, dollar=FALSE) {
#   if (length(x)==0) { return(NA) }
#   if (is.null(digits)) {
#     xFormat <- format(x, big.mark=",", scientific=FALSE) 
#   } else {
#     xFormat <- format(round(x,digits), big.mark=",", scientific=FALSE) 
#   }
#   if (dollar) { xFormat <- paste0("$", xFormat) }
#   return(xFormat)
# } 
# 
# shapefile  <- counties(cb=TRUE, resolution = "20m")
# shapefile$FIPS <- as.numeric(shapefile$GEOID)
# shapefile$id   <- rownames(shapefile@data)
# shapefile@data$NAME <- stri_encode(shape_counties@data$NAME,"","UTF-8")
# 
# 
# stateFromLower <-function(x, faclevs = 'selected') {
#   #Function to convert state FIPS codes to full state names or vice-versa 
#   #x is a vector of state abbreviations, or full state names.
#   #direction (name to code, or code to name) is determined automatically based on the supplied data
# 
#   st.codes<-data.frame(
#     state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
#                       "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
#                       "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
#                       "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
#                       "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
#     full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
#                      "connecticut","district of columbia","delaware","florida","georgia",
#                      "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
#                      "louisiana","massachusetts","maryland","maine","michigan","minnesota",
#                      "missouri","mississippi","montana","north carolina","north dakota",
#                      "nebraska","new hampshire","new jersey","new mexico","nevada",
#                      "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
#                      "rhode island","south carolina","south dakota","tennessee","texas",
#                      "utah","virginia","vermont","washington","wisconsin",
#                      "west virginia","wyoming"))
#   )
#   
#   if (nchar(x[1]) == 2) { st.x <- data.frame(state = x)
#     refac.x <- st.codes$full[match(tolower(st.x$state), tolower(st.codes$state))] 
#   } else { st.x <- data.frame(full = x)
#     refac.x <- st.codes$state[match(tolower(st.x$full), tolower(st.codes$full))] 
#   }
#   
#   if(faclevs == 'all') {return(refac.x)}
#   else {return(factor(refac.x))}
# } 
# 
# df_state_fips <- read.csv("data/state_fips.csv")
# 
# get_state_fips <- function(x) {
#   df_state_fips$state_fips[match(x, df_state_fips$state_abbrev)]
# }
# 
# # df_state_fips2 <- states %>% select(region, group) %>% unique() %>%
# #   mutate(
# #     state_name = firstcap(region),
# #     state_abbr = stateFromLower(region),
# #     state_fips = get_state_fips(state_abbr)
# #          )
# 
# 
# my_leaflet <- function(data_geo, var, pal, popup, title) {
#     
#   map_FIPS <-  leaflet() %>%
#     addProviderTiles("CartoDB.Positron") %>%
#     addPolygons(
#       data = data_geo,
#       fillColor = ~pal(data_geo[[var]]),
#       color = "#b2aeae", # you need to use hex colors
#       fillOpacity = 0.7,
#       weight = 1,
#       smoothFactor = 0.2,
#       popup = popup
#     ) %>%
#     addLegend(pal = pal,
#               values = data_geo[[var]],
#               position = "bottomright",
#               title = title,
#               na.label = "", 
#               labFormat = labelFormat(prefix = "")) %>%
#     setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
#     fitBounds(-125, 25, -67, 50)
#   map_FIPS
# }
# 
# 
# add_map_var <- function(geo_data, var, varname, breaks, large_limits=FALSE) {
#   geo_data[[varname]] <- cut(geo_data@data[[var]], 
#                              breaks = breaks,
#                              ordered_result = TRUE,
#                              labels = my_ordered_levels(breaks,
#                                                         large_limits=large_limits))
#   geo_data[[varname]][geo_data@data[[var]]==0] <- NA
#   geo_data
# }
# 
# # dairy farming ------------------------------------
# 
# df_dairy_farming$FIPS_num <- df_dairy_farming$FIPS %>% as.numeric()
# 
# geo_dairy_farming <- geo_join(shapefile, 
#                               full_join(df_dairy_farming %>% filter(year == 2015),
#                                         df_state_fips,
#                                         by =  c("ST_FIPS" = "state_fips")),
#                               "FIPS", "FIPS_num") # assuming id to merge are named "FIPS"
# 
# 
# gen_jobs_leaf  <- function(geo_data, var, var1, var2, 
#                            label1, label2, file_name, 
#                            breaks,
#                            palette = "RdYlBu",
#                            title = NULL) {
#   
#   mapvar <- paste0(var, "_gr")  
#   geo_data <- add_map_var(geo_data, 
#                           var = var, 
#                           varname = mapvar, 
#                           breaks = breaks)
#   
#   popup <- 
#     paste0(
#       geo_data$NAME, ", ", geo_data$state_abbrev, 
#       "<br> 2012: ", formatcomma(geo_data@data[["emplvl_lag3"]]), 
#       "<br> 2015: ",  formatcomma(geo_data@data[["avg_emplvl"]]), 
#       "<br> change: ",  formatcomma(geo_data@data[["emplvl_diff3"]]),
#       " (", ifelse(geo_data@data[["emplvl_lag3"]]==0, 
#                    "NA)", paste0(round(geo_data@data[["emplvl_diff3"]]/
#                                          geo_data@data[["emplvl_lag3"]], 2)*100, "%)")),
#       "<br> avg wage: $", formatcomma(geo_data@data[["avg_wage"]]) 
#     )
#   
#   
#   pal <- colorFactor(
#     palette = palette,
#     domain = geo_data[[mapvar]],
#     na.color = 	"#FFFFFF"
#   )
#   
#   leaf_map <- my_leaflet(geo_data,
#                                    mapvar, 
#                                    pal = pal,
#                                    popup = popup, 
#                                    title =title)
#   
#   saveWidget(leaf_map,
#              file= paste0(file_name, ".html"), selfcontained=TRUE)
#   
# }
# 
# breaks_geo_change_dairy_farming <- c(- 500, -300, -200, -100, 0, 100, 200, 300, 500)
# breaks_geo_dairy_farming <- c(0, 100, 200, 300, 500, 1000, 2000, 3000, Inf)
# 
# gen_jobs_leaf(geo_dairy_farming, 
#               var = "emplvl_diff3",
#               file_name = "employment_change_dairy_farming", 
#               breaks= breaks_geo_change_dairy_farming,
#               title = "Change in Employment<br>in Milk Production, <br> 2012 to 2015 (jobs)") 
# 
# gen_jobs_leaf(geo_dairy_farming, 
#               var = "avg_emplvl",
#               file_name = "employment_dairy_farming", 
#               breaks= breaks_geo_dairy_farming,
#               palette = "YlGnBu",
#               title = "Employment<br>in Milk Production, <br> 2015 (jobs)") 
# 
# 
# # dairy food manufacturing ----------------------------------------
# 
# 
# df_dairy_food_mf$FIPS_num <- df_dairy_food_mf$FIPS %>% as.numeric()
# 
# geo_dairy_food_mf <- geo_join(shapefile, 
#                               full_join(df_dairy_food_mf %>% filter(year == 2015),
#                                         df_state_fips,
#                                         by =  c("ST_FIPS" = "state_fips")),
#                               "FIPS", "FIPS_num") # assuming id to merge are named "FIPS"
# 
# breaks_geo_change_dairy_food_mf <- c(-2000, - 500, -300, -200, -100, 0, 100, 200, 300, 500, 2000)
# breaks_geo_dairy_food_mf <- c(0, 100, 200, 300, 500, 1000, 2000, 3000, Inf)
# 
# 
# gen_jobs_leaf(geo_dairy_food_mf, 
#               var = "emplvl_diff3",
#               file_name = "employment_change_dairy_food_mf", 
#               breaks= breaks_geo_change_dairy_food_mf,
#               title = "Change in Employment<br>in Dairy Food Manufacturing, <br> 2012 to 2015 (jobs)") 
# 
# gen_jobs_leaf(geo_dairy_food_mf, 
#               var = "avg_emplvl",
#               file_name = "employment_dairy_food_mf", 
#               breaks= breaks_geo_dairy_food_mf,
#               palette = "YlGnBu",
#               title = "Employment<br>in Dairy Food Manufacturing, <br> 2015 (jobs)") 
# 
# 
# 
# # ag census dairy cow inventory -----------------------------------
# 
# load("data/ag_census_92.RData")
# load("data/ag_census_97.RData")
# load("data/ag_census_02.RData")
# 
# df_ag_census_more <- df_ag_census %>% 
#   inner_join(ag_census_02 %>% 
#                filter(LEVEL==1) %>%  
#                select(FIPS, cows_20_up), by = "FIPS") %>% 
#   inner_join( ag_census_97 %>% 
#                 filter(LEVEL==1) %>%
#                 select(FIPS, cows_20_up), by = "FIPS") %>% 
#   inner_join( ag_census_92 %>% 
#                 filter(LEVEL==1) %>%
#                 select(FIPS, cows_20_up), by = "FIPS")  %>% 
#   mutate(
#     cows_20_up_02 = cows_20_up,
#     cows_20_up_97 = cows_20_up.x.x,
#     cows_20_up_92 = cows_20_up.y.y,
#     change_cows_07_12 = cows_20_up_12 - cows_20_up_07,
#     change_cows_02_07 = cows_20_up_07 - cows_20_up_02,
#     change_cows_97_02 = cows_20_up_02 - cows_20_up_97,
#     change_cows_92_97 = cows_20_up_97 - cows_20_up_92
#   )
# 
# head(df_ag_census_more)
# 
# df_ag_census_more$FIPS_num <- df_ag_census_more$FIPS %>% as.numeric()
# 
# geo_ag_census_more <- geo_join(shapefile, 
#                           full_join(df_ag_census_more, 
#                                     df_state_fips,
#                                     by =  c("STATEFP" = "state_fips")),
#                           "FIPS", "FIPS_num") # assuming id to merge are named "FIPS"
# 
# 
# gen_ag_census_leaf  <- function(geo_ag_census, var, var1, var2, 
#                                 label1, label2, file_name, 
#                                 breaks,
#                                 palette = "RdYlBu",
#                                 title = NULL, 
#                                 group =TRUE) {
#   
#   if (group) {
#     mapvar <- paste0(var,"_gr")
#     geo_ag_census <- add_map_var(geo_ag_census, 
#                                 var = var, 
#                                 varname = mapvar, 
#                                 breaks = breaks,
#                                 large_limits = TRUE)
#   } else {
#     mapvar <- var
#   }
#     
#   popup <- 
#     paste0(
#       geo_ag_census$NAME, ", ", geo_ag_census$state_abbrev, 
#       "<br>", label1,  ": ", formatcomma(geo_ag_census@data[[var1]]), 
#       "<br>", label2,  ": ", formatcomma(geo_ag_census@data[[var2]]), 
#       "<br> change: ",  formatcomma(geo_ag_census@data[[var]]),
#       " (", ifelse(geo_ag_census@data[[var1]]==0, 
#                    "NA)", 
#                    paste0(round((geo_ag_census@data[[var2]] - geo_ag_census@data[[var1]])/
#                                          geo_ag_census@data[[var1]], 2)*100, "%)"))
#     )
#   
#   if (group) {
#     pal <- colorFactor(
#       palette = palette,
#       domain = geo_ag_census[[mapvar]],
#       na.color = 	"#FFFFFF"
#     )
#   } else {
#     pal <- colorBin(
#       palette = palette,
#       domain = geo_ag_census[[mapvar]],
#       na.color = 	"#FFFFFF"
#     )
#   }
#   
#   
#   if (is.null(title)) {
#     title <- paste("Change in Milk Cow Inveontory, <br>",
#                  label1, "to", label2, "(heads)")
#   }
#   
#   leaf_map <- my_leaflet(geo_ag_census, 
#                          mapvar, 
#                          pal = pal,
#                          popup = popup,  
#                          title = title)
#   
#   saveWidget(leaf_map,
#              file= paste0(file_name,".html"), selfcontained=TRUE)
#   
# }
# 
# #  ---- changes ----
# breaks_geo_change_cows <- c(-Inf,-5, -3, -2, -1, 0, 1, 2, 3, 5, Inf)*1000
# 
# 
# gen_ag_census_leaf(geo_ag_census_more, 
#                    var="change_cows_07_12", 
#                    var1="cows_20_up_07", var2="cows_20_up_12", 
#                    label1=2007, label2=2012, 
#                    breaks = breaks_geo_change_cows,
#                    file_name="change_cow_inventory_2007_2012")
# 
# gen_ag_census_leaf(geo_ag_census_more, 
#                    var="change_cows_02_07", 
#                    var1="cows_20_up_02", var2="cows_20_up_07", 
#                    label1=2002, label2=2007, 
#                    breaks = breaks_geo_change_cows,
#                    file_name="change_cow_inventory_2002_2007") 
# 
# 
# gen_ag_census_leaf(geo_ag_census_more, 
#                    var="change_cows_97_02", 
#                    var1="cows_20_up_97", var2="cows_20_up_02", 
#                    label1=1997, label2=2002, 
#                    breaks = breaks_geo_change_cows,
#                    file_name="change_cow_inventory_1997_2002") 
# 
# gen_ag_census_leaf(geo_ag_census_more, 
#                    var="change_cows_92_97", 
#                    var1="cows_20_up_92", var2="cows_20_up_97", 
#                    label1=1992, label2=1997, 
#                    breaks = breaks_geo_change_cows,
#                    file_name="change_cow_inventory_1992_1997")
# 
# #  ---- levels ----
# 
# breaks_geo_cows <- c(0, 0.5, 1, 2, 3, 5, 10, 20, Inf)*10
# 
# geo_ag_census_more@data <- 
#   geo_ag_census_more@data %>% 
#   mutate(
#     cows_12_1000 = cows_20_up_12/1000,
#     cows_07_1000 = cows_20_up_07/1000,
#     cows_02_1000 = cows_20_up_02/1000,
#     cows_97_1000 = cows_20_up_97/1000,
#     cows_92_1000 = cows_20_up_92/1000
#   )
# 
# gen_ag_census_leaf(geo_ag_census_more, 
#                    var="cows_12_1000", 
#                    var1="cows_20_up_07", var2="cows_20_up_12", 
#                    label1=2007, label2=2012, 
#                    breaks = breaks_geo_cows,
#                    file_name="change_cow_inventory_2012",
#                    palette = "YlGnBu",
#                    title = "Cow Inventory, 2012 <br> (1,000 heads)")
# 
# gen_ag_census_leaf(geo_ag_census_more, 
#                    var="cows_07_1000", 
#                    var1="cows_20_up_02", var2="cows_20_up_07", 
#                    label1=2002, label2=2007, 
#                    breaks = breaks_geo_cows,
#                    file_name="change_cow_inventory_2007",
#                    palette = "YlGnBu",
#                    title = "Cow Inventory, 2007 <br> (1,000 heads)")
# 
# gen_ag_census_leaf(geo_ag_census_more, 
#                    var="cows_02_1000", 
#                    var1="cows_20_up_97", var2="cows_20_up_02", 
#                    label1=1997, label2=2002, 
#                    breaks = breaks_geo_cows,
#                    file_name="change_cow_inventory_2002",
#                    palette = "YlGnBu",
#                    title = "Cow Inventory, 2002 <br> (1,000 heads)")
# 
# gen_ag_census_leaf(geo_ag_census_more, 
#                    var="cows_97_1000", 
#                    var1="cows_20_up_92", var2="cows_20_up_97", 
#                    label1=1992, label2=1997, 
#                    breaks = breaks_geo_cows,
#                    file_name="change_cow_inventory_1997",
#                    palette = "YlGnBu",
#                    title = "Cow Inventory, 1997 <br> (1,000 heads)")
# 
# gen_ag_census_leaf(geo_ag_census_more, 
#                    var="cows_92_1000", 
#                    var1="cows_20_up_92", var2="cows_20_up_97", 
#                    label1=1992, label2=1997, 
#                    breaks = breaks_geo_cows,
#                    file_name="change_cow_inventory_1992",
#                    palette = "YlGnBu",
#                    title = "Cow Inventory, 1992 <br> (1,000 heads)")
# 
