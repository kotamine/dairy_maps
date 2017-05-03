library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(scales)

setwd("/Users/kota/Dropbox/R_projects/dairy_maps")


# ---- county shape map ----------
# library(tigris)
# library(stringi)
# shape_counties  <- counties(cb=TRUE, resolution = "20m")
# shape_counties$FIPS <- shape_counties$GEOID
# shape_counties$id   <- rownames(shape_counties@data)
# shape_counties@data$NAME <- stri_encode(shape_counties@data$NAME,"","UTF-8")
#
# save(shape_counties,  file = "data/shape_counties.RData")
#

# --------------------
# geographic shape data 
# ---------------------

cnty <- map_data("county") # using ggplot2
states <- map_data("state") # using ggplot2

load("data/shape_counties.RData")
df_shape_counties <- tidy(shape_counties) # convert to data frame
df_shape_counties <- df_shape_counties %>%
  inner_join(shape_counties@data, by="id") 
df_shape_counties$region <- df_shape_counties$FIPS


# ---------------------
# functions for mapping 
# ---------------------

range_to <- function(x) {
  gsub("\\(", "", gsub(",", " to ", gsub("]", "", x)))
}

my_ordered_levels <- function(breaks, large_limits=FALSE, 
                              large_limit_left=FALSE, large_limit_right=FALSE, comma=TRUE) {
  if (comma) breaks <- formatcomma(breaks) 
  rlt <- paste(lag(breaks), "to", breaks)[-1] 
  if (large_limits | large_limit_left) rlt[1] <- paste("<", breaks[2])
  if (large_limits | large_limit_right) rlt[length(rlt)] <- paste(">", breaks[(length(breaks)-1)])
  rlt
}

firstcap <- function(x) {
  lapply(1:length(x), function(i) {
    xi <- x[i]
    if(grepl(" ", xi)) {
      tmp <- gregexpr(" ", xi)[[1]][1] + 1
      substr(xi,  tmp,  tmp) <- toupper(substr(xi, tmp, tmp))
    }
    substr(xi, 1, 1) <- toupper(substr(xi, 1, 1))
    xi
  }) %>% unlist()
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

shapefile  <- counties(cb=TRUE, resolution = "20m")
shapefile$FIPS <- as.numeric(shapefile$GEOID)
shapefile$id   <- rownames(shapefile@data)
shapefile@data$NAME <- stri_encode(shape_counties@data$NAME,"","UTF-8")


stateFromLower <-function(x, faclevs = 'selected') {
  #Function to convert state FIPS codes to full state names or vice-versa 
  #x is a vector of state abbreviations, or full state names.
  #direction (name to code, or code to name) is determined automatically based on the supplied data
  
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  
  if (nchar(x[1]) == 2) { st.x <- data.frame(state = x)
  refac.x <- st.codes$full[match(tolower(st.x$state), tolower(st.codes$state))] 
  } else { st.x <- data.frame(full = x)
  refac.x <- st.codes$state[match(tolower(st.x$full), tolower(st.codes$full))] 
  }
  
  if(faclevs == 'all') {return(refac.x)}
  else {return(factor(refac.x))}
} 

df_state_fips <- read.csv("data/state_fips.csv")

get_state_fips <- function(x) {
  df_state_fips$state_fips[match(x, df_state_fips$state_abbrev)]
}

# df_state_fips2 <- states %>% select(region, group) %>% unique() %>%
#   mutate(
#     state_name = firstcap(region),
#     state_abbr = stateFromLower(region),
#     state_fips = get_state_fips(state_abbr)
#          )

# --------------------------------------------------
# leaflet
# --------------------------------------------------

library(tigris)
# library(acs)
library(stringr)
library(stringi)
library(leaflet)
library(htmlwidgets)


my_leaflet <- function(data_geo, var, pal, popup, title,
                       prefix="", suffix="") {
  
  map_FIPS <-  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
      data = data_geo,
      fillColor = ~pal(data_geo[[var]]),
      color = "#b2aeae", # you need to use hex colors
      fillOpacity = 0.7,
      weight = 1,
      smoothFactor = 0.2,
      popup = popup
    ) %>%
    addLegend(pal = pal,
              values = data_geo[[var]],
              position = "bottomright",
              title = title,
              na.label = "",
              labFormat = labelFormat(prefix = prefix, suffix=suffix)) %>%
    setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
    fitBounds(-125, 25, -67, 50)
  map_FIPS
}

# ---------------------
# functions for mapping BLS data 
# ---------------------

load("data/labor_stats.RData")

prep_bls_data <- function(df, yr = 2015) {
  
  df <- df %>% 
    mutate( 
      FIPS = area_fips,
      FIPS_num = FIPS %>% as.numeric(), 
      ST_FIPS = state %>% as.numeric()
    ) 

  df <- df %>% 
    arrange(FIPS, year) %>%
    group_by(FIPS) %>%
    mutate(
      emplvl_lag3 = dplyr::lag(avg_emplvl, 3),
      emplvl_diff3 = avg_emplvl - emplvl_lag3
    ) %>% ungroup()
  
  print("summary of emplvl_diff3 (employment difference)")
  print(summary(df$emplvl_diff3))
  
  print("summary of avg_emplvl (employment 2015)")
  print(summary(df$avg_emplvl))
  
  print("summary of emplvl_lag3 (employment 2012)")
  print(summary(df$emplvl_lag3))
  
  df_sml <- df %>% dplyr::filter(year == yr) %>%
    full_join(df_state_fips, # from global env 
                      by =  c("ST_FIPS" = "state_fips"))
            
  geo_df <- geo_join(shapefile, df_sml,
                     "FIPS", "FIPS_num") # from global env
  
  geo_df
}


add_map_var <- function(geo_data, var, varname, breaks, 
                        large_limits=FALSE, large_limit_left=FALSE, large_limit_right=FALSE) {
  geo_data[[varname]] <- cut(geo_data@data[[var]], 
                             breaks = breaks,
                             ordered_result = TRUE,
                             labels = my_ordered_levels(breaks,
                                                        large_limits=large_limits,
                                                        large_limit_left=large_limit_left,
                                                        large_limit_right=large_limit_right))
  geo_data[[varname]][geo_data@data[[var]]==0] <- NA
  geo_data
}


gen_leaf_map  <- function(geo_data, var, file_name, 
                           breaks, large_limits = FALSE, 
                           large_limit_left=FALSE, large_limit_right=FALSE,
                           palette = "RdYlBu",
                           title = NULL, popup = NULL, ...) {
  
  if ( breaks[1] == -Inf) large_limit_left <- TRUE
  if ( breaks[length(breaks)] == Inf) large_limit_right <- TRUE
  
  mapvar <- paste0(var, "_gr")  
  geo_data <- add_map_var(geo_data, 
                          var = var, 
                          varname = mapvar, 
                          breaks = breaks,
                          large_limits = large_limits,
                          large_limit_left = large_limit_left,
                          large_limit_right = large_limit_right)
  
  if (is.null(popup)) {
  popup <- 
    paste0(
      geo_data$NAME, ", ", geo_data$state_abbrev, 
      "<br> 2012: ", formatcomma(geo_data@data[["emplvl_lag3"]]), 
      "<br> 2015: ",  formatcomma(geo_data@data[["avg_emplvl"]]), 
      "<br> change: ",  formatcomma(geo_data@data[["emplvl_diff3"]]),
      " (", ifelse(geo_data@data[["emplvl_lag3"]]==0, 
                   "NA)", paste0(round(geo_data@data[["emplvl_diff3"]]/
                                         geo_data@data[["emplvl_lag3"]], 2)*100, "%)")),
      "<br> avg wage: $", formatcomma(geo_data@data[["avg_wage"]]) 
    )
  }
  
  pal <- colorFactor(
    palette = palette,
    domain = geo_data[[mapvar]],
    na.color = 	"#FFFFFF"
  )
  
  leaf_map <- my_leaflet(geo_data,
                         mapvar, 
                         pal = pal,
                         popup = popup, 
                         title =title, ...)
  
  saveWidget(leaf_map,
             file= paste0(file_name, ".html"), selfcontained=TRUE)
  
}



# dairy farming ------------------------------------

geo_dairy_farming <- prep_bls_data(df_dairy_farming)

breaks_geo_change_dairy_farming <- c(- 500, -300, -200, -100, 0, 100, 200, 300, 500)
breaks_geo_dairy_farming <- c(0, 100, 200, 300, 500, 1000, 2000, 3000, Inf)

gen_leaf_map(geo_dairy_farming, 
              var = "emplvl_diff3",
              file_name = "employment_change_dairy_farming", 
              breaks= breaks_geo_change_dairy_farming,
              title = "Change in Employment<br>in Milk Production, <br> 2012 to 2015 (jobs)") 

gen_leaf_map(geo_dairy_farming, 
              var = "avg_emplvl",
              file_name = "employment_dairy_farming", 
              breaks= breaks_geo_dairy_farming,
              palette = "YlGnBu",
              title = "Employment<br>in Milk Production, <br> 2015 (jobs)") 


# dairy food manufacturing ----------------------------------------

geo_dairy_food_mf <- prep_bls_data(df_dairy_food_mf)

breaks_geo_change_dairy_food_mf <- c(-2000, - 500, -300, -200, -100, 0, 100, 200, 300, 500, 2000)
breaks_geo_dairy_food_mf <- c(0, 100, 200, 300, 500, 1000, 2000, 3000, Inf)


gen_leaf_map(geo_dairy_food_mf, 
              var = "emplvl_diff3",
              file_name = "employment_change_dairy_food_mf", 
              breaks= breaks_geo_change_dairy_food_mf,
              title = "Change in Employment<br>in Dairy Food Manufacturing, <br> 2012 to 2015 (jobs)") 

gen_leaf_map(geo_dairy_food_mf, 
              var = "avg_emplvl",
              file_name = "employment_dairy_food_mf", 
              breaks= breaks_geo_dairy_food_mf,
              palette = "YlGnBu",
              title = "Employment<br>in Dairy Food Manufacturing, <br> 2015 (jobs)") 

# animal processing ----------------------------------------

geo_animal_processing <- prep_bls_data(df_animal_processing)

breaks_geo_change_animal_processing <- c(-Inf, - 500, -300, -200, -100, 0, 100, 200, 300, 500, Inf)
breaks_geo_animal_processing <- c(0, 100, 200, 300, 500, 1000, 2000, 3000, Inf)


gen_leaf_map(geo_animal_processing, 
              var = "emplvl_diff3",
              file_name = "employment_change_animal_processing", 
              breaks= breaks_geo_change_animal_processing,
              title = "Change in Employment<br>in Animal Processing, <br> 2012 to 2015 (jobs)") 

gen_leaf_map(geo_animal_processing, 
              var = "avg_emplvl",
              file_name = "employment_animal_processing", 
              breaks= breaks_geo_animal_processing,
              palette = "YlGnBu",
              title = "Employment<br>in Animal Processing, <br> 2015 (jobs)") 


# -----------------------------------------------------------------
# Ag Census data
# -----------------------------------------------------------------

load("data/ag_census_92.RData")
load("data/ag_census_97.RData")
load("data/ag_census_02.RData")
load("data/ag_census_07.RData")
load("data/ag_census_12.RData")


df_ag_census <-  ag_census_12 %>% 
  filter(LEVEL==1) %>% select(FIPS, NAME, STATEFIP, cows_20_up, harvest_corn, harvest_soybean) %>% 
  inner_join(ag_census_07 %>% 
               filter(LEVEL==1) %>%  
               select(FIPS, cows_20_up, harvest_corn, harvest_soybean), by = "FIPS") %>% 
  inner_join(ag_census_02 %>% 
               filter(LEVEL==1) %>%  
               select(FIPS, cows_20_up, harvest_corn, harvest_soybean), by = "FIPS") %>% 
  inner_join( ag_census_97 %>% 
                filter(LEVEL==1) %>%
                select(FIPS, cows_20_up, harvest_corn, harvest_soybean), by = "FIPS") %>% 
  inner_join( ag_census_92 %>% 
                filter(LEVEL==1) %>%
                select(FIPS, cows_20_up, harvest_corn, harvest_soybean), by = "FIPS") %>% 
  mutate(
    cows_20_up_12 = cows_20_up.x,
    cows_20_up_07 = cows_20_up.y,
    cows_20_up_02 = cows_20_up.x.x,
    cows_20_up_97 = cows_20_up.y.y,
    cows_20_up_92 = cows_20_up,
    change_cows_07_12 = cows_20_up_12 - cows_20_up_07,
    change_cows_02_07 = cows_20_up_07 - cows_20_up_02,
    change_cows_97_02 = cows_20_up_02 - cows_20_up_97,
    change_cows_92_97 = cows_20_up_97 - cows_20_up_92,
    harvest_corn_12 = harvest_corn.x/1000,
    harvest_corn_07 = harvest_corn.y/1000,
    harvest_corn_02 = harvest_corn.x.x/1000,
    harvest_corn_97 = harvest_corn.y.y/1000,
    harvest_corn_92 = harvest_corn/1000,
    change_corn_07_12 = harvest_corn_12 - harvest_corn_07,
    change_corn_02_07 = harvest_corn_07 - harvest_corn_02,
    change_corn_97_02 = harvest_corn_02 - harvest_corn_97,
    change_corn_92_97 = harvest_corn_97 - harvest_corn_92,
    harvest_soybean_12 = harvest_soybean.x/1000,
    harvest_soybean_07 = harvest_soybean.y/1000,
    harvest_soybean_02 = harvest_soybean.x.x/1000,
    harvest_soybean_97 = harvest_soybean.y.y/1000,
    harvest_soybean_92 = harvest_soybean/1000,
    change_soybean_07_12 = harvest_soybean_12 - harvest_soybean_07,
    change_soybean_02_07 = harvest_soybean_07 - harvest_soybean_02,
    change_soybean_97_02 = harvest_soybean_02 - harvest_soybean_97,
    change_soybean_92_97 = harvest_soybean_97 - harvest_soybean_92
  )

head(df_ag_census)

df_ag_census$FIPS_num <- df_ag_census$FIPS %>% as.numeric()

geo_ag_census <- geo_join(shapefile, 
                               full_join(df_ag_census, 
                                         df_state_fips,
                                         by =  c("STATEFIP" = "state_fips")),
                               "FIPS", "FIPS_num") # assuming id to merge are named "FIPS"


gen_ag_census_leaf  <- function(geo_data, var, var1, var2, 
                                label1, label2, ...) {
  
  popup <- 
    paste0(
      geo_data$NAME, ", ", geo_data$state_abbrev, 
      "<br>", label1,  ": ", formatcomma(geo_data@data[[var1]], digits=0), 
      "<br>", label2,  ": ", formatcomma(geo_data@data[[var2]], digits=0), 
      "<br> change: ",  formatcomma(geo_data@data[[var]], digits=0), 
      " (", ifelse(geo_data@data[[var1]]==0, 
                   "NA)", 
                   paste0(round((geo_data@data[[var2]] - geo_data@data[[var1]])/
                                  geo_data@data[[var1]], 2)*100, "%)"))
    )
  
    gen_leaf_map(geo_data = geo_data, var=var, popup = popup, ...)
}

# dairy cow inventory -----------------------------------

#  ---- changes ----
breaks_geo_change_cows <- c(-Inf,-5, -3, -2, -1, 0, 1, 2, 3, 5, Inf)*1000


list_specs <- data.frame(
  yr1 = c(2007, 2002, 1997, 1992),
  yr2 = c(2012, 2007, 2002, 1997)
) %>% 
  mutate(
    num1 = substr(yr1, 3,4), 
    num2 = substr(yr2, 3,4),
    var = paste0("change_cows_", num1, "_", num2),
    var1 = paste0("cows_20_up_", num1),
    var2 = paste0("cows_20_up_", num2)
  )


list_specs %>%
  with(
    for (i in 1:length(list_specs[[1]])) {
      
      gen_ag_census_leaf(geo_ag_census, 
                         var=var[[i]], 
                         var1=var1[[i]], var2=var2[[i]], 
                         label1=yr1[[i]], label2=yr2[[i]], 
                         breaks = breaks_geo_change_cows,
                         title = paste("Change in Milk Cow Inventory, <br>", yr1[[i]],
                                       "to", yr2[[i]], "(heads)"), 
                         file_name=paste0("change_cow_inventory_", yr1[[i]], "_", yr2[[i]]))
    }
  )

# 
# gen_ag_census_leaf(geo_ag_census, 
#                    var="change_cows_07_12", 
#                    var1="cows_20_up_07", var2="cows_20_up_12", 
#                    label1=2007, label2=2012, 
#                    breaks = breaks_geo_change_cows,
#                    file_name="change_cow_inventory_2007_2012")
# 


#  ---- levels ----

breaks_geo_cows <- c(0, 0.5, 1, 2, 3, 5, 10, 20, Inf)*10

geo_ag_census@data <- 
  geo_ag_census@data %>% 
  mutate(
    cows_12_1000 = cows_20_up_12/1000,
    cows_07_1000 = cows_20_up_07/1000,
    cows_02_1000 = cows_20_up_02/1000,
    cows_97_1000 = cows_20_up_97/1000,
    cows_92_1000 = cows_20_up_92/1000
  )


list_specs <- data.frame(
  yr1 = c(2007, 2002, 1997, 1992),
  yr2 = c(2012, 2007, 2002, 1997)
) %>% 
  mutate(
    num1 = substr(yr1, 3,4), 
    num2 = substr(yr2, 3,4),
    var  = paste0("cows_", num2, "_1000"),
    var1 = paste0("cows_", num1, "_1000"),
    var2 = paste0("cows_", num2, "_1000")
  )


list_specs %>%
  with(
    for (i in 1:length(list_specs[[1]])) {
      
      gen_ag_census_leaf(geo_ag_census, 
                         var=var[[i]], 
                         var1=var1[[i]], var2=var2[[i]], 
                         label1=yr1[[i]], label2=yr2[[i]], 
                         breaks =breaks_geo_cows,
                         palette = "YlGnBu",
                         title = paste("Cow Inventory,", yr2[[i]], "<br> (1,000 heads)"), 
                         file_name=paste0("cow_inventory_",yr2[[i]]))
      
    }
  )

# the last/oldest census year uses the same popup as one-census before 
gen_ag_census_leaf(geo_ag_census, 
                   var="cows_92_1000", 
                   var1="cows_20_up_92", var2="cows_20_up_97", 
                   label1=1992, label2=1997, 
                   breaks = breaks_geo_cows,
                   file_name="cow_inventory_1992",
                   palette = "YlGnBu",
                   title = "Cow Inventory, 1992 <br> (1,000 heads)")


# --- corn ----------

#  ---- changes ----
breaks_geo_change_corn <- c(-Inf,-5, -3, -2, -1, 0, 1, 2, 3, 5, Inf)*1000

list_specs <- data.frame(
                  yr1 = c(2007, 2002, 1997, 1992),
                  yr2 = c(2012, 2007, 2002, 1997)
                ) %>% 
  mutate(
    num1 = substr(yr1, 3,4), 
    num2 = substr(yr2, 3,4),
    var = paste0("change_corn_", num1, "_", num2),
    var1 = paste0("harvest_corn_", num1),
    var2 = paste0("harvest_corn_", num2)
  )
  

list_specs %>%
  with(
    for (i in 1:length(list_specs[[1]])) {
      
      gen_ag_census_leaf(geo_ag_census, 
                         var=var[[i]], 
                         var1=var1[[i]], var2=var2[[i]], 
                         label1=yr1[[i]], label2=yr2[[i]], 
                         breaks = breaks_geo_change_corn,
                         title = paste("Change in Corn Harvest,", yr1[[i]], "to", yr2[[i]],
                                       "<br> (1,000 bushels)"), 
                         file_name=paste0("change_harvest_corn_", yr1[[i]], "_", yr2[[i]]))
    }
  )

#  ---- levels ----

breaks_geo_corn <- c(0, 1, 2, 3, 5, 10, 20, 40, 60, Inf)*1000

list_specs <- data.frame(
  yr1 = c(2007, 2002, 1997, 1992),
  yr2 = c(2012, 2007, 2002, 1997)
) %>% 
  mutate(
    num1 = substr(yr1, 3,4), 
    num2 = substr(yr2, 3,4),
    var  = paste0("harvest_corn_", num2),
    var1 = paste0("harvest_corn_", num1),
    var2 = paste0("harvest_corn_", num2)
  )


list_specs %>%
  with(
    for (i in 1:length(list_specs[[1]])) {
      
      gen_ag_census_leaf(geo_ag_census, 
                         var=var[[i]], 
                         var1=var1[[i]], var2=var2[[i]], 
                         label1=yr1[[i]], label2=yr2[[i]], 
                         breaks = breaks_geo_corn,
                         palette = "YlGnBu",
                         title = paste("Corn Harvest,", yr2[[i]], "<br> (1,000 bushels)"), 
                         file_name=paste0("harvest_corn_",yr2[[i]]))
      
    }
  )

# the last/oldest census year uses the same popup as one-census before 
gen_ag_census_leaf(geo_ag_census, 
                   var="harvest_corn_92", 
                   var1="harvest_corn_92", var2="harvest_corn_97", 
                   label1=1992, label2=1997, 
                   breaks = breaks_geo_corn,
                   palette = "YlGnBu",
                   title = "Corn Harvest, 1992 <br> (1,000 bushels)", 
                   file_name="harvest_corn_1992")

# --- soybean ----------

#  ---- changes ----
breaks_geo_change_soybean <- c(-Inf,-5, -3, -2, -1, 0, 1, 2, 3, 5, Inf)*1000

list_specs <- data.frame(
  yr1 = c(2007, 2002, 1997, 1992),
  yr2 = c(2012, 2007, 2002, 1997)
) %>% 
  mutate(
    num1 = substr(yr1, 3,4), 
    num2 = substr(yr2, 3,4),
    var = paste0("change_soybean_", num1, "_", num2),
    var1 = paste0("harvest_soybean_", num1),
    var2 = paste0("harvest_soybean_", num2)
  )


list_specs %>%
  with(
    for (i in 1:length(list_specs[[1]])) {
      
      gen_ag_census_leaf(geo_ag_census, 
                         var=var[[i]], 
                         var1=var1[[i]], var2=var2[[i]], 
                         label1=yr1[[i]], label2=yr2[[i]], 
                         breaks = breaks_geo_change_soybean,
                         title = paste("Change in soybean Harvest,", yr1[[i]], "to", yr2[[i]],
                                       "<br> (1,000 bushels)"), 
                         file_name=paste0("change_harvest_soybean_", yr1[[i]], "_", yr2[[i]]))
    }
  )

#  ---- levels ----

breaks_geo_soybean <- c(0, 1, 2, 3, 5, 10, 20, 40, 60, Inf)*1000

list_specs <- data.frame(
  yr1 = c(2007, 2002, 1997, 1992),
  yr2 = c(2012, 2007, 2002, 1997)
) %>% 
  mutate(
    num1 = substr(yr1, 3,4), 
    num2 = substr(yr2, 3,4),
    var  = paste0("harvest_soybean_", num2),
    var1 = paste0("harvest_soybean_", num1),
    var2 = paste0("harvest_soybean_", num2)
  )


list_specs %>%
  with(
    for (i in 1:length(list_specs[[1]])) {
      
      gen_ag_census_leaf(geo_ag_census, 
                         var=var[[i]], 
                         var1=var1[[i]], var2=var2[[i]], 
                         label1=yr1[[i]], label2=yr2[[i]], 
                         breaks = breaks_geo_soybean,
                         palette = "YlGnBu",
                         title = paste("soybean Harvest,", yr2[[i]], "<br> (1,000 bushels)"), 
                         file_name=paste0("harvest_soybean_",yr2[[i]]))
      
    }
  )

# the last/oldest census year uses the same popup as one-census before 
gen_ag_census_leaf(geo_ag_census, 
                   var="harvest_soybean_92", 
                   var1="harvest_soybean_92", var2="harvest_soybean_97", 
                   label1=1992, label2=1997, 
                   breaks = breaks_geo_soybean,
                   palette = "YlGnBu",
                   title = "soybean Harvest, 1992 <br> (1,000 bushels)", 
                   file_name="harvest_soybean_1992")




# --- Hispanic population ----------

load("data/hispanic.RData")


hisp_leaf <- function(data_FIPS, shapefile, var, suffix="%", no_growth =FALSE, ...) {
  
  geo_data <- geo_join(shapefile, data_FIPS, "FIPS", "FIPS")
      # assuming id to merge are named "FIPS"
  
  if (no_growth) {
    popup <- 
      paste0(
        geo_data$NAME, ", ", geo_data$state, 
        "<br> Total pop.: ", formatcomma(geo_data@data[["pop_all"]]), 
        "<br> Hispanic pop.: ", formatcomma(geo_data@data[["pop_hisp"]]), 
        "<br> Hispanic prop.: ",  round(geo_data@data[["rate_hisp"]], 2) ,"%"
      )
     
  } else {
    popup <- 
      paste0(
        geo_data$NAME, ", ", geo_data$state, 
        "<br> Total pop.: ", formatcomma(geo_data@data[["pop_all"]]), 
        "<br> Hispanic pop.: ", formatcomma(geo_data@data[["pop_hisp"]]), 
        "<br> Hispanic prop.: ",  round(geo_data@data[["rate_hisp"]], 2) ,"%",
        "<br> Total pop. growth: ",    round(geo_data@data[["growth_all"]], 2), "%/year",
        "<br> Hispanic pop. growth: ", round(geo_data@data[["growth_hisp"]], 2), "%/year",
        "<br> Hispanic pop. 5 years ago: ", formatcomma(geo_data@data[["pop_hisp_l5"]]),
        "<br> Change in Hispanic pop: ", formatcomma(geo_data@data[["hisp_change"]])
      )
  }
  
    gen_leaf_map(geo_data = geo_data, var=var, popup = popup, ...)
}


hispanic_90_15 <- hispanic_90_15 %>%
  group_by(FIPS) %>% 
  mutate(
    pop_all_l5 = dplyr::lag(pop_all, 5),    
    pop_hisp_l5 = dplyr::lag(pop_hisp, 5),
    hisp_change = pop_hisp - pop_hisp_l5,
    growth_all = (pop_all - pop_all_l5)/pop_all_l5*100/5,
    growth_hisp = (pop_hisp - pop_hisp_l5)/pop_hisp_l5*100/5
  ) %>% ungroup()

hispanic_90_15 %>% filter(FIPS==27053) %>% "["(1:20,)

my_breaks_hispanic <- c(0, 1, 3, 5, 10, 20, 30, 40, 100)

for (y in c(1990, 1995, 2000, 2010, 2015)) {
  if (y==1990) no_growth <- TRUE else no_growth <- FALSE 
  hispanic_90_15 %>% filter(year==y) %>%
    mutate(rate_hisp = rate_hisp * 100) %>%
    hisp_leaf(shapefile, 
              var = "rate_hisp", 
              breaks = my_breaks_hispanic,
              title= paste("Relative Hispanic population <br> by county,", y,"(%):"),
              palette = "YlGnBu",
              no_growth = no_growth,
              file_name = paste0("hispanic_pop_",y))
}


my_breaks_change_pct_hispanic <- c(-Inf, -5, -3, -1, 0, 1, 3, 5, Inf)

for (y in c(1995, 2000, 2005, 2010, 2015)) {
  if (y==1990) no_growth <- TRUE else no_growth <- FALSE 
  hispanic_90_15 %>% filter(year==y) %>%
    mutate(rate_hisp = rate_hisp * 100) %>%
    hisp_leaf(shapefile, 
              var = "growth_hisp", 
              breaks = my_breaks_change_pct_hispanic,
              title= paste("Change in Hispanic population <br> by county,", y,"(%):"),
              palette = "RdYlBu",
              no_growth = no_growth,
              file_name = paste0("change_pct_hispanic_pop_",y))
}


my_breaks_change_hispanic <- c(-Inf, -5000, -3000, -1000, 0, 1000, 3000, 5000, Inf)

for (y in c(1995, 2000, 2005, 2010, 2015)) {
  if (y==1990) no_growth <- TRUE else no_growth <- FALSE 
  hispanic_90_15 %>% filter(year==y) %>%
    mutate(rate_hisp = rate_hisp * 100) %>%
    hisp_leaf(shapefile, 
              var = "hisp_change", 
              breaks = my_breaks_change_hispanic,
              title= paste("Change in Hispanic population <br> by county,", y,"to", (y-5),"(persons):"),
              palette = "RdYlBu",
              no_growth = no_growth,
              file_name = paste0("change_hispanic_pop_",y))
}
