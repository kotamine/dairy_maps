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




my_leaflet <- function(data_geo, var, pal, popup, title) {
  
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
              labFormat = labelFormat(prefix = "")) %>%
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


gen_jobs_leaf  <- function(geo_data, var, var1, var2, 
                           label1, label2, file_name, 
                           breaks, large_limits = FALSE, 
                           large_limit_left=FALSE, large_limit_right=FALSE,
                           palette = "RdYlBu",
                           title = NULL) {
  
  if ( breaks[1] == Inf) large_limit_left <- TRUE
  if ( breaks[length(breaks)] == Inf) large_limit_right <- TRUE
  
  mapvar <- paste0(var, "_gr")  
  geo_data <- add_map_var(geo_data, 
                          var = var, 
                          varname = mapvar, 
                          breaks = breaks,
                          large_limits = large_limits,
                          large_limit_left = large_limit_left,
                          large_limit_right = large_limit_right)
  
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
  
  
  pal <- colorFactor(
    palette = palette,
    domain = geo_data[[mapvar]],
    na.color = 	"#FFFFFF"
  )
  
  leaf_map <- my_leaflet(geo_data,
                         mapvar, 
                         pal = pal,
                         popup = popup, 
                         title =title)
  
  saveWidget(leaf_map,
             file= paste0(file_name, ".html"), selfcontained=TRUE)
  
}



# dairy farming ------------------------------------

geo_dairy_farming <- prep_bls_data(df_dairy_farming)

breaks_geo_change_dairy_farming <- c(- 500, -300, -200, -100, 0, 100, 200, 300, 500)
breaks_geo_dairy_farming <- c(0, 100, 200, 300, 500, 1000, 2000, 3000, Inf)

gen_jobs_leaf(geo_dairy_farming, 
              var = "emplvl_diff3",
              file_name = "employment_change_dairy_farming", 
              breaks= breaks_geo_change_dairy_farming,
              title = "Change in Employment<br>in Milk Production, <br> 2012 to 2015 (jobs)") 

gen_jobs_leaf(geo_dairy_farming, 
              var = "avg_emplvl",
              file_name = "employment_dairy_farming", 
              breaks= breaks_geo_dairy_farming,
              palette = "YlGnBu",
              title = "Employment<br>in Milk Production, <br> 2015 (jobs)") 


# dairy food manufacturing ----------------------------------------

geo_dairy_food_mf <- prep_bls_data(df_dairy_food_mf)

breaks_geo_change_dairy_food_mf <- c(-2000, - 500, -300, -200, -100, 0, 100, 200, 300, 500, 2000)
breaks_geo_dairy_food_mf <- c(0, 100, 200, 300, 500, 1000, 2000, 3000, Inf)


gen_jobs_leaf(geo_dairy_food_mf, 
              var = "emplvl_diff3",
              file_name = "employment_change_dairy_food_mf", 
              breaks= breaks_geo_change_dairy_food_mf,
              title = "Change in Employment<br>in Dairy Food Manufacturing, <br> 2012 to 2015 (jobs)") 

gen_jobs_leaf(geo_dairy_food_mf, 
              var = "avg_emplvl",
              file_name = "employment_dairy_food_mf", 
              breaks= breaks_geo_dairy_food_mf,
              palette = "YlGnBu",
              title = "Employment<br>in Dairy Food Manufacturing, <br> 2015 (jobs)") 

# animal processing ----------------------------------------

geo_animal_processing <- prep_bls_data(df_animal_processing)

breaks_geo_change_animal_processing <- c(-Inf, - 500, -300, -200, -100, 0, 100, 200, 300, 500, Inf)
breaks_geo_animal_processing <- c(0, 100, 200, 300, 500, 1000, 2000, 3000, Inf)


gen_jobs_leaf(geo_animal_processing, 
              var = "emplvl_diff3",
              file_name = "employment_change_animal_processing", 
              breaks= breaks_geo_change_animal_processing,
              title = "Change in Employment<br>in Animal Processing, <br> 2012 to 2015 (jobs)") 

gen_jobs_leaf(geo_animal_processing, 
              var = "avg_emplvl",
              file_name = "employment_animal_processing", 
              breaks= breaks_geo_animal_processing,
              palette = "YlGnBu",
              title = "Employment<br>in Animal Processing, <br> 2015 (jobs)") 


# -----------------------------------------------------------------
# Ag Census data
# -----------------------------------------------------------------



# dairy cow inventory -----------------------------------

load("data/ag_census_92.RData")
load("data/ag_census_97.RData")
load("data/ag_census_02.RData")
load("data/ag_census_07.RData")
load("data/ag_census_12.RData")


df_ag_census_more <- df_ag_census %>% 
  inner_join(ag_census_02 %>% 
               filter(LEVEL==1) %>%  
               select(FIPS, cows_20_up), by = "FIPS") %>% 
  inner_join( ag_census_97 %>% 
                filter(LEVEL==1) %>%
                select(FIPS, cows_20_up), by = "FIPS") %>% 
  inner_join( ag_census_92 %>% 
                filter(LEVEL==1) %>%
                select(FIPS, cows_20_up), by = "FIPS")  %>% 
  mutate(
    cows_20_up_02 = cows_20_up,
    cows_20_up_97 = cows_20_up.x.x,
    cows_20_up_92 = cows_20_up.y.y,
    change_cows_07_12 = cows_20_up_12 - cows_20_up_07,
    change_cows_02_07 = cows_20_up_07 - cows_20_up_02,
    change_cows_97_02 = cows_20_up_02 - cows_20_up_97,
    change_cows_92_97 = cows_20_up_97 - cows_20_up_92
  )

head(df_ag_census_more)

df_ag_census_more$FIPS_num <- df_ag_census_more$FIPS %>% as.numeric()

geo_ag_census_more <- geo_join(shapefile, 
                               full_join(df_ag_census_more, 
                                         df_state_fips,
                                         by =  c("STATEFP" = "state_fips")),
                               "FIPS", "FIPS_num") # assuming id to merge are named "FIPS"


gen_ag_census_leaf  <- function(geo_ag_census, var, var1, var2, 
                                label1, label2, file_name, 
                                breaks,
                                palette = "RdYlBu",
                                title = NULL, 
                                group =TRUE) {
  
  if (group) {
    mapvar <- paste0(var,"_gr")
    geo_ag_census <- add_map_var(geo_ag_census, 
                                 var = var, 
                                 varname = mapvar, 
                                 breaks = breaks,
                                 large_limits = TRUE)
  } else {
    mapvar <- var
  }
  
  popup <- 
    paste0(
      geo_ag_census$NAME, ", ", geo_ag_census$state_abbrev, 
      "<br>", label1,  ": ", formatcomma(geo_ag_census@data[[var1]]), 
      "<br>", label2,  ": ", formatcomma(geo_ag_census@data[[var2]]), 
      "<br> change: ",  formatcomma(geo_ag_census@data[[var]]),
      " (", ifelse(geo_ag_census@data[[var1]]==0, 
                   "NA)", 
                   paste0(round((geo_ag_census@data[[var2]] - geo_ag_census@data[[var1]])/
                                  geo_ag_census@data[[var1]], 2)*100, "%)"))
    )
  
  if (group) {
    pal <- colorFactor(
      palette = palette,
      domain = geo_ag_census[[mapvar]],
      na.color = 	"#FFFFFF"
    )
  } else {
    pal <- colorBin(
      palette = palette,
      domain = geo_ag_census[[mapvar]],
      na.color = 	"#FFFFFF"
    )
  }
  
  
  if (is.null(title)) {
    title <- paste("Change in Milk Cow Inveontory, <br>",
                   label1, "to", label2, "(heads)")
  }
  
  leaf_map <- my_leaflet(geo_ag_census, 
                         mapvar, 
                         pal = pal,
                         popup = popup,  
                         title = title)
  
  saveWidget(leaf_map,
             file= paste0(file_name,".html"), selfcontained=TRUE)
  
}

#  ---- changes ----
breaks_geo_change_cows <- c(-Inf,-5, -3, -2, -1, 0, 1, 2, 3, 5, Inf)*1000


gen_ag_census_leaf(geo_ag_census_more, 
                   var="change_cows_07_12", 
                   var1="cows_20_up_07", var2="cows_20_up_12", 
                   label1=2007, label2=2012, 
                   breaks = breaks_geo_change_cows,
                   file_name="change_cow_inventory_2007_2012")

gen_ag_census_leaf(geo_ag_census_more, 
                   var="change_cows_02_07", 
                   var1="cows_20_up_02", var2="cows_20_up_07", 
                   label1=2002, label2=2007, 
                   breaks = breaks_geo_change_cows,
                   file_name="change_cow_inventory_2002_2007") 


gen_ag_census_leaf(geo_ag_census_more, 
                   var="change_cows_97_02", 
                   var1="cows_20_up_97", var2="cows_20_up_02", 
                   label1=1997, label2=2002, 
                   breaks = breaks_geo_change_cows,
                   file_name="change_cow_inventory_1997_2002") 

gen_ag_census_leaf(geo_ag_census_more, 
                   var="change_cows_92_97", 
                   var1="cows_20_up_92", var2="cows_20_up_97", 
                   label1=1992, label2=1997, 
                   breaks = breaks_geo_change_cows,
                   file_name="change_cow_inventory_1992_1997")

#  ---- levels ----

breaks_geo_cows <- c(0, 0.5, 1, 2, 3, 5, 10, 20, Inf)*10

geo_ag_census_more@data <- 
  geo_ag_census_more@data %>% 
  mutate(
    cows_12_1000 = cows_20_up_12/1000,
    cows_07_1000 = cows_20_up_07/1000,
    cows_02_1000 = cows_20_up_02/1000,
    cows_97_1000 = cows_20_up_97/1000,
    cows_92_1000 = cows_20_up_92/1000
  )

gen_ag_census_leaf(geo_ag_census_more, 
                   var="cows_12_1000", 
                   var1="cows_20_up_07", var2="cows_20_up_12", 
                   label1=2007, label2=2012, 
                   breaks = breaks_geo_cows,
                   file_name="change_cow_inventory_2012",
                   palette = "YlGnBu",
                   title = "Cow Inventory, 2012 <br> (1,000 heads)")

gen_ag_census_leaf(geo_ag_census_more, 
                   var="cows_07_1000", 
                   var1="cows_20_up_02", var2="cows_20_up_07", 
                   label1=2002, label2=2007, 
                   breaks = breaks_geo_cows,
                   file_name="change_cow_inventory_2007",
                   palette = "YlGnBu",
                   title = "Cow Inventory, 2007 <br> (1,000 heads)")

gen_ag_census_leaf(geo_ag_census_more, 
                   var="cows_02_1000", 
                   var1="cows_20_up_97", var2="cows_20_up_02", 
                   label1=1997, label2=2002, 
                   breaks = breaks_geo_cows,
                   file_name="change_cow_inventory_2002",
                   palette = "YlGnBu",
                   title = "Cow Inventory, 2002 <br> (1,000 heads)")

gen_ag_census_leaf(geo_ag_census_more, 
                   var="cows_97_1000", 
                   var1="cows_20_up_92", var2="cows_20_up_97", 
                   label1=1992, label2=1997, 
                   breaks = breaks_geo_cows,
                   file_name="change_cow_inventory_1997",
                   palette = "YlGnBu",
                   title = "Cow Inventory, 1997 <br> (1,000 heads)")

gen_ag_census_leaf(geo_ag_census_more, 
                   var="cows_92_1000", 
                   var1="cows_20_up_92", var2="cows_20_up_97", 
                   label1=1992, label2=1997, 
                   breaks = breaks_geo_cows,
                   file_name="change_cow_inventory_1992",
                   palette = "YlGnBu",
                   title = "Cow Inventory, 1992 <br> (1,000 heads)")

# --- corn ----------



# --- soybean ----------




# --- Hispanic ----------


