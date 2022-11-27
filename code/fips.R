################################################################################
## Author:    Jacob C. Alder
## Email:     alderjc@iu.edu
## Filename:  fips.R
## Purpose:   Add FIPS codes to prepare for join with LAUS
## Inputs:    Tidycensus FIPS codes (2019) and API from the FCC using 2010 FIPS codes
## Outputs:   Cleaned data.table with FIPS codes
## Other:     Access the FCC's API here: https://geo.fcc.gov/api/census
################################################################################
# Load packages -----------------------------------------------------------
p_load( tigris
       ,tidycensus
       ,stringi)

# Fix fips_codes
dt.fips = tidycensus::fips_codes
setDT(dt.fips)
dt.fips[,`:=`(county = gsub(" County","",county),
              fips = paste0(state_code,county_code))]

# Fix County Names
fips_gens = gens[,county:=stringi::stri_trans_totitle(county)]
setnames(fips_gens, old = "plant_state", new = "state")

# Use Lat/Long to look up missing county FIPS -----------------------------
# if (!require("RJSONIO")) install.packages("RJSONIO")
# https://geo.fcc.gov/api/census/#!/area/get_area
latlong2fips =
  function(latitude, longitude) {
    if(!is.na(as.numeric(latitude)) & !is.na(as.numeric(longitude))){
      url <- "https://geo.fcc.gov/api/census/area?lat=%s&lon=%s&censusYear=2010&format=json"
      url <- sprintf(url, latitude, longitude)
      json <- RJSONIO::fromJSON(url)
        if(length(json[["results"]]) == 0){return("0")} else {return(as.character(json[["results"]][[1]][["county_fips"]]))}
    } else {return("0")}
  }

# Quick for loop function
latlong2fips_convert = 
  function(dt){
    for(i in 1:nrow(dt)){
      dt[i, fips := latlong2fips(latitude = dt[i, latitude], longitude = dt[i, longitude])]
    }
  }

# # Subset to remove the missing latitudes and longitudes
lat_lon = c("latitude","longitude")
fips_gens[,(lat_lon) := lapply(.SD, as.numeric),.SDcols = lat_lon]

# Store a list of generators with missing lat/lon
missing_lat_lon = fips_gens[is.na(latitude) | is.na(longitude),]

# Drop blank lat/lon
fips_gens = gens[!is.na(latitude),][!is.na(longitude),]

# Try merging to get county codes -----------------------------------------
# Merge FIPS codes
fips_merge = merge(x = fips_gens, y = dt.fips, by = c("state","county"), all.x = TRUE)
fips_merge[is.na(county_code),
           .(n = uniqueN(county)), keyby = state]

# Get the stragglers (FYI, I checked that the plant ids are unique on a state,county,lat_lon basis)
missing_county_codes = fips_merge[is.na(fips),.(plant_id = unique(plant_id)),
                                    keyby = list(state,county,latitude,longitude,fips,entity_id)]

# Convert fips on missing county codes (this takes a while)
latlong2fips_convert(missing_county_codes)

# Add back in the missing county codes
fips_merge_fix = merge(x = fips_merge, y = missing_county_codes, 
                       by = c("entity_id","plant_id"), all.x = TRUE)

# Clean up ---------------------------------------------------------------
gens_cleaned = fips_merge_fix[,`:=`(fips=fifelse(test = 
                                                 is.na(fips.x),
                                               yes = fips.y,
                                               no = fips.x),
                                  latitude = fifelse(test = 
                                                       is.na(latitude.x),
                                                     yes = latitude.y,
                                                     no = latitude.x),
                                  longitude = fifelse(test = 
                                                        is.na(longitude.x),
                                                      yes = longitude.y,
                                                      no = longitude.x),
                                  state = fifelse(test = 
                                                    is.na(state.x),
                                                  yes = state.y,
                                                  no = state.x),
                                  county = fifelse(test = 
                                                    is.na(county.x),
                                                  yes = county.y,
                                                  no = county.x))
                            ]

# Drop the merged cols
gens_c <- gens_cleaned %>% 
  dplyr::select(-contains(c('.x', '.y')))

# Save vars
save(dt.fips,gens_c,missing_county_codes,missing_lat_lon,file = file.path(data.dir,"Rdata","fips.Rdata"))

# Clean Environment
rm( fips_merge
   ,fips_merge_fix
   ,fips_gens
   ,gens_cleaned
   ,latlong2fips
   ,latlong2fips_convert)
# -------------------------------------------------------------------------