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
fips = tidycensus::fips_codes
setDT(fips)
fips[,county := gsub(" County","",county)]
fips[,fips:=paste0(state_code,county_code)]

# Fix County Names
x <- re[,county:=stringi::stri_trans_totitle(county)]
setnames(x, old = "plant_state", new = "state")

# Use Lat/Long to look up missing county FIPS -----------------------------
# if (!require("RJSONIO")) install.packages("RJSONIO")
# http://www.fcc.gov/developers/census-block-conversions-api
latlong2fips =
  function(latitude, longitude) {
    if(!is.na(as.numeric(latitude)) & !is.na(as.numeric(longitude))){
      url <- "https://geo.fcc.gov/api/census/area?lat=%s&lon=%s&censusYear=2010&format=json"
      url <- sprintf(url, latitude, longitude)
      url <- sprintf(url, lat, lon)
      json <- RJSONIO::fromJSON(url)
        if(length(json[["results"]]) == 0){return("0")} else {return(as.character(json[["results"]][[1]][["county_fips"]]))}
    } else {return("0")}
  }

# Quick for loop function
latlong2fips_convert = 
  function(dt){
    for(i in 1:nrow(dt)){
      lat = dt[i, latitude]
      lon = dt[i, longitude]
      dt[i, fips := latlong2fips(latitude = lat, longitude = lon)]
    }
  }

# # Subset to remove the missing latitudes and longitudes
lat_lon = c("latitude","longitude")
a = re[,(lat_lon) := lapply(.SD, as.numeric),.SDcols = lat_lon
       ][!is.na(latitude),][!is.na(longitude),][,fips:="0"]

# Convert fips for all (doesn't work, function problem, leaving for now)
latlong2fips_convert(a)

# Store a list of generators with missing lat/lon
missing_lat_lon = re[is.na(latitude) | is.na(longitude),]


# Try merging to get county codes -----------------------------------------
# Merge FIPS codes
fips_merge = merge(x = x, y = fips, by = c("state","county"), all.x = TRUE)
fips_merge[is.na(county_code),
           .(n = uniqueN(county)), keyby = state]

# Get the stragglers
missing_county_codes = fips_merge[is.na(fips),.(ID = unique(ID)), keyby = list(state,county,latitude,longitude,fips)]

# Convert fips on missing county codes
latlong2fips_convert(missing_county_codes)

# Add back in the missing county codes
fips_merge_fix = merge(x = fips_merge, y = missing_county_codes, 
                       by = "ID", all.x = TRUE)

# Clean up ---------------------------------------------------------------
re_cleaned = fips_merge_fix[,`:=`(fips=fifelse(test = 
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
re_c <- re_cleaned %>% 
  dplyr::select(-contains(c('.x', '.y')))

# Clean Environment
rm(a
   ,fips_merge
   ,fips_merge_fix
   ,missing_county_codes
   ,re_cleaned
   ,x
   ,i
   ,lat
   ,lon
   ,lat_lon
   ,url
   ,merged_cols_to_drop
   ,latlong2fips
   ,latlong2fips_convert)

# -------------------------------------------------------------------------