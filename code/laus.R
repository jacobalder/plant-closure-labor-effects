################################################################################
## Author:    Jacob C. Alder
## Email:     alderjc@iu.edu
## Filename:  laus.R
## Purpose:   String together 14-month segments from BLS
## Inputs:    Local Area Unemployment Statistics (LAUS) from U.S. Bureau of Labor Statistics (BLS).
## Outputs:   
## Other:     https://www.bls.gov/lau/lauov.htm
## Note:      Downloaded from BLS using Internet Archive
################################################################################
# Load Packages ----------------------------------------------------------
## LAUS package from @jjchern does Annual LAUS
# remotes::install_github("jjchern/laus@v0.0.4")
# install.package("remotes")
## Variable Names
# names(laus::state_year)
# labelled::var_label(laus::state_year)
## Annual County info 
# library(tidyverse)
# laus::state_year
# c = laus::county_year

# Load manually downloaded LAUS data --------------------------------------
laus_read = 
  function(fname){
    # Read the data and drop the bad rows
    temp = fread(file.path(data.dir,"bls","laucnty",fname))[!(1:6),]
    
    # Rename the columns
    colnames(temp) = c("laus_code","state_fips","county_fips","county_state","period","labor_force","emp","unemp","unemp_rate")
    
    # Clean up the months and years
    temp = temp[,`:=`(preliminary = 0, corrected = 0)
             # Drop NAs and blanks
                 ][!is.na(period) & !(period == ""),
                   # Correct the Preliminary (p) and the Corrected (c)
                   ][period %like% "\\(c\\)", `:=`(corrected = 1, period = gsub("\\(c\\)","",period))
                     ][period %like% "\\(p\\)", `:=`(preliminary = 1, period = gsub("\\(p\\)","",period))
                       # Split the month and year, then make the year whole
                       ][,c("month","year") := tstrsplit(period, "-")
                         ][,`:=`(year = as.numeric(year) + 2000)]
    
    # Clean up the counties, states
    temp = temp[,c("county","state") := tstrsplit(gsub("County","",county_state),", ")
                  ][,`:=`(county = stringr::str_trim(county, "left"),
                          state = stringr::str_trim(state, "right"))
                    # Fix District of Columbia
                    ][county_state == "District of Columbia", state:="DC"
                      # Drop Puerto Rico
                      ][state != "PR",]
    
    # Convert to numerics using a function
    numeric_cols = c("labor_force","emp","unemp","unemp_rate")
    temp = temp[,(numeric_cols) := lapply(.SD, function(x){as.numeric(gsub(",","",x))}),.SDcols = numeric_cols]
    
    # Set keys
    keycols = c(  "state"
                  ,"county"
                  ,"year"
                  ,"month")
    
    setkeyv(temp,
            cols = keycols)
    setorderv(temp,
              cols = keycols)
    
    # Clean names
    return(temp)
  }

# List of files
laucnty = list.files(file.path(data.dir,"bls","laucnty"))

# Loop over files, save 'em
for (i in 1:length(laucnty)){
    temp = laus_read(laucnty[i])
    if (i==1) {
      t = temp
      print(paste0("\'",laucnty[i],"\' = temp"))
    } else {
      t = rbind(t,temp)
      print(paste0("t = rbind(\'",laucnty[i],"\' to temp)"))
      rm(temp)
  }
}

# Remove duplicates
laus = funique(t, c("state","county","year","month"))

# FYI, there's CN, PA, and PS, not sure what the differences are
cn = laus[,cn:=substring(laus_code,1,2)]
cn[,.(n = .N),keyby = list(cn,year)]

# Fix the FIPS codes
laus[,`:=`(state_fips = stringr::str_pad(state_fips,2,side = "left",pad = "0"),
           county_fips = stringr::str_pad(county_fips,3,side = "left",pad = "0"))
     ][,fips:=paste0(state_fips,county_fips)]

# Clean up
rm(laus_read,i,t,cn)

# End of file -------------------------------------------------------------