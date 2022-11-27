################################################################################
## Author:    Jacob C. Alder
## Email:     alderjc@iu.edu
## Filename:  clean.R
## Purpose:   Load packages and data dependencies
## Inputs:    EIA 860m, supplement to EIA 860
## Outputs:   
## Other:     https://www.eia.gov/electricity/data/eia860m/
################################################################################
names(eia.CN)
names(eia.RE)
names(eia.OP)

# Start with retired generators -------------------------------------------
cols_of_interest = c(
  "ID"
  ,"entity_id"
  ,"plant_id"
  # ,"generator_id"
  ,"operational_status"
  ,"operating_year"                
  ,"operating_month"
  ,"retirement_year"               
  ,"retirement_month"
  # ,"entity_name"                   
  # ,"plant_name"                 
  ,"plant_state"
  ,"county"                  
  ,"balancing_authority_code"
  ,"sector"
  # ,"unit_code"                     
  ,"nameplate_capacity_mw"
  ,"net_summer_capacity_mw"
  ,"net_winter_capacity_mw"
  ,"technology"
  ,"energy_source_code"
  ,"prime_mover_code"              
  # ,"nameplate_energy_capacity_m_wh"
  # ,"dc_net_capacity_mw"            
  ,"latitude"
  ,"longitude"
)

# Drop retirements outside panel
re = eia.RE[!is.na(retirement_year) & 
              (retirement_year >= panel_start) & 
              (retirement_year <= panel_end),
            ..cols_of_interest]

# Fuel Source... derived from 
# https://catalystcoop-pudl.readthedocs.io/en/latest/data_dictionaries/codes_and_labels.html#energy-sources-eia
energy_sources_eia = fread(file.path(data.dir,"pudl","energy_sources_eia.csv"))

# Merge info about energy sources, drop blanks
re = re[energy_sources_eia, on = .(energy_source_code = code)
              ][!is.na(ID),]

# Add generator age @ retirement
re[,age:=retirement_year-operating_year]

# Add generator info
re = fastDummies::dummy_cols(re, select_columns = c("fuel_type_code_pudl"))

# Count number of retired units by year
re[,.(coal = sum(fuel_type_code_pudl_coal),
      gas = sum(fuel_type_code_pudl_gas),
      wind = sum(fuel_type_code_pudl_wind),
      n = .N),
   keyby = retirement_year]

# Count number of retired units by state
re[,.(coal = sum(fuel_type_code_pudl_coal),
      gas = sum(fuel_type_code_pudl_gas),
      wind = sum(fuel_type_code_pudl_wind),
      n = .N),
   keyby = plant_state]


# Then do Operating units -------------------------------------------------
# Remove retirement bits and add Operating bits
cols_of_interest = 
  c(cols_of_interest,
          "planned_derate_month"
          ,"planned_derate_of_summer_capacity_mw"
          ,"planned_derate_year"
          ,"planned_uprate_month"
          ,"planned_uprate_of_summer_capacity_mw"
          ,"planned_uprate_year"
          ,"planned_retirement_month"
          ,"planned_retirement_year"
          ,"status"
  )[!cols_of_interest %in% c("retirement_year","retirement_month")]

# Merge info about energy sources, drop blanks, keep only active OPerating gens
op = eia.OP[status %like% "OP",..cols_of_interest
            ][energy_sources_eia, on = .(energy_source_code = code)
              ][!is.na(ID),]

# Add generator age @ retirement
op[,`:=`(age = panel_end - operating_year)]

# Add generator info
op = fastDummies::dummy_cols(op, select_columns = c("fuel_type_code_pudl"))

# Count number of operating units by state
op[,.(coal = sum(fuel_type_code_pudl_coal),
      gas = sum(fuel_type_code_pudl_gas),
      wind = sum(fuel_type_code_pudl_wind),
      n = .N),
   keyby = plant_state]

# Combine Operating and Retired -------------------------------------------
gens = merge(op,re, all = TRUE)

# Save to speed up load process
save(gens,op,re,file = file.path(data.dir,"Rdata","clean_R_gens.Rdata"))

# -------------------------------------------------------------------------