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
# Drop generators retired before 2002
cols_of_interest = c(
  "ID"
  # ,"entity_id"
  # ,"plant_id"
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
  # ,"sector"                        
  # ,"unit_code"                     
  # ,"nameplate_capacity_mw"
  # ,"net_summer_capacity_mw"        
  # ,"net_winter_capacity_mw"
  # ,"technology"                    
  ,"energy_source_code"
  ,"prime_mover_code"              
  # ,"nameplate_energy_capacity_m_wh"
  # ,"dc_net_capacity_mw"            
  # ,"latitude"
  # ,"longitude"               
)

# Drop anything before 2001
re = eia.RE[!is.na(retirement_year) & 
              (retirement_year >= 2002) & 
              (retirement_year <= 2020),
            ..cols_of_interest]

# Add a Covid dummy
re[retirement_year == 2020 & retirement_month >= 3, post_covid := 1]

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
