################################################################################
## Author:    Jacob C. Alder
## Email:     alderjc@iu.edu
## Filename:  load.R
## Purpose:   Load packages and data dependencies
## Inputs:    EIA 860m, supplement to EIA 860
## Outputs:   
## Other:     https://www.eia.gov/electricity/data/eia860m/
################################################################################
# Load EIA data -----------------------------------------------------------
load.eia_860m = 
  function(sheet){
    temp = openxlsx::read.xlsx(
      xlsxFile = file.path(data.dir,"eia","eia-860m","january_generator2020.xlsx")
      ,sheet = sheet
      ,startRow = 2
      ,colNames = TRUE
      )
    
  # Make clean `data.table`
  temp = janitor::clean_names(temp)
  setDT(temp)
  temp[,`:=`(operational_status = paste0(sheet),
                    ID = paste0(entity_id,"_",
                            plant_id,"_",
                            generator_id)
                )
          ]
  
  # Set keys
  keycols = c(  "ID"
                ,"entity_id"
                ,"plant_id"
                ,"generator_id")
  
  setkeyv(temp,
          cols = keycols)
  setorderv(temp,
            cols = keycols)
  
  # Clean names
  return(temp)
}

load.census = 
  function(){
    return(NULL)
}

# Load Data ---------------------------------------------------------------
eia.RE = load.eia_860m(sheet = "Retired")
eia.OP = load.eia_860m(sheet = "Operating")
eia.CN = load.eia_860m(sheet = "Canceled or Postponed")

# Remove unnecessary functions --------------------------------------------
rm(list=ls(pattern = "^load*"))

# Save to speed up load process
save(eia.RE,eia.OP,eia.CN,file = file.path(data.dir,"Rdata","january_generator2020.Rdata"))

# -------------------------------------------------------------------------