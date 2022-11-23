################################################################################
## Author:    Jacob C. Alder
## Email:     alderjc@iu.edu
## Filename:  main.R
## Purpose:   
## Inputs:    
## Outputs:   
## Other:     https://www.eia.gov/electricity/data/eia860m/
################################################################################
# Set Directory -----------------------------------------------------------
my.dir = "/Users/jacobalder/Library/Mobile Documents/com~apple~CloudDocs/JCA/SCH/IU-SPEA/MPA/iu-coursework/SPEA-P762 (Wing)"
main.dir = file.path(my.dir,"plant-closure-labor-effects")
code.dir = file.path(main.dir,"code")
data.dir = file.path(main.dir,"data")
if(!getwd() == main.dir){setwd(main.dir)}

# Set Options
options("digits" = 4)

# Run source code ---------------------------------------------------------
# Load files (make sure they're downloaded on iCloud or OneDrive)
source("code/load.R")

# Clean files
source("code/clean.R")
