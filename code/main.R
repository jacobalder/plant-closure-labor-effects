################################################################################
## Author:    Jacob C. Alder
## Email:     alderjc@iu.edu
## Filename:  main.R
## Purpose:   
## Inputs:    
## Outputs:   
## Other:     https://www.eia.gov/electricity/data/eia860m/
################################################################################
# Load Packages ----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load( collapse
                ,data.table
                ,fixest
                ,ggplot2
                ,stats
                ,stargazer
)

# Set Directory -----------------------------------------------------------
my.dir = "/Users/jacobalder/Library/Mobile Documents/com~apple~CloudDocs/JCA/SCH/IU-SPEA/MPA/iu-coursework/SPEA-P762 (Wing)"
main.dir = file.path(my.dir,"plant-closure-labor-effects")
code.dir = file.path(main.dir,"code")
data.dir = file.path(main.dir,"data")
if(!getwd() == main.dir){setwd(main.dir)}

# Set Options
options("digits" = 16)

# Global definitions ------------------------------------------------------
# Panel start year and end year
panel_start = 2009
panel_end = 2019

# Run source code ---------------------------------------------------------
# Load files (make sure they're downloaded on iCloud or OneDrive)
# source("code/load.R")

# Clean files
# source("code/clean.R")

# Local Area Unemployment Statistics (LAUS) from U.S. Bureau of Labor Statistics (BLS).
# Load files (make sure they're downloaded on iCloud or OneDrive)
# source("code/laus.R")
load(file.path(data.dir,"Rdata","laus.Rdata"))

# Add FIPS codes
load(file.path(data.dir,"Rdata","fips.Rdata"))
source("code/fips.R")
