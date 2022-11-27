################################################################################
## Author:    Jacob C. Alder
## Email:     alderjc@iu.edu
## Filename:  combine.R
## Purpose:   Combine LAUS unemployment data and EIA-860m generator data
## Inputs:    laus.Rdata and fips.Rdata
## Outputs:   combo.Rdata (combined file)
## Other:     
################################################################################
# Practice combinations ---------------------------------------------------
s.laus = laus[(year >= panel_end - 1) & (year <= panel_end),][county == "Monroe",]

# Rename because otherwise "unemp" looks same as "unemp_rate" in the regexp for melt(...)
setnames(s.laus, old = c("emp","unemp"),new = c("n_emp","n_unemp"))

# Cast to Wide
s.laus_dcast = dcast(s.laus,
                         fips ~ period,
                         value.var = c("labor_force","n_emp","n_unemp","unemp_rate"))

# Merge together
s.combo = merge(x = gens_c[county == "Monroe"], y = s.laus_dcast, by = "fips",all.x = TRUE)
s.melt_names = colnames(gens_c)

# Make a function to melt and combine
melter = 
  function(dt,var){
    temp = melt(dt,
              id.vars = s.melt_names,
              measure.vars = patterns(c(paste0("^",var))),
              variable.name = "date",
              variable.factor = FALSE,
              value.name = c(var)
              )
    temp[,period:=gsub(pattern = paste0(var,"_"),
                       replacement = "",
                       x = date)][,c("month","year") := tstrsplit(period, "-")
                               ][,`:=`(year = as.numeric(year) + 2000)]
    return(temp)
  }

# Certainly not the most elegant... but run for each of the variables
lf = melter(s.combo,"labor_force")
emp = melter(s.combo,"n_emp")
unemp = melter(s.combo,"n_unemp")
unemp_rate = melter(s.combo,"unemp_rate")

# Then merge together
c = lf[,`:=`(n_emp = emp[,n_emp],
             n_unemp = unemp[,n_unemp],
             unemp_rate = unemp_rate[,unemp_rate])     ]

# Pare down observations and run on full ----------------------------------
gens_c[,.(n = .N),by = list(operational_status, fuel_group_eia)]
gens_c[fuel_group_eia == "fossil",.(n = .N),by = list(operational_status,fuel_type_code_pudl)]
gens_c[fuel_group_eia == "other",.(n = .N),by = list(operational_status,fuel_type_code_pudl)]

# Subset to fossil
gens_fossil = gens_c[fuel_group_eia == "fossil",]

# Prepare join
f.laus = laus[year >= panel_start & year <= panel_end,]

# Rename because otherwise "unemp" looks same as "unemp_rate" in the regexp for melt(...)
setnames(f.laus, old = c("emp","unemp"),new = c("n_emp","n_unemp"))

# Cast to Wide
f.laus_dcast = dcast(f.laus,
                     fips ~ period,
                     value.var = c("labor_force","n_emp","n_unemp","unemp_rate"),
                     fun.aggregate = mean)

# Merge together
f.combo = merge(x = gens_fossil, y = f.laus_dcast, by = "fips", all.x = TRUE)
f.melt_names = colnames(gens_fossil)

# Use the melter function from above
lf = melter(f.combo,"labor_force")
emp = melter(f.combo,"n_emp")
unemp = melter(f.combo,"n_unemp")
unemp_rate = melter(f.combo,"unemp_rate")

# Then merge together
f = lf[,`:=`(n_emp = emp[,n_emp],
             n_unemp = unemp[,n_unemp],
             unemp_rate = unemp_rate[,unemp_rate])     ]

# Remove the excess to save RAM!
rm(f.combo,f.laus,f.laus_dcast,lf,emp,unemp,unemp_rate)
rm(s.combo,s.laus,s.laus_dcast)

# Fix month
f[,month:=match(tolower(month), tolower(month.abb))]

# Save and clean up -- FYI, `f` is f-ing big, like 500 MB. Be warned.
save(f,gens_fossil,file = file.path(data.dir,"Rdata","combo.Rdata"))

# -------------------------------------------------------------------------