## CHAPMAN ET AL.
## 'Quantifying the biodiversity pressures of fruit
## and vegetable consumption in the United Kingdom, India, and 
## South Africa'
##
## R Script to accompany manuscript (2021-2024)
##
## Note that the code included here excludes testing, for
## a cleaner script. For tests and further scripting, please
## feel free to contact the corresponding author.

## Script Part 2 (run part 1 first).

###################################################################################################################
###################################################################################################################
# 3. Calculating Affected Species Range (species richness * harvested area)
# and Biodiversity Pressure (species richness * harvested area / production).
###################################################################################################################
###################################################################################################################

outDir_ASR = "SHEFS/SHEFS_Sept2023_Update/2_ASR/"
outDir_pressure = "SHEFS/SHEFS_Sept2023_Update/3_BP/"
vertebrates_projected = vertebrates_projected1

pressure_fun = function(ras_stack2) {
  for(i in 1:nlayers(ras_stack2)) {
    ras_stack2[[i]][ras_stack2[[i]] == 0] <- NA
    richness_mask = mask(vertebrates_projected, ras_stack2[[i]])
    process_step1 = (ras_stack2[[i]] * (vertebrates_projected))
    raster::writeRaster(process_step1, filename = paste0(outDir_ASR, "ASR_calculated", names(ras_stack2[[i]])), format = "GTiff", overwrite = TRUE)
    dry_weight_production_maps_global[[i]] [dry_weight_production_maps_global [[i]] == 0] <- NA
    process_step2 = (process_step1/dry_weight_production_maps_global[[i]])
    raster::writeRaster(process_step2, filename = paste0(outDir_pressure, "pressure_calculated", names(ras_stack2[[i]])), format = "GTiff", overwrite = TRUE)
  }}

# pressure_calculating =  pressure_fun(ras_stack2 = fnv_harvestedarea_global) # This is only commented out as it is slow to run.
pressuremaps = paste(outDir_pressure, dir(path = outDir_pressure, recursive = TRUE), sep = "")
pressuremaps = pressuremaps[!grepl(".ovr", pressuremaps)]
pressure_dry = stack(pressuremaps)
asrmaps = paste(outDir_ASR, dir(path = outDir_ASR, recursive = TRUE), sep = "")
asrmaps = asrmaps[!grepl(".ovr", asrmaps)]
asr = stack(asrmaps)

###################################################################################################################
###################################################################################################################
# 4. Computing per-country values.
###################################################################################################################
###################################################################################################################

# 4.1 BP per country:

# Below only commented out as slow to run:
# pressure_dry_percountry = exact_extract(pressure_dry, wrld_simpl4, fun = 'weighted_mean', weights = dry_weight_production_maps_global, include_area = FALSE, include_cell = FALSE,
# include_xy = FALSE, force_df = TRUE, full_colnames = TRUE, stack_apply = TRUE, summarize_df = FALSE, progress = TRUE)
# pressure_dry_percountry
# pressure_dry_percountry$countrycode = wrld_simpl4$ISO3
# pressure_dry_percountry$country = wrld_simpl4$NAME

# Below just tidies and writes the output from above:
outDir_pressure_percountry = "SHEFS/SHEFS_Sept2023_Update/3.1_BP_percountry/"
form_percountry1 = paste0(outDir_pressure_percountry, "pressure_dry_per_country_update1Dec2023.csv")
# write.csv(pressure_dry_percountry, file = form_percountry1)
pressure_dry_percountry = read.csv(paste0(outDir_pressure_percountry, "pressure_dry_per_country_update1Dec2023.csv"))
colnames(pressure_dry_percountry)
pressure_dry_percountry1 = pressure_dry_percountry
colnames(pressure_dry_percountry1) = gsub("weighted_mean.pressure_calculated", "", colnames(pressure_dry_percountry1), fixed = TRUE)
colnames(pressure_dry_percountry1) = gsub("\\..*", "", colnames(pressure_dry_percountry1), fixed = FALSE)
pressure_dry_percountry1 = subset(pressure_dry_percountry1, select= -c(X))
str(pressure_dry_percountry1)
head(pressure_dry_percountry1)
pressure_dry_percountry2 = data.frame(t(pressure_dry_percountry1[,1:53]))
colnames(pressure_dry_percountry2) = wrld_simpl4$NAME
head(pressure_dry_percountry2)
pressure_dry_percountry3 = data.frame(t(pressure_dry_percountry2))
pressure_dry_percountry3$countrycode = wrld_simpl4$ISO3
pressure_dry_percountry3$country = wrld_simpl4$NAME
pressure_dry_percountry3
pressure_dry_percountry4 = melt(pressure_dry_percountry3, id.vars = c("countrycode", "country"))
head(pressure_dry_percountry4); str(pressure_dry_percountry4)
colnames(pressure_dry_percountry4) = c("countrycode", "country", "crop", "pressure_dry")
form_percountry1a = paste0(outDir_pressure_percountry, "pressure_dry_percountry_organised_update_1stDec2023.csv")
# write.csv(pressure_dry_percountry4, file = form_percountry1a)

# Total BP across fruits and vegetables, per country:
pressure_dry_percountry_sums = pressure_dry_percountry4 %>% 
  group_by(countrycode) %>% 
  summarise(sum_fv  = sum(pressure_dry, na.rm = TRUE))
form_percountry1b = paste0(outDir_pressure_percountry, "pressure_dry_percountry_fvsums_organised_update_1stdec2023.csv")
# write.csv(pressure_dry_percountry_sums, file = form_percountry1b)

# and what are the global rankings of fruit and veg?
globalrank_pressure_dry = na.omit(pressure_dry_percountry4) %>% 
  mutate(rank = rank(pressure_dry)) %>% 
  arrange(-rank)
# how about if you sum the pressure_dry across all countries?
globalrank_pressure_dry2 = pressure_dry_percountry4 %>% 
  group_by(crop) %>% 
  summarise(sum_percrop = sum(pressure_dry, na.rm = TRUE)) %>% 
  mutate(rank = rank(sum_percrop)) %>% 
  arrange(-rank)
# outdir = "SHEFS/SHEFS_Sept2023_Update/5_FigureData_and_Figs/"
# write.csv(data.frame(globalrank_pressure_dry2), paste0(outdir, "global_rankings_crops_based_on_pressure_dry.csv"))
# top is sourcherry, then persimmon, then okra, but you still have plantain and date up there


# 4.2 ASR per country:

# Commented out below as slow to run:
# asr_dry_percountry = exact_extract(asr, wrld_simpl4, fun = 'weighted_mean', weights = dry_weight_production_maps_global, include_area = FALSE, include_cell = FALSE,
# include_xy = FALSE, force_df = TRUE, full_colnames = TRUE, stack_apply = TRUE, summarize_df = FALSE, progress = TRUE)
# asr_dry_percountry
# asr_dry_percountry$countrycode = wrld_simpl4$ISO3
# asr_dry_percountry$country = wrld_simpl4$NAME
outDir_asr_percountry = "SHEFS/SHEFS_Sept2023_Update/2.1_ASR_percountry/"
form_percountry2 = paste0(outDir_asr_percountry, "asr_per_country_update_1stdec2023.csv")
# write.csv(asr_dry_percountry, file = form_percountry2)

# Below just writes the outputs from above in a tidier form:
asr_percountry = read.csv(paste0(outDir_asr_percountry, "asr_per_country_update_1stdec2023.csv"))
colnames(asr_percountry)
asr_dry_percountry1 = asr_percountry
colnames(asr_dry_percountry1) = gsub("weighted_mean.ASR_calculated", "", colnames(asr_dry_percountry1), fixed = TRUE)
colnames(asr_dry_percountry1) = gsub("\\..*", "", colnames(asr_dry_percountry1), fixed = FALSE)
asr_dry_percountry1 = subset(asr_dry_percountry1, select= -c(X))
asr_dry_percountry2 = data.frame(t(asr_dry_percountry1[,1:53]))
colnames(asr_dry_percountry2) = wrld_simpl4$NAME
head(asr_dry_percountry2)
asr_dry_percountry3 = data.frame(t(asr_dry_percountry2))
asr_dry_percountry3$countrycode = wrld_simpl4$ISO3
asr_dry_percountry3$country = wrld_simpl4$NAME
asr_dry_percountry3
asr_dry_percountry4 = melt(asr_dry_percountry3, id.vars = c("countrycode", "country"))
head(asr_dry_percountry4); str(asr_dry_percountry4)
colnames(asr_dry_percountry4) = c("countrycode", "country", "crop", "asr")
form_percountry1a = paste0(outDir_asr_percountry, "asr_dry_percountry_organised_update_1stdec2023.csv")
# write.csv(asr_dry_percountry4, file = form_percountry1a)

# what are the ranges of this?
range(asr_dry_percountry4$asr, na.rm = TRUE) #  0 1321752
1321752-0 # ASR RANGE: 1321752

# and what are the global rankings of fruit and veg?
globalrank_asr = na.omit(asr_dry_percountry4) %>% 
  mutate(rank = rank(asr)) %>% 
  arrange(-rank)
# how about if you sum the asr across all countries?
globalrank_asr2 = asr_dry_percountry4 %>% 
  group_by(crop) %>% 
  summarise(sum_percrop = sum(asr, na.rm = TRUE)) %>% 
  mutate(rank = rank(sum_percrop)) %>% 
  arrange(-rank)
# outdir = "SHEFS/SHEFS_Sept2023_Update/5_FigureData_and_Figs/"
# write.csv(data.frame(globalrank_asr2), paste0(outdir, "global_rankings_crops_based_on_asr.csv"))
# top is plantain, then banana, then tomato, then date

# Total ASR, across fruits and vegetables, per country:
asr_percountry_sums = asr_dry_percountry4 %>% 
  group_by(countrycode) %>% 
  summarise(sum_fv  = sum(asr, na.rm = TRUE))
form_percountry1c = paste0(outDir_asr_percountry, "asr_percountry_fvsums_organised_update_1stdec2023.csv")
# write.csv(asr_percountry_sums, file = form_percountry1c)

# 4.3 Production per country (doesn't need the weighting so taking the mean per country):

# Commented out below as slow to run:
# production_dry_percountry = exact_extract(dry_weight_production_maps_global, wrld_simpl4, fun = 'mean', include_area = FALSE, include_cell = FALSE,
#                             include_xy = FALSE, force_df = TRUE, full_colnames = TRUE, stack_apply = TRUE, summarize_df = FALSE, progress = TRUE)
# production_dry_percountry
# production_dry_percountry$countrycode = wrld_simpl4$ISO3
# production_dry_percountry$country = wrld_simpl4$NAME

# Below just writes a tidier output:
outDir_production_percountry = "SHEFS/SHEFS_Sept2023_Update/1.1_DryWeight_Production_percountry/"
# production_percountry_outform = paste0(outDir_production_percountry, "production_per_country_update.csv")
# write.csv(production_dry_percountry, file = production_percountry_outform)
production_percountry = read.csv(paste0(outDir_production_percountry, "production_per_country_update.csv"))
colnames(production_percountry)
production_dry_percountry1 = production_percountry
colnames(production_dry_percountry1) = gsub("mean.dry_weight_production_tonnes", "", colnames(production_dry_percountry1), fixed = TRUE)
production_dry_percountry1 = subset(production_dry_percountry1, select= -c(X))
production_dry_percountry2 = data.frame(t(production_dry_percountry1[,1:53]))
colnames(production_dry_percountry2) = wrld_simpl4$NAME
head(production_dry_percountry2)
production_dry_percountry3 = data.frame(t(production_dry_percountry2))
production_dry_percountry3$countrycode = wrld_simpl4$ISO3
production_dry_percountry3$country = wrld_simpl4$NAME
production_dry_percountry3
production_dry_percountry4 = melt(production_dry_percountry3, id.vars = c("countrycode", "country"))
head(production_dry_percountry4); str(production_dry_percountry4)
colnames(production_dry_percountry4) = c("countrycode", "country", "crop", "production")
form_percountry1a = paste0(outDir_production_percountry, "production_dry_percountry_organised_update.csv")
# write.csv(production_dry_percountry4, file = form_percountry1a)

# what are the ranges of this?
range(production_dry_percountry4$production, na.rm = TRUE) #  2.382207e-44 1.605996e+04
1.605996e+04-2.382207e-44 # Production RANGE: 16059.96

# and what are the global rankings of fruit and veg?
globalrank_production = na.omit(production_dry_percountry4) %>% 
  mutate(rank = rank(production)) %>% 
  arrange(-rank)
#print(globalrank_production, n = 25)
# how about if you sum the production across all countries?
globalrank_production2 = production_dry_percountry4 %>% 
  group_by(crop) %>% 
  summarise(sum_percrop = sum(production, na.rm = TRUE)) %>% 
  mutate(rank = rank(sum_percrop)) %>% 
  arrange(-rank)
# outdir = "SHEFS/SHEFS_Sept2023_Update/5_FigureData_and_Figs/"
# write.csv(data.frame(globalrank_production2), paste0(outdir, "global_rankings_crops_based_on_production.csv"))
# top is dates, then plantain, then bananas...



# Total production, across fruits and vegetables, per country:
prod_percountry_sums = production_dry_percountry4 %>% 
  group_by(countrycode) %>% 
  summarise(sum_fv  = sum(production, na.rm = TRUE))
form_percountry1d = paste0(outDir_production_percountry, "production_percountry_fvsums_organised_update.csv")
# write.csv(prod_percountry_sums, file = form_percountry1d)

# 4.4 Totals instead of averages per country:

# 4.4.1 Total ASR
# Below commented out as before, because slow to run:
# asr_dry_percountry_total = exact_extract(asr, wrld_simpl4, fun = 'sum', include_area = FALSE, include_cell = FALSE,
# include_xy = FALSE, force_df = TRUE, full_colnames = TRUE, stack_apply = TRUE, summarize_df = FALSE, progress = TRUE)
# asr_dry_percountry_total
# asr_dry_percountry_total$countrycode = wrld_simpl4$ISO3
# asr_dry_percountry_total$country = wrld_simpl4$NAME

# And again, tidying up the output:
outDir_asr_percountry = "SHEFS/SHEFS_Sept2023_Update/2.1_ASR_percountry/"
form_percountry2 = paste0(outDir_asr_percountry, "total_asr_per_country_update_1stdec2023.csv")
# write.csv(asr_dry_percountry_total, file = form_percountry2)
total_asr_percountry = read.csv(paste0(outDir_asr_percountry, "total_asr_per_country_update_1stdec2023.csv"))
colnames(total_asr_percountry)
total_asr_percountry1 = total_asr_percountry
colnames(total_asr_percountry1) = gsub("sum.ASR_calculated", "", colnames(total_asr_percountry1), fixed = TRUE)
total_asr_percountry2 = data.frame(t(total_asr_percountry1[,1:53]))
colnames(total_asr_percountry2) = wrld_simpl4$NAME
head(total_asr_percountry2)
total_asr_percountry3 = data.frame(t(total_asr_percountry2))
total_asr_percountry3$countrycode = wrld_simpl4$ISO3
total_asr_percountry3$country = wrld_simpl4$NAME
total_asr_percountry3
total_asr_percountry3 = subset(total_asr_percountry3, select = -c(X))
total_asr_percountry4 = melt(total_asr_percountry3, id.vars = c("countrycode", "country"))
head(total_asr_percountry4); str(total_asr_percountry4)
colnames(total_asr_percountry4) = c("countrycode", "country", "crop", "total_asr")
form_percountry1a = paste0(outDir_asr_percountry, "totalasr_dry_percountry_organised_update_1stdec2023.csv")
# write.csv(total_asr_percountry4, file = form_percountry1a)

# 4.4.2 Total production

# Below commented out as before, because slow to run:
# production_dry_percountry_total = exact_extract(dry_weight_production_maps_global, wrld_simpl4, fun = 'sum', include_area = FALSE, include_cell = FALSE,
#                             include_xy = FALSE, force_df = TRUE, full_colnames = TRUE, stack_apply = TRUE, summarize_df = FALSE, progress = TRUE)
# production_dry_percountry_total
# production_dry_percountry_total$countrycode = wrld_simpl4$ISO3
# production_dry_percountry_total$country = wrld_simpl4$NAME
outDir_production_percountry = "SHEFS/SHEFS_Sept2023_Update/1.1_DryWeight_Production_percountry/"
# production_percountry_outform1 = paste0(outDir_production_percountry, "total_production_per_country_update.csv")
# write.csv(production_dry_percountry_total, file = production_percountry_outform1)

# And again, tidying up the output:
production_dry_percountry_total = read.csv(paste0(outDir_production_percountry, "total_production_per_country_update.csv"))
colnames(production_dry_percountry_total)
production_dry_percountry_total1 = production_dry_percountry_total
colnames(production_dry_percountry_total1) = gsub("sum.dry_weight_production_tonnes", "", colnames(production_dry_percountry_total1), fixed = TRUE)
production_dry_percountry_total2 = data.frame(t(production_dry_percountry_total1[,1:53]))
colnames(production_dry_percountry_total2) = wrld_simpl4$NAME
head(production_dry_percountry_total2)
production_dry_percountry_total3 = data.frame(t(production_dry_percountry_total2))
production_dry_percountry_total3$countrycode = wrld_simpl4$ISO3
production_dry_percountry_total3$country = wrld_simpl4$NAME
production_dry_percountry_total3
production_dry_percountry_total3 = subset(production_dry_percountry_total3, select = -c(X))
production_dry_percountry_total4 = melt(production_dry_percountry_total3, id.vars = c("countrycode", "country"))
head(production_dry_percountry_total4); str(production_dry_percountry_total4)
colnames(production_dry_percountry_total4) = c("countrycode", "country", "crop", "total_production")
form_percountry1aa = paste0(outDir_production_percountry, "total_production_dry_percountry_organised_update.csv")
# write.csv(production_dry_percountry_total4, file = form_percountry1aa)

###################################################################################################################
###################################################################################################################
#### 5. Calculating the consumption-based Biodiversity Pressure.
###################################################################################################################
###################################################################################################################
# This requires the trade data, so is a bit more involved, especially as there are domestic and imported components per focal country.

## UK data:
outdir = "SHEFS/SHEFS_Sept2023_Update/4_TradeData_and_Outputs/"
# write.csv(trade_data_c2000, paste0(outdir, "trade_data_c2000_tidied.csv"))
# Removing these crops as they don't marry up with trade data and create NA issues:
pressure_dry_percountry4 = filter(pressure_dry_percountry4, crop != "greenonion" & crop != "melonetc" & crop!= "pepper"
                                  & crop != "cashewapple" & crop != "stringbean") 
trade_data_c2000 = filter(trade_data_c2000, crop != "greenonion" & crop != "melonetc" & crop!= "pepper"
                          & crop != "cashewapple" & crop != "stringbean") 
# As the UK, South Africa, and India don't have matching issues between wrld_simpl4 and the trade data, I can filter by country names at this point:
uk_bp = filter(pressure_dry_percountry4, country == "United Kingdom") # country, crop, pressure_dry (BP)
# View(uk_bp)

uk_production_minus_exports = filter(trade_data_c2000, reporter == "United Kingdom" & partner == "United Kingdom")
uk_production_minus_exports
total_uk_production_minus_exports = sum(uk_production_minus_exports$dmi_tonnes_average_2000, na.rm = TRUE)
total_uk_production_minus_exports# 2939933
# write.csv(uk_production_minus_exports, paste0(outdir, "uk_production_minus_exports.csv"))

head(uk_bp); head(uk_production_minus_exports)
uk_bp_and_consumption = data.frame(full_join(uk_production_minus_exports, uk_bp, by = "crop"))
uk_bp_and_consumption$pressure_dry
uk_bp_and_consumption$bp_multiplied_by_tonnes_uk = uk_bp_and_consumption$pressure_dry * uk_bp_and_consumption$dmi_tonnes_average_2000
uk_bp_and_consumption$bp_multiplied_by_tonnes_uk # This is BPcons part 1 (domestic component) and can be used for the BPcons panel c of the plots.
# write.csv(uk_bp_and_consumption, paste0(outdir, "uk_bp_and_consumption.csv"))
total_bp_of_uk_consumption = sum(uk_bp_and_consumption$pressure_dry, na.rm = TRUE) # 3257.688
unique(uk_bp_and_consumption$country)

uk_imports = filter(trade_data_c2000, reporter == "United Kingdom" & partner != "United Kingdom")
uk_total_imports = sum(uk_imports$dmi_tonnes_average_2000, na.rm = TRUE)
uk_total_imports # Total imports of all fruits and vegetables analysed # 5758902
uk_total_imports_per_crop = data.frame(uk_imports %>% 
                                         group_by(crop) %>% 
                                         summarise(sum_imports = sum(dmi_tonnes_average_2000, na.rm = TRUE)))
# write.csv(uk_total_imports, paste0(outdir, "uk_total_imports.csv"))
# write.csv(uk_total_imports_per_crop, paste0(outdir, "uk_total_imports_per_crop.csv"))

## Getting the proportion consumed and imported etc. for in text:
(t1 = sum(uk_production_minus_exports$dmi_tonnes_average_2000, na.rm = T))
uk_total_consumption = t1+uk_total_imports
uk_total_consumption
(uk_imports_percent = (uk_total_imports/uk_total_consumption)*100) # 66.20314
(uk_home_percent = (t1/uk_total_consumption)*100) # 33.79686
uk_production_minus_exports
uk_total_imports_per_crop
uk_consumption = full_join(uk_production_minus_exports, uk_total_imports_per_crop, by = "crop")
uk_consumption$uk_total_consumption_per_crop = uk_consumption$dmi_tonnes_average_2000 + uk_consumption$sum_imports
uk_consumption_total = sum(uk_consumption$uk_total_consumption_per_crop, na.rm = TRUE) # 8698838
# write.csv(uk_consumption, paste0(outdir, "uk_consumption.csv"))

head(uk_imports); head(pressure_dry_percountry4) # crop, reporter, partner, dmi_tonnes_average_2000 # country, crop, pressure_dry (BP)
pressure_dry_percountry4$ISO_partner = pressure_dry_percountry4$countrycode # for the purposes of joining up
uk_imports_and_bp = full_join(uk_imports, pressure_dry_percountry4, by = c("crop", "ISO_partner"))
head(uk_imports_and_bp)

uk_imports_and_bp$uk_imports_multiplied_by_BP_of_importpartners = uk_imports_and_bp$pressure_dry * uk_imports_and_bp$dmi_tonnes_average_2000
head(uk_imports_and_bp)
# uk_imports_and_bp$uk_imports_multiplied_by_BP_of_importpartners - This will be BPcons part 2 (import component) and can be used for the BPcons panel c parts of the plots.
# write.csv(uk_imports_and_bp, paste0(outdir, "uk_imports_and_bp.csv"))
(total_uk_import_bp = sum(uk_imports_and_bp$pressure_dry, na.rm = TRUE)) # 2.985185e+24

head(uk_bp_and_consumption) # we want uk_bp_and_consumption$bp_multiplied_by_tonnes_uk
head(uk_imports_and_bp) # and uk_imports_and_bp$uk_imports_multiplied_by_BP_of_importpartners

# To get the BPcons overall, I need to sum per crop the uk_imports_multiplied_by_BP_of_importpartners:
uk_BPcons_import_component_per_crop = uk_imports_and_bp %>% group_by(crop) %>% summarise(uk_imports_multiplied_by_bp_imports_per_crop = sum(uk_imports_multiplied_by_BP_of_importpartners, na.rm = TRUE))
# write.csv(paste0(outdir, uk_imports_and_bp, "uk_imports_and_bp.csv"))
head(uk_BPcons_import_component_per_crop)

BPcons_components_uk = full_join(uk_bp_and_consumption, uk_BPcons_import_component_per_crop, by = "crop")
head(BPcons_components_uk)

BPcons_components_uk$BPcons_UK = BPcons_components_uk$bp_multiplied_by_tonnes_uk + BPcons_components_uk$uk_imports_multiplied_by_bp_imports_per_crop
# BPcons_components_uk$BPcons_UK - this is BPcons, overall, for the UK
head(BPcons_components_uk)
# write.csv(BPcons_components_uk, paste0(outdir, "BPcons_components_uk.csv"))
(total_BPcons_uk = sum(BPcons_components_uk$BPcons_UK, na.rm = TRUE)) # 427552467

uk_exports = filter(trade_data_c2000, reporter != "United Kingdom" & partner == "United Kingdom")
uk_total_exports = sum(uk_exports$dmi_tonnes_average_2000, na.rm = TRUE)      
uk_total_exports # 50359.24
uk_exports_per_crop = uk_exports %>% group_by(crop) %>% 
  summarise(uk_exports_tonnes = sum(dmi_tonnes_average_2000, na.rm = TRUE))
# write.csv(uk_exports, paste0(outdir, "uk_exports.csv"))
# write.csv(uk_exports_per_crop, paste0(outdir, "uk_exports_per_crop.csv"))

## The above script is then repeated for South Africa's data:
pressure_dry_percountry = pressure_dry_percountry4
SouthAfrica_bp = filter(pressure_dry_percountry, country == "South Africa") # country, crop, pressure_dry (BP)
SouthAfrica_production_minus_exports = filter(trade_data_c2000, reporter == "South Africa" & partner == "South Africa")
SouthAfrica_production_minus_exports
# write.csv(SouthAfrica_production_minus_exports, paste0(outdir, "SouthAfrica_production_minus_exports.csv"))
(total_southafrica_productionminusexports = sum(SouthAfrica_production_minus_exports$dmi_tonnes_average_2000, na.rm = TRUE)) # 5319822
head(SouthAfrica_bp); head(SouthAfrica_production_minus_exports)
SouthAfrica_bp_and_consumption = full_join(SouthAfrica_production_minus_exports, SouthAfrica_bp, by = "crop") 
SouthAfrica_bp_and_consumption$bp_multiplied_by_tonnes_SouthAfrica = SouthAfrica_bp_and_consumption$pressure_dry * SouthAfrica_bp_and_consumption$dmi_tonnes_average_2000
SouthAfrica_bp_and_consumption$bp_multiplied_by_tonnes_SouthAfrica # this is BPcons part 1
# write.csv(SouthAfrica_bp_and_consumption, paste0(outdir, "SouthAfrica_bp_and_consumption.csv"))
(total_prodminusexport_bp_southafrica = sum(SouthAfrica_bp_and_consumption$pressure_dry, na.rm = TRUE)) # 14779.04
SouthAfrica_imports = filter(trade_data_c2000, reporter == "South Africa" & partner != "South Africa")
SouthAfrica_total_imports = sum(SouthAfrica_imports$dmi_tonnes_average_2000, na.rm = TRUE)
SouthAfrica_total_imports # total imports of all fruits and vegetables analysed # 32983.66
SouthAfrica_total_imports_per_crop = SouthAfrica_imports %>% group_by(crop) %>%  summarise(SouthAfrica_total_imports_per_crop = sum(dmi_tonnes_average_2000, na.rm = TRUE))
# write.csv(SouthAfrica_total_imports, paste0(outdir, "SouthAfrica_total_imports.csv"))
# write.csv(SouthAfrica_total_imports_per_crop, paste0(outdir, "SouthAfrica_total_imports_per_crop.csv"))
SouthAfrica_production_minus_exports
SouthAfrica_total_imports_per_crop
SouthAfrica_consumption = full_join(SouthAfrica_production_minus_exports, SouthAfrica_total_imports_per_crop, by = "crop")
SouthAfrica_consumption$SouthAfrica_total_consumption_per_crop = SouthAfrica_consumption$dmi_tonnes_average_2000 + SouthAfrica_consumption$SouthAfrica_total_imports_per_crop
# write.csv(SouthAfrica_consumption, paste0(outdir, "SouthAfrica_consumption.csv"))
(southafrica_total_consumption = sum(SouthAfrica_consumption$SouthAfrica_total_consumption_per_crop, na.rm = TRUE)) # 5352806
head(SouthAfrica_imports); head(pressure_dry_percountry) # crop, reporter, partner, dmi_tonnes_average_2000 # country, crop, pressure_dry (BP)
SouthAfrica_imports_and_bp = full_join(SouthAfrica_imports, pressure_dry_percountry, by = c("crop", "ISO_partner"))
head(SouthAfrica_imports_and_bp) 
SouthAfrica_imports_and_bp$SouthAfrica_imports_multiplied_by_BP_of_importpartners = SouthAfrica_imports_and_bp$pressure_dry * SouthAfrica_imports_and_bp$dmi_tonnes_average_2000
head(SouthAfrica_imports_and_bp)
# write.csv(SouthAfrica_imports_and_bp, paste0(outdir, "SouthAfrica_imports_and_bp.csv"))
(total_bp_of_imports_southafrica = sum(SouthAfrica_imports_and_bp$pressure_dry, na.rm = TRUE)) #  2.985185e+24
# SouthAfrica_imports_and_bp$SouthAfrica_imports_multiplied_by_BP_of_importpartners - this will be BPcons part 2
head(SouthAfrica_bp_and_consumption) # we want SouthAfrica_bp_and_consumption$bp_multiplied_by_tonnes_SouthAfrica
head(SouthAfrica_imports_and_bp) # and SouthAfrica_imports_and_bp$SouthAfrica_imports_multiplied_by_BP_of_importpartners
# To get the BPcons overall, I need to sum per crop the SouthAfrica_imports_multiplied_by_BP_of_importpartners
SouthAfrica_BPcons_import_component_per_crop = SouthAfrica_imports_and_bp %>% 
  group_by(crop) %>% 
  summarise(SouthAfrica_imports_multiplied_by_bp_imports_per_crop = sum(SouthAfrica_imports_multiplied_by_BP_of_importpartners, na.rm = TRUE))
head(SouthAfrica_BPcons_import_component_per_crop)

BPcons_components_SouthAfrica = full_join(SouthAfrica_bp_and_consumption, SouthAfrica_BPcons_import_component_per_crop, by = "crop")
head(BPcons_components_SouthAfrica)
BPcons_components_SouthAfrica$BPcons_SouthAfrica = BPcons_components_SouthAfrica$bp_multiplied_by_tonnes_SouthAfrica + BPcons_components_SouthAfrica$SouthAfrica_imports_multiplied_by_bp_imports_per_crop
# BPcons_components_SouthAfrica$BPcons_SouthAfrica - this is BPcons for the SouthAfrica
# write.csv(BPcons_components_SouthAfrica, paste0(outdir, "BPcons_components_SouthAfrica.csv"))
totalBPcons_southafrica = sum(BPcons_components_SouthAfrica$BPcons_SouthAfrica, na.rm = TRUE) 
totalBPcons_southafrica
SouthAfrica_exports = filter(trade_data_c2000, reporter != "South Africa" & partner == "South Africa")
SouthAfrica_total_exports = sum(SouthAfrica_exports$dmi_tonnes_average_2000, na.rm = TRUE)      
SouthAfrica_total_exports # 1790516
SouthAfrica_exports_per_crop = SouthAfrica_exports %>% group_by(crop) %>% 
  summarise(SouthAfrica_exports_tonnes = sum(dmi_tonnes_average_2000, na.rm = TRUE))
# write.csv(SouthAfrica_exports, paste0(outdir, "SouthAfrica_exports.csv"))
# write.csv(SouthAfrica_exports_per_crop, paste0(outdir, "SouthAfrica_exports_per_crop.csv"))
(t2 = sum(SouthAfrica_production_minus_exports$dmi_tonnes_average_2000, na.rm = T))
sa_total_consumption = t2+SouthAfrica_total_imports
sa_total_consumption # 5352806
(sa_imports_percent = (SouthAfrica_total_imports/sa_total_consumption)*100) # 0.62
(sa_home_percent = (t2/sa_total_consumption)*100) # 99.38%

## The above script is then repeated for India's data:
India_bp = filter(pressure_dry_percountry, country == "India") # country, crop, pressure_dry (BP)
India_production_minus_exports = filter(trade_data_c2000, reporter == "India" & partner == "India")
India_production_minus_exports
# write.csv(India_production_minus_exports, paste0(outdir, "India_production_minus_exports.csv"))
(india_production_minus_exports_sum = sum(India_production_minus_exports$dmi_tonnes_average_2000, na.rm = TRUE)) #76842306
head(India_bp); head(India_production_minus_exports)
India_bp_and_consumption = full_join(India_production_minus_exports, India_bp, by = "crop") 
head(India_bp_and_consumption)
India_bp_and_consumption$bp_multiplied_by_tonnes_India = India_bp_and_consumption$pressure_dry * India_bp_and_consumption$dmi_tonnes_average_2000
India_bp_and_consumption$bp_multiplied_by_tonnes_India # this is BPcons part 1 and can be used for the weighted BP plot
# write.csv(India_bp_and_consumption, paste0(outdir, "India_bp_and_consumption.csv"))
(bp_of_production_minus_exports_india = sum(India_bp_and_consumption$pressure_dry, na.rm = T)) # 13860.33
India_imports = filter(trade_data_c2000, reporter == "India" & partner != "India")
India_total_imports = sum(India_imports$dmi_tonnes_average_2000)
India_total_imports # total imports of all fruits and vegetables analysed # 302558.4
India_total_imports_per_crop = India_imports %>% group_by(crop) %>%  summarise(India_total_imports_per_crop = sum(dmi_tonnes_average_2000, na.rm = TRUE))
# write.csv(India_total_imports, paste0(outdir, "India_total_imports.csv"))
# write.csv(India_total_imports_per_crop, paste0(outdir, "India_total_imports_per_crop.csv"))
India_production_minus_exports 
India_total_imports_per_crop
India_consumption = full_join(India_production_minus_exports, India_total_imports_per_crop, by = "crop")
India_consumption$India_total_consumption_per_crop = India_consumption$dmi_tonnes_average_2000 + India_consumption$India_total_imports_per_crop
# write.csv(India_consumption, paste0(outdir, "India_consumption.csv"))
(india_total_consumption = sum(India_consumption$India_total_consumption_per_crop, na.rm = TRUE)) # 77144863
head(India_imports); head(pressure_dry_percountry) # crop, reporter, partner, dmi_tonnes_average_2000 # country, crop, pressure_dry (BP)
India_imports_and_bp = full_join(India_imports, pressure_dry_percountry, by = c("crop", "ISO_partner"))
head(India_imports_and_bp) #
India_imports_and_bp$India_imports_multiplied_by_BP_of_importpartners = India_imports_and_bp$pressure_dry * India_imports_and_bp$dmi_tonnes_average_2000
head(India_imports_and_bp)
# India_imports_and_bp$India_imports_multiplied_by_BP_of_importpartners - this will be BPcons part 2 
# write.csv(India_imports_and_bp, paste0(outdir, "India_imports_and_bp.csv"))
(india_total_bp_of_imports = sum(India_imports_and_bp$pressure_dry, na.rm = TRUE)) # 2.985185e+24
head(India_bp_and_consumption) # we want India_bp_and_consumption$bp_multiplied_by_tonnes_India
head(India_imports_and_bp) # and India_imports_and_bp$India_imports_multiplied_by_BP_of_importpartners
# To get the BPcons overall, I need to sum per crop the India_imports_multiplied_by_BP_of_importpartners
India_BPcons_import_component_per_crop = India_imports_and_bp %>% group_by(crop) %>% summarise(India_imports_multiplied_by_bp_imports_per_crop = sum(India_imports_multiplied_by_BP_of_importpartners, na.rm = TRUE))
head(India_BPcons_import_component_per_crop)
BPcons_components_India = full_join(India_bp_and_consumption, India_BPcons_import_component_per_crop, by = "crop")
head(BPcons_components_India)
BPcons_components_India$BPcons_India = BPcons_components_India$bp_multiplied_by_tonnes_India + BPcons_components_India$India_imports_multiplied_by_bp_imports_per_crop
# BPcons_components_India$BPcons_India - this is BPcons for the India
# write.csv(BPcons_components_India, paste0(outdir, "BPcons_components_India.csv"))
(total_bpcons_india = sum(BPcons_components_India$BPcons_India, na.rm = TRUE)) # 1.644275e+17
India_exports = filter(trade_data_c2000, reporter != "India" & partner == "India")
India_total_exports = sum(India_exports$dmi_tonnes_average_2000, na.rm = TRUE)      
India_total_exports # 486092.1
India_exports_per_crop = India_exports %>% group_by(crop) %>% 
  summarise(India_exports_tonnes = sum(dmi_tonnes_average_2000, na.rm = TRUE))
# write.csv(India_exports, paste0(outdir, "India_exports.csv"))
# write.csv(India_exports_per_crop, paste0(outdir, "India_exports_per_crop.csv"))
(t3 = sum(India_production_minus_exports$dmi_tonnes_average_2000, na.rm = T))
india_total_consumption = t3+India_total_imports
india_total_consumption # 77144864
(india_imports_percent = (India_total_imports/india_total_consumption)*100) # 0.39
(india_home_percent = (t3/india_total_consumption)*100) # 99.6

###################################################################################################################
###################################################################################################################
## 6. Extracting the relevant trade data for post-2003 trends.
###################################################################################################################
###################################################################################################################

# For the figure showing the trade changes since 2003, I need to pull off the trade averages for 2003-present.
## I know, from other work, that there are anomalies in the data for 2018 and 2019, so although these years are available,
## I will not use them, as the data were perhaps incomplete at time of recording.

## As before, as the datasets aren't mine to share, I include basic scripting below. I brought in csv files, subset
## for the years and crops of interest, and then read them in as a single file, as listed below.

outdir_old = "SHEFS/OLD/SHEFS_March2023_Update/19July2023/"
df_2 = read.csv(paste0(outdir_old,"trade_data_all_28082023.csv")) # This is the one that isn't just filtered to 1997-2003.
head(df_2)
tradedatapost2003 = subset(df_2, select = -c(X))
head(tradedatapost2003)
colnames(tradedatapost2003) = c("reporter", "partner", "dmi_tonnes", "crop", "year")
trade_data_post2003 = tradedatapost2003 %>%
  group_by(crop, reporter, partner) %>%
  summarise(dmi_tonnes_average_1997onwards = mean(dmi_tonnes, na.rm = TRUE))

head(trade_data_post2003)
countrylist = data.frame(unique(trade_data_post2003$reporter))
colnames(countrylist) = "country"
countrylist2 = data.frame(unique(trade_data_post2003$partner))
colnames(countrylist2) = "country"
countrylist3 = rbind(countrylist, countrylist2)
head(countrylist3)
countrylist4 = data.frame(unique(countrylist3$country))
colnames(countrylist4) = "NAME"
worldnames = data.frame(wrld_simpl4)
head(worldnames, n = 5)
worldnames_short = subset(worldnames, select = c("NAME", "ISO3"))
head(worldnames_short)
testnames = full_join(countrylist4, worldnames_short)
testnames # This contains the reporters and partners from the trade data with ISO codes and has NAs for the ones I need to match.
# View(worldnames_short)
outdir = "SHEFS/SHEFS_Sept2023_Update/4_TradeData_and_Outputs/"
# write.csv(testnames, paste0(outdir, "tradenamematching_post2003.csv")) # - Exporting to try and fix using the worldnames from wrld_simpl.
namematches = read.csv(paste0(outdir, "tradenamematching_post2003_FILLED.csv"))
head(namematches)
# Next, as before, we want to find a way to replace the names in the reporter and partner columns of trade data with the ISO codes in namematches,
# noting that there are a couple of NAs where the wrld_simpl map does have some out of date countries included.
head(trade_data_post2003)
trade_data_post2003$NAME = trade_data_post2003$reporter
head(trade_data_post2003)
trade_data_test = trade_data_post2003
trade_data_test = left_join(trade_data_test, namematches, by = "NAME")
head(trade_data_test)
sum(is.na(trade_data_test$ISO3))
(na_rows = trade_data_test[!complete.cases(trade_data_test),]) # This is OK as it's Serbia and Montenegro, USSR, and Yugoslavia.
sum(na_rows$dmi_tonnes_average_1997onwards) # 329917 tonnes total globally across all partners
trade_data1 = subset(trade_data_test, select = -c(NAME, X))
trade_data1$NAME = trade_data1$partner
trade_data2 = left_join(trade_data1, namematches, by = "NAME")
head(trade_data2)
(na_rows1 = trade_data2[!complete.cases(trade_data2$ISO3.y),]) # Again USSR, Yugoslav SFR and Serbia & Montenegro
na_rows1
sum(na_rows1$dmi_tonnes_average_1997onwards) # 198 tonnes
trade_data_post2003 = subset(trade_data2, select = -c(X, NAME))
colnames(trade_data_post2003) = c("crop", "reporter", "partner", "dmi_tonnes_average_1997onwards",
                                  "ISO_reporter", "ISO_partner")
head(trade_data_post2003)