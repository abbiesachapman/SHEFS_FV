##
## Date: 11th December 2023
## Author: Abbie S. A. Chapman
## Title: R script to accompany 'Quantifying the biodiversity pressures of fruit and vegetable consumption in the 
## United Kingdom, India, and South Africa.'
##

## 2_Calculating_Biodiversity_Pressure

#####
# 1. Calculating Biodiversity Pressure
# This is: species richness * harvested area / production,
# and Affected Species Range is computed and stored as an early step in this calculation.

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

## (Checks folded below)
#####
# test1 = fnv_harvestedarea_global[[1]]
# summary(test1) # min 0, median 0, max 1015.929, NA 0 (based on a summary of cells)
# test1; max(values(test1$apple)) # 2720.231 is the actual max
# test1[test1 == 0] <- NA # we do this because, if there is no harvested area, we can't then just call the pressure species * 0 / production (presumably also zero)
# summary(test1) # min now very small, max 2.72e+03, NAs many
# test2 = mask(vertebrates_projected, test1)
# test2
# test3 = test1 * vertebrates_projected
# test3
# test_prod = dry_weight_production_maps_global[[1]]
# test_prod
# test_prod[test_prod == 0] <- NA 
# summary(test_prod) # same no of nas as harvested area - good :)
# testout = test3/test_prod
# testout

#####

# Note that this is slow to run:
pressure_calculating =  pressure_fun(ras_stack2 = fnv_harvestedarea_global)

pressuremaps = paste(outDir_pressure, dir(path = outDir_pressure, recursive = TRUE), sep = "")
pressuremaps = pressuremaps[!grepl(".ovr", pressuremaps)]
pressure_dry = stack(pressuremaps)
asrmaps = paste(outDir_ASR, dir(path = outDir_ASR, recursive = TRUE), sep = "")
asrmaps = asrmaps[!grepl(".ovr", asrmaps)]
asr = stack(asrmaps)

#####
# 2. Extracting the values per country. (Note that this is slow to run.)

pressure_dry_percountry = exact_extract(pressure_dry, wrld_simpl4, fun = 'weighted_mean', weights = dry_weight_production_maps_global, include_area = FALSE, include_cell = FALSE,
include_xy = FALSE, force_df = TRUE, full_colnames = TRUE, stack_apply = TRUE, summarize_df = FALSE, progress = TRUE)
pressure_dry_percountry
pressure_dry_percountry$countrycode = wrld_simpl4$ISO3
pressure_dry_percountry$country = wrld_simpl4$NAME

# Below, I read in the output (to save running the slow code every time) and tidy up the data frame:
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

# I can also calculate the total pressure across all fruits and vegetables (per country) as follows:
pressure_dry_percountry_sums = pressure_dry_percountry4 %>% 
  group_by(countrycode) %>% 
  summarise(sum_fv  = sum(pressure_dry, na.rm = TRUE))
form_percountry1b = paste0(outDir_pressure_percountry, "pressure_dry_percountry_fvsums_organised_update_1stdec2023.csv")
# write.csv(pressure_dry_percountry_sums, file = form_percountry1b)
# I checked these manually and the ones which didn't marry up with excel made sense as they were the ones excel wasn't reading correctly as the scientific notation was being read as text!

# I repeat the process above for Affected Species Range data as follows:
asr_dry_percountry = exact_extract(asr, wrld_simpl4, fun = 'weighted_mean', weights = dry_weight_production_maps_global, include_area = FALSE, include_cell = FALSE,
include_xy = FALSE, force_df = TRUE, full_colnames = TRUE, stack_apply = TRUE, summarize_df = FALSE, progress = TRUE)
asr_dry_percountry
asr_dry_percountry$countrycode = wrld_simpl4$ISO3
asr_dry_percountry$country = wrld_simpl4$NAME
outDir_asr_percountry = "SHEFS/SHEFS_Sept2023_Update/2.1_ASR_percountry/"
form_percountry2 = paste0(outDir_asr_percountry, "asr_per_country_update_1stdec2023.csv")
# write.csv(asr_dry_percountry, file = form_percountry2)
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
asr_percountry_sums = asr_dry_percountry4 %>% 
  group_by(countrycode) %>% 
  summarise(sum_fv  = sum(asr, na.rm = TRUE))
form_percountry1c = paste0(outDir_asr_percountry, "asr_percountry_fvsums_organised_update_1stdec2023.csv")
# write.csv(asr_percountry_sums, file = form_percountry1c)

#####
# 3. Extracting the TOTAL values per country. (Note that this is slow to run.)

asr_dry_percountry_total = exact_extract(asr, wrld_simpl4, fun = 'sum', include_area = FALSE, include_cell = FALSE,
include_xy = FALSE, force_df = TRUE, full_colnames = TRUE, stack_apply = TRUE, summarize_df = FALSE, progress = TRUE)
asr_dry_percountry_total
asr_dry_percountry_total$countrycode = wrld_simpl4$ISO3
asr_dry_percountry_total$country = wrld_simpl4$NAME

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

