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

## Script Part 3 (run parts 1 and 2 first).

###################################################################################################################
###################################################################################################################
# 7. Identifying and tidying the data the figures will be pulling from.
###################################################################################################################
###################################################################################################################

# Weighted BP, local component (BPcons part 1):  Focalcountry_bp_and_consumption$bp_multiplied_by_tonnes_Focalcountry
head(uk_bp_and_consumption) # These data are outputs from the previous scripts, so the previous scripts need running first.
colnames(uk_bp_and_consumption) = c("crop", "reporter", "partner", "dmi_tonnes_average_2000",
                                    "ISO_reporter", "ISO_partner",
                                    "countrycode", "country", "pressure_dry",
                                    "bp_multiplied_by_tonnes") # tidying up the labelling
head(SouthAfrica_bp_and_consumption)
colnames(SouthAfrica_bp_and_consumption)
SouthAfrica_bp_and_consumption = subset(SouthAfrica_bp_and_consumption, select = -c(ISO_partner.y))
colnames(SouthAfrica_bp_and_consumption) = c("crop", "reporter", "partner", "dmi_tonnes_average_2000",
                                             "ISO_reporter", "ISO_partner",
                                             "countrycode", "country", 
                                             "pressure_dry",
                                             "bp_multiplied_by_tonnes")
head(India_bp_and_consumption)
India_bp_and_consumption = subset(India_bp_and_consumption, select = -c(ISO_partner.y))
colnames(India_bp_and_consumption) = c("crop", "reporter", "partner", "dmi_tonnes_average_2000",
                                       "ISO_reporter", "ISO_partner",
                                       "countrycode", "country", 
                                       "pressure_dry",
                                       "bp_multiplied_by_tonnes")
all_country_bp_and_consumption = rbind(uk_bp_and_consumption, SouthAfrica_bp_and_consumption, India_bp_and_consumption) # bringing data together from all 3 focal countries

outdir = "SHEFS/SHEFS_Sept2023_Update/5_FigureData_and_Figs/"

head(all_country_bp_and_consumption) # bp_multiplied_by_tonnes is still the BPcons part 1, weighted BP local component
# write.csv(all_country_bp_and_consumption, paste0(outdir, "all_country_bp_and_consumption.csv"))

max1 = all_country_bp_and_consumption[which.max(all_country_bp_and_consumption$pressure_dry),]
max1 
min_asparagus = filter(all_country_bp_and_consumption, crop == "asparagus")
min_asparagus 
max2 = all_country_bp_and_consumption[which.max(all_country_bp_and_consumption$dmi_tonnes_average_2000),]
max2 
min_banana = filter(all_country_bp_and_consumption, crop == "banana")
min_banana 

# Weighted BP, imported component (BPcons part 2): Focalcountry_imports_and_bp$Focalcountry_imports_multiplied_by_BP_of_importpartners 
head(uk_imports_and_bp)
colnames(uk_imports_and_bp) = c("crop", "reporter", "partner", "dmi_tonnes_average_2000",
                                "ISO_reporter", "ISO_partner",
                                "countrycode", "country", "pressure_dry",
                                "imports_multiplied_by_BP_of_importpartners")
uk_imports_and_bp_summary = data.frame(uk_imports_and_bp %>% 
                                         group_by(ISO_partner) %>% 
                                         summarise(sum_pressure_dry = sum(imports_multiplied_by_BP_of_importpartners, na.rm = TRUE)))


head(SouthAfrica_imports_and_bp)
colnames(SouthAfrica_imports_and_bp) = c("crop", "reporter", "partner", "dmi_tonnes_average_2000",
                                         "ISO_reporter", "ISO_partner",
                                         "countrycode","country", "pressure_dry",
                                         "imports_multiplied_by_BP_of_importpartners")
SouthAfrica_imports_and_bp_summary = data.frame(SouthAfrica_imports_and_bp %>% 
                                                  group_by(ISO_partner) %>% 
                                                  summarise(sum_pressure_dry = sum(imports_multiplied_by_BP_of_importpartners, na.rm = TRUE)))

head(India_imports_and_bp)
colnames(India_imports_and_bp) = c("crop", "reporter", "partner", "dmi_tonnes_average_2000",
                                   "ISO_reporter", "ISO_partner",
                                   "countrycode", "country", "pressure_dry",
                                   "imports_multiplied_by_BP_of_importpartners")

India_imports_and_bp_summary = data.frame(India_imports_and_bp %>% 
                                            group_by(ISO_partner) %>% 
                                            summarise(sum_pressure_dry = sum(imports_multiplied_by_BP_of_importpartners, na.rm = TRUE)))

all_country_imports_and_bp = rbind(uk_imports_and_bp, SouthAfrica_imports_and_bp, India_imports_and_bp)
head(all_country_imports_and_bp) # imports_multiplied_by_BP_of_importpartners is still the BPcons part 2, weighted BP import component
# write.csv(all_country_imports_and_bp, paste0(outdir, "all_country_imports_and_bp_1stdec.csv"))
all_country_imports_and_bp1 = filter(all_country_imports_and_bp, dmi_tonnes_average_2000 != 0)
max3 = all_country_imports_and_bp1[which.max(all_country_imports_and_bp1$pressure_dry),]
max3 
min_sourcherry = data.frame(filter(all_country_imports_and_bp1, crop == "sourcherry"))
min_sourcherry = min_sourcherry[which.min(min_sourcherry$pressure_dry),]
min_sourcherry 
max4 = all_country_imports_and_bp1[which.max(all_country_imports_and_bp1$dmi_tonnes_average_2000),]
max4 
min_tomatoes = filter(all_country_imports_and_bp, crop == "tomato" & dmi_tonnes_average_2000 >0)
min_tomatoes =  min_tomatoes[which.min(min_tomatoes$pressure_dry),]
min_tomatoes 
min1 = filter(all_country_imports_and_bp, dmi_tonnes_average_2000 >0 & pressure_dry >0)
min1 = min1[which.min(min1$pressure_dry),]
min1 

# BPcons: BPcons_components_Focalcountry$BPcons_Focalcountry
head(BPcons_components_uk)
colnames(BPcons_components_uk) = c("crop", "reporter", "partner", "dmi_tonnes_average_2000",
                                   "ISO_reporter", "ISO_partner",
                                   "countrycode", "country", "pressure_dry",
                                   "bp_multiplied_by_tonnes", "imports_multiplied_by_bp_imports_per_crop",
                                   "BPcons")
head(BPcons_components_SouthAfrica)
BPcons_components_SouthAfrica = subset(BPcons_components_SouthAfrica, select = -c(ISO_partner.y))
colnames(BPcons_components_SouthAfrica) = c("crop", "reporter", "partner", "dmi_tonnes_average_2000", 
                                            "ISO_reporter", "ISO_partner",
                                            "countrycode", "country", "pressure_dry",
                                            "bp_multiplied_by_tonnes", "imports_multiplied_by_bp_imports_per_crop",
                                            "BPcons")
head(BPcons_components_India)
BPcons_components_India = subset(BPcons_components_India, select = -c(ISO_partner.y))
colnames(BPcons_components_India) = c("crop", "reporter", "partner", "dmi_tonnes_average_2000",
                                      "ISO_reporter", "ISO_partner",
                                      "countrycode",  "country", "pressure_dry",
                                      "bp_multiplied_by_tonnes", "imports_multiplied_by_bp_imports_per_crop",
                                      "BPcons")
all_country_bpcons = rbind(BPcons_components_uk, BPcons_components_SouthAfrica, BPcons_components_India)
head(all_country_bpcons) # BPcons is still as needed
# write.csv(all_country_bpcons, paste0(outdir, "all_country_bpcons_1stdec.csv"))
maxdata = filter(all_country_bpcons, dmi_tonnes_average_2000>0)
max5 = maxdata[which.max(maxdata$pressure_dry),]
max5 
max5a = maxdata[which.max(maxdata$BPcons),]
max5a 
min_asparagus = filter(maxdata, crop == "asparagus")
min_asparagus = min_asparagus[which.min(min_asparagus$pressure_dry),]
min_asparagus 
min_date = filter(maxdata, crop == "date")
min_date = min_date[which.min(min_date$BPcons),]
min_date 

# Local consumption: focalcountry_bp_and_consumption$dmi_tonnes_average_2000
all_country_bp_and_consumption # use the dmi_tonnes_average_2000 from this now for local

###########
# What's the range of values for each index per country?
uk_extract = filter(all_country_bp_and_consumption, countrycode == "GBR")
head(uk_extract)
india_extract = filter(all_country_bp_and_consumption, countrycode == "IND")
head(india_extract)
southafrica_extract = filter(all_country_bp_and_consumption, countrycode == "ZAF")
head(southafrica_extract)

(uk_min_domestic_consumption = min(uk_extract$dmi_tonnes_average_2000, na.rm = TRUE)) # 0.7185045
(uk_min_domestic_BP = min(uk_extract$pressure_dry, na.rm = TRUE)) # 1.536106e-18
(uk_min_domestic_BPcons = min(uk_extract$bp_multiplied_by_tonnes, na.rm = TRUE)) # 1.536106e-18
(uk_max_domestic_consumption = max(uk_extract$dmi_tonnes_average_2000, na.rm = TRUE)) # 776208.2
(uk_max_domestic_BP = max(uk_extract$pressure_dry, na.rm = TRUE)) # 1007.039
(uk_max_domestic_BPcons = max(uk_extract$bp_multiplied_by_tonnes, na.rm = TRUE)) # 31716422
(uk_range_domestic_consumption = uk_max_domestic_consumption - uk_min_domestic_consumption) # 776207.4
(uk_range_domestic_BP = uk_max_domestic_BP - uk_min_domestic_BP) # 1007.039
(uk_range_domestic_BPcons = uk_max_domestic_BPcons - uk_min_domestic_BPcons) # 31716422

(india_min_domestic_consumption = min(india_extract$dmi_tonnes_average_2000, na.rm = TRUE)) # 0.6978195
(india_min_domestic_BP = min(india_extract$pressure_dry, na.rm = TRUE)) # 3.357107e-19
(india_min_domestic_BPcons = min(india_extract$bp_multiplied_by_tonnes, na.rm = TRUE)) # 3.357107e-19
(india_max_domestic_consumption = max(india_extract$dmi_tonnes_average_2000, na.rm = TRUE)) # 14391439
(india_max_domestic_BP = max(india_extract$pressure_dry, na.rm = TRUE)) # 1228.051
(india_max_domestic_BPcons = max(india_extract$bp_multiplied_by_tonnes, na.rm = TRUE)) # 2569450540
(india_range_domestic_consumption = india_max_domestic_consumption - india_min_domestic_consumption) # 14391439
(india_range_domestic_BP = india_max_domestic_BP - india_min_domestic_BP) # 1228.051
(india_range_domestic_BPcons = india_max_domestic_BPcons - india_min_domestic_BPcons) # 2569450540

(southafrica_min_domestic_consumption = min(southafrica_extract$dmi_tonnes_average_2000, na.rm = TRUE)) # 0.4700108
(southafrica_min_domestic_BP = min(southafrica_extract$pressure_dry, na.rm = TRUE)) # 2.15632e-18
(southafrica_min_domestic_BPcons = min(southafrica_extract$bp_multiplied_by_tonnes, na.rm = TRUE)) # 2.15632e-18
(southafrica_max_domestic_consumption = max(southafrica_extract$dmi_tonnes_average_2000, na.rm = TRUE)) # 1203239
(southafrica_max_domestic_BP = max(southafrica_extract$pressure_dry, na.rm = TRUE)) # 2740.76
(southafrica_max_domestic_BPcons = max(southafrica_extract$bp_multiplied_by_tonnes, na.rm = TRUE)) # 149833574
(southafrica_range_domestic_consumption = southafrica_max_domestic_consumption - southafrica_min_domestic_consumption) # 1203239
(southafrica_range_domestic_BP = southafrica_max_domestic_BP - southafrica_min_domestic_BP) # 2740.76
(southafrica_range_domestic_BPcons = southafrica_max_domestic_BPcons - southafrica_min_domestic_BPcons) # 149833574


# Imported consumption: focalcountry_imports_and_bp$dmi_tonnes_average_2000
all_country_imports_and_bp # use the dmi_tonnes_average_2000 from this now for imported
head(all_country_imports_and_bp)

uk_extract_imported = filter(all_country_imports_and_bp, reporter == "United Kingdom")
head(uk_extract_imported)
india_extract_imported = filter(all_country_imports_and_bp, reporter == "India")
head(india_extract_imported)
southafrica_extract_imported = filter(all_country_imports_and_bp, reporter == "South Africa")
head(southafrica_extract_imported)

(uk_min_imported_consumption = min(uk_extract_imported$dmi_tonnes_average_2000, na.rm = TRUE)) # -9.820298e-17
(uk_min_imported_BP = min(uk_extract_imported$pressure_dry, na.rm = TRUE)) # 8.194631e-20
(uk_min_imported_BPcons = min(uk_extract_imported$imports_multiplied_by_BP_of_importpartners , na.rm = TRUE)) # -1.335942e-15
(uk_max_imported_consumption = max(uk_extract_imported$dmi_tonnes_average_2000, na.rm = TRUE)) # 513418.7
(uk_max_imported_BP = max(uk_extract_imported$pressure_dry, na.rm = TRUE)) # 8.342407e+23
(uk_max_imported_BPcons = max(uk_extract_imported$imports_multiplied_by_BP_of_importpartners , na.rm = TRUE)) # 1.554207e+23
(uk_range_imported_consumption = uk_max_imported_consumption - uk_min_imported_consumption) # 513418.7
(uk_range_imported_BP = uk_max_imported_BP - uk_min_imported_BP) # 8.342407e+23
(uk_range_imported_BPcons = uk_max_imported_BPcons - uk_min_imported_BPcons) # 1.554207e+23

(india_min_imported_consumption = min(india_extract_imported$dmi_tonnes_average_2000, na.rm = TRUE)) # -2.131905e-15
(india_min_imported_BP = min(india_extract_imported$pressure_dry, na.rm = TRUE)) # 8.194631e-20
(india_min_imported_BPcons = min(india_extract_imported$imports_multiplied_by_BP_of_importpartners , na.rm = TRUE)) # -3.066594e-13
(india_max_imported_consumption = max(india_extract_imported$dmi_tonnes_average_2000, na.rm = TRUE)) # 140133
(india_max_imported_BP = max(india_extract_imported$pressure_dry, na.rm = TRUE)) # 8.342407e+23
(india_max_imported_BPcons = max(india_extract_imported$imports_multiplied_by_BP_of_importpartners , na.rm = TRUE)) # 1.644275e+17
(india_range_imported_consumption = india_max_imported_consumption - india_min_imported_consumption) # 140133
(india_range_imported_BP = india_max_imported_BP - india_min_imported_BP) # 8.342407e+23
(india_range_imported_BPcons = india_max_imported_BPcons - india_min_imported_BPcons) # 1.644275e+17

(southafrica_min_imported_consumption = min(southafrica_extract_imported$dmi_tonnes_average_2000, na.rm = TRUE)) # -1.723949e-18
(southafrica_min_imported_BP = min(southafrica_extract_imported$pressure_dry, na.rm = TRUE)) # 8.194631e-20-18
(southafrica_min_imported_BPcons = min(southafrica_extract_imported$imports_multiplied_by_BP_of_importpartners , na.rm = TRUE)) # -1.662012e-18
(southafrica_max_imported_consumption = max(southafrica_extract_imported$dmi_tonnes_average_2000, na.rm = TRUE)) # 8577.851
(southafrica_max_imported_BP = max(southafrica_extract_imported$pressure_dry, na.rm = TRUE)) # 8.342407e+23
(southafrica_max_imported_BPcons = max(southafrica_extract_imported$imports_multiplied_by_BP_of_importpartners , na.rm = TRUE)) #  3.281807e+16
(southafrica_range_imported_consumption = southafrica_max_imported_consumption - southafrica_min_imported_consumption) # 8577.851
(southafrica_range_imported_BP = southafrica_max_imported_BP - southafrica_min_imported_BP) # 8.342407e+23
(southafrica_range_imported_BPcons = southafrica_max_imported_BPcons - southafrica_min_imported_BPcons) # 3.281807e+16


### I need a column defining whether fruit or vegetable, for plotting:
unique(BPcons_components_uk$crop)
fruitveg = data.frame(unique(BPcons_components_uk$crop))
colnames(fruitveg) = c("crop")
fruitveg$fruitveg = fruitveg$crop
head(fruitveg)
fruitveg$fruitveg[fruitveg$fruitveg == "apple"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "apricot"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "artichoke"] <- "veg"
fruitveg$fruitveg[fruitveg$fruitveg == "asparagus"] <- "veg"
fruitveg$fruitveg[fruitveg$fruitveg == "avocado"] <- "veg"
fruitveg$fruitveg[fruitveg$fruitveg == "banana"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "blueberry"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "cabbage"] <- "veg"
fruitveg$fruitveg[fruitveg$fruitveg == "carob"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "carrot"] <- "veg"
# fruitveg$fruitveg[fruitveg$fruitveg == "cashewapple"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "cauliflower"] <- "veg"
fruitveg$fruitveg[fruitveg$fruitveg == "cherry"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "chilleetc"] <- "veg"
fruitveg$fruitveg[fruitveg$fruitveg == "cranberry"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "cucumberetc"] <- "veg"
fruitveg$fruitveg[fruitveg$fruitveg == "currant"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "date"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "eggplant"] <- "veg"
fruitveg$fruitveg[fruitveg$fruitveg == "fig"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "garlic"] <- "veg"
fruitveg$fruitveg[fruitveg$fruitveg == "gooseberry"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "grape"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "grapefruitetc"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "greenbean"] <- "veg"
fruitveg$fruitveg[fruitveg$fruitveg == "greencorn"] <- "veg"
# fruitveg$fruitveg[fruitveg$fruitveg == "greenonion"] <- "veg"
fruitveg$fruitveg[fruitveg$fruitveg == "greenpea"] <- "veg"
fruitveg$fruitveg[fruitveg$fruitveg == "kiwi"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "lemonlime"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "lettuce"] <- "veg"
fruitveg$fruitveg[fruitveg$fruitveg == "mango"] <- "fruit"
# fruitveg$fruitveg[fruitveg$fruitveg == "melonetc"] <- "veg"
fruitveg$fruitveg[fruitveg$fruitveg == "mushroom"] <- "veg"
fruitveg$fruitveg[fruitveg$fruitveg == "okra"] <- "veg"
fruitveg$fruitveg[fruitveg$fruitveg == "onion"] <- "veg"
fruitveg$fruitveg[fruitveg$fruitveg == "orange"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "papaya"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "peachetc"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "pear"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "persimmon"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "pineapple"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "plantain"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "plum"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "pumpkinetc"] <- "veg"
fruitveg$fruitveg[fruitveg$fruitveg == "quince"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "rasberry"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "sourcherry"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "spinach"] <- "veg"
fruitveg$fruitveg[fruitveg$fruitveg == "strawberry"] <- "fruit"
# fruitveg$fruitveg[fruitveg$fruitveg == "stringbean"] <- "veg"
fruitveg$fruitveg[fruitveg$fruitveg == "tangetc"] <- "fruit"
fruitveg$fruitveg[fruitveg$fruitveg == "tomato"] <- "veg"
fruitveg$fruitveg[fruitveg$fruitveg == "watermelon"] <- "veg"
# fruitveg$fruitveg[fruitveg$fruitveg == "pepper"] <- "NA"

all_country_bp_and_consumption = data.frame(full_join(all_country_bp_and_consumption, fruitveg, by = "crop"))
head(all_country_bp_and_consumption)
unique(all_country_bp_and_consumption$fruitveg) # checking no NA

all_country_imports_and_bp = data.frame(full_join(all_country_imports_and_bp, fruitveg, by = "crop"))
head(all_country_imports_and_bp)
unique(all_country_imports_and_bp$fruitveg) # checking no NA

all_country_bpcons = data.frame(full_join(all_country_bpcons, fruitveg, by = "crop"))
head(all_country_bpcons)
unique(all_country_bpcons$fruitveg) # checking no NA


###################################################################################################################
###################################################################################################################
# 8. Figure 1
###################################################################################################################
###################################################################################################################

### Mapping BP globally and in each country.
# The per country and global maps were exported from data in R and then produced in ArcMap.
## For the global figs, I want all F&V maps for i) BP, ii) yield, and iii) ASR.

head(pressure_dry_percountry4) # countrycode, country, crop, pressure, ISO_partner (same as country)
pressure_data_nozero = filter(pressure_dry_percountry4, pressure_dry != 0)
pressure_data_nozero1 = filter(pressure_data_nozero, pressure_dry != "")
# what are the ranges of this?
range(pressure_data_nozero1$pressure_dry, na.rm = TRUE) #  8.194631e-20 8.342407e+23
8.342407e+23-8.194631e-20 # BP RANGE: 8.342407e+23

# need the pressure for fruit and veg more generally
pressure_data_mapping = full_join(pressure_data_nozero1, fruitveg, by = "crop")
head(pressure_data_mapping)
pressure_data_sums = pressure_data_mapping %>% 
  group_by(countrycode) %>% 
  summarise(summed_BP = sum(pressure_dry, na.rm = TRUE))

fv_sum = sum(pressure_dry, na.rm = TRUE) # this is summing the pressure_dry raster not column
fv_sum
plot(fv_sum)
rasterdir = "SHEFS/SHEFS_Sept2023_Update/6_GlobalRasters/"
# writeRaster(x=fv_sum, filename = paste0(rasterdir, "BP_summed_allFV"), format = "GTiff", overwrite = TRUE)

dry_weight_production_maps_global
fv_sum_production = sum(dry_weight_production_maps_global, na.rm = TRUE)
fv_sum_production
plot(fv_sum_production)
rasterdir = "SHEFS/SHEFS_Sept2023_Update/6_GlobalRasters/"
# writeRaster(x=fv_sum_production, filename = paste0(rasterdir, "production_summed_allFV"), format = "GTiff", overwrite = TRUE)

asr
fv_sum_asr = sum(asr, na.rm = TRUE)
fv_sum_asr
plot(fv_sum_asr)
rasterdir = "SHEFS/SHEFS_Sept2023_Update/6_GlobalRasters/"
# writeRaster(x=fv_sum_asr, filename = paste0(rasterdir, "asr_summed_allFV"), format = "GTiff", overwrite = TRUE)

###################################################################################################################
###################################################################################################################
# 9. Figure 2
###################################################################################################################
###################################################################################################################

# a) consumption

(fig1adata1 = all_country_bp_and_consumption %>% 
    filter(dmi_tonnes_average_2000>1) %>% 
    group_by(country, fruitveg) %>% 
    summarise(median = median(dmi_tonnes_average_2000)))
# India fruit = 1053859, veg = 1911875
# South Africa fruit = 68396, veg = 33044
# UK fruit = 10431, veg = 128552

# Domestic consumption data are in all_country_bp_and_consumption
# Import consumption data are in all_country_imports_and_bp
consumption_data_local = subset(all_country_bp_and_consumption, select = c(crop, reporter, partner, ISO_reporter, ISO_partner, dmi_tonnes_average_2000, 
                                                                           country, fruitveg))
consumption_data_import = subset(all_country_imports_and_bp, select = c(crop, reporter, partner,ISO_reporter, ISO_partner, dmi_tonnes_average_2000,
                                                                        country, fruitveg))
consumptiondata_all = rbind(consumption_data_local, consumption_data_import)
head(consumptiondata_all); str(consumptiondata_all)
consumptiondata_all$country = as.character(consumptiondata_all$country)
consumptiondata_all$import_domestic = consumptiondata_all$country
consumptiondata_all$import_domestic[consumptiondata_all$import_domestic == "India" | 
                                      consumptiondata_all$import_domestic == "United Kingdom" |
                                      consumptiondata_all$import_domestic == "South Africa"] <- "domestic"
consumptiondata_all$import_domestic[consumptiondata_all$import_domestic != "domestic"] <- "import"
unique(consumptiondata_all$import_domestic)
# NAs are crops consumed in some countries and not others


summary(consumptiondata_all$dmi_tonnes_average_2000)

fvchecker_consumption = data.frame(consumptiondata_all) %>% 
  group_by(ISO_reporter, import_domestic, fruitveg) %>% 
  filter(dmi_tonnes_average_2000 != "NA") %>% 
  summarise(n_percountry = length(crop),
            consumed_tonnes = sum(dmi_tonnes_average_2000)) 

(fvchecker_consumption1 = data.frame(na.omit(fvchecker_consumption)) %>% 
    arrange(fruitveg))

# GBR example:
# imported fruit = 2937935
# domestic fruit = 623682
# 
((2937935)/(2937935+623682))*100 # 82% import GBR fruit
((623682)/(2937935+623682))*100 # 18% domestic GBR fruit

# IND example:
# imported fruit = 262565.43
# domestic fruit = 34656104.32
# 
((262565.43)/(262565.43+34656104.32))*100 # 0.75% import IND fruit
((34656104.32)/(262565.43+34656104.32))*100 # 99% domestic IND fruit

# ZAF example:
# imported fruit = 10187.15
# domestic fruit = 3458612.56
# 
((10187.15)/(10187.15+3458612.56))*100 # 0.3% import ZAF fruit
((3458612.56)/(10187.15+3458612.56))*100 # 99% domestic ZAF fruit

# GBR example:
# imported veg = 2476312.02
# domestic veg = 2660906.25
# 
((2476312.02)/(2476312.02+2660906.25))*100 # 48% import GBR veg
((2660906.25)/(2476312.02+2660906.25))*100 # 52% domestic GBR veg

# IND example:
# imported veg = 39303.50
# domestic veg = 42186891.12
# 
((39303.50)/(39303.50+42186891.12))*100 # 0.09% import IND veg
((42186891.12)/(39303.50+42186891.12))*100 # 99.9% domestic IND veg

# ZAF example:
# imported veg = 22653.15
# domestic veg = 1861353.29
# 
((22653.15)/(22653.15+1861353.29))*100 # 1% import ZAF veg
((1861353.29)/(22653.15+1861353.29))*100 # 99% domestic ZAF veg


(fig1adata1 = consumptiondata_all %>% 
    filter(dmi_tonnes_average_2000>0) %>% 
    group_by(import_domestic, reporter, fruitveg) %>% 
    summarise(median = round(median(dmi_tonnes_average_2000, na.rm = TRUE),2),
              mean = round(mean(dmi_tonnes_average_2000, na.rm = TRUE),2)))

(fig1adata1 = consumptiondata_all %>% 
    filter(dmi_tonnes_average_2000>0) %>% 
    group_by(reporter, fruitveg) %>% 
    summarise(median = median(dmi_tonnes_average_2000, na.rm = TRUE),
              mean = mean(dmi_tonnes_average_2000, na.rm = TRUE)))

(fig1a_consumption_violin = na.omit(consumptiondata_all) %>% 
    filter(dmi_tonnes_average_2000>0) %>% # 
    ggplot(aes(x = import_domestic, y = dmi_tonnes_average_2000, fill = fruitveg)) +
    geom_violin(draw_quantiles = c(0.5), 
                trim = TRUE) + # adding the quantiles (median only) and trimming the tails to the range of the data
    facet_wrap(~ISO_reporter) + # creating the separate boxes
    theme(strip.text.x = element_text(margin = margin(2,0,2,0), size = 12), strip.background = element_rect(colour = "black", fill = "gray93"),
          panel.border = element_rect(colour = "black", fill = NA)) + # specifying the appearance of the facet wrap
    scale_fill_manual(values = c("#E69F00", "#009E73"), labels = c("Fruits", "Vegetables")) + # colourblind-friendly palette and distinct so should display ok on black and white printout - see https://grafify-vignettes.netlify.app/colour_palettes.html
    scale_y_log10() +
    theme(text = element_text(family="sans"), # sans serif
          axis.title.y = element_text(size = 12, vjust = 5, face = "bold"), # ensuring position of axis label is away from the ticks
          axis.text.y = element_text(size = 12),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = 12),
          legend.title = element_blank(), # removing the legend title as unnecessary here
          legend.text = element_text(size = 12),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "gray93"), # adding the lines to show the log scale
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20)) + # expanding the margins so the axis title isn't cut
    scale_x_discrete(labels = c("domestic", "imported", "domestic", "imported", "domestic", "imported")) + # specifying x axis labels
    labs(x = "", y = "Consumption (dry-weight, tons)")) # expanding the margin for the y axis, as the y axis label was being clipped

## no labels version
(fig1a_consumption_violin_nolabels = na.omit(consumptiondata_all) %>% 
    filter(dmi_tonnes_average_2000>0) %>% # 
    ggplot(aes(x = import_domestic, y = dmi_tonnes_average_2000, fill = fruitveg)) +
    geom_violin(draw_quantiles = c(0.5), 
                trim = TRUE) + # adding the quantiles (median only) and trimming the tails to the range of the data
    facet_wrap(~ISO_reporter) + # creating the separate boxes
    theme(strip.text.x = element_blank(), strip.background = element_rect(colour = "black", fill = "gray93"),
          panel.border = element_blank()) + # specifying the appearance of the facet wrap
    scale_fill_manual(values = c("#E69F00", "#009E73"), labels = c("Fruits", "Vegetables")) + # colourblind-friendly palette and distinct so should display ok on black and white printout - see https://grafify-vignettes.netlify.app/colour_palettes.html
    scale_y_log10() +
    theme(text = element_blank(), # sans serif
          axis.title.y = element_blank(), # ensuring position of axis label is away from the ticks
          axis.text.y = element_blank(),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_blank(), # removing the legend title as unnecessary here
          legend.text = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "gray93"), # adding the lines to show the log scale
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20)))# + # expanding the margins so the axis title isn't cut
    #scale_x_discrete(labels = c("domestic", "imported", "domestic", "imported", "domestic", "imported")) + # specifying x axis labels
    #labs(x = "", y = "Consumption (dry-weight, tons)")) # expanding the margin for the y axis, as the y axis label was being clipped

## median values:
(median_data_consumption = consumptiondata_all %>% 
    filter(dmi_tonnes_average_2000>0) %>% 
  group_by(ISO_reporter, import_domestic, fruitveg) %>% 
  summarise(median_consumption = median(dmi_tonnes_average_2000, na.rm = TRUE)))
print(median_data_consumption, n = 24)

# b) BP 

# What I want for this is twofold:
# i) panel b components: focalcountry_bp$pressure_dry is the pressure per country for the focal country
# This needs grouping into F&V
# focalcountry_imports_and_bp$pressure_dry is the BP per crop and partner combination for imports
# This needs to be averaged per crop and then grouped into F&V

head(uk_bp, n = 5) # countrycode, country, crop, BP
head(uk_imports_and_bp, n = 5) # crop, reporter, partner, tonnes, ISO reporter, ISO partner, 
# countrycode, country (partner), pressure_dry, imports_multiplied....

# Per crop average imports:
uk_imports_and_bp1 = uk_imports_and_bp
uk_weightedaverage_bp = uk_imports_and_bp %>% 
  group_by(crop) %>% 
  summarise(import_weightedaverage_percrop = (sum(dmi_tonnes_average_2000*pressure_dry, na.rm = TRUE))/(sum(dmi_tonnes_average_2000, na.rm = TRUE)))

# Joining the imports and focal data:
head(uk_bp)
colnames(uk_bp) = c("ISO_reporter", "country", "crop", "pressure_dry")
uk_bp = subset(uk_bp, select = -c(country))
uk_bp$domestic_imported = "domestic"
head(uk_weightedaverage_bp)
uk_weightedaverage_bp$domestic_imported = "imported"
uk_weightedaverage_bp$ISO_reporter = "GBR"
uk_weightedaverage_bp$pressure_dry = uk_weightedaverage_bp$import_weightedaverage_percrop
uk_weightedaverage_bp1 = subset(uk_weightedaverage_bp, select = -c(import_weightedaverage_percrop))
uk_bp_data = full_join(uk_bp, uk_weightedaverage_bp1, by = c("crop", "ISO_reporter", 
                                                             "domestic_imported", "pressure_dry"))
head(uk_bp_data)
# View(uk_bp_data)
# View(uk_bp)
# View(uk_imports_and_bp2)

# Assigning the fruit vs vegetables ('veg') identifiers:
uk_bp_data1 = full_join(uk_bp_data, fruitveg, by = "crop")
# View(uk_bp_data1)

# Repeating for the other countries and then joining the data together:

head(SouthAfrica_bp, n = 5) # countrycode, country, crop, BP
head(SouthAfrica_imports_and_bp, n = 5) # crop, reporter, partner, tonnes, ISO reporter, ISO partner, 
# countrycode, country (partner), pressure_dry, imports_multiplied....
SouthAfrica_imports_and_bp1 = SouthAfrica_imports_and_bp
SouthAfrica_weightedaverage_bp = SouthAfrica_imports_and_bp %>% 
  group_by(crop) %>% 
  summarise(import_weightedaverage_percrop = (sum(dmi_tonnes_average_2000*pressure_dry, na.rm = TRUE))/(sum(dmi_tonnes_average_2000, na.rm = TRUE)))

(importaverage_pressure_dry_percrop = mean(pressure_dry, na.rm = TRUE))

head(SouthAfrica_bp)
SouthAfrica_bp = subset(SouthAfrica_bp, select = c("crop", "ISO_partner", "pressure_dry"))
colnames(SouthAfrica_bp) = c("crop", "ISO_reporter", "pressure_dry")
SouthAfrica_bp$domestic_imported = "domestic"
head(SouthAfrica_weightedaverage_bp)
SouthAfrica_weightedaverage_bp$domestic_imported = "imported"
SouthAfrica_weightedaverage_bp$ISO_reporter = "ZAF"
SouthAfrica_weightedaverage_bp$pressure_dry = SouthAfrica_weightedaverage_bp$import_weightedaverage_percrop
SouthAfrica_weightedaverage_bp1 = subset(SouthAfrica_weightedaverage_bp, select = -c(import_weightedaverage_percrop))
SouthAfrica_bp_data = full_join(SouthAfrica_bp, SouthAfrica_weightedaverage_bp1, by = c("crop", "ISO_reporter", 
                                                                                        "domestic_imported", "pressure_dry"))
head(SouthAfrica_bp_data)
SouthAfrica_bp_data1 = full_join(SouthAfrica_bp_data, fruitveg, by = "crop")

head(India_bp, n = 5) # countrycode, country, crop, BP
head(India_imports_and_bp, n = 5) # crop, reporter, partner, tonnes, ISO reporter, ISO partner, 
# countrycode, country (partner), pressure_dry, imports_multiplied....
India_imports_and_bp1 = India_imports_and_bp
India_imports_and_bp1 = data.frame(India_imports_and_bp1 %>% 
                                     group_by(crop, ISO_reporter) %>% 
                                     summarise(importaverage_pressure_dry_percrop = mean(pressure_dry, na.rm = TRUE)))
str(India_imports_and_bp1)
head(India_imports_and_bp1, n = 5)
India_imports_and_bp1 = India_imports_and_bp
India_weightedaverage_bp = India_imports_and_bp %>% 
  group_by(crop) %>% 
  summarise(import_weightedaverage_percrop = (sum(dmi_tonnes_average_2000*pressure_dry, na.rm = TRUE))/(sum(dmi_tonnes_average_2000, na.rm = TRUE)))
head(India_bp)
colnames(India_bp) = c("ISO_reporter", "country", "crop", "pressure_dry")
India_bp$domestic_imported = "domestic"
head(India_weightedaverage_bp)
India_weightedaverage_bp$domestic_imported = "imported"
India_weightedaverage_bp$ISO_reporter = "IND"
India_weightedaverage_bp$pressure_dry = India_weightedaverage_bp$import_weightedaverage_percrop
India_weightedaverage_bp1 = subset(India_weightedaverage_bp, select = -c(import_weightedaverage_percrop))
India_bp_data = full_join(India_bp, India_weightedaverage_bp1, by = c("crop", "ISO_reporter", 
                                                                      "domestic_imported", "pressure_dry"))

head(India_bp_data)
India_bp_data1 = full_join(India_bp_data, fruitveg, by = "crop")
India_bp_data1 = India_bp_data1[,-5]

# Bringing the country data together:
head(uk_bp_data1)
head(SouthAfrica_bp_data1)
head(India_bp_data1)

all_bpdata1 = full_join(uk_bp_data1, SouthAfrica_bp_data1, by = c("crop",
                                                                  "ISO_reporter",
                                                                  "pressure_dry",
                                                                  "domestic_imported",
                                                                  "fruitveg"))

India_bp_data2 = subset(India_bp_data1, select = -c(country))
all_bpdata2 = full_join(all_bpdata1, India_bp_data2, by = c("crop",
                                                            "ISO_reporter",
                                                            "pressure_dry",
                                                            "domestic_imported",
                                                            "fruitveg"))

# Plotting domestic vs imported fruits vs veg:

# How many fruits and vegetables included per country?
fvchecker = data.frame(all_bpdata2) %>% 
  group_by(ISO_reporter, domestic_imported) %>% 
  filter(pressure_dry != "NA") %>% 
  summarise(n_percountry = length(crop)) 

options(scipen = 999)

(figbadata1 = all_bpdata2 %>% 
    filter(pressure_dry >0) %>% 
    group_by(domestic_imported, ISO_reporter, fruitveg) %>% 
    summarise(median = round(median(pressure_dry, na.rm = TRUE), 2),
              mean = round(mean(pressure_dry, na.rm = TRUE), 2)))

library(ggforce) # for breaking the axes
min(all_bpdata2$pressure_dry, na.rm = TRUE)


## as there are outliers at both ends of the scale, we need to zoom at both ends

(fig1b_BP_violin_GBR = na.omit(all_bpdata2) %>% 
    filter(pressure_dry  > 0) %>% 
    filter(ISO_reporter == "GBR") %>% 
    ggplot(aes(x = domestic_imported, y = pressure_dry, fill = fruitveg)) +
    geom_violin(draw_quantiles = c(0.5), trim = TRUE) +
    facet_zoom(ylim = c(1e-01, 1e+04), zoom.data = ifelse(pressure_dry <= 1e+04, NA, FALSE)) +
    theme(strip.text.x = element_text(margin = margin(2,0,2,0), size = 12), strip.background = element_rect(colour = "black", fill = "gray93"),
          panel.border = element_rect(colour = "black", fill = NA)) + # specifying the appearance of the facet wrap
    scale_fill_manual(values = c("#E69F00", "#009E73"), labels = c("Fruits", "Vegetables")) + # colourblind-friendly palette and distinct so should display ok on black and white printout - see https://grafify-vignettes.netlify.app/colour_palettes.html
    scale_y_log10() + 
    theme(text = element_text(family="sans"), # sans serif
          axis.title.y = element_text(size = 12, vjust = 5, face = "bold"), # ensuring position of axis label is away from the ticks
          axis.text.y = element_text(size = 12),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = 12),
          legend.title = element_blank(), # removing the legend title as unnecessary here
          legend.text = element_text(size = 12),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(color = "gray93"), # adding the lines to show the log scale
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20)) + # expanding the margins so the axis title isn't cut
    #scale_x_discrete(labels = c("domestic", "imported", "domestic", "imported", "domestic", "imported")) + # specifying x axis labels
    labs(x = "", y = "BP (species*ha/tonne)"))


(fig1b_BP_violin_GBR_nolabels = na.omit(all_bpdata2) %>% 
    filter(pressure_dry  > 0) %>% 
    filter(ISO_reporter == "GBR") %>% 
    ggplot(aes(x = domestic_imported, y = pressure_dry, fill = fruitveg)) +
    geom_violin(draw_quantiles = c(0.5), trim = TRUE) +
    facet_zoom(ylim = c(1e-01, 1e+04), zoom.data = ifelse(pressure_dry <= 1e+04, NA, FALSE)) +
    theme(strip.text.x = element_blank(), strip.background = element_rect(colour = "black", fill = "gray93"),
          panel.border = element_rect(colour = "black", fill = NA)) + # specifying the appearance of the facet wrap
    scale_fill_manual(values = c("#E69F00", "#009E73"), labels = c("Fruits", "Vegetables")) + # colourblind-friendly palette and distinct so should display ok on black and white printout - see https://grafify-vignettes.netlify.app/colour_palettes.html
    scale_y_log10() + 
    theme(text = element_blank(), # sans serif
          axis.title.y = element_blank(), # ensuring position of axis label is away from the ticks
          axis.text.y = element_blank(),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_blank(), # removing the legend title as unnecessary here
          legend.text = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(color = "gray93"), # adding the lines to show the log scale
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20)) #+ # expanding the margins so the axis title isn't cut
    #scale_x_discrete(labels = c("domestic", "imported", "domestic", "imported", "domestic", "imported")) + # specifying x axis labels
    #labs(x = "", y = "BP (species*ha/tonne)"))
)

(fig1b_BP_violin_IND = na.omit(all_bpdata2) %>% 
    filter(pressure_dry  > 0) %>% 
    filter(ISO_reporter == "IND") %>% 
    ggplot(aes(x = domestic_imported, y = pressure_dry, fill = fruitveg)) +
    geom_violin(draw_quantiles = c(0.5), trim = TRUE) +
    facet_zoom(ylim = c(1e-01, 1e+04), zoom.data = ifelse(pressure_dry <= 1e+04, NA, FALSE)) +
    theme(strip.text.x = element_text(margin = margin(2,0,2,0), size = 12), strip.background = element_rect(colour = "black", fill = "gray93"),
          panel.border = element_rect(colour = "black", fill = NA)) + # specifying the appearance of the facet wrap
    scale_fill_manual(values = c("#E69F00", "#009E73"), labels = c("Fruits", "Vegetables")) + # colourblind-friendly palette and distinct so should display ok on black and white printout - see https://grafify-vignettes.netlify.app/colour_palettes.html
    scale_y_log10() + 
    theme(text = element_text(family="sans"), # sans serif
          axis.title.y = element_text(size = 12, vjust = 5, face = "bold"), # ensuring position of axis label is away from the ticks
          axis.text.y = element_text(size = 12),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = 12),
          legend.title = element_blank(), # removing the legend title as unnecessary here
          legend.text = element_text(size = 12),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(color = "gray93"), # adding the lines to show the log scale
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20)) + # expanding the margins so the axis title isn't cut
    #scale_x_discrete(labels = c("domestic", "imported", "domestic", "imported", "domestic", "imported")) + # specifying x axis labels
    labs(x = "", y = "BP (species*ha/tonne)"))


(fig1b_BP_violin_IND_nolabels = na.omit(all_bpdata2) %>% 
    filter(pressure_dry  > 0) %>% 
    filter(ISO_reporter == "IND") %>% 
    ggplot(aes(x = domestic_imported, y = pressure_dry, fill = fruitveg)) +
    geom_violin(draw_quantiles = c(0.5), trim = TRUE) +
    facet_zoom(ylim = c(1e-01, 1e+04), zoom.data = ifelse(pressure_dry <= 1e+04, NA, FALSE)) +
    theme(strip.text.x = element_blank(), strip.background = element_rect(colour = "black", fill = "gray93"),
          panel.border = element_rect(colour = "black", fill = NA)) + # specifying the appearance of the facet wrap
    scale_fill_manual(values = c("#E69F00", "#009E73"), labels = c("Fruits", "Vegetables")) + # colourblind-friendly palette and distinct so should display ok on black and white printout - see https://grafify-vignettes.netlify.app/colour_palettes.html
    scale_y_log10() + 
    theme(text = element_blank(), # sans serif
          axis.title.y = element_blank(), # ensuring position of axis label is away from the ticks
          axis.text.y = element_blank(),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_blank(), # removing the legend title as unnecessary here
          legend.text = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(color = "gray93"), # adding the lines to show the log scale
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20)) #+ # expanding the margins so the axis title isn't cut
  #scale_x_discrete(labels = c("domestic", "imported", "domestic", "imported", "domestic", "imported")) + # specifying x axis labels
  #labs(x = "", y = "BP (species*ha/tonne)"))
)

(fig1b_BP_violin_ZAF = na.omit(all_bpdata2) %>% 
    filter(pressure_dry  > 0) %>% 
    filter(ISO_reporter == "ZAF") %>% 
    ggplot(aes(x = domestic_imported, y = pressure_dry, fill = fruitveg)) +
    geom_violin(draw_quantiles = c(0.5), trim = TRUE) +
    facet_zoom(ylim = c(10, 1e+04), zoom.data = ifelse(pressure_dry <= 1e+04, NA, FALSE)) +
    theme(strip.text.x = element_text(margin = margin(2,0,2,0), size = 12), strip.background = element_rect(colour = "black", fill = "gray93"),
          panel.border = element_rect(colour = "black", fill = NA)) + # specifying the appearance of the facet wrap
    scale_fill_manual(values = c("#E69F00", "#009E73"), labels = c("Fruits", "Vegetables")) + # colourblind-friendly palette and distinct so should display ok on black and white printout - see https://grafify-vignettes.netlify.app/colour_palettes.html
    scale_y_log10() + 
    theme(text = element_text(family="sans"), # sans serif
          axis.title.y = element_text(size = 12, vjust = 5, face = "bold"), # ensuring position of axis label is away from the ticks
          axis.text.y = element_text(size = 12),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = 12),
          legend.title = element_blank(), # removing the legend title as unnecessary here
          legend.text = element_text(size = 12),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(color = "gray93"), # adding the lines to show the log scale
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20)) + # expanding the margins so the axis title isn't cut
    #scale_x_discrete(labels = c("domestic", "imported", "domestic", "imported", "domestic", "imported")) + # specifying x axis labels
    labs(x = "", y = "BP (species*ha/tonne)"))


(fig1b_BP_violin_ZAF_nolabels = na.omit(all_bpdata2) %>% 
    filter(pressure_dry  > 0) %>% 
    filter(ISO_reporter == "ZAF") %>% 
    ggplot(aes(x = domestic_imported, y = pressure_dry, fill = fruitveg)) +
    geom_violin(draw_quantiles = c(0.5), trim = TRUE) +
    facet_zoom(ylim = c(10, 1e+04), zoom.data = ifelse(pressure_dry <= 1e+04, NA, FALSE)) +
    theme(strip.text.x = element_blank(), strip.background = element_rect(colour = "black", fill = "gray93"),
          panel.border = element_rect(colour = "black", fill = NA)) + # specifying the appearance of the facet wrap
    scale_fill_manual(values = c("#E69F00", "#009E73"), labels = c("Fruits", "Vegetables")) + # colourblind-friendly palette and distinct so should display ok on black and white printout - see https://grafify-vignettes.netlify.app/colour_palettes.html
    scale_y_log10() + 
    theme(text = element_blank(), # sans serif
          axis.title.y = element_blank(), # ensuring position of axis label is away from the ticks
          axis.text.y = element_blank(),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_blank(), # removing the legend title as unnecessary here
          legend.text = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(color = "gray93"), # adding the lines to show the log scale
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20)) #+ # expanding the margins so the axis title isn't cut
  #scale_x_discrete(labels = c("domestic", "imported", "domestic", "imported", "domestic", "imported")) + # specifying x axis labels
  #labs(x = "", y = "BP (species*ha/tonne)"))
)


## median values:
median_data_bp = all_bpdata2 %>%
  filter(pressure_dry  > 1) %>% 
  group_by(ISO_reporter, domestic_imported, fruitveg) %>% 
  summarise(median_BP = median(pressure_dry, na.rm = TRUE))

# Panel c components: focalcountry_bp_and_consumption$bp_multiplied_by_tonnes is the BPcons for the focal country per crop
# This also needs grouping into F&V for the plot.

head(uk_bp_and_consumption, n = 5) # crop, reporter, partner, tonnes, countrycode, country, BP, BP multiplied by tonnes for the focal country
head(uk_imports_and_bp, n = 5)

# all_country_bp_and_consumption$bp_multiplied_by_tonnes is still the BPcons part 1, weighted BP local component
# all_country_imports_and_bp$imports_multiplied_by_BP_of_importpartners is still the BPcons part 2, weighted BP import component

# For BPcons, it's the sum of all import partners that we want rather than the average:
uk_imports_and_bp_cons = uk_imports_and_bp
uk_imports_and_bp_cons = data.frame(uk_imports_and_bp_cons %>% 
                                      group_by(crop, ISO_reporter) %>% 
                                      summarise(importaverage_bpcons_percrop = sum(imports_multiplied_by_BP_of_importpartners , na.rm = TRUE)))
str(uk_imports_and_bp_cons)
head(uk_imports_and_bp_cons, n = 5)

# Joining the imports and focal data:
head(uk_bp_and_consumption)
uk_bp_and_consumption1 = subset(uk_bp_and_consumption, select = -c(reporter, partner, 
                                                                   dmi_tonnes_average_2000,
                                                                   ISO_partner, countrycode,
                                                                   pressure_dry))
colnames(uk_bp_and_consumption1) = c("crop", "ISO_reporter", "country", "BPcons")
uk_bp_and_consumption1$domestic_imported = "domestic"

head(uk_imports_and_bp_cons)
uk_imports_and_bp_cons2 = filter(uk_imports_and_bp_cons, ISO_reporter == "GBR")
colnames(uk_imports_and_bp_cons2) = c("crop", "ISO_reporter", "BPcons")
head(uk_imports_and_bp_cons2)
uk_imports_and_bp_cons2$domestic_imported = "imported"
uk_bp_data_bpcons = full_join(uk_bp_and_consumption1, uk_imports_and_bp_cons2, by = c("crop", "ISO_reporter", 
                                                                                      "domestic_imported", "BPcons"))
head(uk_bp_data_bpcons)
uk_bp_data_bpcons = subset(uk_bp_data_bpcons, select = -c(country))

# Assigning the fruit and veg identifiers:
uk_bp_data_bpcons1 = full_join(uk_bp_data_bpcons, fruitveg, by = "crop")

# Repeating for the other countries and then joining the data together:
SouthAfrica_imports_and_bp_cons = SouthAfrica_imports_and_bp
SouthAfrica_imports_and_bp_cons = data.frame(SouthAfrica_imports_and_bp_cons %>% 
                                               group_by(crop, ISO_reporter) %>% 
                                               summarise(importaverage_bpcons_percrop = sum(imports_multiplied_by_BP_of_importpartners , na.rm = TRUE)))
str(SouthAfrica_imports_and_bp_cons)
head(SouthAfrica_imports_and_bp_cons, n = 5)
head(SouthAfrica_bp_and_consumption)
SouthAfrica_bp_and_consumption1 = subset(SouthAfrica_bp_and_consumption, select = -c(reporter, partner, 
                                                                                     dmi_tonnes_average_2000,
                                                                                     ISO_partner, countrycode,
                                                                                     pressure_dry))
colnames(SouthAfrica_bp_and_consumption1) = c("crop", "ISO_reporter", "country", "BPcons")
SouthAfrica_bp_and_consumption1$domestic_imported = "domestic"
head(SouthAfrica_imports_and_bp_cons)
SouthAfrica_imports_and_bp_cons2 = filter(SouthAfrica_imports_and_bp_cons, ISO_reporter == "ZAF")
colnames(SouthAfrica_imports_and_bp_cons2) = c("crop", "ISO_reporter", "BPcons")
head(SouthAfrica_imports_and_bp_cons2)
SouthAfrica_imports_and_bp_cons2$domestic_imported = "imported"
SouthAfrica_bp_data_bpcons = full_join(SouthAfrica_bp_and_consumption1, SouthAfrica_imports_and_bp_cons2, by = c("crop", "ISO_reporter", 
                                                                                                                 "domestic_imported", "BPcons"))
head(SouthAfrica_bp_data_bpcons)
SouthAfrica_bp_data_bpcons = subset(SouthAfrica_bp_data_bpcons, select = -c(country))
SouthAfrica_bp_data_bpcons1 = full_join(SouthAfrica_bp_data_bpcons, fruitveg, by = "crop")

India_imports_and_bp_cons = India_imports_and_bp
India_imports_and_bp_cons = data.frame(India_imports_and_bp_cons %>% 
                                         group_by(crop, ISO_reporter) %>% 
                                         summarise(importaverage_bpcons_percrop = sum(imports_multiplied_by_BP_of_importpartners , na.rm = TRUE)))
str(India_imports_and_bp_cons)
head(India_imports_and_bp_cons, n = 5)
head(India_bp_and_consumption)
India_bp_and_consumption1 = subset(India_bp_and_consumption, select = -c(reporter, partner, 
                                                                         dmi_tonnes_average_2000,
                                                                         ISO_partner, countrycode,
                                                                         pressure_dry))
colnames(India_bp_and_consumption1) = c("crop", "ISO_reporter", "country", "BPcons")
India_bp_and_consumption1$domestic_imported = "domestic"
head(India_imports_and_bp_cons)
India_imports_and_bp_cons2 = filter(India_imports_and_bp_cons, ISO_reporter == "IND")
colnames(India_imports_and_bp_cons2) = c("crop", "ISO_reporter", "BPcons")
head(India_imports_and_bp_cons2)
India_imports_and_bp_cons2$domestic_imported = "imported"
India_bp_data_bpcons = full_join(India_bp_and_consumption1, India_imports_and_bp_cons2, by = c("crop", "ISO_reporter", 
                                                                                               "domestic_imported", "BPcons"))
head(India_bp_data_bpcons)
India_bp_data_bpcons = subset(India_bp_data_bpcons, select = -c(country))
India_bp_data_bpcons1 = full_join(India_bp_data_bpcons, fruitveg, by = "crop")

# Join the data together:
head(uk_bp_data_bpcons1)
head(SouthAfrica_bp_data_bpcons1)
head(India_bp_data_bpcons1)

all_bpdata1_bpcons = full_join(uk_bp_data_bpcons1, SouthAfrica_bp_data_bpcons1, by = c("crop",
                                                                                       "ISO_reporter",
                                                                                       "BPcons",
                                                                                       "domestic_imported",
                                                                                       "fruitveg"))

all_bpdata2_bpcons = full_join(all_bpdata1_bpcons, India_bp_data_bpcons1, by = c("crop",
                                                                                 "ISO_reporter",
                                                                                 "BPcons",
                                                                                 "domestic_imported",
                                                                                 "fruitveg"))


# Plotting panels:

(fig1cdata1 = all_bpdata2_bpcons %>% 
    filter(BPcons >0) %>% 
    group_by(domestic_imported, ISO_reporter, fruitveg) %>% 
    summarise(median = round(median(BPcons, na.rm = TRUE),2),
              mean = round(mean(BPcons, na.rm = TRUE),2)))

(fig1c_BPcons_violin = na.omit(all_bpdata2_bpcons) %>% 
    filter(BPcons  > 0) %>% 
    ggplot(aes(x = domestic_imported, y = BPcons, fill = fruitveg)) +
    geom_violin(draw_quantiles = c(0.5), trim = TRUE) +
    facet_wrap(~ISO_reporter) + # creating the separate boxes
    theme(strip.text.x = element_text(margin = margin(2,0,2,0), size = 12), strip.background = element_rect(colour = "black", fill = "gray93"),
          panel.border = element_rect(colour = "black", fill = NA)) + # specifying the appearance of the facet wrap
    scale_fill_manual(values = c("#E69F00", "#009E73"), labels = c("Fruits", "Vegetables")) + # colourblind-friendly palette and distinct so should display ok on black and white printout - see https://grafify-vignettes.netlify.app/colour_palettes.html
    scale_y_log10() +
    theme(text = element_text(family="sans"), # sans serif
          axis.title.y = element_text(size = 12, vjust = 5, face = "bold"), # ensuring position of axis label is away from the ticks
          axis.text.y = element_text(size = 12),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = 12),
          legend.title = element_blank(), # removing the legend title as unnecessary here
          legend.text = element_text(size = 12),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(color = "gray93"), # adding the lines to show the log scale
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20)) + # expanding the margins so the axis title isn't cut
    #scale_x_discrete(labels = c("domestic", "imported", "domestic", "imported", "domestic", "imported")) + # specifying x axis labels
    labs(x = "", y = "BPcons"))


(fig1b_BP_nolabels_violin = na.omit(all_bpdata2_bpcons) %>% 
    filter(BPcons  > 0) %>% 
    ggplot(aes(x = domestic_imported, y = BPcons, fill = fruitveg)) +
    geom_violin(draw_quantiles = c(0.5), trim = TRUE) +
    facet_wrap(~ISO_reporter) + # creating the separate boxes
    theme(strip.text.x = element_blank(), strip.background = element_blank(),
          panel.border = element_blank()) + # specifying the appearance of the facet wrap
    scale_fill_manual(values = c("#E69F00", "#009E73"), labels = c("Fruits", "Vegetables")) + # colourblind-friendly palette and distinct so should display ok on black and white printout - see https://grafify-vignettes.netlify.app/colour_palettes.html
    scale_y_log10() + 
    theme(text = element_text(family="sans"), # sans serif
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_blank(), # removing the legend title as unnecessary here
          legend.text = element_blank(),
          legend.position = "none",
          panel.background = element_blank(),
          panel.grid.major.y = element_line(color = "gray93"), # adding the lines to show the log scale
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20)) + # expanding the margins so the axis title isn't cut
    #scale_x_discrete(labels = c("domestic", "imported", "domestic", "imported", "domestic", "imported")) + # specifying x axis labels
    labs(x = "", y = "BPcons"))

## median values:
median_data_bpcons = all_bpdata2_bpcons %>% 
  filter(BPcons  > 0) %>% 
  group_by(ISO_reporter, domestic_imported, fruitveg) %>% 
  summarise(median_BPcons = median(BPcons, na.rm = TRUE))

###################################################################################################################
###################################################################################################################
# 10. Figure 3
###################################################################################################################
###################################################################################################################

# Next, I want to produce the panels above but for individual fruits and vegetables - for the top 5 crops for each country.

(crops_top5_consumption_uk = data.frame(consumptiondata_all %>% 
                                          filter(dmi_tonnes_average_2000  >0) %>%
                                          filter(ISO_reporter == "GBR") %>% 
                                          group_by(import_domestic, crop) %>% 
                                          summarise(mean_dmi_per_crop = mean(dmi_tonnes_average_2000, na.rm = TRUE)) %>% 
                                          arrange(desc(mean_dmi_per_crop)) %>% 
                                          slice(1:5)))
# UK:
# domestic: carrots, greenpeas, onion, cabbage, apple
# import: tomato, banana, grape, orange, apple
(crops_top5_consumption_southafrica = data.frame(consumptiondata_all %>% 
                                                   filter(dmi_tonnes_average_2000  >0) %>%
                                                   filter(ISO_reporter == "ZAF") %>% 
                                                   group_by(import_domestic, crop) %>% 
                                                   summarise(mean_dmi_per_crop = mean(dmi_tonnes_average_2000, na.rm = TRUE)) %>% 
                                                   arrange(desc(mean_dmi_per_crop)) %>% 
                                                   slice(1:5)))
# southafrica:
# domestic: grape, pumpkinetc, orange, pear, tomato
# import: tomato, grape, greencorn, cauliflower, currant
(crops_top5_consumption_india = data.frame(consumptiondata_all %>% 
                                             filter(dmi_tonnes_average_2000  >0) %>%
                                             filter(ISO_reporter == "IND") %>% 
                                             group_by(import_domestic, crop) %>% 
                                             summarise(mean_dmi_per_crop = mean(dmi_tonnes_average_2000, na.rm = TRUE)) %>% 
                                             arrange(desc(mean_dmi_per_crop)) %>% 
                                             slice(1:5)))
# india:
# domestic: banana, eggplant, cabbage, cauliflower, mango
# import: date, cabbage, grape, greenpea, garlic

crops_top5_consumption_india$country = "IND"
crops_top5_consumption_southafrica$country = "ZAF"
crops_top5_consumption_uk$country = "GBR"
consumption_top5_allcountries = rbind(crops_top5_consumption_india, crops_top5_consumption_southafrica, 
                                      crops_top5_consumption_uk)
consumption_top5_allcountries1 = na.omit(consumption_top5_allcountries)

# Setting up a colour scale to try to capture the crops easily and ensure it's consistent across plots:
p2 = c("chartreuse", "gold", "darkolivegreen1", "tan3", "bisque", "deeppink2", "saddlebrown", "purple4", "palegreen2",
       "plum3", "peachpuff1", "springgreen1", "lightskyblue", "steelblue",
       "darkorange1", "turquoise", "orangered", "red1")
unique(sort(consumption_top5_allcountries1$crop))
# apple = chartreuse
# avocado = * NEW - forestgreen
# banana = gold
# cabbage = darkolivegreen1
# carrot = tan3
# cauliflower = bisque
# currant = deeppink2
# date = saddlebrown
# eggplant = purple4
# garlic = palegreen2
# grape = plum3
# grapefruitetc = violetred1
# greencorn = peachpuff1
# greenpea = springgreen1
# mango = lightskyblue
# mushroom = rosybrown4
# onion = steelblue
# orange = darkorange1
# pear = turquoise
# pumpkinetc = orangered
# tomato = red1

summary(consumption_top5_allcountries1$mean_dmi_per_crop)

(fig2a_consumption_bar = consumption_top5_allcountries1 %>%
    ggplot(aes(x = import_domestic, y = mean_dmi_per_crop)) + # domestic import used here as we only need that distinction as we have the facet for the countries
    geom_bar(stat = "identity", position = position_dodge2(), aes(fill = crop)) +
    scale_fill_manual(values = p2)+
    scale_y_log10() + # 
    facet_wrap(~country) + # creating the separate boxes
    theme(strip.text.x = element_text(margin = margin(2,0,2,0), size = 12), strip.background = element_rect(colour = "black", fill = "gray93"),
          panel.border = element_rect(colour = "black", fill = NA)) + # specifying the appearance of the facet wrap
    theme(text = element_text(family="sans"), # sans serif
          axis.title.y = element_text(size = 12, vjust = 5, face = "bold"), # ensuring position of axis label is away from the ticks
          axis.text.y = element_text(size = 12),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = 12),
          legend.title = element_blank(), # removing the legend title as unnecessary here
          legend.text = element_blank(),
          legend.key = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(color = "gray73"), # adding the lines to show the log scale
          panel.grid.minor.y = element_line(color = "gray93"),
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20)) + # expanding the margins so the axis title isn't cut
    labs(x = "", y = "Consumption (dry-weight, tonnes)")+ # expanding the margin for the y axis, as the y axis label was being clipped
    geom_text(aes(fill = crop, label = crop), size = 5, colour = "black", angle = 90, vjust = 0, hjust = -0.05, position = position_dodge2(width = 0.9, preserve = "single")))

## No labels
(fig2a_consumption_bar = consumption_top5_allcountries1 %>%
    ggplot(aes(x = import_domestic, y = mean_dmi_per_crop)) + # domestic import used here as we only need that distinction as we have the facet for the countries
    geom_bar(stat = "identity", position = position_dodge2(), aes(fill = crop)) +
    scale_fill_manual(values = p2)+
    scale_y_log10() +  
    facet_wrap(~country) + # creating the separate boxes
    theme(strip.text.x = element_blank(), strip.background = element_blank(),
          panel.border = element_blank()) + # specifying the appearance of the facet wrap
    theme(text = element_text(family="sans"), # sans serif
          axis.title.y = element_blank(), # ensuring position of axis label is away from the ticks
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_blank(), # removing the legend title as unnecessary here
          legend.text = element_blank(),
          legend.key = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(color = "gray73"), # adding the lines to show the log scale
          panel.grid.minor.y = element_line(color = "gray93"),
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20))) #+ # expanding the margins so the axis title isn't cut

# b) BP 
all_bpdata2
consumptiondata_all
consumption_top5_allcountries1
## Updating as we want the domestic component as is, but we want the imported component to actually be a weighted average BP of
## an imported product.
(crops_top5_bp_uk = data.frame(all_bpdata2 %>% 
                                 filter(pressure_dry  >0) %>%
                                 filter(ISO_reporter == "GBR") %>%
                                 filter(domestic_imported == "domestic" & crop == "apple" | 
                                          domestic_imported == "domestic" & crop == "cabbage" |
                                          domestic_imported == "domestic" & crop == "carrot" |
                                          domestic_imported == "domestic" & crop == "greenpea"| 
                                          domestic_imported == "domestic" & crop == "onion" | 
                                          domestic_imported == "imported" & crop == "banana" |
                                          domestic_imported == "imported" & crop == "grape" |
                                          domestic_imported == "imported" & crop == "orange" |
                                          domestic_imported == "imported" & crop == "apple" |
                                          domestic_imported == "imported" & crop == "tomato") %>% 
                                 group_by(domestic_imported, crop) %>% 
                                 summarise(mean_bp = mean(pressure_dry, na.rm = TRUE)))) 
(crops_top5_bp_southafrica = data.frame(all_bpdata2 %>% 
                                          filter(pressure_dry  >0) %>%
                                          filter(ISO_reporter == "ZAF") %>% 
                                          filter(domestic_imported == "domestic" & crop == "grape" |
                                                   domestic_imported == "domestic" & crop == "orange" |
                                                   domestic_imported == "domestic" & crop == "pear" | 
                                                   domestic_imported == "domestic" & crop == "pumpkinetc"|
                                                   domestic_imported == "domestic" & crop == "tomato" |
                                                   domestic_imported == "imported" & crop == "tomato" |
                                                   domestic_imported == "imported" & crop == "cauliflower" |
                                                   domestic_imported == "imported" & crop == "currant" | 
                                                   domestic_imported == "imported" & crop == "grape" |
                                                   domestic_imported == "imported" & crop == "greencorn") %>% 
                                          group_by(domestic_imported, crop) %>% 
                                          summarise(mean_bp = mean(pressure_dry, na.rm = TRUE))))
(crops_top5_bp_india = data.frame(all_bpdata2 %>% 
                                    filter(pressure_dry  >0) %>%
                                    filter(ISO_reporter == "IND") %>% 
                                    filter(domestic_imported == "domestic" & crop == "banana" | 
                                             domestic_imported == "domestic" & crop == "cabbage" | 
                                             domestic_imported == "domestic" & crop == "cauliflower" |
                                             domestic_imported == "domestic" & crop == "eggplant" |
                                             domestic_imported == "domestic" & crop == "mango" |
                                             domestic_imported == "imported" & crop == "date" | 
                                             domestic_imported == "imported" & crop == "garlic" |
                                             domestic_imported == "imported" & crop == "cabbage" |
                                             domestic_imported == "imported" & crop == "grape" |
                                             domestic_imported == "imported" & crop == "greenpea") %>% 
                                    group_by(domestic_imported, crop) %>% 
                                    summarise(mean_bp = mean(pressure_dry, na.rm = TRUE))))
crops_top5_bp_india$country = "IND"
crops_top5_bp_southafrica$country = "ZAF"
crops_top5_bp_uk$country = "GBR"
bp_top5_allcountries = rbind(crops_top5_bp_india, crops_top5_bp_southafrica, 
                             crops_top5_bp_uk)
bp_top5_allcountries1 = na.omit(bp_top5_allcountries)

unique(sort(bp_top5_allcountries1$crop))

p2 = c("chartreuse", "gold", "darkolivegreen1", "tan3", "bisque", "deeppink2", "saddlebrown", "purple4", "palegreen2",
       "plum3", "peachpuff1", "springgreen1", "lightskyblue", "steelblue",
       "darkorange1", "turquoise", "orangered", "red1")

colnames(bp_top5_allcountries1) = c("import_domestic", "crop", "mean_bp", "country")

bp_top5_allcountries2 = bp_top5_allcountries1
bp_top5_allcountries2$alphatest = as.factor(ifelse(bp_top5_allcountries2$crop == "currant", 0.1, 1))

(fig2_BP = bp_top5_allcountries2 %>%
    ggplot(aes(x = import_domestic, y = mean_bp)) + # domestic import used here as we only need that distinction as we have the facet for the countries
    geom_bar(stat = "identity", position = position_dodge2(), aes(fill = crop, alpha = factor(alphatest))) +
    scale_fill_manual(values = p2)+
    scale_y_log10() +
    facet_wrap(~country) + # creating the separate boxes
    theme(strip.text.x = element_text(margin = margin(2,0,2,0), size = 12), strip.background = element_rect(colour = "black", fill = "gray93"),
          panel.border = element_rect(colour = "black", fill = NA)) + # specifying the appearance of the facet wrap
    theme(text = element_text(family="sans"), # sans serif
          axis.title.y = element_text(size = 12, vjust = 5, face = "bold"), # ensuring position of axis label is away from the ticks
          axis.text.y = element_text(size = 12),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = 12),
          legend.title = element_blank(), # removing the legend title as unnecessary here
          legend.text = element_blank(),
          legend.key = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(color = "gray73"), # adding the lines to show the log scale
          panel.grid.minor.y = element_line(color = "gray93"),
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20)) + # expanding the margins so the axis title isn't cut
    labs(x = "", y = "Biodiversity Pressure (species * ha / tonnes)")+ # expanding the margin for the y axis, as the y axis label was being clipped
    geom_text(aes(fill = crop, label = crop), size = 5, colour = "black", angle = 90, vjust = 0, hjust = -0.05, position = position_dodge2(width = 0.9, preserve = "single")))

# No labels
(fig2_BP_nl = bp_top5_allcountries2 %>%
    ggplot(aes(x = import_domestic, y = mean_bp)) + # domestic import used here as we only need that distinction as we have the facet for the countries
    geom_bar(stat = "identity", position = position_dodge2(), aes(fill = crop, alpha = factor(alphatest))) +
    scale_fill_manual(values = p2)+
    scale_y_log10() +  
    facet_wrap(~country) + # creating the separate boxes
    theme(strip.text.x = element_blank(), strip.background = element_blank(),
          panel.border = element_blank()) + # specifying the appearance of the facet wrap
    theme(text = element_text(family="sans"), # sans serif
          axis.title.y = element_blank(), # ensuring position of axis label is away from the ticks
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_blank(), # removing the legend title as unnecessary here
          legend.text = element_blank(),
          legend.key = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(color = "gray73"), # adding the lines to show the log scale
          panel.grid.minor.y = element_line(color = "gray93"),
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20))) #+ # expanding the margins so the axis title isn't cut

# BPcons
all_bpdata2_bpcons
consumption_top5_allcountries1

(crops_top5_bpcons_uk = data.frame(all_bpdata2_bpcons %>% 
                                     filter(BPcons  >0) %>%
                                     filter(ISO_reporter == "GBR") %>%
                                     filter(domestic_imported == "domestic" & crop == "apple" | 
                                              domestic_imported == "domestic" &   crop == "cabbage" | 
                                              domestic_imported == "domestic" & crop == "carrot" | 
                                              domestic_imported == "domestic" & crop == "greenpea"| 
                                              domestic_imported == "domestic" & crop == "onion" | 
                                              domestic_imported == "imported" & crop == "banana" | 
                                              domestic_imported == "imported" & crop == "grape" |
                                              domestic_imported == "imported" & crop == "orange" |
                                              domestic_imported == "imported" & crop == "apple" |
                                              domestic_imported == "imported" & crop == "tomato") %>% 
                                     group_by(domestic_imported, crop) %>% 
                                     summarise(mean_bpcons = mean(BPcons, na.rm = TRUE)))) 
(crops_top5_bpcons_southafrica = data.frame(all_bpdata2_bpcons %>% 
                                              filter(BPcons  >0) %>%
                                              filter(ISO_reporter == "ZAF") %>% 
                                              filter(domestic_imported == "domestic" & crop == "grape" |
                                                       domestic_imported == "domestic" & crop == "orange" |
                                                       domestic_imported == "domestic" & crop == "pear" |
                                                       domestic_imported == "domestic" & crop == "pumpkinetc" |
                                                       domestic_imported == "domestic" & crop == "tomato" | 
                                                       domestic_imported == "imported" & crop == "tomato" |
                                                       domestic_imported == "imported" & crop == "cauliflower" |
                                                       domestic_imported == "imported" & crop == "currant" | 
                                                       domestic_imported == "imported" & crop == "grape" |
                                                       domestic_imported == "imported" & crop == "greencorn") %>% 
                                              group_by(domestic_imported, crop) %>% 
                                              summarise(mean_bpcons = mean(BPcons, na.rm = TRUE))))
(crops_top5_bpcons_india = data.frame(all_bpdata2_bpcons %>% 
                                        filter(BPcons  >0) %>%
                                        filter(ISO_reporter == "IND") %>% 
                                        filter(domestic_imported == "domestic" & crop == "banana" | 
                                                 domestic_imported == "domestic" & crop == "cabbage" | 
                                                 domestic_imported == "domestic" & crop == "cauliflower" |
                                                 domestic_imported == "domestic" & crop == "eggplant" |
                                                 domestic_imported == "domestic" & crop == "mango" |
                                                 domestic_imported == "imported" & crop == "date" |
                                                 domestic_imported == "imported" & crop == "garlic" | 
                                                 domestic_imported == "imported" & crop == "cabbage" |
                                                 domestic_imported == "imported" & crop == "grape" |
                                                 domestic_imported == "imported" & crop == "greenpea") %>% 
                                        group_by(domestic_imported, crop) %>% 
                                        summarise(mean_bpcons = mean(BPcons, na.rm = TRUE))))
crops_top5_bpcons_india$country = "IND"
crops_top5_bpcons_southafrica$country = "ZAF"
crops_top5_bpcons_uk$country = "GBR"
bpcons_top5_allcountries = rbind(crops_top5_bpcons_india, crops_top5_bpcons_southafrica, 
                                 crops_top5_bpcons_uk)
bpcons_top5_allcountries = na.omit(bpcons_top5_allcountries)


unique(sort(bpcons_top5_allcountries$crop))

p2 = c("chartreuse", "gold", "darkolivegreen1", "tan3", "bisque", "deeppink2", "saddlebrown", "purple4", "palegreen2",
       "plum3", "peachpuff1", "springgreen1", "lightskyblue", "steelblue",
       "darkorange1", "turquoise", "orangered", "red1")

colnames(bpcons_top5_allcountries) = c("import_domestic", "crop", "mean_bpcons", "country")

bpcons_top5_allcountries1 = bpcons_top5_allcountries
bpcons_top5_allcountries1$alphatest = as.factor(ifelse(bpcons_top5_allcountries1$crop == "currant", 0.1, 1))

(fig2_BPcons = bpcons_top5_allcountries1 %>%
    ggplot(aes(x = import_domestic, y = mean_bpcons)) + # domestic import used here as we only need that distinction as we have the facet for the countries
    geom_bar(stat = "identity", position = position_dodge2(), aes(fill = crop, alpha = factor(alphatest))) +
    scale_fill_manual(values = p2)+
    scale_y_log10() + 
    facet_wrap(~country) + # creating the separate boxes
    theme(strip.text.x = element_text(margin = margin(2,0,2,0), size = 12), strip.background = element_rect(colour = "black", fill = "gray93"),
          panel.border = element_rect(colour = "black", fill = NA)) + # specifying the appearance of the facet wrap
    theme(text = element_text(family="sans"), # sans serif
          axis.title.y = element_text(size = 12, vjust = 5, face = "bold"), # ensuring position of axis label is away from the ticks
          axis.text.y = element_text(size = 12),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = 12),
          legend.title = element_blank(), # removing the legend title as unnecessary here
          legend.text = element_blank(),
          legend.key = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(color = "gray73"), # adding the lines to show the log scale
          panel.grid.minor.y = element_line(color = "gray93"),
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20)) + # expanding the margins so the axis title isn't cut
    labs(x = "", y = "BPcons")+ # expanding the margin for the y axis, as the y axis label was being clipped
    geom_text(aes(fill = crop, label = crop), size = 5, colour = "black", angle = 90, vjust = 0, hjust = -0.05, position = position_dodge2(width = 0.9, preserve = "single")))

## No labels
(fig2_BPcons_nl = bpcons_top5_allcountries1 %>%
    ggplot(aes(x = import_domestic, y = mean_bpcons)) + # domestic import used here as we only need that distinction as we have the facet for the countries
    geom_bar(stat = "identity", position = position_dodge2(), aes(fill = crop, alpha = factor(alphatest))) +
    scale_fill_manual(values = p2)+
    scale_y_log10() +  
    facet_wrap(~country) + # creating the separate boxes
    theme(strip.text.x = element_blank(), strip.background = element_blank(),
          panel.border = element_blank()) + # specifying the appearance of the facet wrap
    theme(text = element_text(family="sans"), # sans serif
          axis.title.y = element_blank(), # ensuring position of axis label is away from the ticks
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_blank(), # removing the legend title as unnecessary here
          legend.text = element_blank(),
          legend.key = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(color = "gray73"), # adding the lines to show the log scale
          panel.grid.minor.y = element_line(color = "gray93"),
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20))) #+ # expanding the margins so the axis title isn't cut

###################################################################################################################
###################################################################################################################
# 11. Figure 4
###################################################################################################################
###################################################################################################################

### Next, I need the highest BPcons crops which are produced domestically and imported, ready for a figure:
(bpcons_ordered = all_bpdata2_bpcons %>% 
    group_by(ISO_reporter) %>% 
    arrange(desc(BPcons)) %>% 
    slice(1:10))
# UK:
# 1. sourcherry - but this isn't produced domestically
# 2. plantain - not produced domestically
# 3. date - not produced domestically
# 4. tomato (1)
# 5. orange  - not produced domestically
# 6. grape (2)
# 7. banana - not produced domestically
# 8. greenpea (3)
# 9. cauliflower (4)
# 10. apple (5)

# India:
# 1. date (1)
# 2. mango (2)
# 3. tomato (3)
# 4. eggplant - no imports
# 5. pumpkinetc - no imports
# 6. onion (4)
# 7. okra - no imports
# 8. cauliflower - no imports
# 9. cabbage (5)

# South Africa:
# 1. date (1)
# 2. grape (2)
# 3. orange (3)
# 4. tomato (4)
# 5. pumpkinetc - no imports
# 6. onion (5)

# Now I need the top 5 trade partners for these top BPcons crops:
View(all_country_imports_and_bp)
uk_bp_fig_data_imports_top5tonnes = data.frame(all_country_imports_and_bp %>% 
                                                 filter(ISO_reporter == "GBR") %>% 
                                                 filter(dmi_tonnes_average_2000 >0) %>% 
                                                 filter(crop == "tomato" | crop == "grape" |
                                                          crop == "greenpea" | crop == "cauliflower" |
                                                          crop == "apple") %>% 
                                                 group_by(crop) %>% 
                                                 arrange(desc(dmi_tonnes_average_2000)) %>% 
                                                 slice(1:5))

southafrica_bp_fig_data_imports_top5tonnes = data.frame(all_country_imports_and_bp %>% 
                                                          filter(ISO_reporter == "ZAF") %>% 
                                                          filter(dmi_tonnes_average_2000 >0) %>% 
                                                          filter(crop == "date" | crop == "grape" |
                                                                   crop == "orange" | crop == "tomato" |
                                                                   crop == "onion") %>% 
                                                          group_by(crop) %>% 
                                                          arrange(desc(dmi_tonnes_average_2000)) %>% 
                                                          slice(1:5))

india_bp_fig_data_imports_top5tonnes = data.frame(all_country_imports_and_bp %>% 
                                                    filter(ISO_reporter == "IND") %>% 
                                                    filter(dmi_tonnes_average_2000 >0) %>% 
                                                    filter(crop == "date" | crop == "mango" |
                                                             crop == "tomato" | crop == "onion" |
                                                             crop == "cabbage") %>% 
                                                    group_by(crop) %>% 
                                                    arrange(desc(dmi_tonnes_average_2000)) %>% 
                                                    slice(1:5))

unique(sort(uk_bp_fig_data_imports_top5tonnes$ISO_partner))

# Setting up a colour scale for the countries, so they're consistent across plots.
colours_uk_partners = c("sienna1", "yellow3", "lightgoldenrod", "turquoise1",
                        "limegreen", "tan1", "deepskyblue", "blue1",
                        "darkorchid", "mediumspringgreen", "red1",
                        "royalblue1", "orchid1", "yellow1", "slateblue1")

unique(sort(southafrica_bp_fig_data_imports_top5tonnes$ISO_partner))
colours_southafrica_partners = c("gold", "darkred", "cadetblue1", "forestgreen",
                                 "firebrick1", "lightgoldenrod", "turquoise1",
                                 "tan1", "lawngreen", "midnightblue", "blue1",
                                 "red4", "hotpink4", "mediumspringgreen", "red1",
                                 "royalblue1", "mediumslateblue", "chocolate1", "orchid1",
                                 "yellow1", "plum")

unique(sort(india_bp_fig_data_imports_top5tonnes$ISO_partner))
colours_india_partners = c("darkred", "pink", "seagreen", "firebrick1",
                           "lawngreen", "blue1", "salmon", "mediumspringgreen",
                           "seagreen1", "aquamarine4", "royalblue1", "violetred2",
                           "chocolate1", "olivedrab", "orchid1", "yellow1")

# Chile = sienna 1
# France = limegreen 
# Germany = yellow 3 =
# Greece = tan1
# Italy = blue1 
# Netherlands = mediumspringgreen=
# New Zealand = red1 
# Portugal = royalblue1 
# South Africa = slateblue1 
# Spain = turquoise1 
# Turkey = orchid1 
# USA = yellow1 
# Guatemala  = deepskyblue
# Kenya = darkorchid
# Angola = gold
# Argentina = cadetblue1
# Austria = dodgerblue
# Brazil = forestgreen
# Bulgaria = darkseagreen1
# China, mainland = firebrick1
# Egypt = lightgoldenrod
# India - hotpink1
# Iran (Islamic Republic of) = lawngreen
# Israel = midnightblue
# Mozambique = hotpink4
# Poland = mediumslateblue
# Republic of Korea = red4
# Zimbabwe = plum
# Australia = pink
# Bangladesh = seagreen
# Myanmar = salmon
# Nepal = seagreen1
# Pakistan = royalblue1
# Philippines = violetred2
# Thailand = olivedrab
# United Arab Emirates = darkred
# Saudi Arabia = chocolate1
# Oman = aquamarine4

uk_bp_and_consumption_top5 = filter(uk_bp_and_consumption, crop == "tomato" | crop == "grape" |
                                      crop == "greenpea" | crop == "cauliflower" |
                                      crop == "apple")

summary(uk_bp_fig_data_imports_top5tonnes$pressure_dry) # max 489.969 

(figure4_uk1 = uk_bp_fig_data_imports_top5tonnes %>%
    filter(pressure_dry  > 0) %>% 
    ggplot(aes(x = ISO_partner, y = pressure_dry)) +
    geom_bar(stat = "identity", position = "dodge", aes(fill = ISO_partner)) +
    theme_classic() +
    facet_wrap(~crop, scales = "free_x") + # scales = free sets the x axis up in a way that it can have different countries per facet
    theme(strip.text.x = element_text(margin = margin(2,5,5,2), size = 12, face = "bold"), strip.background = element_rect(colour = "black", fill = "gray93"),
          panel.border = element_rect(colour = "black", fill = NA)) + # specifying the appearance of the facet wrap
    theme(legend.position = "none") +
    scale_y_continuous(limits = c(0, 600), label = scales::comma) + # making sure the y axis numbers have comma separation
    scale_fill_manual(values = colours_uk_partners)+
    theme(text = element_text(family="sans"), # sans serif
          axis.title.y = element_text(size = 12, vjust = 5, face = "bold"), # ensuring position of axis label is away from the ticks
          axis.text.y = element_text(size = 11),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_blank(), # removing the legend title as unnecessary here
          legend.text = element_text(size = 11),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "gray93"), # adding the lines to show the log scale
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20)) + # expanding the margins so the axis title isn't cut
    geom_hline(data = uk_bp_and_consumption_top5, aes(yintercept = pressure_dry), linewidth = 1.1,
               colour = "gray50")+
    geom_text(aes(label = ISO_partner, vjust = 0, hjust = -0.2, angle = 90, size = 15, fontface = "bold"),
              colour = "black") +
    labs(x = "", y = "BP")) # expanding the margin for the y axis, as the y axis label was being clipped

SouthAfrica_bp_and_consumption_top5 = filter(SouthAfrica_bp_and_consumption, crop == "date" | crop == "grape" |
                                               crop == "orange" | crop == "tomato" |
                                               crop == "onion")

summary(southafrica_bp_fig_data_imports_top5tonnes$pressure_dry) # max 851.467

(figure4_southafrica1 = southafrica_bp_fig_data_imports_top5tonnes %>%
    filter(pressure_dry  > 0) %>% 
    ggplot(aes(x = ISO_partner, y = pressure_dry)) +
    geom_bar(stat = "identity", position = "dodge", aes(fill = ISO_partner)) +
    theme_classic() +
    facet_wrap(~crop, scales = "free_x") + # scales = free sets the x axis up in a way that it can have different countries per facet
    theme(strip.text.x = element_text(margin = margin(2,5,5,2), size = 12, face = "bold"), strip.background = element_rect(colour = "black", fill = "gray93"),
          panel.border = element_rect(colour = "black", fill = NA)) + # specifying the appearance of the facet wrap
    theme(legend.position = "none") +
    scale_y_continuous(limits = c(0, 1025), label = scales::comma) + # making sure the y axis numbers have comma separation
    scale_fill_manual(values = colours_southafrica_partners)+
    theme(text = element_text(family="sans"), # sans serif
          axis.title.y = element_text(size = 12, vjust = 5, face = "bold"), # ensuring position of axis label is away from the ticks
          axis.text.y = element_text(size = 11),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_blank(), # removing the legend title as unnecessary here
          legend.text = element_text(size = 11),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "gray93"), # adding the lines to show the log scale
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20)) + # expanding the margins so the axis title isn't cut
    geom_hline(data = SouthAfrica_bp_and_consumption_top5, aes(yintercept = pressure_dry), linewidth = 1.1,
               colour = "gray50")+
    geom_text(aes(label = ISO_partner, vjust = 0, hjust = -0.2, angle = 90, size = 15, fontface = "bold"),
              colour = "black") +
    labs(x = "", y = "BP")) # expanding the margin for the y axis, as the y axis label was being clipped

india_bp_and_consumption_top5 = filter(India_bp_and_consumption, crop == "date" | crop == "mango" |
                                         crop == "tomato" | crop == "onion" |
                                         crop == "cabbage")

(figure4_india1 = india_bp_fig_data_imports_top5tonnes %>%
    filter(pressure_dry  > 0) %>% 
    ggplot(aes(x = ISO_partner, y = pressure_dry)) +
    geom_bar(stat = "identity", position = "dodge", aes(fill = ISO_partner)) +
    theme_classic() +
    facet_wrap(~crop, scales = "free_x") + # scales = free sets the x axis up in a way that it can have different countries per facet
    theme(strip.text.x = element_text(margin = margin(2,5,5,2), size = 12, face = "bold"), strip.background = element_rect(colour = "black", fill = "gray93"),
          panel.border = element_rect(colour = "black", fill = NA)) + # specifying the appearance of the facet wrap
    theme(legend.position = "none") +
    scale_y_continuous(limits = c(0, 1000), label = scales::comma) + # making sure the y axis numbers have comma separation
    scale_fill_manual(values = colours_india_partners)+
    theme(text = element_text(family="sans"), # sans serif
          axis.title.y = element_text(size = 12, vjust = 5, face = "bold"), # ensuring position of axis label is away from the ticks
          axis.text.y = element_text(size = 11),
          axis.ticks.length.x = unit(-0.1, "cm"), # length of tick marks - negative sign places ticks inwards
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_blank(), # removing the legend title as unnecessary here
          legend.text = element_text(size = 11),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "gray93"), # adding the lines to show the log scale
          axis.line = element_line(color = "black"),
          plot.margin = margin(10,10,10,20)) + # expanding the margins so the axis title isn't cut
    geom_hline(data = india_bp_and_consumption_top5, aes(yintercept = pressure_dry), linewidth = 1.1,
               colour = "gray50")+
    geom_text(aes(label = ISO_partner, vjust = 0, hjust = -0.2, angle = 90, size = 15, fontface = "bold"),
              colour = "black") +
    labs(x = "", y = "BP", size = 12)) # expanding the margin for the y axis, as the y axis label was being clipped


###################################################################################################################
###################################################################################################################
# 12. Figure 5
###################################################################################################################
###################################################################################################################

#### Imports and production trends through time (since 2003).

uk_production_minus_exports_post2003 = filter(tradedatapost2003, reporter == "United Kingdom" & partner == "United Kingdom")
head(uk_production_minus_exports_post2003)
uk_imports_post2003 = filter(tradedatapost2003, reporter == "United Kingdom" & partner != "United Kingdom")
head(uk_imports_post2003)

uk_imports_per_year = data.frame(uk_imports_post2003 %>% 
                                   filter(crop == "tomato" | crop == "grape" | crop == "greenpea" |
                                            crop == "cauliflower" | crop == "apple") %>% 
                                   group_by(crop, year) %>% 
                                   summarise(total_tonnes = sum(as.numeric(dmi_tonnes), na.rm = TRUE), .groups = "keep"))

unique(uk_imports_per_year$year)
summary(uk_imports_per_year$total_tonnes)
str(uk_imports_per_year)
uk_imports_per_year$year = as.numeric(uk_imports_per_year$year)

(ukimportplot = uk_imports_per_year %>% 
    ggplot(aes(x = factor(year), y = total_tonnes, group = crop,linetype = crop, colour = crop)) +
    geom_smooth(fill = "lightgrey") +
    scale_colour_manual(values = c("chartreuse", "bisque3", "plum3", "springgreen1", "red1"))+
    scale_x_discrete(labels = c("1997", "", "1999", "", "2001", "", "2003", "",
                                "2005", "", "2007", "", "2009", "", "2011",
                                "", "2013", "", "2015", "", "2017")) +
    scale_y_continuous(label = scales::comma) +
    theme(axis.text.y = element_text(size = 11))+
    labs(title = "b)", x = "Year", y = "UK imports, tonnes") +
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, size = 11, hjust = 1),
          legend.text = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          legend.title = element_blank()) +
    geom_vline(xintercept = "2003", linetype = 2))

uk_production_minus_exports_peryear = data.frame(uk_production_minus_exports_post2003 %>% 
                                                   filter(crop == "tomato" | crop == "grape" | crop == "greenpea" |
                                                            crop == "cauliflower" | crop == "apple") %>% 
                                                   group_by(crop, year) %>% 
                                                   summarise(total_tonnes = sum(as.numeric(dmi_tonnes), na.rm = TRUE), .groups = "keep"))

unique(uk_production_minus_exports_peryear$year)
summary(uk_production_minus_exports_peryear$total_tonnes)
str(uk_production_minus_exports_peryear)

(ukproductionplot = uk_production_minus_exports_peryear %>% 
    ggplot(aes(x = factor(year), y = total_tonnes, group = crop, linetype = crop, colour = crop)) +
    geom_smooth(fill = "lightgrey") +
    scale_colour_manual(values = c("chartreuse", "bisque3", "plum3", "springgreen1", "red1"))+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, size = 11, hjust = 1),
          legend.text = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          legend.title = element_blank()) +
    scale_x_discrete(labels = c("1997", "", "1999", "", "2001", "", "2003", "",
                                "2005", "", "2007", "", "2009", "", "2011",
                                "", "2013", "", "2015", "", "2017")) +
    scale_y_continuous(label = scales::comma) +
    theme(axis.text.y = element_text(size = 11))+
    labs(title = "a)", x = "Year", y = "UK production - exports, tonnes") +
    geom_vline(xintercept = "2003", linetype = 2))

southafrica_production_minus_exports_post2003 = filter(tradedatapost2003, reporter == "South Africa" & partner == "South Africa")
head(southafrica_production_minus_exports_post2003)
southafrica_imports_post2003 = filter(tradedatapost2003, reporter == "South Africa" & partner != "South Africa")
head(southafrica_imports_post2003)

southafrica_imports_per_year = data.frame(southafrica_imports_post2003 %>% 
                                            filter(crop == "date" | crop == "grape" | crop == "orange" |
                                                     crop == "tomato" | crop == "onion") %>% 
                                            group_by(crop, year) %>% 
                                            summarise(total_tonnes = sum(as.numeric(dmi_tonnes), na.rm = TRUE), .groups = "keep"))

unique(southafrica_imports_per_year$year)
summary(southafrica_imports_per_year$total_tonnes)
str(southafrica_imports_per_year)
southafrica_imports_per_year$year = as.numeric(southafrica_imports_per_year$year)

(southafricaimportplot = southafrica_imports_per_year %>% 
    ggplot(aes(x = factor(year), y = total_tonnes, group = crop, linetype = crop, colour = crop)) +
    geom_smooth(fill = "lightgrey") +
    scale_colour_manual(values = c("saddlebrown", "plum3", "darkorange1", "red1", "steelblue"))+
    scale_x_discrete(labels = c("1997", "", "1999", "", "2001", "", "2003", "",
                                "2005", "", "2007", "", "2009", "", "2011",
                                "", "2013", "", "2015", "", "2017")) +
    scale_y_continuous(label = scales::comma) +
    theme(axis.text.y = element_text(size = 11))+
    labs(title = "f)", x = "Year", y = "South Africa imports, tonnes") +
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, size = 11, hjust = 1),
          legend.text = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          legend.title = element_blank()) +
    geom_vline(xintercept = "2003", linetype = 2))

southafrica_production_minus_exports_peryear = data.frame(southafrica_production_minus_exports_post2003 %>% 
                                                            filter(crop == "date" | crop == "grape" | crop == "orange" |
                                                                     crop == "tomato" | crop == "onion") %>% 
                                                            group_by(crop, year) %>% 
                                                            summarise(total_tonnes = sum(as.numeric(dmi_tonnes), na.rm = TRUE), .groups = "keep"))

unique(southafrica_production_minus_exports_peryear$year)
summary(southafrica_production_minus_exports_peryear$total_tonnes)
str(southafrica_production_minus_exports_peryear)

unique(sort(southafrica_production_minus_exports_peryear$crop))

(southafricaproductionplot = southafrica_production_minus_exports_peryear %>% 
    ggplot(aes(x = factor(year), y = total_tonnes, group = crop, linetype = crop, colour = crop)) +
    geom_smooth(fill = "lightgrey") +
    scale_colour_manual(values = c("saddlebrown", "plum3", "steelblue", "darkorange1", "red1"))+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, size = 11, hjust = 1),
          legend.text = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          legend.title = element_blank()) +
    scale_x_discrete(labels = c("1997", "", "1999", "", "2001", "", "2003", "",
                                "2005", "", "2007", "", "2009", "", "2011",
                                "", "2013", "", "2015", "", "2017")) +
    scale_y_continuous(label = scales::comma) +
    theme(axis.text.y = element_text(size = 11))+
    labs(title = "e)", x = "Year", y = "South Africa production - exports, tonnes") +
    geom_vline(xintercept = "2003", linetype = 2))

india_production_minus_exports_post2003 = filter(tradedatapost2003, reporter == "India" & partner == "India")
head(india_production_minus_exports_post2003)
india_imports_post2003 = filter(tradedatapost2003, reporter == "India" & partner != "India")
head(india_imports_post2003)

india_imports_per_year = data.frame(india_imports_post2003 %>% 
                                      filter(crop == "date" | crop == "mango" | crop == "tomato" |
                                               crop == "onion" | crop == "cabbage") %>% 
                                      group_by(crop, year) %>% 
                                      summarise(total_tonnes = sum(as.numeric(dmi_tonnes), na.rm = TRUE), .groups = "keep"))

unique(india_imports_per_year$year)
summary(india_imports_per_year$total_tonnes)
str(india_imports_per_year)
india_imports_per_year$year = as.numeric(india_imports_per_year$year)
unique(india_imports_per_year$crop)

(indiaimportplot = india_imports_per_year %>% 
    ggplot(aes(x = factor(year), y = total_tonnes, group = crop, linetype = crop, colour = crop)) +
    geom_smooth(fill = "lightgrey") +
    scale_colour_manual(values = c( "darkolivegreen","saddlebrown", "lightskyblue", "steelblue", "red1"))+
    scale_x_discrete(labels = c("1997", "", "1999", "", "2001", "", "2003", "",
                                "2005", "", "2007", "", "2009", "", "2011",
                                "", "2013", "", "2015", "", "2017")) +
    scale_y_continuous(label = scales::comma) +
    theme(axis.text.y = element_text(size = 11))+
    labs(title = "d)", x = "Year", y = "India imports, tonnes") +
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, size = 11, hjust = 1),
          legend.text = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          legend.title = element_blank()) +
    geom_vline(xintercept = "2003", linetype = 2))

india_production_minus_exports_peryear = data.frame(india_production_minus_exports_post2003 %>% 
                                                      filter(crop == "date" | crop == "mango" | crop == "tomato" |
                                                               crop == "onion" | crop == "cabbage") %>% 
                                                      group_by(crop, year) %>% 
                                                      summarise(total_tonnes = sum(as.numeric(dmi_tonnes), na.rm = TRUE), .groups = "keep"))

unique(india_production_minus_exports_peryear$year)
summary(india_production_minus_exports_peryear$total_tonnes)
str(india_production_minus_exports_peryear)

(indiaproductionplot = india_production_minus_exports_peryear %>% 
    ggplot(aes(x = factor(year), y = total_tonnes, group = crop, linetype = crop, colour = crop)) +
    geom_smooth(fill = "lightgrey") +
    scale_colour_manual(values = c( "darkolivegreen","saddlebrown", "lightskyblue", "steelblue", "red1"))+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, size = 11, hjust = 1),
          legend.text = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          legend.title = element_blank()) +
    scale_x_discrete(labels = c("1997", "", "1999", "", "2001", "", "2003", "",
                                "2005", "", "2007", "", "2009", "", "2011",
                                "", "2013", "", "2015", "", "2017")) +
    scale_y_continuous(label = scales::comma) +
    theme(axis.text.y = element_text(size = 11))+
    labs(title = "c)", x = "Year", y = "India production - exports, tonnes") +
    geom_vline(xintercept = "2003", linetype = 2))


library(ggpubr)
test1 = ggarrange(ukproductionplot, ukimportplot,
                  indiaproductionplot, indiaimportplot, 
                  southafricaproductionplot, southafricaimportplot,
                  ncol = 2, nrow = 3)
test1

###################################################################################################################
###################################################################################################################
# 13. Figure 6
###################################################################################################################
###################################################################################################################

### Map example crops BP per focal country.
# Choosing some of the ones that have a lower pressure when grown domestically:
# UK - tomatoes/peas
# South Africa - oranges
# India - cabbages

outDir_pressure = "SHEFS/SHEFS_Sept2023_Update/3_BP/"
bp_files =paste(outDir_pressure, dir(path = outDir_pressure, recursive = TRUE), sep = "")
bp_files

GlobalExtent <- extent(-180,180,-90,90)
reference_GBR = readOGR("Country Outlines/gadm36_GBR_0.shp")
UKExtent <- extent(reference_GBR)
reference_ZAF = readOGR("Country Outlines/gadm36_ZAF_0.shp")
SouthAfricaExtent <- extent(reference_ZAF)
reference_IND = readOGR("Country Outlines/gadm36_IND_0.shp")
IndiaExtent <- extent(reference_IND)
outprj = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # updated code for the proj6 transition

UKExtent 
outname_GBR = "GBR_"
SouthAfricaExtent
outname_ZAF = "ZAF_"
IndiaExtent
outname_IND = "IND_"

# This function takes a raster, ensures it has our desired projection, crops it to match the reference shapefile 
# and then the mask is needed to ensure it cuts around the shapefile, rather than forming a box of similar extent.
# Credit here also needs to go to Dr Tim Newbold, UCL, as I also used this part of the script for mapping in another project.
cropping_bp_IND <- function(x){
  r <- raster::raster(x)
  raster::crs(r) = outprj
  r = raster::extend(r, GlobalExtent)
  r[is.na(r[])]<-0
  r <- (raster::crop(r,IndiaExtent))
  r <- (raster::mask(x = r, mask = reference_IND))
  if (class(r) != "try-error"){
    return(r)
  }
}

cropping_bp_GBR <- function(x){
  r <- raster::raster(x)
  raster::crs(r) = outprj
  r = raster::extend(r, GlobalExtent)
  r[is.na(r[])]<-0
  r <- (raster::crop(r,UKExtent))
  r <- (raster::mask(x = r, mask = reference_GBR))
  if (class(r) != "try-error"){
    return(r)
  }
}

cropping_bp_ZAF <- function(x){
  r <- raster::raster(x)
  raster::crs(r) = outprj
  r = raster::extend(r, GlobalExtent)
  r[is.na(r[])]<-0
  r <- (raster::crop(r,SouthAfricaExtent))
  r <- (raster::mask(x = r, mask = reference_ZAF))
  if (class(r) != "try-error"){
    return(r)
  }
}

bp_focal_dir = "SHEFS/SHEFS_Sept2023_Update/7_Pressure_Dry_per_focal/"

# Commenting out as slow to run:
# 
# India_BP_cropped = stack(lapply(1:53,function(i){
#   cropfile = bp_files[i]
#   cropfile_raster <- cropping_bp_IND(cropfile)
#   outPath <- paste0(bp_focal_dir, outname_IND, basename(cropfile))
#   writeRaster(x=cropfile_raster, filename = outPath, format = "GTiff", overwrite = TRUE)
# }))
# 
# UK_BP_cropped = stack(lapply(1:53,function(i){
#   cropfile = bp_files[i]
#   cropfile_raster <- cropping_bp_GBR(cropfile)
#   outPath <- paste0(bp_focal_dir, outname_GBR, basename(cropfile))
#   writeRaster(x=cropfile_raster, filename = outPath, format = "GTiff", overwrite = TRUE)
# }))
# 
# SouthAfrica_BP_cropped = stack(lapply(1:53,function(i){
#   cropfile = bp_files[i]
#   cropfile_raster <- cropping_bp_ZAF(cropfile)
#   outPath <- paste0(bp_focal_dir, outname_ZAF, basename(cropfile))
#   writeRaster(x=cropfile_raster, filename = outPath, format = "GTiff", overwrite = TRUE)
# }))

## These data can now be mapped in ArcMap.