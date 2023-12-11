##
## Date: 11th December 2023
## Author: Abbie S. A. Chapman
## Title: R script to accompany 'Quantifying the biodiversity pressures of fruit and vegetable consumption in the 
## United Kingdom, India, and South Africa.'
##

## 3_Calculating_Consumption_Based_Biodiversity_Pressure

#####
# 1. Bringing in the trade data for calculating consumption-based biodiversity pressure.

# Please note that this can be very slow and memory-intensive as the trade data are large.

# Bringing in the trade data and subsetting to fruit and veg 
cropdir = "SHEFS/6_Trade_Matrices/Corrected_trade_long/" # bringing in the long format data to try instead of the wide
outdir = "SHEFS/SHEFS_March2023_Update/19July2023/"
cropdir = "SHEFS/6_Trade_Matrices/Corrected_trade_long/" # bringing in the long format data to try instead of the wide

cropdata = list.files(path = cropdir, pattern="*.csv", all.files = TRUE, full.names = TRUE)
cropdata
for (i in 1:length(cropdata)) assign(cropdata[i], read.csv(cropdata[i]))

yearslist = c("1997", "1998", "1999", "2000", "2001", "2002", "2003")
cropdata_sub = lapply(yearslist, FUN = function(x) {
  cropdata[grepl(x, cropdata)]})
cropdata_sub
cropdata_sub1 = unlist(cropdata_sub)

croplist = read.csv(paste0("SHEFS/0_HarvestedArea_Yield_EarthStat/", "EarthStat_Crops.csv"))
croplist
fruitveg = filter(croplist, cropgroup == "Vegetables&Melons" | cropgroup == "Fruit")
fruitveg
fruitveg = fruitveg %>% filter(!grepl(", nes", crop_longname))
fruitveg = fruitveg %>% filter(!grepl("Nes", crop_longname))
fruitveg = fruitveg %>% filter(!grepl(" nes", crop_longname))
fruitveg
colnames(fruitveg) = c("crop", "crop_longname", "cropgroup")

cropdata_sub2 = lapply(fruitveg$crop, FUN = function(x) {
  cropdata_sub1[grepl(x, cropdata_sub1)]
})
cropdata_sub2
cropdata_sub3 = unlist(cropdata_sub2)
cropdata = cropdata_sub3
str(cropdata)


#### Tests are folded down below:
######
# test = read.csv(cropdata[1])
# unique(test$reporter)
# unique(pressure_dry_percountry$country)
# setdiff(test$reporter, pressure_dry_percountry$country) 
# As the pressure_dry_percountry is a single file, whereas there are many csvs of the test to read in,
# I propose matching the names in pressure dry to the cropdata file names.
# Please note that country name changes here do not reflect the accurate names of each country. They are being used
# for analysis purposes only and do not reflect the up-to-date names for each country. We apologise for this.
# setdiff(pressure_dry_percountry$country, test$reporter) 
# setdiff(test$reporter, test$partner) # at least the cols and rows in these data are consistent (this is not the case with the wide when I manipulated that before)

######

pressure_dry_percountry[pressure_dry_percountry == "Bolivia"] <- "Bolivia (Plurinational State of)"
pressure_dry_percountry[pressure_dry_percountry == "China"] <- "China, mainland"
pressure_dry_percountry[pressure_dry_percountry == "Cote d'Ivoire"] <- "Ivory Coast"
pressure_dry_percountry[pressure_dry_percountry == "Czech Republic"] <- "Czechia"
pressure_dry_percountry[pressure_dry_percountry == "Hong Kong"] <- "China, Hong Kong SAR"
pressure_dry_percountry[pressure_dry_percountry == "Taiwan"] <- "China, Taiwan Province of"
pressure_dry_percountry[pressure_dry_percountry == "United States"] <- "United States of America"
pressure_dry_percountry[pressure_dry_percountry == "Venezuela"] <- "Venezuela (Bolivarian Republic of)"
pressure_dry_percountry[pressure_dry_percountry == "Cape Verde"] <- "Cabo Verde"
pressure_dry_percountry[pressure_dry_percountry == "Slovakia"] <- "Czechoslovakia"
pressure_dry_percountry[pressure_dry_percountry == "Micronesia, Federated States of"] <- "Micronesia (Federated States of)"
pressure_dry_percountry[pressure_dry_percountry == "Korea, Republic of"] <- "Republic of Korea"
pressure_dry_percountry[pressure_dry_percountry == "Libyan Arab Jamahiriya"] <- "Libya"
pressure_dry_percountry[pressure_dry_percountry == "The former Yugoslav Republic of Macedonia"] <- "North Macedonia"
pressure_dry_percountry[pressure_dry_percountry == "Burma"] <- "Myanmar"
pressure_dry_percountry[pressure_dry_percountry == "Serbia and Montenegro"] <- "Montenegro"
pressure_dry_percountry[pressure_dry_percountry == "Korea, Democratic People's Republic of"] <- "Democratic People's Republic of Korea"
pressure_dry_percountry[pressure_dry_percountry == "Russia"] <- "Russian Federation"
# pressure_dry_percountry[pressure_dry_percountry == "Serbia and Montenegro"] <- "Serbia"
# as Serbia and Montenegro get joined together and Serbia is all set to 1, I am not going to have the Serbia overwrite of this grouping
# pressure_dry_percountry[pressure_dry_percountry == "Sudan"] <- "South Sudan" # South Sudan also has this issue, so has to be captured under Sudan
# pressure_dry_percountry[pressure_dry_percountry == "Bolivia"] <- "USSR" # and the USSR
pressure_dry_percountry[pressure_dry_percountry == "Swaziland"] <- "Eswatini"
pressure_dry_percountry[pressure_dry_percountry == "Bolivia"] <- "Yugoslav SFR"# and this
pressure_dry_percountry[pressure_dry_percountry == "Guinea-Bissau"] <- "Guinea Bissau"
pressure_dry_percountry[pressure_dry_percountry == "Iran (Islamic Republic of)"] <- "Iran  Islamic Republic of "
pressure_dry_percountry[pressure_dry_percountry == "Lao People's Democratic Republic"] <- "Lao People s Democratic Republic"
pressure_dry_percountry[pressure_dry_percountry == "Timor-Leste"] <- "Timor Leste"

setdiff(test$reporter, pressure_dry_percountry$country) # these are ones we are expecting to have issues, so I will need to delete these
setdiff(pressure_dry_percountry$country, test$reporter)  # some of these differences will be because the countries don't trade apples

### Tests/checks folded below
######
#
# #head(test)
# test1 = filter(test, reporter == "United Kingdom" & partner == "United Kingdom")
# test1
# test2 = filter(test, reporter == "United Kingdom" & partner != "United Kingdom")
# test2
# test3 = filter(test, reporter != "United Kingdom" & partner == "United Kingdom")
# test3
#
# a.test = read.csv(cropdata[1])
# b.test = a.test[complete.cases(a.test),]
# a.cropname = stringr::str_remove(cropdata[1], "SHEFS/6_Trade_Matrices/Corrected_trade_long/")
# a.cropname1 = stringr::str_remove(a.cropname, ".csv")
# a.cropname2 = stringr::str_split_fixed(a.cropname1,"_", 2) # check this works as this is new - yep
# c.test = cbind(b.test, a.cropname2)
# str(b.test)
# str(c.test)# 1444 obs
# # what happens if I add in more than one cropfile and rbind them, as I want to do this I think, but want to check first
# a.test.crop2 = read.csv(cropdata[2])
# b.test.crop2 = a.test.crop2[complete.cases(a.test.crop2),]
# a.cropname_crop2 = stringr::str_remove(cropdata[2], "SHEFS/6_Trade_Matrices/Corrected_trade_long/")
# a.cropname1_crop2 = stringr::str_remove(a.cropname_crop2, ".csv")
# a.cropname2_crop2 = stringr::str_split_fixed(a.cropname1_crop2,"_", 2) # check this works as this is new - yep
# c.test.crop2 = cbind(b.test.crop2, a.cropname2_crop2)
# c.test.crop2
# overall_test = rbind(c.test, c.test.crop2)
# str(overall_test) # 1645 obs
# str(c.test.crop2) # 201 obs
# 1444+201 # 1645 - this has worked
# 

######


#####
# 2. Computing the consumption-based biodiversity pressure. 
# As consumption-based biodiversity pressure is focal-country-specific, I compute per focal country.

## UK data:

outdir = "SHEFS/SHEFS_Sept2023_Update/4_TradeData_and_Outputs/"
# write.csv(trade_data_c2000, paste0(outdir, "trade_data_c2000_tidied.csv"))
pressure_dry_percountry4 = filter(pressure_dry_percountry4, crop != "greenonion" & crop != "melonetc" & crop!= "pepper"
                                  & crop != "cashewapple" & crop != "stringbean") # removing these crops as they don't marry up with trade data and create NA issues
trade_data_c2000 = filter(trade_data_c2000, crop != "greenonion" & crop != "melonetc" & crop!= "pepper"
                          & crop != "cashewapple" & crop != "stringbean") # removing these crops
# As the UK, South Africa, and India don't have matching issues between wrld_simpl4 and the trade data, I can filter by country names at this point
uk_bp = filter(pressure_dry_percountry4, country == "United Kingdom") # country, crop, pressure_dry (BP)
# View(uk_bp)

uk_production_minus_exports = filter(trade_data_c2000, reporter == "United Kingdom" & partner == "United Kingdom")
uk_production_minus_exports
total_uk_production_minus_exports = sum(uk_production_minus_exports$dmi_tonnes_average_2000, na.rm = TRUE)
total_uk_production_minus_exports# 2939933
# write.csv(uk_production_minus_exports, paste0(outdir, "uk_production_minus_exports.csv"))

head(uk_bp); head(uk_production_minus_exports)
uk_bp_and_consumption = data.frame(full_join(uk_production_minus_exports, uk_bp, by = "crop")) # checked examples using View(file) and comparing, and this has worked OK
uk_bp_and_consumption$pressure_dry
uk_bp_and_consumption$bp_multiplied_by_tonnes_uk = uk_bp_and_consumption$pressure_dry * uk_bp_and_consumption$dmi_tonnes_average_2000
uk_bp_and_consumption$bp_multiplied_by_tonnes_uk # This is BPcons part 1 (the left-hand-side of the equation)
# write.csv(uk_bp_and_consumption, paste0(outdir, "uk_bp_and_consumption.csv"))
total_bp_of_uk_consumption = sum(uk_bp_and_consumption$pressure_dry, na.rm = TRUE) # 3257.688
unique(uk_bp_and_consumption$country)

uk_imports = filter(trade_data_c2000, reporter == "United Kingdom" & partner != "United Kingdom")
uk_total_imports = sum(uk_imports$dmi_tonnes_average_2000, na.rm = TRUE)
uk_total_imports # total imports of all fruits and vegetables analysed # 5758902
uk_total_imports_per_crop = data.frame(uk_imports %>% 
                                         group_by(crop) %>% 
                                         summarise(sum_imports = sum(dmi_tonnes_average_2000, na.rm = TRUE)))
# checking by adding the uk_total_imports of cauliflower
# value in the summary: 102011 is a match (check by getting the values from filtered trade data and excluding UK as a partner)
# write.csv(uk_total_imports, paste0(outdir, "uk_total_imports.csv"))
# write.csv(uk_total_imports_per_crop, paste0(outdir, "uk_total_imports_per_crop.csv"))

## Getting the proportion consumed and imported etc. for in the manuscript text:
(t1 = sum(uk_production_minus_exports$dmi_tonnes_average_2000, na.rm = T))
uk_total_consumption = t1+uk_total_imports
uk_total_consumption
(uk_imports_percent = (uk_total_imports/uk_total_consumption)*100) # 66.20314
(uk_home_percent = (t1/uk_total_consumption)*100) # 33.79686

uk_production_minus_exports
# carrots then greenpeas, then onions
uk_total_imports_per_crop
# tomatoes, then bananas then grapes
uk_consumption = full_join(uk_production_minus_exports, uk_total_imports_per_crop, by = "crop")
uk_consumption$uk_total_consumption_per_crop = uk_consumption$dmi_tonnes_average_2000 + uk_consumption$sum_imports
uk_consumption_total = sum(uk_consumption$uk_total_consumption_per_crop, na.rm = TRUE) # 8698838
# write.csv(uk_consumption, paste0(outdir, "uk_consumption.csv"))

head(uk_imports); head(pressure_dry_percountry4) # crop, reporter, partner, dmi_tonnes_average_2000 # country, crop, pressure_dry (BP)
pressure_dry_percountry4$ISO_partner = pressure_dry_percountry4$countrycode # for the purposes of joining up
uk_imports_and_bp = full_join(uk_imports, pressure_dry_percountry4, by = c("crop", "ISO_partner"))
head(uk_imports_and_bp) # checking: cauliflowers, uk from ecuador, 7 in uk_imports and 19237 in bp data - both a match in this joined dataset

uk_imports_and_bp$uk_imports_multiplied_by_BP_of_importpartners = uk_imports_and_bp$pressure_dry * uk_imports_and_bp$dmi_tonnes_average_2000
head(uk_imports_and_bp)
# uk_imports_and_bp$uk_imports_multiplied_by_BP_of_importpartners - this will be BPcons part 2 (right-hand-side of the equation)
# write.csv(uk_imports_and_bp, paste0(outdir, "uk_imports_and_bp.csv"))
(total_uk_import_bp = sum(uk_imports_and_bp$pressure_dry, na.rm = TRUE)) # 2.985185e+24

head(uk_bp_and_consumption) # we want uk_bp_and_consumption$bp_multiplied_by_tonnes_uk
head(uk_imports_and_bp) # and uk_imports_and_bp$uk_imports_multiplied_by_BP_of_importpartners

# To get the BPcons overall, I need to sum per crop the uk_imports_multiplied_by_BP_of_importpartners
uk_BPcons_import_component_per_crop = uk_imports_and_bp %>% group_by(crop) %>% summarise(uk_imports_multiplied_by_bp_imports_per_crop = sum(uk_imports_multiplied_by_BP_of_importpartners, na.rm = TRUE))
# checking cauliflower: 1.25e 7
# write.csv(paste0(outdir, uk_imports_and_bp, "uk_imports_and_bp.csv"))
# opened and summed in excel: 12452234.22
head(uk_BPcons_import_component_per_crop)

BPcons_components_uk = full_join(uk_bp_and_consumption, uk_BPcons_import_component_per_crop, by = "crop")
head(BPcons_components_uk)

BPcons_components_uk$BPcons_UK = BPcons_components_uk$bp_multiplied_by_tonnes_uk + BPcons_components_uk$uk_imports_multiplied_by_bp_imports_per_crop
# BPcons_components_uk$BPcons_UK - this is BPcons for the UK
head(BPcons_components_uk)
# write.csv(BPcons_components_uk, paste0(outdir, "BPcons_components_uk.csv"))
(total_BPcons_uk = sum(BPcons_components_uk$BPcons_UK, na.rm = TRUE)) # 427552467

uk_exports = filter(trade_data_c2000, reporter != "United Kingdom" & partner == "United Kingdom")
uk_total_exports = sum(uk_exports$dmi_tonnes_average_2000, na.rm = TRUE)      
uk_total_exports # 50359.24
uk_exports_per_crop = uk_exports %>% group_by(crop) %>% 
  summarise(uk_exports_tonnes = sum(dmi_tonnes_average_2000, na.rm = TRUE))
# carrots, then onions, then apples
# write.csv(uk_exports, paste0(outdir, "uk_exports.csv"))
# write.csv(uk_exports_per_crop, paste0(outdir, "uk_exports_per_crop.csv"))

## South Africa data:

pressure_dry_percountry = pressure_dry_percountry4
SouthAfrica_bp = filter(pressure_dry_percountry, country == "South Africa") # country, crop, pressure_dry (BP)
SouthAfrica_production_minus_exports = filter(trade_data_c2000, reporter == "South Africa" & partner == "South Africa")
SouthAfrica_production_minus_exports
# write.csv(SouthAfrica_production_minus_exports, paste0(outdir, "SouthAfrica_production_minus_exports.csv"))
(total_southafrica_productionminusexports = sum(SouthAfrica_production_minus_exports$dmi_tonnes_average_2000, na.rm = TRUE)) # 5319822

head(SouthAfrica_bp); head(SouthAfrica_production_minus_exports)
SouthAfrica_bp_and_consumption = full_join(SouthAfrica_production_minus_exports, SouthAfrica_bp, by = "crop") # checked examples using View(file) and comparing, and this has worked OK
SouthAfrica_bp_and_consumption$bp_multiplied_by_tonnes_SouthAfrica = SouthAfrica_bp_and_consumption$pressure_dry * SouthAfrica_bp_and_consumption$dmi_tonnes_average_2000
SouthAfrica_bp_and_consumption$bp_multiplied_by_tonnes_SouthAfrica # this is BPcons part 1 
# write.csv(SouthAfrica_bp_and_consumption, paste0(outdir, "SouthAfrica_bp_and_consumption.csv"))
(total_prodminusexport_bp_southafrica = sum(SouthAfrica_bp_and_consumption$pressure_dry, na.rm = TRUE)) # 14779.04

SouthAfrica_imports = filter(trade_data_c2000, reporter == "South Africa" & partner != "South Africa")
SouthAfrica_total_imports = sum(SouthAfrica_imports$dmi_tonnes_average_2000, na.rm = TRUE)
SouthAfrica_total_imports # total imports of all fruits and vegetables analysed # 32983.66
SouthAfrica_total_imports_per_crop = SouthAfrica_imports %>% group_by(crop) %>%  summarise(SouthAfrica_total_imports_per_crop = sum(dmi_tonnes_average_2000, na.rm = TRUE))
# tomatoes, then grapes, then greencorn
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
# oranges then grapes then apples
# write.csv(SouthAfrica_exports, paste0(outdir, "SouthAfrica_exports.csv"))
# write.csv(SouthAfrica_exports_per_crop, paste0(outdir, "SouthAfrica_exports_per_crop.csv"))

(t2 = sum(SouthAfrica_production_minus_exports$dmi_tonnes_average_2000, na.rm = T))
sa_total_consumption = t2+SouthAfrica_total_imports
sa_total_consumption # 5352806
(sa_imports_percent = (SouthAfrica_total_imports/sa_total_consumption)*100) # 0.62
(sa_home_percent = (t2/sa_total_consumption)*100) # 99.38%

## India data:

India_bp = filter(pressure_dry_percountry, country == "India") # country, crop, pressure_dry (BP)
India_production_minus_exports = filter(trade_data_c2000, reporter == "India" & partner == "India")
India_production_minus_exports
# write.csv(India_production_minus_exports, paste0(outdir, "India_production_minus_exports.csv"))
(india_production_minus_exports_sum = sum(India_production_minus_exports$dmi_tonnes_average_2000, na.rm = TRUE)) #76842306

head(India_bp); head(India_production_minus_exports)
India_bp_and_consumption = full_join(India_production_minus_exports, India_bp, by = "crop") # checked examples using View(file) and comparing, and this has worked OK
head(India_bp_and_consumption)
India_bp_and_consumption$bp_multiplied_by_tonnes_India = India_bp_and_consumption$pressure_dry * India_bp_and_consumption$dmi_tonnes_average_2000
India_bp_and_consumption$bp_multiplied_by_tonnes_India # this is BPcons part 1 
# write.csv(India_bp_and_consumption, paste0(outdir, "India_bp_and_consumption.csv"))
(bp_of_production_minus_exports_india = sum(India_bp_and_consumption$pressure_dry, na.rm = T)) # 13860.33

India_imports = filter(trade_data_c2000, reporter == "India" & partner != "India")
India_total_imports = sum(India_imports$dmi_tonnes_average_2000)
India_total_imports # total imports of all fruits and vegetables analysed # 302558.4
India_total_imports_per_crop = India_imports %>% group_by(crop) %>%  summarise(India_total_imports_per_crop = sum(dmi_tonnes_average_2000, na.rm = TRUE))
# write.csv(India_total_imports, paste0(outdir, "India_total_imports.csv"))
# write.csv(India_total_imports_per_crop, paste0(outdir, "India_total_imports_per_crop.csv"))

India_production_minus_exports # banana, then mango, then eggplant
India_total_imports_per_crop
# dates, then grapes, then garlic

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
# onion, then mango, then grape, then mushroom, then banana
# write.csv(India_exports, paste0(outdir, "India_exports.csv"))
# write.csv(India_exports_per_crop, paste0(outdir, "India_exports_per_crop.csv"))

(t3 = sum(India_production_minus_exports$dmi_tonnes_average_2000, na.rm = T))
india_total_consumption = t3+India_total_imports
india_total_consumption # 77144864
(india_imports_percent = (India_total_imports/india_total_consumption)*100) # 0.39
(india_home_percent = (t3/india_total_consumption)*100) # 99.6




