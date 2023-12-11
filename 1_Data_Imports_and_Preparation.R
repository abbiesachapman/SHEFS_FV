##
## Date: 11th December 2023
## Author: Abbie S. A. Chapman
## Title: R script to accompany 'Quantifying the biodiversity pressures of fruit and vegetable consumption in the 
## United Kingdom, India, and South Africa.'
##

## 1_Data_Imports_and_Preparation

rm(list = ls()) # clearing the workspace

library(raster); library(maptools); library(sp); library(readbulk); library(plyr);
library(rgdal); library(rgeos); library(ggplot2); library(snow); library(dplyr); library(viridis);
library(gridExtra); library(reshape2); library(terra); library(sf); library(stars); library(ggplot2);
library(ggbreak); library(ggrepel); library(rlang)

setwd("C:/Users/Dr Abbie/Documents/Data/")

#### 
# 0. Combining Adrienne Etard's amphibian, bird, mammal, and reptile files to create a single vertebrate raster layer.
# Using the 10km resolution files (50km also available).

species.dir = "Species Richness/From_Adrienne/"
species.files = paste(species.dir, dir(path = species.dir, recursive = TRUE), sep = "")
head(species.files)
species.files = species.files[!grepl("code_shared", species.files)]
species.files = species.files[!grepl("READ_ME", species.files)]
species.files = species.files[!grepl("50k", species.files)]
species.files
species.files = species.files[!grepl("Sentinel_Countries", species.files)]
species.files

amphibians = readRDS(species.files[1])
# raster::plot(amphibians)
res(amphibians)
unique(values(amphibians), na.rm = TRUE) # this has an anomalous value in it - 1440541056
# I checked this with the data owner (Dr Adrienne Etard) and the value is anomalous but also in the ocean,
# so does not affect terrestrial analyses.
check = as.data.frame(amphibians)
check
(check1 = check %>% 
    arrange(desc(layer)) %>% 
    slice(1:5))
amphibians[amphibians >= 1440541056] <- NA # I remove this erroneous value for completeness.

birds = readRDS(species.files[2])
# raster::plot(birds)
unique(values(birds), na.rm = TRUE) 
check = as.data.frame(birds)
check
(check1 = check %>% 
    arrange(desc(layer)) %>% 
    slice(1:5)) # no unusual values

mammals = readRDS(species.files[3])
# raster::plot(mammals)
unique(values(mammals), na.rm = TRUE) 
check = as.data.frame(mammals)
check
(check1 = check %>% 
    arrange(desc(layer)) %>% 
    slice(1:5)) # no unusual values

reptiles = readRDS(species.files[4])
# raster::plot(reptiles)
unique(values(reptiles), na.rm = TRUE) 
check = as.data.frame(reptiles)
check
(check1 = check %>% 
    arrange(desc(layer)) %>% 
    slice(1:5)) # no unusual values

vertstack = raster::stack(amphibians, birds, mammals, reptiles) # bringing together the four files into a single
vertstack
# raster::plot(vertstack)
vertebrates = raster::calc(vertstack, sum)
vertebrates # max 997
crs(vertebrates) = CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs')
vertebrates
str(vertebrates)
extent(vertebrates) 
# raster::plot(vertebrates) # terra package kept crashing so sticking with raster
# for this script, especially as I am using WGS84 anyway so proj4 deprecation not yet an issue
vertebrates # This is in equal area projection, in metres
# and it looks sensible, with hotspots in the tropics, when plotted.
rasterdir = "SHEFS/SHEFS_Sept2023_Update/6_GlobalRasters/"
# writeRaster(x=vertebrates, filename = paste0(rasterdir, "vertebrate_richness"), format = "GTiff", overwrite = TRUE)

#### 
# 1. Extracting data per country requires me to ensure we have country-specific data which are compatible with
# the global trade matrices that I will be using later. 
# Serbia and Montenegro are separated differently in the wrld_simpl data that I am using with exact_extract
# and in the trade data, so I resolve this below.

library(raster); library(maptools); library(exactextractr)
data(wrld_simpl)
wrld_simpl2 = crop(wrld_simpl, extent(-180.9363, 180.9837, -61.13384, 87.57736))
# plot(wrld_simpl2)
wrld_simpl2$NAME # 238 is Serbia and 140 is Montenegro - these are separated in this map but not in the trade data, so I must resolve this
# to continue with the analyses later on.
wrld_simpl2
# Used help with this issue from: https://stackoverflow.com/questions/40631947/merging-two-spatialpolygonsdataframe-objects
serbia = wrld_simpl2[wrld_simpl2$NAME == "Serbia",]
montenegro = wrld_simpl2[wrld_simpl2$NAME == "Montenegro",]
# plot(serbia)
# plot(montenegro)
serbia_and_montenegro = aggregate(rbind(serbia, montenegro))
# plot(serbia_and_montenegro)
wrld_simpl3 = wrld_simpl2
wrld_simpl3 = wrld_simpl3[!wrld_simpl3$NAME %in% c("Serbia", "Montenegro"),]
wrld_simpl3$NAME
wrld_simpl3$OBJECTID = c(1:length(wrld_simpl3$NAME))
wrld_simpl3 = spChFIDs(wrld_simpl3, as.character(wrld_simpl3$OBJECTID))
serbia_and_montenegro <- spChFIDs(serbia_and_montenegro, as.character(max(wrld_simpl3$OBJECTID) + 1))
serbia_and_montenegro
#serbia_and_montenegro1 = SpatialPolygonsDataFrame(serbia_and_montenegro, serbia_and_montenegro)
wrld_simpl4 = bind(wrld_simpl3, serbia_and_montenegro)
# plot(wrld_simpl4)
wrld_simpl3
wrld_simpl4
wrld_simpl3a = wrld_simpl3
check = data.frame(unique(wrld_simpl3$NAME))
check = data.frame(unique(wrld_simpl2$NAME))
check = data.frame(unique(wrld_simpl4$NAME)) # Serbia and Montenegro are being listed as 'NA' in the files used in exactextract later
# test1 = wrld_simpl4
# test2 = replace(test1$ISO3, 245, "SRB")
# test2
wrld_simpl4$ISO3 = replace(wrld_simpl4$ISO3, 245, "SRB")
wrld_simpl4
unique(wrld_simpl4$ISO3)

#### 
# 2. Bringing in the trade data.

outdir = "SHEFS/SHEFS_March2023_Update/19July2023/"
cropdir = "SHEFS/6_Trade_Matrices/Corrected_trade_long/" # bringing in the long format data to try instead of the wide
cropdata = list.files(path = cropdir, pattern="*.csv", all.files = TRUE, full.names = TRUE)
cropdata
for (i in 1:length(cropdata)) assign(cropdata[i], read.csv(cropdata[i]))
yearslist = c("1997", "1998", "1999", "2000", "2001", "2002", "2003") # selecting the years we are focusing on (those which match Monfreda et al., 2008)
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
str(cropdata) # these data are reading in correctly (test checked with apricots 11/11/2023 12:02)

str(cropdata)
library(stringr)

# Note that the for loop below is quite slow to run.
str(cropdata)
dfa = NULL
 for (m in 1:389) {
 file1 = read.csv(cropdata[m])
 file2 = file1[complete.cases(file1),]
 cropname = stringr::str_remove(cropdata[m], "SHEFS/6_Trade_Matrices/Corrected_trade_long/")
 cropname1 = stringr::str_remove(cropname, ".csv")
 cropname2 = stringr::str_split_fixed(cropname1,"_", 2)
 file_with_name = cbind(file2, cropname2)
 dfa = rbind(dfa, file_with_name)}

str(dfa)
form_1a = paste0(outdir, "trade_data_all_28082023a.csv")
write.csv(dfa, file = form_1a)

outdir = "SHEFS/SHEFS_March2023_Update/19July2023/"
tradedata = read.csv(paste0(outdir, "trade_data_all_28082023a.csv")) # bringing in the data so I don't have to run the 
# loop each time (can comment out lines 168-181)
tradedata = subset(tradedata, select = -c(X))
colnames(tradedata) = c("reporter", "partner", "dmi_tonnes", "crop", "year")
trade_data_c2000 = tradedata %>% 
  group_by(crop, reporter, partner) %>% 
  summarise(dmi_tonnes_average_2000 = mean(dmi_tonnes, na.rm = TRUE))
trade_data_c2000 
View(trade_data_c2000) # I checked apricot Afghanistan to Afghanistan average using raw csv data versus this output
# and it is OK - 11/11/2023 12:55
# apricotcheck_mean = (37243+31518+30596+32030.68966+31463.96552+37383.34483+39728.72414)/7

# Checks also performed to make sure the tradedata values are preserved in the c2000 version correctly
# e.g. apples, Sri Lanka reporter, Afghanistan partner - 
# 1997: 9, 1998: 12, 1999: 61, 2000: 26, 2001: 6, 2002: 3, 2003: 1
9+12+61+26+6+3+1
118/7 # 16.85714
# in trade_data_c2000: 16.857143
# check 2:
# e.g. bananas, uk reporter, ghana partner - 
# 1997: 564, 1998: 950, 1999: 408, 2000: 661, 2001: 2144, 2002: 1992, 2003: 323
564+950+408+661+2144+1992+323
7042/7 # 1920.286
# 1006 is the number in trade_data_c2000

# Here, we need to ensure the trade data have the ISO codes so everything matches up safely rather than depending on country spellings and punctuation
head(trade_data_c2000)
countrylist = data.frame(unique(trade_data_c2000$reporter))
colnames(countrylist) = "country"
countrylist2 = data.frame(unique(trade_data_c2000$partner))
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
testnames # this contains the reporters and partners from the trade data with ISO codes and has NAs for the ones I need to match
View(worldnames_short)
outdir = "SHEFS/SHEFS_Sept2023_Update/4_TradeData_and_Outputs/"
# write.csv(testnames, paste0(outdir, "tradenamematching.csv")) - exporting to try and fix using the worldnames from wrld_simpl and 
namematches = read.csv(paste0(outdir, "tradenamematching_filled.csv"))
head(namematches) # here, now the Serbia and Montenegro <NA> is marked properly in ISO

# Next, we want to find a way to replace the names in the reporter and partner columns of trade data with the ISO codes in namematches
# noting that there are a couple of NAs where the wrld_simpl map does have some out of date countries included
head(trade_data_c2000)
trade_data_c2000$NAME = trade_data_c2000$reporter
head(trade_data_c2000)
trade_data_test = trade_data_c2000
trade_data_test = left_join(trade_data_test, namematches, by = "NAME")
head(trade_data_test)
sum(is.na(trade_data_test$ISO3))
(na_rows = trade_data_test[!complete.cases(trade_data_test),]) # this is OK as it's Serbia and Montenegro, USSR, and Yugoslavia, which I expect to be out of date
sum(na_rows$dmi_tonnes_average_2000) # 102 tonnes total globally across all partners, so this is not a major impact
trade_data1 = subset(trade_data_test, select = -c(NAME, X))
trade_data1$NAME = trade_data1$partner
trade_data2 = left_join(trade_data1, namematches, by = "NAME")
head(trade_data2)
na_rows1 = trade_data2[!complete.cases(trade_data2$ISO3.y),] # Again USSR, Yugoslav SFR and Serbia & Montenegro
na_rows1
sum(na_rows1$dmi_tonnes_average_2000) # 102 tonnes
trade_data_c2000 = subset(trade_data2, select = -c(X, NAME))
colnames(trade_data_c2000) = c("crop", "reporter", "partner", "dmi_tonnes_average_2000",
                               "ISO_reporter", "ISO_partner")
head(trade_data_c2000)
# Serbia and Montenegro are separate in the trade data but with values of zero.
# Their values are attributed to a joint 'Serbia and Montenegro' classification.
# I will therefore give this an ISO code for Serbia for these data, just to ensure everything works together ok.


#### 
# 3. Crop data from Monfreda et al.2008 - http://www.earthstat.org/

production.dir = "Overlay Analysis/1a_EarthStatCrops/HarvestedAreaYield175Crops_Geotiff/HarvestedAreaYield175Crops_GeoTiff/Geotiff/"
# Here, we find all of the files in the earthstat.org downloaded crop files that represent harvested area in hectares.
# We use harvested area as fractional area has the same issue as harvested where multiple harvests can increase the value, but it's about relativising the species impacted locally.
harvest.files = paste(production.dir,dir(path = production.dir, pattern = "*HarvestedAreaHectares.tif", recursive = TRUE),sep="")
# Then, because some of these files come with accompanying metadata that we don't need to work with, we request only the tiff files:
harvest.files = harvest.files[!grepl("ovr", harvest.files)]
harvest.files <- harvest.files[!grepl("xml",harvest.files)]
harvested_area = stack(harvest.files)
names(harvested_area) = gsub("_HarvestedAreaHectares", "", names(harvested_area), fixed = TRUE) 

# Repeating the process to pull out the production files:
production.files = paste(production.dir,dir(path = production.dir, pattern = "*Production.tif", recursive = TRUE),sep="")
# Then, because some of these files come with accompanying metadata that we don't need to work with, we request only the tiff files:
production.files = production.files[!grepl("ovr", production.files)]
production.files <- production.files[!grepl("xml",production.files)]
production = stack(production.files)
names(production) = gsub("_Production", "", names(production), fixed = TRUE) 

#### 
# 4. We want to work with dry-weight production for our analyses. I therefore marry up the water content of fruits
# and vegetables with the production crop list. From here, I multiply the production maps by 
# 1-x/100, where x is the water content, to ensure that the values included in any calculations involving
# production are dry-weight values.

water_dir = "SHEFS/9_FoodCompositionData_from_USDA/" # I downloaded data on nutrient composition (including water)
# from the USDA. Source: https://data.nal.usda.gov/dataset/composition-foods-raw-processed-prepared-usda-national-nutrient-database-standard-reference-release-28-0
# Downloaded 21st October 2022.
waterdata = read.csv(paste0(water_dir, "nutrient_comp_data_no_nes.csv")) # These are the data cut back to the 
# foods we are studying using Excel and manual searches. Where multiple varieties of a food were available,
# I averaged. These are all values based on the raw food item.
head(waterdata)
waterdata = subset(waterdata, select = c("Earthstat", "Water_.g."))
head(waterdata)
colnames(waterdata) = c("crop", "water_g")
waterdata1 = waterdata %>% 
  group_by(crop) %>% 
  summarise(mean_water_g = mean(water_g))
head(waterdata1)
waterdata1 = filter(waterdata1, mean_water_g !="NA")
waterdata1

# For the dry weight, I'll need to filter the production maps to just fruit and veg and make sure they marry up with the water list
fnv_productionmaps_global = raster::subset(production, waterdata1$crop)
names(fnv_productionmaps_global)
waterdata1$crop # 53 crops (cashewapple is removed from fruit and veg later so this is OK that this is missing)
waterdata2 = data.frame(waterdata1)
outDir_dry = "SHEFS/SHEFS_Sept2023_Update/1_DryWeight_Production_Global/"
dryweightfun = function(ras_stack2) {
  for(i in 1:nlayers(ras_stack2)) {
    ras_stack2[[i]][ras_stack2[[i]] == 0] <- NA
    dryweight = ((ras_stack2[[i]] * (1-(waterdata2[i,2])/100)))
    raster::writeRaster(dryweight, filename = paste0(outDir_dry, "dry_weight_production_tonnes", names(ras_stack2[[i]])), format = "GTiff", overwrite = TRUE)
  }}
# Note that this is quite slow to run:
dry_production_global =  dryweightfun(ras_stack2 = fnv_productionmaps_global)

dry_weight_production_global = paste(outDir_dry, dir(path = outDir_dry, recursive = TRUE), sep = "")
dry_weight_production_maps_global = stack(dry_weight_production_global)
# Whilst the harvested area are fine as is, I'll filter to just the crops we want:
fnv_harvestedarea_global = raster::subset(harvested_area, waterdata1$crop)
names(fnv_harvestedarea_global) # 53 crops

#### 
# 5. To work with the crop and vertebrate data together, I need to ensure extents and projections are compatible.

vertebrates
dry_weight_production_maps_global
fnv_harvestedarea_global
crs(fnv_harvestedarea_global)

vertebrates_projected = projectRaster(from = vertebrates, to = fnv_harvestedarea_global[[1]],  method = "bilinear")
# plot(vertebrates); vertebrates
# plot(vertebrates_projected); vertebrates_projected
# Note that some of these values are now less than zero (only to -6 so not huge), so this needs editing as described here https://stackoverflow.com/questions/36960974/how-to-replace-raster-values-less-than-0-to-na-in-r-code
vertebrates_projected1 = reclassify(vertebrates_projected, cbind(-Inf, 0, NA), right = FALSE)

# Now that I have these data, I can also pull off some summary information as follows:

# Maximum number of vertebrate species per country:
species_extract = exact_extract(vertebrates_projected1 , wrld_simpl4, fun = 'max', include_area = FALSE, include_cell = FALSE,
                                include_xy = FALSE, force_df = TRUE, full_colnames = TRUE, stack_apply = TRUE, summarize_df = FALSE, progress = TRUE)
# max 996.2698 - which is good as the max vertebrates original layer is 997
species_extract$countrycode = wrld_simpl4$ISO3
species_extract$country = wrld_simpl4$NAME
# write.csv(species_extract, paste0("SHEFS/SHEFS_Sept2023_Update/", "max_species_per_country_dec2023_final.csv"))

# Total number of vertebrate species per country (note that this will include multiple counts of species
# as we don't have the identities, so species with large ranges are counted across many cells per country):
species_extract_total = exact_extract(vertebrates_projected1 , wrld_simpl4, fun = 'sum', include_area = FALSE, include_cell = FALSE,
                                      include_xy = FALSE, force_df = TRUE, full_colnames = TRUE, stack_apply = TRUE, summarize_df = FALSE, progress = TRUE)
# max 69800520 - this is a relative number, as described above.
species_extract_total$countrycode = wrld_simpl4$ISO3
species_extract_total$country = wrld_simpl4$NAME
# write.csv(species_extract_total, paste0("SHEFS/SHEFS_Sept2023_Update/", "total_species_per_country_dec2023_final.csv"))

#### 
# 6. Calculating the mean production per country:
# (Note that below is slow to run.)
production_dry_percountry = exact_extract(dry_weight_production_maps_global, wrld_simpl4, fun = 'mean', include_area = FALSE, include_cell = FALSE,
                            include_xy = FALSE, force_df = TRUE, full_colnames = TRUE, stack_apply = TRUE, summarize_df = FALSE, progress = TRUE)
production_dry_percountry
production_dry_percountry$countrycode = wrld_simpl4$ISO3
production_dry_percountry$country = wrld_simpl4$NAME

# Below, I read in the output to save running each time and also tidy this up:
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

# I can also compute the total production across all fruits and vegetables, per country, as follows:
prod_percountry_sums = production_dry_percountry4 %>% 
  group_by(countrycode) %>% 
  summarise(sum_fv  = sum(production, na.rm = TRUE))
form_percountry1d = paste0(outDir_production_percountry, "production_percountry_fvsums_organised_update.csv")
# write.csv(prod_percountry_sums, file = form_percountry1d)

# And the total per country:

production_dry_percountry_total = exact_extract(dry_weight_production_maps_global, wrld_simpl4, fun = 'sum', include_area = FALSE, include_cell = FALSE,
                            include_xy = FALSE, force_df = TRUE, full_colnames = TRUE, stack_apply = TRUE, summarize_df = FALSE, progress = TRUE)
production_dry_percountry_total
production_dry_percountry_total$countrycode = wrld_simpl4$ISO3
production_dry_percountry_total$country = wrld_simpl4$NAME

outDir_production_percountry = "SHEFS/SHEFS_Sept2023_Update/1.1_DryWeight_Production_percountry/"
# production_percountry_outform1 = paste0(outDir_production_percountry, "total_production_per_country_update.csv")
# write.csv(production_dry_percountry_total, file = production_percountry_outform1)
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

