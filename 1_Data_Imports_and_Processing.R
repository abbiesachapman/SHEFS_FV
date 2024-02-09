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

rm(list = ls())

library(raster); library(maptools); library(sp); library(readbulk); library(plyr);
library(rgdal); library(rgeos); library(ggplot2); library(dplyr); library(viridis);
library(gridExtra); library(reshape2); library(terra); library(sf); library(stars); library(ggplot2);
library(ggbreak); library(ggrepel); library(rlang)

setwd("C:/Users/Dr Abbie/Documents/Data/") # This will need to be set to the user's directory.

###################################################################################################################
###################################################################################################################
# 0. Bringing in datasets and processing.
###################################################################################################################
###################################################################################################################

# 0.1 Combining Adrienne Etard's amphibian, bird, mammal, and reptile files to create a vertebrate layer.
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
unique(values(amphibians), na.rm = TRUE) # this has the unusual value in it
check = as.data.frame(amphibians)
check
(check1 = check %>% 
    arrange(desc(layer)) %>% 
    slice(1:5))
amphibians[amphibians >= 1440541056] <- NA # removing this erroneous value (checked with creator
# Adrienne Etard and this is an error but is in the ocean so does not affect this analysis
# - removed for completeness)

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

vertstack = raster::stack(amphibians, birds, mammals, reptiles) # Stacking the different rasters to complete a 'vertebrate' layer
vertstack
# raster::plot(vertstack)
vertebrates = raster::calc(vertstack, sum)
vertebrates # max 997
crs(vertebrates) = CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs')
vertebrates
str(vertebrates)
extent(vertebrates) 
# raster::plot(vertebrates) # Note: terra package kept crashing so I stuck with raster, as I am using WGS84 anyway so proj4 deprecation is not yet an issue.
vertebrates # This is in equal area projection and metres
# and it looks sensible, with hotspots in the tropics, when plotted.
rasterdir = "SHEFS/SHEFS_Sept2023_Update/6_GlobalRasters/"
# writeRaster(x=vertebrates, filename = paste0(rasterdir, "vertebrate_richness"), format = "GTiff", overwrite = TRUE)

#### 
# 0.2 Extracting country outline data and correcting the issue I identified wherein Serbia and 
# Montenegro are separated differently in the wrld_simpl and trade datasets.

library(raster); library(maptools); library(exactextractr)
data(wrld_simpl)
wrld_simpl2 = crop(wrld_simpl, extent(-180.9363, 180.9837, -61.13384, 87.57736))
# plot(wrld_simpl2)
wrld_simpl2$NAME # 238 is Serbia and 140 is Montenegro - these are separated in this map but not in the trade data, so I must resolve this
# to continue with the analyses later on.
wrld_simpl2
# Help with this issue was available here: https://stackoverflow.com/questions/40631947/merging-two-spatialpolygonsdataframe-objects
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
wrld_simpl4$ISO3 = replace(wrld_simpl4$ISO3, 245, "SRB")
wrld_simpl4
unique(wrld_simpl4$ISO3)

#### 
# 0.3 Bringing in the trade data.
# As these data are not mine to share, I cite them in the manuscript so users can request the data from the relevant authors.
# The script reads in csv files, subsets them to the years and crops needed, and then reads them into a single file,
# which I then read in below for analysis. I comment out most of this code as it is very slow to run.

# outdir = "SHEFS/SHEFS_March2023_Update/19July2023/"
# cropdir = "SHEFS/6_Trade_Matrices/Corrected_trade_long/" # bringing in the long format data to try instead of the wide
# cropdata = list.files(path = cropdir, pattern="*.csv", all.files = TRUE, full.names = TRUE)
# cropdata
# for (i in 1:length(cropdata)) assign(cropdata[i], read.csv(cropdata[i]))
# yearslist = c("1997", "1998", "1999", "2000", "2001", "2002", "2003")
# cropdata_sub = lapply(yearslist, FUN = function(x) {
#   cropdata[grepl(x, cropdata)]})
# cropdata_sub
# cropdata_sub1 = unlist(cropdata_sub)
# croplist = read.csv(paste0("SHEFS/0_HarvestedArea_Yield_EarthStat/", "EarthStat_Crops.csv"))
# croplist
# fruitveg = filter(croplist, cropgroup == "Vegetables&Melons" | cropgroup == "Fruit")
# fruitveg
# fruitveg = fruitveg %>% filter(!grepl(", nes", crop_longname))
# fruitveg = fruitveg %>% filter(!grepl("Nes", crop_longname))
# fruitveg = fruitveg %>% filter(!grepl(" nes", crop_longname))
# fruitveg
# colnames(fruitveg) = c("crop", "crop_longname", "cropgroup")
# cropdata_sub2 = lapply(fruitveg$crop, FUN = function(x) {
#   cropdata_sub1[grepl(x, cropdata_sub1)]
# })
# cropdata_sub2
# cropdata_sub3 = unlist(cropdata_sub2)
# cropdata = cropdata_sub3
# str(cropdata) # these data are reading in correctly (test checked with apricots 11/11/2023 12:02)
# str(cropdata)
# library(stringr)
# # commented out below as quite slow to run
# str(cropdata)
# # dfa = NULL
# # for (m in 1:389) {
# #   file1 = read.csv(cropdata[m])
# #   file2 = file1[complete.cases(file1),]
# #   cropname = stringr::str_remove(cropdata[m], "SHEFS/6_Trade_Matrices/Corrected_trade_long/")
# #   cropname1 = stringr::str_remove(cropname, ".csv")
# #   cropname2 = stringr::str_split_fixed(cropname1,"_", 2)
# #   file_with_name = cbind(file2, cropname2)
# #   dfa = rbind(dfa, file_with_name)}
# str(dfa)
# form_1a = paste0(outdir, "trade_data_all_28082023a.csv")
# #write.csv(dfa, file = form_1a)

outdir = "SHEFS/SHEFS_March2023_Update/19July2023/"
tradedata = read.csv(paste0(outdir, "trade_data_all_28082023a.csv"))
tradedata = subset(tradedata, select = -c(X))
colnames(tradedata) = c("reporter", "partner", "dmi_tonnes", "crop", "year")
trade_data_c2000 = tradedata %>% 
  group_by(crop, reporter, partner) %>% 
  summarise(dmi_tonnes_average_2000 = mean(dmi_tonnes, na.rm = TRUE))

trade_data_c2000 
# The scripting below is written to ensure the trade data have the ISO codes so map and 
# trade data match up safely, rather than relying on country names (which can have different
# spellings, punctuation, etc.).
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
testnames # This contains the reporters and partners from the trade data with ISO codes and has NAs for the ones I need to match.
View(worldnames_short)
outdir = "SHEFS/SHEFS_Sept2023_Update/4_TradeData_and_Outputs/"
# write.csv(testnames, paste0(outdir, "tradenamematching.csv")) - Exporting to try and fix using the worldnames from wrld_simpl
namematches = read.csv(paste0(outdir, "tradenamematching_filled.csv"))
head(namematches) # Here, now the Serbia and Montenegro <NA> is marked properly in ISO.

# Next, we want to find a way to replace the names in the reporter and partner columns of trade data with the ISO codes in namematches,
# noting that there are a couple of NAs where the wrld_simpl map does have some out of date country names/boundaries included.
head(trade_data_c2000)
trade_data_c2000$NAME = trade_data_c2000$reporter
head(trade_data_c2000)
trade_data_test = trade_data_c2000
trade_data_test = left_join(trade_data_test, namematches, by = "NAME")
head(trade_data_test)
sum(is.na(trade_data_test$ISO3))
(na_rows = trade_data_test[!complete.cases(trade_data_test),]) # This is OK as it's Serbia and Montenegro, USSR, and Yugoslavia.
sum(na_rows$dmi_tonnes_average_2000) # 102 tonnes total globally across all partners
trade_data1 = subset(trade_data_test, select = -c(NAME, X))
trade_data1$NAME = trade_data1$partner
trade_data2 = left_join(trade_data1, namematches, by = "NAME")
head(trade_data2)
na_rows1 = trade_data2[!complete.cases(trade_data2$ISO3.y),] # Again USSR, Yugoslav SFR and Serbia & Montenegro.
na_rows1
sum(na_rows1$dmi_tonnes_average_2000) # 102 tonnes
trade_data_c2000 = subset(trade_data2, select = -c(X, NAME))
colnames(trade_data_c2000) = c("crop", "reporter", "partner", "dmi_tonnes_average_2000",
                               "ISO_reporter", "ISO_partner")
head(trade_data_c2000)
# Serbia and Montenegro are separate in the trade data but with values of zero.
# Their values are attributed to a joint 'Serbia and Montenegro' classification.
# I will therefore give this an ISO code for Serbia for these data, just to ensure everything works together OK.

#### 
# 0.4 Bringing in the crop data (Monfreda et al., 2008)

production.dir = "Overlay Analysis/1a_EarthStatCrops/HarvestedAreaYield175Crops_Geotiff/HarvestedAreaYield175Crops_GeoTiff/Geotiff/"
# Here, we find all of the files in the Monfreda et al. 2008 ('EarthStat') downloaded crop files that represent harvested area in hectares.
# We use harvested area as fractional area has the same issue as harvested where multiple harvests can increase the value, but it's about relativising the species impacted locally.
harvest.files = paste(production.dir,dir(path = production.dir, pattern = "*HarvestedAreaHectares.tif", recursive = TRUE),sep="")
# Then, because some of these files come with accompanying metadata that we don't need to work with, we request only the tiff files.
harvest.files = harvest.files[!grepl("ovr", harvest.files)]
harvest.files <- harvest.files[!grepl("xml",harvest.files)]
harvested_area = stack(harvest.files)
names(harvested_area) = gsub("_HarvestedAreaHectares", "", names(harvested_area), fixed = TRUE) 

# Production files:
production.files = paste(production.dir,dir(path = production.dir, pattern = "*Production.tif", recursive = TRUE),sep="")
# Then, because some of these files come with accompanying metadata that we don't need to work with, we request only the tiff files.
production.files = production.files[!grepl("ovr", production.files)]
production.files <- production.files[!grepl("xml",production.files)]
production = stack(production.files)
names(production) = gsub("_Production", "", names(production), fixed = TRUE) 

###################################################################################################################
###################################################################################################################
# 1. Processing the crop data - computing dry-weight production.
###################################################################################################################
###################################################################################################################

# I will be working with the dry weights of fruits and vegetables for this manuscript.
## I therefore need to marry up the water data with the production crop list.
# From here, I need to multiply the production maps by 1-x/100 where x is the water content
# They'll then be dry-weight production values to include in any calculations involving production.

water_dir = "SHEFS/9_FoodCompositionData_from_USDA/"
waterdata = read.csv(paste0(water_dir, "nutrient_comp_data_no_nes.csv"))
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
# Commented out below as slow to run (ran most recently: 11/09/2023)
# dry_production_global =  dryweightfun(ras_stack2 = fnv_productionmaps_global)
dry_weight_production_global = paste(outDir_dry, dir(path = outDir_dry, recursive = TRUE), sep = "")
dry_weight_production_maps_global = stack(dry_weight_production_global)
# Whilst the harvested area are fine as is, I'll filter to just the crops we want.
fnv_harvestedarea_global = raster::subset(harvested_area, waterdata1$crop)
names(fnv_harvestedarea_global) # 53 crops

###################################################################################################################
###################################################################################################################
# 2. Ensuring matching extents and projections.
###################################################################################################################
###################################################################################################################

vertebrates
dry_weight_production_maps_global
fnv_harvestedarea_global
crs(fnv_harvestedarea_global)
vertebrates_projected = projectRaster(from = vertebrates, to = fnv_harvestedarea_global[[1]],  method = "bilinear")
# plot(vertebrates); vertebrates
# plot(vertebrates_projected); vertebrates_projected
# Note that some of these values are now less than zero (only to -6 so not huge), so this needs editing as described here https://stackoverflow.com/questions/36960974/how-to-replace-raster-values-less-than-0-to-na-in-r-code
vertebrates_projected1 = reclassify(vertebrates_projected, cbind(-Inf, 0, NA), right = FALSE)

# Can read this in for max per country:
species_extract = exact_extract(vertebrates_projected1 , wrld_simpl4, fun = 'max', include_area = FALSE, include_cell = FALSE,
                                include_xy = FALSE, force_df = TRUE, full_colnames = TRUE, stack_apply = TRUE, summarize_df = FALSE, progress = TRUE)
# This doesn't have minus numbers for species - minimum is 0
# max 996.2698 - which is good as the max vertebrates original layer is 997
species_extract$countrycode = wrld_simpl4$ISO3
species_extract$country = wrld_simpl4$NAME
# write.csv(species_extract, paste0("SHEFS/SHEFS_Sept2023_Update/", "max_species_per_country_dec2023_final.csv"))
species_extract_total = exact_extract(vertebrates_projected1 , wrld_simpl4, fun = 'sum', include_area = FALSE, include_cell = FALSE,
                                      include_xy = FALSE, force_df = TRUE, full_colnames = TRUE, stack_apply = TRUE, summarize_df = FALSE, progress = TRUE)
# max 69800520
species_extract_total$countrycode = wrld_simpl4$ISO3
species_extract_total$country = wrld_simpl4$NAME
# write.csv(species_extract_total, paste0("SHEFS/SHEFS_Sept2023_Update/", "total_species_per_country_dec2023_final.csv"))
