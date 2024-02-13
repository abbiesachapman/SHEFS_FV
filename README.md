# SHEFS_FV
# R scripts and data to accompany 'Quantifying the biodiversity pressures of fruit and vegetables consumption in the UK, India, and South Africa'.

# Please cite the associated manuscript when using these data and/or scripts.

# Please note that some of the data files are not available here. Trade data will need to be requested from C.Dalin & H.Kennard, as they have been used with permission for this work.
# Crop data are available from Monfreda et al. (2008): http://www.earthstat.org/harvested-area-yield-175-crops/.

# Please note that any country names referred to in this script are as per the source data being referenced, rather than representing the official, up to date names of all countries.
# We are therefore not showing agreement with country names used, nor intending to cause any offence by using these names. The names we use in our manuscript text are correct and up to date.

# Below is a sumamry of what the R scripts do:

# 1. Bring in & process the species data
# 2. Bring in world polygons
# 3. Bring in and process ~2000 trade data
# 4. Bring in crop data: harvested area (ha)
# 5. Bring in crop data: production (tonnes) and get the dry weight using the water content data
# 6. Project the species data to the same projection as harvested area using bilinear resampling 
# 7. Calculate BP: Species Richness * Harvested Area / Production and calculate ASR during this BP calculation as: Species Richness * Harvested Area
# 8. Get the per-country weighted means
# 9. Calculate summary statistics
# 10. Remove the following crops as not present in the trade data, so cannot be used for BPcons and are therefore excluded from our analyses: greenonion, melonetc, pepper, cashewapple, stringbean
# 11. Calculate BPcons - domestic component: BPfocalcountry * tonnes focal country
# 12. Calculate BPcons - import component: BPimports * tonnes imports
# 13. Calculate BPcons: domestic component + import component
# 14. Get trade data post 2003
# 15. Get per crop averages: group by crop then sum (tonnes*pressure)/sum(tonnes)
# 16. BPcons: sum the imports * BP of import partners per crop and partner

The script then continues to bring things together for figures (e.g. filtering for the top 5 consumed crops per country for figure 3 and the highest BPcons crops produced domestically and exported for figure 4).
 
