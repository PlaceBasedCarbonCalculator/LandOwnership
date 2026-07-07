# Convert Jonathan's enhanced (pre-parsed) version of the missing overseas
# owners back into the addressLine/adminDistrict/postalCode format used by
# the Bing geocoding batches.

# miss_uk <- read.csv("data/UK_Owners_Missing.csv")
# jon_uk <- read.csv("data/from_jonathan/malcom_enhanced/malcom_uk_enhanced.csv")
# 
# miss_uk <- miss_uk[order(miss_uk$Title.Number),]
# jon_uk <- jon_uk[order(jon_uk$Title.Number),]
# 
# summary(miss_uk$Title.Number == jon_uk$Title.Number)
# summary(miss_uk$Property.Address == jon_uk$Property.Address)

jon_oversea <- read.csv("data/from_jonathan/malcom_enhanced/Malcom_Overseas_Owners_Missing_enhanced.csv")

jon_oversea <- jon_oversea[,c("unique_id","title_number","property_address","unit_id",
                              "unit_type","building_name","street_number","street_name","postcode","city",
                              "district","region")]

jon_oversea$unit_type[!jon_oversea$unit_type %in% c("apartment","flat","suite","unit","units","room","penthouse")] <- ""

jon_oversea$unit_all <- ifelse(jon_oversea$unit_type == "","",paste0(jon_oversea$unit_type," ",
                                                                     jon_oversea$unit_id,", "))


jon_oversea$addressLine <- paste0(jon_oversea$unit_all,
                                  jon_oversea$building_name," ",
                                  jon_oversea$street_number," ",
                                  jon_oversea$street_name," "
                                  )

jon_oversea <- jon_oversea[,c("unique_id","title_number","property_address","addressLine","district","postcode")]
names(jon_oversea) <- c("Id","Title Number","Property Address","addressLine","adminDistrict","postalCode")
jon_oversea$countryRegion <- "GB"

write.csv(jon_oversea, "D:/OneDrive - University of Leeds/Data/Land Ownership/for_geocoding/jon_overseas.csv", row.names = FALSE) 

