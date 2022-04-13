library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.providers)
library(leaflet.extras)
library(readr)
library(readxl)
library(sf)

mentalhealthproviders <- read_csv("mentalhealthproviders.csv", 
                                  col_types = cols(.default = "c")) %>% 
  select(1,2,5:13,29:34)

colnames(mentalhealthproviders) <- c("npi", "type_code","organization_name", "provider_last_name","provider_first_name",
                "provider_middle_name", "provider_name_prefix_text", "provider_name_suffix_text",
                "provider_credential_text","other_org_name", "other_org_type", "address",
                "address_extra","city","state","zip","country_code")

mentalhealthproviders$zip <- substr(mentalhealthproviders$zip,1,5)

zip2county <- read_excel("ZIP_COUNTY_122021.xlsx") %>%
  group_by(zip) %>% # for each unique sample
  arrange(-tot_ratio) %>% # order by total_reads DESC
  slice(1) %>% select(1:4)
colnames(zip2county) <- c("zip","county","city","state_zip")
zip2county <- subset(zip2county, !(zip2county$state_zip %in% c("PR","VI","GU","AS","MP")))
zip2county$county <- ifelse(zip2county$county=="46113","46102",zip2county$county)
zip2county$county <- ifelse(zip2county$county=="02261","02063",zip2county$county)
zip2county$county <- ifelse(zip2county$county=="02270","02158",zip2county$county)
# fix a few stray wrong states
# zip2county$county <- ifelse(zip2county$county=="02270","02158",zip2county$county)

mentalhealthproviders <- left_join(mentalhealthproviders,zip2county %>% select(1:2,4),by="zip")

# quick test that we have no duplicated providers
# unique(mentalhealthproviders$npi) # returns 472,336 unique npi/identifiers

providers_by_county <- mentalhealthproviders %>%
  group_by(county) %>%
  summarise(providers=n())

# Get demographic data for Census block groups to aggregate/apportion to precinct geography
# Also transforming to match the planar projection of NYPD's beats spatial file
# This also reduces us down to just the numeric population est and geometry
counties <- get_acs(geography = "county", 
                       year = 2020,
                       output = 'wide',
                       variables = "B03002_001", 
                       geometry = TRUE) %>%
  rename("population"="B03002_001E") %>% 
  select(-4) %>%
  janitor::clean_names()

providers_by_county_formap <- full_join(counties,providers_by_county,
                                         by=c("geoid"="county"))

providers_by_county_formap$providers[is.na(providers_by_county_formap$providers)] <- 0
providers_by_county_formap$per100Kpeople <- providers_by_county_formap$providers/(providers_by_county_formap$population/100000)
providers_by_county_formap$prov_patient_ratio <- providers_by_county_formap$population/providers_by_county_formap$providers

providers_by_county_formap <- providers_by_county_formap %>% st_transform(4326)

# Set bins for numbers of crimes for murders map
pal <- colorQuantile(c("#667f99",
                       "#00318b",
                       "#0058f6",
                       "#ffba00"), providers_by_county_formap$prov_patient_ratio, n = 4, na.color = "#be0000")
label <- paste(sep = "<br>", providers_by_county_formap$name,
                  "<br>Patient To Provider Ratio: ",providers_by_county_formap$prov_patient_ratio,
                  "<br>Number of providers: ",providers_by_county_formap$providers)


providers_by_county_map <- leaflet(providers_by_county_formap) %>%
  setView(-85.9,38.7, zoom = 4) %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(color = "white", popup = label, weight = 1, smoothFactor = 0.5,
              opacity = 0.8, fillOpacity = 0.4,
              fillColor = ~pal(`prov_patient_ratio`)) %>%
addLegend(opacity = 0.6,
          values = murders_beat$rate_prior3years, 
          colors = c("#667f99",
                     "#00318b",
                     "#0058f6",
                     "#ffba00"),
          labels=c("Highest Availability", 
                   "Higher Availability", 
                   "Lower Availability", 
                   "No Availability"),
          position = "bottomleft", 
          title = "Patient to Provider<br>Ratio By County") 
providers_by_county_map

# this takes the data we used for the map, removes the geo col (size reasons)
# and then saves for backup as csv for analysis for story
providers_by_county_table <- providers_by_county_formap %>% st_drop_geometry()
write_csv(providers_by_county_table,"providers_by_county_table.csv")
  