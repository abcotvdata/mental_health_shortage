library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.providers)
library(leaflet.extras)
library(readr)
library(readxl)
library(htmlwidgets)
library(sf)

# import the cleaned file Maggie made from NPI
# That includes just the mental health professionals we're studying
# And reduces down to the basic fields we need to shrink the size of the massive file
mentalhealthproviders <- read_csv("mentalhealthproviders2.csv", 
                                  col_types = cols(.default = "c")) %>% 
  select(1,2,5:13,29:34)
# standardize and simplify the names of fields
colnames(mentalhealthproviders) <- c("npi", "type_code","organization_name", "provider_last_name","provider_first_name",
                "provider_middle_name", "provider_name_prefix_text", "provider_name_suffix_text",
                "provider_credential_text","other_org_name", "other_org_type", "address",
                "address_extra","city","state","zip","country_code")

# Turns the longform zip code into the five digit zip we need to map
# We are using the providers' PRACTICE LOCATION rather than business/mailing location
mentalhealthproviders$zip <- substr(mentalhealthproviders$zip,1,5)

# ZIP crosswalk that adds the county FIPS number for the county that most of the zip sits in
# This is typically 95+ percent of a zip in a county; but this is the best we can do to assign
# these providers by their addresses to counties
zip2county <- read_excel("ZIP_COUNTY_122021.xlsx") %>%
  group_by(zip) %>% # for each unique sample
  arrange(-tot_ratio) %>% # order by total_reads DESC
  slice(1) %>% select(1:4)
colnames(zip2county) <- c("zip","county","city","state_zip")
zip2county <- subset(zip2county, !(zip2county$state_zip %in% c("PR","VI","GU","AS","MP")))
# fixes a few outdated FIPS codes in rural America
zip2county$county <- ifelse(zip2county$county=="46113","46102",zip2county$county)
zip2county$county <- ifelse(zip2county$county=="02261","02063",zip2county$county)
zip2county$county <- ifelse(zip2county$county=="02270","02158",zip2county$county)

# this joins the mental health providers table to add the county FIPS code that most
# closely corresponds to each zip code for providers
mentalhealthproviders <- left_join(mentalhealthproviders,zip2county %>% select(1:2,4),by="zip")

# quick test that we have no duplicated providers
# unique(mentalhealthproviders$npi) # returns 472,336 unique npi/identifiers

# Quick pivot table of the number of providers by county
providers_by_county <- mentalhealthproviders %>%
  group_by(county) %>%
  summarise(providers=n())

# Get demographic data for COUNTIES 
# Gives us population and geometry for every county, by FIPS code, needed for mapping later
counties <- get_acs(geography = "county", 
                       year = 2020,
                       output = 'wide',
                       variables = "B03002_001", 
                       geometry = TRUE) %>%
  rename("population"="B03002_001E") %>% 
  select(-4) %>%
  janitor::clean_names()

# this joins the count of providers per county to the geometry file with population from Census Bureau
providers_by_county_formap <- full_join(counties,providers_by_county,
                                         by=c("geoid"="county"))

# replaces the blank NA fields for counties with no providers to actual zeros
providers_by_county_formap$providers[is.na(providers_by_county_formap$providers)] <- 0
# Calculates ratios/rates in two different ways to help us think about this for reporting
# Per Capita number of providers as well as a patient-to-provider ratio; either shows the same
# just two different ways to think about the availability question
providers_by_county_formap$per100Kpeople <- round(providers_by_county_formap$providers/(providers_by_county_formap$population/100000),1)
providers_by_county_formap$prov_patient_ratio <- round(providers_by_county_formap$population/providers_by_county_formap$providers,1)
# For cases with infinity on ratio, changed to ratio NA so areas with zero providers are flagged separately on map later
providers_by_county_formap <- providers_by_county_formap %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
providers_by_county_formap <- providers_by_county_formap %>% filter(!is.na(geoid))

# bring in census rural urban
urban_rural <- read_excel("PctUrbanRural_County.xls") %>% janitor::clean_names()
urban_rural$urban_or_rural <- ifelse(urban_rural$poppct_rural > 50, "Rural", "Urban")
urban_rural$geoid <- paste0(urban_rural$state,urban_rural$county,sep="")
# add urban rural designator to providers analysis file for map and table
providers_by_county_formap <- left_join(providers_by_county_formap, urban_rural %>% select(27:28),
                                        by="geoid")
providers_by_county_formap$urban_or_rural <- ifelse(is.na(providers_by_county_formap$urban_or_rural), "Rural", providers_by_county_formap$urban_or_rural)



# transforming the projection of the map to something leaflet can work with easily
providers_by_county_formap <- providers_by_county_formap %>% st_transform(4326)



# Set bins for numbers of patients to providers, in four quartiles
# we can change that to five or six, but simple is always better for viewers


bins <- c(0, 1, 10, 50, 100, 500, 1200)
pal <- colorBin(palette="plasma",bins=bins,domain=providers_by_county_formap$per100Kpeople)

# crude popup label, which we can improve on if we decide to publish the map
label <- paste(sep = "<br>", "<b>",providers_by_county_formap$name,
               "<br><b>Number of providers: </b>",providers_by_county_formap$providers,
               "<br><b>Providers Per 100K people: </b>",providers_by_county_formap$per100Kpeople,
               "<br><b>Patient To Provider Ratio: </b>",providers_by_county_formap$prov_patient_ratio)

# creates a color-coded county map based on the ratio of patients to providers in each county
# adds legend that we need to do some more work on to get the wording right; add sourcing; etc
providers_by_county_map <- leaflet(providers_by_county_formap, options = leafletOptions(zoomControl = FALSE)) %>%
  htmlwidgets::onRender("function(el, x) {
L.control.zoom({ position: 'topright' }).addTo(this)
}") %>%
  setView(-95.6,38.8, zoom = 4) %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(color = "white", popup = label, weight = 1, smoothFactor = 0.5,
              opacity = 0.6, fillOpacity = 0.4,
              fillColor = ~pal(`per100Kpeople`)) %>%
addLegend(opacity = 0.4,
          values = ~per100Kpeople, 
          pal=pal,
          position = "topleft", 
          title = "<big>Mental health care access by county</big><br><small>Providers per 100,000 residents. Click any county for details.") 
providers_by_county_map
saveWidget(providers_by_county_map, 'docs/providers_by_county.html', title = "ABC OTV Mental Health Access By County Interactive Map", selfcontained = TRUE)


# this takes the data we used for the map, removes the geo col (size reasons)
# and then saves for backup as csv for analysis for story
providers_by_county_table <- providers_by_county_formap %>% st_drop_geometry()
write_csv(providers_by_county_table,"providers_by_county_table.csv")




  