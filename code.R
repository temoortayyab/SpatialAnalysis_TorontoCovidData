#Loading libraries
library(readxl) 
library(dplyr)
library(car) 
library(data.table)
library(tidyr)
library(sf) 
library(sp)
library(tmap)
library(leaflet)
library(spdep)
library(grid)
library(gridExtra)
library(spgwr)

#Setting working directory
setwd("") #Set it to your directory with the required files  


# ---------------------------------------------------------------------------------------------------

### Loading in census data by toronto neighbourhood
TorontoCensus <- read.csv('neighbourhood-profiles-2016-140-model.csv')

# Step 1: Transpose the dataframe
TorontoCensus <- as.data.frame(t(TorontoCensus))

TorontoCensus <- TorontoCensus[-(1:4), ]  # Remove the first four rows

# Step 2: Set the first row as column names
colnames(TorontoCensus) <- TorontoCensus[1, ]  # Use the first row as column names

TorontoCensus <- TorontoCensus[-(1:2), ]  # Remove the first two rows

# Step 3: Move row names into a column
TorontoCensus <- cbind("Neighbourhood Name" = rownames(TorontoCensus), TorontoCensus)

# Step 4: Reset row names to default numeric
rownames(TorontoCensus) <- NULL

# Check for duplicate column names
duplicates <- duplicated(colnames(TorontoCensus))

# If there are duplicates, resolve them
if (any(duplicates)) {
  unique_colnames <- make.unique(colnames(TorontoCensus))
  colnames(TorontoCensus) <- unique_colnames
}

# ---------------------------------------------------------------------------------------------------

### Loading in Toronto COVID-19 Cases Data 
TorontoCovidCaseData <- read.csv('COVID19 cases.csv')

# Renaming Neighbourhood name column to match the census dataframe:
colnames(TorontoCovidCaseData)[colnames(TorontoCovidCaseData) == "Neighbourhood.Name"] <- "Neighbourhood Name"

# Convert 'Reported.Date' column to Date format and filter the data
TorontoCovidCaseData <- TorontoCovidCaseData %>%
  mutate(Reported.Date = as.Date(Reported.Date, format = "%Y-%m-%d")) %>% # Convert to Date format
  filter(Reported.Date < as.Date("2023-03-01")) # Remove cases after February 2023

# ---------------------------------------------------------------------------------------------------

### Totaling/Aggregating the number of COVID-19 cases within each Toronto Neighbourhood
# We then merge this information with the census data 
# Produces a dataset with all census data + covid case data by each neighbourhood

# Step 1: Aggregate the total number of Covid cases by neighbourhood
aggregated_cases <- TorontoCovidCaseData %>%
  group_by(`Neighbourhood Name`) %>%
  summarise(TotalCovidCases = n())

# Step 2: Remove cases with missing or blank neighbourhood info (about 3% of data)
aggregated_cases <- aggregated_cases %>%
  filter(`Neighbourhood Name` != "" & !is.na(`Neighbourhood Name`))

# Step 3: Directly assign the neighbourhood names from TorontoCensus to aggregated_cases by row order
aggregated_cases$"Neighbourhood Name" <- TorontoCensus$"Neighbourhood Name"

# Step 4: Perform the join (left join)
TorontoCensus <- merge(
  TorontoCensus,
  aggregated_cases,
  by = "Neighbourhood Name",
  all.x = TRUE)

# ---------------------------------------------------------------------------------------------------

### Loading in shapefile of COVID-19 Testing Sites (Point Data)
CovidTestingSitesShapefile <- st_read("covid-19-testing-sites - 4326.shp")

# ---------------------------------------------------------------------------------------------------

### Loading in shapefile of COVID-19 Immunization Sites (Point Data)
CovidImmunizationSitesShapefile <- st_read("covid-19-immunization-clinics - 4326.shp")

# ---------------------------------------------------------------------------------------------------

### Loading in shapefile of Toronto split by its 158 neighbourhoods (Polygon Data)
TorontoShapefile <- st_read("Neighbourhoods - historical 140 - 4326.shp")

# Renaming Neighbourhood name column to match the census dataframe:
TorontoShapefile <- TorontoShapefile %>%
  rename(`Neighbourhood Name` = AREA_NA7)

# ---------------------------------------------------------------------------------------------------

### Merging the census data with the Toronto shapefile
# This will produce a final dataset with neighbourhoods as polygons
# along with census data and covid case data by neighbourhood

# Step 1: Sort both datasets by the "Neighbourhood Name" column
TorontoCensus <- TorontoCensus[order(TorontoCensus$`Neighbourhood Name`), ]
TorontoShapefile <- TorontoShapefile[order(TorontoShapefile$`Neighbourhood Name`), ]

# Step 2: Assign the sorted names from TorontoCensus to TorontoShapefile
TorontoShapefile$`Neighbourhood Name` <- TorontoCensus$`Neighbourhood Name`

# Step 3: Merge the datasets
TorontoCensusShapefile <- merge(
  TorontoCensus, 
  TorontoShapefile[, c("Neighbourhood Name", "geometry")],  # Select relevant columns from TorontoShapefile
  by = "Neighbourhood Name", 
  all.x = TRUE  # Retain all rows from TorontoCensus
)

# Convert back to shapefile format
TorontoCensusShapefile <- st_as_sf(TorontoCensusShapefile)

### Creating variables of interest, and converting variables from character to numeric format:

TorontoCensusShapefile$"Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households - 25% sample data" <- as.numeric(TorontoCensusShapefile$"Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households - 25% sample data")
TorontoCensusShapefile$"    University certificate, diploma or degree at bachelor level or above.1" <- as.numeric(TorontoCensusShapefile$"    University certificate, diploma or degree at bachelor level or above.1")

TorontoCensusShapefile$PercentBachelorsorHigher <- ((TorontoCensusShapefile$"    University certificate, diploma or degree at bachelor level or above.1") / (TorontoCensusShapefile$"Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households - 25% sample data")) * 100

TorontoCensusShapefile$"Total - Visible minority for the population in private households - 25% sample data" <- 
  as.numeric(gsub(",", "", TorontoCensusShapefile$"Total - Visible minority for the population in private households - 25% sample data"))

TorontoCensusShapefile$"  Total visible minority population" <- 
  as.numeric(gsub(",", "", TorontoCensusShapefile$"  Total visible minority population"))

TorontoCensusShapefile$PercentVisibleMinority <- ((TorontoCensusShapefile$"  Total visible minority population") / (TorontoCensusShapefile$"Total - Visible minority for the population in private households - 25% sample data")) * 100

TorontoCensusShapefile$"Unemployment rate" <- as.numeric(TorontoCensusShapefile$"Unemployment rate")

TorontoCensusShapefile$"Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)" <- as.numeric(TorontoCensusShapefile$"Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)")

TorontoCensusShapefile$"Population density per square kilometre" <- 
  as.numeric(gsub(",", "", TorontoCensusShapefile$"Population density per square kilometre"))

TorontoCensusShapefile$"Total - Population aged 15 years and over by Labour force status - 25% sample data" <- 
  as.numeric(gsub(",", "", TorontoCensusShapefile$"Total - Population aged 15 years and over by Labour force status - 25% sample data"))

TorontoCensusShapefile$"Total - Population aged 15 years and over by Labour force status (Males) - 25% sample data" <- 
  as.numeric(gsub(",", "", TorontoCensusShapefile$"Total - Population aged 15 years and over by Labour force status (Males) - 25% sample data"))

TorontoCensusShapefile$PercentMale <- ((TorontoCensusShapefile$"Total - Population aged 15 years and over by Labour force status (Males) - 25% sample data") / (TorontoCensusShapefile$"Total - Population aged 15 years and over by Labour force status - 25% sample data")) * 100

TorontoCensusShapefile$"First official language spoken for the total population excluding institutional residents" <- 
  as.numeric(gsub(",", "", TorontoCensusShapefile$"First official language spoken for the total population excluding institutional residents"))

TorontoCensusShapefile$"  Neither English nor French" <- 
  as.numeric(gsub(",", "", TorontoCensusShapefile$"  Neither English nor French"))

TorontoCensusShapefile$PercentNonEnglishorFrench <- ((TorontoCensusShapefile$"  Neither English nor French") / (TorontoCensusShapefile$"First official language spoken for the total population excluding institutional residents")) * 100

# Remove the percentage signs and convert to numeric
TorontoCensusShapefile$`Persons living alone (per cent)` <- as.numeric(gsub("%", "", TorontoCensusShapefile$`Persons living alone (per cent)`))

# Perform spatial join: for each polygon, join immunization sites that intersect with it
# This will create a new sf object with all immunization sites inside each polygon
joined_data <- st_join(TorontoCensusShapefile, CovidImmunizationSitesShapefile, join = st_intersects)

# Now, count how many immunization sites there are in each neighbourhood (polygon)
# We group by Neighbourhood Name and count the immunization sites
site_count <- joined_data %>%
  group_by(`Neighbourhood Name`) %>%  # Correct column reference with backticks
  summarise(TotalImmunizationSites = n(), .groups = 'drop')

# Ensure site_count is a regular data frame by removing geometry
site_count <- as.data.frame(site_count)  # Convert to a regular data frame

TorontoCensusShapefile <- merge(TorontoCensusShapefile, site_count[, c("Neighbourhood Name", "TotalImmunizationSites")], 
                                by = "Neighbourhood Name", all.x = TRUE)

TorontoCensusShapefile$TotalImmunizationSites <- as.numeric(TorontoCensusShapefile$TotalImmunizationSites)

### A Priori Variables of interest that are ready and now in numeric format
TorontoCensusShapefile$"Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)"
TorontoCensusShapefile$"Unemployment rate"
TorontoCensusShapefile$"Population density per square kilometre"
TorontoCensusShapefile$"PercentBachelorsorHigher" # Percent of pop. aged 25-64 that has atleast a bachelors
TorontoCensusShapefile$"PercentVisibleMinority" # Percent of pop. considered a visible minority group
TorontoCensusShapefile$"Persons living alone (per cent)" # Percent of pop. over 15 years of age that is living alone
TorontoCensusShapefile$"PercentMale" # Percent of pop. over 15 years of age that is male
TorontoCensusShapefile$"PercentNonEnglishorFrench" # Percent of pop. with first language neither English or French
TorontoCensusShapefile$"TotalImmunizationSites" # Total number of COVID immunization sites in each Toronto Neighbourhood

# Mapping
tm_shape(TorontoCensusShapefile) +
  tm_polygons("TotalCovidCases", 
              style = "quantile", 
              palette = "Blues", 
              title = "Total COVID-19 Cases") +
  tm_layout(main.title = "Choropleth Map of Total COVID-19 Cases from Jan 2020 to \n Feb 2023 by Toronto Neighbourhood",
            main.title.size = 1.0,  # Adjust this value to make the title larger
            legend.outside = TRUE,
            legend.title.size = 0.9)

### Hotspot-coldspot map using Getis Ord Stats and Permutation Testing
TorontoCensusShapefileSpatialFormat <- as_Spatial(TorontoCensusShapefile)

# creates centroid and joins neighbours within 0 and x unit
nb <- dnearneigh(coordinates(TorontoCensusShapefileSpatialFormat),0,0.04)

# creates listw
nb_lw <- nb2listw(nb, style = 'B')

local_g_perm = localG_perm(TorontoCensusShapefileSpatialFormat$TotalCovidCases, nb_lw, nsim=999)
local_g = local_g_perm[1:140]
internals <- attr(local_g_perm, "internals")
quadrant <- vector(mode = "numeric", length =nrow(TorontoCensusShapefileSpatialFormat))
quadrant[internals[,7] >0.1] <-0
quadrant[local_g >0 & internals [,7] <0.1] <- 2
quadrant[local_g <0 & internals [,7] <0.1] <- 1
brks <- c(0,1,2)
colors <- c("white", "blue", "red")
plot(TorontoCensusShapefileSpatialFormat, border = "lightgray", col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
box()
legend("bottomright", legend=c("Insignificant (p>0.1)", "cold (p<0.1)", "hot(p<0.1)"), fill=colors, bty="n")
title(main = "Hot and Cold Spots of Total COVID-19 Cases by Toronto's Neighbourhoods")

# Moran Method:

# Global Moran Spatial Autocorrelation Test
neighbours <- poly2nb(TorontoCensusShapefileSpatialFormat)
listw <- nb2listw(neighbours)
moran.test(TorontoCensusShapefileSpatialFormat$TotalCovidCases, listw)

# Local Moran's Analysis
local <- localmoran(x = TorontoCensusShapefileSpatialFormat$TotalCovidCases,listw = nb2listw(neighbours, style = "W"))

# binds results to our polygon shapefile
moran.map <- cbind(TorontoCensusShapefileSpatialFormat, local)

# maps the results
tm_shape(moran.map) + tm_fill(col = "Ii", style = "quantile",title = "local moran statistic") + tm_borders(alpha=.4)

# Creating LISA Plot

#creating a numeric vetor
quadrant <- vector(mode="numeric",length=nrow(local))

#center the variable of interest around its mean
m.covid <- TorontoCensusShapefileSpatialFormat$TotalCovidCases - mean(TorontoCensusShapefileSpatialFormat$TotalCovidCases)

# centers the local Moran's around the mean
m.local <- local[,1] - mean(local[,1])

# significance threshold
signif <- 0.1

# builds a data quadrant
quadrant[m.covid >0 & m.local>0] <- 4
quadrant[m.covid <0 & m.local<0] <- 1
quadrant[m.covid <0 & m.local>0] <- 2
quadrant[m.covid >0 & m.local<0] <- 3

quadrant[local[,5]>signif] <- 0

# plot in R
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(TorontoCensusShapefileSpatialFormat,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
box()
legend("bottomright",legend=c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n")

# ---------------------------------------------------------------------------------------------------

#Building a linear model and checking VIFs of pre-selected variables

#1
model1 <- lm(TotalCovidCases ~ `Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)` + 
               `Unemployment rate` + `Population density per square kilometre` + 
               `PercentBachelorsorHigher` + `PercentVisibleMinority` +
               `Persons living alone (per cent)` + `PercentMale` + 
               `PercentNonEnglishorFrench` + `TotalImmunizationSites`, data = TorontoCensusShapefile)
               
vif(model1)

#some variables have VIFs between 4-5
#% low income and unemployment rate seem to be similar. 

hist(TorontoCensusShapefile$`Unemployment rate`)
hist(TorontoCensusShapefile$`Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)`)

#distirbutions are similar
#Low income may provide more information than unemployment rate as it would include information on 
#individuals who work, but do not make enough money in comparison to average income

#removing unemployment rate

#2
model2 <- lm(TotalCovidCases ~ `Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)` + 
                + `Population density per square kilometre` + 
               `PercentBachelorsorHigher` + `PercentVisibleMinority` +
               `Persons living alone (per cent)` + `PercentMale` + 
               `PercentNonEnglishorFrench` + `TotalImmunizationSites`, data = TorontoCensusShapefile)

vif(model2)

#All VIFs below 4. Percent visible minority very close to 4. 


# ---------------------------------------------------------------------------------------------------

#Building choroleth maps

#removing spaces so easier to call for variables when using tmap()

# Replace spaces and special characters with underscores
#names(TorontoCensusShapefile) <- gsub("[[:space:]]+", "_", names(TorontoCensusShapefile))
#names(TorontoCensusShapefile) <- gsub("[[:punct:]]", "", names(TorontoCensusShapefile))

TorontoCensusShapefile$"Population, 2016" <- as.numeric(gsub(",", "", TorontoCensusShapefile$"Population, 2016"))

TorontoCensusShapefile$"TotalCasesper1000" <- ((TorontoCensusShapefile$"TotalCovidCases")/(TorontoCensusShapefile$"Population, 2016")) * 1000


#Total COVID-19 cases
tm_shape(TorontoCensusShapefile) +
  tm_polygons("TotalCovidCases", 
              style = "quantile", 
              palette = "Blues", 
              title = "Low Income") +
  tm_layout(main.title = "Choropleth Map of Total COVID-19 Cases from Jan 2020 to Feb 2023 by Toronto Neighbourhood",
            main.title.size = 1.0,  # Adjust this value to make the title larger
            legend.outside = TRUE,
            legend.title.size = 0.9)

#Low Income
tm_shape(TorontoCensusShapefile) +
  tm_polygons("Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)", 
              style = "quantile", 
              palette = "Blues", 
              title = "Low Income") +
  tm_layout(main.title = "Choropleth Map of Total COVID-19 Cases from Jan 2020 to Feb 2023 by Toronto Neighbourhood",
            main.title.size = 1.0,  # Adjust this value to make the title larger
            legend.outside = TRUE,
            legend.title.size = 0.9)

#Low income with covid cases
tm_shape(TorontoCensusShapefile) + 
  tm_polygons("Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)", 
              style = "quantile", 
              palette = "Blues", 
              title = " % Low Income") +
  tm_bubbles(size = "TotalCovidCases", 
             col = "red",        # Change the dot color to purple
             scale = 0.7) +         # Adjust the scale of the dots
  tm_layout(main.title = "Figure 2. Map of % Low Income and Total COVID-19 Cases, \n by Toronto Neighbourhood",
            main.title.size = 1.0,  # Adjust this value to make the title larger
            legend.outside = TRUE,
            legend.title.size = 1.5)

#Population density
tm_shape(TorontoCensusShapefile) +
  tm_polygons("Population density per square kilometre", 
              style = "quantile", 
              palette = "Blues", 
              title = "Population density") +
  tm_layout(main.title = "Choropleth Map of population density from Jan 2020 to Feb 2023 by Toronto Neighbourhood",
            main.title.size = 1.0,  # Adjust this value to make the title larger
            legend.outside = TRUE,
            legend.title.size = 0.9)

#Population density with covid-19 cases
tm_shape(TorontoCensusShapefile) +
  tm_polygons("Population density per square kilometre", 
              style = "quantile", 
              palette = "Blues", 
              title = "Population density/km^2") +
  tm_bubbles(size = "TotalCovidCases", 
             col = "red",        
             scale = 0.7) +
  tm_layout(main.title = "Figure 3. Map of Population Density and Total Covid-19 Cases, by \n Toronto Neighbourhood",
            main.title.size = 1.0,  
            legend.outside = TRUE,
            legend.title.size = 0.9)

#% Bachelor's
tm_shape(TorontoCensusShapefile) +
  tm_polygons("PercentBachelorsorHigher", 
              style = "quantile", 
              palette = "Blues", 
              title = "% Bachelor's") +
  tm_layout(main.title = "ChoroplethMap of % Bachelor's from Jan 2020 to Feb 2023 by Toronto Neighbourhood",
            main.title.size = 1.0,  # Adjust this value to make the title larger
            legend.outside = TRUE,
            legend.title.size = 0.9)

#% Bachelor's with covid-19 cases
tm_shape(TorontoCensusShapefile) +
  tm_polygons("PercentBachelorsorHigher", 
              style = "quantile", 
              palette = "Blues", 
              title = "% Bachelor's") +
  tm_bubbles(size = "TotalCovidCases", 
             col = "red",        
             scale = 0.7) +
  tm_layout(main.title = "Figure 4. Map of % with Bachelor's degree and \n Total COVID-19 Cases, by Toronto Neighbourhood",
            main.title.size = 1.0,  # Adjust this value to make the title larger
            legend.outside = TRUE,
            legend.title.size = 0.9)

#PercentVisibleMinority
tm_shape(TorontoCensusShapefile) +
  tm_polygons("PercentVisibleMinority", 
              style = "quantile", 
              palette = "Blues", 
              title = "% Minority") +
  tm_layout(main.title = "Choropleth Map of % visible minority from Jan 2020 to Feb 2023 by Toronto Neighbourhood",
            main.title.size = 1.0,  # Adjust this value to make the title larger
            legend.outside = TRUE,
            legend.title.size = 0.9)

#PercentVisibleMinority with covid-19 cases
tm_shape(TorontoCensusShapefile) +
  tm_polygons("PercentVisibleMinority", 
              style = "quantile", 
              palette = "Blues", 
              title = "% Minority") +
  tm_bubbles(size = "TotalCovidCases", 
             col = "red",        
             scale = 0.7) +
  tm_layout(main.title = "Figure 5. Map of % Visible Minority and Total COVID-19 Cases, by Toronto Neighbourhood",
            main.title.size = 1.0,  # Adjust this value to make the title larger
            legend.outside = TRUE,
            legend.title.size = 0.9)


#%living alone
tm_shape(TorontoCensusShapefile) +
  tm_polygons("Persons living alone (per cent)", 
              style = "quantile", 
              palette = "Blues", 
              title = "% Living alone") +
  tm_layout(main.title = "Choropleth Map of % living alone from Jan 2020 to Feb 2023 by Toronto Neighbourhood",
            main.title.size = 1.0,  # Adjust this value to make the title larger
            legend.outside = TRUE,
            legend.title.size = 0.9)

#%living alone
tm_shape(TorontoCensusShapefile) +
  tm_polygons("Persons living alone (per cent)", 
              style = "quantile", 
              palette = "Blues", 
              title = "% Living alone") +
  tm_bubbles(size = "TotalCovidCases", 
             col = "red",        
             scale = 0.7) +
  tm_layout(main.title = "Figure 6. Map of % living alone and Total COVID-19 Cases, by Toronto Neighbourhood",
            main.title.size = 1.0,  # Adjust this value to make the title larger
            legend.outside = TRUE,
            legend.title.size = 0.9)


# % Male
tm_shape(TorontoCensusShapefile) +
  tm_polygons("PercentMale", 
              style = "quantile", 
              palette = "Blues", 
              title = "%Male") +
  tm_layout(main.title = "Choropleth Map of % male from Jan 2020 to Feb 2023 by Toronto Neighbourhood",
            main.title.size = 1.0,  # Adjust this value to make the title larger
            legend.outside = TRUE,
            legend.title.size = 0.9)

# % Male with covid-19 cases
tm_shape(TorontoCensusShapefile) +
  tm_polygons("PercentMale", 
              style = "quantile", 
              palette = "Blues", 
              title = "%Male") +
  tm_bubbles(size = "TotalCovidCases", 
             col = "red",        
             scale = 0.7) +
  tm_layout(main.title = "Figure 7. Map of % Male and Total COVID-19 Cases, by Toronto Neighbourhood",
            main.title.size = 1.0,  # Adjust this value to make the title larger
            legend.outside = TRUE,
            legend.title.size = 0.9)

#% Non-English/French
tm_shape(TorontoCensusShapefile) +
  tm_polygons("PercentNonEnglishorFrench", 
              style = "quantile", 
              palette = "Blues", 
              title = "% Non-English/French") +
  tm_layout(main.title = "Choropleth Map of % Non-English/French from Jan 2020 to Feb 2023 by Toronto Neighbourhood",
            main.title.size = 1.0,  # Adjust this value to make the title larger
            legend.outside = TRUE,
            legend.title.size = 0.9)

#% Non-English/French with covid-19 cases
tm_shape(TorontoCensusShapefile) +
  tm_polygons("PercentNonEnglishorFrench", 
              style = "quantile", 
              palette = "Blues", 
              title = "% Non-English/French") +
  tm_bubbles(size = "TotalCovidCases", 
             col = "red",        
             scale = 0.7) +
  tm_layout(main.title = "Figure 8. Map of % Non-English/French and Total COVID-19 Cases, by Toronto Neighbourhood",
            main.title.size = 1.0,  # Adjust this value to make the title larger
            legend.outside = TRUE,
            legend.title.size = 0.9)

#Total immunizations
tm_shape(TorontoCensusShapefile) +
  tm_polygons("TotalImmunizationSites", 
              style = "quantile", 
              palette = "Blues", 
              title = "% immunizations") +
  tm_layout(main.title = "Map of % immunizations from Jan 2020 to Feb 2023 by Toronto Neighbourhood",
            main.title.size = 1.0,  # Adjust this value to make the title larger
            legend.outside = TRUE,
            legend.title.size = 0.9)

#Total immunizations with covid-19 cases
tm_shape(TorontoCensusShapefile) +
  tm_polygons("TotalImmunizationSites", 
              style = "quantile", 
              palette = "Blues", 
              title = "Number of immunization sites") +
  tm_bubbles(size = "TotalCovidCases", 
             col = "red",        
             scale = 0.7) +
  tm_layout(main.title = "Figure 9. Map of Number of Immunization sites and Total COVID-19 Cases, \n by Toronto Neighbourhood",
            main.title.size = 1.0,  # Adjust this value to make the title larger
            legend.outside = TRUE,
            legend.title.size = 0.9)

# ---------------------------------------------------------------------------------------------------

#Immunization sites variable has issues with bandwidth selection as it only has 13 unique values. Will remove this

#Geographically-weighted Regression

#as_spatial again
TorontoCensusShapefileSpatialFormat <- as_Spatial(TorontoCensusShapefile)

# Extract coordinates from the SpatialPolygonsDataFrame
coords <- coordinates(TorontoCensusShapefileSpatialFormat)

# Check the structure of the coordinates
length(coords)


#GWR bandwitch
GWRbandwidth <- gwr.sel(TotalCovidCases ~ `Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)` + 
                          + `Population density per square kilometre` + 
                          `PercentBachelorsorHigher` + `PercentVisibleMinority` +
                          `Persons living alone (per cent)` + `PercentMale` + 
                          `PercentNonEnglishorFrench` + `TotalImmunizationSites`, data = TorontoCensusShapefile,
                        coords = coords, adapt = TRUE) 


#GWRbandwidth <- gwr.sel(m.covid ~ m.lowincome + 
#                          + m.popdens + 
#                          m.per_bach + m.per_minority +
#                         m.alone + m.male + 
#                          m.nonenglish + m.immunization_sites, data = TorontoCensusShapefile,
#                       coords = coords, adapt = FALSE) #Getting warnings but works

#Centering other variables
#TorontoCensusShapefile$m.lowincome <- TorontoCensusShapefile$`Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)` - mean(TorontoCensusShapefile$`Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)`)
#TorontoCensusShapefile$m.popdens <- TorontoCensusShapefile$`Population density per square kilometre` - mean(TorontoCensusShapefile$`Population density per square kilometre`)
#TorontoCensusShapefile$m.per_bach  <- TorontoCensusShapefile$`PercentBachelorsorHigher` - mean(TorontoCensusShapefile$`PercentBachelorsorHigher`)
#TorontoCensusShapefile$m.per_minority  <- TorontoCensusShapefile$`PercentVisibleMinority` - mean(TorontoCensusShapefile$`PercentVisibleMinority`)
#TorontoCensusShapefile$m.alone  <- TorontoCensusShapefile$`Persons living alone (per cent)` - mean(TorontoCensusShapefile$`Persons living alone (per cent)`)
#TorontoCensusShapefile$m.male  <- TorontoCensusShapefile$`PercentMale` - mean(TorontoCensusShapefile$`PercentMale`)
#TorontoCensusShapefile$m.nonenglish  <- TorontoCensusShapefile$`PercentNonEnglishorFrench` - mean(TorontoCensusShapefile$`PercentNonEnglishorFrench`)
#TorontoCensusShapefile$m.immunization_sites  <- TorontoCensusShapefile$`TotalImmunizationSites` - mean(TorontoCensusShapefile$`TotalImmunizationSites`)


#GWR
gwr.model <- TorontoCensusShapefile$TotalCovidCases ~ 
  TorontoCensusShapefile$`Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)` +
  TorontoCensusShapefile$`Population density per square kilometre` +
  TorontoCensusShapefile$`PercentBachelorsorHigher` +
  TorontoCensusShapefile$`PercentVisibleMinority` +
  TorontoCensusShapefile$`Persons living alone (per cent)` +
  TorontoCensusShapefile$`PercentMale` +
  TorontoCensusShapefile$`PercentNonEnglishorFrench` +
  TorontoCensusShapefile$`TotalImmunizationSites`
  

gwr.model.fit <- gwr(
  formula = gwr.model,
  data = TorontoCensusShapefile,
  coords = coords,
  bandwidth = GWRbandwidth, 
  hatmatrix = TRUE,
  se.fit = TRUE
)

#print the results of the model
gwr.model.fit

results <- as.data.frame(gwr.model.fit$SDF)
names(results)

#merging results
gwr.map <- cbind(TorontoCensusShapefile, as.matrix(results))
qtm(gwr.map, fill = "localR2")

#Mapping %Low Income
#map1 <- tm_shape(gwr.map) +
#  # Choropleth layer for Total Beds coefficients
#  tm_fill(
#    col = "TorontoCensusShapefile..Prevalence.of.low.income.based.on.the.Low.income.cut.offs..after.tax..LICO.AT......",  # Column for Total Beds coefficients
#    palette = "Blues",
#    title = "% Low Income Coefficient"
#  ) + tm_polygons() +
  # Dot layer for Local R2 values
#  tm_dots(
#    size = "localR2",  # Column for local R2 values
#    scale = 0.5,
#    col = "red",       # Dot color
#    alpha = 0.6,       # Transparency
#    title.size = "Local R2"
#  ) +
  # Layout adjustments
#  tm_layout(
#    legend.outside = TRUE, 
#    legend.text.size = 1.2, 
#    legend.title.size = 1.2
# )

#Mapping %Low Income
map1 <- tm_shape(gwr.map) + tm_fill("TorontoCensusShapefile..Prevalence.of.low.income.based.on.the.Low.income.cut.offs..after.tax..LICO.AT......", n = 5, style = "quantile", title = "Percentage Low Income") + tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6) +
  tm_layout(
    legend.outside = TRUE, 
    legend.text.size = 1.2, 
    legend.title.size = 1.2
  ) + tm_polygons()
map1

#Mapping Population density
#map2 <- tm_shape(gwr.map) +
  # Choropleth layer for Total Beds coefficients
#  tm_fill(
#    col = "TorontoCensusShapefile..Population.density.per.square.kilometre._se_EDF",  
#    palette = "Blues",
#    title = "Population Density Coefficient"
#  ) + tm_polygons() +
  # Layout adjustments
#  tm_layout(
#    legend.outside = TRUE, 
#    legend.text.size = 1.2, 
#    legend.title.size = 1.2
#  )
#map2

#Population Density
map2 <- tm_shape(gwr.map) + tm_fill("TorontoCensusShapefile..Population.density.per.square.kilometre._se_EDF", n = 5, style = "quantile", title = "Population density Coefficient") + tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6) +
  tm_layout(
    legend.outside = TRUE, 
    legend.text.size = 1.2, 
    legend.title.size = 1.2
  ) + tm_polygons()
map2

#Mapping % Bachelor's
#map3 <- tm_shape(gwr.map) +
#  # Choropleth layer for Total Beds coefficients
#  tm_fill(
#    col = "TorontoCensusShapefile.PercentBachelorsorHigher_se",  
#    palette = "Blues",
#    title = "% Bachelor's Coefficient"
#  ) + tm_polygons() + 
  # Dot layer for Local R2 values
#  tm_dots(
#    size = "localR2",  # Column for local R2 values
#    scale = 0.5,
#    col = "red",       # Dot color
#    alpha = 0.6,       # Transparency
#    title.size = "Local R2"
#  ) +
  # Layout adjustments
#  tm_layout(
#    legend.outside = TRUE, 
#    legend.text.size = 1.2, 
#    legend.title.size = 1.2
#  )
# map3

# % Bachelor's Education
map3 <- tm_shape(gwr.map) + tm_fill("TorontoCensusShapefile.PercentBachelorsorHigher_se", n = 5, style = "quantile", title = "Bachelor's education Coefficient") + tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6) +
  tm_layout(
    legend.outside = TRUE, 
    legend.text.size = 1.2, 
    legend.title.size = 1.2
  ) + tm_polygons()
map3

#Mapping % Visible Minority
#map4 <- tm_shape(gwr.map) +
  # Choropleth layer for Total Beds coefficients
#  tm_fill(
#    col = "TorontoCensusShapefile.PercentVisibleMinority",  
#    palette = "Blues",
#    title = "% Visible Minority Coefficient"
#  ) + tm_polygons() + 
  # Dot layer for Local R2 values
#  tm_dots(
#    size = "localR2",  # Column for local R2 values
#    scale = 0.5,
#    col = "red",       # Dot color
#    alpha = 0.6,       # Transparency
#    title.size = "Local R2"
#  ) +
  # Layout adjustments
#  tm_layout(
#    legend.outside = TRUE, 
#    legend.text.size = 1.2, 
#    legend.title.size = 1.2
#  )
#map4

# % Visible minority
map4 <- tm_shape(gwr.map) + tm_fill("TorontoCensusShapefile.PercentVisibleMinority", n = 5, style = "quantile", title = "% Visible Minority Coefficient") + tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6) +
  tm_layout(
    legend.outside = TRUE, 
    legend.text.size = 1.2, 
    legend.title.size = 1.2
  ) + tm_polygons()
map4

#Mapping % Living alone
#map5 <- tm_shape(gwr.map) +
  # Choropleth layer for Total Beds coefficients
#  tm_fill(
#    col = "TorontoCensusShapefile..Persons.living.alone..per.cent..",  
#    palette = "Blues",
#    title = "% Living alone Coefficient"
#  ) + tm_polygons() +
  # Dot layer for Local R2 values
 # tm_dots(
#    size = "localR2",  # Column for local R2 values
#    scale = 0.5,
#    col = "red",       # Dot color
#    alpha = 0.6,       # Transparency
#    title.size = "Local R2"
#  ) +
  # Layout adjustments
#  tm_layout(
#    legend.outside = TRUE, 
#    legend.text.size = 1.2, 
#    legend.title.size = 1.2
#  )
#map5

# % Living alone
map5 <- tm_shape(gwr.map) + tm_fill("TorontoCensusShapefile..Persons.living.alone..per.cent..", n = 5, style = "quantile", title = "% Living alone Coefficient") + tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6) +
  tm_layout(
    legend.outside = TRUE, 
    legend.text.size = 1.2, 
    legend.title.size = 1.2
  ) + tm_polygons()
map5

#Mapping %Low Income
#map6 <- tm_shape(gwr.map) +
#  # Choropleth layer for Total Beds coefficients
#  tm_fill(
#    col = "TorontoCensusShapefile.PercentMale",  
#    palette = "Blues",
#    title = "% Male Coefficient"
#  ) + tm_polygons() +
  # Dot layer for Local R2 values
#  tm_dots(
#    size = "localR2",  # Column for local R2 values
#    scale = 0.5,
#    col = "red",       # Dot color
#    alpha = 0.6,       # Transparency
#    title.size = "Local R2"
#  ) +
  # Layout adjustments
#  tm_layout(
#    legend.outside = TRUE, 
#    legend.text.size = 1.2, 
#    legend.title.size = 1.2
#  )
# map6

# % Male
map6 <- tm_shape(gwr.map) + tm_fill("TorontoCensusShapefile.PercentMale", n = 5, style = "quantile", title = "% Male Coefficient") + tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6) +
  tm_layout(
    legend.outside = TRUE, 
    legend.text.size = 1.2, 
    legend.title.size = 1.2
  ) + tm_polygons()
map6

#Mapping %Low Income
#map7 <- tm_shape(gwr.map) +
  # Choropleth layer for coefficients
#  tm_fill(
#    col = "TorontoCensusShapefile.PercentNonEnglishorFrench",  
#    palette = "Blues",
#    title = "% Non-english/French Coefficient"
#  ) + tm_polygons() + 
  # Dot layer for Local R2 values
#  tm_dots(
#    size = "localR2",  # Column for local R2 values
#    scale = 0.5,
#    col = "red",       # Dot color
#    alpha = 0.6,       # Transparency
#    title.size = "Local R2"
#  ) +
  # Layout adjustments
#  tm_layout(
#    legend.outside = TRUE, 
#    legend.text.size = 1.2, 
#    legend.title.size = 1.2
#  )
#map7

# % Non-english/French
map7 <- tm_shape(gwr.map) + tm_fill("TorontoCensusShapefile.PercentNonEnglishorFrench", n = 5, style = "quantile", title = "% Non-english/French Coefficient") + tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6) +
  tm_layout(
    legend.outside = TRUE, 
    legend.text.size = 1.2, 
    legend.title.size = 1.2
  ) + tm_polygons()
map7

#Mapping %Low Income
#map8 <- tm_shape(gwr.map) +
  # Choropleth layer for Total Beds coefficients
#  tm_fill(
#    col = "TorontoCensusShapefile.TotalImmunizationSites",  
#    palette = "Blues",
#    title = "Total Immunization Sites Coefficient"
#  ) + tm_polygons() + 
  # Dot layer for Local R2 values
#  tm_dots(
#    size = "localR2",  # Column for local R2 values
#    scale = 0.5,
#    col = "red",       # Dot color
#    alpha = 0.6,       # Transparency
#    title.size = "Local R2"
#  ) +
  # Layout adjustments
#  tm_layout(
#    legend.outside = TRUE, 
#    legend.text.size = 1.2, 
#    legend.title.size = 1.2
#  )
#map8

#Total immunizations
map8 <- tm_shape(gwr.map) + tm_fill("TorontoCensusShapefile.TotalImmunizationSites", n = 5, style = "quantile", title = "Total Immunization Sites Coefficient") + tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6) +
  tm_layout(
    legend.outside = TRUE, 
    legend.text.size = 1.2, 
    legend.title.size = 1.2
  ) + tm_polygons()
map8

#Mapping together
grid.newpage()
pushViewport(viewport(layout=grid.layout(4,2)))
print(map1, vp=viewport(layout.pos.col = 1, layout.pos.row = 1))
print(map2, vp=viewport(layout.pos.col = 2, layout.pos.row = 1))
print(map3, vp=viewport(layout.pos.col = 1, layout.pos.row = 2))
print(map4, vp=viewport(layout.pos.col = 2, layout.pos.row = 2))
print(map5, vp=viewport(layout.pos.col = 1, layout.pos.row = 3))
print(map6, vp=viewport(layout.pos.col = 2, layout.pos.row = 3))
print(map7, vp=viewport(layout.pos.col = 1, layout.pos.row = 4))
print(map8, vp=viewport(layout.pos.col = 2, layout.pos.row = 4))
