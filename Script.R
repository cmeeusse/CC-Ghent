library(tidyverse)  # Hadley Wickham's tidyverse - the theme of this tutorial
library(broom)  # To summarise model outputs
library(ggExtra)  # To make pretty graphs - addon package to ggplot2
library(maps)  # To make pretty maps - warning: maps masks map from purr!
library(RColorBrewer)  # To make pretty colours
library(gridExtra)  # To arrange multi-plot panels

####################################################################
####################################################################
# Setting a custom ggplot2 function ---
# *** Functional Programming ***
# This function makes a pretty ggplot theme
# This function takes no arguments!
theme_LPD <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.title = element_blank(),                              
          legend.position = c(0.5, 0.8))
}
####################################################################
####################################################################
# Load data ----
load("LPDdata_Feb2016.RData")

# Inspect data ----
head(LPDdata_Feb2016)

# Format data for analysis ----

# Transform from wide to long format usign gather (opposite is spread)
# *** gather() function from the dplyr package in the tidyverse ***
LPD_long <- gather(data = LPDdata_Feb2016, key = "year", value = "pop", select = 26:70)

# Get rid of the X in front of years
# *** parse_number() from the readr package in the tidyverse ***
LPD_long$year <- parse_number(LPD_long$year)

# Rename variable names for consistency
names(LPD_long)
names(LPD_long) <- tolower(names(LPD_long)) # alles met een kleine letter
names(LPD_long)

# Create new column with genus and species together
LPD_long$species.name <- paste(LPD_long$genus, LPD_long$species, sep = " ")

# Get rid of strange characters like " / "
LPD_long$country.list <- gsub(",", "", LPD_long$country.list, fixed = TRUE)
LPD_long$biome <- gsub("/", "", LPD_long$biome, fixed = TRUE)

# Examine the tidy data frame
head(LPD_long)

####################################################################
####################################################################
# Data manipulation ----

# *** piping from from dplyr
LPD_long2 <- LPD_long %>%
  # Remove duplicate rows
  # *** distinct() function from dplyr
  distinct(LPD_long) %>%
  # remove NAs in the population column
  # *** filter() function from dplyr
  filter(is.finite(pop)) %>%
  # Group rows so that each group is one population
  # *** group_by() function from dplyr
  group_by(id) %>%  
  # Make some calculations
  # *** mutate() function from dplyr
  mutate(maxyear = max(year), minyear = min(year),
         # Calculate duration
         duration = maxyear - minyear,
         # Scale population trend data
         scalepop = (pop - min(pop))/(max(pop) - min(pop))) %>%
  # Keep populations with >5 years worth of data and calculate length of monitoring
  filter(is.finite(scalepop),
         length(unique(year)) > 5) %>%
  # Remove any groupings you've greated in the pipe
  ungroup()

# Calculate summary statistics for each biome
LPD_biome_sum <- LPD_long2 %>%
  # Group by biome
  group_by(biome) %>%
  # Create columns, number of populations
  #  *** summarise()/summarize() function from dplyr in the tidyverse ***
  summarise(populations = n(),   
            # Calculate the mean study length
            mean_study_length_years = mean(duration),
            # Model sampling method
            dominant_sampling_method = names(which.max(table(sampling.method))),
            # Model unit type
            dominant_units = names(which.max(table(units)))) %>%
  # Remove any groupings you've greated in the pipe
  ungroup()

# Take a look at some of the records
head(LPD_biome_sum)

# Subset to just temperate forest species -----
# Notice the difference between | and & when filtering
# | is an "or" whereas & is "and", i.e. both conditions have to be met
# at the same time
LPD_long2$biome <- as.factor(LPD_long2$biome)
LPD.forest <- filter(LPD_long2, biome == "Temperate broadleaf and mixed forests" |
                       biome == "Temperate coniferous forests")

####################################################################
####################################################################

# Data visualisation ----
# Data distribution - a histogram
(forest.hist <- ggplot(LPD.forest, aes(x = scalepop)) +
   geom_histogram(aes(fill = biome),
                  position = "identity", colour = "grey40") +
   geom_vline(aes(xintercept = mean(scalepop)),  # Adding a line for mean abundance
              colour = "darkred", linetype = "dashed", size = 1) +
   scale_fill_manual(values = c("#66CD00", "#53868B")) +
   theme_LPD() +
   labs(title = "a) Data distribution\n") +
   guides(fill = F)) # Hiding the legend - this will be a two plot panel
# thus we don't need the same legend twice

# Density histogram of duration of studies in the two biomes
(duration.forests <- ggplot(LPD.forest, aes(duration, colour = biome)) +
    stat_density(geom = "line", size = 2, position = "identity") +
    theme_LPD() +
    scale_colour_manual(values = c("#66CD00", "#53868B")) +
    labs(x = "\nYears", y = "Density\n", title = "b) Monitoring duration\n"))

# Calculate population change for each forest population
# 1785 models in one go!
# Using a pipe
forest.slopes <- LPD.forest %>%
  # Group by the key variables that we want to interate over
  group_by(decimal.latitude, decimal.longitude, class, species.name, id, duration, location.of.population) %>%
  # Create a linear model for each group
  do(mod = lm(scalepop ~ year, data = .)) %>%
  # Extract model coefficients using tidy() from the
  # *** tidy() function from the broom package ***
  tidy(mod) %>%
  # Filter out slopes and remove intercept values
  filter(term == "year") %>%
  # Get rid of the column term as we don't need it any more
  #  *** select() function from dplyr in the tidyverse ***
  dplyr::select(-term) %>%
  # Remove any groupings you've greated in the pipe
  ungroup()


####################################################################
####################################################################
# Visualising model outputs ----

# Plotting slope estimates and standard errors for all populations and adding histograms along the margins
(all.slopes <- ggplot(forest.slopes, aes(x = duration, y = estimate)) +
   geom_pointrange(aes(ymin = estimate - std.error,
                       ymax = estimate + std.error),
                   alpha = 0.3, size = 0.3) +
   geom_hline(yintercept = 0, linetype = "dashed") +
   theme_LPD() +
   ylab("Population change\n") +
   xlab("\nDuration (years)"))

(density.slopes <- ggExtra::ggMarginal(
  p = all.slopes,
  type = 'density',
  margins = 'both',
  size = 5,
  col = 'gray40',
  fill = 'gray'
))

# Save the plot
ggsave(density.slopes, filename = "slopes_duration.png", height = 6, width = 6)


####################################################################
####################################################################
####################################################################
####################################################################
# PART 2: Using pipes to make figures with large datasets ----

# Make histograms of slope estimates for each taxa -----
# Set up new folder for figures
# Set path to relevant path on your computer/in your repository
path1 <- "Taxa_Forest_LPD/"
# Create new folder
dir.create(path1)

# First we will do this using dplyr and a pipe

forest.slopes %>%
  # Select the relevant data
  dplyr::select(id, class, species.name, estimate) %>%
  # Group by taxa
  group_by(class) %>%
  # Save all plots in new folder
  do(ggsave(ggplot(., aes(x = estimate)) +
              # Add histograms
              geom_histogram(colour = "darkgreen", fill = "darkgreen", binwidth = 0.02) +
              # Use custom theme
              theme_LPD() +
              # Add axis lables
              xlab("Rate of population change (slopes)"),
            # Set up file names to print to
            filename = gsub("", "", paste0(path1, unique(as.character(.$class)),
                                           ".pdf")), device = "pdf"))

# Here we select the relevant data
# Let's get rid of the other levels of 'class'
forest.slopes$class <- as.character(forest.slopes$class)
# Selecting the relevant data and splitting it into a list
taxa.slopes <- forest.slopes %>%
  dplyr::select(id, class, estimate) %>%
  spread(class, estimate) %>%
  dplyr::select(-id)

taxa.mean <- purrr::map(taxa.slopes, ~mean(., na.rm = TRUE))
# This plots the mean population change per taxa
taxa.mean

### Intro to the purrr package ----

# First let's write a function to make the plots
# *** Functional Programming ***
# This function takes one argument x, the data vector that we want to make a histogram
plot.hist <- function(x) {
  ggplot() +
    geom_histogram(aes(x), colour = "darkgreen", fill = "darkgreen", binwidth = 0.02) +
    theme_LPD() +
    xlab("Rate of population change (slopes)")
}

taxa.plots <- purrr::map(taxa.slopes, ~plot.hist(.))
# We need to make a new folder to put these figures in
path2 <- "Users\gebruiker\Documents\CC-Ghent\Taxa_Forest_LPD"
dir.create(path2)

# *** walk2() function in purrr from the tidyverse ***
walk2(paste0(path2, names(taxa.slopes), ".pdf"), taxa.plots, ggsave)


####################################################################
####################################################################
####################################################################
####################################################################
### PART 3: Downloading and mapping data from large datasets ----
#### How to map distributions and monitoring locations for one or more taxa

# Packages ----
library(rgbif)  # To extract GBIF data
# library(CoordinateCleaner)  # To clean coordinates if you want to explore that later
library(gridExtra)  # To make pretty graphs
library(ggrepel)  # To add labels with rounded edges
library(png)  # To add icons
library(mapdata)  # To plot maps
library(ggthemes)  # To make maps extra pretty

# Download species occurrence records from the Global Biodiversity Information Facility
# *** rgbif package and the occ_search() function ***
# You can increase the limit to get more records - 5000 takes a couple of minutes
deer.locations <- occ_search(scientificName = "Cervus elaphus", limit = 5000,
                             hasCoordinate = TRUE, return = "data") %>%
  # Simplify occurrence data frame
  dplyr::select(key, name, decimalLongitude, 
                decimalLatitude, year, 
                individualCount, country)
# Data formatting & manipulation ----

# Filter out population data for chosen species - red deer
deer.data <- LPD_long2 %>%
  filter(species.name == "Cervus elaphus") %>%
  dplyr::select(id, species.name, location.of.population, year, pop)

# Filter out population estimates for chosen species - red deer
deer.slopes <- forest.slopes %>%
  filter(species.name == "Cervus elaphus")

# Make an occurrence map and include the locations of the populations part of the Living Planet Database
(deer.map.LPD <- ggplot(deer.locations, aes(x = decimalLongitude, y = decimalLatitude)) +
    # Add map data
    borders("world", colour = "gray80", fill = "gray80", size = 0.3) +
    # Use custom map theme from ggthemes package
    theme_map() +
    # Add the points from the population change data
    geom_point(alpha = 0.3, size = 2, colour = "aquamarine3") +
    # Specify where the data come from when plotting from more than one data frame using data = ""
    geom_point(data = deer.slopes, aes(x = decimal.longitude, y = decimal.latitude),
               size = 2, colour = "darkgreen"))
               
##Customising map to make it more beautiful ----
  
  # Check site names
print(deer.slopes$location.of.population)

# Beautify site names
deer.slopes$location.of.population <- recode(deer.slopes$location.of.population,
                                             "Northern Yellowstone National Park" 
                                             = "Yellowstone National Park")
deer.slopes$location.of.population <- recode(deer.slopes$location.of.population,
                                             "Mount Rainier National Park, USA" 
                                             = "Mount Rainier National Park")
deer.slopes$location.of.population <- recode(deer.slopes$location.of.population,
                                             "Bow Valley - eastern zone, Banff National Park, Alberta" = 
                                               "Banff National Park, Alberta")
deer.slopes$location.of.population <- recode(deer.slopes$location.of.population,
                                             "Bow Valley - western zone, Banff National Park, Alberta" = 
                                               "Banff National Park, Alberta")
deer.slopes$location.of.population <- recode(deer.slopes$location.of.population,
                                             "Bow Valley - central zone, Banff National Park, Alberta" = 
                                               "Banff National Park, Alberta")
deer.slopes$location.of.population <- recode(deer.slopes$location.of.population,
                                             "Study area within Bow Valley, Banff National Park, Alberta" = 
                                               "Banff National Park, Alberta")
deer.slopes$location.of.population <- recode(deer.slopes$location.of.population,
                                             "Bow Valley watershed of Banff National Park, Alberta" = 
                                               "Banff National Park, Alberta")               

# Load packages for adding images
packs <- c("png","grid")
lapply(packs, require, character.only = TRUE)

# Load red deer icon
icon <- readPNG("reddeer.png")
icon <- rasterGrob(icon, interpolate = TRUE)

# Update map
# Note - this takes a while depending on your computer
(deer.map.final <- ggplot(deer.locations, aes(x = decimalLongitude, y = decimalLatitude)) +
    # For more localized maps use "worldHires" instead of "world"
    borders("world", colour = "gray80", fill = "gray80", size = 0.3) +
    theme_map() +
    geom_point(alpha = 0.3, size = 2, colour = "aquamarine3") +
    # We are specifying the data frame for the labels - one site has three monitored populations
    # but we only want to label it once so we are subsetting using data = deer.slopes[c(2, 4, 5, 9),]
    # to get only the first rows number 2, 4, 5 and 9
    geom_label_repel(data = deer.slopes[c(2, 4, 5, 9),], aes(x = decimal.longitude, y = decimal.latitude,
                                                             label = location.of.population),
                     box.padding = 1, size = 5, nudge_x = 1,
                     # We are specifying the size of the labels and nudging the points so that they
                     # don't hide data points, along the x axis we are nudging by one
                     min.segment.length = 0, inherit.aes = FALSE) +
    # We can recreate the shape of a dropped pin by overlaying a circle and a triangle
    geom_point(data = deer.slopes, aes(x = decimal.longitude, y = decimal.latitude + 0.6),
               size = 4, colour = "darkgreen") +
    geom_point(data = deer.slopes, aes(x = decimal.longitude, y = decimal.latitude - 0.3),
               size = 3, fill = "darkgreen", colour = "darkgreen", shape = 25) +
    # Adding the icon using the coordinates on the x and y axis
    annotation_custom(icon, xmin = -210, xmax = -100, ymin = -60 , ymax = -30) +
    # Adding a title
    labs(title = "a. Red Deer GBIF occurrences", size = 12))

# Visualise the number of occurrence records through time ----
# This plot is more impressive if you have downloaded more records
# as GBIF downloads the most recent records first
yearly.obs <- deer.locations %>% group_by(year) %>% tally() %>% ungroup() %>% filter(is.na(year) == FALSE)

(occurrences <- ggplot(yearly.obs, aes(x = year, y = n)) +
    geom_smooth(colour = "aquamarine3", method = 'loess', size = 1) +
    labs(x = NULL, y = "Number of occurrences\n",
         title = "b. GBIF occurrences\n", size = 12) +
    # Use our customised theme, saves many lines of code!
    theme_LPD() +
    # if you want to change things about your theme, you need to include the changes after adding the theme
    theme(plot.title = element_text(size = 12), axis.title.y = element_text(size = 10)))

##Visualise population trends ----
  # Visualising the population trends of four deer populations
  
  # Let's practice functional programming here
  # *** Functional Programming ***
  # Let's make a function to make the population trend plots
  # First we need to decide what values the function needs to take
  # x - The population data
  # y - the slope value
  # z - the location of the monitoring
  # This function needs to take three arguments
  
# Let's make the ggplot function
pop.graph <- function(x, y, z) {
  # Make a ggplot graph with the 'x'
  ggplot(x, aes(x = year, y = pop)) +
    # Shape 21 chooses a point with a black outline filled with aquamarine
    geom_point(shape = 21, fill = "aquamarine3", size = 2) +
    # Adds a linear model fit, alpha controls the transparency of the confidence intervals
    geom_smooth(method = "lm", colour = "aquamarine3", fill = "aquamarine3", alpha = 0.4) +
    # Add the monitoring location 'y' into the plot
    labs(x = "", y = "Individuals\n", title = paste("c. ", y, "\n"), size = 7) +
    # Set the y limit to the maximum population for each 'x'
    ylim(0, max(x$pop)) +
    # Set the x limit to the range of years of data
    xlim(1970, 2010) +
    # Add the slope 'y' into the plot
    annotate("text", x = 1972, y = 0,  hjust = 0, vjust = -2, label = paste("Slope =", z), size = 3) +
    theme_LPD() +
    theme(plot.title = element_text(size=12), axis.title.y = element_text(size=10))
}
# Find all unique ids for red deer populations
unique(deer.slopes$id)

# Create an object each of the unique populations
# Deer population 1 - Northern Yellowstone National Park
deer1 <- filter(deer.data, id == "6395")
slope_deer1 <- round(deer.slopes$estimate[deer.slopes$id == "6395"],2)
location_deer1 <- deer.slopes$location.of.population[deer.slopes$id == "6395"]
yellowstone <- pop.graph(deer1, location_deer1, slope_deer1)

# Deer population 2 - Mount Rainier National Park, USA
deer2 <- filter(deer.data, id == "3425")
slope_deer2 <- round(deer.slopes$estimate[deer.slopes$id == "3425"],2)
location_deer2 <- deer.slopes$location.of.population[deer.slopes$id == "3425"]
rainier <- pop.graph(deer2, location_deer2, slope_deer2)

# Deer population 3 - Switzerland
deer3 <- filter(deer.data, id == "11170")
slope_deer3 <- round(deer.slopes$estimate[deer.slopes$id == "11170"],2)
location_deer3 <- deer.slopes$location.of.population[deer.slopes$id == "11170"]
switzerland <- pop.graph(deer3, location_deer3, slope_deer3)

# Deer population 4 - Banff National Park, Alberta (there are more populations here)
deer4 <- filter(deer.data, id == "4383")
slope_deer4 <- round(deer.slopes$estimate[deer.slopes$id == "4383"],2)
location_deer4 <- deer.slopes$location.of.population[deer.slopes$id == "4383"]
banff <- pop.graph(deer4, location_deer4, slope_deer4)

# Create panel of all graphs
# Makes a panel of the map and occurrence plot and specifies the ratio
# i.e., we want the map to be wider than the other plots
# suppressWarnings() suppresses warnings in the ggplot call here
row1 <- suppressWarnings(grid.arrange(deer.map.final, occurrences, ncol = 2, widths = c(1.96, 1.04)))
# Makes a panel of the four population plots
row2 <- grid.arrange(yellowstone, rainier, switzerland, banff, ncol = 4, widths = c(1, 1, 1, 1))

# Makes a panel of all the population plots and sets the ratio
# Stich all of your plots together
deer.panel <- grid.arrange(row1, row2, nrow = 2, heights = c(1.2, 0.8))

ggsave(deer.panel, filename = "deer_panel2.png", height = 10, width = 15)