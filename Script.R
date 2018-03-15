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