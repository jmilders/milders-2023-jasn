#### Install and load required packages ####
#install.packages("pacman")
library(pacman)
pacman::p_load("viridis",        # Viridis colours
               "readxl",         # Open Excel files 
               "readr",          # Open CSV files    
               "dplyr",          # Data manipulation
               "ggplot2",        # Data visualization
               "eulerr",         # Euler diagram
               "scales",         # Control appearance of axis and legend labels
               "maps",           # Load maps for world map
               "magrittr",       # Efficient piping
               "ggplotify",      # Convert plot to ggplot
               "tableone",       # Table one
               "stringr")     
#### Load data #####
dat <- read_xlsx('H:/Projects PhD/2. Scoping review of prediction models in nephrology/Data/DEF_final.xlsx')

#### FIGURES AND TABLES ####
#### Figure 2 Number of studies published per year ####

#### Figure 3 World map #### 
# Get world data
world = map_data("world")

# Get continents and world data
continents <- read_csv('H:/Projects PhD/2. Scoping review of prediction models in nephrology/Data/contintents.csv', show_col_types = FALSE) %>%
  # Drop irrelevant columns
  dplyr::select(-Code, - Year) %>%
  # Change names
  set_colnames(c("region", "continent")) %>%
  # Change region names
  mutate(# Change region names
    region2 = case_when(region == "United Kingdom" ~ "UK",
                        region == "United States" ~ "USA",
                        region == "Czechia" ~ "Czech Republic", 
                        region == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
                        region == "Cote d'Ivoire" ~ "Ivory Coast",
                        region == "Saint Martin (French part)" ~ "Saint Martin",
                        region == "Sint Maarten (Dutch part)" ~ "Sint Maarten",
                        region == "Pitcairn" ~ "Pitcairn Islands",
                        region == "French Southern Territories" ~ "French Southern and Antarctic Lands",
                        region == "Micronesia (country)" ~ "Micronesia",
                        region == "Congo" ~ "Republic of Congo",
                        region == "Timor" ~ "Timor-Leste",
                        region == "Eswatini" ~ "Swaziland",
                        region == "Faeroe Islands" ~ "Faroe Islands"),
    # Replace region variable
    region = ifelse(is.na(region2), region, region2)) %>%
  # Drop region2 variable
  dplyr::select(-region2)

# Add missing regions for continents
continents.extra <- data.frame(region = c("Bonaire", "Sint Eustatius", "Saba", "Antigua", "Barbuda", "Saint Kitts", "Nevis",
                                          "Saint Barthelemy", "Saint Vincent", "Grenadines", "Trinidad", "Tobago",
                                          "Virgin Islands", "Heard Island", "South Georgia", "South Sandwich Islands"),
                               continent = c(rep("North America", 13), "Antarctica", rep("South America", 2)))

# Combine continent data
continents <- rbind(continents, continents.extra)

# Join contintent data to world data
world <- left_join(world, continents, "region") %>%
  # Drop regions without continent (Ascension Island, Azores, Canary Islands, Chagos Archipelago, Faroe Islands, 
  # Madeira Islands, Siachen Glacier)
  filter(!is.na(continent))

# Create data frame with results per contintent
dat.results = data.frame(continent = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"),
                         models = c(3, 181, 221, 210, 15, 17))

# Join data and world map 
dat.plot = inner_join(world, dat.results, by = "continent")

# Create plot
ggplot(data = dat.plot, aes(x = long, y = lat, group = group, fill = models)) +
  # Geometries
  geom_polygon() +
  # Transformations
  coord_fixed(1.3) +
  scale_fill_gradient(low = viridis_pal(alpha = 0.1)(1), high = viridis_pal()(1)) +
  # Aesthetics
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, face = "bold", vjust = 0.5, size = 10)) + 
  ggtitle("Continents from which patient populations were derived") + labs(fill="Number of studies")

#### Figure 4 Euler diagram #### 

#### Figure 5 Outcomes ####

#### Figure 6 Sample size ####
# Dirty function for data breaks
tens <- function(x){return((1:10) * x)}

# Get breaks
breaks <- unique(c(tens(10), tens(100), tens(1000), tens(10000), tens(100000), tens(1e6)))

# Prepare data for plotting
dat_plot <- dat %>%
  # Create new variables
  mutate(# Create bins
         sampbins = cut(samp, breaks = breaks, include.lowest = TRUE),
         # Create labels
         lab_min = gsub(" ", "", format(as.numeric(gsub("\\[|\\(", "", gsub(",\\d+.*\\]", "", sampbins))), scientific = FALSE)),
         lab_max = gsub(" ", "", format(as.numeric(gsub("[\\[|\\(]\\d+.*,", "", gsub("\\]", "", sampbins))), scientific = FALSE)),
         labs = paste0(lab_min, "-", lab_max)) %>%
  # Arrange for extraction
  arrange(as.numeric(lab_min))

# Extract labels for x-axis
labs <- unique(dat_plot[["labs"]])

# Make bar chart
ggplot(data = dat_plot, aes(x = sampbins)) +
  # Geometries
  geom_bar(fill="#440154FF") +
  # Scaling
  scale_x_discrete(labels = labs) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  # Labels
  ggtitle("Sample size of the included studies") +
  xlab("Sample size") + 
  ylab("Number of studies") +
  # Aesthetics
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 22),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank())


ggsave(filename = "figure6.png", dpi = 600, width = 15, height = 14)

#### Figure 7 External validations ####
pop <- c(rep("All" , 3) , rep("CKD" , 3) , rep("Dialysis" , 3) , rep("Kidney transplantation" , 3))
val <- rep(c("One time" , "2-5 times" , ">5 times") , 4)
value <- rep(c(70,37,4,22,11,2,15,12,1,33,13,2))
f7 <- data.frame(pop,val,value)
                                                
f7$val <- factor(f7$val, 
                 levels = c(">5 times", "2-5 times", "One time"))

show_col(viridis_pal()(3)) # Show main colours of viridis 

ggplot(f7, aes(fill=val, y=value, x=pop)) + 
  geom_bar(position="stack", stat="identity", width = 0.6) +
  geom_text(label = value, colour = "white",   position = position_stack(vjust = 0.5), size = 5) + 
  ggtitle("Number of times models were externally validated") +
  xlab("Population") + ylab("Number of models") + labs(fill = "Number of times validated") +
  scale_fill_manual(values = c(alpha("#440154FF", 1), alpha("#440154FF", 0.5), alpha("#440154FF", 0.3)),
                    limits = c("One time" , "2-5 times" , ">5 times")) +
  scale_y_continuous(breaks = seq(0,130,10)) +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.title = element_text(size=16), 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank())

ggsave(filename = "figure7.png", dpi = 600, width = 15, height = 14)

#### Validation and updating #### 
# Load data on validations and updates
median <- read_xlsx('H:/Projects PhD/2. Scoping review of prediction models in nephrology/Data/median.xlsx')

# Median validations of all models
summary(median$val_tot)

#### SUPPLEMENT ####
#### Figure S1 TRIPOD reference ####
#### Figure S2 discrimination and calibration ####

#### Figure S4 Internal and external validation within the development study ####
table(dat$pub_year, dat$intval)
table(dat$pub_year, dat$extval)

year <- c(rep("2007" , 2) , rep("2008" , 2) , rep("2009" , 2) , rep("2010" , 2),rep("2011" , 2), 
          rep("2012" , 2) , rep("2013" , 2) , rep("2014" , 2), rep("2015" , 2) , rep("2016" , 2), 
          rep("2017" , 2) , rep("2018" , 2), rep("2019" , 2) , rep("2020" , 2) , rep("2021" , 2))
validation <- rep(c("Internal validation" , "External validation"), 15)
value <- rep(c(18.2, 0, 70, 0, 80, 0 ,77.8, 22.0 ,62.5, 25, 58.3, 8.3, 68.8, 18.8, 47.4, 10.5, 80,
               12, 66.7, 16.7, 65.6, 12.5, 67.6, 13.5, 53.5, 27.9, 85.4, 17.1, 66.2, 13.8))
sf4 <- data.frame(year,validation,value)

sf4$validation <- factor(sf4$validation, levels = c('Internal validation', 'External validation'))

# Make grouped bar chart 
ggplot(sf4, aes(fill=validation, y=value, x=year)) + 
  geom_bar(position="dodge", stat="identity", width = 0.8) + 
  ggtitle("Percentage of development studies that performed internal and external validation within the same study") +
  xlab("Year of publication") + ylab("Percentage of studies") + labs(fill = "") +
  scale_fill_manual(values = c("#440154FF","#21908CFF"), 
                    limits = c("Internal validation" , "External validation")) + 
  scale_y_continuous(breaks = seq(0,100,10)) +
  theme_bw() + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank())

ggsave(filename = "figuresf_2.png", dpi = 600, width = 10, height = 10)
