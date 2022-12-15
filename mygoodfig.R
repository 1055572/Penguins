#mygoodfig

# set working directory
# load packages
library(palmerpenguins)
library(ggplot2)
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(svglite)

#----------------FUNC----------------------
#load functions - cleaning & filtering 

cleaning <- function(data_raw){
  data_raw %>%
    clean_names() %>%
    remove_empty(c("rows", "cols")) %>%
    select(-starts_with("delta")) %>%
    select(-comments)
}

A_mass_clength <- function(data_clean){
  data_clean %>%
    filter(!is.na(body_mass_g), species == "Adelie Penguin (Pygoscelis adeliae)", sex =="MALE") %>%
    select(species, body_mass_g, culmen_length_mm, island)
}

#------------------RUN CODE -------------------
# run code through functions --> clean & filtered objects
# Loading the raw penguin data
write.csv(penguins_raw, "data_raw/penguins_raw.csv")
penguins_raw <- read.csv("data_raw/penguins_raw.csv")

# clean it
penguins_clean <- cleaning(penguins_raw) 
write.csv(penguins_clean, "data_clean/penguins_clean.csv")

# filter it - subset
massXclength <- A_mass_clength(penguins_clean)

#------------PLOT (& PLOT FUNC)--------------------
#plot Adelie male body mass & culmen length, colour-coded depending on island
plot_A_mass_clength <- function(massXclength){
  massXclength %>%
    ggplot( aes(x = body_mass_g, y = culmen_length_mm)) +
    geom_point(aes(color = island) , show.legend = TRUE) +
    scale_color_manual(values = c("pink","purple","cyan4")) +
    ggtitle("The relationship between Adelie penguin's body mass and culmen length") +
    labs(x = "Body mass (g)", 
         y = "Culmen length (mm)") +
    theme_bw()
}

massXclength_plot <- plot_A_mass_clength(massXclength)
massXclength_plot

model1 <- lm(culmen_length_mm ~ body_mass_g, massXclength)
summary(model1)

#--------------SAVE AS SVG----------------------
# save as a svg.

# Func = Save the plot as a svg and define the size and scaling
save_massXclength_plot_svg <- function(massXclength, filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width = size_inches, height = size_inches, scaling = scaling)
  massXclength_plot <- plot_A_mass_clength(massXclength)
  print(massXclength_plot)
  dev.off()
}



# Save the plot as a vector (no resolution needed)
save_massXclength_plot_svg(massXclength, "figures/fig1_vector.svg", 
                      size = 15, scaling = 1)
