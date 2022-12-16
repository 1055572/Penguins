#Function - plotting
plot_AG_MxCL_fig <- function(AG_MxCL){
  AG_MxCL %>% 
    ggplot( aes(x = body_mass_g, y = culmen_length_mm)) +
    ggtitle("The relationship between body mass and culmen length of penguins on Biscoe Island") +
    geom_smooth(method = "lm" ) +
    geom_point(aes(color = species) , alpha = 0.4, show.legend = TRUE) +
    scale_color_manual(values = c("blue","purple")) +
    labs(x = "Body mass (g)", 
         y = "Culmen length (mm)") +
    theme_bw()
}