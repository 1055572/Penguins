#Function - saving AG_MxCL plot as a svg file
save_AG_MxCL_plot_svg <- function(AG_MxCL, filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width = size_inches, height = size_inches, scaling = scaling)
  AG_MxCL_plot <- plot_AG_MxCL_fig(AG_MxCL)
  print(AG_MxCL_plot)
  dev.off()
}