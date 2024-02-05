
# Define default  plot functions
my_save_plot <- function(filename, plot) {# make my own save_plot
    save_plot(filename, plot, path = "../../figures", unit = "mm")
}

wet_dry_color <- function() {
  c(
    "No Watered" = "#FF0000",
    "Not Watered" = "#FF0000",
    "Watered" = "#02A5E0"
  )
}

trait_color <- function() {
  c(
    "LDMC" = "#A5770B",
    "SLA" = "#3AB795"
  )
}

# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
species_color <- function() {
  c(
    "A. cytisoides" = "#0072B2",#sky blue #bluish green"#009E73"
    "P. lentiscus" = "#D55E00",#orange
    "D. pentaphyllum" = "#F0E442" #yellow
  )
}

ggsave_multiple <- function(fns, ...) {
  map(fns, function(x) ggsave(x, ...))
}
