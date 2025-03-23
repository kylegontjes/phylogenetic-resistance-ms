# Consistent themes

## Packages
library(gridExtra)
library(kableExtra)
library(knitr)

# Manuscript Background theme
theme_bw_me <- theme(panel.background = element_rect(fill = "white",colour = NA), panel.grid = element_blank(),
                     strip.background = element_rect(fill = "white",colour = "black"),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.line = element_line(colour = "black"),legend.position = "bottom")
# Resistance categories
resistance_cat_colors <- c("Susceptible" = "#005AB5","Intermediate"="#FFC20A","Resistant" = "#DC3220")
resistance_cat_scale <- scale_fill_manual(values=resistance_cat_colors, name="Resistance Profile", guide = guide_legend(nrow=1, title.position = "top", label.position = "right"))

# Resistance categories
feature_colors <- c(`1` = "black",`0`="white")
feature_scale <- scale_fill_manual(values=feature_colors,labels=c("Present","Absent"),name="Tip State", guide = guide_legend(nrow=1, title.position = "top", label.position = "right"))


# Sfigure2 format
format <-   theme(legend.position = "bottom",
                  axis.text =   element_text(size=18,color="black"),
                  axis.title = element_text(size = 22,color="black"),
                  legend.text =   element_text(size=20,color="black"),
                  legend.title = element_text(size = 22,color="black"),
                  plot.title = element_text(size = 24,color="black")
)

# Figure 1 tree theme
consistent_theme <- theme(legend.position = 'bottom',legend.direction="horizontal", legend.justification = "center", legend.key = element_rect(colour = c('black')),legend.box.spacing = unit(.0001, "cm"),legend.key.size = unit(.75, "cm"),legend.key.width = unit(.75, "cm"),legend.spacing.x=unit(.75, "cm"), legend.title = element_text(size=18,hjust=0.5),legend.text = element_text(size=16,hjust=0))

# Figure 1 table
mytheme <- ttheme_minimal(core = list(fg_params = list(hjust=0, x=0.01,
                                                       fontsize=18),
                                      padding=unit(c(5,2.5), "mm")),
                          colhead = list(fg_params = list(hjust=0, x=0.01,fontsize=18,
                                                          fontface="bold")),
                          rowhead=list(fg_params=list(hjust=0, x=0)))

# Favorite kable
favorite_kable <- function (x){
  x %>% kable(., format = "html", table.attr = "style='width:100%;'",
              row.names = F) %>% kable_styling(bootstrap_options = c("striped",
                                                                    "hover", "condensed", "responsive"))
}
