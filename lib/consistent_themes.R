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
resistance_cat_scale <- scale_fill_manual(values=resistance_cat_colors, name="Resistance profile", guide = guide_legend(nrow=1, title.position = "top", label.position = "right"))

# Resistance categories
feature_colors <- c(`1` = "black",`0`="white")
feature_scale <- scale_fill_manual(values=feature_colors,labels=c("Present","Absent"),name="Tip state", guide = guide_legend(nrow=1, title.position = "top", label.position = "right"))

# Figure 1 theme
figure_1_theme <- theme(axis.ticks = element_line(color = "black"), axis.text = element_text(color = "black", size = 14), axis.title = element_text(size = 16))

# Supplemental Figure 1 format
format <- theme(legend.position = "bottom",
                  axis.text =   element_text(size=20, color="black"),
                  axis.title = element_text(size = 22, color="black"),
                  legend.text =   element_text(size=22, color="black"),
                  legend.title = element_text(size = 24, color="black"),
                  plot.title = element_text(size = 0, color="black")
)

# Figure 2 tree theme
consistent_theme <- theme(legend.position = 'bottom',legend.direction="horizontal", legend.justification = "center", legend.key = element_rect(colour = c('black')),legend.box.spacing = unit(.0001, "cm"),legend.key.size = unit(.75, "cm"),legend.key.width = unit(.75, "cm"),legend.spacing.x=unit(.75, "cm"), legend.title = element_text(size=22,hjust=0.5),legend.text = element_text(size=20,hjust=0))

# Figure 2 table
mytheme <- ttheme_minimal(core = list(fg_params = list(hjust=0, vjust = 0.5 ,x=0.1, fontsize=16),padding = unit(c(4, 2), "mm")),
                          colhead = list(fg_params = list(hjust=0, vjust = 0.5, x=0.1, fontsize=16, fontface="bold"),padding = unit(c(4, 4), "mm")),
                          rowhead=list(fg_params=list(hjust=0, x=0)))

# Favorite kable
favorite_kable <- function (x){
  x %>% kable(., format = "html", table.attr = "style='width:100%;'", row.names = F) %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
}
