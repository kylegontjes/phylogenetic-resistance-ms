# Consistent themes

## Packages
library(gridExtra)
library(kableExtra)
library(knitr)
library(hues)

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

# Clade scale
clade_scale <- scale_fill_manual(breaks =c("Clade I","Clade II"),values=c("red","blue" ),name="Clade", guide = guide_legend(ncol=1, title.position = "top", label.position = "right",order=1))

# Resistant scasle
resistance_scale <- scale_fill_manual(breaks = c(0,1),values=c("white","black"),labels = c("Susceptible","Resistant"),name="Tip state",guide = guide_legend(title.position = "top", label.position = "right",nrow=2,order=2),drop = FALSE )

# Cluster theme
## Get pallete
### Load data
clustering_df <- readRDS("./data/asr_clustering/asr_clustering_df.RDS")

### Get max size
phenotypes <- c("TMP_SMX","gentamicin","AMK","CST","blbli") %>% paste0(.,"_asr_cluster_renamed_resistant")
sizes <- c()
for(i in phenotypes){
  sizes[i] <- table(clustering_df[,i])  %>% subset(names(.) != "Resistant Singleton" & grepl("Cluster",names(.))) %>% as.numeric %>% length
}

ncluster <- max(sizes)
color_palette <- grDevices::colors() %>% subset(grepl("red",.)==F)
clusters_col <- iwanthue(ncluster,hmin=15,hmax=360,lmin = 5,lmax = 95,cmin=5,cmax=90,random = F,plot = F)

clusters_name <- paste0("Resistant Cluster ",1:ncluster)
names(clusters_col) <- clusters_name

## White
cluster_scale <- scale_fill_manual(breaks =c("Susceptible","Resistant Singleton",names(clusters_col)),values=c("Susceptible" = "white","Resistant Singleton" = "red",clusters_col),labels=c("Susceptible","Singleton",gsub("Resistant ","",clusters_name)),name="Phylogenetics of resistance", guide = guide_legend(ncol=4, order=3,title.position = "top", label.position = "right"))
cluster_scale_3_col <- scale_fill_manual(breaks =c("Susceptible","Resistant Singleton",names(clusters_col)),values=c("Susceptible" = "white","Resistant Singleton" = "red",clusters_col),labels=c("Susceptible","Singleton",gsub("Resistant ","",clusters_name)),name="Phylogenetics of resistance", guide = guide_legend(ncol=3, order=3,title.position = "top", label.position = "right"))

## Gray
cluster_scale_gray <- scale_fill_manual(breaks =c("Susceptible","Resistant Singleton",names(clusters_col)),values=c("Susceptible" = "gray","Resistant Singleton" = "red",clusters_col),labels=c("Susceptible","Singleton",gsub("Resistant ","",clusters_name)),name="Phylogenetics of resistance", guide = guide_legend(nrow=5, title.position = "top", label.position = "right"))

# Supplemental Figure 1 theme
sfigure_1_theme <- theme(axis.ticks = element_line(color = "black"), axis.text = element_text(color = "black", size = 14), axis.title = element_text(size = 16))

# Supplemental Figure 2 format
sfigure_2_theme <- theme(legend.position = "bottom",
                  axis.text =   element_text(size=14, color="black"),
                  axis.title = element_text(size = 16, color="black"),
                  legend.text =   element_text(size=16, color="black"),
                  legend.title = element_text(size = 18, color="black"),
                  plot.title = element_text(size = 0, color="black")
)

# Supplemental Figure 3 format
sfigure_3_theme <- theme(axis.ticks = element_line(color = "black"),
                      axis.text = element_text(color = "black", size = 10),
                      axis.title = element_text(size = 12),
                      legend.title = element_text(color = "black", size = 12),
                      legend.text =  element_text(color = "black", size = 10))

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
