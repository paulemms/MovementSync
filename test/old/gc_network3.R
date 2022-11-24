rm(list=ls())
library(igraph)
library(dplyr)
library(png)
devtools::load_all()

# order spliced view, restricted series plotted

r3 <- get_recording("NIRP1_MAK_Jaun", fps = 25)

fv_list <- get_filtered_views(r3, data_points = "Nose", n = 41, p = 3)

autoplot(fv_list$Cam1_Harmonium)
autoplot(fv_list$Cam2_Singer)
autoplot(fv_list$Cam2_Tabla)

# join the views
jv <- get_joined_view(fv_list)
autoplot(jv)

# splice time based on duration object
d <- get_duration_annotation_data(r3)
splicing_df <- splice_time(d, expr = "Tier == 'Form'")
splicing_df

# get a spliced view object
sv <- get_spliced_view(jv, splicing_df = splicing_df)
autoplot(sv)

# Plot granger causality tests on each splice of data
pg <- plot_network_diagram(sv, c("Nose_x_Cam1_Harmonium", "Nose_x_Cam2_Singer",
                           "Nose_x_Cam2_Tabla"))

# P-Value table
pg

# Calculate granger tests for three players
g1 <- granger_test(sv, "Nose_x_Cam1_Harmonium", "Nose_x_Cam2_Singer")
g2 <- granger_test(sv, "Nose_x_Cam2_Singer", "Nose_x_Cam2_Tabla")
g3 <- granger_test(sv, "Nose_x_Cam2_Tabla", "Nose_x_Cam1_Harmonium")

# Single data frame of P-values
sig_level <- 0.05
df <- bind_rows(list(g1$df, g2$df, g3$df), .id = "Player")
df$mlog10pv <- ifelse(df$P_Value < sig_level, -log10(df$P_Value), NA_real_)

par(mfrow=c(1,3), mar=c(1,1,1,1))

# Select a Tier for network diagram

for (tier in unique(df$Tier)) {

  splice_df <- df %>% filter(Tier == !!tier)
  print(splice_df)

  nodes <- data.frame(id = sapply(strsplit(unique(df$Var1), "_"), function(x) x[4]))

  splice_df$Var1 <- sapply(strsplit(splice_df$Var1, "_"), function(x) x[4])
  splice_df$Var2 <- sapply(strsplit(splice_df$Var2, "_"), function(x) x[4])
  links <- data.frame(from = splice_df$Var2, to = splice_df$Var1, x = splice_df$mlog10pv)

  net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)
  net

  l <- layout_in_circle(net)
  V(net)$size <- 80
  E(net)$color <- ifelse(!is.na(E(net)$x), "grey", "white")
  E(net)$label <- round(E(net)$x, 1)
  E(net)$label.color <- ifelse(!is.na(E(net)$x), "red", "white")
  plot(net, layout=l, edge.curved=.4, main=tier)
}
# Arrows are 'is influencing'
#dev.off()
