rm(list=ls())
library(igraph)
library(dplyr)
library(png)
library(igraph)
devtools::load_all()

r4 <- get_recording("NIRP1_VS_Hams", fps = 25)

fv_list <- get_filtered_views(r4, data_points = "Nose", n = 41, p = 3)

plot(fv_list$Central_Harmonium)
plot(fv_list$Central_Singer)
plot(fv_list$Central_Tabla)
plot(fv_list$Central_TanpuraL)
plot(fv_list$Central_TanpuraR)

# splice time based on duration object
d <- get_duration_annotation_data(r4)
splicing_df <- splice_time(d, expr = "Tier == 'Event'")
splicing_df

# join the views
jv <- get_joined_view(fv_list)

# get a spliced view object
sv <- get_spliced_view(jv, splicing_df = splicing_df)
autoplot(sv)

# apply grangertest on each Tier
g <- granger_test(sv, "Nose_x_Central_Harmonium", "Nose_x_Central_Tabla")
g
autoplot(g, splicing_df)
autoplot(g, splicing_df) +
  autolayer(d, 'Tier == "Influence H>T" | Tier == "Influence T>H"', fill_column = "Tier")

# modify duration object to annotate facets separately - add as function?
d <- d %>%
  mutate(Test = case_when(
    Tier == "Influence H>T" ~ "Nose_x_Central_Tabla <- \nNose_x_Central_Harmonium",
    Tier == "Influence T>H" ~ "Nose_x_Central_Harmonium <- \nNose_x_Central_Tabla"))

autoplot(g, splicing_df) +
  autolayer(d, 'Tier %in% c("Influence H>T", "Influence T>H")', fill_column = "Tier")

# Cannot seem to just use Test column - possibly layering conflicts with data in autoplot

# Calculate granger tests for three players
g1 <- granger_test(sv, "Nose_x_Central_Harmonium", "Nose_x_Central_Tabla")
g2 <- granger_test(sv, "Nose_x_Central_Harmonium", "Nose_x_Central_Singer")
g3 <- granger_test(sv, "Nose_x_Central_Tabla", "Nose_x_Central_Singer")

# Single data frame of P-values
sig_level <- 0.05
df <- bind_rows(list(g1$df, g2$df, g3$df), .id = "Player")
df$mlog10pv <- ifelse(df$P_Value < sig_level, -log10(df$P_Value), NA_real_)


# Select a Tier for network
tabla_fill_df <- df %>% filter(Tier == "tabla fill")

nodes <- data.frame(id = sapply(strsplit(unique(df$Var1), "_"), function(x) x[4]))

tabla_fill_df$Var1 <- sapply(strsplit(tabla_fill_df$Var1, "_"), function(x) x[4])
tabla_fill_df$Var2 <- sapply(strsplit(tabla_fill_df$Var2, "_"), function(x) x[4])
links <- data.frame(from = tabla_fill_df$Var2, to = tabla_fill_df$Var1, x = tabla_fill_df$mlog10pv)

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)
net

l <- do.call(layout, list(net))
#V(net)$label <- nodes[,1]
V(net)$size <- 80
#E(net)$width <- E(net)$x
# E(net)$arrow.size <- 1:6
#E(net)$arrow.size <- ifelse(!is.na(E(net)$x), .1*E(net)$x, 0)
#E(net)$length <- E(net)$length*10
E(net)$color <- ifelse(!is.na(E(net)$x), "grey", "white")
E(net)$label <- round(E(net)$x, 1)
E(net)$label.color <- ifelse(!is.na(E(net)$x), "red", "white")
plot(net, layout=l, edge.curved=.4)

# Arrows are 'is influencing'
