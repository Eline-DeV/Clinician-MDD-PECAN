###################################################
###### INDIVIDUAL PECAN ANALYSES
###################################################
library(qgraph)
library(dplyr)
library(purrr)
library(psych)
library(igraph)
###################################################
###### GRAPHING PECAN ANALYSES
###################################################

MDD_symptoms <- paste(c('Psychomotor agitation or retardation', 'Suicidal thoughts or behaviour', 'Insomnia or Hypersomnia', 'Diminished concentration or indecisiveness', 'Depressed mood', 'Fatigue or loss of energy', 'Feelings of worthlessness or excessive/inappropriate guilt', 'Weight or appetite fluctuations', 'Anhedonia'))

# PP1

MDD_symptoms_pp1_full <- c('Psychomotor agitation or retardation', 'Suicidal thoughts or behaviour', 'Insomnia or Hypersomnia', 'Diminished concentration or indecisiveness', 'Depressed mood', 'Fatigue or loss of energy', 'Feelings of worthlessness or excessive/inappropriate guilt', 'Weight or appetite fluctuations', 'Anhedonia', 'Feeling of restriction of freedom')

PECAN_1_full <- qgraph(PP1_links_full, vsize = PP1_nodes_full_sizes, minimum = 0.5, theme = 'colorblind', layout = 'spring', nodeNames = MDD_symptoms_pp1_full, legend.cex = 1.5, normalize = TRUE, curveAll = TRUE,label.cex = 2.5, curveDefault = 1.8)

PECAN_1 <- qgraph(PP_1_links, vsize = PP_1_nodes_sizes, minimum = 0.5, theme = 'colorblind', layout = 'spring', legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)
thelayout  <- PECAN_1$layout # establishing common layout

#PP2
PECAN_2 <- qgraph(PP_2_links, vsize = PP_2_nodes_sizes, minimum = 0.5, theme = 'colorblind', layout = thelayout, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)

#PP3
PECAN_3 <- qgraph(PP_3_links, vsize = PP_3_nodes_sizes, minimum = 0.5, theme = 'colorblind', layout = thelayout, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)

#PP4
PECAN_4 <- qgraph(PP_4_links, vsize = PP_4_nodes_sizes, minimum = 0.5, theme = 'colorblind', layout = thelayout, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)

#PP5
MDD_symptoms_pp5_full <- c('Psychomotor agitation or retardation', 'Suicidal thoughts or behaviour', 'Insomnia or Hypersomnia', 'Diminished concentration or indecisiveness', 'Depressed mood', 'Fatigue or loss of energy', 'Feelings of worthlessness or excessive/inappropriate guilt', 'Weight or appetite fluctuations', 'Anhedonia', 'Emotion regulation')

PECAN_5_full <- qgraph(PP5_links_full, vsize = PP5_nodes_full_sizes, minimum = 0.5, theme = 'colorblind', layout = 'spring', nodeNames = MDD_symptoms_pp5_full, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)

PECAN_5 <- qgraph(PP_5_links, vsize = PP_5_nodes_sizes, minimum = 0.5, theme = 'colorblind', layout = thelayout, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)

#PP6
PECAN_6 <- qgraph(PP_6_links, vsize = PP_6_nodes_sizes, minimum = 0.5, theme = 'colorblind', layout = thelayout, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)

#PP7
MDD_symptoms_pp7_full <- c('Psychomotor agitation or retardation', 'Suicidal thoughts or behaviour', 'Insomnia or Hypersomnia', 'Diminished concentration or indecisiveness', 'Depressed mood', 'Fatigue or loss of energy', 'Feelings of worthlessness or excessive/inappropriate guilt', 'Weight or appetite fluctuations', 'Anhedonia', 'Internalising coping')

PECAN_7_full <- qgraph(PP7_links_full, vsize = PP7_nodes_full_sizes, minimum = 0.5, theme = 'colorblind', layout = 'spring', nodeNames = MDD_symptoms_pp7_full, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)

PECAN_7 <- qgraph(PP_7_links, vsize = PP_7_nodes_sizes, minimum = 0.5, theme = 'colorblind', layout = thelayout, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)

#PP8
PECAN_8 <- qgraph(PP_8_links, vsize = PP_8_nodes_sizes, minimum = 0.5, theme = 'colorblind', layout = thelayout, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)

#PP9
PECAN_9 <- qgraph(PP_9_links, vsize = PP_9_nodes_sizes, minimum = 0.5, theme = 'colorblind', layout = thelayout, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)

#PP10
PECAN_10 <- qgraph(PP_10_links, vsize = PP_10_nodes_sizes, minimum = 0.5, theme = 'colorblind', layout = thelayout, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)

#PP11
PECAN_11 <- qgraph(PP_11_links, vsize = PP_11_nodes_sizes, minimum = 0.5, theme = 'colorblind', layout = thelayout, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)

#PP12
PECAN_12 <- qgraph(PP_12_links, vsize = PP_12_nodes_sizes, minimum = 0.5, theme = 'colorblind', layout = thelayout, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)

#PP13
PECAN_13 <- qgraph(PP_13_links, vsize = PP_13_nodes_sizes, minimum = 0.5, theme = 'colorblind', layout = thelayout, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)

###################################################
###### DESCRIPTIVES AND VISUALISING (OF STRONG EDGES)
###################################################

# Participant 1
psych::describe(PP_1_links$size, interp = TRUE, quant = TRUE, IQR = TRUE)
summary(PECAN_1) #number of edges and unique weights
# How many edges have a causal strength of >= 0.5?
strong_1_links <- PP_1_links[PP_1_links$size >= 0.5,]
strong_PECAN_1 <- qgraph(strong_1_links, vsize = PP_1_nodes_sizes, theme = 'colorblind', layout = thelayout, minimum = 0.5, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE) # This graph contains only edge weights >= 0.5, however it looks similar to the graph on the original data as in both cases the edge weigh widths are displayed relatively and in both graphs weights < 0.5 are not displayed (but only in the second are they removed, impacting the amount of included edges in the model). These PECAN are not further visualised for reporting but with the above code this can easily be done.
summary(strong_PECAN_1)

# Participant 2
psych::describe(PP_2_links$size, interp = TRUE, quant = TRUE, IQR = TRUE)
summary(PECAN_2) #number of edges and unique weights
# How many edges have a causal strength of >= 0.5?
strong_2_links <- PP_2_links[PP_2_links$size >= 0.5,]
strong_PECAN_2 <- qgraph(strong_2_links, vsize = PP_2_nodes_sizes, theme = 'colorblind', layout = thelayout, minimum = 0.5, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)
summary(strong_PECAN_2)

# Participant 3
psych::describe(PP_3_links$size, interp = TRUE, quant = TRUE, IQR = TRUE)
summary(PECAN_3) #number of edges and unique weights
# How many edges have a causal strength of >= 0.5?
strong_3_links <- PP_3_links[PP_3_links$size >= 0.5,]
strong_PECAN_3 <- qgraph(strong_3_links, vsize = PP_3_nodes_sizes, theme = 'colorblind', layout = thelayout, minimum = 0.5, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)
summary(strong_PECAN_3)

# Participant 4
psych::describe(PP_4_links$size, interp = TRUE, quant = TRUE, IQR = TRUE)
summary(PECAN_4) #number of edges and unique weights
# How many edges have a causal strength of >= 0.5?
strong_4_links <- PP_4_links[PP_4_links$size >= 0.5,]
strong_pecan_names_4 <- c('Psychomotor agitation or retardation', 'Insomnia or Hypersomnia', 'Diminished concentration or indecisiveness', 'Depressed mood', 'Fatigue or loss of energy', 'Feelings of worthlessness or excessive/inappropriate guilt', 'Weight or appetite fluctuations', 'Anhedonia', 'Suicidal thoughts or behaviour')
strong_PECAN_4 <- qgraph(strong_4_links, vsize = PP_4_nodes_sizes, theme = 'colorblind', layout = thelayout, minimum = 0.5, legend.mode = 'names', nodeNames = strong_pecan_names_4, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)
summary(strong_PECAN_4)

# Participant 5
psych::describe(PP_5_links$size, interp = TRUE, quant = TRUE, IQR = TRUE)
summary(PECAN_5) #number of edges and unique weights
# How many edges have a causal strength of >= 0.5?
strong_5_links <- PP_5_links[PP_5_links$size >= 0.5,]
strong_pecan_names_5 <- c('Psychomotor agitation or retardation', 'Suicidal thoughts or behaviour', 'Insomnia or Hypersomnia', 'Diminished concentration or indecisiveness', 'Depressed mood', 'Fatigue or loss of energy', 'Feelings of worthlessness or excessive/inappropriate guilt', 'Anhedonia', 'Weight or appetite fluctuations')
strong_PECAN_5 <- qgraph(strong_5_links, vsize = PP_5_nodes_sizes, theme = 'colorblind', layout = thelayout, minimum = 0.5, legend.mode = 'names', nodeNames = strong_pecan_names_5, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)
summary(strong_PECAN_5)

# Participant 6
psych::describe(PP_6_links$size, interp = TRUE, quant = TRUE, IQR = TRUE)
summary(PECAN_6) #number of edges and unique weights
# How many edges have a causal strength of >= 0.5?
strong_6_links <- PP_6_links[PP_6_links$size >= 0.5,]
# Not all nodes are included but these should still appear in the network, even if unconnected, so using the labels argument to adjust for this
strong_PECAN_6_node_names <- PP_6_nodes$nodeId
strong_PECAN_6 <- qgraph(strong_6_links, vsize = PP_6_nodes_sizes, theme = 'colorblind', layout = thelayout, minimum = 0.5, labels = strong_PECAN_6_node_names, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)
summary(strong_PECAN_6)

# Participant 7
psych::describe(PP_7_links$size, interp = TRUE, quant = TRUE, IQR = TRUE)
summary(PECAN_7) #number of edges and unique weights
# How many edges have a causal strength of >= 0.5?
strong_7_links <- PP_7_links[PP_7_links$size >= 0.5,]
strong_PECAN_7_node_names <- PP_7_nodes$nodeId
strong_PECAN_7 <- qgraph(strong_7_links, vsize = PP_7_nodes_sizes, theme = 'colorblind', layout = thelayout, minimum = 0.5, labels = strong_PECAN_7_node_names, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)
summary(strong_PECAN_7)

# Participant 8
psych::describe(PP_8_links$size, interp = TRUE, quant = TRUE, IQR = TRUE)
summary(PECAN_8) #number of edges and unique weights
# How many edges have a causal strength of >= 0.5?
strong_8_links <- PP_8_links[PP_8_links$size >= 0.5,]
strong_PECAN_8 <- qgraph(strong_8_links, vsize = PP_8_nodes_sizes, theme = 'colorblind', layout = thelayout, minimum = 0.5,  legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)
summary(strong_PECAN_8)

# Participant 9
psych::describe(PP_9_links$size, interp = TRUE, quant = TRUE, IQR = TRUE)
summary(PECAN_9) #number of edges and unique weights
# How many edges have a causal strength of >= 0.5?
strong_9_links <- PP_9_links[PP_9_links$size >= 0.5,]
strong_PECAN_9 <- qgraph(strong_9_links, vsize = PP_9_nodes_sizes, theme = 'colorblind', layout = thelayout, minimum = 0.5, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)
summary(strong_PECAN_9)

# Participant 10
psych::describe(PP_10_links$size, interp = TRUE, quant = TRUE, IQR = TRUE)
summary(PECAN_10) #number of edges and unique weights
# How many edges have a causal strength of >= 0.5?
strong_10_links <- PP_10_links[PP_10_links$size >= 0.5,]
strong_pecan_names_10 <- c('Psychomotor agitation or retardation', 'Suicidal thoughts or behaviour', 'Insomnia or Hypersomnia', 'Depressed mood', 'Fatigue or loss of energy','Feelings of worthlessness or excessive/inappropriate guilt', 'Weight or appetite fluctuations', 'Anhedonia', 'Diminished concentration or indecisiveness')
strong_PECAN_10 <- qgraph(strong_10_links, vsize = PP_10_nodes_sizes, theme = 'colorblind', layout = thelayout, minimum = 0.5, legend.mode = 'names', nodeNames = strong_pecan_names_10, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)
summary(strong_PECAN_10)

# Participant 11
psych::describe(PP_11_links$size, interp = TRUE, quant = TRUE, IQR = TRUE)
summary(PECAN_11) #number of edges and unique weights
# How many edges have a causal strength of >= 0.5?
strong_11_links <- PP_11_links[PP_11_links$size >= 0.5,]
strong_PECAN_11 <- qgraph(strong_11_links, vsize = PP_11_nodes_sizes, theme = 'colorblind', layout = thelayout, minimum = 0.5, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)
summary(strong_PECAN_11)

# Participant 12
psych::describe(PP_12_links$size, interp = TRUE, quant = TRUE, IQR = TRUE)
summary(PECAN_12) #number of edges and unique weights
# How many edges have a causal strength of >= 0.5?
strong_12_links <- PP_12_links[PP_12_links$size >= 0.5,]
strong_PECAN_12_node_names <- PP_12_nodes$nodeId
strong_PECAN_12 <- qgraph(strong_12_links, vsize = PP_12_nodes_sizes, theme = 'colorblind', layout = thelayout, minimum = 0.5, labels = strong_PECAN_12_node_names, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)
summary(strong_PECAN_12)

# Participant 13
psych::describe(PP_13_links$size, interp = TRUE, quant = TRUE, IQR = TRUE)
summary(PECAN_13) #number of edges and unique weights
# How many edges have a causal strength of >= 0.5?
strong_13_links <- PP_13_links[PP_13_links$size >= 0.5,]
strong_PECAN_13 <- qgraph(strong_13_links, vsize = PP_13_nodes_sizes, theme = 'colorblind', layout = thelayout, minimum = 0.5, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)
summary(strong_PECAN_13)


### Nodewise analysis (frequency of each node)
# all_nodes_df[,1]
# 1] "rest"   "suic"   "sleep"  "conc"   "mood"  
# [6] "fatig"  "worth"  "weight" "anhed" 
nodewise_analysis_df <- subset(all_nodes_df, select = -nodeId )

rowMeans(nodewise_analysis_df)

apply(nodewise_analysis_df, 1, sd) 
apply(nodewise_analysis_df, 1, min)
apply(nodewise_analysis_df, 1, max)

###################################################
###### CENTRALITY MEASURES OF INDIVIDUAL PECANS
###################################################

ppN <- 13 # 13 participants
pp_pecans <- paste0('PECAN_', 1:ppN)
pp_centralities <- list()
for(i in seq_along(pp_pecans)){
  pp_pecan <- get(pp_pecans[i])
  
  indegree <- centrality(pp_pecan, weighted = FALSE)$InDegree
  outdegree <- centrality(pp_pecan, weighted = FALSE)$OutDegree
  instrength <- centrality_auto(pp_pecan, weighted = TRUE, signed = TRUE)$node.centrality[,3]
  outstrength <- centrality_auto(pp_pecan, weighted = TRUE, signed = TRUE)$node.centrality[,4]
  
  pp_centrality_matrix <- cbind(indegree, outdegree, instrength, outstrength)
  colnames(pp_centrality_matrix) <- paste0(c("inDeg", "outDeg", "inStr", "outStr"), i)
  pp_centralities[[i]] <- pp_centrality_matrix
}
all_pp_centralities <- do.call(cbind, pp_centralities)

#Calculating centrality descriptives (mean and SD per centrality measure); using full-dataset (no threshold for causal strength)
centralities <- as.data.frame(all_pp_centralities) %>%
  rowwise() %>% 
  mutate(mean_indegree = mean(c_across(paste0('inDeg', 1:ppN)))) %>%
  rowwise() %>% 
  mutate(sd_indegree = sd(c_across(paste0('inDeg', 1:ppN)))) %>%
  rowwise() %>% 
  mutate(mean_outdegree = mean(c_across(paste0('outDeg', 1:ppN)))) %>%
  rowwise() %>% 
  mutate(sd_outdegree = sd(c_across(paste0('outDeg', 1:ppN)))) %>%
  rowwise() %>% 
  mutate(mean_instrength = mean(c_across(paste0('inStr', 1:ppN)))) %>% 
  rowwise() %>% 
  mutate(sd_instrength = sd(c_across(paste0('inStr', 1:ppN)))) %>% 
  rowwise() %>% 
  mutate(mean_outstrength = mean(c_across(paste0('outStr', 1:ppN)))) %>%
  rowwise() %>% 
  mutate(sd_outstrength = sd(c_across(paste0('outStr', 1:ppN))))
centralities <- as.data.frame(centralities)
# Node IDs are all in same order, so can be used as a name column added to the centralities dataframe for clarity
Node_Names <- strong_PECAN_12_node_names
centralities <- cbind(Node_Names, centralities)

library(rempsyc)
centralities_for_table <- centralities[c(1, 54:61)]
colnames(centralities_for_table) <- c('Symptom node', "Mean in-degree", "SD in-degree", "Mean out-degree", "SD out-degree", "Mean in-strength", "SD in-strength", "Mean out-strength", "SD out-strength")
nice_centralities_table <- nice_table(centralities_for_table, title = c("Table 4", "Clinician PECAN centrality measures"))
print(nice_centralities_table, preview = "docx")

#### Creating bar chart of the above
library(ggplot2)
library(tidyr)
  #renaming first coloumn to make it easier to write code hereunder
centralities_for_table_plot <-centralities_for_table
colnames(centralities_for_table_plot) <- c('Symptom', "Mean_in-degree", "SD_in-degree", "Mean_out-degree", "SD_out-degree", "Mean_in-strength", "SD_in-strength", "Mean_out-strength", "SD_out-strength")

centralities_means_df <- centralities_for_table_plot %>% 
  select(Symptom, starts_with("Mean")) %>% 
  pivot_longer(-Symptom, names_to = "centrality", values_to = "mean") %>%
  mutate(centrality = gsub("Mean_", "", centrality))

centralities_SD_df <- centralities_for_table_plot %>% 
  select(Symptom, starts_with("SD")) %>% 
  pivot_longer(-Symptom, names_to = "centrality", values_to = "SD") %>%
  mutate(centrality = gsub("SD_", "", centrality))

print(unique(centralities_means_df$centrality))
print(unique(centralities_SD_df$centrality)) # checking all went well above

centralities_df_long <- left_join(centralities_means_df, centralities_SD_df, by = c("Symptom", "centrality")) %>%
  mutate(
    metric = factor(centrality,
                    levels = c("in-degree", "out-degree", "in-strength", "out-strength"),
                    labels = c("In-degree", "Out-degree", "In-strength", "Out-strength"))
  )
  

ggplot(centralities_df_long, aes(x = Symptom, y = mean, fill = centrality)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean - SD, ymax = mean + SD),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +
  scale_fill_brewer(palette = "Set2", name = "Centrality measure") +
  labs(
    x     = "Symptom",
    y     = "Mean"
  ) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

#### Centrality measures for PECANs 1,5,7 with additional nodes
# PP1
indegree_full1 <- centrality(PECAN_1_full, weighted = FALSE)$InDegree
outdegree_full1 <- centrality(PECAN_1_full, weighted = FALSE)$OutDegree
instrength_full1 <- centrality_auto(PECAN_1_full, weighted = TRUE, signed = TRUE)$node.centrality[,3]
outstrength_full1 <- centrality_auto(PECAN_1_full, weighted = TRUE, signed = TRUE)$node.centrality[,4]

# PP5
indegree_full5 <- centrality(PECAN_5_full, weighted = FALSE)$InDegree
outdegree_full5 <- centrality(PECAN_5_full, weighted = FALSE)$OutDegree
instrength_full5 <- centrality_auto(PECAN_5_full, weighted = TRUE, signed = TRUE)$node.centrality[,3]
outstrength_full5 <- centrality_auto(PECAN_5_full, weighted = TRUE, signed = TRUE)$node.centrality[,4]

#PP7
indegree_full7 <- centrality(PECAN_7_full, weighted = FALSE)$InDegree
outdegree_full7 <- centrality(PECAN_7_full, weighted = FALSE)$OutDegree
instrength_full7 <- centrality_auto(PECAN_7_full, weighted = TRUE, signed = TRUE)$node.centrality[,3]
outstrength_full7 <- centrality_auto(PECAN_7_full, weighted = TRUE, signed = TRUE)$node.centrality[,4]

###################################################
###### Weighted density = sum of absolute edge weights/total possible edges
###################################################

weighted_density <- function(l){
  sum(abs(l[,3]))/72
}
pp_links <- paste0('PP', 1:ppN, '_links')
pp_links_list <- mget(pp_links) 
pp_weighted_densities <- sapply(pp_links_list, weighted_density)
summary(pp_weighted_densities)
sd(pp_weighted_densities)

strong_pp_links <- paste0('strong_', 1:ppN, '_links') # For strong links only
strong_pp_links_list <- mget(strong_pp_links) 
strong_pp_weighted_densities <- sapply(strong_pp_links_list, weighted_density)
summary(strong_pp_weighted_densities)
sd(strong_pp_weighted_densities)
###################################################
###### Measure of similarity (inspired by leave-one-out cross-validation)
###################################################

leave_one_out_corr_links <- function(i, pp_links_list) {
  target_pp <- pp_links_list[[i]]
  weights_pp <- target_pp[[3]]
  other_pps <- pp_links_list[-i]
  
  others_combined <- purrr::reduce(other_pps, dplyr::inner_join, by = c('source.nodeId', 'target.nodeId'))
  n_other_pps <- length(other_pps)
  newnames <- paste0("pp", setdiff(1:ppN, i))
  colnames(others_combined)[3:(2 + n_other_pps)] <- newnames
  
  grouped_comparison <- others_combined %>%
    rowwise() %>% 
    mutate(weight_mean = mean(c_across(all_of(newnames)))) %>% ungroup()
  
  cor(weights_pp, grouped_comparison$weight_mean)
}

comparisons <- sapply(1:ppN, leave_one_out_corr_links, pp_links_list = pp_links_list)
comparisons_df <- as.data.frame(rbind(comparisons))
colnames(comparisons_df) <- paste0('pp', 1:ppN)

describe(comparisons) # descriptives of similarity measures
#View(comparisons_df)

###### Same as the above for node frequencies
leave_one_out_corr_nodes <- function(i, pp_links_list) {
  target_pp <- pp_links_list[[i]]
  weights_pp <- target_pp[[3]]
  other_pps <- pp_links_list[-i]
  
  others_combined <- purrr::reduce(other_pps, dplyr::inner_join, by = c('source.nodeId', 'target.nodeId'))
  n_other_pps <- length(other_pps)
  newnames <- paste0("pp", setdiff(1:ppN, i))
  colnames(others_combined)[3:(2 + n_other_pps)] <- newnames
  
  grouped_comparison <- others_combined %>%
    rowwise() %>% 
    mutate(weight_mean = mean(c_across(all_of(newnames)))) %>% ungroup()
  
  cor(weights_pp, grouped_comparison$weight_mean)
}

comparisons <- sapply(1:ppN, leave_one_out_corr_nodes, pp_links_list = pp_links_list)
comparisons_df <- as.data.frame(rbind(comparisons))
colnames(comparisons_df) <- paste0('pp', 1:ppN)
describe(comparisons)

comparisons_df_table <- comparisons_df
mean_comparison <- mean(as.numeric(comparisons_df_table[1,]))
med_comparisons <- median(as.numeric(comparisons_df_table[1,]))
sd_comparisons <- sd(as.numeric(comparisons_df_table[1,]))
comparisons_df_table <- cbind(comparisons_df_table, mean_comparison, med_comparisons, sd_comparisons)
colnames(comparisons_df_table) <- c(paste(1:ppN), 'Mean', 'Median', 'SD')
tidy_comparisons_df_table <- tidyr::pivot_longer(comparisons_df_table, cols = c(paste(1:ppN), 'Mean', 'Median', 'SD'))

summary(comparisons)

#recalculating descriptives
mean(as.numeric(comparisons_df[1,]))
# DEscriptives to check
mean(c(-0.09580977, -0.2539929, 0.1004387, 0.4565244, 0.3916569, 0.04802557, 0.2013935, 0.1074719, -0.009418096, -0.2747348, 0.1077428, 0.1203131, 0.2698387))
median(c(-0.09580977, -0.2539929, 0.1004387, 0.4565244, 0.3916569, 0.04802557, 0.2013935, 0.1074719, -0.009418096, -0.2747348, 0.1077428, 0.1203131, 0.2698387))
sd(c(-0.09580977, -0.2539929, 0.1004387, 0.4565244, 0.3916569, 0.04802557, 0.2013935, 0.1074719, -0.009418096, -0.2747348, 0.1077428, 0.1203131, 0.2698387))

nice_comparisons_PP_table <- rempsyc::nice_table(tidy_comparisons_df_table, title = c("Table 5", "Clinician PECAN Correlation Comparison Metrics for Edge Strengths"), note = "The correlation coefficient for each clinician's edge list compared to the aggregation of the remaining clinician edge lists")

print(nice_comparisons_PP_table, preview = "docx")

###################################################
###### Multiple PECANs preparation
###################################################

#Creating one larger EDGE dataset with source, target, and matching edge strength for all pps
newnames <- paste0('pp', 1:ppN) # new column names, participant number
k_link <- ppN + 2 # last column

pp_links_df <- paste0('PP_', 1:ppN, '_links')
links_df <- mget(pp_links_df) # list of all individual link dataframes
combined_links <- purrr::reduce(links_df, dplyr::inner_join, by = c('source.nodeId', 'target.nodeId')) %>%
  rename_at(3:k_link, ~ newnames)

# Adding new columns for mean and SD, per link
combined_links_sum <- as.data.frame(combined_links) %>% 
  rowwise() %>% 
  mutate(m = mean(c_across(paste0('pp', 1:ppN)))) %>% # means
  rowwise() %>% 
  mutate(med = median(c_across(paste0('pp', 1:ppN)))) %>% # medians
  rowwise() %>% 
  mutate(sd = sd(c_across(paste0('pp', 1:ppN)))) %>% # SDs
  rowwise() %>% 
  mutate(var = var(c_across(paste0('pp', 1:ppN)))) # variance
combined_links_summ <- as.data.frame(combined_links_sum) # back to dataframe

#####
#Creating one larger NODES dataset with variable and frequency for all pps
newnames <- paste0('pp', 1:ppN) # new column names, participant number
k_node <- ppN + 1 # last column

pp_nodes_dfs <- paste0('PP_', 1:ppN, '_nodes')
nodes_dfs <- mget(pp_nodes_dfs) # retrieve all individual node dataframes
combined_nodes <- purrr::reduce(nodes_dfs, dplyr::inner_join, by = 'nodeId') %>%
  rename_at(2:k_node, ~ newnames) # frequencies range retained from 0 - 100, for easier translation to %

# Adding new columns for mean and SD, per link
combined_nodes_sum <- as.data.frame(combined_nodes) %>% 
  rowwise() %>% 
  mutate(m = mean(c_across(paste0('pp', 1:ppN)))) %>% # means
  rowwise() %>% 
  mutate(med = median(c_across(paste0('pp', 1:ppN)))) %>% # medians
  rowwise() %>% 
  mutate(sd = sd(c_across(paste0('pp', 1:ppN)))) %>% # SDs
  rowwise() %>% 
  mutate(var = var(c_across(paste0('pp', 1:ppN)))) # variance
combined_nodes_summ <- as.data.frame(combined_nodes_sum) # back to dataframe
