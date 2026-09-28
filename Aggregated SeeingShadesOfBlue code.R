## ----------------------------
##### AGGREGATED PECAN
## ----------------------------
# only edgelist needed as input

setwd() # adjust accordingly

# node sizes adjusted for visualisation; interpreted as a % originally, but divided by 10 instead of 100 for visualisation to avoid too small nodes
nodesizes_aggr <- combined_nodes_summ$m/10
links_aggr <- combined_links_summ[c('source.nodeId', 'target.nodeId', 'm')]
colnames(links_aggr)[3] <- "weights"
PECAN_aggr <- qgraph(links_aggr, vsize = nodesizes_aggr, minimum = 0.5, theme = 'gray', layout = 'spring', normalize = TRUE, curveAll = TRUE, label.cex = 2.5, curveDefault = 1.8)
thelayout_aggr <- PECAN_aggr$layout

## ----------------------------
##### REPLOTTING CLINICIANS PECANS, NOW USING AGGREGATED LAYOUT
## ----------------------------

# Save the plots as svg, with w = 1900 and h = 1300

PECAN_1_full <- qgraph(PP1_links_full, vsize = PP1_nodes_full_sizes, minimum = 0.5, theme = 'gray', layout = 'spring', nodeNames = MDD_symptoms_pp1_full, legend.cex = 0.5, normalize = TRUE, curveAll = TRUE, label.cex = 2.5, curveDefault = 1.8)

PECAN_1 <- qgraph(PP_1_links, vsize = PP_1_nodes_sizes, minimum = 0.5, theme = 'gray', layout = thelayout_aggr, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.5, normalize = TRUE, curveAll = TRUE, label.cex = 2.5, curveDefault = 1.8)

#PP2
PECAN_2 <- qgraph(PP_2_links, vsize = PP_2_nodes_sizes, minimum = 0.5, theme = 'gray', layout = thelayout_aggr, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.5, normalize = TRUE, curveAll = TRUE, label.cex = 2.5, curveDefault = 1.8)

#PP3
PECAN_3 <- qgraph(PP_3_links, vsize = PP_3_nodes_sizes, minimum = 0.5, theme = 'gray', layout = thelayout_aggr, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.5, normalize = TRUE, curveAll = TRUE, label.cex = 2.5, curveDefault = 1.8)

#PP4
PECAN_4 <- qgraph(PP_4_links, vsize = PP_4_nodes_sizes, minimum = 0.5, theme = 'gray', layout = thelayout_aggr, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.5, normalize = TRUE, curveAll = TRUE, label.cex = 2.5, curveDefault = 1.8)

#PP5
MDD_symptoms_pp5_full <- c('Psychomotor agitation or retardation', 'Suicidal thoughts or behaviour', 'Insomnia or Hypersomnia', 'Diminished concentration or indecisiveness', 'Depressed mood', 'Fatigue or loss of energy', 'Feelings of worthlessness or excessive/inappropriate guilt', 'Weight or appetite fluctuations', 'Anhedonia', 'Emotion regulation')

PECAN_5_full <- qgraph(PP5_links_full, vsize = PP5_nodes_full_sizes, minimum = 0.5, theme = 'gray', layout = 'spring', nodeNames = MDD_symptoms_pp5_full, legend.cex = 0.5, normalize = TRUE, curveAll = TRUE, label.cex = 2.5, curveDefault = 1.8)

PECAN_5 <- qgraph(PP_5_links, vsize = PP_5_nodes_sizes, minimum = 0.5, theme = 'gray', layout = thelayout_aggr, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.5, normalize = TRUE, curveAll = TRUE, label.cex = 2.5, curveDefault = 1.8)

#PP6
PECAN_6 <- qgraph(PP_6_links, vsize = PP_6_nodes_sizes, minimum = 0.5, theme = 'gray', layout = thelayout_aggr, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.5, normalize = TRUE, curveAll = TRUE, label.cex = 2.5, curveDefault = 1.8)

#PP7
MDD_symptoms_pp7_full <- c('Psychomotor agitation or retardation', 'Suicidal thoughts or behaviour', 'Insomnia or Hypersomnia', 'Diminished concentration or indecisiveness', 'Depressed mood', 'Fatigue or loss of energy', 'Feelings of worthlessness or excessive/inappropriate guilt', 'Weight or appetite fluctuations', 'Anhedonia', 'Internalising coping')

PECAN_7_full <- qgraph(PP7_links_full, vsize = PP7_nodes_full_sizes, minimum = 0.5, theme = 'gray', layout = 'spring', nodeNames = MDD_symptoms_pp7_full, legend.cex = 0.5, normalize = TRUE, curveAll = TRUE, label.cex = 2.5, curveDefault = 1.8)

PECAN_7 <- qgraph(PP_7_links, vsize = PP_7_nodes_sizes, minimum = 0.5, theme = 'gray', layout = thelayout_aggr, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.5, normalize = TRUE, curveAll = TRUE, label.cex = 2.5, curveDefault = 1.8)

#PP8
PECAN_8 <- qgraph(PP_8_links, vsize = PP_8_nodes_sizes, minimum = 0.5, theme = 'gray', layout = thelayout_aggr, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.5, normalize = TRUE, curveAll = TRUE, label.cex = 2.5, curveDefault = 1.8)

#PP9
PECAN_9 <- qgraph(PP_9_links, vsize = PP_9_nodes_sizes, minimum = 0.5, theme = 'gray', layout = thelayout_aggr, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.5, normalize = TRUE, curveAll = TRUE, label.cex = 2.5, curveDefault = 1.8)

#PP10
PECAN_10 <- qgraph(PP_10_links, vsize = PP_10_nodes_sizes, minimum = 0.5, theme = 'gray', layout = thelayout_aggr, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.5, normalize = TRUE, curveAll = TRUE, label.cex = 2.5, curveDefault = 1.8)

#PP11
PECAN_11 <- qgraph(PP_11_links, vsize = PP_11_nodes_sizes, minimum = 0.5, theme = 'gray', layout = thelayout_aggr, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)

#PP12
PECAN_12 <- qgraph(PP_12_links, vsize = PP_12_nodes_sizes, minimum = 0.5, theme = 'gray', layout = thelayout_aggr, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.5, normalize = TRUE, curveAll = TRUE, label.cex = 2.5, curveDefault = 1.8)

#PP13
PECAN_13 <- qgraph(PP_13_links, vsize = PP_13_nodes_sizes, minimum = 0.5, theme = 'gray', layout = thelayout_aggr, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.5, normalize = TRUE, curveAll = TRUE, label.cex = 2.5, curveDefault = 1.8)

## ----------------------------
##### DESCRIPTIVES AGGREGATED PECAN
## ----------------------------
psych::describe(links_aggr$weights, interp = TRUE, quant = TRUE, IQR = TRUE) # Descriptive statistics
summary(PECAN_aggr) #number of edges and unique weights
# How many edges have a causal strength of >= 0.5?
Strong_links_aggr <- links_aggr[links_aggr$weights >= 0.5,]
# not all nodes are target nodes, so need labels to correctly identify nodes
strong_aggr_node_names <- combined_nodes_summ$nodeId
strong_pilotPECAN_aggr <- qgraph(Strong_links_aggr, vsize = nodesizes_aggr, theme = 'gray', layout = thelayout, minimum = 0.5, labels = strong_aggr_node_names)
summary(strong_pilotPECAN_aggr)
psych::describe(Strong_links_aggr$weights, interp = TRUE, quant = TRUE, IQR = TRUE)

hist(links_aggr$weights, freq =TRUE)

weighted_density(links_aggr) 

# Making table for supplementary materials; adjusted manually
links_aggr_nice <- links_aggr
colnames(links_aggr_nice) <- c('Source node', 'Target node', 'Weight')
nice_aggregated_edge_list_table <- nice_table(links_aggr_nice, title = c("Edge weights of aggregated MDD PECAN"), note = c('rest: Psychomotor agitation or retardation', 'suic: Suicidal thoughts or behaviour', 'sleep: Insomnia or Hypersomnia', 'conc: Diminished concentration or indecisiveness', 'mood: Depressed mood', 'fatig: Fatigue or loss of energy', 'worth: Feelings of worthlessness or excessive/inappropriate guilt', 'weight: Weight or appetite fluctuations', 'anhed: Anhedonia'))
print(nice_aggregated_edge_list_table, preview = "docx")

## ----------------------------
##### CENTRALITY AGGREGATED PECAN
## ----------------------------
indegree_aggr <- centrality(PECAN_aggr, weighted = FALSE)$InDegree
outdegree_aggr <- centrality(PECAN_aggr, weighted = FALSE)$OutDegree
instrength_aggr <- centrality_auto(PECAN_aggr, weighted = TRUE, signed = TRUE)$node.centrality[,3]
outstrength_aggr <- centrality_auto(PECAN_aggr, weighted = TRUE, signed = TRUE)$node.centrality[,4]
centralities_aggr <- cbind(indegree_aggr, outdegree_aggr,instrength_aggr, outstrength_aggr)

###################################################
# Dividing participants based on their beliefs about the aetiology of MDD

################-------------- Cognitive aetiology
Cognitive_PP <- PP_data_Qualtrics[PP_data_Qualtrics$Cognitions == '1',]
Cognitive_PP$Q19_Name # "pecan_data_pp7"  "pecan_data_pp9"  "pecan_data_pp11"

cog_n <- length(Cognitive_PP$Q19_Name)
cognitive_names <- paste0('C_pp_', 1:cog_n) # new column names, participant number
cognitive_k_link <- cog_n + 2 # last column
cognitive_pp_links <- c("PP_7_links", "PP_9_links", "PP_11_links")
cognitive_links_df <- mget(cognitive_pp_links) # list of cognitive individual link dataframes
cognitive_combined_links <- purrr::reduce(cognitive_links_df, dplyr::inner_join, by = c('source.nodeId', 'target.nodeId')) %>%
  rename_at(3:cognitive_k_link, ~ cognitive_names)

# Adding new columns for mean and SD, per link
cognitive_combined_links_sum <- as.data.frame(cognitive_combined_links) %>% 
  rowwise() %>% 
  mutate(m = mean(c_across(paste0('C_pp_', 1:cog_n)))) %>% # means
  mutate(med = median(c_across(paste0('C_pp_', 1:cog_n)))) %>% # medians
  mutate(sd = sd(c_across(paste0('C_pp_', 1:cog_n)))) %>% # SDs
  mutate(var = var(c_across(paste0('C_pp_', 1:cog_n)))) # variance
cognitive_combined_links_df <- as.data.frame(cognitive_combined_links_sum) # back to dataframe

Cognitive_links <- cognitive_combined_links_df[c('source.nodeId', 'target.nodeId', 'm')]
colnames(Cognitive_links)[3] <- "weights"
range(Cognitive_links$weights)

# Creating one cognitive NODES dataset with variable and frequency for cognitive pps
cognitive_k_node <- cog_n + 1 # last column
cognitive_pp_nodes <-  c("PP_7_nodes", "PP_9_nodes", "PP_11_nodes")
cognitive_nodes_dfs <- mget(cognitive_pp_nodes)
cognitive_combined_nodes <- purrr::reduce(cognitive_nodes_dfs, dplyr::inner_join, by = 'nodeId') %>%
  rename_at(2:cognitive_k_node, ~ cognitive_names)

# Adding new columns for mean and SD, per link
cognitive_combined_nodes_sum <- as.data.frame(cognitive_combined_nodes) %>% 
  rowwise() %>% 
  mutate(m = mean(c_across(paste0('C_pp_', 1:cog_n)))) %>% # means
  mutate(med = median(c_across(paste0('C_pp_', 1:cog_n)))) %>% # medians
  mutate(sd = sd(c_across(paste0('C_pp_', 1:cog_n)))) %>% # SDs
  mutate(var = var(c_across(paste0('C_pp_', 1:cog_n)))) # variance
cognitive_combined_nodes_df <- as.data.frame(cognitive_combined_nodes_sum) # back to dataframe

Cognitive_nodes_aggr <- cbind(cognitive_combined_nodes_df$nodeId, cognitive_combined_nodes_df$m)
colnames(Cognitive_nodes_aggr) <- c('nodeId', 'weights')
Cognitive_node_sizes <- as.numeric(Cognitive_nodes_aggr[,2]) / 10

################-------------- Behavioural aetiology
Behavioural_PP <- PP_data_Qualtrics[PP_data_Qualtrics$Behaviour == '1',]
Behavioural_PP$Q19_Name # "pecan_data_pp1"  "pecan_data_pp4"  "pecan_data_pp8"  "pecan_data_pp10"

beh_n <- length(Behavioural_PP$Q19_Name)
behavioural_names <- paste0('B_pp_', 1:beh_n)
behavioural_k_link <- beh_n + 2
behavioural_pp_links <- c("PP_1_links", "PP_4_links", "PP_8_links", "PP_10_links")
behavioural_links_df <- mget(behavioural_pp_links) # list of behavioural individual link dataframes
behavioural_combined_links <- purrr::reduce(behavioural_links_df, dplyr::inner_join, by = c('source.nodeId', 'target.nodeId')) %>%
  rename_at(3:behavioural_k_link, ~ behavioural_names)

# Adding new columns for mean and SD, per link
behavioural_combined_links_sum <- as.data.frame(behavioural_combined_links) %>% 
  rowwise() %>% 
  mutate(m = mean(c_across(paste0('B_pp_', 1:beh_n)))) %>% # means
  mutate(med = median(c_across(paste0('B_pp_', 1:beh_n)))) %>% # medians
  mutate(sd = sd(c_across(paste0('B_pp_', 1:beh_n)))) %>% # SDs
  mutate(var = var(c_across(paste0('B_pp_', 1:beh_n)))) # variance
behavioural_combined_links_df <- as.data.frame(behavioural_combined_links_sum) # back to dataframe

Behavioural_links <- behavioural_combined_links_df[c('source.nodeId', 'target.nodeId', 'm')]
colnames(Behavioural_links)[3] <- "weights"
range(Behavioural_links$weights)

behavioural_k_node <- beh_n + 1 # last column
behavioural_pp_nodes <- c("PP_1_nodes", "PP_4_nodes", "PP_8_nodes", "PP_10_nodes")
behavioural_nodes_dfs <- mget(behavioural_pp_nodes)
behavioural_combined_nodes <- purrr::reduce(behavioural_nodes_dfs, dplyr::inner_join, by = 'nodeId') %>%
  rename_at(2:behavioural_k_node, ~ behavioural_names)

# Adding new columns for mean and SD, per link
behavioural_combined_nodes_sum <- as.data.frame(behavioural_combined_nodes) %>% 
  rowwise() %>% 
  mutate(m = mean(c_across(paste0('B_pp_', 1:beh_n)))) %>% # means
  mutate(med = median(c_across(paste0('B_pp_', 1:beh_n)))) %>% # medians
  mutate(sd = sd(c_across(paste0('B_pp_', 1:beh_n)))) %>% # SDs
  mutate(var = var(c_across(paste0('B_pp_', 1:beh_n)))) # variance
behavioural_combined_nodes_df <- as.data.frame(behavioural_combined_nodes_sum) # back to dataframe

Behavioural_nodes_aggr <- cbind(behavioural_combined_nodes_df$nodeId, behavioural_combined_nodes_df$m)
colnames(Behavioural_nodes_aggr) <- c('nodeId', 'weights')
Behavioural_node_sizes <- as.numeric(Behavioural_nodes_aggr[,2]) / 10

################-------------- Youth and/or subconscious aetiology
YouthSubconscious_PP <- PP_data_Qualtrics[PP_data_Qualtrics$`Experiences during youth and/or subconscious` == '1',]
YouthSubconscious_PP$Q19_Name # "pecan_data_pp2"  "pecan_data_pp3"  "pecan_data_pp5"  "pecan_data_pp6" "pecan_data_pp12" "pecan_data_pp13"

you_n <- length(YouthSubconscious_PP$Q19_Name)
youth_names <- paste0('Y_pp_', 1:you_n)
youth_k_link <- you_n + 2
youth_pp_links <- c("PP_2_links", "PP_3_links", "PP_5_links", "PP_6_links", "PP_12_links", "PP_13_links")
youth_links_df <- mget(youth_pp_links) # list of youth/subconscious individual link dataframes
youth_combined_links <- purrr::reduce(youth_links_df, dplyr::inner_join, by = c('source.nodeId', 'target.nodeId')) %>%
  rename_at(3:youth_k_link, ~ youth_names)

# Adding new columns for mean and SD, per link
youth_combined_links_sum <- as.data.frame(youth_combined_links) %>% 
  rowwise() %>% 
  mutate(m = mean(c_across(paste0('Y_pp_', 1:you_n)))) %>% # means
  mutate(med = median(c_across(paste0('Y_pp_', 1:you_n)))) %>% # medians
  mutate(sd = sd(c_across(paste0('Y_pp_', 1:you_n)))) %>% # SDs
  mutate(var = var(c_across(paste0('Y_pp_', 1:you_n)))) # variance
youth_combined_links_df <- as.data.frame(youth_combined_links_sum) # back to dataframe

Youth_links <- youth_combined_links_df[c('source.nodeId', 'target.nodeId', 'm')]
colnames(Youth_links)[3] <- "weights"
range(Youth_links$weights)

youth_k_node <- you_n + 1 # last column
youth_pp_nodes <- c("PP_2_nodes", "PP_3_nodes", "PP_5_nodes", "PP_6_nodes", "PP_12_nodes", "PP_13_nodes")
youth_nodes_dfs <- mget(youth_pp_nodes)
youth_combined_nodes <- purrr::reduce(youth_nodes_dfs, dplyr::inner_join, by = 'nodeId') %>%
  rename_at(2:youth_k_node, ~ youth_names)

# Adding new columns for mean and SD, per link
youth_combined_nodes_sum <- as.data.frame(youth_combined_nodes) %>% 
  rowwise() %>% 
  mutate(m = mean(c_across(paste0('Y_pp_', 1:you_n)))) %>% # means
  mutate(med = median(c_across(paste0('Y_pp_', 1:you_n)))) %>% # medians
  mutate(sd = sd(c_across(paste0('Y_pp_', 1:you_n)))) %>% # SDs
  mutate(var = var(c_across(paste0('Y_pp_', 1:you_n)))) # variance
youth_combined_nodes_df <- as.data.frame(youth_combined_nodes_sum) # back to dataframe

Youth_nodes_aggr <- cbind(youth_combined_nodes_df$nodeId, youth_combined_nodes_df$m)
colnames(Youth_nodes_aggr) <- c('nodeId', 'weights')
Youth_node_sizes <- as.numeric(Youth_nodes_aggr[,2]) / 10

## ----------------------------
##### ORIENTATION PECAN
## ----------------------------
# saved at w = 1900, h = 1300

PECAN_cog <- qgraph(Cognitive_links, vsize = Cognitive_node_sizes, minimum = 0.5, theme = 'gray', layout = thelayout_aggr, normalize = TRUE, curveAll = TRUE,label.cex = 2.5, curveDefault = 1.8)
summary(Cognitive_links$weights)

PECAN_beh <- qgraph(Behavioural_links, vsize = Behavioural_node_sizes, minimum = 0.5, theme = 'gray', layout = thelayout_aggr, normalize = TRUE, curveAll = TRUE,label.cex = 2.5, curveDefault = 1.8)
summary(Behavioural_links$weights)

PECAN_you <- qgraph(Youth_links, vsize = Youth_node_sizes, minimum = 0.5, theme = 'gray', layout = thelayout_aggr, normalize = TRUE, curveAll = TRUE,label.cex = 2.5, curveDefault = 1.8)
summary(Youth_links$weights)

## ----------------------------
##### DESCRIPTIVES AGGREGATED PECAN
## ----------------------------
psych::describe(Cognitive_links$weights, interp = TRUE, quant = TRUE, IQR = TRUE) # Descriptive statistics
summary(PECAN_cog) #number of edges and unique weights
# How many edges have a causal strength of >= 0.5?
Strong_links_cog <- Cognitive_links[Cognitive_links$weights >= 0.5,]
strong_cog_node_names <- cognitive_combined_nodes_df$nodeId
strong_PECAN_cog <- qgraph(Strong_links_cog, vsize = Cognitive_node_sizes, theme = 'gray', layout = thelayout_aggr, minimum = 0.5, labels = strong_cog_node_names, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.5, normalize = TRUE, curveAll = TRUE)
summary(strong_PECAN_cog)
summary(Strong_links_cog$weights)
plot(Strong_links_cog$weights)

psych::describe(Behavioural_links$weights, interp = TRUE, quant = TRUE, IQR = TRUE) # Descriptive statistics
summary(PECAN_beh) #number of edges and unique weights
# How many edges have a causal strength of >= 0.5?
Strong_links_beh <- Behavioural_links[Behavioural_links$weights >= 0.5,]
strong_beh_node_names <- behavioural_combined_nodes_df$nodeId
strong_PECAN_beh <- qgraph(Strong_links_beh, vsize = Behavioural_node_sizes, theme = 'gray', layout = thelayout_aggr, minimum = 0.5, labels = strong_beh_node_names, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)
summary(strong_PECAN_beh)
summary(Strong_links_beh$weights)
plot(Strong_links_beh$weights)

psych::describe(Youth_links$weights, interp = TRUE, quant = TRUE, IQR = TRUE) # Descriptive statistics
summary(PECAN_you) #number of edges and unique weights
# How many edges have a causal strength of >= 0.5?
Strong_links_you <- Youth_links[Youth_links$weights >= 0.5,]
strong_you_node_names <- youth_combined_nodes_df$nodeId
strong_PECAN_you <- qgraph(Strong_links_you, vsize = Youth_node_sizes, theme = 'gray', layout = thelayout_aggr, minimum = 0.5, labels = strong_you_node_names, legend.mode = 'names', nodeNames = MDD_symptoms, legend.cex = 0.4, normalize = TRUE, curveAll = TRUE)
summary(strong_PECAN_you)
summary(Strong_links_you$weights)
plot(Strong_links_you$weights)  

## ----------------------------
##### CENTRALITY AGGREGATED PECAN
## ----------------------------
indegree_cog <- centrality(PECAN_cog, weighted = FALSE)$InDegree
outdegree_cog <- centrality(PECAN_cog, weighted = FALSE)$OutDegree
instrength_cog <- centrality_auto(PECAN_cog, weighted = TRUE, signed = TRUE)$node.centrality[,3]
outstrength_cog <- centrality_auto(PECAN_cog, weighted = TRUE, signed = TRUE)$node.centrality[,4]

indegree_cog_strong <- centrality(strong_PECAN_cog, weighted = FALSE)$InDegree
outdegree_cog_strong <- centrality(strong_PECAN_cog, weighted = FALSE)$OutDegree
instrength_cog_strong <- centrality_auto(strong_PECAN_cog, weighted = TRUE, signed = TRUE)$node.centrality[,3]
outstrength_cog_strong <- centrality_auto(strong_PECAN_cog, weighted = TRUE, signed = TRUE)$node.centrality[,4]

indegree_beh <- centrality(PECAN_beh, weighted = FALSE)$InDegree
outdegree_beh <- centrality(PECAN_beh, weighted = FALSE)$OutDegree
instrength_beh <- centrality_auto(PECAN_beh, weighted = TRUE, signed = TRUE)$node.centrality[,3]
outstrength_beh <- centrality_auto(PECAN_beh, weighted = TRUE, signed = TRUE)$node.centrality[,4]

indegree_beh_strong <- centrality(strong_PECAN_beh, weighted = FALSE)$InDegree
outdegree_beh_strong <- centrality(strong_PECAN_beh, weighted = FALSE)$OutDegree
instrength_beh_strong <- centrality_auto(strong_PECAN_beh, weighted = TRUE, signed = TRUE)$node.centrality[,3]
outstrength_beh_strong <- centrality_auto(strong_PECAN_beh, weighted = TRUE, signed = TRUE)$node.centrality[,4]

indegree_you <- centrality(PECAN_you, weighted = FALSE)$InDegree
outdegree_you <- centrality(PECAN_you, weighted = FALSE)$OutDegree
instrength_you <- centrality_auto(PECAN_you, weighted = TRUE, signed = TRUE)$node.centrality[,3]
outstrength_you <- centrality_auto(PECAN_you, weighted = TRUE, signed = TRUE)$node.centrality[,4]

indegree_you_strong <- centrality(strong_PECAN_you, weighted = FALSE)$InDegree
outdegree_you_strong <- centrality(strong_PECAN_you, weighted = FALSE)$OutDegree
instrength_you_strong <- centrality_auto(strong_PECAN_you, weighted = TRUE, signed = TRUE)$node.centrality[,3]
outstrength_you_strong <- centrality_auto(strong_PECAN_you, weighted = TRUE, signed = TRUE)$node.centrality[,4]

centralities_orientations <- cbind(indegree_cog, indegree_beh, indegree_you, outdegree_cog, outdegree_beh, outdegree_you,instrength_cog,instrength_beh,instrength_you, outstrength_cog, outstrength_beh, outstrength_you)
# View(centralities_orientations)

centralities_aetiologies_nice <- centralities_orientations
colnames(centralities_aetiologies_nice) <- c("Cog. in-degree", "Beh. in-degree", "You. in-degree",  "Cog. out-degree", "Beh. out-degree", "You. out-degree", "Cog. in-strength", "Beh. in-strength",  "You. in-strength", "Cog. out-strength", "Beh. out-strength", "You. out-strength")
nice_centralities_aetiology_table <- nice_table(centralities_aetiologies_nice, title = c("Centrality measures for the Cogntive, Behavioural, and Youth/Subconscious PECANs"), note = c('Cog.: Cogntiive. Beh.: Behavioural. You.: Youth/Subconscious'))
print(nice_centralities_aetiology_table, preview = "docx")

#### Creating bar chart of the above
library(ggplot2)
library(tidyr)
#renaming first coloumn to make it easier to write code hereunder
centralities_aetiologies_plot <-centralities_aetiologies_nice[,7:12]
# adding column with symptom names
colnames(centralities_aetiologies_plot) <- c("Mean_Cog.in-strength", "Mean_Beh.in-strength",  "Mean_You.in-strength", "Mean_Cog.out-strength", "Mean_Beh.out-strength", "Mean_You.out-strength")
Symptom <- c("rest", "suic", "sleep", "conc", "mood", "fatig", "worth", "weight", "anhed")
centralities_aetiologies_plot <- cbind(Symptom, centralities_aetiologies_plot)

centralities_aetiologies_df_long <- as.data.frame(centralities_aetiologies_plot) %>%
  select(Symptom, starts_with("Mean")) %>% 
  mutate(across(starts_with("Mean"), as.numeric)) %>%
  pivot_longer(-Symptom, names_to = "centrality", values_to = "mean") %>%
  mutate(centrality = gsub("Mean_", "", centrality)) %>% 
  separate(centrality, into = c("School", "Strength"), sep = "\\.") %>% 
  mutate(
    Strength = factor(Strength, 
                      levels = c("in-strength", "out-strength"), 
                      labels = c("In-Strength", "Out-strength")),
    School = factor(School, 
                    levels = c("Cog", "Beh", "You"),
                    labels = c("Cognitive", "Behavioural", "Psychodynamic"))
  )


ggplot(centralities_aetiologies_df_long, aes(x = Symptom, y = mean, fill = School)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ Strength, ncol = 1) +
  scale_fill_brewer(palette = "Dark2", name = "Etiological Belief") +
  labs(
    x     = "Symptom",
    y     = "Centrality Measure Value"
  ) +
  theme_classic() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "top", 
    strip.text = element_text(face = "bold")
  )

# strong centrality measures
names_strong <- c('rest', 'suic', 'sleep', 'conc', 'mood', 'fatig', 'worth', 'weight', 'anhed')
strong_strengths_orientations <- cbind(names_strong, indegree_cog_strong, indegree_beh_strong, indegree_you_strong, outdegree_cog_strong, outdegree_beh_strong, outdegree_you_strong, instrength_cog_strong, instrength_beh_strong, instrength_you_strong, outstrength_cog_strong, outstrength_beh_strong, outstrength_you_strong)
nice_centralities_aetiologies_strong <- strong_strengths_orientations
colnames(nice_centralities_aetiologies_strong) <- c("Symptoms", "Cog. in-degree", "Beh. in-degree", "You. in-degree",  "Cog. out-degree", "Beh. out-degree", "You. out-degree", "Cog. in-strength", "Beh. in-strength",  "You. in-strength", "Cog. out-strength", "Beh. out-strength", "You. out-strength")
nice_centralities_aetiology_table_strong <- nice_table(nice_centralities_aetiologies_strong, title = c("Centrality measures for the Strong Cogntive, Behavioural, and Youth/Subconscious PECANs"), note = c('Cog.: Cogntiive. Beh.: Behavioural. You.: Youth/Subconscious'))
print(nice_centralities_aetiology_table_strong, preview = "docx")

###################################################
###### Weighted density = sum of absolute edge weights/total possible edges
###################################################
# Cognitive
WD_cog <- weighted_density(Cognitive_links)
WD_cog_strong <- weighted_density(Strong_links_cog)

# Behavioural
WD_beh <- weighted_density(Behavioural_links)
WD_beh_strong <- weighted_density(Strong_links_beh)

# Youth/Subconscious
WD_you <- weighted_density(Youth_links)
WD_you_strong <- weighted_density(Strong_links_you)

###################################################
###### Measure of similarity (inspired by leave-one-out cross-validation)
###################################################
ppN <- cog_n
comparisons_cog <- sapply(1:cog_n, leave_one_out_corr_links, pp_links_list = cognitive_links_df)
comparisons_cog_df <- as.data.frame(rbind(comparisons_cog))
colnames(comparisons_cog_df) <- paste0('pp_C', 1:ppN)
summary(comparisons_cog)

ppN <- beh_n
comparisons_beh <- sapply(1:beh_n, leave_one_out_corr_links, pp_links_list = behavioural_links_df)
comparisons_beh_df <- as.data.frame(rbind(comparisons_beh))
colnames(comparisons_beh_df) <- paste0('pp_B', 1:ppN)
summary(comparisons_beh)


ppN <- you_n
comparisons_you <- sapply(1:you_n, leave_one_out_corr_links, pp_links_list = youth_links_df)
comparisons_you_df <- as.data.frame(rbind(comparisons_you))
colnames(comparisons_you_df) <- paste0('pp_Y', 1:ppN)
summary(comparisons_you)


