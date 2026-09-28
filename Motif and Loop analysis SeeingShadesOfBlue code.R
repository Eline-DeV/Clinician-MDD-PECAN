
############  Seeing which (binarised) edges exist in all individual PECANs  ###############
# Binarised versions of the individual matrices
devtools::session_info()

library(dplyr)

binarising <- function(pecan_no_b) {
  ifelse(pecan_no_b[,3] > 0, 1, 0)
}
every_DSM_link <- list(PP_1_links, PP_2_links, PP_3_links, PP_4_links, PP_5_links, PP_6_links, PP_7_links, PP_8_links, PP_9_links, PP_10_links, PP_11_links, PP_12_links,PP_13_links)

binarised_every_DSM_link <- lapply(every_DSM_link, binarising)
# visually checking that all went well with some PECANs
check_bin_1 <- cbind(PP_1_links, binarised_every_DSM_link[[1]])
View(check_bin_1)
check_bin_6 <- cbind(PP_6_links, binarised_every_DSM_link[[6]])
View(check_bin_6)
check_bin_7 <- cbind(PP_7_links, binarised_every_DSM_link[[7]])
View(check_bin_7)

binarised_every_DSM_link <- as.data.frame(binarised_every_DSM_link)
colnames(binarised_every_DSM_link) <- paste0('PP', 1:13, '_nodes')

binarised_every_DSM_link$IncludedByAll <- apply(binarised_every_DSM_link, 1, function(row) all(row == row[1]))
# adding the two involved nodes for clarity and placing whether something is included by all earlier in df
binarised_every_DSM_link <- cbind(PP_1_links$source.nodeId, PP_1_links$target.nodeId, binarised_every_DSM_link$IncludedByAll, binarised_every_DSM_link)
View(binarised_every_DSM_link)
sum(binarised_every_DSM_link$IncludedByAll) # 40 edges have been included by all participants
# In the above df, we can see which edges were included by everyone (rated >0 causal strength)

############  Seeing which (binarised) edges exist in all STRONG individual PECANs  ###############
# adjusting binarising function (as earlier strong link lists cannot easily be used as they have differing lengths)
strong_binarising <- function(pecan_no_b) {
  ifelse(pecan_no_b[,3] >= 0.5, 1, 0)
}

strong_binarised_every_DSM_link <- lapply(every_DSM_link, strong_binarising)

strong_binarised_every_DSM_link <- as.data.frame(strong_binarised_every_DSM_link)
colnames(strong_binarised_every_DSM_link) <- paste0('PP', 1:13, '_nodes')
# visually checking that all went well with some PECANs
scheck_bin_1 <- cbind(PP_1_links, strong_binarised_every_DSM_link[[1]])
View(scheck_bin_1)
scheck_bin_6 <- cbind(PP_6_links, strong_binarised_every_DSM_link[[6]])
View(scheck_bin_6)
scheck_bin_7 <- cbind(PP_7_links, strong_binarised_every_DSM_link[[7]])
View(scheck_bin_7)

strong_binarised_every_DSM_link$IncludedByAll <- apply(strong_binarised_every_DSM_link, 1, function(row) all(row == row[1]))

# adding the two involved nodes for clarity and placing whether something is included by all earlier in df
strong_binarised_every_DSM_link <- cbind(PP_1_links$source.nodeId, PP_1_links$target.nodeId, strong_binarised_every_DSM_link$IncludedByAll, strong_binarised_every_DSM_link)

View(strong_binarised_every_DSM_link)

sum(strong_binarised_every_DSM_link$IncludedByAll) # 1

############## MOTIF PREPARATION ####
# igraph preparation of binarised copy op strong individual clinician PECAN
# first removing edges from the dataframe which are 0 (and hence should be excluded) as to binarise the links (e.g. present & absent)

### PP1
PP_1_igraph_prep <- PP_1_links[PP_1_links$size > 0,]
PP_1_igraph <- graph_from_data_frame(PP_1_igraph_prep, directed = TRUE)
PP_1_strong_igraph <- graph_from_data_frame(strong_1_links, directed = TRUE)
is_weighted(PP_1_igraph) # double-check
plot(PP_1_igraph)
is_weighted(PP_1_strong_igraph) # double-check
plot(PP_1_strong_igraph)

### PP2
PP_2_igraph_prep <- PP_2_links[PP_2_links$size > 0,]
PP_2_igraph <- graph_from_data_frame(PP_2_igraph_prep, directed = TRUE)
PP_2_strong_igraph <- graph_from_data_frame(strong_2_links, directed = TRUE)
is_weighted(PP_2_igraph) # double-check
plot(PP_2_igraph)
is_weighted(PP_2_strong_igraph) # double-check
plot(PP_2_strong_igraph)

### PP3
PP_3_igraph_prep <- PP_3_links[PP_3_links$size > 0,]
PP_3_igraph <- graph_from_data_frame(PP_3_igraph_prep, directed = TRUE)
PP_3_strong_igraph <- graph_from_data_frame(strong_3_links, directed = TRUE)
is_weighted(PP_3_igraph) # double-check
plot(PP_3_igraph)
is_weighted(PP_3_strong_igraph) # double-check
plot(PP_3_strong_igraph)

### PP4
PP_4_igraph_prep <- PP_4_links[PP_4_links$size > 0,]
PP_4_igraph <- graph_from_data_frame(PP_4_igraph_prep, directed = TRUE)
PP_4_strong_igraph <- graph_from_data_frame(strong_4_links, directed = TRUE)
is_weighted(PP_4_igraph) # double-check
plot(PP_4_igraph)
is_weighted(PP_4_strong_igraph) # double-check
plot(PP_4_strong_igraph)

### PP5
PP_5_igraph_prep <- PP_5_links[PP_5_links$size > 0,]
PP_5_igraph <- graph_from_data_frame(PP_5_igraph_prep, directed = TRUE)
PP_5_strong_igraph <- graph_from_data_frame(strong_5_links, directed = TRUE)
is_weighted(PP_5_igraph) # double-check
plot(PP_5_igraph)
is_weighted(PP_5_strong_igraph) # double-check
plot(PP_5_strong_igraph)

### PP6
PP_6_igraph_prep <- PP_6_links[PP_6_links$size > 0,]
PP_6_igraph <- graph_from_data_frame(PP_6_igraph_prep, directed = TRUE)
PP_6_strong_igraph <- graph_from_data_frame(strong_6_links, directed = TRUE)
is_weighted(PP_6_igraph) # double-check
plot(PP_6_igraph)
is_weighted(PP_6_strong_igraph) # double-check
plot(PP_6_strong_igraph)

### PP7
PP_7_igraph_prep <- PP_7_links[PP_7_links$size > 0,]
PP_7_igraph <- graph_from_data_frame(PP_7_igraph_prep, directed = TRUE)
PP_7_strong_igraph <- graph_from_data_frame(strong_7_links, directed = TRUE)
is_weighted(PP_7_igraph) # double-check
plot(PP_7_igraph)
is_weighted(PP_7_strong_igraph) # double-check
plot(PP_7_strong_igraph)

### PP8
PP_8_igraph_prep <- PP_8_links[PP_8_links$size > 0,]
PP_8_igraph <- graph_from_data_frame(PP_8_igraph_prep, directed = TRUE)
PP_8_strong_igraph <- graph_from_data_frame(strong_8_links, directed = TRUE)
is_weighted(PP_8_igraph) # double-check
plot(PP_8_igraph)
is_weighted(PP_8_strong_igraph) # double-check
plot(PP_8_strong_igraph)

### PP9
PP_9_igraph_prep <- PP_9_links[PP_9_links$size > 0,]
PP_9_igraph <- graph_from_data_frame(PP_9_igraph_prep, directed = TRUE)
PP_9_strong_igraph <- graph_from_data_frame(strong_9_links, directed = TRUE)
is_weighted(PP_9_igraph) # double-check
plot(PP_9_igraph)
is_weighted(PP_9_strong_igraph) # double-check
plot(PP_9_strong_igraph)

### PP10
PP_10_igraph_prep <- PP_10_links[PP_10_links$size > 0,]
PP_10_igraph <- graph_from_data_frame(PP_10_igraph_prep, directed = TRUE)
PP_10_strong_igraph <- graph_from_data_frame(strong_10_links, directed = TRUE)
is_weighted(PP_10_igraph) # double-check
plot(PP_10_igraph)
is_weighted(PP_10_strong_igraph) # double-check
plot(PP_10_strong_igraph)

### PP11
PP_11_igraph_prep <- PP_11_links[PP_11_links$size > 0,]
PP_11_igraph <- graph_from_data_frame(PP_11_igraph_prep, directed = TRUE)
PP_11_strong_igraph <- graph_from_data_frame(strong_11_links, directed = TRUE)
is_weighted(PP_11_igraph) # double-check
plot(PP_11_igraph)
is_weighted(PP_11_strong_igraph) # double-check
plot(PP_11_strong_igraph)

### PP12
PP_12_igraph_prep <- PP_12_links[PP_12_links$size > 0,]
PP_12_igraph <- graph_from_data_frame(PP_12_igraph_prep, directed = TRUE)
PP_12_strong_igraph <- graph_from_data_frame(strong_12_links, directed = TRUE)
is_weighted(PP_12_igraph) # double-check
plot(PP_12_igraph)
is_weighted(PP_12_strong_igraph) # double-check
plot(PP_12_strong_igraph)

### PP13
PP_13_igraph_prep <- PP_13_links[PP_13_links$size > 0,]
PP_13_igraph <- graph_from_data_frame(PP_13_igraph_prep, directed = TRUE)
PP_13_strong_igraph <- graph_from_data_frame(strong_13_links, directed = TRUE)
is_weighted(PP_13_igraph) # double-check
plot(PP_13_igraph)
is_weighted(PP_13_strong_igraph) # double-check
plot(PP_13_strong_igraph)

#### Binarised, unthresholded PECANS - non-strong ####
igraph_PECANs <- list(PP_1_igraph, PP_2_igraph, PP_3_igraph, PP_4_igraph, PP_5_igraph, PP_6_igraph, PP_7_igraph, PP_8_igraph, PP_9_igraph, PP_10_igraph, PP_11_igraph, PP_12_igraph, PP_13_igraph)
names(igraph_PECANs) <- paste0(1:13, '_PECAN_igraph')

#### Strong binarised PECANs ####
strong_igraph_PECANs <- list(PP_1_strong_igraph, PP_2_strong_igraph, PP_3_strong_igraph, PP_4_strong_igraph, PP_5_strong_igraph, PP_6_strong_igraph, PP_7_strong_igraph, PP_8_strong_igraph, PP_9_strong_igraph, PP_10_strong_igraph, PP_11_strong_igraph, PP_12_strong_igraph, PP_13_strong_igraph)
names(strong_igraph_PECANs) <- paste0('strong_', 1:13, '_PECAN_igraph')

############  Seeing which (binarised) edges exist in all individual PECANs  ###############
# Binarised versions of the individual matrices

binarising <- function(pecan_no_b) {
  ifelse(pecan_no_b[,3] > 0, 1, 0)
}
every_DSM_link <- list(PP_1_links, PP_2_links, PP_3_links, PP_4_links, PP_5_links, PP_6_links, PP_7_links, PP_8_links, PP_9_links, PP_10_links, PP_11_links, PP_12_links,PP_13_links)

binarised_every_DSM_link <- lapply(every_DSM_link, binarising)
# visually checking that all went well with some PECANs
check_bin_1 <- cbind(PP_1_links, binarised_every_DSM_link[[1]])
View(check_bin_1)
check_bin_6 <- cbind(PP_6_links, binarised_every_DSM_link[[6]])
View(check_bin_6)
check_bin_7 <- cbind(PP_7_links, binarised_every_DSM_link[[7]])
View(check_bin_7)

binarised_every_DSM_link <- as.data.frame(binarised_every_DSM_link)
colnames(binarised_every_DSM_link) <- paste0('PP', 1:13, '_nodes')

binarised_every_DSM_link$IncludedByAll <- apply(binarised_every_DSM_link, 1, function(row) all(row == row[1]))
# adding the two involved nodes for clarity and placing whether something is included by all earlier in df
binarised_every_DSM_link <- cbind(PP_1_links$source.nodeId, PP_1_links$target.nodeId, binarised_every_DSM_link$IncludedByAll, binarised_every_DSM_link)

View(binarised_every_DSM_link)
sum(binarised_every_DSM_link$`binarised_every_DSM_link$IncludedByAll`)
# In the above df, we can see which edges were included by everyone (rated >0 causal strength). # This is a repeated version of the initial lines of code, but not repeating on the binarised versions of the PECANs to prepare for subsequent analyses

############  Seeing which (binarised) edges exist in all STRONG individual PECANs  ###############
# adjusting binarising function (as earlier strong link lists cannot easily be used as they have differing lengths)
# This is a repeated version of the initial lines of code, but not repeating on the binarised versions of the PECANs to prepare for subsequent analyses
strong_binarising <- function(pecan_no_b) {
  ifelse(pecan_no_b[,3] >= 0.5, 1, 0)
}

strong_binarised_every_DSM_link <- lapply(every_DSM_link, strong_binarising)

strong_binarised_every_DSM_link <- as.data.frame(strong_binarised_every_DSM_link)
colnames(strong_binarised_every_DSM_link) <- paste0('PP', 1:13, '_nodes')
# visually checking that all went well with some PECANs
scheck_bin_1 <- cbind(PP_1_links, strong_binarised_every_DSM_link[[1]])
View(scheck_bin_1)
scheck_bin_6 <- cbind(PP_6_links, strong_binarised_every_DSM_link[[6]])
View(scheck_bin_6)
scheck_bin_7 <- cbind(PP_7_links, strong_binarised_every_DSM_link[[7]])
View(scheck_bin_7)

strong_binarised_every_DSM_link$IncludedByAll <- apply(strong_binarised_every_DSM_link, 1, function(row) all(row == row[1]))

# adding the two involved nodes for clarity and placing whether something is included by all earlier in df
strong_binarised_every_DSM_link <- cbind(PP_1_links$source.nodeId, PP_1_links$target.nodeId, strong_binarised_every_DSM_link$IncludedByAll, strong_binarised_every_DSM_link)

View(strong_binarised_every_DSM_link)
sum(strong_binarised_every_DSM_link$`strong_binarised_every_DSM_link$IncludedByAll`)

## Motifs analysed in the thresholded verison (strong PECANs) and once including all edges



############  Re-analysing feedback loops in different manner - igraph  ###############
# install.packages("remotes") 
# remotes::install_github("igraph/rigraph") # access to experimental functions required
# finding all feedback loops/cycles in the binarised version of the strong PECANs

  # PP1 STRONG loops
strongPP1loops <- simple_cycles(PP_1_strong_igraph, mode = "out")
strongPP1loops_df <- data.frame(sapply(strongPP1loops,c))
View(strongPP1loops_df)
nrow(strongPP1loops_df) # amount of loops

  # PP2 STRONG loops
strongPP2loops <- simple_cycles(PP_2_strong_igraph, mode = "out")
strongPP2loops_df <- data.frame(sapply(strongPP2loops,c))
nrow(strongPP2loops_df) # amount of loops

  # PP3 STRONG loops
strongPP3loops <- simple_cycles(PP_3_strong_igraph, mode = "out")
strongPP3loops_df <- data.frame(sapply(strongPP3loops,c))
nrow(strongPP3loops_df) # amount of loops

  # PP4 STRONG loops
strongPP4loops <- simple_cycles(PP_4_strong_igraph, mode = "out")
strongPP4loops_df <- data.frame(sapply(strongPP4loops,c))
nrow(strongPP4loops_df) # amount of loops

  # PP5 STRONG loops
strongPP5loops <- simple_cycles(PP_5_strong_igraph, mode = "out")
strongPP5loops_df <- data.frame(sapply(strongPP5loops,c))
nrow(strongPP5loops_df) # amount of loops

# PP6 STRONG loops
strongPP6loops <- simple_cycles(PP_6_strong_igraph, mode = "out")
strongPP6loops_df <- data.frame(sapply(strongPP6loops,c))
nrow(strongPP6loops_df) # amount of loops

# PP7 STRONG loops
strongPP7loops <- simple_cycles(PP_7_strong_igraph, mode = "out")
strongPP7loops_df <- data.frame(sapply(strongPP7loops,c))
nrow(strongPP7loops_df) # amount of loops

# PP8 STRONG loops
strongPP8loops <- simple_cycles(PP_8_strong_igraph, mode = "out")
strongPP8loops_df <- data.frame(sapply(strongPP8loops,c))
nrow(strongPP8loops_df) # amount of loops

# PP9 STRONG loops
strongPP9loops <- simple_cycles(PP_9_strong_igraph, mode = "out")
strongPP9loops_df <- data.frame(sapply(strongPP9loops,c))
nrow(strongPP9loops_df) # amount of loops

# PP10 STRONG loops
strongPP10loops <- simple_cycles(PP_10_strong_igraph, mode = "out")
strongPP10loops_df <- data.frame(sapply(strongPP10loops,c))
nrow(strongPP10loops_df) # amount of loops

# PP11 STRONG loops
strongPP11loops <- simple_cycles(PP_11_strong_igraph, mode = "out")
strongPP11loops_df <- data.frame(sapply(strongPP11loops,c))
nrow(strongPP11loops_df) # amount of loops

# PP12 STRONG loops
strongPP12loops <- simple_cycles(PP_12_strong_igraph, mode = "out")
strongPP12loops_df <- data.frame(sapply(strongPP12loops,c))
nrow(strongPP12loops_df) # amount of loops

# PP13 STRONG loops
strongPP13loops <- simple_cycles(PP_13_strong_igraph, mode = "out")
strongPP13loops_df <- data.frame(sapply(strongPP13loops,c))
nrow(strongPP13loops_df) # amount of loops

    ### combining these dataframes
strong_non0_loops_df <- gdata::cbindX(strongPP1loops_df, strongPP2loops_df, strongPP3loops_df, strongPP4loops_df, strongPP5loops_df, strongPP6loops_df, strongPP8loops_df, strongPP9loops_df, strongPP10loops_df, strongPP11loops_df, strongPP12loops_df, strongPP13loops_df) # not including PP7 because their PECAN had no loops

non0loops <- c(1,1,2,2,3,3,4,4,5,5,6,6,8,8,9,9,10,10,11,11,12,12,13,13)
options <- c("node", "edge")
pattern <- rep(options, length.out = 24)
colnames(strong_non0_loops_df) <- paste0(pattern, '_strongPP_', non0loops) # making loop df nicer

### We now have a large dataframe where we can look at all feedback loops across individual PECANs

  ###### Putting all individual feedback loop lengths into one df, to investigate descriptives ######

all_strong_loops_list <- list(strongPP1loops_df, strongPP2loops_df, strongPP3loops_df, strongPP4loops_df, strongPP5loops_df, strongPP6loops_df, strongPP7loops_df, strongPP8loops_df, strongPP9loops_df, strongPP10loops_df, strongPP11loops_df, strongPP12loops_df, strongPP13loops_df) # putting dataframes together in a list, so lapply can be used
strong_loop_lengths <- as.data.frame(lapply(all_strong_loops_list, nrow))
colnames(strong_loop_lengths) <- paste0('Strong_PP_length', 1:13)
View(strong_loop_lengths)
describe(as.numeric(strong_loop_lengths[1,]))
  # mean = 11072.31
  # sd = 24526.43
  # median = 2205

# PP6 has the second smallest amount of feedback loops (11, PP7 has 0 loops), and therefore we can compare if these loops are also present in the rest of the PECANs

strongPP6loops_df$vertices
as.numeric(strongPP6loops_df$vertices[[11]]) # by transforming into numeric, the node combinations can be checked across individual PECANs to see if any of these are shared. This code is a test to see what the result of the transformation would be.

# Checking to see which node names as associated with the node numbers igraph uses
V(PP_6_strong_igraph) # Noticing that the strong igraphs contain different Node IDs because not all contain all 9 symptoms (therefore, comparing nodes simply by their numerical ID won't work). Need to find a way to standardise the node IDs across the strong igraphs if we want to compare them through the above suggested numeric transformation. Another method can also be found (e.g. using the names)

# step 1: retrieve node names and values for each individual column. This has to be done per participant, as the node order and their numerical equivalents differ. Adjustments done in a new version of the dataframe with all strong loops (strong_non0_loops_df)
V(PP_1_strong_igraph)
# + 9/9 vertices, named, from 6120135:
 #  [1] rest   suic   sleep  conc   mood   fatig  worth  weight anhed 
  # loops have to be separated in order to be transformed, hence we need to use the individual strong dataframes

namesV1_strongPP1loops_df <- strongPP1loops_df %>% mutate(across(vertices, as.character)) # less than ideal solution, but will do with some cleaning up
namesV1_strongPP2loops_df <- strongPP2loops_df %>% mutate(across(vertices, as.character))
namesV1_strongPP3loops_df <- strongPP3loops_df %>% mutate(across(vertices, as.character))
namesV1_strongPP4loops_df <- strongPP4loops_df %>% mutate(across(vertices, as.character))
namesV1_strongPP5loops_df <- strongPP5loops_df %>% mutate(across(vertices, as.character))
namesV1_strongPP6loops_df <- strongPP6loops_df %>% mutate(across(vertices, as.character))
namesV1_strongPP8loops_df <- strongPP8loops_df %>% mutate(across(vertices, as.character))
namesV1_strongPP9loops_df <- strongPP9loops_df %>% mutate(across(vertices, as.character))
namesV1_strongPP10loops_df <- strongPP10loops_df %>% mutate(across(vertices, as.character))
namesV1_strongPP11loops_df <- strongPP11loops_df %>% mutate(across(vertices, as.character))
namesV1_strongPP12loops_df <- strongPP12loops_df %>% mutate(across(vertices, as.character))
namesV1_strongPP13loops_df <- strongPP13loops_df %>% mutate(across(vertices, as.character))

# combine into dataframe (only node columns so [,1])
names_nodes_strong_loops_df <- gdata::cbindX(namesV1_strongPP1loops_df, namesV1_strongPP2loops_df, namesV1_strongPP3loops_df, namesV1_strongPP4loops_df, namesV1_strongPP5loops_df, namesV1_strongPP6loops_df, namesV1_strongPP8loops_df, namesV1_strongPP9loops_df, namesV1_strongPP10loops_df, namesV1_strongPP11loops_df, namesV1_strongPP12loops_df, namesV1_strongPP12loops_df)
colnames(names_nodes_strong_loops_df) <- paste0(pattern, '_strongPP_', non0loops) # making loop df nicer

# Adjusting values which were not transformed per PP, as they may contain different numeric assignments
V(PP_1_strong_igraph)
  # + 9/9 vertices, named, from 6120135:
  # [1] rest   suic   sleep  conc   mood   fatig  worth  weight anhed 

names_nodes_strong_loops_df[592, 1] <- "c(mood = 5, fatig = 6)" # PP 1, 5:6

V(PP_2_strong_igraph)
  # + 9/9 vertices, named, from 6144071:
  # [1] rest   suic   sleep  conc   mood   fatig  worth  weight anhed 

names_nodes_strong_loops_df[1, 3] <- "c(rest = 1, suic = 2)" # PP 2, 1:2
names_nodes_strong_loops_df[2, 3] <- "c(rest = 1, suic = 2, sleep = 3)" # PP 2, 1:3
names_nodes_strong_loops_df[3, 3] <- "c(rest = 1, suic = 2, sleep = 3, conc = 4)" # PP 2, 1:4
names_nodes_strong_loops_df[4, 3] <- "c(rest = 1, suic = 2, sleep = 3, conc = 4, mood = 5)" # PP 2, 1:5
names_nodes_strong_loops_df[5, 3] <- "c(rest = 1, suic = 2, sleep = 3, conc = 4, mood = 5, fatig = 6)" # PP 2, 1:6
names_nodes_strong_loops_df[6, 3] <- "c(rest = 1, suic = 2, sleep = 3, conc = 4, mood = 5, fatig = 6, worth = 7)" # PP 2, 1:7
names_nodes_strong_loops_df[7, 3] <- "c(rest = 1, suic = 2, sleep = 3, conc = 4, mood = 5, fatig = 6, worth = 7, weight = 8)" # PP 2, 1:8
names_nodes_strong_loops_df[8, 3] <- "c(rest = 1, suic = 2, sleep = 3, conc = 4, mood = 5, fatig = 6, worth = 7, weight = 8, anhed = 9)" # PP 2, 1:9
names_nodes_strong_loops_df[77373, 3] <- "c(suic = 2, sleep = 3)"# PP2, 2:3
names_nodes_strong_loops_df[77374, 3] <- "c(suic = 2, sleep = 3, conc = 4)"# PP2, 2:4
names_nodes_strong_loops_df[77375, 3] <- "c(suic = 2, sleep = 3, conc = 4, mood = 5)"# PP2, 2:5
names_nodes_strong_loops_df[77376, 3] <- "c(suic = 2, sleep = 3, conc = 4, mood = 5, fatig = 6)"# PP2, 2:6
names_nodes_strong_loops_df[77377, 3] <- "c(rest = 1, suic = 2, sleep = 3, conc = 4, mood = 5, fatig = 6, worth = 7)"# PP2, 2:7
names_nodes_strong_loops_df[77378, 3] <- "c(rest = 1, suic = 2, sleep = 3, conc = 4, mood = 5, fatig = 6, worth = 7, weight = 8)"# PP2, 2:8
names_nodes_strong_loops_df[77379, 3] <- "c(suic = 2, sleep = 3, conc = 4, mood = 5, fatig = 6, worth = 7, weight = 8, anhed = 9)"# PP2, 2:9
names_nodes_strong_loops_df[86652, 3] <- "c(sleep = 3, conc = 4, mood = 5)"# PP2, 3:5
names_nodes_strong_loops_df[86653, 3] <- "c(sleep = 3, conc = 4, mood = 5, fatig = 6)"# PP2, 3:6
names_nodes_strong_loops_df[86654, 3] <- "c(sleep = 3, conc = 4, mood = 5, fatig = 6, worth = 7)"# PP2, 3:7
names_nodes_strong_loops_df[86655, 3] <- "c(sleep = 3, conc = 4, mood = 5, fatig = 6, worth = 7, weight = 8)"# PP2, 3:8
names_nodes_strong_loops_df[86656, 3] <- "c(sleep = 3, conc = 4, mood = 5, fatig = 6, worth = 7, weight = 8, anhed = 9)"# PP2, 3:9
names_nodes_strong_loops_df[87809, 3] <- "c(conc = 4, mood = 5)"# PP2, 4:5
names_nodes_strong_loops_df[87810, 3] <- "c(conc = 4, mood = 5, fatig = 6)"# PP2, 4:6
names_nodes_strong_loops_df[87811, 3] <- "c(conc = 4, mood = 5, fatig = 6, worth = 7)"# PP2, 4:7
names_nodes_strong_loops_df[87812, 3] <- "c(conc = 4, mood = 5, fatig = 6, worth = 7, weight = 8)"# PP2, 4:8
names_nodes_strong_loops_df[87813, 3] <- "c(conc = 4, mood = 5, fatig = 6, worth = 7, weight = 8, anhed = 9)"# PP2, 4:9
names_nodes_strong_loops_df[88069, 3] <- "c(mood = 5, fatig = 6)"# PP2, 5:6
names_nodes_strong_loops_df[88070, 3] <- "c(mood = 5, fatig = 6, worth = 7)"# PP2, 5:7
names_nodes_strong_loops_df[88071, 3] <- "c(mood = 5, fatig = 6, worth = 7, weight = 8)"# PP2, 5:8
names_nodes_strong_loops_df[88072, 3] <- "c(mood = 5, fatig = 6, worth = 7, weight = 8, anhed = 9)"# PP2, 5:9
names_nodes_strong_loops_df[88133, 3] <- "c(fatig = 6, worth = 7)"# PP2, 6:7
names_nodes_strong_loops_df[88134, 3] <- "c(fatig = 6, worth = 7, weight = 8)"# PP2, 6:8
names_nodes_strong_loops_df[88135, 3] <- "c(fatig = 6, worth = 7, weight = 8, anhed = 9)"# PP2, 6:9
names_nodes_strong_loops_df[88148, 3] <- "c(worth = 7, weight = 8)"# PP2, 7:8
names_nodes_strong_loops_df[88149, 3] <- "c(worth = 7, weight = 8, anhed = 9)"# PP2, 7:9
names_nodes_strong_loops_df[88152, 3] <- "c(weight = 8, anhed = 9)"# PP2, 8:9

V(PP_3_strong_igraph)
  #+ 9/9 vertices, named, from 617d3dc:
  # [1] rest   suic   sleep  conc   mood   fatig  worth  weight anhed 

names_nodes_strong_loops_df[12106, 5] <- "c(suic = 2, sleep = 3)"# PP3, 2:3
names_nodes_strong_loops_df[12107, 5] <- "c(suic = 2, sleep = 3, conc = 4, mood = 5)"# PP3, 2:5
names_nodes_strong_loops_df[12108, 5] <- "c(suic = 2, sleep = 3, conc = 4, mood = 5, fatig = 6)"# PP3, 2:6
names_nodes_strong_loops_df[14374, 5] <- "c(sleep = 3, conc = 4)"# PP3, 3:4
names_nodes_strong_loops_df[14375, 5] <- "c(sleep = 3, conc = 4, mood = 5)"# PP3, 3:5
names_nodes_strong_loops_df[14376, 5] <- "c(sleep = 3, conc = 4, mood = 5, fatig = 6)"# PP3, 3:6
names_nodes_strong_loops_df[14862, 5] <- "c(conc = 4, mood = 5)"# PP3, 4:5
names_nodes_strong_loops_df[14863, 5] <- "c(conc = 4, mood = 5, fatig = 6)"# PP3, 4:6
names_nodes_strong_loops_df[14919, 5] <- "c(mood = 5, fatig = 6)"# PP3, 5:6
names_nodes_strong_loops_df[14935, 5] <- "c(worth = 7, weight = 8)"# PP3, 7:8

V(PP_4_strong_igraph)
  # + 9/9 vertices, named, from 61a718d:
  # [1] rest   sleep  conc   mood   fatig  worth  weight anhed  suic

names_nodes_strong_loops_df[21, 7] <- "c(sleep = 2, conc = 3)"# PP4, 2:3

V(PP_5_strong_igraph)
  # + 9/9 vertices, named, from 61d1985:
  # [1] rest   suic   sleep  conc   mood   fatig  worth  anhed  weight

names_nodes_strong_loops_df[1234, 9] <- "c(suic = 2, sleep = 3)"# PP5, 2:3
names_nodes_strong_loops_df[1235, 9] <- "c(suic = 2, sleep = 3, conc = 4)"# PP5, 2:4
names_nodes_strong_loops_df[1236, 9] <- "c(suic = 2, sleep = 3, conc = 4, mood = 5)"# PP5, 2:5
names_nodes_strong_loops_df[1237, 9] <- "c(suic = 2, sleep = 3, conc = 4, mood = 5, fatig = 6)"# PP5, 2:6
names_nodes_strong_loops_df[2222, 9] <- "c(sleep = 3, conc = 4)"# PP5, 3:4
names_nodes_strong_loops_df[2223, 9] <- "c(sleep = 3, conc = 4, mood = 5)"# PP5, 3:5
names_nodes_strong_loops_df[2224, 9] <- "c(sleep = 3, conc = 4, mood = 5, fatig = 6)"# PP5, 3:6
names_nodes_strong_loops_df[2325, 9] <- "c(conc = 4, mood = 5)"# PP5, 4:5
names_nodes_strong_loops_df[2326, 9] <- "c(conc = 4, mood = 5, fatig = 6)"# PP5, 4:6
names_nodes_strong_loops_df[2378, 9] <- "c(mood = 5, fatig = 6)"# PP5, 5:6
names_nodes_strong_loops_df[2392, 9] <- "c(worth = 7, anhed = 8)"# PP5, 7:8

V(PP_6_strong_igraph)
  # + 8/8 vertices, named, from 61f43fb:
  # [1] rest   sleep  conc   mood   fatig  worth  anhed  weight
names_nodes_strong_loops_df[1,11] <- "c(rest = 1, sleep = 2)"# PP6, 1:2
    # Remember that pp7 is not included, as they has no loops
V(PP_8_strong_igraph)
  # + 9/9 vertices, named, from 622c903:
  # [1] rest   suic   sleep  conc   mood   fatig  worth  weight anhed 
names_nodes_strong_loops_df[1,13] <- "c(rest = 1, suic = 2)"# PP8, 1:2
names_nodes_strong_loops_df[2,13] <- "c(rest = 1, suic = 2, sleep = 3)"# PP8, 1:3
names_nodes_strong_loops_df[3,13] <- "c(rest = 1, suic = 2, sleep = 3, conc = 4, mood = 5)"# PP8, 1:5
names_nodes_strong_loops_df[4,13] <- "c(rest = 1, suic = 2, sleep = 3, conc = 4, mood = 5)"# PP8, 1:6
names_nodes_strong_loops_df[1662,13] <- "c(suic = 2, sleep = 3)"# PP8, 2:3
names_nodes_strong_loops_df[1663,13] <- "c(suic = 2, sleep = 3, conc = 4, mood = 5)"# PP8, 2:5
names_nodes_strong_loops_df[2190,13] <- "c(sleep = 3, conc = 4, mood = 5)"# PP8, 3:5
names_nodes_strong_loops_df[2321,13] <- "c(conc = 4, mood = 5)"# PP8, 4:5
names_nodes_strong_loops_df[2364,13] <- "c(mood = 5, fatig = 6)"# PP8, 5:6

V(PP_9_strong_igraph)
  # + 9/9 vertices, named, from 6249e21:
  # [1] rest   suic   sleep  conc   mood   fatig  worth  weight anhed
names_nodes_strong_loops_df[1,15] <- "c(rest = 1, suic = 2, sleep = 3)"# PP9, 1:3
names_nodes_strong_loops_df[2,15] <- "c(rest = 1, suic = 2, sleep = 3, conc = 4)"# PP9, 1:4
names_nodes_strong_loops_df[4228,15] <- "c(suic = 2, sleep = 3)"# PP9, 2:3
names_nodes_strong_loops_df[4842,15] <- "c(sleep = 3, conc = 4)"# PP9, 3:4
names_nodes_strong_loops_df[5287,15] <- "c(mood = 5, fatig = 6)"# PP9, 5:6
names_nodes_strong_loops_df[5288,15] <- "c(mood = 5, fatig = 6, worth = 7)"# PP9, 5:7
names_nodes_strong_loops_df[5332,15] <- "c(fatig = 6, worth = 7)"# PP9, 6:7

V(PP_10_strong_igraph)
# + 9/9 vertices, named, from 626e3e1:
#  [1] rest   suic   sleep  mood   fatig  worth  weight anhed  conc
names_nodes_strong_loops_df[31,17] <- "c(sleep = 3, mood = 4)"# PP10, 3:4
names_nodes_strong_loops_df[32,17] <- "c(sleep = 3, mood = 4, fatig = 5)"# PP10, 3:5
names_nodes_strong_loops_df[33,17] <- "c(sleep = 3, mood = 4, fatig = 5, worth = 6)"# PP10, 3:6
names_nodes_strong_loops_df[52,17] <- "c(mood = 4, fatig = 5)"# PP10, 4:5
names_nodes_strong_loops_df[53,17] <- "c(mood = 4, fatig = 5, worth = 6)"# PP10, 4:6
names_nodes_strong_loops_df[61,17] <- "c(fatig = 5, worth = 6)"# PP10, 5:6

V(PP_11_strong_igraph)
  # + 9/9 vertices, named, from 628d0f6:
  # [1] rest   suic   sleep  conc   mood   fatig  worth  weight anhed 
names_nodes_strong_loops_df[25443,19] <- "c(sleep = 3, conc = 4)"# PP11, 3:4
names_nodes_strong_loops_df[25444,19] <- "c(sleep = 3, conc = 4, mood = 5)"# PP11, 3:5
names_nodes_strong_loops_df[25445,19] <- "c(sleep = 3, conc = 4, mood = 5, fatig = 6)"# PP11, 3:6
names_nodes_strong_loops_df[25446,19] <- "c(sleep = 3, conc = 4, mood = 5, fatig = 6, worth = 7)"# PP11, 3:7
names_nodes_strong_loops_df[25447,19] <- "c(sleep = 3, conc = 4, mood = 5, fatig = 6, worth = 7, weight = 8)"# PP11, 3:8
names_nodes_strong_loops_df[25448,19] <- "c(sleep = 3, conc = 4, mood = 5, fatig = 6, worth = 7, weight = 8, anhed = 9)"# PP11, 3:9
names_nodes_strong_loops_df[27399,19] <- "c(conc = 4, mood = 5)"# PP11, 4:5
names_nodes_strong_loops_df[27400,19] <- "c(conc = 4, mood = 5, fatig = 6)"# PP11, 4:6
names_nodes_strong_loops_df[27401,19] <- "c(conc = 4, mood = 5, fatig = 6, worth = 7)"# PP11, 4:7
names_nodes_strong_loops_df[27402,19] <- "c(conc = 4, mood = 5, fatig = 6, worth = 7, weight = 8)"# PP11, 4:8
names_nodes_strong_loops_df[27403,19] <- "c(conc = 4, mood = 5, fatig = 6, worth = 7, weight = 8, anhed = 9)"# PP11, 4:9
names_nodes_strong_loops_df[27724,19] <- "c(mood = 5, fatig = 6)"# PP11, 5:6
names_nodes_strong_loops_df[27725,19] <- "c(mood = 5, fatig = 6, worth = 7)"# PP11, 5:7
names_nodes_strong_loops_df[27726,19] <- "c(mood = 5, fatig = 6, worth = 7, weight = 8)"# PP11, 5:8
names_nodes_strong_loops_df[27727,19] <- "c(mood = 5, fatig = 6, worth = 7, weight = 8, anhed = 9)"# PP11, 5:9
names_nodes_strong_loops_df[27788,19] <- "c(fatig = 6, worth = 7)"# PP11, 6:7
names_nodes_strong_loops_df[27789,19] <- "c(fatig = 6, worth = 7, weight = 8)"# PP11, 6:8
names_nodes_strong_loops_df[27790,19] <- "c(fatig = 6, worth = 7, weight = 8, anhed = 9)"# PP11, 6:9
names_nodes_strong_loops_df[27803,19] <- "c(worth = 7, weight = 8)"# PP11, 7:8
names_nodes_strong_loops_df[27804,19] <- "c(worth = 7, weight = 8, anhed = 9)"# PP11, 7:9
names_nodes_strong_loops_df[27807,19] <- "c(weight = 8, anhed = 9)"# PP11, 8:9

V(PP_12_strong_igraph)
  # + 8/8 vertices, named, from 62b0dcc:
  # [1] suic  sleep conc  mood  fatig anhed worth rest 
names_nodes_strong_loops_df[1,21] <- "c(sleep = 2, conc = 3)"# PP12, 2:3

V(PP_13_strong_igraph)
  # + 9/9 vertices, named, from 62d1931:
  #  [1] rest   suic   sleep  conc   mood   fatig  worth  weight anhed 
names_nodes_strong_loops_df[1,23] <- "c(suic = 2, sleep = 3)"# PP13, 2:3

#### Now that all values are recoded into characther string with node names, excessive elements thereof can be removed so we are left only with node names and commas in between. This will facilitate comparison

  # removing all excessive text

names_nodes_strong_loops_df[] <- data.frame(lapply(names_nodes_strong_loops_df, gsub, pattern = "c(", replacement = "", fixed = TRUE))
names_nodes_strong_loops_df[] <- data.frame(lapply(names_nodes_strong_loops_df, gsub, pattern = ")", replacement = "", fixed = TRUE))
names_nodes_strong_loops_df[] <- data.frame(lapply(names_nodes_strong_loops_df, gsub, pattern = " = 1", replacement = "", fixed = TRUE))
names_nodes_strong_loops_df[] <- data.frame(lapply(names_nodes_strong_loops_df, gsub, pattern = " = 2", replacement = "", fixed = TRUE))
names_nodes_strong_loops_df[] <- data.frame(lapply(names_nodes_strong_loops_df, gsub, pattern = " = 3", replacement = "", fixed = TRUE))
names_nodes_strong_loops_df[] <- data.frame(lapply(names_nodes_strong_loops_df, gsub, pattern = " = 4", replacement = "", fixed = TRUE))
names_nodes_strong_loops_df[] <- data.frame(lapply(names_nodes_strong_loops_df, gsub, pattern = " = 5", replacement = "", fixed = TRUE))
names_nodes_strong_loops_df[] <- data.frame(lapply(names_nodes_strong_loops_df, gsub, pattern = " = 6", replacement = "", fixed = TRUE))
names_nodes_strong_loops_df[] <- data.frame(lapply(names_nodes_strong_loops_df, gsub, pattern = " = 7", replacement = "", fixed = TRUE))
names_nodes_strong_loops_df[] <- data.frame(lapply(names_nodes_strong_loops_df, gsub, pattern = " = 8", replacement = "", fixed = TRUE))
names_nodes_strong_loops_df[] <- data.frame(lapply(names_nodes_strong_loops_df, gsub, pattern = " = 9", replacement = "", fixed = TRUE))

# Storing nodes and edges information separately, as the focus for feedback loops/cycles will be on the nodes
Names_nodes_strong_loops_df <- names_nodes_strong_loops_df |> select(starts_with("node"))
Names_edges_strong_loops_df <- names_nodes_strong_loops_df |> select(starts_with("edge"))

# Finding how many times each loop occurs across all participants, and sorting by frequency
all_text_nodes_loops <- unlist(Names_nodes_strong_loops_df, use.names = FALSE)
count_all_text_nodes_loops <- sort(table(all_text_nodes_loops), decreasing = TRUE)
View(count_all_text_nodes_loops)
length(count_all_text_nodes_loops) # Across all participants, 101132 total loops
df_count_all_text_nodes_loops <- as.data.frame(count_all_text_nodes_loops) 
sum(df_count_all_text_nodes_loops$Freq == 1) # 68806 loops only occur once

########################################
# Also look into simple_cycles of aggregated & aetiological PECANs
# Strong versions thereof
g_aggr <- graph_from_data_frame(Strong_links_aggr, directed = TRUE)
strongAgreggatedloops <- simple_cycles(g_aggr, mode = "out")
strongAggregatedloops_df <- data.frame(sapply(strongAgreggatedloops,c))
nrow(strongAggregatedloops_df) # amount of loops, 146
V(g_aggr)
  # + 9/9 vertices, named, from bcc1a0d:
  # [1] suic   sleep  mood   fatig  worth  weight anhed  rest   conc 
names_strongAggregatedloops_df <- strongAggregatedloops_df %>% mutate(across(vertices, as.character)) # Transforming the values into characters names afer nodes, as in the above PECANs
names_strongAggregatedloops_df[68,1] <- "c(sleep = 2, mood = 3)" # 2:3
names_strongAggregatedloops_df[69,1] <- "c(sleep = 2, mood = 3, fatig = 4)" # 2:4
names_strongAggregatedloops_df[126,1] <- "c(mood = 3, fatig = 4)" # 3:4

# removing all excessive text

names_strongAggregatedloops_df[] <- data.frame(lapply(names_strongAggregatedloops_df, gsub, pattern = "c(", replacement = "", fixed = TRUE))
names_strongAggregatedloops_df[] <- data.frame(lapply(names_strongAggregatedloops_df, gsub, pattern = ")", replacement = "", fixed = TRUE))
names_strongAggregatedloops_df[] <- data.frame(lapply(names_strongAggregatedloops_df, gsub, pattern = " = 1", replacement = "", fixed = TRUE))
names_strongAggregatedloops_df[] <- data.frame(lapply(names_strongAggregatedloops_df, gsub, pattern = " = 2", replacement = "", fixed = TRUE))
names_strongAggregatedloops_df[] <- data.frame(lapply(names_strongAggregatedloops_df, gsub, pattern = " = 3", replacement = "", fixed = TRUE))
names_strongAggregatedloops_df[] <- data.frame(lapply(names_strongAggregatedloops_df, gsub, pattern = " = 4", replacement = "", fixed = TRUE))
names_strongAggregatedloops_df[] <- data.frame(lapply(names_strongAggregatedloops_df, gsub, pattern = " = 5", replacement = "", fixed = TRUE))
names_strongAggregatedloops_df[] <- data.frame(lapply(names_strongAggregatedloops_df, gsub, pattern = " = 6", replacement = "", fixed = TRUE))
names_strongAggregatedloops_df[] <- data.frame(lapply(names_strongAggregatedloops_df, gsub, pattern = " = 7", replacement = "", fixed = TRUE))
names_strongAggregatedloops_df[] <- data.frame(lapply(names_strongAggregatedloops_df, gsub, pattern = " = 8", replacement = "", fixed = TRUE))
names_strongAggregatedloops_df[] <- data.frame(lapply(names_strongAggregatedloops_df, gsub, pattern = " = 9", replacement = "", fixed = TRUE))

  # We can simply use the search function in the dataframe to see how often each symptom appears
  # mood = 118 entries
  # anhed = 118
  # suic = 67
  # sleep = 112
  # rest = 0
  # conc = 0
  # fatig = 115
  # worth = 111
  # weight = 45

  ############## Cognitive Aetiological PECAN
g_cog <- graph_from_data_frame(Strong_links_cog, directed = TRUE)
strongCognitiveloops <- simple_cycles(g_cog, mode = "out")
strongCognitiveloops_df <- data.frame(sapply(strongCognitiveloops,c))
nrow(strongCognitiveloops_df) # amount of loops, 279
V(g_cog)
  # + 9/9 vertices, named, from 83df498:
  # [1] rest   sleep  conc   mood   fatig  worth  weight anhed  suic 
  names_strongCognitiveloops_df <- strongCognitiveloops_df %>% mutate(across(vertices, as.character))
  # No need to transform any values manually

# removing all excessive text

  names_strongCognitiveloops_df[] <- data.frame(lapply(names_strongCognitiveloops_df, gsub, pattern = "c(", replacement = "", fixed = TRUE))
  names_strongCognitiveloops_df[] <- data.frame(lapply(names_strongCognitiveloops_df, gsub, pattern = ")", replacement = "", fixed = TRUE))
  names_strongCognitiveloops_df[] <- data.frame(lapply(names_strongCognitiveloops_df, gsub, pattern = " = 1", replacement = "", fixed = TRUE))
  names_strongCognitiveloops_df[] <- data.frame(lapply(names_strongCognitiveloops_df, gsub, pattern = " = 2", replacement = "", fixed = TRUE))
  names_strongCognitiveloops_df[] <- data.frame(lapply(names_strongCognitiveloops_df, gsub, pattern = " = 3", replacement = "", fixed = TRUE))
  names_strongCognitiveloops_df[] <- data.frame(lapply(names_strongCognitiveloops_df, gsub, pattern = " = 4", replacement = "", fixed = TRUE))
  names_strongCognitiveloops_df[] <- data.frame(lapply(names_strongCognitiveloops_df, gsub, pattern = " = 5", replacement = "", fixed = TRUE))
  names_strongCognitiveloops_df[] <- data.frame(lapply(names_strongCognitiveloops_df, gsub, pattern = " = 6", replacement = "", fixed = TRUE))
  names_strongCognitiveloops_df[] <- data.frame(lapply(names_strongCognitiveloops_df, gsub, pattern = " = 7", replacement = "", fixed = TRUE))
  names_strongCognitiveloops_df[] <- data.frame(lapply(names_strongCognitiveloops_df, gsub, pattern = " = 8", replacement = "", fixed = TRUE))
  names_strongCognitiveloops_df[] <- data.frame(lapply(names_strongCognitiveloops_df, gsub, pattern = " = 9", replacement = "", fixed = TRUE))

  # We can simply use the search function in the dataframe to see how often each symptom appears
  # mood = 193
  # anhed = 226
  # suic = 0
  # sleep = 242
  # rest = 76
  # conc = 151
  # fatig = 242
  # worth = 179
  # weight = 180

############## Behavioural Aetiological PECAN
   g_beh <- graph_from_data_frame(Strong_links_beh, directed = TRUE)
   strongBehaviouralloops <- simple_cycles(g_beh, mode = "out")
   strongBehaviouralloops_df <- data.frame(sapply(strongBehaviouralloops,c))
   nrow(strongBehaviouralloops_df) # amount of loops, 12
   V(g_beh)
   # + 9/9 vertices, named, from ef0427c:
   # [1] rest   sleep  mood   fatig  worth  weight anhed  conc   suic
   names_strongBehaviouralloops_df <- strongBehaviouralloops_df %>% mutate(across(vertices, as.character))
   names_strongBehaviouralloops_df[1,1] <- "c(sleep = 2, mood = 3)"# 2:3
   
   # removing all excessive text
   
   names_strongBehaviouralloops_df[] <- data.frame(lapply(names_strongBehaviouralloops_df, gsub, pattern = "c(", replacement = "", fixed = TRUE))
   names_strongBehaviouralloops_df[] <- data.frame(lapply(names_strongBehaviouralloops_df, gsub, pattern = ")", replacement = "", fixed = TRUE))
   names_strongBehaviouralloops_df[] <- data.frame(lapply(names_strongBehaviouralloops_df, gsub, pattern = " = 1", replacement = "", fixed = TRUE))
   names_strongBehaviouralloops_df[] <- data.frame(lapply(names_strongBehaviouralloops_df, gsub, pattern = " = 2", replacement = "", fixed = TRUE))
   names_strongBehaviouralloops_df[] <- data.frame(lapply(names_strongBehaviouralloops_df, gsub, pattern = " = 3", replacement = "", fixed = TRUE))
   names_strongBehaviouralloops_df[] <- data.frame(lapply(names_strongBehaviouralloops_df, gsub, pattern = " = 4", replacement = "", fixed = TRUE))
   names_strongBehaviouralloops_df[] <- data.frame(lapply(names_strongBehaviouralloops_df, gsub, pattern = " = 5", replacement = "", fixed = TRUE))
   names_strongBehaviouralloops_df[] <- data.frame(lapply(names_strongBehaviouralloops_df, gsub, pattern = " = 6", replacement = "", fixed = TRUE))
   names_strongBehaviouralloops_df[] <- data.frame(lapply(names_strongBehaviouralloops_df, gsub, pattern = " = 7", replacement = "", fixed = TRUE))
   names_strongBehaviouralloops_df[] <- data.frame(lapply(names_strongBehaviouralloops_df, gsub, pattern = " = 8", replacement = "", fixed = TRUE))
   names_strongBehaviouralloops_df[] <- data.frame(lapply(names_strongBehaviouralloops_df, gsub, pattern = " = 9", replacement = "", fixed = TRUE))
   
   # We can simply use the search function in the dataframe to see how often each symptom appears
   # mood = 10
   # anhed = 9
   # suic = 0
   # sleep = 6
   # rest = 0
   # conc = 0
   # fatig = 6
   # worth = 4
   # weight = 2
   
############## Psychodynamic Aetiological PECAN
   g_you <- graph_from_data_frame(Strong_links_you, directed = TRUE)
   strongPsychodynamicloops <- simple_cycles(g_you, mode = "out")
   strongPsychodynamicloops_df <- data.frame(sapply(strongPsychodynamicloops,c))
   nrow(strongPsychodynamicloops_df) # amount of loops, 409
   V(g_you)
   # + 9/9 vertices, named, from d1f7c2b:
   # [1] rest   suic   sleep  conc   mood   fatig  worth  anhed  weight
   names_strongPsychodynamicloops_df <- strongPsychodynamicloops_df %>% mutate(across(vertices, as.character))
   names_strongPsychodynamicloops_df[402,1] <- "c(mood = 5, fatig = 6)"# 5:6
   names_strongPsychodynamicloops_df[409,1] <- "c(worth = 7, anhed = 8)"# 7:8
      
   # removing all excessive text
   
   names_strongPsychodynamicloops_df[] <- data.frame(lapply(names_strongPsychodynamicloops_df, gsub, pattern = "c(", replacement = "", fixed = TRUE))
   names_strongPsychodynamicloops_df[] <- data.frame(lapply(names_strongPsychodynamicloops_df, gsub, pattern = ")", replacement = "", fixed = TRUE))
   names_strongPsychodynamicloops_df[] <- data.frame(lapply(names_strongPsychodynamicloops_df, gsub, pattern = " = 1", replacement = "", fixed = TRUE))
   names_strongPsychodynamicloops_df[] <- data.frame(lapply(names_strongPsychodynamicloops_df, gsub, pattern = " = 2", replacement = "", fixed = TRUE))
   names_strongPsychodynamicloops_df[] <- data.frame(lapply(names_strongPsychodynamicloops_df, gsub, pattern = " = 3", replacement = "", fixed = TRUE))
   names_strongPsychodynamicloops_df[] <- data.frame(lapply(names_strongPsychodynamicloops_df, gsub, pattern = " = 4", replacement = "", fixed = TRUE))
   names_strongPsychodynamicloops_df[] <- data.frame(lapply(names_strongPsychodynamicloops_df, gsub, pattern = " = 5", replacement = "", fixed = TRUE))
   names_strongPsychodynamicloops_df[] <- data.frame(lapply(names_strongPsychodynamicloops_df, gsub, pattern = " = 6", replacement = "", fixed = TRUE))
   names_strongPsychodynamicloops_df[] <- data.frame(lapply(names_strongPsychodynamicloops_df, gsub, pattern = " = 7", replacement = "", fixed = TRUE))
   names_strongPsychodynamicloops_df[] <- data.frame(lapply(names_strongPsychodynamicloops_df, gsub, pattern = " = 8", replacement = "", fixed = TRUE))
   names_strongPsychodynamicloops_df[] <- data.frame(lapply(names_strongPsychodynamicloops_df, gsub, pattern = " = 9", replacement = "", fixed = TRUE))

   
   # We can simply use the search function in the dataframe to see how often each symptom appears
   # mood = 343
   # anhed = 364
   # suic = 324
   # sleep = 338
   # rest = 161
   # conc = 227
   # fatig = 267
   # worth = 256
   # weight = 0

  
   