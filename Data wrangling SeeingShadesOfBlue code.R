## ----------------------------
##### PPs PECAN FILE WRANGLING
## ----------------------------
# Per participant
# Including the 9 MDD symptoms, and the 10 (in case of extra factor) in two versions
# E.g. PP1_links_full contains 10 nodes' links
# E.g. PP1_links contains the 9 DSM MDD symptoms
# E.g. PP1_nodes_full ...
# E.g. PP1_nodes ...
# Check working directory, so file can be drawn from there
setwd() ### Adjust based on where file is drawn from
###################################################
###################################################
library(jsonlite)
library(purrr)
library(dplyr)

PP_1 <- fromJSON(txt = 'pecan_data_pp1.json', flatten = TRUE)
# View(PP_1)
# if you receive an error related to the name of the file when loading in the .json file, make sure your working directory is set; this often clears up the error

# Notice how the file consists of both node and link dataframes
#####-------------------------
##### Cleaning json file & wrangling
# renaming dataframes to something more intelligible
PP_1_nodes <- PP_1$nodes
PP_1_links <- PP_1$links

# Viewing to check dataframes (optional)
# View(PP_1_nodes)
# View(PP_1_links)

# Cleaning df links to only include relevant info. Important to clean links first because you need to make sure the id's match the symptoms you will rename them as
PP_cleaning_links_1 <- PP_1_links[, -c(2:3, 5, 7)]
# changing the id names to the symptom names
# sombere-stemming = mood
# anhedonie = anhed
# veranderde-eetlust-gewichtsschommeling = weight
# veranderde-slaapduur = sleep
# lichamelijke-rusteloosheid = rest
# vermoeidheid = fatig
# gevoel-van-waardeloosheid-vermatig-schuldgevoel = worth
# verminderd-concentratievermogen = conc
# suicidaliteit = suic

# Explicit naming and coding as to avoid mislabelling when variables appear in different orders

PP_cleaning_links_1[PP_cleaning_links_1 == 'sombere-stemming'] <-
  'mood'
PP_cleaning_links_1[PP_cleaning_links_1 == 'anhedonie'] <-
  'anhed'
PP_cleaning_links_1[PP_cleaning_links_1 == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP_cleaning_links_1[PP_cleaning_links_1 == 'veranderde-slaapduur'] <-
  'sleep'
PP_cleaning_links_1[PP_cleaning_links_1 == 'lichamelijke-rusteloosheid'] <-
  'rest'
PP_cleaning_links_1[PP_cleaning_links_1 == 'vermoeidheid'] <-
  'fatig'
PP_cleaning_links_1[PP_cleaning_links_1 == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP_cleaning_links_1[PP_cleaning_links_1 == 'verminderd-concentratievermogen'] <-
  'conc'
PP_cleaning_links_1[PP_cleaning_links_1 == 'suicidaliteit'] <-
  'suic'
PP_cleaning_links_1[PP_cleaning_links_1 == 'custom-9']<- 'restr' # custom factor

#changing the column order
PP1_links_full <-
  PP_cleaning_links_1[c('source.nodeId', 'target.nodeId', 'size')]
# Links df includes edge strength, source and target node
# It also contains and extra factor added by participant: Gevoel van beperking van vrijheid (Feeling of restriction of freedom)

# bounding edge weights between 0 and 1
PP1_links_full$size <- PP1_links_full$size / 100
#View(PP1_links_full)
# checking link characteristics (can add or remove later)
summary(PP1_links_full$size)  # bounds need to be 0 and 1

PP1_links <- PP1_links_full[!grepl('restr', PP1_links_full$source.nodeId),]
PP1_links <- PP1_links[!grepl('restr', PP1_links$target.nodeId),]

#####-------------------------
# Node info
PP1_nodes_full <-
  PP_1_nodes[, c(1, 19)] # removing irrelevant columns
PP1_nodes_full[PP1_nodes_full == 'sombere-stemming'] <- 'mood'
PP1_nodes_full[PP1_nodes_full == 'anhedonie'] <- 'anhed'
PP1_nodes_full[PP1_nodes_full == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP1_nodes_full[PP1_nodes_full == 'veranderde-slaapduur'] <- 'sleep'
PP1_nodes_full[PP1_nodes_full == 'lichamelijke-rusteloosheid'] <- 'rest'
PP1_nodes_full[PP1_nodes_full == 'vermoeidheid'] <- 'fatig'
PP1_nodes_full[PP1_nodes_full == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP1_nodes_full[PP1_nodes_full == 'verminderd-concentratievermogen'] <-
  'conc'
PP1_nodes_full[PP1_nodes_full == 'suicidaliteit'] <- 'suic'
PP1_nodes_full[PP1_nodes_full == 'custom-9']<- 'restr' # custom factor

PP1_nodes_full_sizes <-
  PP1_nodes_full[, 2] / 10 # node sizes adjusted for visualisation

PP1_nodes <- PP1_nodes_full[!grepl('restr', PP1_nodes_full$nodeId),]

PP1_nodes_sizes <-  PP1_nodes[, 2] / 10

################################################### Code is repeated to wrangle/clean explicitly until end of document (where the last few lines create larger participant wide dataframes)
###### PARTICIPANT 2
###################################################

PP_2 <- fromJSON(txt = "pecan_data_pp2.json", flatten = TRUE)
# View(PP_2)

PP_2_nodes <- PP_2$nodes
PP_2_links <- PP_2$links

# No extra node added
# View(PP_2_nodes)
# View(PP_2_links)

# Cleaning df links to only include relevant info. Important to clean links first because you need to make sure the id's match the symptoms you will rename them as
PP_cleaning_links_2 <- PP_2_links[, -c(2:3, 5, 7)]
# Explicit naming and coding as to avoid mislabelling when variables appear in different orders

PP_cleaning_links_2[PP_cleaning_links_2 == 'sombere-stemming'] <-
  'mood'
PP_cleaning_links_2[PP_cleaning_links_2 == 'anhedonie'] <-
  'anhed'
PP_cleaning_links_2[PP_cleaning_links_2 == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP_cleaning_links_2[PP_cleaning_links_2 == 'veranderde-slaapduur'] <-
  'sleep'
PP_cleaning_links_2[PP_cleaning_links_2 == 'lichamelijke-rusteloosheid'] <-
  'rest'
PP_cleaning_links_2[PP_cleaning_links_2 == 'vermoeidheid'] <-
  'fatig'
PP_cleaning_links_2[PP_cleaning_links_2 == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP_cleaning_links_2[PP_cleaning_links_2 == 'verminderd-concentratievermogen'] <-
  'conc'
PP_cleaning_links_2[PP_cleaning_links_2 == 'suicidaliteit'] <-
  'suic'

#changing the column order
PP2_links <-
  PP_cleaning_links_2[c('source.nodeId', 'target.nodeId', 'size')]
# Links df includes edge strength, source and target node

# bounding edge weights between 0 and 1
PP2_links$size <- PP2_links$size / 100
# View(PP2_links)
# checking link characteristics (can add or remove later)
summary(PP2_links$size)  # bounds need to be 0 and 1

#####-------------------------
# Node info
PP2_nodes <-
  PP_2_nodes[, c(1, 19)] # removing irrelevant columns
PP2_nodes[PP2_nodes == 'sombere-stemming'] <- 'mood'
PP2_nodes[PP2_nodes == 'anhedonie'] <- 'anhed'
PP2_nodes[PP2_nodes == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP2_nodes[PP2_nodes == 'veranderde-slaapduur'] <- 'sleep'
PP2_nodes[PP2_nodes == 'lichamelijke-rusteloosheid'] <- 'rest'
PP2_nodes[PP2_nodes == 'vermoeidheid'] <- 'fatig'
PP2_nodes[PP2_nodes == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP2_nodes[PP2_nodes == 'verminderd-concentratievermogen'] <-
  'conc'
PP2_nodes[PP2_nodes == 'suicidaliteit'] <- 'suic'

PP2_nodes_sizes <-
  PP2_nodes[, 2] / 10 # node sizes adjusted for visualisation

###################################################
###### PARTICIPANT 3
###################################################

PP_3 <- fromJSON(txt = "pecan_data_pp3.json", flatten = TRUE)
# View(PP_3)

PP_3_nodes <- PP_3$nodes
PP_3_links <- PP_3$links

# No extra node added
# View(PP_3_nodes)
# View(PP_3_links)

# Cleaning df links to only include relevant info. Important to clean links first because you need to make sure the id's match the symptoms you will rename them as
PP_cleaning_links_3 <- PP_3_links[, -c(2:3, 5, 7)]
# Explicit naming and coding as to avoid mislabelling when variables appear in different orders

PP_cleaning_links_3[PP_cleaning_links_3 == 'sombere-stemming'] <-
  'mood'
PP_cleaning_links_3[PP_cleaning_links_3 == 'anhedonie'] <-
  'anhed'
PP_cleaning_links_3[PP_cleaning_links_3 == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP_cleaning_links_3[PP_cleaning_links_3 == 'veranderde-slaapduur'] <-
  'sleep'
PP_cleaning_links_3[PP_cleaning_links_3 == 'lichamelijke-rusteloosheid'] <-
  'rest'
PP_cleaning_links_3[PP_cleaning_links_3 == 'vermoeidheid'] <-
  'fatig'
PP_cleaning_links_3[PP_cleaning_links_3 == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP_cleaning_links_3[PP_cleaning_links_3 == 'verminderd-concentratievermogen'] <-
  'conc'
PP_cleaning_links_3[PP_cleaning_links_3 == 'suicidaliteit'] <-
  'suic'

#changing the column order
PP3_links <-
  PP_cleaning_links_3[c('source.nodeId', 'target.nodeId', 'size')]
# Links df includes edge strength, source and target node

# bounding edge weights between 0 and 1
PP3_links$size <- PP3_links$size / 100
# View(PP3_links)
# checking link characteristics (can add or remove later)
summary(PP3_links$size)  # bounds need to be 0 and 1

#####-------------------------
# Node info
PP3_nodes <-
  PP_3_nodes[, c(1, 19)] # removing irrelevant columns
PP3_nodes[PP3_nodes == 'sombere-stemming'] <- 'mood'
PP3_nodes[PP3_nodes == 'anhedonie'] <- 'anhed'
PP3_nodes[PP3_nodes == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP3_nodes[PP3_nodes == 'veranderde-slaapduur'] <- 'sleep'
PP3_nodes[PP3_nodes == 'lichamelijke-rusteloosheid'] <- 'rest'
PP3_nodes[PP3_nodes == 'vermoeidheid'] <- 'fatig'
PP3_nodes[PP3_nodes == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP3_nodes[PP3_nodes == 'verminderd-concentratievermogen'] <-
  'conc'
PP3_nodes[PP3_nodes == 'suicidaliteit'] <- 'suic'

PP3_nodes_sizes <-
  PP3_nodes[, 2] / 10 # node sizes adjusted for visualisation

###################################################
###### PARTICIPANT 4
###################################################

PP_4 <- fromJSON(txt = "pecan_data_pp4.json", flatten = TRUE)
# View(PP_4)

PP_4_nodes <- PP_4$nodes
PP_4_links <- PP_4$links

# No extra node added
# View(PP_4_nodes)
# View(PP_4_links)

# Cleaning df links to only include relevant info. Important to clean links first because you need to make sure the id's match the symptoms you will rename them as
PP_cleaning_links_4 <- PP_4_links[, -c(2:3, 5, 7)]
# Explicit naming and coding as to avoid mislabelling when variables appear in different orders

PP_cleaning_links_4[PP_cleaning_links_4 == 'sombere-stemming'] <-
  'mood'
PP_cleaning_links_4[PP_cleaning_links_4 == 'anhedonie'] <-
  'anhed'
PP_cleaning_links_4[PP_cleaning_links_4 == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP_cleaning_links_4[PP_cleaning_links_4 == 'veranderde-slaapduur'] <-
  'sleep'
PP_cleaning_links_4[PP_cleaning_links_4 == 'lichamelijke-rusteloosheid'] <-
  'rest'
PP_cleaning_links_4[PP_cleaning_links_4 == 'vermoeidheid'] <-
  'fatig'
PP_cleaning_links_4[PP_cleaning_links_4 == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP_cleaning_links_4[PP_cleaning_links_4 == 'verminderd-concentratievermogen'] <-
  'conc'
PP_cleaning_links_4[PP_cleaning_links_4 == 'suicidaliteit'] <-
  'suic'

#changing the column order
PP4_links <-
  PP_cleaning_links_4[c('source.nodeId', 'target.nodeId', 'size')]
# Links df includes edge strength, source and target node

# bounding edge weights between 0 and 1
PP4_links$size <- PP4_links$size / 100
# View(PP4_links)
# checking link characteristics (can add or remove later)
summary(PP4_links$size)  # bounds need to be 0 and 1

#####-------------------------
# Node info
PP4_nodes <-
  PP_4_nodes[, c(1, 19)] # removing irrelevant columns
PP4_nodes[PP4_nodes == 'sombere-stemming'] <- 'mood'
PP4_nodes[PP4_nodes == 'anhedonie'] <- 'anhed'
PP4_nodes[PP4_nodes == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP4_nodes[PP4_nodes == 'veranderde-slaapduur'] <- 'sleep'
PP4_nodes[PP4_nodes == 'lichamelijke-rusteloosheid'] <- 'rest'
PP4_nodes[PP4_nodes == 'vermoeidheid'] <- 'fatig'
PP4_nodes[PP4_nodes == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP4_nodes[PP4_nodes == 'verminderd-concentratievermogen'] <-
  'conc'
PP4_nodes[PP4_nodes == 'suicidaliteit'] <- 'suic'

PP4_nodes_sizes <-
  PP4_nodes[, 2] / 10 # node sizes adjusted for visualisation

###################################################
###### PARTICIPANT 5
###################################################

PP_5 <- fromJSON(txt = "pecan_data_pp5.json", flatten = TRUE)
# View(PP_5)

PP_5_nodes <- PP_5$nodes
PP_5_links <- PP_5$links

# Extra node added: Emotieregulatie (emotion regulation)
# View(PP_5_nodes)
# View(PP_5_links)

# Cleaning df links to only include relevant info. Important to clean links first because you need to make sure the id's match the symptoms you will rename them as
PP_cleaning_links_5 <- PP_5_links[, -c(2:3, 5, 7)]
# Explicit naming and coding as to avoid mislabelling when variables appear in different orders

PP_cleaning_links_5[PP_cleaning_links_5 == 'sombere-stemming'] <-
  'mood'
PP_cleaning_links_5[PP_cleaning_links_5 == 'anhedonie'] <-
  'anhed'
PP_cleaning_links_5[PP_cleaning_links_5 == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP_cleaning_links_5[PP_cleaning_links_5 == 'veranderde-slaapduur'] <-
  'sleep'
PP_cleaning_links_5[PP_cleaning_links_5 == 'lichamelijke-rusteloosheid'] <-
  'rest'
PP_cleaning_links_5[PP_cleaning_links_5 == 'vermoeidheid'] <-
  'fatig'
PP_cleaning_links_5[PP_cleaning_links_5 == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP_cleaning_links_5[PP_cleaning_links_5 == 'verminderd-concentratievermogen'] <-
  'conc'
PP_cleaning_links_5[PP_cleaning_links_5 == 'suicidaliteit'] <-
  'suic'
PP_cleaning_links_5[PP_cleaning_links_5 == 'custom-9']<- 'emo' # custom factor

#changing the column order
PP5_links_full <-
  PP_cleaning_links_5[c('source.nodeId', 'target.nodeId', 'size')]
# Links df includes edge strength, source and target node
# It also contains and extra factor added by participant: Emotieregulatie

# bounding edge weights between 0 and 1
PP5_links_full$size <- PP5_links_full$size / 100
#View(PP5_links_full)
# checking link characteristics (can add or remove later)
summary(PP5_links_full$size)  # bounds need to be 0 and 1

PP5_links <- PP5_links_full[!grepl('emo', PP5_links_full$source.nodeId),]
PP5_links <- PP5_links[!grepl('emo', PP5_links$target.nodeId),]

#####-------------------------
# Node info
PP5_nodes_full <-
  PP_5_nodes[, c(1, 19)] # removing irrelevant columns
PP5_nodes_full[PP5_nodes_full == 'sombere-stemming'] <- 'mood'
PP5_nodes_full[PP5_nodes_full == 'anhedonie'] <- 'anhed'
PP5_nodes_full[PP5_nodes_full == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP5_nodes_full[PP5_nodes_full == 'veranderde-slaapduur'] <- 'sleep'
PP5_nodes_full[PP5_nodes_full == 'lichamelijke-rusteloosheid'] <- 'rest'
PP5_nodes_full[PP5_nodes_full == 'vermoeidheid'] <- 'fatig'
PP5_nodes_full[PP5_nodes_full == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP5_nodes_full[PP5_nodes_full == 'verminderd-concentratievermogen'] <-
  'conc'
PP5_nodes_full[PP5_nodes_full == 'suicidaliteit'] <- 'suic'
PP5_nodes_full[PP5_nodes_full == 'custom-9']<- 'emo' # custom factor

PP5_nodes_full_sizes <-
  PP5_nodes_full[, 2] / 10 # node sizes adjusted for visualisation

PP5_nodes <- PP5_nodes_full[!grepl('emo', PP5_nodes_full$nodeId),]

PP5_nodes_sizes <-  PP5_nodes[, 2] / 10

###################################################
###### PARTICIPANT 6
###################################################

PP_6 <- fromJSON(txt = "pecan_data_pp6.json", flatten = TRUE)
# View(PP_6)

PP_6_nodes <- PP_6$nodes
PP_6_links <- PP_6$links

# No extra node added
# View(PP_6_nodes)
# View(PP_6_links)

# Cleaning df links to only include relevant info. Important to clean links first because you need to make sure the id's match the symptoms you will rename them as
PP_cleaning_links_6 <- PP_6_links[, -c(2:3, 5, 7)]
# Explicit naming and coding as to avoid mislabelling when variables appear in different orders

PP_cleaning_links_6[PP_cleaning_links_6 == 'sombere-stemming'] <-
  'mood'
PP_cleaning_links_6[PP_cleaning_links_6 == 'anhedonie'] <-
  'anhed'
PP_cleaning_links_6[PP_cleaning_links_6 == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP_cleaning_links_6[PP_cleaning_links_6 == 'veranderde-slaapduur'] <-
  'sleep'
PP_cleaning_links_6[PP_cleaning_links_6 == 'lichamelijke-rusteloosheid'] <-
  'rest'
PP_cleaning_links_6[PP_cleaning_links_6 == 'vermoeidheid'] <-
  'fatig'
PP_cleaning_links_6[PP_cleaning_links_6 == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP_cleaning_links_6[PP_cleaning_links_6 == 'verminderd-concentratievermogen'] <-
  'conc'
PP_cleaning_links_6[PP_cleaning_links_6 == 'suicidaliteit'] <-
  'suic'

#changing the column order
PP6_links <-
  PP_cleaning_links_6[c('source.nodeId', 'target.nodeId', 'size')]
# Links df includes edge strength, source and target node

# bounding edge weights between 0 and 1
PP6_links$size <- PP6_links$size / 100
# View(PP6_links)
# checking link characteristics (can add or remove later)
summary(PP6_links$size)  # bounds need to be 0 and 1

#####-------------------------
# Node info
PP6_nodes <-
  PP_6_nodes[, c(1, 19)] # removing irrelevant columns
PP6_nodes[PP6_nodes == 'sombere-stemming'] <- 'mood'
PP6_nodes[PP6_nodes == 'anhedonie'] <- 'anhed'
PP6_nodes[PP6_nodes == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP6_nodes[PP6_nodes == 'veranderde-slaapduur'] <- 'sleep'
PP6_nodes[PP6_nodes == 'lichamelijke-rusteloosheid'] <- 'rest'
PP6_nodes[PP6_nodes == 'vermoeidheid'] <- 'fatig'
PP6_nodes[PP6_nodes == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP6_nodes[PP6_nodes == 'verminderd-concentratievermogen'] <-
  'conc'
PP6_nodes[PP6_nodes == 'suicidaliteit'] <- 'suic'

PP6_nodes_sizes <-
  PP6_nodes[, 2] / 10 # node sizes adjusted for visualisation

###################################################
###### PARTICIPANT 7
###################################################

PP_7 <- fromJSON(txt = "pecan_data_pp7.json", flatten = TRUE)
# View(PP_7)

PP_7_nodes <- PP_7$nodes
PP_7_links <- PP_7$links

# Extra node added: internaliserende coping (Internalising coping)
# View(PP_7_nodes)
# View(PP_7_links)

# Cleaning df links to only include relevant info. Important to clean links first because you need to make sure the id's match the symptoms you will rename them as
PP_cleaning_links_7 <- PP_7_links[, -c(2:3, 5, 7)]
# Explicit naming and coding as to avoid mislabelling when variables appear in different orders

PP_cleaning_links_7[PP_cleaning_links_7 == 'sombere-stemming'] <-
  'mood'
PP_cleaning_links_7[PP_cleaning_links_7 == 'anhedonie'] <-
  'anhed'
PP_cleaning_links_7[PP_cleaning_links_7 == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP_cleaning_links_7[PP_cleaning_links_7 == 'veranderde-slaapduur'] <-
  'sleep'
PP_cleaning_links_7[PP_cleaning_links_7 == 'lichamelijke-rusteloosheid'] <-
  'rest'
PP_cleaning_links_7[PP_cleaning_links_7 == 'vermoeidheid'] <-
  'fatig'
PP_cleaning_links_7[PP_cleaning_links_7 == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP_cleaning_links_7[PP_cleaning_links_7 == 'verminderd-concentratievermogen'] <-
  'conc'
PP_cleaning_links_7[PP_cleaning_links_7 == 'suicidaliteit'] <-
  'suic'
PP_cleaning_links_7[PP_cleaning_links_7 == 'custom-9']<- 'cope' # custom factor

#changing the column order
PP7_links_full <-
  PP_cleaning_links_7[c('source.nodeId', 'target.nodeId', 'size')]
# Links df includes edge strength, source and target node
# It also contains and extra factor added by participant: Internalising coping

# bounding edge weights between 0 and 1
PP7_links_full$size <- PP7_links_full$size / 100
#View(PP7_links_full)
# checking link characteristics (can add or remove later)
summary(PP7_links_full$size)  # bounds need to be 0 and 1

PP7_links <- PP7_links_full[!grepl('cope', PP7_links_full$source.nodeId),]
PP7_links <- PP7_links[!grepl('cope', PP7_links$target.nodeId),]

#####-------------------------
# Node info
PP7_nodes_full <-
  PP_7_nodes[, c(1, 19)] # removing irrelevant columns
PP7_nodes_full[PP7_nodes_full == 'sombere-stemming'] <- 'mood'
PP7_nodes_full[PP7_nodes_full == 'anhedonie'] <- 'anhed'
PP7_nodes_full[PP7_nodes_full == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP7_nodes_full[PP7_nodes_full == 'veranderde-slaapduur'] <- 'sleep'
PP7_nodes_full[PP7_nodes_full == 'lichamelijke-rusteloosheid'] <- 'rest'
PP7_nodes_full[PP7_nodes_full == 'vermoeidheid'] <- 'fatig'
PP7_nodes_full[PP7_nodes_full == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP7_nodes_full[PP7_nodes_full == 'verminderd-concentratievermogen'] <-
  'conc'
PP7_nodes_full[PP7_nodes_full == 'suicidaliteit'] <- 'suic'
PP7_nodes_full[PP7_nodes_full == 'custom-9']<- 'cope' # custom factor

PP7_nodes_full_sizes <-
  PP7_nodes_full[, 2] / 10 # node sizes adjusted for visualisation

PP7_nodes <- PP7_nodes_full[!grepl('cope', PP7_nodes_full$nodeId),]

PP7_nodes_sizes <-  PP7_nodes[, 2] / 10

###################################################
###### PARTICIPANT 8
###################################################

PP_8 <- fromJSON(txt = "pecan_data_pp8.json", flatten = TRUE)
# View(PP_8)

PP_8_nodes <- PP_8$nodes
PP_8_links <- PP_8$links

# No extra node added
# View(PP_8_nodes)
# View(PP_8_links)

# Cleaning df links to only include relevant info. Important to clean links first because you need to make sure the id's match the symptoms you will rename them as
PP_cleaning_links_8 <- PP_8_links[, -c(2:3, 5, 7)]
# Explicit naming and coding as to avoid mislabelling when variables appear in different orders

PP_cleaning_links_8[PP_cleaning_links_8 == 'sombere-stemming'] <-
  'mood'
PP_cleaning_links_8[PP_cleaning_links_8 == 'anhedonie'] <-
  'anhed'
PP_cleaning_links_8[PP_cleaning_links_8 == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP_cleaning_links_8[PP_cleaning_links_8 == 'veranderde-slaapduur'] <-
  'sleep'
PP_cleaning_links_8[PP_cleaning_links_8 == 'lichamelijke-rusteloosheid'] <-
  'rest'
PP_cleaning_links_8[PP_cleaning_links_8 == 'vermoeidheid'] <-
  'fatig'
PP_cleaning_links_8[PP_cleaning_links_8 == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP_cleaning_links_8[PP_cleaning_links_8 == 'verminderd-concentratievermogen'] <-
  'conc'
PP_cleaning_links_8[PP_cleaning_links_8 == 'suicidaliteit'] <-
  'suic'

#changing the column order
PP8_links <-
  PP_cleaning_links_8[c('source.nodeId', 'target.nodeId', 'size')]
# Links df includes edge strength, source and target node

# bounding edge weights between 0 and 1
PP8_links$size <- PP8_links$size / 100
# View(PP8_links)
# checking link characteristics (can add or remove later)
summary(PP8_links$size)  # bounds need to be 0 and 1

#####-------------------------
# Node info
PP8_nodes <-
  PP_8_nodes[, c(1, 19)] # removing irrelevant columns
PP8_nodes[PP8_nodes == 'sombere-stemming'] <- 'mood'
PP8_nodes[PP8_nodes == 'anhedonie'] <- 'anhed'
PP8_nodes[PP8_nodes == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP8_nodes[PP8_nodes == 'veranderde-slaapduur'] <- 'sleep'
PP8_nodes[PP8_nodes == 'lichamelijke-rusteloosheid'] <- 'rest'
PP8_nodes[PP8_nodes == 'vermoeidheid'] <- 'fatig'
PP8_nodes[PP8_nodes == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP8_nodes[PP8_nodes == 'verminderd-concentratievermogen'] <-
  'conc'
PP8_nodes[PP8_nodes == 'suicidaliteit'] <- 'suic'

PP8_nodes_sizes <-
  PP8_nodes[, 2] / 10 # node sizes adjusted for visualisation

###################################################
###### PARTICIPANT 9
###################################################

PP_9 <- fromJSON(txt = "pecan_data_pp9.json", flatten = TRUE)
# View(PP_9)

PP_9_nodes <- PP_9$nodes
PP_9_links <- PP_9$links

# No extra node added
# View(PP_9_nodes)
# View(PP_9_links)

# Cleaning df links to only include relevant info. Important to clean links first because you need to make sure the id's match the symptoms you will rename them as
PP_cleaning_links_9 <- PP_9_links[, -c(2:3, 5, 7)]
# Explicit naming and coding as to avoid mislabelling when variables appear in different orders

PP_cleaning_links_9[PP_cleaning_links_9 == 'sombere-stemming'] <-
  'mood'
PP_cleaning_links_9[PP_cleaning_links_9 == 'anhedonie'] <-
  'anhed'
PP_cleaning_links_9[PP_cleaning_links_9 == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP_cleaning_links_9[PP_cleaning_links_9 == 'veranderde-slaapduur'] <-
  'sleep'
PP_cleaning_links_9[PP_cleaning_links_9 == 'lichamelijke-rusteloosheid'] <-
  'rest'
PP_cleaning_links_9[PP_cleaning_links_9 == 'vermoeidheid'] <-
  'fatig'
PP_cleaning_links_9[PP_cleaning_links_9 == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP_cleaning_links_9[PP_cleaning_links_9 == 'verminderd-concentratievermogen'] <-
  'conc'
PP_cleaning_links_9[PP_cleaning_links_9 == 'suicidaliteit'] <-
  'suic'

#changing the column order
PP9_links <-
  PP_cleaning_links_9[c('source.nodeId', 'target.nodeId', 'size')]
# Links df includes edge strength, source and target node

# bounding edge weights between 0 and 1
PP9_links$size <- PP9_links$size / 100
# View(PP9_links)
# checking link characteristics (can add or remove later)
summary(PP9_links$size)  # bounds need to be 0 and 1

#####-------------------------
# Node info
PP9_nodes <-
  PP_9_nodes[, c(1, 19)] # removing irrelevant columns
PP9_nodes[PP9_nodes == 'sombere-stemming'] <- 'mood'
PP9_nodes[PP9_nodes == 'anhedonie'] <- 'anhed'
PP9_nodes[PP9_nodes == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP9_nodes[PP9_nodes == 'veranderde-slaapduur'] <- 'sleep'
PP9_nodes[PP9_nodes == 'lichamelijke-rusteloosheid'] <- 'rest'
PP9_nodes[PP9_nodes == 'vermoeidheid'] <- 'fatig'
PP9_nodes[PP9_nodes == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP9_nodes[PP9_nodes == 'verminderd-concentratievermogen'] <-
  'conc'
PP9_nodes[PP9_nodes == 'suicidaliteit'] <- 'suic'

PP9_nodes_sizes <-
  PP9_nodes[, 2] / 10 # node sizes adjusted for visualisation

###################################################
###### PARTICIPANT 10
###################################################

PP_10 <- fromJSON(txt = "pecan_data_pp10.json", flatten = TRUE)
# View(PP_10)

PP_10_nodes <- PP_10$nodes
PP_10_links <- PP_10$links

# No extra node added
# View(PP_10_nodes)
# View(PP_10_links)

# Cleaning df links to only include relevant info. Important to clean links first because you need to make sure the id's match the symptoms you will rename them as
PP_cleaning_links_10 <- PP_10_links[, -c(2:3, 5, 7)]
# Explicit naming and coding as to avoid mislabelling when variables appear in different orders

PP_cleaning_links_10[PP_cleaning_links_10 == 'sombere-stemming'] <-
  'mood'
PP_cleaning_links_10[PP_cleaning_links_10 == 'anhedonie'] <-
  'anhed'
PP_cleaning_links_10[PP_cleaning_links_10 == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP_cleaning_links_10[PP_cleaning_links_10 == 'veranderde-slaapduur'] <-
  'sleep'
PP_cleaning_links_10[PP_cleaning_links_10 == 'lichamelijke-rusteloosheid'] <-
  'rest'
PP_cleaning_links_10[PP_cleaning_links_10 == 'vermoeidheid'] <-
  'fatig'
PP_cleaning_links_10[PP_cleaning_links_10 == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP_cleaning_links_10[PP_cleaning_links_10 == 'verminderd-concentratievermogen'] <-
  'conc'
PP_cleaning_links_10[PP_cleaning_links_10 == 'suicidaliteit'] <-
  'suic'

#changing the column order
PP10_links <-
  PP_cleaning_links_10[c('source.nodeId', 'target.nodeId', 'size')]
# Links df includes edge strength, source and target node

# bounding edge weights between 0 and 1
PP10_links$size <- PP10_links$size / 100
# View(PP10_links)
# checking link characteristics (can add or remove later)
summary(PP10_links$size)  # bounds need to be 0 and 1

#####-------------------------
# Node info
PP10_nodes <-
  PP_10_nodes[, c(1, 19)] # removing irrelevant columns
PP10_nodes[PP10_nodes == 'sombere-stemming'] <- 'mood'
PP10_nodes[PP10_nodes == 'anhedonie'] <- 'anhed'
PP10_nodes[PP10_nodes == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP10_nodes[PP10_nodes == 'veranderde-slaapduur'] <- 'sleep'
PP10_nodes[PP10_nodes == 'lichamelijke-rusteloosheid'] <- 'rest'
PP10_nodes[PP10_nodes == 'vermoeidheid'] <- 'fatig'
PP10_nodes[PP10_nodes == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP10_nodes[PP10_nodes == 'verminderd-concentratievermogen'] <-
  'conc'
PP10_nodes[PP10_nodes == 'suicidaliteit'] <- 'suic'

PP10_nodes_sizes <-
  PP10_nodes[, 2] / 10 # node sizes adjusted for visualisation

###################################################
###### PARTICIPANT 11
###################################################

PP_11 <- fromJSON(txt = "pecan_data_pp11.json", flatten = TRUE)
# View(PP_11)

PP_11_nodes <- PP_11$nodes
PP_11_links <- PP_11$links

# No extra node added
# View(PP_11_nodes)
# View(PP_11_links)

# Cleaning df links to only include relevant info. Important to clean links first because you need to make sure the id's match the symptoms you will rename them as
PP_cleaning_links_11 <- PP_11_links[, -c(2:3, 5, 7)]
# Explicit naming and coding as to avoid mislabelling when variables appear in different orders

PP_cleaning_links_11[PP_cleaning_links_11 == 'sombere-stemming'] <-
  'mood'
PP_cleaning_links_11[PP_cleaning_links_11 == 'anhedonie'] <-
  'anhed'
PP_cleaning_links_11[PP_cleaning_links_11 == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP_cleaning_links_11[PP_cleaning_links_11 == 'veranderde-slaapduur'] <-
  'sleep'
PP_cleaning_links_11[PP_cleaning_links_11 == 'lichamelijke-rusteloosheid'] <-
  'rest'
PP_cleaning_links_11[PP_cleaning_links_11 == 'vermoeidheid'] <-
  'fatig'
PP_cleaning_links_11[PP_cleaning_links_11 == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP_cleaning_links_11[PP_cleaning_links_11 == 'verminderd-concentratievermogen'] <-
  'conc'
PP_cleaning_links_11[PP_cleaning_links_11 == 'suicidaliteit'] <-
  'suic'

#changing the column order
PP11_links <-
  PP_cleaning_links_11[c('source.nodeId', 'target.nodeId', 'size')]
# Links df includes edge strength, source and target node

# bounding edge weights between 0 and 1
PP11_links$size <- PP11_links$size / 100
# View(PP11_links)
# checking link characteristics (can add or remove later)
summary(PP11_links$size)  # bounds need to be 0 and 1

#####-------------------------
# Node info
PP11_nodes <-
  PP_11_nodes[, c(1, 19)] # removing irrelevant columns
PP11_nodes[PP11_nodes == 'sombere-stemming'] <- 'mood'
PP11_nodes[PP11_nodes == 'anhedonie'] <- 'anhed'
PP11_nodes[PP11_nodes == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP11_nodes[PP11_nodes == 'veranderde-slaapduur'] <- 'sleep'
PP11_nodes[PP11_nodes == 'lichamelijke-rusteloosheid'] <- 'rest'
PP11_nodes[PP11_nodes == 'vermoeidheid'] <- 'fatig'
PP11_nodes[PP11_nodes == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP11_nodes[PP11_nodes == 'verminderd-concentratievermogen'] <-
  'conc'
PP11_nodes[PP11_nodes == 'suicidaliteit'] <- 'suic'

PP11_nodes_sizes <-
  PP11_nodes[, 2] / 10 # node sizes adjusted for visualisation

###################################################
###### PARTICIPANT 12
###################################################

PP_12 <- fromJSON(txt = "pecan_data_pp12.json", flatten = TRUE)
# View(PP_12)

PP_12_nodes <- PP_12$nodes
PP_12_links <- PP_12$links

# No extra node added
# View(PP_12_nodes)
# View(PP_12_links)

# Cleaning df links to only include relevant info. Important to clean links first because you need to make sure the id's match the symptoms you will rename them as
PP_cleaning_links_12 <- PP_12_links[, -c(2:3, 5, 7)]
# Explicit naming and coding as to avoid mislabelling when variables appear in different orders

PP_cleaning_links_12[PP_cleaning_links_12 == 'sombere-stemming'] <-
  'mood'
PP_cleaning_links_12[PP_cleaning_links_12 == 'anhedonie'] <-
  'anhed'
PP_cleaning_links_12[PP_cleaning_links_12 == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP_cleaning_links_12[PP_cleaning_links_12 == 'veranderde-slaapduur'] <-
  'sleep'
PP_cleaning_links_12[PP_cleaning_links_12 == 'lichamelijke-rusteloosheid'] <-
  'rest'
PP_cleaning_links_12[PP_cleaning_links_12 == 'vermoeidheid'] <-
  'fatig'
PP_cleaning_links_12[PP_cleaning_links_12 == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP_cleaning_links_12[PP_cleaning_links_12 == 'verminderd-concentratievermogen'] <-
  'conc'
PP_cleaning_links_12[PP_cleaning_links_12 == 'suicidaliteit'] <-
  'suic'

#changing the column order
PP12_links <-
  PP_cleaning_links_12[c('source.nodeId', 'target.nodeId', 'size')]
# Links df includes edge strength, source and target node

# bounding edge weights between 0 and 1
PP12_links$size <- PP12_links$size / 100
# View(PP12_links)
# checking link characteristics (can add or remove later)
summary(PP12_links$size)  # bounds need to be 0 and 1

#####-------------------------
# Node info
PP12_nodes <-
  PP_12_nodes[, c(1, 19)] # removing irrelevant columns
PP12_nodes[PP12_nodes == 'sombere-stemming'] <- 'mood'
PP12_nodes[PP12_nodes == 'anhedonie'] <- 'anhed'
PP12_nodes[PP12_nodes == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP12_nodes[PP12_nodes == 'veranderde-slaapduur'] <- 'sleep'
PP12_nodes[PP12_nodes == 'lichamelijke-rusteloosheid'] <- 'rest'
PP12_nodes[PP12_nodes == 'vermoeidheid'] <- 'fatig'
PP12_nodes[PP12_nodes == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP12_nodes[PP12_nodes == 'verminderd-concentratievermogen'] <-
  'conc'
PP12_nodes[PP12_nodes == 'suicidaliteit'] <- 'suic'

PP12_nodes_sizes <-
  PP12_nodes[, 2] / 10 # node sizes adjusted for visualisation

###################################################
###### PARTICIPANT 13
###################################################

PP_13 <- fromJSON(txt = "pecan_data_pp13.json", flatten = TRUE)
# View(PP_13)

PP_13_nodes <- PP_13$nodes
PP_13_links <- PP_13$links

# No extra node added
# View(PP_13_nodes)
# View(PP_13_links)

# Cleaning df links to only include relevant info. Important to clean links first because you need to make sure the id's match the symptoms you will rename them as
PP_cleaning_links_13 <- PP_13_links[, -c(2:3, 5, 7)]
# Explicit naming and coding as to avoid mislabelling when variables appear in different orders

PP_cleaning_links_13[PP_cleaning_links_13 == 'sombere-stemming'] <-
  'mood'
PP_cleaning_links_13[PP_cleaning_links_13 == 'anhedonie'] <-
  'anhed'
PP_cleaning_links_13[PP_cleaning_links_13 == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP_cleaning_links_13[PP_cleaning_links_13 == 'veranderde-slaapduur'] <-
  'sleep'
PP_cleaning_links_13[PP_cleaning_links_13 == 'lichamelijke-rusteloosheid'] <-
  'rest'
PP_cleaning_links_13[PP_cleaning_links_13 == 'vermoeidheid'] <-
  'fatig'
PP_cleaning_links_13[PP_cleaning_links_13 == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP_cleaning_links_13[PP_cleaning_links_13 == 'verminderd-concentratievermogen'] <-
  'conc'
PP_cleaning_links_13[PP_cleaning_links_13 == 'suicidaliteit'] <-
  'suic'

#changing the column order
PP13_links <-
  PP_cleaning_links_13[c('source.nodeId', 'target.nodeId', 'size')]
# Links df includes edge strength, source and target node

# bounding edge weights between 0 and 1
PP13_links$size <- PP13_links$size / 100
# View(PP13_links)
# checking link characteristics (can add or remove later)
summary(PP13_links$size)  # bounds need to be 0 and 1

#####-------------------------
# Node info
PP13_nodes <-
  PP_13_nodes[, c(1, 19)] # removing irrelevant columns
PP13_nodes[PP13_nodes == 'sombere-stemming'] <- 'mood'
PP13_nodes[PP13_nodes == 'anhedonie'] <- 'anhed'
PP13_nodes[PP13_nodes == 'veranderde-eetlust-gewichtsschommeling'] <-
  'weight'
PP13_nodes[PP13_nodes == 'veranderde-slaapduur'] <- 'sleep'
PP13_nodes[PP13_nodes == 'lichamelijke-rusteloosheid'] <- 'rest'
PP13_nodes[PP13_nodes == 'vermoeidheid'] <- 'fatig'
PP13_nodes[PP13_nodes == 'gevoel-van-waardeloosheid-vermatig-schuldgevoel'] <-
  'worth'
PP13_nodes[PP13_nodes == 'verminderd-concentratievermogen'] <-
  'conc'
PP13_nodes[PP13_nodes == 'suicidaliteit'] <- 'suic'

PP13_nodes_sizes <-
  PP13_nodes[, 2] / 10 # node sizes adjusted for visualisation

###################################################
###################################################

#Making sure the order of links is the same for all participants

all_link_names <- paste0('PP', 1:13, '_links')
all_links <- 
  mget(all_link_names) # list of all individual link dataframes
all_links_df <-
  purrr::reduce(all_links,
                dplyr::inner_join,
                by = c('source.nodeId', 'target.nodeId'))

# 9 Symptom PECANS, recreating participant dataframes, now all sorted identicalyl
PP_1_links <- all_links_df[, c(1:3)]
PP_2_links <- all_links_df[, c(1:2, 4)]
PP_3_links <- all_links_df[, c(1:2, 5)]
PP_4_links <- all_links_df[, c(1:2, 6)]
PP_5_links <- all_links_df[, c(1:2, 7)]
PP_6_links <- all_links_df[, c(1:2, 8)]
PP_7_links <- all_links_df[, c(1:2, 9)]
PP_8_links <- all_links_df[, c(1:2, 10)]
PP_9_links <- all_links_df[, c(1:2, 11)]
PP_10_links <- all_links_df[, c(1:2, 12)]
PP_11_links <- all_links_df[, c(1:2, 13)]
PP_12_links <- all_links_df[, c(1:2, 14)]
PP_13_links <- all_links_df[, c(1:2, 15)]

# Full networks for all pps; 1,5,7 have different networks
full_all_link_names1 <- 'PP1_links_full'
faln2 <- paste0('PP', c(2:4), '_links')
faln3 <- 'PP5_links_full'
faln4 <- paste0('PP', 6, '_links')
faln5 <- 'PP7_links_full'
faln6 <- paste0('PP', c(8:13), '_links')
full_all_link_names <- c(full_all_link_names1, faln2, faln3, faln4, faln5, faln6)

full_all_links <-
  mget(full_all_link_names) # list of all individual link dataframes
full_all_links_df <-
  purrr::reduce(full_all_links,
                dplyr::inner_join,
                by = c('source.nodeId', 'target.nodeId'))

# Including the DSM networks, only need to fix those that were now adjusted. Full (including extra node) can be found in individual code
PP_1_links_full <- full_all_links_df[, c(1:3)]
PP_5_links_full <- full_all_links_df[, c(1:2, 7)]
PP_7_links_full <- full_all_links_df[, c(1:2, 11)]

########## Making sure the order of nodes is the same for all participants
all_node_names <- paste0('PP', 1:13, '_nodes')
all_nodes <- 
  mget(all_node_names) # list of all individual node dataframes
all_nodes_df <-
  purrr::reduce(all_nodes, dplyr::inner_join, by = 'nodeId')
# recreating participant dataframes, now sorted
PP_1_nodes <- all_nodes_df[, c(1:2)]
PP_2_nodes <- all_nodes_df[, c(1,3)]
PP_3_nodes <- all_nodes_df[, c(1,4)]
PP_4_nodes <- all_nodes_df[, c(1,5)]
PP_5_nodes <- all_nodes_df[, c(1,6)]
PP_6_nodes <- all_nodes_df[, c(1,7)]
PP_7_nodes <- all_nodes_df[, c(1,8)]
PP_8_nodes <- all_nodes_df[, c(1,9)]
PP_9_nodes <- all_nodes_df[, c(1,10)]
PP_10_nodes <- all_nodes_df[, c(1,11)]
PP_11_nodes <- all_nodes_df[, c(1,12)]
PP_12_nodes <- all_nodes_df[, c(1,13)]
PP_13_nodes <- all_nodes_df[, c(1,14)]

# adjusting nodes sizes for visualisation
PP_1_nodes_sizes <- PP_1_nodes[, 2] / 10
PP_2_nodes_sizes <- PP_2_nodes[, 2] / 10
PP_3_nodes_sizes <- PP_3_nodes[, 2] / 10
PP_4_nodes_sizes <- PP_4_nodes[, 2] / 10
PP_5_nodes_sizes <- PP_5_nodes[, 2] / 10
PP_6_nodes_sizes <- PP_6_nodes[, 2] / 10
PP_7_nodes_sizes <- PP_7_nodes[, 2] / 10
PP_8_nodes_sizes <- PP_8_nodes[, 2] / 10
PP_9_nodes_sizes <- PP_9_nodes[, 2] / 10
PP_10_nodes_sizes <- PP_10_nodes[, 2] / 10
PP_11_nodes_sizes <- PP_11_nodes[, 2] / 10
PP_12_nodes_sizes <- PP_12_nodes[, 2] / 10
PP_13_nodes_sizes <- PP_13_nodes[, 2] / 10


