## ----------------------------
##### QUALTRICS DATA WRANGLING
## ----------------------------

setwd() # adjust accordingly

library(readxl)
library(dplyr)
library(psych)

PP_data_Qualtrics <- read_excel("FMG-12693 Clinician MDD PECAN_June 6 2025_02.57.xlsx")
# Filtering out failed manipulation checks, incomplete pecan files, and NA's from incomplete responses

PP_data_Qualtrics <- PP_data_Qualtrics[-c(1:5),] #remove data from pilot (first five responses from master students with clinical experience)
# Total = 30

PP_data_Qualtrics <- PP_data_Qualtrics[PP_data_Qualtrics$`Manipulation check 1` == 'Soms', ] # remove failed manipulation checks
# Total = 29

PP_data_Qualtrics <- PP_data_Qualtrics[PP_data_Qualtrics$Q19_Type == 'application/json', ] # including only individuals with a valid PECAN file
# Total = 13

PP_data_Qualtrics <- PP_data_Qualtrics[is.na(PP_data_Qualtrics$Q19_Id) == FALSE, ]
# Removing remaining rows with NAs in PECANID column

PP_data_Qualtrics <- PP_data_Qualtrics[,-c(1:14, 44:45, 49, 53, 62, 64:65)] #superfluous columns

# Rename columns appropriately & fit answers from in-text answers into variable columns
colnames(PP_data_Qualtrics)[2:3] <- c('Age', 'Gender')
PP_data_Qualtrics <- PP_data_Qualtrics[,-4]
colnames(PP_data_Qualtrics)[c(4:5, 7:8, 10, 12:23, 25, 35, 37, 39:42)] <- c('Experience (years)', 'Workplace', 'Top 3 encountered disorders', 'Position', 'MDD encounter frequency', 'MDD treatment frequency', 'I feel competent in my knowledge about MDD', 'I feel competent treating patients with MDD', 'Behaviour', 'Cognitions', 'Biology/Neurology', 'Experiences during youth and/or subconscious', 'Behavioural activation and/or experiments', 'Challenging cognitions', 'Antidepressives/Medical intervention', 'Tackle patterns like defense mechanisms and schemas', 'Other', 'Familiarity with network theory', 'Reference point(s) PECAN', 'Reference point(s) additional factors', 'Top 3 factors to treat', 'Justification top 3 treated factors', 'Intervention choice top 3 factors', 'Self-identified theoretical orientation')
PP_data_Qualtrics$Age <- as.numeric(PP_data_Qualtrics$Age)
PP_data_Qualtrics$Gender <- as.factor(PP_data_Qualtrics$Gender)
PP_data_Qualtrics$Workplace[c(5,10)] <- 'Universiteit'; PP_data_Qualtrics$Workplace[8] <- 'Specialistische GGZ, Universiteit'; PP_data_Qualtrics$Workplace[9] <- 'Privépraktijk, Aanvullende zorg'
PP_data_Qualtrics <- PP_data_Qualtrics[,-6] # Above question's open text option

PP_data_Qualtrics$Position[9] <- 'Psychosociaal therapeut'
PP_data_Qualtrics$Position[c(5,10)] <- 'Universitair docent'
PP_data_Qualtrics <- PP_data_Qualtrics[,-8] # Above question's open text option

barplot(table(PP_data_Qualtrics$`Experience (years)`)) # Visualisation of varying responses

colnames(PP_data_Qualtrics)[c(22, 34, 36, 42)] <- c('Other intervention', 'Other reference point(s) PECAN', 'Other reference points additional factors', 'PP PECAN')
PP_data_Qualtrics <- PP_data_Qualtrics[,-c(26, 29, 32)] # Superfluous column removal; Qualtrics timing page submission data
PP_data_Qualtrics[,11:21] <- PP_data_Qualtrics[,11:21] %>% mutate_if(is.character,as.numeric) # Setting Likert answer options as numeric
PP_data_Qualtrics$`Experience (years)` <- as.factor(PP_data_Qualtrics$`Experience (years)`)
PP_data_Qualtrics$Workplace <- as.factor(PP_data_Qualtrics$Workplace)
PP_data_Qualtrics$Position <- as.factor(PP_data_Qualtrics$Position)
PP_data_Qualtrics$`MDD encounter frequency` <- as.factor(PP_data_Qualtrics$`MDD encounter frequency`)
PP_data_Qualtrics$`MDD treatment frequency` <- as.factor(PP_data_Qualtrics$`MDD treatment frequency`)

PP_data_Qualtrics <- PP_data_Qualtrics[,-c(24, 26, 28)] # Superfluous column removal; Qualtrics first click data
colnames(PP_data_Qualtrics)[c(24:26)] <- c('Survey p 1 time', 'PECAN Making time', 'Survey p 2 time') # Total procedure time as sum of both PECAN sections. Telemetry from PECAN is extracted from JSON files

PP_data_Qualtrics[,24:26] <- PP_data_Qualtrics[,24:26] %>% mutate_if(is.character,as.numeric)
## ----------------------------
##### DESCRIPTIVES
## ----------------------------

summary(PP_data_Qualtrics$Age) # Mean = 39.85, Ranged 26 - 61
describe(PP_data_Qualtrics$Age)
summary(PP_data_Qualtrics$Gender) # Women = 7, Men = 5, Other identified = 1
summary(PP_data_Qualtrics$`Experience (years)`) # >1 year = 2, 1 > 3 years = 4, 5 > 10 years = 4, 10 > 15 years = 2, >20 years = 1 
summary(PP_data_Qualtrics$Position) #  Basis/Masterpsycholoog = 6, GZ psycholoog = 3, Psychosociaal therapeut = 1      Psychotherapeut = 1, Universitair docent = 2
summary(PP_data_Qualtrics$Workplace)
summary(PP_data_Qualtrics$`MDD encounter frequency`)
summary(PP_data_Qualtrics$`MDD treatment frequency`)

# Time taken to complete questionnaire (both sections):
PP_data_Qualtrics <- as.data.frame(PP_data_Qualtrics) %>%
  rowwise() %>% 
  mutate(survey_time_total = sum(c_across(paste('Survey', 'p', 1:2, 'time')))/60)

## ----------------------------
# Translating responses into English
clinician_numbers <- c(1:13)
demog_PP_table <- cbind(clinician_numbers, PP_data_Qualtrics[,c(4:5, 7:8, 10:12, 23)])
colnames(demog_PP_table) <- c('Clinician', 'Clinical experience (years)', 'Work setting(s)', 'Profession', 'Frequency of encountering MDD', 'Frequency of treating MDD', 'Perceived competency of MDD knowledge', 'Perceived competency in treating MDD', 'Familiarity with psychological networks')
# demog_PP_table[,2] = [1] Tussen 1 en 3 jaar   Minder dan 1 jaar    Minder dan 1 jaar    20 jaar of langer    Tussen 1 en 3 jaar  [6] Tussen 5 en 10 jaar  Tussen 5 en 10 jaar  Tussen 5 en 10 jaar  Tussen 1 en 3 jaar   Tussen 10 en 15 jaar [11] Tussen 1 en 3 jaar   Tussen 10 en 15 jaar Tussen 5 en 10 jaar
demog_PP_table[,2] <- c('1 > 3', '< 1', '< 1', '> 20', '1 > 3', '5 > 10', '5 > 10', '5 > 10', '1 < 3', '10 > 15', '1 > 3', '10 > 15', '5 > 10')
# demog_PP_table[,3]
# [1] Specialistische GGZ                         Basis GGZ,Specialistische GGZ              
# [3] Specialistische GGZ                         Basis GGZ,Specialistische GGZ              
# [5] Universiteit                                Specialistische GGZ                        
# [7] Specialistische GGZ                         Specialistische GGZ, Universiteit          
# [9] Privépraktijk, Aanvullende zorg             Universiteit                               
# [11] Specialistische GGZ                         Specialistische GGZ                        
# [13] Basis GGZ,Specialistische GGZ,Privépraktijk
demog_PP_table[,3] <- c('Specialist care', 'Basic care, Specialist care', 'Specialist care', 'Basic care, specialist care', 'University', 'Specialist care', 'specialist care', 'Specialist care, University', 'Private practice, Supportive care', 'University', 'Specialist care', 'Specialist care', 'Basic care, Specialist care, Private practice')
# demog_PP_table[,4]
# [1] Basis/Masterpsycholoog  Basis/Masterpsycholoog  Basis/Masterpsycholoog  GZ psycholoog          
# [5] Universitair docent     Basis/Masterpsycholoog  Psychotherapeut         GZ psycholoog          
# [9] Psychosociaal therapeut Universitair docent     Basis/Masterpsycholoog  GZ psycholoog          
# [13] Basis/Masterpsycholoog 
demog_PP_table[,4] <- c('Psychologist', 'Psychologist', 'Psychologist', 'Healthcare psychologist', 'University lecturer', 'Psychologist', 'Psychotherapist', 'Healthcare psychologist', 'Psychosocial therapist', 'University lecturer', 'Psychologist', 'Healthcare psychologist', 'Psychologist')
# demog_PP_table[,5]
# [1] Meerdere keren op een dag                                  
# [2] Eens per dag                                               
# [3] Meerdere keren op een dag                                  
# [4] Meerdere keren op een dag                                  
# [5] Ik zie (bijna) nooit patiënten met een depressieve stoornis
# [6] Eens per week                                              
# [7] Meerdere keren op een dag                                  
# [8] Eens per week                                              
# [9] Ik zie (bijna) nooit patiënten met een depressieve stoornis
# [10] Ik zie (bijna) nooit patiënten met een depressieve stoornis
# [11] Meerdere keren op een dag                                  
# [12] Eens per week                                              
# [13] Een paar keer per week      
demog_PP_table[,5] <- c('Multiple times a day', 'Once a day', 'Multiple times a day', 'Multiple times a day', 'I (almost) never see patients with MDD', 'Once a week', 'Multiple times a day', 'Once a week', 'I (almost) never see patients with MDD', 'I (almost) never see patients with MDD', 'Multiple times a day', 'Once a week', 'A few times a week')
# demog_PP_table[,6]
# [1] Een paar keer per week                                          
# [2] Eens per dag                                                    
# [3] Meerdermaal op een dag                                          
# [4] Eens per dag                                                    
# [5] Ik behandel (bijna) nooit patiënten met een depressieve stoornis
# [6] Eens per week                                                   
# [7] Meerdermaal op een dag                                          
# [8] Eens per week                                                   
# [9] Ik behandel (bijna) nooit patiënten met een depressieve stoornis
# [10] Ik behandel (bijna) nooit patiënten met een depressieve stoornis
# [11] Meerdermaal op een dag                                          
# [12] Eens per week                                                   
# [13] Een paar keer per week 
demog_PP_table[,6] <- c('A few times a week', 'Once a day', 'Multiple times a day', 'Once a day', 'I (almost) never treat patients with MDD', 'Once a week', 'Multiple times a day', 'Once a week', 'I (almost) never see patients with MDD', 'I (almost) never see patients with MDD', 'Multiple times a day', 'Once a week', 'A few times a week')
# demog_PP_table[,9]
# [1] "Ik ben ermee bekend en heb er interesse in/ben er actief mee bezig"
# [2] "Ik ben ermee bekend maar ben er niet actief mee bezig"             
# [3] "Ik ben ermee bekend maar ben er niet actief mee bezig"             
# [4] "Ik ben ermee bekend en heb er interesse in/ben er actief mee bezig"
# [5] "Ik ben ermee bekend maar ben er niet actief mee bezig"             
# [6] "Ik heb er wel eens van gehoord maar weet er weinig vanaf"          
# [7] "Nog nooit van gehoord"                                             
# [8] "Ik ben ermee bekend en heb er interesse in/ben er actief mee bezig"
# [9] "Ik ben ermee bekend maar ben er niet actief mee bezig"             
# [10] "Ik ben ermee bekend en heb er interesse in/ben er actief mee bezig"
# [11] "Ik ben ermee bekend en heb er interesse in/ben er actief mee bezig"
# [12] "Ik ben ermee bekend maar ben er niet actief mee bezig"             
# [13] "Nog nooit van gehoord" 
demog_PP_table[,9] <- c('I am familiar and have an interest/am actively involved', 'I am familiar but not actively involved', 'I am familiar but not actively involved', 'I am familiar and have an interest/am actively involved', 'I am familiar but not actively involved', 'I have heard of it but do not know much', 'I have never heard of it', 'I am familiar and have an interest/am actively involved', 'I am familiar but not actively involved', 'I am familiar and have an interest/am actively involved', 'I am familiar and have an interest/am actively involved', 'I am familiar but not actively involved', 'I have never heard of it')

library(rempsyc)
# only selecting demographic and work-related characteristics

nice_demog_PP_table <- nice_table(demog_PP_table, title = c("Table 1", "Clinician Demographic Characteristics"),
                                  note = "Age and gender variables are not included above to maintain anonymity. Perceived competency was measured on a 6-point Likert scale with 1 being the lowest option and 6 the highest.")
# names(nice_demog_PP_table) <- c('Clinician', 'Clinical experience (years)', 'Work setting(s)', 'Profession', 'Frequency of encountering MDD', 'Frequency of treating MDD', 'Perceived competency of MDD knowledge', 'Perceived competency in treating MDD', 'Familiarity with psychological networks')

print(nice_demog_PP_table, preview = "docx")

# Median time taken to complete the survey (robust against obvious outlier)
median(PP_data_Qualtrics$survey_time_total)


### Reference points for PECANs
summary(as.factor(PP_data_Qualtrics$`Reference point(s) PECAN`))

