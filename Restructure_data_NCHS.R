#Access all necessary libraries 
library(haven)
library(readxl)
library(dplyr)

#The way of restructuring is inspired by and partially taken from Novosad et al. (2022)'s Stata code
#Starat by reading the data in R
US_YYYY = read_dta("path")

struct9298 = function(df, year){
  #origin
  #remove all the foreigners from the data
  df = df[df$restatus <= 3, ]
  
  #age
  #keep the people from correct age group(s) in the data (age 25-29 until 75-79)
  df = df[df$ager52>=31 & df$ager52 <= 41,]
  
  #Convert the numerical values to age-groups
  df <- df %>%
    mutate(agegr = case_when(
      ager52 == 31 ~ "25-29",
      ager52 == 32 ~ "30-34",
      ager52 == 33 ~ "35-39",
      ager52 == 34 ~ "40-44",
      ager52 == 35 ~ "45-49",
      ager52 == 36 ~ "50-54",
      ager52 == 37 ~ "55-59",
      ager52 == 38 ~ "60-64",
      ager52 == 39 ~ "65-69",
      ager52 == 40 ~ "70-74",
      ager52 == 41 ~ "75-79",
      TRUE         ~ NA_character_
    ))
  
  #sex
  #As long as M = 1 and F = 2 its fine
  if (any(df$sex == "M")) {
    stop("Error: Column 'sex' contains 'M'.")
  }
  
  #race
  #Create 'binary' columns for race
  df = df %>% 
      mutate(
        WNH = ifelse(df$hspanicr == 6, 1, 0),
        BNH = ifelse(df$hspanicr == 7, 1, 0),
        H = ifelse(df$hspanicr <= 5, 1, 0),
        O = ifelse(df$hspanicr == 8, 1, 0),
        #quickly add the year as well
        year = year
      )
  df = df[df$hspanic != 9, ]
  #'easier' implementable origin ranks as well
  df = df %>%
    mutate(origin = case_when(
      WNH == 1 ~ 1,
      BNH == 1 ~ 2,
      H == 1 ~ 3,
      O == 1 ~ 4
    ))
  
  #education
  #Constructing the education rank for each level, based on the 1989 notation
  df = df %>%
    mutate(eduRank = case_when(
      educ <= 11 ~ 1,
      educ == 12 ~ 2,
      educ <= 15 ~ 3,
      educ <= 17 ~ 4,
      TRUE ~ 9
    ))
  
  
  #Cause of death
  #Create binary variables for the main cause of death -> using the same numbers as Novosad et al. used with ICD-9n data
  #Poisoning
  df = df %>% 
      mutate(
        #Create a variable for all the deceased
        one = 1,
        #Poisoning cases in icd-9: E850-E860 and E980 (Alcohol and drug poisoning with undetermined intent)
        poison = case_when(
          ucod >=8500 & ucod<= 8609 ~ 1,
          ucod >=9800 & ucod<= 9804 ~ 1,
          TRUE ~ 0
          ),
        #Deze is nog wel even een dingetje, kijken we nu naar 950.0-959.9 0f E950.0-E959.9?
        suicide = case_when(
          ucod >= 9500 & ucod <= 9599 ~ 1,
          TRUE ~ 0
          ),
        #Liver ziet er kloppend uit in vergelijking met de wikipediapagina
        liver = case_when(
          ucod >= 5710 & ucod<= 5719 ~ 1,
          TRUE ~ 0
          ),
        #Zelfde geldt voor Heart
        heart = case_when(
          ucod >= 3900 & ucod <= 4299 ~ 1,
          TRUE ~ 0
          ),
        #Ook (Lung)Cancer
        cancer = case_when(
          ucod >= 1400 & ucod <= 2089 ~ 1,
          TRUE ~ 0
        ),
        lungCancer = case_when(
          ucod >= 1622 & ucod <= 1629 ~ 1,
          TRUE ~ 0
        ),
        #cerebrovascular ook prima gedefinieerd
        cereb = case_when(
          ucod >= 4300 & ucod <= 4389 ~ 1,
          TRUE ~ 0
        ),
        #CLRD
        clrd = case_when(
          ucod >= 4900 & ucod <= 4969 ~ 1,
          TRUE ~ 0
        ))
  
  df = df %>% 
    mutate(
      #accidents/war
      accidents = case_when(
        ucod >= 8000 & ucod <= 9999 & poison + liver + suicide == 0 ~ 1,
        TRUE ~ 0,
      ),
      #Other diseases
      other_diseases = ifelse(accidents + clrd + cereb + cancer + heart + liver + suicide + poison == 0, 1, 0)
    )
  
  #Dan nog de lijst met alle andere diseases
  
  df = df %>% select(year, eduRank, ager52, agegr, origin, sex, poison, liver, suicide, heart, accidents, clrd, cereb, cancer, lungCancer, other_diseases, one)
  #en alle toe te voegen death causes
  return(df)
}

aggYear = function(df){
  #Group the data such that each cell specifies another disease
  df = df %>%
    group_by(year, origin, agegr, sex, eduRank) %>%
    summarize(
      tMort = sum(one, na.rm = TRUE),
      poisMort = sum(poison, na.rm = TRUE),
      liverMort = sum (liver, na.rm = TRUE),
      suicideMort = sum(suicide, na.rm = TRUE),
      heartMort = sum(heart, na.rm = TRUE),
      accidentsMort = sum(accidents, na.rm = TRUE),
      clrdMort = sum(clrd, na.rm = TRUE),
      cerebMort = sum(cereb, na.rm = TRUE),
      cancerMort = sum(cancer, na.rm = TRUE),
      lungCancerMort = sum(lungCancer, na.rm = TRUE),
      otherDiseasesMort = sum(other_diseases, na.rm = TRUE),
    )
  #Hie nog de edu_rank_sex en cum_edu_rank toevoegen? 
  return(df)
}


US_1992 = read.csv("C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/mort1992.csv")

better_1992 = struct9298(US_1992, 1992)
agg_1992 = aggYear(better_1992)
