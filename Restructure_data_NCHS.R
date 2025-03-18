#Access all necessary libraries 
library(haven)
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)


#The way of restructuring is inspired by and partially taken from Novosad et al. (2022)'s Stata code
#Starat by reading the data in R
US_YYYY = read_dta("path")

#The data that Novosad et al. used, we use this to compare our constructed aggregated data with theirs
US_raw_data = read_dta("C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/mortality_by_ed_group.dta")

#Convert the data to obtain a dataset with a 'time series' from the Novosad data
#smaller_raw = US_raw_data %>% select(year, edclass, age, sex, race, tpop)
#timeSeries = smaller_raw %>%
  #pivot_wider(names_from = year, values_from = tpop)

#Updated Version (converts to seperate columns -> implementable in Eviews)
#smaller_raw = US_raw_data %>% select(year, edclass, age, sex, race, tpop)
#smaller_raw = smaller_raw %>%
  #Make single identifier
#  mutate(name = paste(edclass, age, sex, race, sep = "_")) %>% 
  #Remove old columns
#  select(-edclass, -age, -sex, -race) %>%
#  arrange(year)
#timeSeries <- smaller_raw %>%
#  pivot_wider(names_from = name, values_from = tpop)

#write_xlsx(timeSeries, "C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/totalPopTimeSeries.xlsx")



#Here we start the function that reads and shapes the yearly data
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
  #df$ager52 = as.numeric(df$ager52)
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
  df = df[df$hspanicr != 9, ]
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
        #As explained in the description of NVSS, the data uses E800-E900 instead of 800-900
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
      ),,
      #Add the deaths of despair
      despair = ifelse(poison + suicide + liver == 1, 1, 0),
      #Other diseases
      other_diseases = ifelse(accidents + clrd + cereb + cancer + heart + liver + suicide + poison == 0, 1, 0)
    )
  
  #Dan nog de lijst met alle andere diseases
  
  df = df %>% select(year, eduRank, ager52, agegr, origin, sex, poison, liver, despair, suicide, heart, accidents, clrd, cereb, cancer, lungCancer, other_diseases, one)
  #en alle toe te voegen death causes
  return(df)
}

struct9902 = function(df, year){
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
  #df$ager52 = as.numeric(df$ager52)
  #sex
  #As long as M = 1 and F = 2 its fine
  if (any(df$sex == "M")) {
    df = df %>% 
      mutate( sex = case_when(
        substr(sex,1,1) %in% "M" ~ 1,
        TRUE ~ 2
      ))
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
        substr(ucod,1,3) %in% paste0("Y", 10:15) ~ 1,
        substr(ucod,1,3) %in% paste0("X", 40:45) ~ 1,
        substr(ucod,1,3) %in% paste0("Y", seq(45, 49, by = 2)) ~ 1,
        TRUE ~ 0
      ),
      #Deze is nog wel even een dingetje, kijken we nu naar 950.0-959.9 0f E950.0-E959.9?
      suicide = case_when(
        substr(ucod,1,3) %in% paste0("X", 60:84) ~ 1,
        substr(ucod,1,4) %in% paste0("Y870") ~ 1,
        TRUE ~ 0
      ),
      #Liver ziet er kloppend uit in vergelijking met de wikipediapagina
      liver = case_when(
        substr(ucod,1,3) %in% paste0("K70") ~ 1,
        substr(ucod,1,4) %in% paste0("K70", 0:9) ~ 1,
        substr(ucod,1,4) %in% paste0("K7", 30:49) ~ 1,
        TRUE ~ 0
      ),
      #Zelfde geldt voor Heart
      heart = case_when(
        substr(ucod,1,4) %in% paste0("I0", 10:99) ~ 1,
        substr(ucod,1,4) %in% paste0("I00", 0:9) ~ 1,
        substr(ucod,1,3) %in% paste0("I", seq(11,13,by = 2)) ~ 1,
        substr(ucod,1,4) %in% paste0("I11", 0:9) ~ 1,
        substr(ucod,1,4) %in% paste0("I13", 0:9) ~ 1,
        substr(ucod,1,3) %in% paste0("I", 20:51) ~ 1,
        substr(ucod,1,4) %in% paste0("I", 200:519) ~ 1,
        TRUE ~ 0
      ),
      #Ook (Lung)Cancer
      cancer = case_when(
        substr(ucod,1,1) %in% paste0("C") ~ 1,
        TRUE ~ 0
      ),
      lungCancer = case_when(
        substr(ucod,1,3) %in% paste0("C34") ~ 1,
        TRUE ~ 0
      ),
      #cerebrovascular ook prima gedefinieerd
      cereb = case_when(
        substr(ucod,1,3) %in% paste0("I", 60:69) ~ 1,
        TRUE ~ 0
      ),
      #CLRD
      clrd = case_when(
        substr(ucod,1,3) %in% paste0("J", 40:47) ~ 1,
        TRUE ~ 0
      ))
  
  df = df %>% 
    mutate(
      #accidents/war
      accidents = case_when(
        substr(ucod,1,1) %in% c("V","W","X","Y") & (poison + liver + suicide == 0) ~ 1,
        TRUE ~ 0,
      ),
      #Add the deaths of despair
      despair = ifelse(poison + suicide + liver == 1, 1, 0),
      #Other diseases
      other_diseases = ifelse(accidents + clrd + cereb + cancer + heart + liver + suicide + poison == 0, 1, 0)
    )
  
  #Dan nog de lijst met alle andere diseases
  
  df = df %>% select(year, eduRank, ager52, agegr, origin, sex, poison, liver, suicide, despair, heart, accidents, clrd, cereb, cancer, lungCancer, other_diseases, one)
  #en alle toe te voegen death causes
  return(df)
}

struct0321 = function(df, year){
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
  #df$ager52 = as.numeric(df$ager52)
  #sex
  #As long as M = 1 and F = 2 its fine
  if (any(df$sex == "M")) {
    df = df %>% 
      mutate( sex = case_when(
        substr(sex,1,1) %in% "M" ~ 1,
        TRUE ~ 2
      ))
  }
  
  #race
  #Create 'binary' columns for race
  if(!all(is.na(df$hspanicr))){
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
  } else {
    df <- df %>%
      mutate(
        origin = case_when(
          hispanic %in% 100:199 & race40 == 1 ~ 1,
          hispanic %in% 100:199 & race40 == 2 ~ 2,
          hispanic %in% 200:299 ~ 3,
          hispanic %in% 100:199 & race40 %in% 3:40 ~ 4,
          TRUE ~ 9
        )
      )
    df = df[df$origin != 9,]
  }
    
  #education
  #Constructing the education rank for each level, based on the 1989 and 2003 notation
  if (any(colnames(df) == "educ89")){
    df <- df %>%
      mutate(
        eduRank = case_when(
          educflag == 0 & educ89 <= 11 ~ 1,
          educflag == 0 & educ89 == 12 ~ 2,
          educflag == 0 & educ89 <= 15 ~ 3,
          educflag == 0 & educ89 <= 17 ~ 4,
          educflag == 1 & educ <= 2 ~ 1,
          educflag == 1 & educ == 3 ~ 2,
          educflag == 1 & educ <= 5 ~ 3,
          educflag == 1 & educ <= 8 ~ 4,
          TRUE ~ 9
        )
      )
  } else if (any(colnames(df) == "educ1989" & colnames(df) == "educ2003")){
    df <- df %>%
      mutate(
        eduRank = case_when(
          educflag == 0 & educ1989 <= 11 ~ 1,
          educflag == 0 & educ1989 == 12 ~ 2,
          educflag == 0 & educ1989 <= 15 ~ 3,
          educflag == 0 & educ1989 <= 17 ~ 4,
          educflag == 1 & educ2003 <= 2 ~ 1,
          educflag == 1 & educ2003 == 3 ~ 2,
          educflag == 1 & educ2003 <= 5 ~ 3,
          educflag == 1 & educ2003 <= 8 ~ 4,
          TRUE ~ 9
        )
        #In 2021 the educ1989 is completely deleted from the data
      )
    }else if (any(colnames(df) == "educ2003")){
        df <- df %>%
          mutate(
            eduRank = case_when(
              educflag == 1 & educ2003 <= 2 ~ 1,
              educflag == 1 & educ2003 == 3 ~ 2,
              educflag == 1 & educ2003 <= 5 ~ 3,
              educflag == 1 & educ2003 <= 8 ~ 4,
              TRUE ~ 9
            )
          )
  } else {
    stop("No correct colnames for education")
  }
  
  #Cause of death
  #Create binary variables for the main cause of death -> using the same numbers as Novosad et al. used with ICD-9n data
  #Poisoning
  df = df %>% 
    mutate(
      #Create a variable for all the deceased
      one = 1,
      #Poisoning cases in icd-9: E850-E860 and E980 (Alcohol and drug poisoning with undetermined intent)
      poison = case_when(
        substr(ucod,1,3) %in% paste0("Y", 10:15) ~ 1,
        substr(ucod,1,3) %in% paste0("X", 40:45) ~ 1,
        substr(ucod,1,3) %in% paste0("Y", seq(45, 49, by = 2)) ~ 1,
        TRUE ~ 0
      ),
      #Deze is nog wel even een dingetje, kijken we nu naar 950.0-959.9 0f E950.0-E959.9?
      suicide = case_when(
        substr(ucod,1,3) %in% paste0("X", 60:84) ~ 1,
        substr(ucod,1,4) %in% paste0("Y870") ~ 1,
        TRUE ~ 0
      ),
      #Liver ziet er kloppend uit in vergelijking met de wikipediapagina
      liver = case_when(
        substr(ucod,1,3) %in% paste0("K70") ~ 1,
        substr(ucod,1,4) %in% paste0("K70", 0:9) ~ 1,
        substr(ucod,1,4) %in% paste0("K7", 30:49) ~ 1,
        TRUE ~ 0
      ),
      #Zelfde geldt voor Heart
      heart = case_when(
        substr(ucod,1,4) %in% paste0("I0", 10:99) ~ 1,
        substr(ucod,1,4) %in% paste0("I00", 0:9) ~ 1,
        substr(ucod,1,3) %in% paste0("I", seq(11,13,by = 2)) ~ 1,
        substr(ucod,1,4) %in% paste0("I11", 0:9) ~ 1,
        substr(ucod,1,4) %in% paste0("I13", 0:9) ~ 1,
        substr(ucod,1,3) %in% paste0("I", 20:51) ~ 1,
        substr(ucod,1,4) %in% paste0("I", 200:519) ~ 1,
        TRUE ~ 0
      ),
      #Ook (Lung)Cancer
      cancer = case_when(
        substr(ucod,1,1) %in% paste0("C") ~ 1,
        TRUE ~ 0
      ),
      lungCancer = case_when(
        substr(ucod,1,3) %in% paste0("C34") ~ 1,
        TRUE ~ 0
      ),
      #cerebrovascular ook prima gedefinieerd
      cereb = case_when(
        substr(ucod,1,3) %in% paste0("I", 60:69) ~ 1,
        TRUE ~ 0
      ),
      #CLRD
      clrd = case_when(
        substr(ucod,1,3) %in% paste0("J", 40:47) ~ 1,
        TRUE ~ 0
      ),
      corona = case_when(
        substr(ucod, 1, 4) %in% "U071" ~ 1,
        TRUE ~ 0
      ))
  
  df = df %>% 
    mutate(
      #accidents/war
      accidents = case_when(
        substr(ucod,1,1) %in% c("V","W","X","Y") & (poison + liver + suicide == 0) ~ 1,
        TRUE ~ 0,
      ),
      #Add the deaths of despair
      despair = ifelse(poison + suicide + liver == 1, 1, 0),
      #Other diseases
      other_diseases = ifelse(accidents + clrd + cereb + cancer + heart + liver + suicide + poison == 0, 1, 0)
    )
  
  #Dan nog de lijst met alle andere diseases
  
  df = df %>% select(year, eduRank, ager52, agegr, origin, sex, poison, liver, suicide, despair, corona, heart, accidents, clrd, cereb, cancer, lungCancer, other_diseases, one)
  #en alle toe te voegen death causes
  return(df)
}

aggYear = function(df){
  #Group the data such that each cell specifies a different disease
  df = df %>%
    group_by(year, origin, agegr, sex, eduRank) %>%
    summarise(
      tMort = sum(one, na.rm = TRUE),
      poisMort = sum(poison, na.rm = TRUE),
      liverMort = sum (liver, na.rm = TRUE),
      suicideMort = sum(suicide, na.rm = TRUE),
      despMort = sum(despair, na.rm = TRUE),
      coronaMort = sum(corona, na.rm = TRUE),
      cancerMort = sum(cancer, na.rm = TRUE),
      lungCancerMort = sum(lungCancer, na.rm = TRUE),
      heartMort = sum(heart, na.rm = TRUE),
      accidentsMort = sum(accidents, na.rm = TRUE),
      clrdMort = sum(clrd, na.rm = TRUE),
      cerebMort = sum(cereb, na.rm = TRUE),
      otherDiseasesMort = sum(other_diseases, na.rm = TRUE),
    )  %>%
    as.data.frame()
  #Adjust unknown education data (eduRank == 9) distributively to the other data
  #Because the inputted dataframe has the same indices we can take hard numbers
  for(i in 0:87){
    for(j in 6:18){
      tot = sum(df[(5*i + 1):(5*i + 4), j])
      if (is.na(tot) || tot == 0) next 
      #Calculating the share of unknown education rank that should be added to a specific rank
      df[5*i + 1, j]  = df[5*i + 1, j] + (df[5*i + 1, j]/tot)*df[5*i + 5, j]
      df[5*i + 2, j]  = df[5*i + 2, j] + (df[5*i + 2, j]/tot)*df[5*i + 5, j]
      df[5*i + 3, j]  = df[5*i + 3, j] + (df[5*i + 3, j]/tot)*df[5*i + 5, j]
      df[5*i + 4, j]  = df[5*i + 4, j] + (df[5*i + 4, j]/tot)*df[5*i + 5, j]
    }
  }
  #Add edu_rank_sex and cum_edu_rank_sex
#  df$edu_rank_sex = numeric(nrow(df))
#  df$cum_edu_rank_sex = numeric(nrow(df))
#  for(i in 0:87){
#    tot = sum(df$tMort[(5*i + 1):(5*i + 4)])
#    #Compute the (cumulative) education rank as well
#    df$edu_rank_sex[5*i + 1] = 100*(df$tMort[5*i + 1]/2)/tot 
#    df$edu_rank_sex[5*i + 2] = 2*df$edu_rank_sex[5*i + 1] + 100*((df$tMort[5*i + 2]/2)/tot)
#    df$edu_rank_sex[5*i + 3] = 2*df$edu_rank_sex[5*i + 2] + 100*((df$tMort[5*i + 3]/2)/tot)
#    df$edu_rank_sex[5*i + 4] = 2*df$edu_rank_sex[5*i + 3] + 100*((df$tMort[5*i + 4]/2)/tot)
#    
#    df$cum_edu_rank_sex[5*i + 1] = df$tMort[5*i + 1]/tot 
#    df$cum_edu_rank_sex[5*i + 2] = df$cum_edu_rank_sex[5*i + 1] + df$tMort[5*i + 2]/tot
#    df$cum_edu_rank_sex[5*i + 3] = df$cum_edu_rank_sex[5*i + 2] + df$tMort[5*i + 3]/tot
#    df$cum_edu_rank_sex[5*i + 4] = df$cum_edu_rank_sex[5*i + 3] + df$tMort[5*i + 4]/tot
# }
  #edu_rank_sex en cum_edu_rank Kunnen pas toegevoegd worden met de total population data
  
  df = df[df$eduRank != 9,]
  
  return(df)
}

calc = function(df, year){
  if(year <= 1998){
    x = struct9298(df, year)
  } else if(year <= 2002){
    x = struct9902(df,year)
  } else {
    x = struct0321(df,year)
  }
  y = aggYear(x)
  #y = eduRank(y)
  return(y)
}
#US_1992 = read.csv("C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/mort1992.csv")
#nice_1992 = calc(US_1992, 1992)

#US_1999 = read.csv("C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/mort1999.csv")
#nice_1999 = calc(US_1999, 1999)

#US_2003 = read.csv("C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/mort2003.csv")
#nice_2003 = calc(US_2003, 2003)

#US_2019 = read.csv("C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/Mort2019US.PubUse.csv")
#nice_2019 = calc(US_2019, 2019)

#US_2020 = read.csv("C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/Mort2020US.PubUse.csv")
nice_2020 = calc(US_2020, 2020)

#US_2021 = read_dta("C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/mort2021us.dta")
#US_2021 = as.data.frame(US_2021)
nice_2021 = calc(US_2021, 2021)

