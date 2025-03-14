#Access all necessary libraries
library(haven)
library(readxl)

#The data that Novosad et al. used, we use this to compare our constructed aggregated data with theirs
US_raw_data = read_dta("C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/mortality_by_ed_group.dta")

#Construct a dataframe that will help us 'count' the amount of people in each area
#Or is a separate function more suitable for this?
emptyCount = data.frame(
  Year = rep(0,11),
  AgeGroup = c("25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79"),
  M_1_White = rep(0,11),
  M_2_White = rep(0,11),
  M_3_White = rep(0,11),
  M_4_White = rep(0,11),
  M_1_Black = rep(0,11),
  M_2_Black = rep(0,11),
  M_3_Black = rep(0,11),
  M_4_Black = rep(0,11),
  M_1_Hispanic = rep(0,11),
  M_2_Hispanic = rep(0,11),
  M_3_Hispanic = rep(0,11),
  M_4_Hispanic = rep(0,11),
  M_1_Other = rep(0,11),
  M_2_Other = rep(0,11),
  M_3_Other = rep(0,11),
  M_4_Other = rep(0,11),
  F_1_White = rep(0,11),
  F_2_White = rep(0,11),
  F_3_White = rep(0,11),
  F_4_White = rep(0,11),
  F_1_Black = rep(0,11),
  F_2_Black = rep(0,11),
  F_3_Black = rep(0,11),
  F_4_Black = rep(0,11),
  F_1_Hispanic = rep(0,11),
  F_2_Hispanic = rep(0,11),
  F_3_Hispanic = rep(0,11),
  F_4_Hispanic = rep(0,11),
  F_1_Other = rep(0,11),
  F_2_Other = rep(0,11),
  F_3_Other = rep(0,11),
  F_4_Other = rep(0,11)
)

#Create a function for reproducability different years
#The function is yet able to aggregate data for cause of death and marital status
createAgg = function(df, year, count, exCount){
  
  #Filter the data set to all the usable data (Starting of with the age-range 25-29 until 75-79)
  df = df[df$ager52 >= 31 & df$ager52 <= 41, ]
  df = df[order(df$ager52),]
  
  #Adding the year to the dataframe
  for(i in 1:11){
    count$Year[i] = year
  }
  #separating education based on 1989 and 2003[i] format
  #Marstat is hier nog ergens in te fietsen
  for(i in 1:nrow(df)){
    if(df$educflag[i] == 1){ #To make sure that edu2003 has a value
      if(df$sex[i] == "M"){
        #Creating the 4 ranks/bins of education
        if(df$educ2003[i] >= 1 & df$educ2003[i] <= 2){
          if(df$race[i] == 1){
            #If Hispanic is rate 100-199 than its non-hispanic
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_1_White[j-30] = count$M_1_White[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_1_Hispanic[j-30] = count$M_1_Hispanic[j-30] + 1
                  break
                }
              }
            }
          } else if(df$race[i] == 2){
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_1_Black[j-30] = count$M_1_Black[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_1_Hispanic[j-30] = count$M_1_Hispanic[j-30] + 1
                  break
                }
              }
            }
          } else if(df$race[i] >= 3){
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_1_Other[j-30] = count$M_1_Other[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_1_Hispanic[j-30] = count$M_1_Hispanic[j-30] + 1
                  break
                }
              }
            }
          }
        } else if(df$educ2003[i] == 3){
          if(df$race[i] == 1){
            #If Hispanic is rate 100-199 than its non-hispanic
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_2_White[j-30] = count$M_2_White[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_2_Hispanic[j-30] = count$M_2_Hispanic[j-30] + 1
                  break
                }
              }
            }
          } else if(df$race[i] == 2){
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_2_Black[j-30] = count$M_2_Black[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_2_Hispanic[j-30] = count$M_2_Hispanic[j-30] + 1
                  break
                }
              }
            }
          } else if(df$race[i] >= 3){
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_2_Other[j-30] = count$M_2_Other[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_2_Hispanic[j-30] = count$M_2_Hispanic[j-30] + 1
                  break
                }
              }
            }
          }
        } else if(df$educ2003[i] >= 4 & df$educ2003[i] <= 5){
          if(df$race[i] == 1){
            #If Hispanic is rate 100-199 than its non-hispanic
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_3_White[j-30] = count$M_3_White[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_3_Hispanic[j-30] = count$M_3_Hispanic[j-30] + 1
                  break
                }
              }
            }
          } else if(df$race[i] == 2){
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_3_Black[j-30] = count$M_3_Black[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_3_Hispanic[j-30] = count$M_3_Hispanic[j-30] + 1
                  break
                }
              }
            }
          } else if(df$race[i] >= 3){
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_3_Other[j-30] = count$M_3_Other[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_3_Hispanic[j-30] = count$M_3_Hispanic[j-30] + 1
                  break
                }
              }
            }
          }
        } else if(df$educ2003[i] >= 6 & df$educ2003[i] <= 8){
          if(df$race[i] == 1){
            #If Hispanic is rate 100-199 than its non-hispanic
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_4_White[j-30] = count$M_4_White[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_4_Hispanic[j-30] = count$M_4_Hispanic[j-30] + 1
                  break
                }
              }
            }
          } else if(df$race[i] == 2){
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_4_Black[j-30] = count$M_4_Black[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_4_Hispanic[j-30] = count$M_4_Hispanic[j-30] + 1
                  break
                }
              }
            }
          } else if(df$race[i] >= 3){
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_4_Other[j-30] = count$M_4_Other[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$M_4_Hispanic[j-30] = count$M_4_Hispanic[j-30] + 1
                  break
                }
              }
            }
          }
        } 
      } else if(df$sex[i] == "F"){
        #Creating the 4 ranks/bins of education
        if(df$educ2003[i] >= 1 & df$educ2003[i] <= 2){
          if(df$race[i] == 1){
            #If Hispanic is rate 100-199 than its non-hispanic
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_1_White[j-30] = count$F_1_White[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_1_Hispanic[j-30] = count$F_1_Hispanic[j-30] + 1
                  break
                }
              }
            }
          } else if(df$race[i] == 2){
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_1_Black[j-30] = count$F_1_Black[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_1_Hispanic[j-30] = count$F_1_Hispanic[j-30] + 1
                  break
                }
              }
            }
          } else if(df$race[i] >= 3){
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_1_Other[j-30] = count$F_1_Other[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_1_Hispanic[j-30] = count$F_1_Hispanic[j-30] + 1
                  break
                }
              }
            }
          }
        } else if(df$educ2003[i] == 3){
          if(df$race[i] == 1){
            #If Hispanic is rate 100-199 than its non-hispanic
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_2_White[j-30] = count$F_2_White[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_2_Hispanic[j-30] = count$F_2_Hispanic[j-30] + 1
                  break
                }
              }
            }
          } else if(df$race[i] == 2){
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_2_Black[j-30] = count$F_2_Black[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_2_Hispanic[j-30] = count$F_2_Hispanic[j-30] + 1
                  break
                }
              }
            }
          } else if(df$race[i] >= 3){
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_2_Other[j-30] = count$F_2_Other[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_2_Hispanic[j-30] = count$F_2_Hispanic[j-30] + 1
                  break
                }
              }
            }
          }
        } else if(df$educ2003[i] >= 4 & df$educ2003[i] <= 5){
          if(df$race[i] == 1){
            #If Hispanic is rate 100-199 than its non-hispanic
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_3_White[j-30] = count$F_3_White[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_3_Hispanic[j-30] = count$F_3_Hispanic[j-30] + 1
                  break
                }
              }
            }
          } else if(df$race[i] == 2){
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_3_Black[j-30] = count$F_3_Black[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_3_Hispanic[j-30] = count$F_3_Hispanic[j-30] + 1
                  break
                }
              }
            }
          } else if(df$race[i] >= 3){
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_3_Other[j-30] = count$F_3_Other[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_3_Hispanic[j-30] = count$F_3_Hispanic[j-30] + 1
                  break
                }
              }
            }
          }
        } else if(df$educ2003[i] >= 6 & df$educ2003[i] <= 8){
          if(df$race[i] == 1){
            #If Hispanic is rate 100-199 than its non-hispanic
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_4_White[j-30] = count$F_4_White[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_4_Hispanic[j-30] = count$F_4_Hispanic[j-30] + 1
                  break
                }
              }
            }
          } else if(df$race[i] == 2){
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_4_Black[j-30] = count$F_4_Black[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_4_Hispanic[j-30] = count$F_4_Hispanic[j-30] + 1
                  break
                }
              }
            }
          } else if(df$race[i] >= 3){
            if(df$hispanic[i] >= 100 & df$hispanic[i] <= 199){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_4_Other[j-30] = count$F_4_Other[j-30] + 1
                  break
                }
              }
            } else if(df$hispanic[i]>=200 & df$hispanic[i] <= 299){
              for(j in 31:41){
                if(df$ager52[i] == j){
                  count$F_4_Hispanic[j-30] = count$F_4_Hispanic[j-30] + 1
                  break
                }
              }
            }
          }
        }
      }
    } else if(df$educflag[i] == 0){
      #Hier nog iets toveren over de education code van 1989
    }
  }
  #Adding the aggregated data to the existing dataframe
  newCount = rbind(exCount, count)
  return(newCount)
}


#Now we can use the individual data to obtain the aggregate dataset

US_2013 = read.csv("path")
count_2013 = createAgg(US_2013, 2013, emptyCount, count_2012)


#Calculating the aggregated data for 2014, this can also be done for previous and latter years
US_2014 = read.csv("C:/Users/michi/OneDrive/Documenten/Eur Jaar 3/Blok 4/Major/Data US/mort2014.csv")
count_2014 = createAgg(US_2014, 2014, emptyCount, emptyCount)
