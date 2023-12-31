# load library
library(tidyverse)
# import data
df <- read_csv('dataset/Raw STUDENT SRESS SURVEY Questionnaire Data.csv')
#remove 2 rows
df <- df[-c(1,2),]
#renaming columns
df <- df %>% rename(
  "email" = `Enter your email address if you would like to get the web link for this research work`,
  "age" =  `Pleas select your age`,
  "gender" = `Please select your gender` ,
  "college" = `Please select your college`,
  "level" = `Please select your level` ,
  "G.P.A" = `Please select the range of your current G.P.A` ,
  "C.G.P" = `Please select the range of your current C.G.P.A` ,
  "department" = `Please select your department`,
  "sponsor" = `Please select your sponsor` ,
  "religion" = `Please select your religion` ,
  "status" = `Please select your marital status`,
  "S1" = `Have you lost of a close family member in the past twelve month?`,
  "E1" = `The death of a close family member is likely to affect your academic result`,
  "S2" = `Transportation problem`,
  "E2" = `Having a transportation problem is likely to affect your academic result`,
  "S3" = `Have you lost of a close friend in the past twelve month?` ,
  "E3" = `The death of a close friend is likely to affect your academic result` ,
  "S4" = `Is your parent divorced?` ,
  "E4" = `Divorce between one's parent is likely to affect your academic result`,
  "S5" = `Have you been held in police custody?`,
  "E5" = `Staying in police custody is likely to affect your academic result`,
  "S6" = `Do you have any personal illness or injury?`,
  "E6" = `Having personal injury or illness is likely to affect academic result` ,
  "S7" = `Are you married?`,
  "E7" = `Getting married is likely to affect your academic performance`,
  "S8" = `Did you fail any Carry (carry over)`,
  "E8" = `Having carry over is likely to affect your academic result`,
  "S9" = `Major change in the health of a family member?` ,
  "E9" = `Deteriorating in the health of a family member is likely to affect your academic result` ,
  "S10" = `Sex problems (abortion, pregnancy, miscarriage e.t.c)`,
  "E10" = `Experiencing sex problem is likely to affect academic result`,
  "S11" = `Fight with close friend` ,
  "E11" = `Fight with close friend is likely to affect your academic result`,
  "S12" = `Low financial status`,
  "E12" = `Inadequate finance is likely to affect your academic result`,
  "S13" = `Change of course` ,
  "E13" = `Not studying your first-choice course is likely to affect your academic result`,
  "S14" = `Trouble with parent`,
  "E14" = `Having trouble with parent is likely to affect your academic result`,
  "S15" = `Fight with boyfriend/girlfriend` ,
  "E15" = `Fight with boyfriend/girlfriend is likely to affect your academic result`,
  "S16" = `Compulsory holiday (ASUU/NASU strike)` ,
  "E16" = `Strike is likely to affect your academic result` ,
  "S17" = `Messy living condition ( bad hostel )` ,
  "E17" = `Living in a bad hostel is likely to affect your academic result` ,
  "S18" = `Trouble working with new people`,
  "E18" = `Having trouble working with new people is likely to affect your academic result`,
  "S19" = `Trouble with lecturer`,
  "E19" = `Having trouble with lecturer is likely to affect your academic result` ,
  "S20" = `Water shortage`,
  "E20" = `Having water shortage is likely to affect your academic performance`,
  "S21" = `Change in religious believe` ,
  "E21" = `Having a change in religious believe is likely to affect your academic result`,
  "S22" = `Major change in religious activities (a lot more or a lot less)`,
  "E22" = `Having a major change in religious activities is likely to affect your academic result`,
  "S23" = `Major change in sleeping habit ( a lot more or a lot less)` ,
  "E23" = `Having a major change in sleeping habit is likely to affect your academic performance`,
  "S24" = `Having a major change in eating habit ( a lot more or a lot less)` ,
  "E24" = `Having a major change in eating habit is likely to affect your academic performance` ,
  "S25" = `Major change in social activities (party, club, visitation e.t.c)` ,
  "E25" = `Having a major change in social activities is likely to affect your academic result` ,
  "S26" = `Waited in long queues`,
  "E26" =  `Waiting in a long queue is likely to affect your academic result`,
  "S27" = `Room mate conflict`,
  "E27" = `Having conflicts with room mate is likely to affect academic performance`,
  "S28" = `Missed too many classes`,
  "E28" = `Missing too many classes is likely to affect your academic result`,
  "S29" = `Computer problems (mobile phone, laptops e.t.c)` ,
  "E29" = `Having a computer problem is likely to affect your academic result` ,
  "S30" = `Electricity Shortage (blackout)`,
  "E30" = `Electricity shortage is likely to affect your academic result`
)
# subset main data
survey_data <- df[,c("age","gender","college","department","level","religion","G.P.A","C.G.P","status","sponsor","email",
                  "S1","S2","S3","S4","S5","S6","S7","S8","S9","S10","S11","S12","S13","S14","S15","S16","S17","S18","S19","S20",
                  "S21","S22","S23","S24","S25","S26","S27","S28","S29","S30","E1","E2","E3","E4","E5","E6","E7","E8","E9","E10",
                  "E11","E12","E13","E14","E15","E16","E17","E18","E19","E20","E21","E22","E23","E24","E25","E26","E27","E28","E29","E30")]
# reverse code likert item
survey_data[, c(42:71)] = 6 - survey_data[,c(42:71)]

# converting to factors
survey_data$age <- factor(survey_data$age, levels = unique(c("under 18","18 - 23","24 - 29","30 - 35","36 - 41","42 - 46","above 46")))
survey_data$level <-factor(survey_data$level, levels = unique(c(100,200,300,400,500,600)))
survey_data$gender <-factor(survey_data$gender, levels = unique(c("Male","Female")))
survey_data$religion <-factor(survey_data$religion, levels = unique(c("Christianity","Islamic","Other")))
survey_data$G.P.A <-factor(survey_data$G.P.A, levels = unique(c("4.5 - 5.0","3.5 - 4.4","2.5 - 3.4","1.5- 2.4","1.4 - below")))
survey_data$C.G.P <-factor(survey_data$C.G.P, levels = unique(c("4.5- 5.0","3.5 - 4.4","2.5 - 3.4","1.5- 2.4","1.4 - below")))
survey_data$status <-factor(survey_data$status, levels = unique(c("Single","Married","Divorced","Seperated","Widowed","Cohabiting")))
survey_data$sponsor <-factor(survey_data$sponsor, levels = unique(c("Parent","Self","Guardian","Other")))
survey_data$college <-factor(survey_data$college, levels = unique(c("COLPHYS","COLBIOS","COLERM","COLPHEC","COLPLANT","COLENG","COLANIM","COLMAS","COLAMRUD","COLVET")))
survey_data$department <-factor(survey_data$department, levels = unique(c("AGAD","AEFM","AERD","ABG","ANN","APH","PRM","ANP","BCH","MCB","PAB","PAZ","AGE","CVE","ELE","MCE","MTE","AQFM","EMT","FWM","WMA","FST","HTM","HSM","NTD","ETS","CHM","PHS","CSC","MTS","STS","CPT","HRT","PBST","SSLM","VET")))

# working on gender column
table(survey_data$gender)
survey_data$gender[which(is.na(survey_data$gender))] <- "Female"
table(survey_data$gender)
sum(is.na(survey_data$college)) # check if there is missing value

# working on college column
table(survey_data$college)
survey_data$college[which(is.na(survey_data$college))] <- "COLPHEC"
table(survey_data$college)
sum(is.na(survey_data$college)) # check if there is missing value

# convert E1:E30 to ordered factors
survey_data[,c(42:71)] <- survey_data[,c(42:71)] %>%
  mutate(
    across(
      .cols = everything(),
      .fns = function(x) factor(x, levels = c(1,2,3,4,5), ordered = TRUE )
    ))

# convert S1:S30 to two level factor
survey_data[,c(12:34)] <- survey_data[,c(12:34)] %>%
  mutate(
    across(
      .cols = everything(),
      .fns = function(x) factor(x, levels = c("YES","NO"))
    )
  )

# scoring each scale
survey_data[,12] <- survey_data %>% select(S1) %>% transmute(S1 = ifelse(S1 == "YES", 100,0))
survey_data[,13] <- survey_data %>% select(S2) %>% transmute(S2 = ifelse(S2 == "YES", 25,0))
survey_data[,14] <- survey_data %>% select(S3) %>% transmute(S3 = ifelse(S3 == "YES", 73,0))
survey_data[,15] <- survey_data %>% select(S4) %>% transmute(S4 = ifelse(S4 == "YES", 65,0))
survey_data[,16] <- survey_data %>% select(S5) %>% transmute(S5 = ifelse(S5 == "YES", 63,0))
survey_data[,17] <- survey_data %>% select(S6) %>% transmute(S6 = ifelse(S6 == "YES", 53,0))
survey_data[,18] <- survey_data %>% select(S7) %>% transmute(S7 = ifelse(S7 == "YES", 50,0))
survey_data[,19] <- survey_data %>% select(S8) %>% transmute(S8 = ifelse(S8 == "YES", 47,0))
survey_data[,20] <- survey_data %>% select(S9) %>% transmute(S9 = ifelse(S9 == "YES", 45,0))
survey_data[,21] <- survey_data %>% select(S10) %>% transmute(S10 = ifelse(S10 == "YES", 44,0))
survey_data[,22] <- survey_data %>% select(S11) %>% transmute(S11 = ifelse(S11 == "YES", 40,0))
survey_data[,23] <- survey_data %>% select(S12) %>% transmute(S12 = ifelse(S12 == "YES", 39,0))
survey_data[,24] <- survey_data %>% select(S13) %>% transmute(S13 = ifelse(S13 == "YES", 39,0))
survey_data[,25] <- survey_data %>% select(S14) %>% transmute(S14 = ifelse(S14 == "YES", 39,0))
survey_data[,26] <- survey_data %>% select(S15) %>% transmute(S15 = ifelse(S15 == "YES", 38,0))
survey_data[,27] <- survey_data %>% select(S16) %>% transmute(S16 = ifelse(S16 == "YES", 35,0))
survey_data[,28] <- survey_data %>% select(S17) %>% transmute(S17 = ifelse(S17 == "YES", 31,0))
survey_data[,29] <- survey_data %>% select(S18) %>% transmute(S18 = ifelse(S18 == "YES", 30,0))
survey_data[,30] <- survey_data %>% select(S19) %>% transmute(S19 = ifelse(S19 == "YES", 30,0))
survey_data[,31] <- survey_data %>% select(S20) %>% transmute(S20 = ifelse(S20 == "YES", 29,0))
survey_data[,32] <- survey_data %>% select(S21) %>% transmute(S21 = ifelse(S21 == "YES", 29,0))
survey_data[,33] <- survey_data %>% select(S22) %>% transmute(S22 = ifelse(S22 == "YES", 29,0))
survey_data[,34] <- survey_data %>% select(S23) %>% transmute(S23 = ifelse(S23 == "YES", 28,0))
survey_data[,35] <- survey_data %>% select(S24) %>% transmute(S24 = ifelse(S24 == "YES", 28,0))
survey_data[,36] <- survey_data %>% select(S25) %>% transmute(S25 = ifelse(S25 == "YES", 28,0))
survey_data[,37] <- survey_data %>% select(S26) %>% transmute(S26 = ifelse(S26 == "YES", 25,0))
survey_data[,38] <- survey_data %>% select(S27) %>% transmute(S27 = ifelse(S27 == "YES", 25,0))
survey_data[,39] <- survey_data %>% select(S28) %>% transmute(S28 = ifelse(S28 == "YES", 25,0))
survey_data[,40] <- survey_data %>% select(S29) %>% transmute(S29 = ifelse(S29 == "YES", 25,0))
survey_data[,41] <- survey_data %>% select(S30) %>% transmute(S30 = ifelse(S30 == "YES", 35,0))


# create stress score column
survey_data <- survey_data %>% mutate(stress_score = rowSums(across(c(12:34))))
# create stress state column
survey_data <- survey_data %>% mutate(stress_level = ifelse(stress_score < 150,"Very little",
                                                      ifelse(stress_score >= 150 & stress_score < 200,"Mild",
                                                             ifelse(stress_score >= 200 & stress_score < 250,"Moderate",
                                                                    ifelse(stress_score >= 250 & stress_score < 300,"Serious","Major")))))



# write new data to file
#write_rds(survey,"new_my_survey.rds")
write.csv(survey_data,"dataset/cleandf.csv")
View(survey_data)


