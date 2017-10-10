library(dplyr)
library(reshape2)
library(DT)

data2015 <- read.csv("/home/aeb/project/CollegeScorecard_Raw_Data/MERGED2014_15_PP.csv", stringsAsFactors=FALSE)

#Clean the data by eliminating rows which don't contain all the values we want to analyze

cleaned <- data2015 %>% filter(PREDDEG == "3")  #Only institutions which grant bachelor's degrees
cleaned <- cleaned %>% filter(GRAD_DEBT_MDN_SUPP != "PrivacySuppressed")
cleaned <- cleaned %>% filter(SAT_AVG != "NULL")
cleaned <- cleaned %>% filter(MD_FAMINC != "NULL" &
                              CDR3 != "NULL" &
                              PCTFLOAN != "NULL" &
                              PCTPELL != "NULL" &
                              LO_INC_RPY_3YR_RT != "NULL" & !is.na(LO_INC_RPY_3YR_RT) & LO_INC_RPY_3YR_RT != "PrivacySuppressed" &
                              MD_INC_RPY_3YR_RT != "NULL" & !is.na(MD_INC_RPY_3YR_RT) & MD_INC_RPY_3YR_RT != "PrivacySuppressed" &
                              HI_INC_RPY_3YR_RT != "NULL" & !is.na(HI_INC_RPY_3YR_RT) & HI_INC_RPY_3YR_RT != "PrivacySuppressed")

na_count = sapply(cleaned, function(x) sum(length(which(x == "NULL"))))

#Select only those rows we are interested in

cleaned <- cleaned %>% select(Institution = INSTNM,
                              State = STABBR,
                              Students = UGDS,
                              type=CONTROL,
                              `Admission Rate (%)` = ADM_RATE,
                              `Average Tuition ($)` = TUITFTE,
                              `Average Family Income ($)` = FAMINC,
                              `Median Family Income ($)` = MD_FAMINC,
                              `Loans (%)` = PCTFLOAN,
                              `Pell Grants (%)` = PCTPELL,
                              `Median Debt ($)` = GRAD_DEBT_MDN_SUPP,
                              `Default Rate (%)` = CDR3,
                              `Low Income Repayment Rate (3yr %)` = LO_INC_RPY_3YR_RT,
                              `Middle Income Repayment Rate (3yr %)` = MD_INC_RPY_3YR_RT,
                              `High Income Repayment Rate (3yr %)` = HI_INC_RPY_3YR_RT,
                              `Average Income (Dependents $)` = DEP_INC_AVG,
                              `Average Income (Independents $)` = IND_INC_AVG)

#Make type human readable and convert numerical rows from strings

cleaned <- cleaned %>%  mutate(`School Type`=ifelse(type==1, "Public", ifelse(type==2, "Private", "For-profit")),
                               `Admission Rate (%)`=as.double(`Admission Rate (%)`)) %>% select(-type)

cleaned <- cleaned %>% mutate(`Average Family Income ($)`=as.double(`Average Family Income ($)`),
                              `Median Family Income ($)`=as.double(`Median Family Income ($)`),
                              `Default Rate (%)`=as.double(`Default Rate (%)`),
                              `Low Income Repayment Rate (3yr %)`=as.double(`Low Income Repayment Rate (3yr %)`),
                              `Middle Income Repayment Rate (3yr %)`=as.double(`Middle Income Repayment Rate (3yr %)`),
                              `High Income Repayment Rate (3yr %)`=as.double(`High Income Repayment Rate (3yr %)`),
                              `Loans (%)`=as.double(`Loans (%)`),
                              `Pell Grants (%)`=as.double(`Pell Grants (%)`))

cleaned <- cleaned %>% mutate(`Average Tuition ($)`=as.double(`Average Tuition ($)`),
                              `Average Family Income ($)`=as.double(`Average Family Income ($)`),
                              `Median Family Income ($)`=as.double(`Median Family Income ($)`),
                              `Median Debt ($)`=as.double(`Median Debt ($)`),
                              `Low Income Repayment Rate (3yr %)`=as.double(`Low Income Repayment Rate (3yr %)`),
                              `Middle Income Repayment Rate (3yr %)`=as.double(`Middle Income Repayment Rate (3yr %)`),
                              `High Income Repayment Rate (3yr %)`=as.double(`High Income Repayment Rate (3yr %)`))

#Save the cleaned data
saveRDS(cleaned, "/home/aeb/project/CollegeScorecard_Raw_Data/cleaned_data.Rda")