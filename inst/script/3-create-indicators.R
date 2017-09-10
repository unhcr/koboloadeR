### Survey objects

household.survey <- svydesign(ids = ~ section1.location.district ,  data = household,  weights = ~Normalized.Weight ,  fpc = ~fpc )
case_number_details.survey <- svydesign(ids = ~ section1.location.district ,  data = case_number_details ,  weights = ~Normalized.Weight ,  fpc = ~fpc )
individual_biodata.survey <- svydesign(ids = ~ section1.location.district ,  data = individual_biodata ,  weights = ~Normalized.Weight ,  fpc = ~fpc )

### Create indicators

# 1. % of household have specific needs
needs <- individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.spec_needs.disability == "Yes"  | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.spec_needs.preg_lactating == "Yes" | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.spec_needs.chronic_illness == "Yes"  | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.spec_needs.temp_illness == "Yes"  | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.spec_needs.serious_med_cond == "Yes"  | ((individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.need_assistance == "Yes") & (individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age >=60))

#1. Average % children versus adults
child <- individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age <=18
adult <- individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age >=18
svyratio(~child, adult, design = individual_biodata.survey, na.rm=TRUE)

# 1. Average % of female versus male
M <- individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.sex == "Male"
F <- individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.sex == "Female"
svyratio(~F, M, design = individual_biodata.survey, na.rm=TRUE)

# 1. Children per household on average (%)
child.per.hh <- household$section2.tot_under_18/ household$section2.total_hh
svymean(~child.per.hh, household.survey)

# 1. Children per household on average (#)
svymean(~household$section2.tot_under_18, household.survey)

# 1. Mean household size
svymean(~household$section2.total_hh, household.survey)

# 5. HH applying crisis coping strategies
crisis <- household$section6.food_nonfood_strat.selling_assets == "Yes" | household$section6.food_nonfood_strat.reduce_essential == "Yes" | household$section6.food_nonfood_strat.reduce_edu == "Yes" | household$section6.food_nonfood_strat.no_school == "Yes" | household$section6.food_nonfood_strat.child_mariage == "Yes"

# 5. HH applying emergency coping strategies
emergency <- household$section6.food_nonfood_strat.sold_house == "Yes" | household$section6.food_nonfood_strat.child_labour == "Yes" | household$section6.food_nonfood_strat.begging == "Yes" | household$section6.food_nonfood_strat.exploitative_work == "Yes"

# 5. HH applying stress coping strategies
stress <- household$section6.food_nonfood_strat.selling_goods == "Yes" |  household$section6.food_nonfood_strat.spent_saving == "Yes" | household$section6.food_nonfood_strat.food_oncredit == "Yes" | household$income_expenditure.borrowing_debt.borrowed_Yesno == "Yes"

# 5. Percentage of households with residency permits
legal.res <- household$section7.critical_info_hh.no_legal_residence == 0
svymean(~legal.res, household.survey)

# 7. Total of % refugee population receiving assistance/versus no assistance (past yr)
any.assist <- case_number_details$section2.case_number_details.assistance.diff_ass.mcap_cash == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_blankets == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_stove == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_edu == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_shelter == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_furniture_clothes == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_water_storage == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_water_services == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_latrines == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_cooking == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_legal == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_other == "Yes"

# 7. Total of % refugee population receiving assistance/versus no assistance (in-kind assistance, past 3 months)
inkind.assist <- case_number_details$section2.case_number_details.assistance.tech_assist == "Yes"  | case_number_details$section2.case_number_details.assistance.technical_assistance == "Yes"  | case_number_details$section2.case_number_details.assistance.food_inkind == "Yes"  | case_number_details$section2.case_number_details.assistance.health_care == "Yes"  | case_number_details$section2.case_number_details.assistance.fuel_subsidy == "Yes"  | case_number_details$section2.case_number_details.assistance.rent_subsidy == "Yes"  | case_number_details$section2.case_number_details.assistance.hygiene_kits == "Yes"  | case_number_details$section2.case_number_details.assistance.other_nfi == "Yes"  | case_number_details$section2.case_number_details.assistance.edu_hygiene == "Yes"  | case_number_details$section2.case_number_details.assistance.desludging_ser == "Yes"  | case_number_details$section2.case_number_details.assistance.solid_waste_bins == "Yes"  | case_number_details$section2.case_number_details.assistance.solid_was_services == "Yes"  | case_number_details$section2.case_number_details.assistance.water_trucking == "Yes"

#############################################################################################################
# 8. Dependency - the variables needed for dependency are only available in the individual_biodata dataframe
#% CHH with dependents
#% Females headed households with dependents
#% HH by dependency ratio category (IIV)

#############################################################################################################

##HH compoisition - HH size
# 8. HH with less than 4 members
HH4 <- household$section2.total_hh <= 4

# 8. HH with 5 - 6 members
HH56 <- household$section2.total_hh >= 5 & household$section2.total_hh <=6

# 8. HH with more than 7 members
HH7 <- household$section2.total_hh >= 7

#############################################################################################################
#These are again only available in individual dataframe

# 8. % HH taking care of non-immediate related children
# below represents proportion of children not immediately related to an adult member of HH; it is not the proportion of HHs
#! check naming encoding
non.related <- individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.child_relation == "Extended family (uncle/aunt/cousin/niece/nephew etc)" | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.child_relation == "No family relationship" | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.child_relation == "Other (please specify)"
svymean(~non.related, individual_biodata.survey, na.rm=TRUE)


#8.  % HH with 100% dependents
#8.  % HH with children under 2
# under2 <- individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age < 2

#8.  % HH with children under 5
# under5 <- individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age < 5

#8.  % HH with elders above 59 (Segregated by the Sex of the HoH)
# over59 <- individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age > 59

#8.  % of HH with children 12-14
# 12to14 <- individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age >= 12 & individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age <= 14

#8.  % of HH with children 15-17
# 15to17 <- individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age >= 15 & individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age <= 17

#8.  % single headed households with dependents
#dependents <- individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age<=14 | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age>=65
#! needs to be per HH
#SHH <- (individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.civil_status == "Single (SN)") [(individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.hoh_relation == "Head of Household")]
# above does not work with survey object due to different length
#SHH <- individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.civil_status == "Single (SN)" & individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.hoh_relation == "Head of Household"

#8.  Mean dependency ratio

#8.  Mean household size
svymean(~household$section2.total_hh, household.survey, na.rm=TRUE)


#8.  Mean number of members by age category: <=5
#8.  Mean number of members by age category: 6 - 17
#8.  Mean number of members by age category: 18 - 65
#8.  Mean number of non-immediate related children per HH

#### Head of HH Age structure
### Need to convert DOB to age
household$section1.identify_interviewee.dob_HHHead
#ageHHH <- as.Date(household$section1.identify_interviewee.dob_HHHead, format("%d-%b-%y"))

# Get number of CHHH for % below
#CHHH <- household$age_HHHead>=18

#8. Proportion of CHHHs: 15 - 18
#HHH15.18 <- household$age_HHHead>=15 & household$age_HHHead<=18
#HHH15.18/ CHHH

#8. Proportion of CHHHs: < 15
#HHH15 <- household$age_HHHead<=15
#HHH15/ CHHH

#8. Proportion of elder headed household
#HHH59 <- household$age_HHHead>=59

# 8. Ratio of female to male headed households
FHHH <- household$section1.identify_interviewee.sex_HHHead == "female"
MHHH <- household$section1.identify_interviewee.sex_HHHead == "male"
svyratio(~FHHH, MHHH, design = household.survey, na.rm=TRUE)

# 8. Proportion of HH by HHH nationality
as.data.frame(summary(household$section1.identify_interviewee.nationality_hoh))
as.data.frame(svymean(~household$section1.identify_interviewee.nationality_hoh == "Syrian", design = household.survey, na.rm=TRUE))
as.data.frame(svymean(~household$section1.identify_interviewee.nationality_hoh == "Palestinian", design = household.survey, na.rm=TRUE))
as.data.frame(svymean(~household$section1.identify_interviewee.nationality_hoh == "Lebanese", design = household.survey, na.rm=TRUE))
as.data.frame(svymean(~household$section1.identify_interviewee.nationality_hoh == "Other (please specify)", design = household.survey, na.rm=TRUE))

# 8. Sex ratio
M <- individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.sex == "Male"
F <- individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.sex == "Female"
svyratio(~F, M, design = individual_biodata.survey, na.rm=TRUE)

######################### Needed for correlations ################################################################
# 11. Expenditure
# Survival Minimum  Expenditure Basket (SMEB) < US$ 87
SMEB <- (household$income_expenditure.expenditure.expenditure_total/1500)/household$section2.total_hh <87

#SMEB- Minimum Expenditure Basket (MEB) US$ 87 - US$ 113
SMEB.MEB <- (household$income_expenditure.expenditure.expenditure_total/1500)/household$section2.total_hh >=87 & (household$income_expenditure.expenditure.expenditure_total/1500)/household$section2.total_hh <=113

#MEB – 125% of MEB US $114 - US$ 142
MEB125 <- (household$income_expenditure.expenditure.expenditure_total/1500)/household$section2.total_hh >=114 & (household$income_expenditure.expenditure.expenditure_total/1500)/household$section2.total_hh <=142

#>125% MEB >US$ 143
MEB125.greater <- (household$income_expenditure.expenditure.expenditure_total/1500)/household$section2.total_hh > 143

#################################################################################################################
# Shelter - density
# Get density per HH member
HHdensitypp <- household$section3_household.housing.living_space/household$section2.total_hh

# 19. % HH by density group (number of m2 per person)   >10.5 m2/person
density4 <- HHdensitypp > 10.5
svymean(~density4, household.survey, na.rm = TRUE)

# 19. % HH by density group (number of m2 per person)   4.6 to 10.5 m2/person

density3 <- HHdensitypp >= 4.6 & HHdensity <= 10.5
svymean(~density3, household.survey, na.rm = TRUE)

# 19. % HH by density group (number of m2 per person)   3.6 to 4.5 m2/person
density2 <- HHdensitypp >= 3.6 & HHdensity <= 4.5
svymean(~density2, household.survey, na.rm = TRUE)

# 19. % HH by density group (number of m2 per person)  ≤3.5 m2/person
density1 <- HHdensitypp <= 3.5
svymean(~density1, household.survey, na.rm = TRUE)

# 19. % HH sharing latrine w./ 15 people or more
share.toil <- household$section3_household.wash.sharing_toilets >= 15
svymean(~share.toil, household.survey, na.rm = TRUE)

#19. % HH with access to bathroom (refer to WatSan)
wash.access <- household$section3_household.wash.toilets != 0
svymean(~wash.access, household.survey, na.rm = TRUE)

# 19. Average density
svymean(~HHdensitypp, household.survey, na.rm = TRUE)
#svyby(~HHdensitypp, ~houseloc, design = household.survey, svymean)
summary(HHdensitypp)

# 19. Median density

# 19. Average number of people per room
ppl.room.rm <- ((household$section2.total_hh/ household$section3_household.housing.num_rooms)[ !is.na(household$section3_household.housing.num_rooms)])
#can't reduce frame size and still use HHsurvey object
#ppl.room <- is.finite(household$section2.total_hh/ household$section3_household.housing.num_rooms)
# replace 0 in number of rooms
#replace(household$section3_household.housing.num_rooms, household$section3_household.housing.num_rooms==0, NA)
#ppl.room <- (household$section2.total_hh/ household$section3_household.housing.num_rooms)
summary(ppl.room)
svymean(~ppl.room, household.survey, na.rm = TRUE)
mean(ppl.room, na.rm = TRUE)
# double check this
svyratio(household$section2.total_hh, household$section3_household.housing.num_rooms, household.survey, na.rm = TRUE)


# 19. Median number of people per room

# 19. Average rent for HH renting
svymean(~household$section3_household.housing.rent_amount, household.survey, na.rm = TRUE)

# 19. Median rent for those households renting

# 19. Average  service costs for HH paying for services
service.cost <- (household$income_expenditure.expenditure.expenditure_All.expenditure_water + household$income_expenditure.expenditure.expenditure_All.expenditure_gas + household$income_expenditure.expenditure.expenditure_All.expenditure_fuel + household$income_expenditure.expenditure.expenditure_All.expenditure_electricity + household$income_expenditure.expenditure.expenditure_All.expenditure_comm)
svymean(~service.cost, household.survey, na.rm = TRUE)

# 19. Median service costs for HH paying for services

# 19. % shelters in lack of adequate WASH facilities (more 15 pp/toilet)
wash.inadeq <- household$section3_household.wash.sharing_toilets >= 15
svymean(~wash.inadeq, household.survey, na.rm = TRUE)

# 21. % HH that received the required secondary health assistance
prim.rec.req <- household$section7.critical_info.health_required2 == "Yes" & household$section7.critical_info.health_access2 == "Yes"
svymean(~prim.rec.req, household.survey, na.rm = TRUE)

# 21. % HH that required primary health assistance and could not get it  received the required assistance
prim.notrec.req <- household$section7.critical_info.health_required1 == "Yes" & household$section7.critical_info.health_access1 == "No"
svymean(~prim.notrec.req, household.survey, na.rm = TRUE)

# 21. % HH that required secondary health assistance and could not get it
sec.notrec.req <- household$section7.critical_info.health_required2 == "Yes" & household$section7.critical_info.health_access2 == "No"
svymean(~sec.notrec.req, household.survey, na.rm = TRUE)

# 24. % Child registered and % of not registered

# 24. % proof of birth certificate
birthdocs <- (individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.child_birth_details.leb_docs == "No documents" | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.child_birth_details.other_docs == "No documents")
#check
#birthdoccheck <- (individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.child_birth_details.leb_docs =="Birth notification issued by the doctor/midwife"|individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.child_birth_details.leb_docs =="Birth certificate issued by the Mukhtar"|individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.child_birth_details.leb_docs =="Birth certificate registered with the Nofous"|individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.child_birth_details.leb_docs =="Birth certificate registered with the Foreigners’ Registry"|individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.child_birth_details.leb_docs =="Birth certificate stamped by the Ministry of Foreign Affairs"|individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.child_birth_details.leb_docs =="Birth Certificate stamped by the Syrian Embassy"|individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.child_birth_details.leb_docs =="Family booklet or individual civil extract for the child or family civil extract"|individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.child_birth_details.other_docs =="Individual civil extract or family civil extract"|individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.child_birth_details.other_docs =="Family booklet"|individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.child_birth_details.other_docs =="Other (please specify)")
svymean(~birthdocs, individual_biodata.survey, na.rm = TRUE)


################## Graph indicators ######################
### 5. Respondants employing crisis coping strategies

Crisis <- household$section6.food_nonfood_strat.selling_assets == "Yes" | household$section6.food_nonfood_strat.reduce_essential == "Yes" | household$section6.food_nonfood_strat.reduce_edu == "Yes" | household$section6.food_nonfood_strat.no_school == "Yes" | household$section6.food_nonfood_strat.child_mariage == "Yes"
summary(Crisis)
svymean(~Crisis, household.survey)

#rm(frequ)
#rm(frequ.weight)
#rm(frequ.weight2)
#rm(frequ1)
#rm(frequ2)
#rm(frequ3)

frequ <- as.data.frame(table(Crisis))
figheight <- as.integer(nrow(frequ))
if ( figheight==0){ figheight<-1} else {figheight<-figheight/1.2}

frequ <- as.data.frame(table(Crisis))


## display table")

## Reorder factor")
frequ[ ,1] = factor(frequ[ ,1],levels(frequ[ ,1])[order(frequ$Freq, decreasing = TRUE)])
frequ <- frequ[ order(frequ[ , 1]) ,  ]
names(frequ)[1] <- "Crisis Coping Strategy"


## Frequency table with NA in order to get non response rate
frequ1 <- as.data.frame(prop.table(table(Crisis)))
frequ1 <- frequ1[!(is.na(frequ1$Crisis)), ]
#frequ1 <- frequ1[!(frequ1$Crisis=="NA", ]
percentreponse <- paste0(round(sum(frequ1$Freq)*100,digits=1),"%")

## Frequency table without NA
frequ2 <- as.data.frame(prop.table(table(Crisis,useNA = "no")))

## Frequency table with weight
frequ.weight <- as.data.frame(svymean(~ Crisis, design = household.survey, na.rm=TRUE))

## Binding the two
frequ3 <- cbind(frequ2,frequ.weight)


## Reorder factor
frequ2[ ,1] = factor(frequ2[ ,1],levels(frequ2[ ,1])[order(frequ2$Freq, decreasing = TRUE)])
frequ2 <- frequ2[ order(frequ2[ , 1]) ,  ]
frequ2[ ,3] <- paste0(round(frequ2[ ,2]*100,digits=1),"%")
names(frequ2)[3] <- "freqper2"
frequ3[ ,1] = factor(frequ3[ ,1],levels(frequ3[ ,1])[order(frequ3$mean, decreasing = TRUE)])
frequ3 <- frequ3[ order(frequ3[ , 1]) ,  ]
frequ3[ ,5] <- paste0(round(frequ3[ ,3]*100,digits=1), "%")
names(frequ3)[5] <- "freqper2"

library(ggplot2)

## and now the graph
ggplot(frequ3, aes(x=frequ3$Crisis, y=frequ3$mean)) +
  geom_bar(fill="#2a87c8",colour="#2a87c8", stat ="identity", width=.8) +
  guides(fill=FALSE) +
  geom_label_repel(aes(y = mean, label = freqper2), fill = "#2a87c8", color = 'white') +
  ylab("Frequency") +
  scale_y_continuous(labels=percent)+
  xlab("") +
  coord_flip() +
  ggtitle("Applied Crisis Coping Strategies") +
  theme(plot.title=element_text(face="bold", size=9),
        plot.background = element_rect(fill = "transparent",colour = NA))

### 5. Respondants employing emergency coping strategies

Emergency <- household$section6.food_nonfood_strat.sold_house == "Yes" | household$section6.food_nonfood_strat.child_labour == "Yes" | household$section6.food_nonfood_strat.begging == "Yes" | household$section6.food_nonfood_strat.exploitative_work == "Yes"
summary(Emergency)
svymean(~Emergency, household.survey)

#rm(frequ)
#rm(frequ.weight)
#rm(frequ.weight2)
#rm(frequ1)
#rm(frequ2)
#rm(frequ3)

frequ <- as.data.frame(table(Emergency))
figheight <- as.integer(nrow(frequ))
if ( figheight==0){ figheight<-1} else {figheight<-figheight/1.2}

frequ <- as.data.frame(table(Emergency))


## display table")

## Reorder factor")
frequ[ ,1] = factor(frequ[ ,1],levels(frequ[ ,1])[order(frequ$Freq, decreasing = TRUE)])
frequ <- frequ[ order(frequ[ , 1]) ,  ]
names(frequ)[1] <- "Emergency Coping Strategy"


## Frequency table with NA in order to get non response rate
frequ1 <- as.data.frame(prop.table(table(Emergency)))
frequ1 <- frequ1[!(is.na(frequ1$Emergency)), ]
#frequ1 <- frequ1[!(frequ1$Emergency=="NA", ]
percentreponse <- paste0(round(sum(frequ1$Freq)*100,digits=1),"%")

## Frequency table without NA
frequ2 <- as.data.frame(prop.table(table(Emergency,useNA = "no")))

## Frequency table with weight
frequ.weight <- as.data.frame(svymean(~ Emergency, design = household.survey, na.rm=TRUE))

## Binding the two
frequ3 <- cbind(frequ2,frequ.weight)


## Reorder factor
frequ2[ ,1] = factor(frequ2[ ,1],levels(frequ2[ ,1])[order(frequ2$Freq, decreasing = TRUE)])
frequ2 <- frequ2[ order(frequ2[ , 1]) ,  ]
frequ2[ ,3] <- paste0(round(frequ2[ ,2]*100,digits=1),"%")
names(frequ2)[3] <- "freqper2"
frequ3[ ,1] = factor(frequ3[ ,1],levels(frequ3[ ,1])[order(frequ3$mean, decreasing = TRUE)])
frequ3 <- frequ3[ order(frequ3[ , 1]) ,  ]
frequ3[ ,5] <- paste0(round(frequ3[ ,3]*100,digits=1), "%")
names(frequ3)[5] <- "freqper2"

library(ggplot2)

## and now the graph
ggplot(frequ3, aes(x=frequ3$Emergency, y=frequ3$mean)) +
  geom_bar(fill="#2a87c8",colour="#2a87c8", stat ="identity", width=.8) +
  guides(fill=FALSE) +
  geom_label_repel(aes(y = mean, label = freqper2), fill = "#2a87c8", color = 'white') +
  ylab("Frequency") +
  scale_y_continuous(labels=percent)+
  xlab("") +
  coord_flip() +
  ggtitle("Applied Emergency Coping Strategies") +
  theme(plot.title=element_text(face="bold", size=9),
        plot.background = element_rect(fill = "transparent",colour = NA))


### 5. Respondants employing stress coping strategies

Stress <- household$section6.food_nonfood_strat.selling_goods == "Yes" |  household$section6.food_nonfood_strat.spent_saving == "Yes" | household$section6.food_nonfood_strat.food_oncredit == "Yes" | household$income_expenditure.borrowing_debt.borrowed_yesno == "Yes"
summary(Stress)
svymean(~Stress, household.survey)

#rm(frequ)
#rm(frequ.weight)
#rm(frequ.weight2)
#rm(frequ1)
#rm(frequ2)
#rm(frequ3)

frequ <- as.data.frame(table(Stress))
figheight <- as.integer(nrow(frequ))
if ( figheight==0){ figheight<-1} else {figheight<-figheight/1.2}

frequ <- as.data.frame(table(Stress))


## display table")

## Reorder factor")
frequ[ ,1] = factor(frequ[ ,1],levels(frequ[ ,1])[order(frequ$Freq, decreasing = TRUE)])
frequ <- frequ[ order(frequ[ , 1]) ,  ]
names(frequ)[1] <- "Stress Coping Strategy"


## Frequency table with NA in order to get non response rate
frequ1 <- as.data.frame(prop.table(table(Stress)))
frequ1 <- frequ1[!(is.na(frequ1$Stress)), ]
#frequ1 <- frequ1[!(frequ1$Stress=="NA", ]
percentreponse <- paste0(round(sum(frequ1$Freq)*100,digits=1),"%")

## Frequency table without NA
frequ2 <- as.data.frame(prop.table(table(Stress,useNA = "no")))

## Frequency table with weight
frequ.weight <- as.data.frame(svymean(~ Stress, design = household.survey, na.rm=TRUE))

## Binding the two
frequ3 <- cbind(frequ2,frequ.weight)


## Reorder factor
frequ2[ ,1] = factor(frequ2[ ,1],levels(frequ2[ ,1])[order(frequ2$Freq, decreasing = TRUE)])
frequ2 <- frequ2[ order(frequ2[ , 1]) ,  ]
frequ2[ ,3] <- paste0(round(frequ2[ ,2]*100,digits=1),"%")
names(frequ2)[3] <- "freqper2"
frequ3[ ,1] = factor(frequ3[ ,1],levels(frequ3[ ,1])[order(frequ3$mean, decreasing = TRUE)])
frequ3 <- frequ3[ order(frequ3[ , 1]) ,  ]
frequ3[ ,5] <- paste0(round(frequ3[ ,3]*100,digits=1), "%")
names(frequ3)[5] <- "freqper2"

library(ggplot2)

## and now the graph
ggplot(frequ3, aes(x=frequ3$Stress, y=frequ3$mean)) +
  geom_bar(fill="#2a87c8",colour="#2a87c8", stat ="identity", width=.8) +
  guides(fill=FALSE) +
  geom_label_repel(aes(y = mean, label = freqper2), fill = "#2a87c8", color = 'white') +
  ylab("Frequency") +
  scale_y_continuous(labels=percent)+
  xlab("") +
  coord_flip() +
  ggtitle("Applied Stress Coping Strategies") +
  theme(plot.title=element_text(face="bold", size=9),
        plot.background = element_rect(fill = "transparent",colour = NA))

#### Emergency or crisis coping strategies
svymean(~ (Crisis | Emergency) , design = household.survey, na.rm=TRUE)

### 7. Number receiving any kind of assistance

Any.assist <- case_number_details$section2.case_number_details.assistance.diff_ass.mcap_cash == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_blankets == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_stove == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_edu == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_shelter == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_furniture_clothes == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_water_storage == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_water_services == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_latrines == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_cooking == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_legal == "Yes" | case_number_details$section2.case_number_details.assistance.diff_ass.ass_year_other == "Yes"
summary(Any.assist)
svymean(~Any.assist, case_number_details.survey)

#rm(frequ)
#rm(frequ.weight)
#rm(frequ.weight2)
#rm(frequ1)
#rm(frequ2)
#rm(frequ3)

frequ <- as.data.frame(table(Any.assist))
figheight <- as.integer(nrow(frequ))
if ( figheight==0){ figheight<-1} else {figheight<-figheight/1.2}

frequ <- as.data.frame(table(Any.assist))


## display table")

## Reorder factor")
frequ[ ,1] = factor(frequ[ ,1],levels(frequ[ ,1])[order(frequ$Freq, decreasing = TRUE)])
frequ <- frequ[ order(frequ[ , 1]) ,  ]
names(frequ)[1] <- "Any.assist"


## Frequency table with NA in order to get non response rate
frequ1 <- as.data.frame(prop.table(table(Any.assist)))
frequ1 <- frequ1[!(is.na(frequ1$Any.assist)), ]
#frequ1 <- frequ1[!(frequ1$Any.assist=="NA", ]
percentreponse <- paste0(round(sum(frequ1$Freq)*100,digits=1),"%")

## Frequency table without NA
frequ2 <- as.data.frame(prop.table(table(Any.assist,useNA = "no")))

## Frequency table with weight
frequ.weight <- as.data.frame(svymean(~ Any.assist, design = case_number_details.survey, na.rm=TRUE))

## Binding the two
frequ3 <- cbind(frequ2,frequ.weight)


## Reorder factor
frequ2[ ,1] = factor(frequ2[ ,1],levels(frequ2[ ,1])[order(frequ2$Freq, decreasing = TRUE)])
frequ2 <- frequ2[ order(frequ2[ , 1]) ,  ]
frequ2[ ,3] <- paste0(round(frequ2[ ,2]*100,digits=1),"%")
names(frequ2)[3] <- "freqper2"
frequ3[ ,1] = factor(frequ3[ ,1],levels(frequ3[ ,1])[order(frequ3$mean, decreasing = TRUE)])
frequ3 <- frequ3[ order(frequ3[ , 1]) ,  ]
frequ3[ ,5] <- paste0(round(frequ3[ ,3]*100,digits=1), "%")
names(frequ3)[5] <- "freqper2"

library(ggplot2)

## and now the graph
ggplot(frequ3, aes(x=frequ3$Any.assist, y=frequ3$mean)) +
  geom_bar(fill="#2a87c8",colour="#2a87c8", stat ="identity", width=.8) +
  guides(fill=FALSE) +
  geom_label_repel(aes(y = mean, label = freqper2), fill = "#2a87c8", color = 'white') +
  ylab("Frequency") +
  scale_y_continuous(labels=percent)+
  xlab("") +
  coord_flip() +
  ggtitle("Receiving some kind of assistance") +
  theme(plot.title=element_text(face="bold", size=9),
        plot.background = element_rect(fill = "transparent",colour = NA))


### 7. Number receiving in-kind assistance in the past 3 months

inkind.assist <- case_number_details$section2.case_number_details.assistance.tech_assist == "Yes"  | case_number_details$section2.case_number_details.assistance.technical_assistance == "Yes"  | case_number_details$section2.case_number_details.assistance.food_inkind == "Yes"  | case_number_details$section2.case_number_details.assistance.health_care == "Yes"  | case_number_details$section2.case_number_details.assistance.fuel_subsidy == "Yes"  | case_number_details$section2.case_number_details.assistance.rent_subsidy == "Yes"  | case_number_details$section2.case_number_details.assistance.hygiene_kits == "Yes"  | case_number_details$section2.case_number_details.assistance.other_nfi == "Yes"  | case_number_details$section2.case_number_details.assistance.edu_hygiene == "Yes"  | case_number_details$section2.case_number_details.assistance.desludging_ser == "Yes"  | case_number_details$section2.case_number_details.assistance.solid_waste_bins == "Yes"  | case_number_details$section2.case_number_details.assistance.solid_was_services == "Yes"  | case_number_details$section2.case_number_details.assistance.water_trucking == "Yes"
summary(inkind.assist)
svymean(~inkind.assist, case_number_details.survey, na.rm=TRUE)

#rm(frequ)
#rm(frequ.weight)
#rm(frequ.weight2)
#rm(frequ1)
#rm(frequ2)
#rm(frequ3)

frequ <- as.data.frame(table(inkind.assist))
figheight <- as.integer(nrow(frequ))
if ( figheight==0){ figheight<-1} else {figheight<-figheight/1.2}

frequ <- as.data.frame(table(inkind.assist))


## display table")

## Reorder factor")
frequ[ ,1] = factor(frequ[ ,1],levels(frequ[ ,1])[order(frequ$Freq, decreasing = TRUE)])
frequ <- frequ[ order(frequ[ , 1]) ,  ]
names(frequ)[1] <- "inkind.assist"


## Frequency table with NA in order to get non response rate
frequ1 <- as.data.frame(prop.table(table(inkind.assist)))
frequ1 <- frequ1[!(is.na(frequ1$inkind.assist)), ]
#frequ1 <- frequ1[!(frequ1$inkind.assist=="NA", ]
percentreponse <- paste0(round(sum(frequ1$Freq)*100,digits=1),"%")

## Frequency table without NA
#frequ2 <- as.data.frame(prop.table(table(inkind.assist,useNA = "no")))

## Frequency table with weight
frequ.weight <- as.data.frame(svymean(~ inkind.assist, design = case_number_details.survey, na.rm=TRUE))

## Binding the two
frequ3 <- cbind(frequ2,frequ.weight)


## Reorder factor
frequ2[ ,1] = factor(frequ2[ ,1],levels(frequ2[ ,1])[order(frequ2$Freq, decreasing = TRUE)])
frequ2 <- frequ2[ order(frequ2[ , 1]) ,  ]
frequ2[ ,3] <- paste0(round(frequ2[ ,2]*100,digits=1),"%")
names(frequ2)[3] <- "freqper2"
frequ3[ ,1] = factor(frequ3[ ,1],levels(frequ3[ ,1])[order(frequ3$mean, decreasing = TRUE)])
frequ3 <- frequ3[ order(frequ3[ , 1]) ,  ]
frequ3[ ,5] <- paste0(round(frequ3[ ,3]*100,digits=1), "%")
names(frequ3)[5] <- "freqper2"

library(ggplot2)

## and now the graph
ggplot(frequ3, aes(x=frequ3$inkind.assist, y=frequ3$mean)) +
  geom_bar(fill="#2a87c8",colour="#2a87c8", stat ="identity", width=.8) +
  guides(fill=FALSE) +
  geom_label_repel(aes(y = mean, label = freqper2), fill = "#2a87c8", color = 'white') +
  ylab("Frequency") +
  scale_y_continuous(labels=percent)+
  xlab("") +
  coord_flip() +
  ggtitle("Receiving in-kind assistance") +
  theme(plot.title=element_text(face="bold", size=9),
        plot.background = element_rect(fill = "transparent",colour = NA))

## check those not receiving any in-kind assistance

summary(case_number_details$section2.case_number_details.assistance.tech_assist == "No"  & case_number_details$section2.case_number_details.assistance.technical_assistance == "No"  & case_number_details$section2.case_number_details.assistance.food_inkind == "No"  & case_number_details$section2.case_number_details.assistance.health_care == "No"  & case_number_details$section2.case_number_details.assistance.fuel_subsidy == "No"  & case_number_details$section2.case_number_details.assistance.rent_subsidy == "No"  & case_number_details$section2.case_number_details.assistance.hygiene_kits == "No"  & case_number_details$section2.case_number_details.assistance.other_nfi == "No"  & case_number_details$section2.case_number_details.assistance.edu_hygiene == "No"  & case_number_details$section2.case_number_details.assistance.desludging_ser == "No"  & case_number_details$section2.case_number_details.assistance.solid_waste_bins == "No"  & case_number_details$section2.case_number_details.assistance.solid_was_services == "No"  & case_number_details$section2.case_number_details.assistance.water_trucking == "No")

#### Dependency

dependency <- individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age <= 15 | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age >= 65

summary(dependency)



##### HH w/ less than 4 members

household$section2.total_hh <= 4
summary(household$section2.total_hh <= 4)
svymean(~household$section2.total_hh <= 4, design = household.survey, na.rm=TRUE)
svymean(~household$section2.total_hh, design = household.survey, na.rm=TRUE)

### 1. % of household have specific needs
## ! this needs to be fixed for HH level
needs <- individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.spec_needs.disability == "Yes"  | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.spec_needs.preg_lactating == "Yes" | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.spec_needs.chronic_illness == "Yes"  | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.spec_needs.temp_illness == "Yes"  | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.spec_needs.serious_med_cond == "Yes"  | ((individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.need_assistance == "Yes") & (individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.age >=60))
summary(needs)
svymean(~needs, individual_biodata.survey, na.rm=TRUE)

frequ <- as.data.frame(table(needs))
figheight <- as.integer(nrow(frequ))
if ( figheight==0){ figheight<-1} else {figheight<-figheight/1.2}

frequ <- as.data.frame(table(needs))


## display table")

## Reorder factor")
frequ[ ,1] = factor(frequ[ ,1],levels(frequ[ ,1])[order(frequ$Freq, decreasing = TRUE)])
frequ <- frequ[ order(frequ[ , 1]) ,  ]
names(frequ)[1] <- "needs Coping Strategy"


## Frequency table with NA in order to get non response rate
frequ1 <- as.data.frame(prop.table(table(needs)))
frequ1 <- frequ1[!(is.na(frequ1$needs)), ]
#frequ1 <- frequ1[!(frequ1$needs=="NA", ]
percentreponse <- paste0(round(sum(frequ1$Freq)*100,digits=1),"%")

## Frequency table without NA
frequ2 <- as.data.frame(prop.table(table(needs,useNA = "no")))

## Frequency table with weight
frequ.weight <- as.data.frame(svymean(~ needs, design = individual_biodata.survey, na.rm=TRUE))

## Binding the two
frequ3 <- cbind(frequ2,frequ.weight)


## Reorder factor
frequ2[ ,1] = factor(frequ2[ ,1],levels(frequ2[ ,1])[order(frequ2$Freq, decreasing = TRUE)])
frequ2 <- frequ2[ order(frequ2[ , 1]) ,  ]
frequ2[ ,3] <- paste0(round(frequ2[ ,2]*100,digits=1),"%")
names(frequ2)[3] <- "freqper2"
frequ3[ ,1] = factor(frequ3[ ,1],levels(frequ3[ ,1])[order(frequ3$mean, decreasing = TRUE)])
frequ3 <- frequ3[ order(frequ3[ , 1]) ,  ]
frequ3[ ,5] <- paste0(round(frequ3[ ,3]*100,digits=1), "%")
names(frequ3)[5] <- "freqper2"

library(ggplot2)

## and now the graph
ggplot(frequ3, aes(x=frequ3$needs, y=frequ3$mean)) +
  geom_bar(fill="#2a87c8",colour="#2a87c8", stat ="identity", width=.8) +
  guides(fill=FALSE) +
  geom_label_repel(aes(y = mean, label = freqper2), fill = "#2a87c8", color = 'white') +
  ylab("Frequency") +
  scale_y_continuous(labels=percent)+
  xlab("") +
  coord_flip() +
  ggtitle("Individuals w/ specific needs") +
  theme(plot.title=element_text(face="bold", size=9),
        plot.background = element_rect(fill = "transparent",colour = NA))

#####################################################################################
## frequency table for shelter type## Does not work

frequ <- as.data.frame(table(housetype))
figheight <- as.integer(nrow(frequ))
if ( figheight==0){ figheight<-1} else {figheight<-figheight/1.2}

frequ <- as.data.frame(table(housetype))


## display table")

## Reorder factor")
frequ[ ,1] = factor(frequ[ ,1],levels(frequ[ ,1])[order(frequ$Freq, decreasing = TRUE)])
frequ <- frequ[ order(frequ[ , 1]) ,  ]
names(frequ)[1] <- "housetype Coping Strategy"


## Frequency table with NA in order to get non response rate
frequ1 <- as.data.frame(prop.table(table(housetype)))
frequ1 <- frequ1[!(is.na(frequ1$housetype)), ]
#frequ1 <- frequ1[!(frequ1$housetype=="NA", ]
percentreponse <- paste0(round(sum(frequ1$Freq)*100,digits=1),"%")

## Frequency table without NA
frequ2 <- as.data.frame(prop.table(table(housetype,useNA = "no")))

## Frequency table with weight
frequ.weight <- as.data.frame(svymean(~ housetype, design = household.survey, na.rm=TRUE))

## Binding the two
frequ3 <- cbind(frequ2,frequ.weight)


## Reorder factor
frequ2[ ,1] = factor(frequ2[ ,1],levels(frequ2[ ,1])[order(frequ2$Freq, decreasing = TRUE)])
frequ2 <- frequ2[ order(frequ2[ , 1]) ,  ]
frequ2[ ,3] <- paste0(round(frequ2[ ,2]*100,digits=1),"%")
names(frequ2)[3] <- "freqper2"
frequ3[ ,1] = factor(frequ3[ ,1],levels(frequ3[ ,1])[order(frequ3$mean, decreasing = TRUE)])
frequ3 <- frequ3[ order(frequ3[ , 1]) ,  ]
frequ3[ ,5] <- paste0(round(frequ3[ ,3]*100,digits=1), "%")
names(frequ3)[5] <- "freqper2"

library(ggplot2)

## and now the graph
ggplot(frequ3, aes(x=frequ3$housetype, y=frequ3$mean)) +
  geom_bar(fill="#2a87c8",colour="#2a87c8", stat ="identity", width=.8) +
  guides(fill=FALSE) +
  geom_label_repel(aes(y = mean, label = freqper2), fill = "#2a87c8", color = 'white') +
  ylab("Frequency") +
  scale_y_continuous(labels=percent)+
  xlab("") +
  coord_flip() +
  ggtitle("Applied housetype Coping Strategies") +
  theme(plot.title=element_text(face="bold", size=9),
        plot.background = element_rect(fill = "transparent",colour = NA))

##########################

#################### Add Variables & Indicators for Correlations ###########################

# 11. % child’s closest family relationship to an adult member of the HH:  Brother/Siaster
#(individual_biodata$child_relation == "son_daughter", )

# 11. % child’s closest family relationship to an adult member of the HH:  Brother-in-law/Sister-in-law
#(individual_biodata$child_relation == "husband_wife", )

# 11. % child’s closest family relationship to an adult member of the HH:  Extended family
#(uncle/aunt/cousin/niece/nephew) (individual_biodata$child_relation == "brother_sister", )

# 11. % child’s closest family relationship to an adult member of the HH:  Mother-in-law/Father-in-law
#(individual_biodata$child_relation == "mother-in-law_father-in-law", )

# 11. % child’s closest family relationship to an adult member of the HH:  No family relationship
#(individual_biodata$child_relation == "brother-in-law_sister-in-law", )

# 11. % child’s closest family relationship to an adult member of the HH:  Other (please specify)
#(individual_biodata$child_relation == "extended_family", )

# 11. % child’s closest family relationship to an adult member of the HH: Husband/Wife
#(individual_biodata$child_relation == "no_family", )

# 11. % child’s closest family relationship to an adult member of the HH: Son/Daughter
#(individual_biodata$child_relation == "other", )

# 11. Demographics: households with chronic diseases by family size, above/below S/MEB, geographical location, shelter type (location)
#(individual_biodata$chronic_illness == "yes", household$location)

# 11. Demographics: households with chronic diseases by family size, above/below S/MEB, geographical location, shelter type (family size)
#(individual_biodata$chronic_illness == "yes", household$total_hh)

# 11. Demographics: households with chronic diseases by family size, above/below S/MEB, geographical location, shelter type (shelter)
#(individual_biodata$chronic_illness == "yes", household$type_of_housing)

# 11. Demographics: households with chronic diseases by family size, above/below S/MEB, geographical location, shelter type (S/MEB)
#(individual_biodata$chronic_illness == "yes", (household$expenditure_total/1500)/household$total_hh <87)

# 11. Demographics: households with chronic diseases by family size, above/below S/MEB, geographical location, shelter type (S/MEB)
#(individual_biodata$chronic_illness == "yes", (household$expenditure_total/1500)/household$total_hh >=87 & (household$expenditure_total/1500)/household$total_hh <=113)

# 11. Demographics: households with chronic diseases by family size, above/below S/MEB, geographical location, shelter type (S/MEB)
#(individual_biodata$chronic_illness == "yes", (household$expenditure_total/1500)/household$total_hh >=114 & (household$expenditure_total/1500)/household$total_hh <=142)

# 11. Demographics: households with chronic diseases by family size, above/below S/MEB, geographical location, shelter type (S/MEB)
#(individual_biodata$chronic_illness == "yes", (household$expenditure_total/1500)/household$total_hh > 143)

# 11. Persons with specific needs: effect on geographical location
#(individual_biodata$disability == "yes"  | individual_biodata$preg_lactating == "yes" | individual_biodata$chronic_illness == "yes"  | individual_biodata$temp_illness == "yes"  | individual_biodata$serious_med_cond == "yes"  | (individual_biodata$need_assistance == "yes" & individual_biodata$age >=60), household$location)

# 11. Persons with specific needs: effect on overall expenditure patterns of the family
#(individual_biodata$disability == "yes"  | individual_biodata$preg_lactating == "yes" | individual_biodata$chronic_illness == "yes"  | individual_biodata$temp_illness == "yes"  | individual_biodata$serious_med_cond == "yes"  | (individual_biodata$need_assistance == "yes" & individual_biodata$age >=60), household$total_expenditure)

# 11. Persons with specific needs: effect on variation per shelter type
#(individual_biodata$disability == "yes"  | individual_biodata$preg_lactating == "yes" | individual_biodata$chronic_illness == "yes"  | individual_biodata$temp_illness == "yes"  | individual_biodata$serious_med_cond == "yes"  | (individual_biodata$need_assistance == "yes" & individual_biodata$age >=60), household$type_of_housing)

# 11. Persons with specific needs: effect on the above/below S/MEB
#(individual_biodata$disability == "yes"  | individual_biodata$preg_lactating == "yes" | individual_biodata$chronic_illness == "yes"  | individual_biodata$temp_illness == "yes"  | individual_biodata$serious_med_cond == "yes"  | (individual_biodata$need_assistance == "yes" & individual_biodata$age >=60), (household$expenditure_total/1500)/household$total_hh <87)

# 11. Persons with specific needs: effect on the above/below S/MEB
#(individual_biodata$disability == "yes"  | individual_biodata$preg_lactating == "yes" | individual_biodata$chronic_illness == "yes"  | individual_biodata$temp_illness == "yes"  | individual_biodata$serious_med_cond == "yes"  | (individual_biodata$need_assistance == "yes" & individual_biodata$age >=60), (household$expenditure_total/1500)/household$total_hh >=87 & (household$expenditure_total/1500)/household$total_hh <=113)

# 11. Persons with specific needs: effect on the above/below S/MEB
#(individual_biodata$disability == "yes"  | individual_biodata$preg_lactating == "yes" | individual_biodata$chronic_illness == "yes"  | individual_biodata$temp_illness == "yes"  | individual_biodata$serious_med_cond == "yes"  | (individual_biodata$need_assistance == "yes" & individual_biodata$age >=60), (household$expenditure_total/1500)/household$total_hh >=114 & (household$expenditure_total/1500)/household$total_hh <=142)

# 11. Persons with specific needs: effect on the above/below S/MEB
#(individual_biodata$disability == "yes"  | individual_biodata$preg_lactating == "yes" | individual_biodata$chronic_illness == "yes"  | individual_biodata$temp_illness == "yes"  | individual_biodata$serious_med_cond == "yes"  | (individual_biodata$need_assistance == "yes" & individual_biodata$age >=60), (household$expenditure_total/1500)/household$total_hh > 143)

# 16. Expenditures on health: trends out of the total HH expenditures across different vulnerability levels
#(household$expenditure_health/household$expenditure_total, household$expenditure_health)

# 16. Expenditures on health: trends out of the total HH expenditures across different vulnerability levels
#(household$expenditure_health/household$expenditure_total, household$observations)

# 16. Location and Coping strategy (reducing non-food expenses on health/drugs) (Location and non-food coping strategies)
#(household$reduce_essential, household$location)

#locationon <- recode(household$section1.location.district, as.factor.result=FALSE)

# 18. % of unregistered members
#(household$nbr_of_unreg, )
# 18. Demographics: legal residency status of households disaggregated by family size, family composition, geographical location, shelter type, and education level (if any correlation) (, )

# 18. Economic vulnerability and residency status
#(household$expenditure_total/household$total_hh, household$have_legal_residence/household$total_hh)

# 18. Arrival time and socio-economic vulnerabilities: How coping strategies are developed overtime? (crisis)
#(household$arrival_date, household$selling_assets == "yes" | household$reduce_essential == "yes" | household$reduce_edu == "yes" | household$no_school == "yes" | household$child_mariage == "yes" )

# 18. Arrival time and socio-economic vulnerabilities: How coping strategies are developed overtime? (emergency)
#(household$arrival_date, household$sold_house == "yes" | household$child_labour == "yes" | household$begging == "yes" | household$exploitative_work == "yes" )

# 18. Arrival time and socio-economic vulnerabilities: How coping strategies are developed overtime? (stress)
##(household$arrival_date, household$selling_goods == "yes" |  household$spent_saving == "yes" | household$food_oncredit == "yes" | household$borrowed_yesno == "yes")

# 18. Arrival time and socio-economic vulnerabilities: How vulnerabilities are shifting overtime?
#(household$arrival_date, (household$expenditure_total/1500)/household$total_hh <87)

# 18. Arrival time and socio-economic vulnerabilities: How vulnerabilities are shifting overtime?
#(household$arrival_date, (household$expenditure_total/1500)/household$total_hh >=87 & (household$expenditure_total/1500)/household$total_hh <=113)

# 18. Arrival time and socio-economic vulnerabilities: How vulnerabilities are shifting overtime?
#(household$arrival_date, (household$expenditure_total/1500)/household$total_hh >=114 & (household$expenditure_total/1500)/household$total_hh <=142)

# 18. Arrival time and socio-economic vulnerabilities: How vulnerabilities are shifting overtime?
#(household$arrival_date, (household$expenditure_total/1500)/household$total_hh > 143)

# 18. Residency status and above/below SMEB (difficulties_encountered$no_legal_residence == 0,
#(household$expenditure_total/1500)/household$total_hh <87)

# 18. Residency status and above/below SMEB (difficulties_encountered$no_legal_residence == 0,
#(household$expenditure_total/1500)/household$total_hh >=114)

# 18. Residency status and above/below SMEB (legal_residence$residence_cat,
#(household$expenditure_total/1500)/household$total_hh <87)

# 18. Residency status and above/below SMEB (legal_residence$residence_cat,
#(household$expenditure_total/1500)/household$total_hh >=114)

# 18. Residency status and debt
#(difficulties_encountered$no_legal_residence == 0, household$debt_total)

# 18. Residency status and education level
#(difficulties_encountered$no_legal_residence == 0, individual_biodata$highest_grade)

# 18. Residency status and food insecurity
#(difficulties_encountered$no_legal_residence == 0, )

# 18. Residency status and geographical location
#(difficulties_encountered$no_legal_residence == 0, household$location)

# 18. Residency status and work opportunities:
#(difficulties_encountered$no_legal_residence == 0, individual_biodata$work_type)

# 18. To test: more income  families have more members with legal stay (are able to pay the fees)
#(household$total_income, difficulties_encountered$no_legal_residence)

# 20. Average rental prices per type of shelter or region (shelter)
#(household$rent_amount, household$type_of_housing)

# 20. Average rental prices per type of shelter or region (location)
#(household$rent_amount, household$location)

# 20. Expenditure patterns: Total expenditures variations by shelter type
#(from a households economics perspective) ((household$expenditure_total/1500), household$type_of_housing)

# 20. Shelter type per  S/MEB level
#((household$expenditure_total/1500)/household$total_hh <87, household$type_of_housing)

# 20. Shelter type per  S/MEB level
#((household$expenditure_total/1500)/household$total_hh >=87 & (household$expenditure_total/1500)/household$total_hh <=113, household$type_of_housing)

# 20. Shelter type per  S/MEB level
#((household$expenditure_total/1500)/household$total_hh >=114 & (household$expenditure_total/1500)/household$total_hh <=142, household$type_of_housing)

# 20. Shelter type per  S/MEB level
#((household$expenditure_total/1500)/household$total_hh > 143, household$type_of_housing)

# 22. Barriers for accessing health
#(household$primary_health, household$health_access_bar)

# 23. Knowledge about access to medical assistance/services and location
#(Geographic disparities in knowledge about access to health services) (household$health_access_bar == "dont_know"   |  household$health_access_bar2 == "dont_know", household$location)

# 23. Location and getting the required PHC assistance (yes/no)
#(Geographic disparities in access to PHC) (household$location, household$health_access1 == "yes")

# 23. Location and getting the required SHC assistance (Geographic disparities in access to SHC)
#(household$location, household$health_access2 == "yes")

# 23. Location and reason for not getting the required PHC assistance (Geographic disparities in access to PHC)
#(household$location, household$health_access_bar)

# 23. Location and reason for not getting the required SHC assistance (Geographic disparities in access to SHC)
#(household$location, household$health_access_bar2)

# 23. Location and type of PHC assistance received (Geographic disparities in access to PHC)
#(household$location, household$primary_health)

# 23. Location and type of SHC assistance received (Geographic disparities in access to SHC)
#(household$location, household$secondary_health)

# 23. Registration and getting the required PHC assistance (knowledge about and access to health assistance/services)
#(household$all_registered == "yes", household$health_access1 == "yes")

# 23. Registration and getting the required SHC assistance (knowledge about and access to health assistance/services)
#(household$all_registered == "yes", household$health_access2 == "yes")

# 23. Registration and knowledge about access to medical assistance/services  (knowledge about and access to health assistance/services)
#(household$all_registered == "yes", household$health_access_bar == "dont_know")

# 23. Relationship between refugees and host community and getting the required PHC assistance (Social stability and access to health assistance)
#(household$insecurity_cause == "neighbors", household$health_access1 == "yes")

# 23. Health and residency status
#(household$health_access1 == "yes" | household$health_access2 == "yes", difficulties_encountered$no_legal_residence)

# 28. Number of Social cohesion incident / Security incident within the community and between Refugees and host community per geographical area, per vulnerability, + comparison with 2013, 2014 , 2015
#(household$insecurity_cause == "neighbors", household$insecurity_cause == "refugee_orgs")

# 37. % overlap between food insecurity and economic vulnerability

############################################################################################

# 8. % HH taking care of non-immediate related children
#! This also should be presented at HH level

norelation <- individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.child_relation == "Extended family (uncle/aunt/cousin/niece/nephew etc)" | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.child_relation == "No family relationship" | individual_biodata$section2.case_number_details.case_number_individuals.individual_biodata.child_relation == "Other (please specify)"
svymean(~norelation, individual_biodata.survey, na.rm=TRUE)

## Number unregistered
mean(household$section2.nbr_of_unreg, na.rm=TRUE)
svymean(~household$section2.nbr_of_unreg, household.survey, na.rm=TRUE)
plot(histogram(~section2.nbr_of_unreg, household, nint = 25, xlab = "Number Unregistered"))
