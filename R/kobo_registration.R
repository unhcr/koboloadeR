#' @name kobo_registration
#' @rdname kobo_registration
#' @title  Retrieve registration data from UNHCR proGres database and generate a summary report.
#'
#' @description    A standard query that retrieve refugee registration data from proGres,
#' aggregated at the case level. This dataset can be joined to the survey dataset in order to
#' generate prediction model. to be used with the prediction report generation.
#'
#' This includes the variables below
#'
#'  Arrival Date
#'  Districts of Origin
#'  Districts of Arrival
#'  Household size (case)
#'  Household size (squared)
#'  Share of members under 5 years of age
#'  Share of members between 5 and 17 years of age
#'  Share of male members between 18 and 50
#'  Share of female members between 18 and 50
#'  Share of members between 51 and 70
#'  Share of members above 71
#'  Share of members between 6 and 10 years of age
#'  Share of members between 11 and 17 years of age
#'  Share of members between 18 and 60 years of age
#'  Share of members above 60 years of age
#'  Sum of members under 5 years of age
#'  Sum of members between 6 and 10 years of age
#'  Sum of members between 11 and 17 years of age
#'  Sum of members between 18 and 60 years of age
#'  Sum of members above 60 years of age
#'  Share of members with a disability
#'  Sum of members with a disability
#'  Members above 60 years of age with a medical condition
#'  Dependency ratio
#'  Dependent members with a disability
#'  More than 3 dependents in HH
#'
#'
#'Demographics - Head of Household variables
#'  Head of HH is female
#'  Head of HH age
#'  Head above 60 years of age
#'  Head of HH is female and below 18 years of age
#'  Head of HH is disabled
#'  Head of HH education level
#'  Head of HH with a medical condition
#'  Head of HH below 18

#'
#'
#' @return save a cleaned csv file within the data folder.
#'
#' @export kobo_registration
#'
#' @author Edouard Legoupil
#'
#' @examples
#' kobo_registration()
#'


kobo_registration <- function() {
require(RODBC)

## Db handle for progres Data to SQL server using ODBC##################################
odbcname <- rstudioapi::askForPassword("Give the odbc db name")
user <- rstudioapi::askForPassword("Enter the username that can access the database:")
passw <- rstudioapi::askForPassword("Database password")

dbhandleprogresv3 <- odbcConnect(odbcname, uid = user, pwd = passw)
rm(user, passw)


query <- "
DROP TABLE caseprofile1;
SELECT  PP.ProcessingGroupNumber CaseNo,
COUNT(DISTINCT II.IndividualGUID) Num_Inds,
AVG(II.IndividualAge) AVG_Age,
STDEV(II.IndividualAge) STDEV_Age,
Count( CASE WHEN(II.IndividualAge < 15) THEN(II.IndividualGUID) ELSE(NULL) END) Child_0_14,
Count( CASE WHEN(II.IndividualAge < 19 AND IndividualAge > 14) THEN(II.IndividualGUID) ELSE(NULL) END) Youth_15_17,
Count( CASE WHEN(II.IndividualAge < 65 AND IndividualAge > 14) THEN(II.IndividualGUID) ELSE(NULL) END) Work_15_64,
Count( CASE WHEN(II.IndividualAge > 64) THEN(II.IndividualGUID) ELSE(NULL) END) Eldern_65,
Count( CASE WHEN(II.SexCode = 'M') THEN(SexCode) ELSE(NULL) END) Male,
Count( CASE WHEN(II.SexCode = 'F') THEN(SexCode) ELSE(NULL) END) Female,
Count( CASE WHEN(II.SexCode not in  ('F','M')) THEN('Empty')  END) NOGender,
Count( CASE WHEN(IPGG.RelationshipToPrincipalRepresentative ='HUS' or IPGG.RelationshipToPrincipalRepresentative ='EXM' or IPGG.RelationshipToPrincipalRepresentative ='WIF'
or IPGG.RelationshipToPrincipalRepresentative ='EXF' or IPGG.RelationshipToPrincipalRepresentative ='CLH' or IPGG.RelationshipToPrincipalRepresentative ='CLW') THEN(II.IndividualGUID) ELSE(NULL) END) couple,
Count( CASE WHEN(IPGG.RelationshipToPrincipalRepresentative ='SCF' or IPGG.RelationshipToPrincipalRepresentative ='SCM' or IPGG.RelationshipToPrincipalRepresentative ='FCF'
or IPGG.RelationshipToPrincipalRepresentative ='FCM' or IPGG.RelationshipToPrincipalRepresentative ='SON' or IPGG.RelationshipToPrincipalRepresentative ='DAU' and II.IndividualAge < 19) THEN(II.IndividualGUID) ELSE(NULL) END) minordependant
INTO dbo.caseprofile1
FROM dbo.dataProcessGroup AS PP
INNER JOIN dbo.dataIndividualProcessGroup AS IPGG ON PP.ProcessingGroupGUID = IPGG.ProcessingGroupGUID
INNER JOIN dbo.dataIndividual AS II ON IPGG.IndividualGUID = II.IndividualGUID
WHERE ProcessStatusCode IN('A') GROUP BY ProcessingGroupNumber


DROP TABLE caseprofile2;
SELECT P.ProcessingGroupNumber CaseNo,
P.ProcessingGroupSize Num_Inds1,
IPG.RelationshipToPrincipalRepresentative Relationship,
IPG.PrincipalRepresentative Relationshippa,
I.OriginCountryCode CountryOrigin,
I.NationalityCode dem_birth_country,
DATENAME(mm, I.ArrivalDate) Montharrival,
DATENAME(yyyy, I.ArrivalDate) YearArrival,
I.SexCode dem_sex,
I.IndividualAge dem_age,
I.IndividualAgeCohortCode dem_agegroup,
I.EthnicityCode dem_ethn,
I.ReligionCode dem_religion,
I.MarriageStatusCode dem_marriage,
I.EducationLevelCode edu_highest,
I.RefugeeStatusCode,
I.RefugeeStatusDate,
I.RefugeeStatusCategoryCode,
-- I.RefugeeStatusCategoryDate,
I.RefugeeStatusLegalBasisCode,
--  I.RefugeeStatusLegalBasisDate,
I.ProcessStatusCode,
--  I.ProcessStatusDate,
I.ProcessStatusReasonCode,
--  I.ProcessStatusReasonDate,
I.SPNeeds,
I.HasSPNeed,
I.OccupationCode occupationcode,
K.LocationLevel1Description cool1,
K.LocationLevel1ID cool1id,
K.LocationLevel2Description cool2,
K.LocationLevel2ID cool2id,
K.LocationLevel3Description cool3,
K.LocationLevel3ID cool3id,
K.LocationLevel4Description cool4,
K.LocationLevel4ID cool4id,
K.LocationLevel5Description cool5,
K.LocationLevel5ID cool5id,
J.LocationLevel1Description coal1,
J.LocationLevel1ID coal1id,
J.LocationLevel2Description coal2,
J.LocationLevel2ID coal2id,
J.LocationLevel3Description coal3,
J.LocationLevel3ID coal3id,
J.LocationLevel4Description coal4,
J.LocationLevel4ID coal4id,
J.LocationLevel5Description coal5,
J.LocationLevel5ID coal5id

INTO dbo.caseprofile2
FROM dbo.dataProcessGroup AS P
INNER JOIN dbo.dataIndividualProcessGroup AS IPG ON P.ProcessingGroupGUID = IPG.ProcessingGroupGUID
INNER JOIN dbo.dataIndividual AS I ON IPG.IndividualGUID = I.IndividualGUID
INNER JOIN dbo.vdataAddressCOA AS J ON IPG.IndividualGUID = J.IndividualGUID
INNER JOIN dbo.vdataAddressCOO AS K ON IPG.IndividualGUID = K.IndividualGUID
LEFT OUTER JOIN dbo.dataProcessGroupPhyFile AS PGF ON PGF.ProcessingGroupGUID = P.ProcessingGroupGUID
WHERE I.ProcessStatusCode = 'A' AND IPG.PrincipalRepresentative = 1

DROP TABLE caseprofile;
SELECT P.CaseNo,
P.Num_Inds1,
P.Relationship,
P.Relationshippa,
P.CountryOrigin,
P.dem_birth_country,
P.Montharrival,
P.YearArrival,
P.dem_sex,
P.dem_age,
P.dem_agegroup,
P.dem_ethn,
P.dem_religion,
P.dem_marriage,
P.edu_highest,
P.RefugeeStatusCode,
P.RefugeeStatusCategoryCode,
P.RefugeeStatusLegalBasisCode,
P.ProcessStatusCode,
P.ProcessStatusReasonCode,
P.SPNeeds,
P.HasSPNeed,
P.occupationcode,
P.cool1,
P.cool1id,
P.cool2,
P.cool2id,
P.cool3,
P.cool3id,
P.cool4,
P.cool4id,
P.cool5,
P.cool5id,
P.coal1,
P.coal1id,
P.coal2,
P.coal2id,
P.coal3,
P.coal3id,
P.coal4,
P.coal4id,
P.coal5,
P.coal5id,
Cal_1.Num_Inds,
Cal_1.Child_0_14,
Cal_1.Youth_15_17,
Cal_1.Work_15_64,
Cal_1.Eldern_65,
Cal_1.Male,
Cal_1.Female,
Cal_1.NOGender,
Cal_1.couple,
Cal_1.minordependant,
Cal_1.AVG_Age,
Cal_1.STDEV_Age
INTO [dbo].[caseprofile]
FROM caseprofile2 as P
LEFT JOIN caseprofile1 AS Cal_1  ON P.CaseNo = Cal_1.CaseNo

DROP TABLE caseprofile1;
DROP TABLE caseprofile2;


DROP TABLE caseprofileneeds;
SELECT *
INTO caseprofileneeds
FROM
(SELECT I.IndividualGUID,
I.VulnerabilityDetailsCode as SPNeeds,
I.VulnerabilityDetailsCode as code,
P.ProcessingGroupNumber CaseNo --Colums to pivot
FROM  dataVulnerability as I
INNER JOIN dbo.dataIndividual AS II ON I.IndividualGUID = II.IndividualGUID
INNER JOIN dbo.dataIndividualProcessGroup AS IPG ON IPG.IndividualGUID = II.IndividualGUID
INNER JOIN dbo.dataProcessGroup AS P  ON P.ProcessingGroupGUID = IPG.ProcessingGroupGUID
WHERE I.VulnerabilityActive = 1
AND VulnerabilityDetailsCode in (
'CR', 'CR-AF', 'CR-CC', 'CR-CH', 'CR-CL', 'CR-CP', 'CR-CS',
'CR-LO', 'CR-LW', 'CR-MS', 'CR-NE', 'CR-SE', 'CR-TP', 'DS', 'DS-BD', 'DS-DF', 'DS-MM', 'DS-MS', 'DS-PM',
'DS-PS', 'DS-SD', 'ER', 'ER-FR', 'ER-MC', 'ER-NF', 'ER-OC', 'ER-SC', 'ER-UR', 'FU', 'FU-FR', 'FU-TR',
'LP', 'LP-AF', 'LP-AN', 'LP-AP', 'LP-BN', 'LP-CR', 'LP-DA', 'LP-DN', 'LP-DO', 'LP-DP', 'LP-DT', 'LP-ES',
'LP-FR', 'LP-IH', 'LP-LS', 'LP-MD', 'LP-MM', 'LP-MS', 'LP-NA', 'LP-ND', 'LP-PV', 'LP-RD', 'LP-RP', 'LP-RR',
'LP-ST', 'LP-TA', 'LP-TC', 'LP-TD', 'LP-TO', 'LP-TR', 'LP-UP', 'LP-VA', 'LP-VF', 'LP-VO', 'LP-VP', 'LP-WP',
'PG', 'PG-HR', 'PG-LC', 'SC', 'SC-CH', 'SC-FC', 'SC-IC', 'SC-NC', 'SC-SC', 'SC-UC', 'SC-UF', 'SC-UM',
'SM', 'SM-AD', 'SM-CC', 'SM-CI', 'SM-DP', 'SM-MI', 'SM-MN', 'SM-OT', 'SP', 'SP-CG', 'SP-GP', 'SP-PT',
'SV', 'SV-FM', 'SV-GM', 'SV-HK', 'SV-HP', 'SV-SS', 'SV-VA', 'SV-VF', 'SV-VO', 'TR', 'TR-HO', 'TR-PI',
'TR-WV', 'WR', 'WR-GM', 'WR-HR',
'WR-LC', 'WR-PY', 'WR-SF', 'WR-UW', 'WR-WF', 'WR-WR'))
as sourcetable
pivot(
COUNT(IndividualGUID) --Pivot on this column
for SPNeeds --Make colum where SPNeeds is in one of these.
in([CR], [CR - AF], [CR - CC], [CR - CH], [CR - CL], [CR - CP], [CR - CS], [CR - LO], [CR - LW], [CR - MS], [CR - NE], [CR - SE], [CR - TP], [DS], [DS - BD], [DS - DF], [DS - MM], [DS - MS], [DS - PM], [DS - PS], [DS - SD], [ER], [ER - FR], [ER - MC], [ER - NF], [ER - OC], [ER - SC], [ER - UR], [FU], [FU - FR], [FU - TR], [LP], [LP - AF], [LP - AN], [LP - AP], [LP - BN], [LP - CR], [LP - DA], [LP - DN], [LP - DO], [LP - DP], [LP - DT], [LP - ES], [LP - FR], [LP - IH], [LP - LS], [LP - MD], [LP - MM], [LP - MS], [LP - NA], [LP - ND], [LP - PV], [LP - RD], [LP - RP], [LP - RR], [LP - ST], [LP - TA], [LP - TC], [LP - TD], [LP - TO], [LP - TR], [LP - UP], [LP - VA], [LP - VF], [LP - VO], [LP - VP], [LP - WP], [PG], [PG - HR], [PG - LC], [SC], [SC - CH], [SC - FC], [SC - IC], [SC - NC], [SC - SC], [SC - UC], [SC - UF], [SC - UM], [SM], [SM - AD], [SM - CC], [SM - CI], [SM - DP], [SM - MI], [SM - MN], [SM - OT], [SP], [SP - CG], [SP - GP], [SP - PT], [SV], [SV - FM], [SV - GM], [SV - HK], [SV - HP], [SV - SS], [SV - VA], [SV - VF], [SV - VO], [TR], [TR - HO], [TR - PI], [TR - WV], [WR], [WR - GM], [WR - HR], [WR - LC], [WR - PY], [WR - SF], [WR - UW], [WR - WF], [WR - WR])
)
as CountSpecificNeeds--Pivot table alias
"


cat("Executing the summary table creation within proGres")
final <- sqlQuery(dbhandleprogresv3,paste(query))

cat("fetching the view containing information")
progres.case <-  sqlFetch(dbhandleprogresv3, "caseprofile")

## With general needs
progres.specificneed <- sqlFetch(dbhandleprogresv3, "caseprofileneeds")



## Defining family profile##################################
cat("Create Family profile \n ")

progres.case$nonnuclear <- progres.case$Num_Inds - progres.case$couple - progres.case$minordependant

prop.table(table(progres.case$couple, useNA = "ifany"))
prop.table(table(progres.case$minordependant, useNA = "ifany"))
prop.table(table(progres.case$nonnuclear, useNA = "ifany"))



progres.case$familyprofile <- ""
progres.case$familyprofile[progres.case$Num_Inds==1] <- "single"
prop.table(table(progres.case$familyprofile, useNA = "ifany"))

progres.case$familyprofile[progres.case$Num_Inds==2 & progres.case$couple==1] <- "couple.no.kids"
prop.table(table(progres.case$familyprofile, useNA = "ifany"))

progres.case$familyprofile[progres.case$Num_Inds>1 & progres.case$nonnuclear==1 & progres.case$couple==0 & progres.case$minordependant > 0 ] <- "uniparental.with.kids"
prop.table(table(progres.case$familyprofile, useNA = "ifany"))

progres.case$familyprofile[ progres.case$minordependant > 0 &  progres.case$nonnuclear ==1 & progres.case$couple==1 ] <- "couple.with.kids.no.dependant"
prop.table(table(progres.case$familyprofile, useNA = "ifany"))

progres.case$familyprofile[ progres.case$nonnuclear > 1 ] <- "non.nuclear.or.adult.dependant"
prop.table(table(progres.case$familyprofile, useNA = "ifany"))

progres.case$familyprofile[progres.case$familyprofile==""] <- "non.nuclear.or.adult.dependant"
prop.table(table(progres.case$familyprofile, useNA = "ifany"))


# Recoding ethnicity & religion#################################
cat("Recoding ethnicity & religion \n")

prop.table(table(progres.case$dem_ethn, useNA = "ifany"))

progres.case$dem_ethnCat <- as.character(progres.case$dem_ethn)
progres.case$dem_ethnCat[progres.case$dem_ethn=="Armenian"] <- "Other/noData"
progres.case$dem_ethnCat[progres.case$dem_ethn=="Assyrian"] <- "Other/noData"
progres.case$dem_ethnCat[progres.case$dem_ethn=="Chaldean"] <- "Other/noData"
progres.case$dem_ethnCat[progres.case$dem_ethn=="Cirassain"] <- "Other/noData"
progres.case$dem_ethnCat[progres.case$dem_ethn=="Turkmen"] <- "Other/noData"
progres.case$dem_ethnCat <- as.factor(progres.case$dem_ethnCat)
prop.table(table(progres.case$dem_ethnCat, useNA = "ifany"))

freq.rel <- as.data.frame(prop.table(table(progres.case$dem_religion, useNA = "ifany")))
freq.rel$dem_religion <- freq.rel$Var1
freq.rel$dem_religionCat <- as.character(freq.rel$dem_religion)
#str(freq.rel)
freq.rel[freq.rel$Freq < 0.05, c("dem_religionCat")] <-  "Other.or.noData"
freq.rel$dem_religionCat[freq.rel$dem_religion=="CHR"] <-  "Christian"
freq.rel$dem_religionCat[freq.rel$dem_religion=="MUS"] <-  "Muslim"
freq.rel$dem_religionCat[freq.rel$dem_religion=="SIT"] <-  "Shia"
freq.rel$dem_religionCat[freq.rel$dem_religion=="SUN"] <-  "Sunni"

progres.case <- join(x=progres.case, y=freq.rel, by="dem_religion", type="left")
prop.table(table(progres.case$dem_religionCat, useNA = "ifany"))


## Case size  as factor##############################
cat("Recoding case size \n")

progres.case$Case.size <- as.factor(progres.case$Num_Inds)
progres.case$Case.size <- recode(progres.case$Case.size,"'1'='Case.size.1';
                                 '2'='Case.size.2';
                                 '3'='Case.size.3';
                                 '4'='Case.size.4';
                                 '5'='Case.size.5';
                                 '6'='Case.size.6';
                                 '7'='Case.size.7.and.more';
                                 '8'='Case.size.7.and.more';
                                 '9'='Case.size.7.and.more';
                                 '10'='Case.size.7.and.more';
                                 '11'='Case.size.7.and.more';
                                 '12'='Case.size.7.and.more';
                                 '13'='Case.size.7.and.more';
                                 '14'='Case.size.7.and.more';
                                 '15'='Case.size.7.and.more';
                                 '16'='Case.size.7.and.more';
                                 '17'='Case.size.7.and.more';
                                 '18'='Case.size.7.and.more';
                                 '19'='Case.size.7.and.more';
                                 '20'='Case.size.7.and.more';
                                 '20'='Case.size.7.and.more';
                                 '21'='Case.size.7.and.more';
                                 '22'='Case.size.7.and.more';
                                 '23'='Case.size.7.and.more';
                                 '24'='Case.size.7.and.more';
                                 '25'='Case.size.7.and.more';
                                 '26'='Case.size.7.and.more';
                                 '27'='Case.size.7.and.more';
                                 '29'='Case.size.7.and.more';
                                 '36'='Case.size.7.and.more';
                                 '37'='Case.size.7.and.more';
                                 '42'='Case.size.7.and.more';
                                 '42'='Case.size.7.and.more';
                                 '46'='Case.size.7.and.more';
                                 '55'='Case.size.7.and.more';
                                 '59'='Case.size.7.and.more';
                                 '65'='Case.size.7.and.more';
                                 '67'='Case.size.7.and.more';
                                 '83'='Case.size.7.and.more';
                                 '42'='Case.size.7.and.more';
                                 NA = 'noData'")

prop.table(table(progres.case$Case.size, useNA = "ifany"))


## Calculating dependency ratio ####################
cat("Coding dependency ratio \n")

progres.case$dependency <-  cut( (progres.case$Child_0_14+progres.case$Eldern_65) / progres.case$Work_15_64, c(0.0001,0.99,1.1,Inf))
progres.case$dependency <- as.character(progres.case$dependency)
progres.case$dependency[is.na(progres.case$dependency)] <- "1.no.dependant"
progres.case$dependency <- as.factor(recode(progres.case$dependency,"'(0.0001,0.99]'='2.few.dependant';
                                            '(0.99,1.1]'='3.half.dependant';
                                            '(1.1,Inf]'='4.majority.dependant'"))

prop.table(table(progres.case$dependency, useNA = "ifany"))


## Calculating youth dependency ratio ####################
cat("Coding Youth dependency ratio \n")

progres.case$youthdependency <- cut(progres.case$Child_0_14 / progres.case$Work_15_64, c(0.0001,0.99,1.1,Inf))
progres.case$youthdependency <- as.character(progres.case$youthdependency)
progres.case$youthdependency[is.na(progres.case$youthdependency)] <- "1.no.dependant"
progres.case$youthdependency <- as.factor(recode(progres.case$youthdependency,"'(0.0001,0.99]'='2.few.dependant';
                                                 '(0.99,1.1]'='3.half.dependant';
                                                 '(1.1,Inf]'='4.majority.dependant'"))

prop.table(table(progres.case$youthdependency, useNA = "ifany"))


## Calculating Eldern ratio ####################
cat("Coding Eldern dependency ratio \n")

progres.case$elederndependency <- cut(progres.case$Eldern_65 / progres.case$Work_15_64, c(0.0001,0.99,1.1,Inf))
progres.case$elederndependency <- as.character(progres.case$elederndependency)
progres.case$elederndependency[is.na(progres.case$elederndependency)] <- "1.no.dependant"
progres.case$elederndependency <- as.factor(recode(progres.case$elederndependency,"'(0.0001,0.99]'='2.few.dependant';
                                                   '(0.99,1.1]'='3.half.dependant';
                                                   '(1.1,Inf]'='4.majority.dependant'"))

prop.table(table(progres.case$elederndependency, useNA = "ifany"))


cat("Coding Female ratio \n")

progres.case$female.ratio <- cut(progres.case$Female / progres.case$Num_Inds, c(0.0001,0.45,0.55,0.99,1.1))
prop.table(table(progres.case$female.ratio, useNA = "ifany"))
progres.case$female.ratio <- as.character(progres.case$female.ratio)
progres.case$female.ratio[is.na(progres.case$female.ratio)] <- "1.no.female"
progres.case$female.ratio <- as.factor(recode(progres.case$female.ratio,"'(0.0001,0.45]'='2.few.female'; '(0.45,0.55]'='3.half.female';
                                              '(0.55,0.99]'='4.most.female'; '(0.99,1.1]'='5.all.female'"))

prop.table(table(progres.case$female.ratio, useNA = "ifany"))


## Adding Age cohort of PA ##############
cat("Coding Age cohort of PA \n")
progres.case$agecohort <- cut(progres.case$dem_age,c(0,18,25,35,45,59,Inf))
progres.case$agecohort <- as.character(progres.case$agecohort)

##Eliminating records where PA has no age #####
cat("Eliminating records where PA has no age \n")
progres.case <- progres.case[!is.na(progres.case$agecohort), ]
prop.table(table(progres.case$agecohort, useNA = "ifany"))
progres.case$agecohort <-  as.factor(progres.case$agecohort)

prop.table(table(progres.case$agecohort, useNA = "ifany"))

## Adding Age cohort for Average age##################
progres.case$AVGAgecohort <- cut(progres.case$AVG_Age,c(0.1,18,25,35,45,59,Inf))

progres.case <- progres.case[!is.na(progres.case$AVGAgecohort), ]
prop.table(table(progres.case$AVGAgecohort, useNA = "ifany"))

progres.case$AVGAgecohort <- as.character(progres.case$AVGAgecohort)
progres.case <- progres.case[!is.na(progres.case$AVGAgecohort), ]
#progres.case$AVGAgecohort[is.na(progres.case$AVGAgecohort)] <- "Unknown"
progres.case$AVGAgecohort <- as.factor(progres.case$AVGAgecohort)
progres.case$AVGAgecohort <-  factor(progres.case$AVGAgecohort, levels = c(  "(0.1,18]", "(18,25]", "(25,35]", "(35,45]","(45,59]", "(59,Inf]"))

prop.table(table(progres.case$AVGAgecohort, useNA = "ifany"))


## Adding class for standard age deviation##############################
progres.case$STDEVAgeclass <- cut(progres.case$STDEV_Age,c(0.001,10,15,20,Inf))
progres.case$STDEVAgeclass <- as.character(progres.case$STDEVAgeclass)
progres.case$STDEVAgeclass[is.na(progres.case$STDEVAgeclass)] <- "No.deviation"
progres.case$STDEVAgeclass <- as.factor(progres.case$STDEVAgeclass)
progres.case$STDEVAgeclass <-  factor(progres.case$STDEVAgeclass, levels = c(  "(0.001,10]", "(10,15]", "(15,20]", "(20,Inf]","No.deviation"))

prop.table(table(progres.case$STDEVAgeclass, useNA = "ifany"))


# Age group##############################
progres.case$dem_age_grp1 <- as.factor(ifelse(progres.case$dem_age < 35, 1, 0))
progres.case$dem_age_grp2 <- as.factor(ifelse((progres.case$dem_age >= 35) &  (progres.case$dem_age < 55), 1, 0))
progres.case$dem_age_grp3 <- as.factor(ifelse(progres.case$dem_age >= 55, 1, 0))

progres.case$dem_PA_grp0 <- as.factor(ifelse(progres.case$dem_age < 15, 1, 0))
progres.case$dem_PA_grp1 <- as.factor(ifelse(progres.case$dem_age < 18, 1, 0))
progres.case$dem_PA_grp2 <- as.factor(ifelse((progres.case$dem_age > 17) & (progres.case$dem_age < 60), 1, 0))
progres.case$dem_PA_grp3 <- as.factor(ifelse(progres.case$dem_age > 59, 1, 0))

progres.case$age.PA1 <- as.factor(ifelse(progres.case$dem_age < 35, 1, 0))
progres.case$age.PA2 <- as.factor(ifelse((progres.case$dem_age > 34) & (progres.case$dem_age < 55), 1, 0))
progres.case$age.PA3 <- as.factor(ifelse(progres.case$dem_age > 54, 1, 0))



# Percentage of children##############################
progres.case$p.child.grp1 <- as.factor(ifelse(progres.case$Child_0_14/progres.case$Num_Inds == 0, 1, 0))
progres.case$p.child.grp2 <- as.factor(ifelse((progres.case$Child_0_14/progres.case$Num_Inds > 0) & (progres.case$Child_0_14/progres.case$Num_Inds < 0.50), 1, 0))
progres.case$p.child.grp3 <- as.factor(ifelse((progres.case$Child_0_14/progres.case$Num_Inds >= 0.50) & (progres.case$Child_0_14/progres.case$Num_Inds < 0.75), 1, 0))
progres.case$p.child.grp4 <- as.factor(ifelse(progres.case$Child_0_14/progres.case$Num_Inds >= 0.75, 1, 0))


# Aggregating arrival year##############################
progres.case$YearArrivalCategory2 <- as.factor(recode(progres.case$YearArrival,"'1899'='1900-1980';
                                                      '1928'='1900-1980';
                                                      '1932'='1900-1980';
                                                      '1935'='1900-1980';
                                                      '1936'='1900-1980';
                                                      '1937'='1900-1980';
                                                      '1938'='1900-1980';
                                                      '1939'='1900-1980';
                                                      '1940'='1900-1980';
                                                      '1941'='1900-1980';
                                                      '1942'='1900-1980';
                                                      '1943'='1900-1980';
                                                      '1944'='1900-1980';
                                                      '1945'='1900-1980';
                                                      '1946'='1900-1980';
                                                      '1947'='1900-1980';
                                                      '1948'='1900-1980';
                                                      '1949'='1900-1980';
                                                      '1950'='1900-1980';
                                                      '1951'='1900-1980';
                                                      '1952'='1900-1980';
                                                      '1953'='1900-1980';
                                                      '1954'='1900-1980';
                                                      '1955'='1900-1980';
                                                      '1956'='1900-1980';
                                                      '1957'='1900-1980';
                                                      '1958'='1900-1980';
                                                      '1959'='1900-1980';
                                                      '1960'='1900-1980';
                                                      '1961'='1900-1980';
                                                      '1962'='1900-1980';
                                                      '1963'='1900-1980';
                                                      '1964'='1900-1980';
                                                      '1965'='1900-1980';
                                                      '1966'='1900-1980';
                                                      '1967'='1900-1980';
                                                      '1968'='1900-1980';
                                                      '1969'='1900-1980';
                                                      '1970'='1900-1980';
                                                      '1971'='1900-1980';
                                                      '1972'='1900-1980';
                                                      '1973'='1900-1980';
                                                      '1974'='1900-1980';
                                                      '1975'='1900-1980';
                                                      '1976'='1900-1980';
                                                      '1977'='1900-1980';
                                                      '1978'='1900-1980';
                                                      '1979'='1900-1980';
                                                      '1980'='1900-1980';
                                                      '1981'='1981-1990';
                                                      '1982'='1981-1990';
                                                      '1983'='1981-1990';
                                                      '1984'='1981-1990';
                                                      '1985'='1981-1990';
                                                      '1986'='1981-1990';
                                                      '1987'='1981-1990';
                                                      '1988'='1981-1990';
                                                      '1989'='1981-1990';
                                                      '1990'='1981-1990';
                                                      '1991'='1991-2000';
                                                      '1992'='1991-2000';
                                                      '1993'='1991-2000';
                                                      '1994'='1991-2000';
                                                      '1995'='1991-2000';
                                                      '1996'='1991-2000';
                                                      '1997'='1991-2000';
                                                      '1998'='1991-2000';
                                                      '1999'='1991-2000';
                                                      '2000'='1991-2000';
                                                      '2001'='2001-2005';
                                                      '2002'='2001-2005';
                                                      '2003'='2001-2005';
                                                      '2004'='2001-2005';
                                                      '2005'='2001-2005';
                                                      '2006'='2006-2010';
                                                      '2007'='2006-2010';
                                                      '2008'='2006-2010';
                                                      '2009'='2006-2010';
                                                      '2010'='2006-2010';
                                                      '2011'='2011';
                                                      '2012'='2012';
                                                      '2013'='2013';
                                                      '2014'='2014';
                                                      '2015'='2015';
                                                      NA = 'noData';
                                                      ''='noData'"))


progres.case$YearArrivalCategory <- as.factor(recode(progres.case$YearArrival,"'1899'='2011.or.before.or.unkown';
                                                     '1900'='2011.or.before.or.unkown';
                                                     '1902'='2011.or.before.or.unkown';
                                                     '1903'='2011.or.before.or.unkown';
                                                     '1905'='2011.or.before.or.unkown';
                                                     '1911'='2011.or.before.or.unkown';
                                                     '1926'='2011.or.before.or.unkown';
                                                     '1927'='2011.or.before.or.unkown';
                                                     '1930'='2011.or.before.or.unkown';
                                                     '1931'='2011.or.before.or.unkown';
                                                     '1933'='2011.or.before.or.unkown';
                                                     '1934'='2011.or.before.or.unkown';
                                                     '1928'='2011.or.before.or.unkown';
                                                     '1932'='2011.or.before.or.unkown';
                                                     '1935'='2011.or.before.or.unkown';
                                                     '1936'='2011.or.before.or.unkown';
                                                     '1937'='2011.or.before.or.unkown';
                                                     '1938'='2011.or.before.or.unkown';
                                                     '1939'='2011.or.before.or.unkown';
                                                     '1940'='2011.or.before.or.unkown';
                                                     '1941'='2011.or.before.or.unkown';
                                                     '1942'='2011.or.before.or.unkown';
                                                     '1943'='2011.or.before.or.unkown';
                                                     '1944'='2011.or.before.or.unkown';
                                                     '1945'='2011.or.before.or.unkown';
                                                     '1946'='2011.or.before.or.unkown';
                                                     '1947'='2011.or.before.or.unkown';
                                                     '1948'='2011.or.before.or.unkown';
                                                     '1949'='2011.or.before.or.unkown';
                                                     '1950'='2011.or.before.or.unkown';
                                                     '1951'='2011.or.before.or.unkown';
                                                     '1952'='2011.or.before.or.unkown';
                                                     '1953'='2011.or.before.or.unkown';
                                                     '1954'='2011.or.before.or.unkown';
                                                     '1955'='2011.or.before.or.unkown';
                                                     '1956'='2011.or.before.or.unkown';
                                                     '1957'='2011.or.before.or.unkown';
                                                     '1958'='2011.or.before.or.unkown';
                                                     '1959'='2011.or.before.or.unkown';
                                                     '1960'='2011.or.before.or.unkown';
                                                     '1961'='2011.or.before.or.unkown';
                                                     '1962'='2011.or.before.or.unkown';
                                                     '1963'='2011.or.before.or.unkown';
                                                     '1964'='2011.or.before.or.unkown';
                                                     '1965'='2011.or.before.or.unkown';
                                                     '1966'='2011.or.before.or.unkown';
                                                     '1967'='2011.or.before.or.unkown';
                                                     '1968'='2011.or.before.or.unkown';
                                                     '1969'='2011.or.before.or.unkown';
                                                     '1970'='2011.or.before.or.unkown';
                                                     '1971'='2011.or.before.or.unkown';
                                                     '1972'='2011.or.before.or.unkown';
                                                     '1973'='2011.or.before.or.unkown';
                                                     '1974'='2011.or.before.or.unkown';
                                                     '1975'='2011.or.before.or.unkown';
                                                     '1976'='2011.or.before.or.unkown';
                                                     '1977'='2011.or.before.or.unkown';
                                                     '1978'='2011.or.before.or.unkown';
                                                     '1979'='2011.or.before.or.unkown';
                                                     '1980'='2011.or.before.or.unkown';
                                                     '1981'='2011.or.before.or.unkown';
                                                     '1982'='2011.or.before.or.unkown';
                                                     '1983'='2011.or.before.or.unkown';
                                                     '1984'='2011.or.before.or.unkown';
                                                     '1985'='2011.or.before.or.unkown';
                                                     '1986'='2011.or.before.or.unkown';
                                                     '1987'='2011.or.before.or.unkown';
                                                     '1988'='2011.or.before.or.unkown';
                                                     '1989'='2011.or.before.or.unkown';
                                                     '1990'='2011.or.before.or.unkown';
                                                     '1991'='2011.or.before.or.unkown';
                                                     '1992'='2011.or.before.or.unkown';
                                                     '1993'='2011.or.before.or.unkown';
                                                     '1994'='2011.or.before.or.unkown';
                                                     '1995'='2011.or.before.or.unkown';
                                                     '1996'='2011.or.before.or.unkown';
                                                     '1997'='2011.or.before.or.unkown';
                                                     '1998'='2011.or.before.or.unkown';
                                                     '1999'='2011.or.before.or.unkown';
                                                     '2000'='2011.or.before.or.unkown';
                                                     '2001'='2011.or.before.or.unkown';
                                                     '2002'='2011.or.before.or.unkown';
                                                     '2003'='2011.or.before.or.unkown';
                                                     '2004'='2011.or.before.or.unkown';
                                                     '2005'='2011.or.before.or.unkown';
                                                     '2006'='2011.or.before.or.unkown';
                                                     '2007'='2011.or.before.or.unkown';
                                                     '2008'='2011.or.before.or.unkown';
                                                     '2009'='2011.or.before.or.unkown';
                                                     '2010'='2011.or.before.or.unkown';
                                                     '2011'='2011.or.before.or.unkown';
                                                     '2012'='2012';
                                                     '2013'='2013';
                                                     '2014'='2014';
                                                     '2015'='2015';
                                                     '2016'='2016.and.2017';
                                                     '2017'='2016.and.2017';
                                                     NA = 'noData';
                                                     ''='noData'"))

progres.case$YearArrivalCategory[is.na(progres.case$YearArrivalCategory)] <- "2011.or.before.or.unkown"

prop.table(table(progres.case$YearArrivalCategory, useNA = "ifany"))


# Aggregating country of Origin##############################
progres.case$CountryOriginCategory <- recode(progres.case$CountryOrigin,"'SYR'='SYR';
                                             'IRQ'='IRQ';
                                             'SOM'='HORN';
                                             'AFG'='AFG';
                                             'IRN'='IRN';
                                             'SUD'='HORN';
                                             'ETH'='HORN';
                                             'ERT'='HORN';
                                             'PAL'='MENA';
                                             'TUR'='OTH';
                                             'PAK'='ASIA';
                                             'YEM'='MENA';
                                             'SSD'='AFR';
                                             'NIG'='AFR';
                                             'ICO'='AFR';
                                             'COD'='AFR';
                                             'UGA'='AFR';
                                             'BGD'='ASIA';
                                             'ARE'='MENA';
                                             'CMR'='AFR';
                                             'UZB'='ASIA';
                                             'COB'='AFR';
                                             'TKM'='ASIA';
                                             'MLI'='AFR';
                                             'GUI'='AFR';
                                             'CAR'='AFR';
                                             'LBY'='MENA';
                                             'KGZ'='ASIA';
                                             'LEB'='MENA';
                                             'CHD'='AFR';
                                             'TJK'='ASIA';
                                             'MOR'='AFR';
                                             'JOR'='MENA';
                                             'SEN'='AFR';
                                             'KUW'='MENA';
                                             'MNG'='OTH';
                                             'ALG'='MENA';
                                             'GHA'='AFR';
                                             'TUN'='MENA';
                                             'SLE'='AFR';
                                             'LBR'='AFR';
                                             'RUS'='OTH';
                                             'KEN'='AFR';
                                             'CHI'='ASIA';
                                             'LKA'='ASIA';
                                             'BDI'='AFR';
                                             'KAZ'='ASIA';
                                             'PHI'='ASIA';
                                             'ZZZ'='OTH';
                                             'MYA'='ASIA';
                                             'SAU'='MENA';
                                             'BAH'='MENA';
                                             'RWA'='AFR';
                                             'IND'='ASIA';
                                             'TOG'='AFR';
                                             'DJB'='AFR';
                                             'GAM'='AFR';
                                             'NGR'='AFR';
                                             'ZIM'='AFR';
                                             'ANG'='AFR';
                                             'BKF'='AFR';
                                             'NEP'='ASIA';
                                             'SRV'='OTH';
                                             'U'='OTH';
                                             'BEN'='AFR';
                                             'TAN'='AFR';
                                             'UKR'='OTH';
                                             'MAD'='AFR';
                                             'GAB'='AFR';
                                             'INS'='ASIA';
                                             'MAU'='AFR';
                                             'TIB'='ASIA';
                                             'UAE'='MENA';
                                             'BSN'='OTH';
                                             'COI'='AFR';
                                             'OMN'='MENA';
                                             'GNB'='AFR';
                                             'DOM'='OTH';
                                             'ISR'='MENA';
                                             'ARM'='ASIA';
                                             'AZE'='ASIA';
                                             'BLR'='OTH';
                                             'CUB'='OTH';
                                             'FRA'='OTH';
                                             'HAI'='OTH';
                                             'RSA'='OTH';
                                             'ARG'='OTH';
                                             'GBR'='OTH';
                                             'KRN'='OTH';
                                             'MDA'='OTH';
                                             'MTS'='OTH';
                                             'PER'='OTH';
                                             'SCG'='OTH';
                                             'SOL'='OTH';
                                             'SUR'='OTH';
                                             'YUG'='OTH';
                                             '-'='OTH';
                                             # '-'='Unknown';
                                             'U'='OTH';
                                             NA = 'OTH';
                                             ''= 'OTH'")

progres.case$CountryOriginCategory <- factor(progres.case$CountryOriginCategory, levels = c("SYR","IRQ","AFG","IRN","HORN","AFR", "MENA", "ASIA", "OTH"))



#### Recategorised "cool1","coal1","cool2","coal2", #################################
## -- in each country of asylum if the category
## classes with low numbers - less than 1% - are aggregated. Unknown adresses are recorded as 'other'.

freq1.coo <- as.data.frame(prop.table(table(progres.case$cool1,progres.case$CountryAsylum,progres.case$CountryOrigin),2))
freq1.coo$keycool1 <- paste(freq1.coo$Var1,freq1.coo$Var2,freq1.coo$Var3,sep = "-")
freq1.coo$cool1Cat <- as.character(freq1.coo$Var1)
freq1.coo[freq1.coo$Freq <= 0.01, c("cool1Cat")] <- "Other.or.Unknown"
freq1.coo[freq1.coo$Var1 =="", c("cool1Cat")] <- "Other.or.Unknown"
# levels(as.factor(freq1.coo$cool1Cat))
freq1.coo <- freq1.coo[ ,c("keycool1","cool1Cat")]

#frequ.coo <- as.data.frame(table(progres.case$cool1,progres.case$CountryAsylum))
freq1.coo <- as.data.frame(prop.table(table(progres.case$cool1,progres.case$CountryAsylum,progres.case$CountryOrigin),2))
freq1.coo$keycool1 <- paste(freq1.coo$Var1,freq1.coo$Var2,freq1.coo$Var3,sep = "-")
freq1.coo$cool1Cat <- as.character(freq1.coo$Var1)
freq1.coo[freq1.coo$Freq <= 0.01, c("cool1Cat")] <- "Other.or.Unknown"
freq1.coo[freq1.coo$Var1 =="", c("cool1Cat")] <- "Other.or.Unknown"
# levels(as.factor(freq1.coo$cool1Cat))
freq1.coo <- freq1.coo[ ,c("keycool1","cool1Cat")]

freq1.coa <- as.data.frame(prop.table(table(progres.case$coal1,progres.case$CountryAsylum,progres.case$CountryOrigin),2))
freq1.coa$keycoal1 <- paste(freq1.coa$Var1,freq1.coa$Var2,freq1.coa$Var3,sep = "-")
freq1.coa$coal1Cat <- as.character(freq1.coa$Var1)
freq1.coa[freq1.coa$Freq <= 0.01, c("coal1Cat")] <- "Other.or.Unknown"
freq1.coa[freq1.coa$Var1 =="", c("coal1Cat")] <- "Other.or.Unknown"
freq1.coa <- freq1.coa[ ,c("keycoal1","coal1Cat")]

freq2.coo <- as.data.frame(prop.table(table(progres.case$cool2,progres.case$CountryAsylum,progres.case$CountryOrigin),2))
freq2.coo$keycool2 <- paste(freq2.coo$Var1,freq2.coo$Var2,freq2.coo$Var3,sep = "-")
freq2.coo$cool2Cat <- as.character(freq2.coo$Var1)
freq2.coo[freq2.coo$Freq <= 0.01, c("cool2Cat")] <- "Other.or.Unknown"
freq2.coo[freq2.coo$Var1 =="", c("cool2Cat")] <- "Other.or.Unknown"
freq2.coo <- freq2.coo[ ,c("keycool2","cool2Cat")]

freq2.coa <- as.data.frame(prop.table(table(progres.case$coal2,progres.case$CountryAsylum,progres.case$CountryOrigin),2))
freq2.coa$keycoal2 <- paste(freq2.coa$Var1,freq2.coa$Var2,freq2.coa$Var3,sep = "-")
freq2.coa$coal2Cat <- as.character(freq2.coa$Var1)
freq2.coa[freq2.coa$Freq <= 0.01, c("coal2Cat")] <- "Other.or.Unknown"
freq2.coa[freq2.coa$Var1 =="", c("coal2Cat")] <- "Other.or.Unknown"
freq2.coa <- freq2.coa[ ,c("keycoal2","coal2Cat")]

#View(freq1.coo[freq1.coo$Var2=="JOR" & freq1.coo$Var3=="SYR" &  freq1.coo$Freq>=0.01, ])

## generation of key to join with frequency tables
progres.case$keycool1 <- paste(progres.case$cool1,progres.case$CountryAsylum,progres.case$CountryOrigin,sep = "-")
progres.case$keycoal1 <- paste(progres.case$coal1,progres.case$CountryAsylum,progres.case$CountryOrigin,sep = "-")
progres.case$keycool2 <- paste(progres.case$cool2,progres.case$CountryAsylum,progres.case$CountryOrigin,sep = "-")
progres.case$keycoal2 <- paste(progres.case$coal2,progres.case$CountryAsylum,progres.case$CountryOrigin,sep = "-")

progres.case <- join(x=progres.case, y=freq1.coa, by="keycoal1", type="left")
progres.case <- join(x=progres.case, y=freq2.coa, by="keycoal2", type="left")
progres.case <- join(x=progres.case, y=freq1.coo, by="keycool1", type="left")
progres.case <- join(x=progres.case, y=freq2.coo, by="keycool2", type="left")

### A few check
# prop.table(table(progres.case[progres.case$CountryOrigin=="SYR" & progres.case$CountryOrigin=="JOR", c("cool1Cat") ], useNA = "ifany"))
# prop.table(table(progres.case[progres.case$CountryOrigin=="SYR", c("cool1Cat") ], useNA = "ifany"))


# Aggregating country of Asylum##############################
#ctrAsylum <- as.data.frame(table(progres.case$CountryAsylum))
#rm(ctrAsylum)
### Not necessary


# Aggregating season according to month##############################
progres.case$season <- as.character(progres.case$Montharrival)
prop.table(table(progres.case$Montharrival, useNA = "ifany"))
progres.case$season <- recode(progres.case$season," 'Jan'='Q1';  'Feb'='Q1'; 'Mar'='Q1';
                              'Apr'='Q2'; 'May'='Q2'; 'Jun'='Q2';
                              'Jul'='Q3';  'Aug'='Q3';  'Sept'='Q3';
                              'Oct'='Q4'; 'Nov'='Q4';  'Dec'='Q4' ")
progres.case$season <- as.factor(progres.case$season)
prop.table(table(progres.case$season, useNA = "ifany"))
progres.case$season <- factor(progres.case$season, levels = c("Q1", "Q2", "Q3", "Q4"))

progres.case$season1 <- as.character(progres.case$Montharrival)
#levels(progres.case$Montharrival)
progres.case$season1 <- recode(progres.case$season1,"'Mar'='Spring'; 'Apr'='Spring';    'May'='Spring';
                               'Jun'='Summer'; 'Jul'='Summer';  'Aug'='Summer';
                               'Sep'='Autumn'; 'Oct'='Autumn'; 'Nov'='Autumn';
                               'Jan'='Winter';  'Feb'='Winter'; 'Dec'='Winter' ")
progres.case$season1 <- factor(progres.case$season1, levels = c("Spring", "Summer", "Autumn", "Winter"))
prop.table(table(progres.case$season1, useNA = "ifany"))

#  Month of arrival ordinal ##############################
progres.case$Montharrival <- recode(progres.case$Montharrival,"'January'='Jan';  'February'='Febr';'March'='Mar';
                                    'April'='Apr';  'May'='May'; 'June'='Jun'; 'July'='Jul';  'August'='Aug';
                                    'September'='Sept'; 'October'='Oct'; 'November'='Nov'; 'December'='Dec' ")
progres.case$Montharrival <- factor(progres.case$Montharrival, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"))


# Recoding Education##############################
progres.case$edu_highest_t <- progres.case$edu_highest
prop.table(table(progres.case$edu_highest, useNA = "ifany"))
#table(progres.case$edu_highest, useNA="always")
progres.case$edu_highest_t <- recode(progres.case$edu_highest_t,"'01' = 'Grade 1'; '02' = 'Grade 2';
                                     '03' = 'Grade 3';  '04' = 'Grade 4';   '05' = 'Grade 5';
                                     '06' = 'Grade 6';   '07' = 'Grade 7';  '08' = 'Grade 8';
                                     '09' = 'Grade 9';    '10' = 'Grade 10';  '11' = 'Grade 11';
                                     '12' = 'Grade 12';    '13' = 'Grade 13';   '14' = 'Grade 14';
                                     'IN' = 'Informal Education';    'NE' = 'No education'; 'U' = 'Unknown'; '-' = 'Unknown';
                                     'TC' = 'Techn Vocational';     'UG' = 'University level'; 'PG' = 'Post university level';
                                     'KG' = 'Kindergarten'")

#progres.case$edu_highest_t <- recode(progres.case$edu_highest_t,"'1 year (or Grade 1)' = 'Grade 1'; '2 year (or Grade 2)' = 'Grade 2';
#                                     '3 year (or Grade 3)' = 'Grade 3';  '04' = '4 year (or Grade 4)';
#                                     '5 year (or Grade 5)' = 'Grade 5';
#                                     '6 year (or Grade 6)' = 'Grade 6';
#                                     '7 year (or Grade 7)' = 'Grade 7';
#                                     '8 year (or Grade 8)' = 'Grade 8';
#                                     '9 year (or Grade 9)' = 'Grade 9';
#                                     '10 year (or Grade 10)' = 'Grade 10';  '11 year (or Grade 11)' = 'Grade 11';
#                                     '12 year (or Grade 12)' = 'Grade 12';    '13 year (or Grade 13)' = 'Grade 13';
#                                     '14 year (or Grade 14)' = 'Grade 14';
#                                     'IN' = 'Informal Education'; 'Informal Educaiton' = 'Informal Education';
#                                      'NE' = 'No education';
#                                      'U' = 'Unknown';
#                                      '-' = 'Unknown';
#                                     'TC' = 'Techn Vocational';     'UG' = 'University level'; 'PG' = 'Post university level';
#                                     'KG' = 'Kindergarten'")

progres.case$edu_highest_t <- factor(progres.case$edu_highest_t, levels = c("Unknown", "No education", "Informal Education","Kindergarten",
                                                                            "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5",
                                                                            "Grade 6", "Grade 7", "Grade 8", "Grade 9", "Grade 10",
                                                                            "Grade 11", "Grade 12", "Grade 13", "Grade 14",
                                                                            "Techn Vocational", "University level", "Post university level"))
prop.table(table(progres.case$edu_highest_t, useNA = "ifany"))
progres.case$edu_highestcat <- recode(progres.case$edu_highest_t,"'Unknown'='Unknown';
                                      'Informal Education'='Other';
                                      'Techn Vocational'='Other';
                                      'No education'='No education';
                                      'Kindergarten'='Up to Grade 5';
                                      'Grade 1'='Up to Grade 5';
                                      'Grade 2'='Up to Grade 5';
                                      'Grade 3'='Up to Grade 5';
                                      'Grade 4'='Up to Grade 5';
                                      'Grade 5'='Up to Grade 5';
                                      'Grade 6'='Grade 6-8';
                                      'Grade 7'='Grade 6-8';
                                      'Grade 8'='Grade 6-8';
                                      'Grade 9'='Grade 9-11';
                                      'Grade 10'='Grade 9-11';
                                      'Grade 11'='Grade 9-11';
                                      'Grade 12'='Grade 12-14';
                                      'Grade 13'='Grade 12-14';
                                      'Grade 14'='Grade 12-14';
                                      'University level'='Higher Education';
                                      'Post university level'='Higher Education'")
prop.table(table(progres.case$edu_highestcat, useNA = "ifany"))
progres.case$edu_highestcat <- as.character(progres.case$edu_highestcat)
table(progres.case$edu_highestcat, useNA="always")


progres.case$edu_highestcat <- recode(progres.case$edu_highest_t,"'Unknown'='Informal.Voca.or.Unknown';
                                      'Informal Education'='Informal.Voca.or.Unknown';
                                      'Techn Vocational'='Informal.Voca.or.Unknown';
                                      'No education'='No education';
                                      'Kindergarten'='Up to Grade 5';
                                      'Grade 1'='Up to Grade 5';
                                      'Grade 2'='Up to Grade 5';
                                      'Grade 3'='Up to Grade 5';
                                      'Grade 4'='Up to Grade 5';
                                      'Grade 5'='Up to Grade 5';
                                      'Grade 6'='Grade 6-8';
                                      'Grade 7'='Grade 6-8';
                                      'Grade 8'='Grade 6-8';
                                      'Grade 9'='Grade 9-11';
                                      'Grade 10'='Grade 9-11';
                                      'Grade 11'='Grade 9-11';
                                      'Grade 12'='Grade 12-14';
                                      'Grade 13'='Grade 12-14';
                                      'Grade 14'='Grade 12-14';
                                      'University level'='Higher Education';
                                      'Post university level'='Higher Education'")
progres.case$edu_highestcat[is.na(progres.case$edu_highestcat)] <- "Informal.Voca.or.Unknown"

prop.table(table(progres.case$edu_highestcat, useNA = "ifany"))

# Educational attainment##############################
progres.case$edu.highest.grp1 <- as.factor(ifelse((progres.case$edu_highest_t == "No education" |
                                                       progres.case$edu_highest_t == "Kindergarten" |
                                                       progres.case$edu_highest_t == "Grade 1" |
                                                       progres.case$edu_highest_t == "Grade 2" |
                                                       progres.case$edu_highest_t == "Grade 3" |
                                                       progres.case$edu_highest_t == "Grade 4" |
                                                       progres.case$edu_highest_t == "Grade 5"), 1, 0))

progres.case$edu.highest.grp2 <- as.factor(ifelse((progres.case$edu_highest_t == "Grade 6" |
                                                     progres.case$edu_highest_t == "Grade 7" |
                                                     progres.case$edu_highest_t == "Grade 8"), 1, 0))

progres.case$edu.highest.grp3 <- as.factor(ifelse((progres.case$edu_highest_t == "Grade 9" |
                                                     progres.case$edu_highest_t == "Grade 10" |
                                                     progres.case$edu_highest_t == "Grade 11") , 1, 0))

progres.case$edu.highest.grp4 <- as.factor(ifelse((progres.case$edu_highest_t == "Grade 12" |
                                                     progres.case$edu_highest_t == "or Grade 13" |
                                                     progres.case$edu_highest_t == "Grade 14") , 1, 0))

progres.case$edu.highest.grp5 <- as.factor(ifelse((progres.case$edu_highest_t == "Post university level" |
                                                     progres.case$edu_highest_t == "University level"), 1, 0))


# Extracting main ocupation category from occupation code##############################
#summary(progres.case$occupationcode)
progres.case$occupationcat <- "UnknownOccup"
progres.case$occupationcat[progres.case$occupationcode ==  "0001"] <- "Military"
progres.case$occupationcat[progres.case$occupationcode ==  "None"] <- "Student.or.NoOccup"
progres.case$occupationcat[progres.case$occupationcode ==  "0110"] <- "Student.or.NoOccup"
#progres.case$occupationcat[substr(progres.case$occupationcode, 1,1) == "1"] <- "Manager"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1) == "1"] <- "Manager-Professional-Technician-Clerk"
#progres.case$occupationcat[substr(progres.case$occupationcode, 1,1) == "2"] <- "Professional"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1) == "2"] <- "Manager-Professional-Technician-Clerk"
#progres.case$occupationcat[substr(progres.case$occupationcode, 1,1) == "3"] <- "Technician"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1) == "3"] <- "Manager-Professional-Technician-Clerk"
#progres.case$occupationcat[substr(progres.case$occupationcode, 1,1) == "4"] <- "Clerk"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1) == "4"] <- "Manager-Professional-Technician-Clerk"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1) == "5"] <- "ServiceMarket"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1) == "6"] <- "Agricultural.Craft.Machine"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1) == "7"] <- "Agricultural.Craft.Machine"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1) == "8"] <- "Agricultural.Craft.Machine"
#progres.case$occupationcat[substr(progres.case$occupationcode, 1,1) == "6"] <- "Agricultural"
#progres.case$occupationcat[substr(progres.case$occupationcode, 1,1) == "7"] <- "Craft"
#progres.case$occupationcat[substr(progres.case$occupationcode, 1,1) == "8"] <- "Machine"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1) == "9"] <- "Elementary"
progres.case$occupationcat[is.na(progres.case$occupationcat)] <- "UnknownOccup"

progres.case$occupationcat <- factor(progres.case$occupationcat, levels = c("Manager-Professional-Technician-Clerk", "ServiceMarket",
                                                                            "Agricultural.Craft.Machine", "Elementary", "Military",
                                                                            "UnknownOccup", "Student.or.NoOccup"))
prop.table(table(progres.case$occupationcat, useNA = "ifany"))

#progres.case$occupationcat <- factor(progres.case$occupationcat, levels = c("Manager", "Professional", "Technician", "Clerk", "ServiceMarket",
#                                                                            "Agricultural", "Craft", "Machine", "Elementary", "Military",
#                                                                            "UnknownOccup", "NoOccup", "Student"))

#summary(progres.case$occupationcat)
#progres.case$occupationcat <- substr(progres.case$occupationcode, 1,1 )
#str(progres.case$occupationcat)
##  International Standard Classification of Occupations (ISCO)
# http://www.ilo.org/public/english/bureau/stat/isco/isco08/
#ISCO.08 <- read.csv("data/ISCO-08.csv")
#names(ISCO.08)
#corrtab88.08 <- read.csv("data/corrtab88-08.csv")
#names(corrtab88.08)
#isco <- merge(x=ISCO.08, y=corrtab88.08, by.x="ISCO08Code", by.y="ISCO.08.Code")
#write.csv(isco, "out/isco.csv")
#names(isco)



# Marital status##############################
progres.case$dem_marriagecat <- recode(progres.case$dem_marriage,"'WD'='Widowed';
                                                                  'MA'='Married';
                                                                  'CL'='Married';
                                                                  'SN'='Single-Engaged';
                                                                  'EG'='Single-Engaged';
                                                                  'DV'='Divorced-Separated-Unknown';
                                                                  'SR'='Divorced-Separated-Unknown'")
progres.case$dem_marriagecat <- factor(progres.case$dem_marriagecat, levels = c("Single-Engaged", "Married", "Widowed", "Divorced-Separated-Unknown"))
progres.case$dem_marriagecat[is.na(progres.case$dem_marriagecat)] <- "Divorced-Separated-Unknown"
prop.table(table(progres.case$dem_marriagecat, useNA = "ifany"))

progres.case$mar_widow <- as.factor(ifelse(progres.case$dem_marriage == "WD", 1, 0))
progres.case$mar_single <- as.factor(ifelse(progres.case$dem_marriage == "SN", 1, 0))
progres.case$mar_divorced <- as.factor(ifelse(progres.case$dem_marriage == "DV", 1, 0))
progres.case$mar_married <- as.factor(ifelse(progres.case$dem_marriage == "MA", 1, 0))
progres.case$mar_engaged <- as.factor(ifelse(progres.case$dem_marriage == "EG", 1, 0))
progres.case$mar_g_divorced <- as.factor(ifelse((progres.case$dem_marriage == "DV" | progres.case$dem_marriage == "SR"), 1, 0))
progres.case$mar_g_married <- as.factor(ifelse((progres.case$dem_marriage == "MA" | progres.case$dem_marriage == "L" | progres.case$dem_marriage == "EG"), 1, 0))


# Ethnicity, religion, birth##############################
progres.case$ethn_arab <- as.factor(ifelse(progres.case$dem_ethn == "Arab", 1, 0))
progres.case$rel_sunni <- as.factor(ifelse(progres.case$dem_religion == "SUN Sunni", 1, 0))
progres.case$bir_syria <- as.factor(ifelse(progres.case$dem_birth_country == "SYR", 1, 0))


# Gender PA##############################
progres.case$dem_sex <- recode(progres.case$dem_sex,"'M'='Male'; 'F'='Female';'U'='Unknown'")
progres.case$gender.male <- ifelse(progres.case$dem_sex == "Male", 1, 0)
progres.case$gender.female <- ifelse(progres.case$dem_sex == "Female", 1, 0)
# Removing record when no gender.. ##############################
progres.case <- progres.case[progres.case$dem_sex %in% c("Male","Female"), ]

### Recoding specific needs at the case level###########################################

### load re-encoding file for specific needs
library(readxl)
SpecificNeedsCodesV2 <- read_excel("/home/edouard/R-project/proGres-analysis/data/SpecificNeedsCodesV2.xlsx",
                                   sheet = "Revised")


progres.specificneed.case <- merge(x = progres.specificneed, y = SpecificNeedsCodesV2, by ="code", all.x = TRUE)
## Build the summary per case #####
# progres.specificneed.case <-  melt(progres.specificneed.unique, id.vars = c("CaseNo","VulnerabilityText"),
#                                   variable.name = "VulnerabilityText",
#                                   value.name = "value", na.rm = TRUE)

progres.specificneed.case2 <- dcast(progres.specificneed.case, CaseNo ~ newcat)
#names(progres.specificneed.case2)
#str(progres.specificneed.case2)
rm(progres.specificneed)
rm(progres.specificneed.case)



## Let's recode the vulnerability Text ######
# "At.Risk", Child.Labour"
# "Child.marriage..parent.or.pregnancy"   "Family.Needs"                          "Marginalised"
# "Medical"                               "Need.of.Care"                          "Problem.with.violence.law.recruitment"
# "Separated.Child"                       "Single.Parent"                         "Unaccompanied"
# "Victim.of.Violence"                    "Woman.at.Risk"

progres.specificneed.case2$At.Risk.count <- progres.specificneed.case2$At.Risk
progres.specificneed.case2$At.Risk <- as.factor(ifelse(progres.specificneed.case2$At.Risk >= 1, "yes", "no"))
progres.specificneed.case2$At.Risk <- as.character(progres.specificneed.case2$At.Risk)
progres.specificneed.case2$At.Risk[is.na(progres.specificneed.case2$At.Risk)] <- "no"
progres.specificneed.case2$At.Risk <- as.factor(progres.specificneed.case2$At.Risk)

progres.specificneed.case2$Child.Labour.count <- progres.specificneed.case2$Child.Labour
progres.specificneed.case2$Child.Labour <- as.factor(ifelse(progres.specificneed.case2$Child.Labour >= 1, "yes", "no"))
progres.specificneed.case2$Child.Labour <- as.character(progres.specificneed.case2$Child.Labour)
progres.specificneed.case2$Child.Labour[is.na(progres.specificneed.case2$Child.Labour)] <- "no"
progres.specificneed.case2$Child.Labour <- as.factor(progres.specificneed.case2$Child.Labour)


progres.specificneed.case2$Child.marriage..parent.or.pregnancy.count <- progres.specificneed.case2$Child.marriage..parent.or.pregnancy
progres.specificneed.case2$Child.marriage..parent.or.pregnancy <- as.factor(ifelse(progres.specificneed.case2$Child.marriage..parent.or.pregnancy >= 1, "yes", "no"))
progres.specificneed.case2$Child.marriage..parent.or.pregnancy <- as.character(progres.specificneed.case2$Child.marriage..parent.or.pregnancy)
progres.specificneed.case2$Child.marriage..parent.or.pregnancy[is.na(progres.specificneed.case2$Child.marriage..parent.or.pregnancy)] <- "no"
progres.specificneed.case2$Child.marriage..parent.or.pregnancy <- as.factor(progres.specificneed.case2$Child.marriage..parent.or.pregnancy)


progres.specificneed.case2$Family.Needs.count <- progres.specificneed.case2$Family.Needs
progres.specificneed.case2$Family.Needs <- as.factor(ifelse(progres.specificneed.case2$Family.Needs >= 1, "yes", "no"))
progres.specificneed.case2$Family.Needs <- as.character(progres.specificneed.case2$Family.Needs)
progres.specificneed.case2$Family.Needs[is.na(progres.specificneed.case2$Family.Needs)] <- "no"
progres.specificneed.case2$Family.Needs <- as.factor(progres.specificneed.case2$Family.Needs)


progres.specificneed.case2$Marginalised.count <- progres.specificneed.case2$Marginalised
progres.specificneed.case2$Marginalised <- as.factor(ifelse(progres.specificneed.case2$Marginalised >= 1, "yes", "no"))
progres.specificneed.case2$Marginalised <- as.character(progres.specificneed.case2$Marginalised)
progres.specificneed.case2$Marginalised[is.na(progres.specificneed.case2$Marginalised)] <- "no"
progres.specificneed.case2$Marginalised <- as.factor(progres.specificneed.case2$Marginalised)


progres.specificneed.case2$Medical.count <- progres.specificneed.case2$Medical
progres.specificneed.case2$Medical <- as.factor(ifelse(progres.specificneed.case2$Medical >= 1, "yes", "no"))
progres.specificneed.case2$Medical <- as.character(progres.specificneed.case2$Medical)
progres.specificneed.case2$Medical[is.na(progres.specificneed.case2$Medical)] <- "no"
progres.specificneed.case2$Medical <- as.factor(progres.specificneed.case2$Medical)


progres.specificneed.case2$Need.of.Care.count <- progres.specificneed.case2$Need.of.Care
progres.specificneed.case2$Need.of.Care <- as.factor(ifelse(progres.specificneed.case2$Need.of.Care >= 1, "yes", "no"))
progres.specificneed.case2$Need.of.Care <- as.character(progres.specificneed.case2$Need.of.Care)
progres.specificneed.case2$Need.of.Care[is.na(progres.specificneed.case2$Need.of.Care)] <- "no"
progres.specificneed.case2$Need.of.Care <- as.factor(progres.specificneed.case2$Need.of.Care)


progres.specificneed.case2$Problem.with.violence.law.recruitment.count <- progres.specificneed.case2$Problem.with.violence.law.recruitment
progres.specificneed.case2$Problem.with.violence.law.recruitment <- as.factor(ifelse(progres.specificneed.case2$Problem.with.violence.law.recruitment >= 1, "yes", "no"))
progres.specificneed.case2$Problem.with.violence.law.recruitment <- as.character(progres.specificneed.case2$Problem.with.violence.law.recruitment)
progres.specificneed.case2$Problem.with.violence.law.recruitment[is.na(progres.specificneed.case2$Problem.with.violence.law.recruitment)] <- "no"
progres.specificneed.case2$Problem.with.violence.law.recruitment <- as.factor(progres.specificneed.case2$Problem.with.violence.law.recruitment)


progres.specificneed.case2$Separated.Child.count <- progres.specificneed.case2$Separated.Child
progres.specificneed.case2$Separated.Child <- as.factor(ifelse(progres.specificneed.case2$Separated.Child >= 1, "yes", "no"))
progres.specificneed.case2$Separated.Child <- as.character(progres.specificneed.case2$Separated.Child)
progres.specificneed.case2$Separated.Child[is.na(progres.specificneed.case2$Separated.Child)] <- "no"
progres.specificneed.case2$Separated.Child <- as.factor(progres.specificneed.case2$Separated.Child)


progres.specificneed.case2$Single.Parent.count <- progres.specificneed.case2$Single.Parent
progres.specificneed.case2$Single.Parent <- as.factor(ifelse(progres.specificneed.case2$Single.Parent >= 1, "yes", "no"))
progres.specificneed.case2$Single.Parent <- as.character(progres.specificneed.case2$Single.Parent)
progres.specificneed.case2$Single.Parent[is.na(progres.specificneed.case2$Single.Parent)] <- "no"
progres.specificneed.case2$Single.Parent <- as.factor(progres.specificneed.case2$Single.Parent)


progres.specificneed.case2$Unaccompanied.count <- progres.specificneed.case2$Unaccompanied
progres.specificneed.case2$Unaccompanied <- as.factor(ifelse(progres.specificneed.case2$Unaccompanied >= 1, "yes", "no"))
progres.specificneed.case2$Unaccompanied <- as.character(progres.specificneed.case2$Unaccompanied)
progres.specificneed.case2$Unaccompanied[is.na(progres.specificneed.case2$Unaccompanied)] <- "no"
progres.specificneed.case2$Unaccompanied <- as.factor(progres.specificneed.case2$Unaccompanied)


progres.specificneed.case2$Victim.of.Violence.count <- progres.specificneed.case2$Victim.of.Violence
progres.specificneed.case2$Victim.of.Violence <- as.factor(ifelse(progres.specificneed.case2$Victim.of.Violence >= 1, "yes", "no"))
progres.specificneed.case2$Victim.of.Violence <- as.character(progres.specificneed.case2$Victim.of.Violence)
progres.specificneed.case2$Victim.of.Violence[is.na(progres.specificneed.case2$Victim.of.Violence)] <- "no"
progres.specificneed.case2$Victim.of.Violence <- as.factor(progres.specificneed.case2$Victim.of.Violence)

progres.specificneed.case2$Woman.at.Risk.count <- progres.specificneed.case2$Woman.at.Risk
progres.specificneed.case2$Woman.at.Risk <- as.factor(ifelse(progres.specificneed.case2$Woman.at.Risk >= 1, "yes", "no"))
progres.specificneed.case2$Woman.at.Risk <- as.character(progres.specificneed.case2$Woman.at.Risk)
progres.specificneed.case2$Woman.at.Risk[is.na(progres.specificneed.case2$Woman.at.Risk)] <- "no"
progres.specificneed.case2$Woman.at.Risk <- as.factor(progres.specificneed.case2$Woman.at.Risk)

### Merging needs with progres case info###################################################
progres.case.sp <- merge(x = progres.case, y = progres.specificneed.case2, all.x = TRUE)

#### checking frequency for sp.needs
prop.table(table(progres.case.sp$At.Risk, useNA = "ifany"))
prop.table(table(progres.case.sp$Child.Labour, useNA = "ifany"))
prop.table(table(progres.case.sp$Child.marriage..parent.or.pregnancy, useNA = "ifany"))
prop.table(table(progres.case.sp$Family.Needs, useNA = "ifany"))
prop.table(table(progres.case.sp$Marginalised, useNA = "ifany"))
prop.table(table(progres.case.sp$Medical, useNA = "ifany"))
prop.table(table(progres.case.sp$Need.of.Care, useNA = "ifany"))
prop.table(table(progres.case.sp$Problem.with.violence.law.recruitment, useNA = "ifany"))
prop.table(table(progres.case.sp$Separated.Child, useNA = "ifany"))
prop.table(table(progres.case.sp$Single.Parent, useNA = "ifany"))
prop.table(table(progres.case.sp$Victim.of.Violence, useNA = "ifany"))
prop.table(table(progres.case.sp$Woman.at.Risk, useNA = "ifany"))


write.csv(progres.case.sp, file = "data/progrescase-1.csv", na = "", row.names = FALSE)

}
