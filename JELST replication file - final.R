getwd()

options(scipen = 999)

#load packages

#utility

library(MASS)
library(psych)
library(car)
library(tidyverse)
library(naniar)
library(labelled)
library(haven)

#measurement

library(lavaan)
library(semTools)
library(semPlot)

#multiple imputation

library(mice)
library(miceadds)
library(parallel)

#matching

library(MatchIt)
library(WeightIt)
library(MatchThem)
library(cobalt)

#modelling

library(lme4)
library(lfe)
library(parameters)
library(mitml)
library(MuMIn)

#diagnostics and output

library(broom)
library(jtools)
library(gt)
library(sjPlot)
library(modelsummary)
library(merTools)
library(emmeans)
library(multilevelTools)
library(extraoperators)
library(JWileymisc)

#START FROM SECTION 7 IF WORKING WITH PROVIDED DATA
#EARLIER SECTIONS ARE DATA LINKING, CLEANING AND IMPUTATION USING BCS 1970 DATA FROM UK DATA SERVICE

#####
#1. CONSTRUCT FULL DATASET

#1.1 BIRTH SWEEP
#find and import the BCS birth file

bcs_0 <- read_tsv("C:/Users/ralph/Desktop/Working data/BCS/0/bcs7072a.tab", na = "NA")

bcs_0 <- select(bcs_0, bcsid, a0005a, a0009, a0010, a0255)

summary(bcs_0)

#recode missing values as NA

bcs_0 <- replace_with_na_all(bcs_0, condition = ~.x < 0)

#recode as factors

bcs_0$female <- factor(dplyr::recode(bcs_0$a0255,"1"=0,"2"=1))
bcs_0$a0009 <- factor(bcs_0$a0009)
bcs_0$a0010 <- factor(bcs_0$a0010)

#create new base datafile

bcs_full <- select(bcs_0, bcsid, female, m_age = a0005a, medu_age = a0009, fedu_age = a0010)

bcs_full

#import the birth derived variable file

bcs_0d <- read_tsv("C:/Users/ralph/Desktop/Working data/BCS/0/bcs1derived.tab", na = "NA", )

bcs_0d <- dplyr::select(bcs_0d, BCSID, BD1PSOC)

summary(bcs_0d)

table(bcs_0d$BD1PSOC)

#recode missing and unknown values as NA

bcs_0d$BD1PSOC[bcs_0d$BD1PSOC==-1] <- NA
bcs_0d$BD1PSOC[bcs_0d$BD1PSOC==2] <- NA

#recode as 5-level variable

bcs_0d$parscr <- relevel(factor(dplyr::recode(bcs_0d$BD1PSOC,"1"=1,"2"=99,"3"=1,
                                  "4"=2,"5"=3,"6"=4,"7"=5,"8"=6)), ref="3")

#join

bcs_full <- full_join(bcs_full, select(bcs_0d, bcsid = BCSID, parscr),by="bcsid")

#import childhood conditions datafile

bcs_wp9 <- read_tsv("C:/Users/ralph/Desktop/Working data/BCS/Harmonised/bcs70_closer_wp9.tab", na = "NA")

bcs_wp9

#low birthweight

table(bcs_wp9$lowbwt)

bcs_wp9$lowbwt[bcs_wp9$lowbwt==-111] <- NA
bcs_wp9$lowbwt[bcs_wp9$lowbwt==-999] <- NA

bcs_wp9$lowbwt <- factor(bcs_wp9$lowbwt)

#Overcrowding

table(bcs_wp9$crowd)

bcs_wp9$crowd[bcs_wp9$crowd==-111] <- NA
bcs_wp9$crowd[bcs_wp9$crowd==-999] <- NA

#Home ownership: 1 = rented at both time-points, 2 = owned at one, 3 owned at both

table(bcs_wp9$tenure)

bcs_wp9$tenure[bcs_wp9$tenure==-111] <- NA
bcs_wp9$tenure[bcs_wp9$tenure==-999] <- NA

#full join

bcs_full <- full_join(bcs_full, select(bcs_wp9, bcsid, lowbwt, crowd, chi_ten = tenure), by = "bcsid") 

#1.2 AGE 5 SWEEP
#Mother's questionnaire - for maternal attitudes

bcs_5_mother <- read_tsv("C:/Users/ralph/Desktop/Working data/BCS/5/f699a.tab", na = "NA")

#select attitudinal variables

bcs_5_mother <- select(bcs_5_mother,"bcsid","d070","d079","d080","d081","d087","d090",
                      "d094","d097","d099","d100","d103","d106","d107")

#recode missing data as NAs

bcs_5_mother <- replace_with_na_all(bcs_5_mother, condition = ~.x %in% -3)

#recode so higher score = more authoritarian

bcs_5_mother$d070r <- dplyr::recode(bcs_5_mother$d070, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d079r <- dplyr::recode(bcs_5_mother$d079, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d080r <- dplyr::recode(bcs_5_mother$d080, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d081r <- dplyr::recode(bcs_5_mother$d081, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d087r <- dplyr::recode(bcs_5_mother$d087, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d094r <- dplyr::recode(bcs_5_mother$d094, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d097r <- dplyr::recode(bcs_5_mother$d097, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d099r <- dplyr::recode(bcs_5_mother$d099, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d100r <- dplyr::recode(bcs_5_mother$d100, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d103r <- dplyr::recode(bcs_5_mother$d103, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d106r <- dplyr::recode(bcs_5_mother$d106, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_5_mother$d107r <- dplyr::recode(bcs_5_mother$d107, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)

bcs_5_mother

#join

bcs_full <- full_join(bcs_full, select(bcs_5_mother,bcsid,d070r,d079r,d080r,d081r,d087r,d090,
                                       d094r,d097r,d099r,d100r,d103r,d106r,d107r),by="bcsid")

#Home Interview questionnaire - for parental education

bcs_5_home <- read_tsv("C:/Users/ralph/Desktop/Working data/BCS/5/f699b.tab", na = "NA")

bcs_5_home <- dplyr::select(bcs_5_home, bcsid, e190)

bcs_5_home$pared <- dplyr::recode(bcs_5_home$e190,"-1"=99,"-2"=99,"-3"=99,"1"=1,"2"=2,"3"=2,
                                  "4"=2,"5"=2,"6"=2,"7"=3,"8"=99)

bcs_5_home$pared[bcs_5_home$pared==99] <- NA

bcs_5_home$pared <- as.factor(bcs_5_home$pared)

#join

bcs_full <- full_join(bcs_full, select(bcs_5_home, bcsid, pared), by="bcsid")

bcs_full

#1.3 AGE 10 SWEEP for ability scales

bcs_10d <- read_tsv("C:/Users/ralph/Desktop/Working data/BCS/10/bcs3derived.tab", na = "NA")

#create a tibble with only the variables needed and checks NAs

bcs_10d <- dplyr::select(bcs_10d, bcsid = BCSID, 10:11)

bcs_10d

#recode missing data as NAs

bcs_10d <- replace_with_na_all(bcs_10d, condition = ~.x %in% -1)

bcs_10d

#rescale variables

bcs_10d$BD3RDAGE <- scale(bcs_10d$BD3RDAGE)[,1]
bcs_10d$BD3MATHS <- scale(bcs_10d$BD3MATHS)[,1]

#join

bcs_full <- full_join(bcs_full, select(bcs_10d, bcsid, eng10 = BD3RDAGE, maths10 = BD3MATHS), by="bcsid")

bcs_full

#1.4 AGE 16 SWEEP
#Attitudes scales

bcs_16 <- read_tsv("C:/Users/ralph/Desktop/Working data/BCS/16/bcs7016x.tab", na = "NA")

#create a tibble with only the variables needed

bcs_16_atts <- dplyr::select(bcs_16, bcsid, c5p8, c5p15, c5p1, c5p14, c5p2, c5p3)

#code NAs

bcs_16_atts <- replace_with_na_all(bcs_16_atts, condition = ~.x %in% c(-2, -1))

#best alpha (0.64) is just these two elements:
#c5p8 - Black people should not marry white people
#c5p15 - Black people are just as good as white people

bcs_16_atts$c5p8r <- dplyr::recode(bcs_16_atts$c5p8, "1" = 3, "2" = 2, "3" = 1)

bcs_16_atts$c5p8r

psych::alpha(bcs_16_atts[c("c5p8r","c5p15")], check.keys = T)

#Best alpha (0.63) is of these two elements:
#c5p1 - Flogging should be brought back for violent crime
#c5p14 - Hanging should be brought back (for murder)

bcs_16_atts$c5p1r <- dplyr::recode(bcs_16_atts$c5p1, "1" = 3, "2" = 2, "3" = 1)
bcs_16_atts$c5p14r <- dplyr::recode(bcs_16_atts$c5p14, "1" = 3, "2" = 2, "3" = 1)

bcs_16_atts

psych::alpha(bcs_16_atts[c("c5p1r","c5p14r")], check.keys = T)

#c5p2 #Trade unions are necessary to represent workers rights
#c5p3 #Strikes should be made illegal

bcs_16_atts$c5p3r <- dplyr::recode(bcs_16_atts$c5p3, "1" = 3, "2" = 2, "3" = 1)

psych::alpha(bcs_16_atts[c("c5p2","c5p3r")], check.keys = T)

#join

bcs_full <- full_join(bcs_full, select(bcs_16_atts, bcsid, c5p8r, c5p15, c5p1r, c5p14r,
                                       c5p2, c5p3r), by="bcsid")

bcs_full <- distinct(bcs_full)

#Qualifications
#Derived score from all grades achieved in public examinations at age 16 (Grade A = 7, Grade 5 CSE = 1) (age 16): (range: 0-99)

bcs_16_quals <- bcs_16[c(1,4551:4646)]

bcs_16_quals <- replace_with_na_all(bcs_16_quals, condition = ~.x %in% c(-4, -2, -1))

#recode O-levels

onames <- c(str_subset(names(bcs_16_quals[2:57]),"1.2$"),str_subset(names(bcs_16_quals[58:96]),"1.3$"))

bcs_16_quals <- bcs_16_quals %>%
  mutate_at(onames, funs(dplyr::recode(.,"1"=7,"2"=6,"3"=5,"4"=4,"5"=3,"6"=0)))

#recode CSEs

csenames <- str_subset(names(bcs_16_quals),"2.2$")

bcs_16_quals <- bcs_16_quals %>%
  mutate_at(csenames, funs(dplyr::recode(.,"1"=5,"2"=4,"3"=3,"4"=2,"5"=1,"6"=0)))

#calculate total score (including NAs if all quals are missing)

allquals <- c(onames,csenames)

bcs_16_quals$q16score <- ifelse(apply(is.na(bcs_16_quals[allquals]),1,all),
                                NA,rowSums(bcs_16_quals[allquals],na.rm=TRUE))

table(bcs_16_quals$q16score)

#Whether CM has a Grade A-C English O-level/GCSE or Grade 1 CSE (age 16, 30): 0=no, 1=yes

table(bcs_16_quals$t2a1.2)

bcs_16_quals$eng16 <- ifelse(bcs_16_quals$t2a1.2 > 4 | bcs_16_quals$t2a2.2 == 5,1,
                             ifelse(apply(is.na(bcs_16_quals[allquals]),1,all),NA,0))

table(bcs_16_quals$eng16)

#Whether CM has a Grade A-C Math O-level/GCSE or Grade 1 CSE (age 16, 30): 0=no, 1=yes

bcs_16_quals$maths16 <- ifelse(bcs_16_quals$t2c1.2 > 4 | bcs_16_quals$t2c2.2 == 5,1,
                               ifelse(apply(is.na(bcs_16_quals[allquals]),1,all),NA,0))

table(bcs_16_quals$maths16)

#join

bcs_full <- full_join(bcs_full, select(bcs_16_quals, bcsid, q16score, eng16, maths16), by="bcsid")

bcs_full <- distinct(bcs_full)

#Cognition at 16: A standardised score from a Principal Components Analysis (PCA) of five assessment scores completed by study members at age 16.
#Appendix from this is useful: https://cls.ucl.ac.uk/wp-content/uploads/2017/07/BCS70-Childhood-cognition-in-the-1970-British-Cohort-Study-Nov-2014-final.pdf

#Vocabulary

bcs_16_d <- read_tsv("C:/Users/ralph/Desktop/Working data/BCS/16/bcs4derived.tab", na = "NA")

bcs_16_d$BD4RREAD[bcs_16_d$BD4RREAD==-1] <- NA

bcs_16_d$vocab16 <- scale(bcs_16_d$BD4RREAD)[,1]

#join

bcs_full <- full_join(bcs_full, select(bcs_16_d, bcsid = BCSID, vocab16), by="bcsid")

bcs_full

#Spelling

bcs_16_spell <- bcs_16[c(1,639:838)]

bcs_16_spell <- replace_with_na_all(bcs_16_spell, condition = ~.x %in% c(-2, -1))

bcs_16_spell$total <- ifelse(apply(is.na(bcs_16_spell[2:200]),1,all),
                             NA,rowSums(bcs_16_spell[2:200],na.rm=TRUE))

bcs_16_spell$spell16 <- scale(bcs_16_spell$total)[,1]

#join

bcs_full <- full_join(bcs_full, select(bcs_16_spell, bcsid, spell16), by="bcsid")

bcs_full

#Reading

bcs_16_reading <- read_tsv("C:/Users/ralph/Desktop/Working data/BCS/16/bcs1986_reading_matrices.tab", na = "NA")

bcs_16_reading[177:182] <- replace_with_na_all(bcs_16_reading[177:182], condition = ~.x %in% c(-1))

bcs_16_reading$read16 <- rowSums(bcs_16_reading[177:181])

bcs_16_reading$read16 <- scale(bcs_16_reading$read16)[,1]

#Matrices

table(bcs_16_reading[182])

bcs_16_reading$matrix16 <- scale(bcs_16_reading[182])[,1]

#join

bcs_full <- full_join(bcs_full, dplyr::select(bcs_16_reading, bcsid = BCSID, read16, matrix16), by = "bcsid")

bcs_full

#Arithmetic

bcs_16_arith <- read_tsv("C:/Users/ralph/Desktop/Working data/BCS/16/bcs70_16-year_arithmetic_data.tab", na = "NA")

table(bcs_16_arith$mathscore)

bcs_16_arith$arith16 <- scale(bcs_16_arith$mathscore)[,1]

bcs_full <- full_join(bcs_full, dplyr::select(bcs_16_arith, bcsid, arith16), by = "bcsid")

#check scale and form final version

alpha(select(bcs_full,vocab16,spell16,read16,matrix16,arith16))

bcs_full$cog16 <- scale(rowMeans(select(bcs_full,vocab16,spell16,read16,matrix16,arith16), na.rm = T))[,1]

bcs_full$cog16 <- ifelse(is.nan(bcs_full$cog16), NA, bcs_full$cog16)

hist(bcs_full$cog16)

#1.5 AGE 26 SWEEP

bcs_26 <- read_tsv("C:\\Users\\ralph\\Desktop\\Working data\\BCS\\26\\bcs96x.tab", na = "NA")

#reduce to required variables

bcs_26 <- bcs_26 %>%
  dplyr::select(bcsid,b960219,b960220,b960221,b960120,b960123,b960425,b960428,b960430,
         b960218,hqual26c,b960215,b960165,b960212,b960177,b960175,b960169,b960160,b960154,
         b960148,b960159,b960162,b960153,b960156,b960147,b960150,b960171,b960174,b960129)

bcs_26 <- replace_with_na_all(bcs_26, condition = ~.x < 0)

#ISCED 2011 mapping
#based on: http://uis.unesco.org/en/isced-mappings

table(bcs_26$b960221)

bcs_26$isced26 <- case_when(bcs_26$b960221==26 ~ "7+",
                            bcs_26$b960220==24 ~ "7+",
                            bcs_26$b960219==23 ~ "6",
                            bcs_26$b960218==22 ~ "5",
                            bcs_26$hqual26c==5 ~ "5",
                            bcs_26$hqual26c==4 ~ "4",
                            bcs_26$b960215==20 ~ "3",
                            bcs_26$b960165==11 ~ "3",
                            bcs_26$b960212==19 ~ "3",
                            bcs_26$b960177==17 ~ "3",
                            bcs_26$b960177==17 ~ "3",
                            bcs_26$b960175>4 ~ "3",
                            bcs_26$b960169>4 ~ "3",
                            bcs_26$b960160>4 ~ "3",
                            bcs_26$b960154>4 ~ "3",
                            bcs_26$b960148>4 ~ "3",
                            bcs_26$hqual26c==3 ~ "3",
                            bcs_26$b960159==8 ~ "2",
                            bcs_26$b960162==9 ~ "2",
                            bcs_26$b960153==5 ~ "2",
                            bcs_26$b960156==6 ~ "2",
                            bcs_26$b960147==2 ~ "2",
                            bcs_26$b960150==3 ~ "2",
                            bcs_26$b960171==14 ~ "2",
                            bcs_26$b960174==16 ~ "2",
                            bcs_26$hqual26c==2 ~ "2",
                            bcs_26$b960129>12 ~ "2",
                            bcs_26$b960129<13 ~ "1")

table(is.na(bcs_26$isced26))

table(bcs_26$isced26)

#VALUES
#See p 160 of here: https://cls.ucl.ac.uk/wp-content/uploads/2017/07/Guide_to_the_26-year_data.pdf

#AUTHORITARIANISM
#Alpha of 0.55

table(bcs_26$b960120)
table(bcs_26$b960123)
table(bcs_26$b960428)
table(bcs_26$b960425)

psych::alpha(bcs_26[c("b960120","b960123","b960428","b960425")], check.keys = T)

#recode so a higher value is more authoritarian

bcs_26$b960120r <- dplyr::recode(bcs_26$b960120, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_26$b960123r <- dplyr::recode(bcs_26$b960123, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_26$b960428r <- dplyr::recode(bcs_26$b960428, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_26$b960425r <- dplyr::recode(bcs_26$b960425, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)

#LEFT-RIGHT

bcs_26$b960430

#join

bcs_full <- full_join(bcs_full,dplyr::select(bcs_26,bcsid,isced26,b960120r,b960123r,
                                             b960425r,b960428r,b960430),
                      by = "bcsid")

bcs_full

#1.6 AGE 30 SWEEP
#import both datasets

bcs_30 <- read_dta("C:/Users/ralph/Desktop/Working data/BCS/30/bcs2000.dta")
bcs_30d <- read_tsv("C:/Users/ralph/Desktop/Working data/BCS/30/bcs6derived.tab", na = "NA")

#code ISCED
#first bring in vocational NVQ coding

bcs_30 <- bcs_30 %>%
  left_join(dplyr::select(bcs_30d,bcsid=BCSID,HIVOC00))

bcs_30$edcse1[bcs_30$edcse1==99] <- NA
bcs_30$edcse2[bcs_30$edcse2==99] <- NA
bcs_30$edolev1[bcs_30$edolev1==99] <- NA
bcs_30$edolev1[bcs_30$edolev1==98] <- NA
bcs_30$edolev2[bcs_30$edolev2==99] <- NA
bcs_30$edolev2[bcs_30$edolev2==98] <- NA
bcs_30$edgcse1[bcs_30$edgcse1==99] <- NA
bcs_30$edgcse2[bcs_30$edgcse2==99] <- NA

#then code variable

bcs_30$isced30 <- case_when(bcs_30$numhghdg>0 ~ "7+",
                            bcs_30$pgceyear==1 ~ "7+",
                            bcs_30$numdeg>0 ~ "6",
                            bcs_30$numothdg>0 ~ "6",
                            bcs_30$eddiped>0 ~ "5",
                            bcs_30$HIVOC00==5 ~ "5",
                            bcs_30$HIVOC00==4 ~ "4",
                            bcs_30$numgcsas>0 ~ "3",
                            bcs_30$numaslvl>0 ~ "3",
                            bcs_30$edolev1>4 ~ "3",
                            bcs_30$edcse1>4 ~ "3",
                            bcs_30$edgcse1>4 ~ "3",
                            bcs_30$edscote>0 ~ "3",
                            bcs_30$edscotd>0 ~ "3",
                            bcs_30$edscotc>0 ~ "3",
                            bcs_30$edscotb>0 ~ "3",
                            bcs_30$HIVOC00==3 ~ "3",
                            bcs_30$edolev2>4 ~ "2",
                            bcs_30$edcse2>4 ~ "2",
                            bcs_30$edgcse2>4 ~ "2",
                            bcs_30$edscota>0 ~ "2",
                            bcs_30$HIVOC00==2 ~ "2",
                            bcs_30$actagel2>12 ~ "2",
                            bcs_30$actagel2<13 ~ "1")

table(bcs_30$isced30)

prop.table(table(bcs_30$isced30))

#slim down variables

bcs_30 <- bcs_30 %>%
  dplyr::select(bcsid, isced30, ethnic, lr1, lr2, lr3, lr4, lr5, lr6,
                lr7, ar1, ar2, ar3, ar4, ar5, a1, a2, a3, a4, a5, a6) %>%
  remove_labels()

#Ethnicity

bcs_30$nonbrit30 <- dplyr::recode(bcs_30$ethnic,"1"=0,"2"=1,"3"=1,
                                        "4"=1,"5"=1,"6"=1,"7"=1,"8"=1,"9"=0,"10"=1,
                                        "11"=1,"12"=1,"13"=1,"14"=1,"15"=1,
                                        "16"=1,"98"=99)

bcs_30$nonbrit30[bcs_30$nonbrit30==99] <- NA

bcs_30$nonbrit30 <- as.factor(bcs_30$nonbrit30)

table(bcs_30$nonbrit30)

#Attitudes

#first remove coded NAs from all variables

bcs_30[4:21] <- replace_with_na_all(bcs_30[4:21], condition = ~.x %in% c(8, 9))

bcs_30

#Produce a prejudice scale - need to recode ar5 to be in same direction
#Higher number is more prejudiced

bcs_30$ar5r <- dplyr::recode(bcs_30$ar5, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)

#Produce an authoritarianism scale - higher number is more authoritarian
#All variables need to be recoded

bcs_30$a1r <- dplyr::recode(bcs_30$a1, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_30$a2r <- dplyr::recode(bcs_30$a2, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_30$a3r <- dplyr::recode(bcs_30$a3, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_30$a4r <- dplyr::recode(bcs_30$a4, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_30$a5r <- dplyr::recode(bcs_30$a5, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_30$a6r <- dplyr::recode(bcs_30$a6, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)

#join

bcs_30

bcs_full <- full_join(bcs_full,dplyr::select(bcs_30, bcsid, isced30, nonbrit30, lr1, lr2,
                                             lr3, lr4, lr5, lr6, lr7, ar1, ar2, ar3, ar4,
                                             ar5r, a1r, a2r, a3r, a4r, a5r, a6r),
                      by = "bcsid")

bcs_full

#1.7 AGE 42 SWEEP

bcs_42 <- read_tsv("C:/Users/ralph/Desktop/Working data/BCS/42/bcs70_2012_flatfile.tab", na = "NA")
bcs_42d <- read_tsv("C:/Users/ralph/Desktop/Working data/BCS/42/bcs70_2012_derived.tab", na = "NA")

#code ISCED42
#need to link to main file as dependent on ISCED30

bcs_full <- bcs_full %>%
  left_join(dplyr::select(bcs_42,bcsid=BCSID,B9WHQU02,B9WHQU03,B9SCTQ05,B9SCTQ06,B9SCTQ07,
                          B9WHQU05,B9WHQU06,B9WHQU07,B9WHQU08,B9WHQU10))

bcs_full <- bcs_full %>%
  left_join(dplyr::select(bcs_42d,bcsid=BCSID,BD9VNVQ1))

bcs_full$isced42 <- case_when(bcs_full$B9WHQU10==1 ~ "7+",
                                bcs_full$B9WHQU08==1 ~ "7+",
                                bcs_full$B9WHQU07==1 & bcs_full$isced30 != "7+" ~ "6",
                                bcs_full$B9WHQU06==1 & bcs_full$isced30 != "7+" ~ "6",
                                bcs_full$B9WHQU05==1 & bcs_full$isced30 != "7+" & bcs_full$isced30 != "6" ~ "5",
                                bcs_full$BD9VNVQ1==5 & bcs_full$isced30 != "7+" & bcs_full$isced30 != "6" ~ "5",
                                bcs_full$BD9VNVQ1==4 & bcs_full$isced30 != "7+" & bcs_full$isced30 != "6" & bcs_full$isced30 != "5" ~ "4",
                                bcs_full$B9SCTQ07==1 & bcs_full$isced30 != "7+" & bcs_full$isced30 != "6" & bcs_full$isced30 != "5" & bcs_full$isced30 != "4" ~ "3",
                                bcs_full$B9SCTQ06==1 & bcs_full$isced30 != "7+" & bcs_full$isced30 != "6" & bcs_full$isced30 != "5" & bcs_full$isced30 != "4" ~ "3",
                                bcs_full$B9SCTQ05==1 & bcs_full$isced30 != "7+" & bcs_full$isced30 != "6" & bcs_full$isced30 != "5" & bcs_full$isced30 != "4" ~ "3",
                                bcs_full$B9WHQU03==1 & bcs_full$isced30 != "7+" & bcs_full$isced30 != "6" & bcs_full$isced30 != "5" & bcs_full$isced30 != "4" ~ "3",
                                bcs_full$B9WHQU02==1 & bcs_full$isced30 != "7+" & bcs_full$isced30 != "6" & bcs_full$isced30 != "5" & bcs_full$isced30 != "4" ~ "3",
                                bcs_full$BD9VNVQ1==3 & bcs_full$isced30 != "7+" & bcs_full$isced30 != "6" & bcs_full$isced30 != "5" & bcs_full$isced30 != "4" ~ "3",
                                bcs_full$BD9VNVQ1==2 & bcs_full$isced30 != "7+" & bcs_full$isced30 != "6" & bcs_full$isced30 != "5" & bcs_full$isced30 != "4" ~ "3",
                                TRUE ~ bcs_full$isced30)

table(bcs_full$isced26)
table(bcs_full$isced30)
table(bcs_full$isced42)

bcs_full <- bcs_full %>%
  dplyr::select(-B9WHQU02,-B9WHQU03,-B9SCTQ05,-B9SCTQ06,-B9SCTQ07,-B9WHQU05,-B9WHQU06,
                -B9WHQU07,-B9WHQU08,-B9WHQU10,-BD9VNVQ1)

#Attitudes

bcs_42 <- bcs_42 %>%
  select(bcsid = BCSID,B9SCQ3D,B9SCQ3G,B9SCQ20C,B9SCQ43A,B9SCQ3A,B9SCQ20J,B9SCQ43F,B9SCQ43H,
         B9SCQ43I,B9SCQ3B,B9SCQ20A)

bcs_42 <- replace_with_na_all(bcs_42, condition = ~.x < 0)

#AUTHORITARIANISM
#alpha of 0.28....

psych::alpha(bcs_42[c("B9SCQ3D","B9SCQ20C","B9SCQ43A")], check.keys = T)

#recode so a higher value is more authoritarian

bcs_42$B9SCQ3Dr <- dplyr::recode(bcs_42$B9SCQ3D, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_42$B9SCQ20Cr <- dplyr::recode(bcs_42$B9SCQ20C, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
bcs_42$B9SCQ43Ar <- dplyr::recode(bcs_42$B9SCQ43A, "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)

#LEFT-RIGHT
#alpha of 0.62

psych::alpha(bcs_42[c("B9SCQ3A","B9SCQ20J","B9SCQ43F","B9SCQ43H","B9SCQ43I")],
             check.keys = T)

#alpha of 0.68 with NHS item dropped

psych::alpha(bcs_42[c("B9SCQ3A","B9SCQ43F","B9SCQ43H","B9SCQ43I")],
             check.keys = T)

#PREJUDICE
#alpha of 0.67

psych::alpha(bcs_42[c("B9SCQ3B","B9SCQ20A")],check.keys = T)

#join

bcs_full <- full_join(bcs_full,dplyr::select(bcs_42,-B9SCQ3D,-B9SCQ20C,-B9SCQ20J,-B9SCQ43A),
                      by = "bcsid")

bcs_full

#1.8 FINALISE
#code treatment variables

names(bcs_full)

bcs_full$deg26 <- case_when(bcs_full$isced26 == "7+" ~ 1,
                            bcs_full$isced26 == "6" ~ 1,
                            !is.na(bcs_full$isced26) ~ 0)

table(bcs_full$deg26)

table(is.na(bcs_full$deg26))

bcs_full$deg30 <- case_when(bcs_full$isced30 == "7+" ~ 1,
                            bcs_full$isced30 == "6" ~ 1,
                            bcs_full$isced26 == "7+" ~ 1,
                            bcs_full$isced26 == "6" ~ 1,
                            !is.na(bcs_full$isced26)|!is.na(bcs_full$isced30) ~ 0)

table(bcs_full$deg30)

bcs_full$deg42 <- case_when(bcs_full$isced42 == "7+" ~ 1,
                            bcs_full$isced42 == "6" ~ 1,
                            bcs_full$isced30 == "7+" ~ 1,
                            bcs_full$isced30 == "6" ~ 1,
                            bcs_full$isced26 == "7+" ~ 1,
                            bcs_full$isced26 == "6" ~ 1,
                            !is.na(bcs_full$isced26)|!is.na(bcs_full$isced30)|!is.na(bcs_full$isced42) ~ 0)

table(bcs_full$deg26,bcs_full$deg42)

#examine duplicates - these are attributable to NAs in Maths at 10 variable

duplicates <- bcs_full[duplicated(bcs_full$bcsid)|duplicated(bcs_full$bcsid,fromLast = T),]

View(duplicates)

#remove duplicate IDs keeping Maths data

bcs_full <- bcs_full %>% arrange(bcsid,maths10)

bcs_full <- bcs_full[!duplicated(bcs_full$bcsid),]

bcs_full

table(duplicated(bcs_full$bcsid))

#remove those who only have BCSID
#684 of these

bcs_full %>%
  filter(!is.na(bcsid) & if_all(2:82, ~ is.na(.)))

bcs_full <- bcs_full %>%
  filter(!if_all(2:82, ~ is.na(.)))

#save as RDS and CSV

names(bcs_full)

write_csv(bcs_full, "bcs_full.csv")

write_rds(bcs_full, "bcs_full.rds")

bcs_full <- read_rds("bcs_full.rds")

#####
#2. MULTIPLE IMPUTATION

bcs_full <- read_rds("bcs_full.rds")

bcs_full$bcsid

#Import response dataset and join to remove cases who emigrate or die by 42

bcs_resp <- read_tsv("C:/Users/ralph/Desktop/Working data/BCS/Response/bcs_response.tab")

bcs_resp

#all of those in full file are in response file

setdiff(bcs_full$bcsid,bcs_resp$BCSID)

#code a variable for dead or emigrated at 42

bcs_resp$remove <- ifelse(bcs_resp$OUTCME09 == 7 | bcs_resp$OUTCME09 == 8,1,0)

table(bcs_resp$remove)

#join this and response to birth wave

bcs_mi_42 <- left_join(select(bcs_full,-vocab16,-spell16,-read16,-matrix16,-arith16,-deg26,
                             -deg30,-deg42),
                      select(bcs_resp,bcsid=BCSID,birth_resp=OUTCME01,remove))

#filter out those who were dead or permanently emigrated by 42
#or didn't respond to birth wave

bcs_mi_42 <- bcs_mi_42 %>%
  filter(remove==0 & birth_resp==1)

#then drop these variables

bcs_mi_42 <- bcs_mi_42 %>%
  select(-birth_resp,-remove)

#recode factors as factors

factors <- c("female","parscr","pared","isced26","isced30","isced42","nonbrit30",
             "lowbwt","crowd","chi_ten","eng16","maths16")

bcs_mi_42 <- bcs_mi_42 %>% 
  mutate_at(factors,as.factor)

bcs_mi_42

#save file

write_csv(bcs_mi_42, "bcs_mi_42.csv")

write_rds(bcs_mi_42, "bcs_mi_42.rds")

bcs_mi_42 <- read_rds("bcs_mi_42.rds")

#carry out imputation

#first derive object without imputations to change settings

ini <- mice(bcs_mi_42, maxit = 0, print = FALSE)

ini$loggedEvents

#check predictors

pred_1 <- ini$pred

pred_1

#change methods to predictive mean matching for all

meth_1 <- ini$method

meth_1 <- str_replace_all(meth_1,c("logreg"="pmm","polr"="pmm","polyreg"="pmm"))

#carry out test imputation to review any issues

imp_test <- mice(bcs_mi_42, pred = pred_1, method = meth_1, m = 1, maxit = 1)

imp_test$loggedEvents

#recode to collapse categories of those with few cases (which are being removed)

table(bcs_mi_42$medu_age)

bcs_mi_42$medu_age <- as.numeric(bcs_mi_42$medu_age)

bcs_mi_42$medu_age <- case_when(bcs_mi_42$medu_age<10 ~ 10,
                                bcs_mi_42$medu_age>25 ~ 25,
                                TRUE ~ bcs_mi_42$medu_age)

table(bcs_mi_42$fedu_age)

bcs_mi_42$fedu_age <- as.numeric(bcs_mi_42$fedu_age)

bcs_mi_42$fedu_age <- case_when(bcs_mi_42$fedu_age<10 ~ 10,
                                bcs_mi_42$fedu_age>30 ~ 30,
                                TRUE ~ bcs_mi_42$fedu_age)

table(bcs_mi_42$isced26)

bcs_mi_42$isced26 <- factor(ifelse(bcs_mi_42$isced26=="1","2",bcs_mi_42$isced26))

class(bcs_mi_42$isced26)

#carry out more imputations and iterations

imp_test <- mice(bcs_mi_42, pred = pred_1, method = meth_1, m = 5, maxit = 5)

imp_test$loggedEvents

#fit multiple regression and investigate results

mi_test_fit <- with(imp_test, lm(B9SCQ43H ~ isced42 + female + parscr + pared))

pool_test <- pool(mi_test_fit)

pool_test

summary(pool_test)

#run many more imputations using parallel processing
#according to: https://cls.ucl.ac.uk/wp-content/uploads/2020/04/Handling-missing-data-in-the-National-Child-Development-Study-User-Guide.pdf
#should run as many imputations as highest FMI percentage in the model - so 75 in this case

detectCores()

bcs_mi_42

bcs_42_imp <- parlmice(bcs_mi_42, pred = pred_1, method = meth_1, n.core = 3,
                       n.imp.core = 25, maxit = 20, cluster.seed = 1987, printFlag = T)

save(bcs_42_imp, file = "bcs_42_imp.rda")

load(file = "bcs_42_imp.rda")

#run a regression again

bcs_42_fit <- with(bcs_42_imp, lm(B9SCQ43H ~ isced42 + female + parscr + pared))

pool_bcs_42_fit <- pool(bcs_42_fit)

pool_bcs_42_fit

summary(pool_bcs_42_fit)

#####
#3. ATTITUDE SCALE TESTING AND CREATION

bcs_full <- read_rds("bcs_full.rds")

#remove unneeded variables

names(bcs_full)

mi_vars <- c("m_age","medu_age","fedu_age","crowd","chi_ten","lowbwt","q16score","eng16",
             "maths16","vocab16","spell16","read16","matrix16","arith16","cog16")

bcs_full <- select(bcs_full, !mi_vars)

#reduce to complete cases

bcs_cc_42 <- bcs_full[complete.cases(bcs_full),]

#Inverse covariance weighted scales functions
#From: https://github.com/cdsamii/make_index/blob/master/r/index_comparison.R

matStand <- function(x, sgroup = rep(TRUE, nrow(x))){
  for(j in 1:ncol(x)){
    x[,j] <- (x[,j] - mean(x[sgroup,j]))/sd(x[sgroup,j])
  }
  return(x)
}

icwIndex <- function(	xmat,
                      wgts=rep(1, nrow(xmat)),
                      revcols = NULL,
                      sgroup = rep(TRUE, nrow(xmat))){
  X <- matStand(xmat, sgroup)
  if(length(revcols)>0){
    X[,revcols] <-  -1*X[,revcols]
  }
  i.vec <- as.matrix(rep(1,ncol(xmat)))
  Sx <- cov.wt(X, wt=wgts)[[1]]
  weights <- solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)
  index <- t(solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)%*%t(X))
  return(list(weights = weights, index = index))
}

#9.1 MATERNAL ATTITUDES SCALE
#9.1.1 CFA to test model fit
# 1 Factor model

matts1 <- ' auth =~ d070r + d079r + d080r + d081r + d087r + d090 + d094r + d097r + d099r + d100r + d103r + d106r + d107r '

matts.cfa1 <- cfa(matts1, data=bcs_cc_42, std.lv=TRUE)

summary(matts.cfa1, fit.measures=TRUE)

# 9.1.2 Calculate composite reliability

reliability(matts.cfa1)

psych::alpha(bcs_cc_42[c("d070r","d079r","d080r","d081r","d087r","d090","d094r","d097r","d099r",
                   "d100r","d103r","d106r","d107r")], check.keys = T)

# 9.1.3 Create scale

bcs_cc_42$Mauth <- scale(rowMeans(bcs_cc_42[c("d070r","d079r","d080r","d081r","d087r",
                                           "d090","d094r","d097r","d099r",
                                           "d100r","d103r","d106r","d107r")]))
bcs_cc_42$Mauth <- bcs_cc_42$Mauth[,1]

summary(bcs_cc_42$Mauth)
mean(bcs_cc_42$Mauth)
sd(bcs_cc_42$Mauth)

Mauthindex <- icwIndex(as.matrix(bcs_cc_42[,c("d070r","d079r","d080r","d081r","d087r",
                                               "d090","d094r","d097r","d099r",
                                               "d100r","d103r","d106r","d107r")]))

bcs_cc_42$Mauthw <- as.double(Mauthindex$index)

#9.2 ATTITUDES AT 16
#9.2.1 CFA TO TEST MODEL FIT
# 3 Factor model

atts16.3 <- ' prej =~ c5p8r + c5p15
            auth =~ c5p1r + c5p14r
            LR =~ c5p2 '

atts16cfa3 <- cfa(atts16.3, data=bcs_cc_42, std.lv=TRUE)

summary(atts16cfa3, fit.measures=TRUE)

semPaths(atts16cfa3, what = "std", style = "lisrel", layout = "tree", edge.color = "black",
         filetype = "tiff", filename = "atts16cfa3", height = 5, width = 7, trans = 1)

# 2 Factor model

atts16.2 <- ' auth =~ c5p8r + c5p15 + c5p1r + c5p14r
            LR =~ c5p2 '

atts16cfa2 <- cfa(atts16.2, data=bcs_cc_42, std.lv=TRUE)

summary(atts16cfa2, fit.measures=TRUE)

# 1 Factor model

atts16.1 <- ' values =~ c5p8r + c5p15 + c5p1r + c5p14r + c5p2 '

atts16cfa1 <- cfa(atts16.1, data=bcs_cc_42, std.lv=TRUE)

summary(atts16cfa1, fit.measures=TRUE)

#9.2.2 RACIAL PREJUDICE

psych::alpha(bcs_cc_42[c("c5p8r", "c5p15")], check.keys = T)

#create scale

bcs_cc_42$r16 <- scale(rowMeans(bcs_cc_42[c("c5p8r", "c5p15")]))
bcs_cc_42$r16 <- bcs_cc_42$r16[,1]

summary(bcs_cc_42$r16)

r16index <- icwIndex(as.matrix(bcs_cc_42[,c("c5p8r", "c5p15")]))

bcs_cc_42$rw16 <- as.double(r16index$index)

#9.2.3 AUTHORITARIANISM

psych::alpha(bcs_cc_42[c("c5p1r", "c5p14r")], check.keys = T)

#create scale

bcs_cc_42$a16 <- scale(rowMeans(bcs_cc_42[c("c5p1r", "c5p14r")]))
bcs_cc_42$a16 <- bcs_cc_42$a16[,1]

summary(bcs_cc_42$a16)

a16index <- icwIndex(as.matrix(bcs_cc_42[,c("c5p1r", "c5p14r")]))

bcs_cc_42$aw16 <- as.double(a16index$index)

summary(bcs_cc_42$aw16)
sd(bcs_cc_42$aw16)

#9.2.4 ECON LEFT-RIGHT
#create scale

bcs_cc_42$lr16 <- scale(bcs_cc_42$c5p2)
bcs_cc_42$lr16 <- bcs_cc_42$lr16[,1]

summary(bcs_cc_42$lr16)

#9.3 ATTITUDES AT 26
#9.3.1 CFA TO TEST MODEL FIT

# 2 Factor model

atts26.2 <- ' auth =~ b960120r + b960123r + b960428r + b960425r
            LR =~ b960430 '

atts26cfa2 <- cfa(atts26.2, data=bcs_cc_42, std.lv=TRUE)

summary(atts26cfa2, fit.measures=TRUE)

# 1 Factor model

atts26.1 <- ' values =~ b960120r + b960123r + b960428r + b960425r + b960430 '

atts26cfa1 <- cfa(atts26.1, data=bcs_cc_42, std.lv=TRUE)

summary(atts26cfa1, fit.measures=TRUE)

# Calculate composite reliability

reliability(atts26cfa2)

reliability(atts26cfa1)

#9.3.2 AUTHORITARIANISM

psych::alpha(bcs_cc_42[c("b960120r","b960123r","b960428r","b960425r")], check.keys = T)

#create scale

bcs_cc_42$a26 <- scale(rowMeans(bcs_cc_42[c("b960120r","b960123r","b960428r","b960425r")]))
bcs_cc_42$a26 <- bcs_cc_42$a26[,1]

summary(bcs_cc_42$a26)

a26index <- icwIndex(as.matrix(bcs_cc_42[,c("b960120r","b960123r","b960428r","b960425r")]))

a26index$weights

bcs_cc_42$aw26 <- as.double(a26index$index)

#9.3.3 ECON LEFT-RIGHT
#create scale

bcs_cc_42$lr26 <- scale(bcs_cc_42$b960430)
bcs_cc_42$lr26 <- bcs_cc_42$lr26[,1]

summary(bcs_cc_42$lr26)

#9.4 OUTCOME SCALES AT 30
#9.4.1 CFA TO TEST MODEL FIT
# 3 Factor model

atts30.3 <- ' prej =~ ar1 + ar2 + ar3 + ar4 + ar5r
            auth =~ a1r + a2r + a3r + a4r + a5r + a6r 
            LR =~ lr1 + lr2 + lr3 + lr5 + lr6 + lr7'

atts30cfa3 <- cfa(atts30.3, data=bcs_cc_42, std.lv=TRUE)

summary(atts30cfa3, fit.measures=TRUE)

atts30cfa3

semPaths(atts30cfa3, what = "std", style = "lisrel", layout = "circle", edge.color = "black",
         filetype = "tiff", filename = "atts30cfa3", height = 5, width = 7, trans = 1)

# 2 Factor model

atts30.2 <- ' authprej =~ ar1 + ar2 + ar3 + ar4 + ar5r + a1r + a2r + a3r + a4r + a5r + a6r
            LR =~ lr1 + lr2 + lr3 + lr5 + lr6 + lr7'

atts30cfa2 <- cfa(atts30.2, data=bcs_cc_42, std.lv=TRUE)

summary(atts30cfa2, fit.measures=TRUE)

semPaths(atts30cfa2, what = "std", style = "lisrel", layout = "circle", edge.color = "black",
         filetype = "tiff", filename = "atts30cfa2", height = 5, width = 7, trans = 1)

# 1 Factor model

atts30.1 <- ' values =~ ar1 + ar2 + ar3 + ar4 + ar5r + a1r + a2r + a3r + a4r + a5r + a6r + lr1 + lr2 + lr3 + lr5 + lr6 + lr7'

atts30cfa1 <- cfa(atts30.1, data=bcs_cc_42, std.lv=TRUE)

summary(atts30cfa1, fit.measures=TRUE)

semPaths(atts30cfa1, what = "std", style = "lisrel", layout = "circle", edge.color = "black",
         filetype = "tiff", filename = "atts30cfa1", height = 5, width = 7, trans = 1)

anova(atts30cfa3,atts30cfa1)

# Calculate composite reliability

reliability(atts30cfa3)

reliability(atts30cfa2)

reliability(atts30cfa1)

#9.4.2 RACIAL PREJUDICE

psych::alpha(bcs_cc_42[c("ar1","ar2","ar3","ar4","ar5r")], check.keys = T)

#create scale (rescaled to 0 with sd of 1)

bcs_cc_42$r30 <- scale(rowMeans(bcs_cc_42[c("ar1","ar2","ar3","ar4","ar5r")]))
bcs_cc_42$r30 <- bcs_cc_42$r30[,1]

summary(bcs_cc_42$r30)

r30index <- icwIndex(as.matrix(bcs_cc_42[c("ar1","ar2","ar3","ar4","ar5r")]))

r30index$weights

bcs_cc_42$rw30 <- as.double(r30index$index)

#9.4.3 AUTHORITARIANISM

psych::alpha(bcs_cc_42[c("a1r","a2r","a3r","a4r","a5r","a6r")], check.keys = T)

#create scale (rescaled to 0 with sd of 1)

bcs_cc_42$a30 <- scale(rowMeans(bcs_cc_42[c("a1r","a2r","a3r","a4r","a5r","a6r")]))
bcs_cc_42$a30 <- bcs_cc_42$a30[,1]

summary(bcs_cc_42$a30)

a30index <- icwIndex(as.matrix(bcs_cc_42[c("a1r","a2r","a3r","a4r","a5r","a6r")]))

a30index$weights

bcs_cc_42$aw30 <- as.double(a30index$index)

#9.4.4 ECON LEFT-RIGHT

psych::alpha(bcs_cc_42[c("lr1","lr2","lr3","lr4","lr5","lr6","lr7")], check.keys = T)

#improved alpha if lr4 is dropped

#create scale (rescaled to 0 with sd of 1)

bcs_cc_42$lr30 <- scale(rowMeans(bcs_cc_42[c("lr1","lr2","lr3","lr5","lr6","lr7")]))
bcs_cc_42$lr30 <- bcs_cc_42$lr30[,1]

summary(bcs_cc_42$lr30)

lr30index <- icwIndex(as.matrix(bcs_cc_42[c("lr1","lr2","lr3","lr5","lr6","lr7")]))

lr30index$weights

bcs_cc_42$lrw30 <- as.double(lr30index$index)

#9.5 OUTCOME SCALES AT 42
#9.5.1 CFA TO TEST MODEL FIT
# 3 Factor model

atts42.3 <- ' prej =~ B9SCQ3B + B9SCQ20A
            auth =~ B9SCQ3Dr + B9SCQ20Cr + B9SCQ43Ar 
            LR =~ B9SCQ3A + B9SCQ43F + B9SCQ43H + B9SCQ43I'

atts42cfa3 <- cfa(atts42.3, data=bcs_cc_42, std.lv=TRUE)

summary(atts42cfa3, fit.measures=TRUE)

atts42cfa3

semPaths(atts42cfa3, what = "std", style = "lisrel", layout = "circle", edge.color = "black",
         filetype = "tiff", filename = "atts42cfa3", height = 5, width = 7, trans = 1)

# 2 Factor model

atts42.2 <- ' authprej =~ B9SCQ3B + B9SCQ20A + B9SCQ3Dr + B9SCQ20Cr + B9SCQ43Ar 
            LR =~ B9SCQ3A + B9SCQ43F + B9SCQ43H + B9SCQ43I'

atts42cfa2 <- cfa(atts42.2, data=bcs_cc_42, std.lv=TRUE)

summary(atts42cfa2, fit.measures=TRUE)

semPaths(atts42cfa2, what = "std", style = "lisrel", layout = "circle", edge.color = "black",
         filetype = "tiff", filename = "atts42cfa2", height = 5, width = 7, trans = 1)

# 1 Factor model

atts42.1 <- ' values =~ B9SCQ3B + B9SCQ20A + B9SCQ3Dr + B9SCQ20Cr + B9SCQ43Ar + B9SCQ3A + B9SCQ43F + B9SCQ43H + B9SCQ43I'

atts42cfa1 <- cfa(atts42.1, data=bcs_cc_42, std.lv=TRUE)

summary(atts42cfa1, fit.measures=TRUE)

semPaths(atts42cfa1, what = "std", style = "lisrel", layout = "circle", edge.color = "black",
         filetype = "tiff", filename = "atts42cfa1", height = 5, width = 7, trans = 1)

anova(atts42cfa3,atts42cfa1)

# Calculate composite reliability

reliability(atts42cfa3)

reliability(atts42cfa2)

reliability(atts42cfa1)

#9.5.2 RACIAL PREJUDICE
#psych::alpha of 0.68

psych::alpha(bcs_cc_42[c("B9SCQ3B","B9SCQ20A")],check.keys = T)

#create scale (rescaled to 0 with sd of 1)

bcs_cc_42$r42 <- scale(rowMeans(bcs_cc_42[c("B9SCQ3B","B9SCQ20A")]))
bcs_cc_42$r42 <- bcs_cc_42$r42[,1]

summary(bcs_cc_42$r42)

r42index <- icwIndex(as.matrix(bcs_cc_42[c("B9SCQ3B","B9SCQ20A")]))

r42index$weights

bcs_cc_42$rw42 <- as.double(r42index$index)

#9.5.3 AUTHORITARIANISM
#alpha of .28

psych::alpha(bcs_cc_42[c("B9SCQ3Dr","B9SCQ20Cr","B9SCQ43Ar")], check.keys = T)

#split sample men and women - marginally better for men

bcs_cc_42 %>%
  filter(female==0) %>%
  select(B9SCQ3Dr,B9SCQ20Cr,B9SCQ43Ar) %>%
  psych::alpha()

bcs_cc_42 %>%
  filter(female==1) %>%
  select(B9SCQ3Dr,B9SCQ20Cr,B9SCQ43Ar) %>%
  psych::alpha()

#create scale (rescaled to 0 with sd of 1)

bcs_cc_42$a42 <- scale(rowMeans(bcs_cc_42[c("B9SCQ3Dr","B9SCQ20Cr","B9SCQ43Ar")]))
bcs_cc_42$a42 <- bcs_cc_42$a42[,1]

summary(bcs_cc_42$a42)

a42index <- icwIndex(as.matrix(bcs_cc_42[complete.cases(bcs_cc_42),c("B9SCQ3Dr","B9SCQ20Cr","B9SCQ43Ar")]))

bcs_cc_42$aw42 <- as.double(a42index$index)

#9.5.4 ECON LEFT-RIGHT
#alpha of 0.7 with NHS item dropped

psych::alpha(bcs_cc_42[c("B9SCQ3A","B9SCQ43F","B9SCQ43H","B9SCQ43I")],
             check.keys = T)

#create scale (rescaled to 0 with sd of 1)

bcs_cc_42$lr42 <- scale(rowMeans(bcs_cc_42[c("B9SCQ3A","B9SCQ43F","B9SCQ43H","B9SCQ43I")]))
bcs_cc_42$lr42 <- bcs_cc_42$lr42[,1]

summary(bcs_cc_42$lr42)

lr42index <- icwIndex(as.matrix(bcs_cc_42[c("B9SCQ3A","B9SCQ43F","B9SCQ43H","B9SCQ43I")]))

lr42index$weights

bcs_cc_42$lrw42 <- as.double(lr42index$index)

#specify and scale death pen items

#16

bcs_cc_42$deathpen16 <- scale(bcs_cc_42$c5p14r)
bcs_cc_42$deathpen16 <- bcs_cc_42$deathpen16[,1]

#26

bcs_cc_42$deathpen26 <- scale(bcs_cc_42["b960123r"])
bcs_cc_42$deathpen26 <- bcs_cc_42$deathpen26[,1]

#30

bcs_cc_42$deathpen30 <- scale(bcs_cc_42["a2r"])
bcs_cc_42$deathpen30 <- bcs_cc_42$deathpen30[,1]

#42

bcs_cc_42$deathpen42 <- scale(bcs_cc_42["B9SCQ3Dr"])
bcs_cc_42$deathpen42 <- bcs_cc_42$deathpen42[,1]

#scales across time

#Authoritarianism

psych::alpha(bcs_cc_42[c("c5p1r", "c5p14r","b960120r","b960123r","b960428r","b960425r",
                   "a1r","a2r","a3r","a4r","a5r","a6r","B9SCQ3Dr","B9SCQ20Cr","B9SCQ43Ar")],
             check.keys = T)

psych::omega(bcs_cc_42[c("c5p1r", "c5p14r","b960120r","b960123r","b960428r","b960425r",
                          "a1r","a2r","a3r","a4r","a5r","a6r","B9SCQ3Dr","B9SCQ20Cr","B9SCQ43Ar")],
             check.keys = T)

#Racial prejudice

psych::alpha(bcs_cc_42[c("c5p8r","c5p15","ar1","ar2","ar3","ar4","ar5r","B9SCQ3B","B9SCQ20A")],
             check.keys = T)

psych::omega(bcs_cc_42[c("c5p8r","c5p15","ar1","ar2","ar3","ar4","ar5r","B9SCQ3B","B9SCQ20A")],
             check.keys = T)

#Economic left-right

psych::alpha(bcs_cc_42[c("c5p2","b960430","lr1","lr2","lr3","lr5","lr6","lr7",
                          "B9SCQ3A","B9SCQ43F","B9SCQ43H","B9SCQ43I")],
             check.keys = T)

psych::omega(bcs_cc_42[c("c5p2","c5p3r","b960430","lr1","lr2","lr3","lr5","lr6","lr7",
                          "B9SCQ3A","B9SCQ43F","B9SCQ43H","B9SCQ43I")],
             check.keys = T)

#Drop composite scale items

val_comps <- c("d070r","d079r","d080r","d081r","d087r","d090","d094r","d097r",
               "d099r","d100r","d103r","d106r","d107r","c5p8r","c5p15","c5p1r","c5p3r",
               "c5p14r","c5p2","b960120r","b960123r","b960428r","b960425r","b960430",
               "ar1","ar2","ar3","ar4","ar5r","a1r","a2r","a3r","a4r","a5r","a6r",
               "lr1","lr2","lr3","lr4","lr5","lr6","lr7","B9SCQ3B","B9SCQ20A",
               "B9SCQ3Dr","B9SCQ20Cr","B9SCQ43Ar","B9SCQ3A","B9SCQ43F","B9SCQ43H",
               "B9SCQ43I","B9SCQ3G")

bcs_cc_42 <- bcs_cc_42 %>% 
 select(!val_comps)

#9.6 save file

write_csv(bcs_cc_42, "bcs_cc_42.csv")

write_rds(bcs_cc_42, "bcs_cc_42.rds")

#####
#4. CREATE TREATMENTS, SCALES AND EBAL WEIGHTS IN IMPUTED DATA

load(file = "bcs_42_imp.rda")

long <- mice::complete(bcs_42_imp, "long", include = T)

#Treatments

long$deg26 <- case_when(long$isced26 == "7+" ~ 1,
                        long$isced26 == "6" ~ 1,
                        !is.na(long$isced26) ~ 0)

table(long$deg26)

table(is.na(long$deg26))

long$deg30 <- case_when(long$isced30 == "7+" ~ 1,
                        long$isced30 == "6" ~ 1,
                        long$isced26 == "7+" ~ 1,
                        long$isced26 == "6" ~ 1,
                        !is.na(long$isced26)|!is.na(long$isced30) ~ 0)

table(long$deg30)

long$deg42 <- case_when(long$isced42 == "7+" ~ 1,
                        long$isced42 == "6" ~ 1,
                        long$isced30 == "7+" ~ 1,
                        long$isced30 == "6" ~ 1,
                        long$isced26 == "7+" ~ 1,
                        long$isced26 == "6" ~ 1,
                        !is.na(long$isced26)|!is.na(long$isced30)|!is.na(long$isced42) ~ 0)

table(long$deg42)

#Mauth

long$Mauth <- scale(rowMeans(long[c("d070r","d079r","d080r","d081r","d087r",
                                    "d090","d094r","d097r","d099r",
                                    "d100r","d103r","d106r","d107r")]))
long$Mauth <- long$Mauth[,1]

#auth

#16

long$a16 <- scale(rowMeans(long[c("c5p1r", "c5p14r")]))
long$a16 <- long$a16[,1]

#26

long$a26 <- scale(rowMeans(long[c("b960120r","b960123r","b960428r","b960425r")]))
long$a26 <- long$a26[,1]

#30

long$a30 <- scale(rowMeans(long[c("a1r","a2r","a3r","a4r","a5r","a6r")]))
long$a30 <- long$a30[,1]

#42

long$a42 <- scale(rowMeans(long[c("B9SCQ3Dr","B9SCQ20Cr","B9SCQ43Ar")]))
long$a42 <- long$a42[,1]

#death pen

#16

long$deathpen16 <- scale(long["c5p14r"])
long$deathpen16 <- long$deathpen16[,1]

#26

long$deathpen26 <- scale(long["b960123r"])
long$deathpen26 <- long$deathpen26[,1]

#30

long$deathpen30 <- scale(long["a2r"])
long$deathpen30 <- long$deathpen30[,1]

#42

long$deathpen42 <- scale(long["B9SCQ3Dr"])
long$deathpen42 <- long$deathpen42[,1]

#LR

#16

long$lr16 <- scale(long$c5p2)
long$lr16 <- long$lr16[,1]

#26

long$lr26 <- scale(long$b960430)
long$lr26 <- long$lr26[,1]

#30

long$lr30 <- scale(rowMeans(long[c("lr1","lr2","lr3","lr5","lr6","lr7")]))
long$lr30 <- long$lr30[,1]

#42

long$lr42 <- scale(rowMeans(long[c("B9SCQ3A","B9SCQ43F","B9SCQ43H","B9SCQ43I")]))
long$lr42 <- long$lr42[,1]

#Racial prej

#16

long$r16 <- scale(rowMeans(long[c("c5p8r", "c5p15")]))
long$r16 <- long$r16[,1]

#30

long$r30 <- scale(rowMeans(long[c("ar1","ar2","ar3","ar4","ar5r")]))
long$r30 <- long$r30[,1]

#42

long$r42 <- scale(rowMeans(long[c("B9SCQ3B","B9SCQ20A")]))
long$r42 <- long$r42[,1]

#Drop composite scale items

val_comps <- c("d070r","d079r","d080r","d081r","d087r","d090","d094r","d097r",
               "d099r","d100r","d103r","d106r","d107r","c5p8r","c5p15","c5p1r",
               "c5p14r","c5p2","b960120r","b960123r","b960428r","b960425r","b960430",
               "ar1","ar2","ar3","ar4","ar5r","a1r","a2r","a3r","a4r","a5r","a6r",
               "lr1","lr2","lr3","lr4","lr5","lr6","lr7","B9SCQ3B","B9SCQ20A",
               "B9SCQ3Dr","B9SCQ20Cr","B9SCQ43Ar","B9SCQ3A","B9SCQ43F","B9SCQ43H",
               "B9SCQ43I","B9SCQ3G")

long <- long %>% 
  select(-val_comps)

#Drop auxiliary variables

mi_vars <- c("m_age","medu_age","fedu_age","crowd","chi_ten","lowbwt","q16score","eng16",
             "maths16","cog16")

long <- select(long, !mi_vars)

#construct weights

miw <- as.mids(long)

miw <- weightthem(deg42 ~ female + nonbrit30 + Mauth + parscr + pared + eng10 + maths10 + a16 + r16 + lr16,
                  datasets = miw, estimand = "ATT", method = "ebal", stabilize = T)

get.w(miw)

long$weights <- c(rep(NA,nrow(long[long$`.imp`==0,])),get.w(miw))

#reshape as long dataset

bcs_mi_42_l <- long %>% 
  pivot_longer(
    cols = c("deg26","deg30","deg42","a16","a26","a30","a42","deathpen16",
             "deathpen26","deathpen30","deathpen42","lr16","lr26","lr30",
             "lr42","r16","r30","r42"),
    names_to = c(".value","age"), 
    names_pattern = "([[:alpha:]]+)([[:digit:]]+)")

#9.6 save file

write_rds(bcs_mi_42_l, "bcs_mi_42_l.rds")

bcs_mi_42_l <- read_rds("bcs_mi_42_l.rds")

#####
#5. DESCRIPTIVES

#response rate over time

bcs_resp <- read_tsv("C:/Users/ralph/Desktop/Working data/BCS/Response/bcs_response.tab")

bcs_resp

wave_miss <- as_tibble(t(bcs_resp %>%
                          summarise(across(c(BCSID,OUTCME01,OUTCME02,OUTCME03,OUTCME04,OUTCME05,OUTCME06,OUTCME09), 
                                           ~sum(.==1)))))

wave_miss[1,] <- nrow(bcs_full)

colnames(wave_miss) <- "Valid observations"

wave_miss$Wave <- factor(c("Total\npopulation","0","5","10","16","26","30","42"),
                       levels = c("Total\npopulation","0","5","10","16","26","30","42"))

wave_miss

ggplot(data=wave_miss, aes(x = Wave, y = `Valid observations`)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme_bw()

ggsave("wave_miss.png",type="cairo")

#read in complete case file

bcs_cc_42 <- read_rds("bcs_cc_42.rds")

bcs_cc_42

#basic descriptives for appendix

desc_num1 <- bcs_cc_42 %>%
  select(eng10,maths10,Mauth,a16,a26,a30,a42,lr16,lr26,lr30,lr42,r16,r30,r42) %>%
  gather("var") %>%
  group_by(var) %>%             
  mutate(median = median(value),
         max = max(value),
         min = min(value)) %>%
  select(-value) %>%
  unique() %>%
  ungroup()

#proportions for count variables

desc_disc1 <- bcs_cc_42 %>%
  select(female,nonbrit30,parscr,pared,deg26,deg30,deg42) %>%
  gather("var", "value") %>%
  count(var, value) %>%
  group_by(var) %>%             
  mutate(prop = prop.table(n)) %>%
  ungroup()

#produce same for multiply imputed data
#first unimputed

desc_num2 <- long %>%
  filter(`.imp`=="0") %>%
  select(eng10,maths10,Mauth,a16,a26,a30,a42,lr16,lr26,lr30,lr42,r16,r30,r42) %>%
  gather("var") %>%
  group_by(var) %>%             
  mutate(median = median(value, na.rm = T),
         max = max(value, na.rm = T),
         min = min(value, na.rm = T),
         propna = (sum(is.na(value))/n())*100) %>%
  select(-value) %>%
  unique() %>%
  ungroup()

desc_disc2 <- long %>%
  filter(`.imp`=="0") %>%
  select(female,nonbrit30,parscr,pared,deg26,deg30,deg42) %>%
  gather("var", "value") %>%
  count(var, value) %>%
  group_by(var) %>%             
  mutate(prop = prop.table(n)) %>%
  ungroup()

#second imputed

desc_num3 <- long %>%
  filter(`.imp`!="0") %>%
  select(eng10,maths10,Mauth,a16,a26,a30,a42,lr16,lr26,lr30,lr42,r16,r30,r42) %>%
  gather("var") %>%
  group_by(var) %>%             
  mutate(median = median(value),
         max = max(value),
         min = min(value)) %>%
  select(-value) %>%
  unique() %>%
  ungroup()

desc_disc3 <- long %>%
  filter(`.imp`!="0") %>%
  select(female,nonbrit30,parscr,pared,deg26,deg30,deg42) %>%
  gather("var", "value") %>%
  count(var, value) %>%
  group_by(var) %>%             
  mutate(prop = prop.table(n)) %>%
  ungroup()

#combine

#numeric

desc_nums <- cbind(desc_num1,desc_num2[-1],desc_num3[-1])

desc_nums <- cbind(desc_nums[1],round(desc_nums[-1], 2))

write.table(desc_nums, file = "desc_nums.txt", sep = ",", quote = FALSE, row.names = F)

#discrete

#first reformat

desc_disc2 <- desc_disc2 %>% 
  filter(!is.na(value)) %>%
  group_by(var) %>%             
  mutate(prop = prop.table(n)) %>%
  ungroup() %>%
  left_join(filter(desc_disc2,is.na(value))[c("var","prop")], by = "var")

#then bind

desc_discs <- cbind(desc_disc1[-3],desc_disc2[4:5],desc_disc3[4])

desc_discs <- cbind(desc_discs[1:2],(round(desc_discs[3:6], 3)*100))

write.table(desc_discs, file = "desc_discs.txt", sep = ",", quote = FALSE, row.names = F)

#outcomes by highest level of qualification

refdeg_labels <- c("None","CSEs","O-levels","A-levels","HE Dip","Degree","PG Degree")

table(bcs_cc_42$hqual26)

bcs_cc_42$refdeg26 <- relevel(factor(bcs_cc_42$hqual26+bcs_cc_42$pg26,
                                      labels = refdeg_labels), ref = "Degree")

bcs_cc_42$refdeg30 <- relevel(factor(bcs_cc_42$hqual30+bcs_cc_42$pg30,
                                      labels = refdeg_labels), ref = "Degree")

bcs_cc_42$refdeg42 <- relevel(factor(bcs_cc_42$hqual42+bcs_cc_42$pg42,
                                      labels = refdeg_labels), ref = "Degree")

#stacked bar chart of qualifications by age

isced <- tibble(c(prop.table(table(bcs_cc_42$isced26)),
                  prop.table(table(bcs_cc_42$isced30)),
                  prop.table(table(bcs_cc_42$isced42))))

isced <- rename(isced,Proportion=`c(...)`)

isced$ISCED <- rep(rownames(prop.table(table(bcs_cc_42$isced26))),3)

isced$Age <- c(rep("26",6),rep("30",6),rep("42",6))

ggplot(isced, aes(fill=ISCED, y=Proportion, x=Age)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_grey() + theme_bw()

ggsave("ISCED_bw.png",type="cairo")

#simple model plots

bcs_cc_42$refisced30 <- relevel(factor(bcs_cc_42$isced30), ref = "6")

r30quals <- lm(r30 ~ refisced30, data = bcs_cc_42)
a30quals <- lm(a30 ~ refisced30, data = bcs_cc_42)
lr30quals <- lm(lr30 ~ refisced30, data = bcs_cc_42)

plot_summs(r30quals, a30quals, lr30quals, scale = FALSE, legend.title = "Outcome", colors = "Greys",
           coefs = c("2"="refisced302","3"="refisced303","4"="refisced304",
                     "5"="refisced305","7+"="refisced307+"),
           model.names = c("Racial prejudice", "Authoritarianism", "Economic\nLeft-Right")) + 
  theme_bw() + labs(y = "ISCED")

ggsave("ISCED_outcomes.png",type="cairo")

#trajectory plots
#generate means and SDs by age

traj <- bcs_cc_42 %>%
  dplyr::select(a,r,lr,deg,age) %>%
  group_by(age,deg) %>%
  summarise(n = n(),
            a_mean = mean(a),
            a_sd = sd(a),
            r_mean = mean(r),
            r_sd = sd(r),
            lr_mean = mean(lr),
            lr_sd = sd(lr)) %>% 
  ungroup() %>%
  mutate(deg = as.factor(deg),
         a_se = a_sd / sqrt(n),
         a_lci = a_mean - qt(1 - (0.05 / 2), n - 1) * a_se,
         a_uci = a_mean + qt(1 - (0.05 / 2), n - 1) * a_se,
         r_se = r_sd / sqrt(n),
         r_lci = r_mean - qt(1 - (0.05 / 2), n - 1) * r_se,
         r_uci = r_mean + qt(1 - (0.05 / 2), n - 1) * r_se,
         lr_se = lr_sd / sqrt(n),
         lr_lci = lr_mean - qt(1 - (0.05 / 2), n - 1) * lr_se,
         lr_uci = lr_mean + qt(1 - (0.05 / 2), n - 1) * lr_se) %>%
  pivot_longer(
    cols = c(a_mean:lr_uci),
    names_to = c("value",".value"), 
    names_sep = "_",
    values_drop_na = T) %>%
  mutate(value = dplyr::recode(value, a="Authoritarianism",lr="Left-Right",r="Racial prejudice"))

traj_adds <- traj %>%
  dplyr::filter(age==16) %>%
  mutate(deg = 1)

traj <- rbind(traj,traj_adds) %>% arrange(age,deg,value)

pre_traj_plot <- traj %>%
  ggplot(aes(x = age, y = mean, ymin = lci, ymax = uci, group = factor(deg))) +
  geom_point(aes(shape=factor(deg)),size = 2) + geom_line(aes(linetype=factor(deg))) +
  geom_errorbar(width=.1) + facet_wrap(facets = vars(value)) +
  labs(x = "Age",
       y = "SD",
       group = "Graduate",
       shape = "Graduate",
       linetype = "Graduate")

pre_traj_plot + theme_bw()

ggsave("pre_traj_plot.png",type="cairo",width = 18,height = 8, units = "cm")

#####
#6. MATCH UNTREATED TO EVERTREATED

bcs_cc_42 <- read_rds("bcs_cc_42.rds")

#relevel parental social class

bcs_cc_42$parsc <- factor(bcs_cc_42$parscr, levels = c("1","2","3","4","5","6"))

#construct individual wave weights instead

weverdeg <- weightit(deg42 ~ female + nonbrit30 + Mauth + parsc + pared + eng10 + maths10 + a16 + r16 + lr16,
                data = bcs_cc_42, estimand = "ATT", method = "ebal", stabilize = T)

summary(weverdeg)

#output coef of var and ESS statistics

weverdeg.sum <- summary(weverdeg)

weverdeg.sum$weight.range

weverdeg.sum$coef.of.var

weverdeg.sum$effective.sample.size

#Check balance

degtab <- bal.tab(weverdeg, m.threshold = .05)

varnames <- var.names(degtab, type = "df")

varnames

varnames$new <- c("Female","Non-British","Mother's authoritarianism",
                  "Unskilled","Partly skilled","Skilled manual",
                  "Skilled non-manual","Managerial-technical","Professional",
                  "No qualifications","Below degree-level","University degree or higher",
                  "Reading age at 10","Maths ability at 10","Authoritarian attitudes at 16",
                  "Racial prejudice at 16","Left-right attitudes at 16")

varnames

love.plot(bal.tab(weverdeg, m.threshold = .05),var.names = varnames,
          sample.names = c("Unweighted","Weighted"),stars = "std")

ggsave("everdeg_ebal.png",type="cairo")

#extract individual weights

bcs_cc_42$weights <- weverdeg$weights

#save as file to be converted to long

write_csv(bcs_cc_42, "bcs_cc_42.csv")

write_rds(bcs_cc_42, "bcs_cc_42.rds")

#####
#7. CONSTRUCTING LMER MODELs

bcs_cc_42 <- read_rds("bcs_cc_42.rds")

#make into long dataset

bcs_cc_42 <- bcs_cc_42 %>% 
  pivot_longer(
  cols = c("deg26","isced26","deg30","isced30","deg42","isced42",
           r16:deathpen42),
  names_to = c(".value","age"), 
  names_pattern = "([[:alpha:]]+)([[:digit:]]+)")

bcs_cc_42

#code degree at 16 as 0

bcs_cc_42$deg[bcs_cc_42$age==16] <- 0

#demean time-varying variable, from package "parameters"

bcs_cc_42 <- cbind(bcs_cc_42, demean(bcs_cc_42,
                                     select = "deg",
                                     group = "bcsid"))

#REWB model, distinguishing within and between effects

model_rewb_a <- lmer(a ~ deg_within + deg_between + age + female + Mauth + 
                       parscr + pared + eng10 + maths10 + nonbrit30 +
                       (1 + deg_within | bcsid),
                   data = bcs_cc_42)

summary(model_rewb_a)

model_rewb_lr <- lmer(lr ~ deg_within + deg_between + age + female + Mauth +
                       parscr + pared + eng10 + maths10 + nonbrit30 +
                       (1 + deg_within | bcsid),
                     data = bcs_cc_42)

summary(model_rewb_lr)

model_rewb_r <- lmer(r ~ deg_within + deg_between + age + female + Mauth +
                       parscr + pared + eng10 + maths10 + nonbrit30 +
                       (1 | bcsid),
                     data = bcs_cc_42)

summary(model_rewb_r)

#produce summary tables for each outcome

tab_model(model_rewb_r,model_rewb_a,model_rewb_lr,
          show.ci = FALSE, 
          show.se = TRUE, 
          auto.label = FALSE, 
          string.se = "SE",
          p.style="stars",
          file="rewb_cc.html",
          dv.labels = c("Racial prejudice", "Authoritarianism", "Left-Right"))

#####
#8. MODEL DIAGNOSTICS

#https://cran.r-project.org/web/packages/multilevelTools/vignettes/lmer-vignette.html
#https://cran.r-project.org/web/packages/JWileymisc/vignettes/diagnostics-vignette.html

#model diagnostics

a_md <- modelDiagnostics(model_rewb_a, ev.perc = .01)

plot(a_md, ask = FALSE, ncol = 2, nrow = 3)

lr_md <- modelDiagnostics(model_rewb_lr, ev.perc = .01)

plot(lr_md, ask = FALSE, ncol = 2, nrow = 3)

r_md <- modelDiagnostics(model_rewb_r, ev.perc = .01)

plot(r_md, ask = FALSE, ncol = 2, nrow = 2)

#univariate diagnostics

tmp <- meanDecompose(r ~ bcsid, data = bcs_cc_42)

plot(testDistribution(tmp[["r by bcsid"]]$X,
                      extremevalues = "theoretical", ev.perc = .001),
     varlab = "Between Person Racial Prejudice")

plot(testDistribution(tmp[["r by residual"]]$X,
                      extremevalues = "theoretical", ev.perc = .001),
     varlab = "Within Person Racial Prejudice")

tmp <- meanDecompose(a ~ bcsid, data = bcs_cc_42)

plot(testDistribution(tmp[["a by bcsid"]]$X,
                      extremevalues = "theoretical", ev.perc = .001),
     varlab = "Between Person Authoritarianism")

plot(testDistribution(tmp[["a by residual"]]$X,
                      extremevalues = "theoretical", ev.perc = .001),
     varlab = "Within Person Authoritarianism")

tmp <- meanDecompose(lr ~ bcsid, data = bcs_cc_42)

plot(testDistribution(tmp[["lr by bcsid"]]$X,
                      extremevalues = "theoretical", ev.perc = .001),
     varlab = "Between Person Left-Right")

plot(testDistribution(tmp[["lr by residual"]]$X,
                      extremevalues = "theoretical", ev.perc = .001),
     varlab = "Within Person Left-Right")

#testing removal of extreme values on racial prejudice

mvextreme <- subset(r_md$extremeValues,
                    EffectType == "Random Effect bcsid : (Intercept)")

head(mvextreme)

unique(mvextreme$bcsid)

model_rewb_r2 <- update(model_rewb_r, data = subset(bcs_cc_42,
                                                    bcsid %!in% unique(mvextreme$bcsid)))

md2 <- modelDiagnostics(model_rewb_r2, ev.perc = .01)

plot(md2, ask = FALSE, ncol = 2, nrow = 2)

mvextreme2 <- subset(md2$extremeValues,
                     EffectType == "Multivariate Random Effect bcsid")

head(mvextreme2)

unique(mvextreme2$bcsid)

model_rewb_r3 <- update(model_rewb_r, data = subset(bcs_cc_42,
                                                    bcsid %!in% c(unique(mvextreme$bcsid),unique(mvextreme2$bcsid))))

md3 <- modelDiagnostics(model_rewb_r3, ev.perc = .001)

plot(md3, ask = FALSE, ncol = 2, nrow = 3)

mvextreme3 <- subset(md3$extremeValues,
                     EffectType == "Multivariate Random Effect bcsid")

head(mvextreme3)

unique(mvextreme3$bcsid)

model_rewb_r4 <- update(model_rewb_r, data = subset(bcs_cc_42,
                                                    bcsid %!in% c(unique(mvextreme$bcsid),unique(mvextreme2$bcsid),unique(mvextreme3$bcsid))))

md4 <- modelDiagnostics(model_rewb_r4, ev.perc = .001)

plot(md4, ask = FALSE, ncol = 2, nrow = 3)

mvextreme4 <- subset(md3$extremeValues,
                     EffectType == "Multivariate Random Effect bcsid")

head(mvextreme4)

unique(mvextreme3$bcsid)

#####
#9. ROBUSTNESS CHECKS
#9.1 TWFE models

bcs_cc_42 <- read_rds("bcs_cc_42.rds")

#make into long dataset

bcs_cc_42 <- bcs_cc_42 %>% 
  pivot_longer(
    cols = c("deg26","isced26","deg30","isced30","deg42","isced42",
             r16:deathpen42),
    names_to = c(".value","age"), 
    names_pattern = "([[:alpha:]]+)([[:digit:]]+)")

bcs_cc_42

#code degree at 16 as 0

bcs_cc_42$deg[bcs_cc_42$age==16] <- 0

bcs_cc_42$deg <- factor(bcs_cc_42$deg)

#Complete case models

model_fe_a <- felm(a ~ age + deg | bcsid, data = bcs_cc_42)

summary(model_fe_a)

model_fe_lr <- felm(lr ~ age + deg | bcsid, data = bcs_cc_42)

summary(model_fe_lr)

model_fe_r <- felm(r ~ age + deg | bcsid, data = bcs_cc_42)

summary(model_fe_r)

#produce summary tables for each outcome

tab_model(model_fe_r,model_fe_a,model_fe_lr,
          show.ci = FALSE, 
          show.se = TRUE, 
          auto.label = FALSE, 
          string.se = "SE",
          p.style="stars",
          file="fe_cc.html",
          dv.labels = c("Racial prejudice", "Authoritarianism", "Left-Right"))

#9.2 using death penalty item and inverse covariance weighted scales

#demean time-varying variable, from package "parameters"

bcs_cc_42 <- cbind(bcs_cc_42, demean(bcs_cc_42,
                                     select = "deg",
                                     group = "bcsid"))

#construct models

model_rewb_dp <- lmer(deathpen ~ deg_within + deg_between + age + female + Mauth +
                        parscr + pared + eng10 + maths10 + nonbrit30 +
                        (1 + deg_within | bcsid),
                      data = bcs_cc_42)

summary(model_rewb_dp)

model_rewb_aw <- lmer(aw ~ deg_within + deg_between + age + female + Mauth +
                        parscr + pared + eng10 + maths10 + nonbrit30 +
                        (1 + deg_within | bcsid),
                      data = bcs_cc_42)

summary(model_rewb_aw)

model_rewb_rw <- lmer(rw ~ deg_within + deg_between + age + female + Mauth +
                        parscr + pared + eng10 + maths10 + nonbrit30 +
                        (1 | bcsid),
                      data = bcs_cc_42)

summary(model_rewb_rw)

#produce summary table of results

tab_model(model_rewb_dp,model_rewb_aw,model_rewb_rw,
          show.ci = FALSE, 
          show.se = TRUE, 
          auto.label = FALSE, 
          string.se = "SE",
          p.style="stars",
          file="rewb_cc_robust.html",
          dv.labels = c("Death penalty", "Authoritarianism (ICW)", "Racial prejudice (ICW)"))

#9.3 Entropy balanced models

eb_rewb_a <- lmer(a ~ deg_within + deg_between + age + female + Mauth +
                    parscr + pared + eng10 + maths10 + nonbrit30 +
                    (1 + deg_within | bcsid),
                  data = bcs_cc_42, weights = weights)

summary(eb_rewb_a)

eb_rewb_lr <- lmer(lr ~ deg_within + deg_between + age + female + Mauth +
                     parscr + pared + eng10 + maths10 + nonbrit30 +
                     (1 + deg_within | bcsid),
                   data = bcs_cc_42, weights = weights)

summary(eb_rewb_lr)

eb_rewb_r <- lmer(r ~ deg_within + deg_between + age + female + Mauth +
                    parscr + pared + eng10 + maths10 + nonbrit30 +
                    (1 + deg_within | bcsid),
                  data = bcs_cc_42, weights = weights)

summary(eb_rewb_r)

#produce summary tables for each outcome

tab_model(eb_rewb_r,eb_rewb_a,eb_rewb_lr,
          show.ci = FALSE, 
          show.se = TRUE, 
          auto.label = FALSE, 
          string.se = "SE",
          p.style="stars",
          file="rewb_ebal.html",
          dv.labels = c("Racial prejudice (EB)", "Authoritarianism (EB)", "Left-Right (EB)"))

#9.4 alternative treatment

bcs_cc_42 <- read_rds("bcs_cc_42.rds")

#code treatment variables

bcs_cc_42$altdeg26 <- case_when(bcs_cc_42$isced26 == "7+" ~ 1,
                                bcs_cc_42$isced26 == "6" ~ 1,
                                bcs_cc_42$isced26 == "5" ~ 0,
                                bcs_cc_42$isced26 == "4" ~ 0,
                                bcs_cc_42$isced26 == "3" ~ 0)

table(bcs_cc_42$altdeg26)

table(bcs_cc_42$deg26)

table(is.na(bcs_cc_42$altdeg26))

bcs_cc_42$altdeg30 <- case_when(bcs_cc_42$isced30 == "7+" ~ 1,
                                bcs_cc_42$isced30 == "6" ~ 1,
                                bcs_cc_42$isced26 == "7+" ~ 1,
                                bcs_cc_42$isced26 == "6" ~ 1,
                                bcs_cc_42$isced30 == "5" ~ 0,
                                bcs_cc_42$isced30 == "4" ~ 0,
                                bcs_cc_42$isced30 == "3" ~ 0,
                                bcs_cc_42$isced26 == "5" ~ 0,
                                bcs_cc_42$isced26 == "4" ~ 0,
                                bcs_cc_42$isced26 == "3" ~ 0)

table(bcs_cc_42$altdeg30)

table(bcs_cc_42$deg30)

bcs_cc_42$altdeg42 <- case_when(bcs_cc_42$isced42 == "7+" ~ 1,
                                bcs_cc_42$isced42 == "6" ~ 1,
                                bcs_cc_42$isced30 == "7+" ~ 1,
                                bcs_cc_42$isced30 == "6" ~ 1,
                                bcs_cc_42$isced26 == "7+" ~ 1,
                                bcs_cc_42$isced26 == "6" ~ 1,
                                bcs_cc_42$isced42 == "5" ~ 0,
                                bcs_cc_42$isced42 == "4" ~ 0,
                                bcs_cc_42$isced42 == "3" ~ 0,
                                bcs_cc_42$isced30 == "5" ~ 0,
                                bcs_cc_42$isced30 == "4" ~ 0,
                                bcs_cc_42$isced30 == "3" ~ 0,
                                bcs_cc_42$isced26 == "5" ~ 0,
                                bcs_cc_42$isced26 == "4" ~ 0,
                                bcs_cc_42$isced26 == "3" ~ 0)

table(bcs_cc_42$altdeg42)

table(bcs_cc_42$deg42)

#remove rows without altdeg at 26

bcs_cc_42 <- bcs_cc_42[!is.na(bcs_cc_42$altdeg26),]

#make into long dataset

names(bcs_cc_42)

bcs_cc_42 <- bcs_cc_42 %>% 
  pivot_longer(
    cols = c("deg26","isced26","deg30","isced30","deg42","isced42",
             r16:altdeg42),
    names_to = c(".value","age"), 
    names_pattern = "([[:alpha:]]+)([[:digit:]]+)")

View(bcs_cc_42)

#remove rows without age 

bcs_cc_42 <- bcs_cc_42[!is.na(bcs_cc_42$age),]

bcs_cc_42 %>%
  select(bcsid,age,deg,isced,altdeg) %>%
  print(n=100)

#code degree at 16 as 0

bcs_cc_42$altdeg[bcs_cc_42$age==16] <- 0

bcs_cc_42$altdeg <- factor(bcs_cc_42$altdeg)

#check NAs

table(is.na(bcs_cc_42$altdeg))

#demean time-varying variable, from package "parameters"

bcs_cc_42 <- cbind(bcs_cc_42, demean(bcs_cc_42,
                                     select = "altdeg",
                                     group = "bcsid"))

#construct models

alt_rewb_a <- lmer(a ~ altdeg_within + altdeg_between + age + female + Mauth + 
                       parscr + pared + eng10 + maths10 + nonbrit30 +
                       (1 + altdeg_within | bcsid),
                     data = bcs_cc_42)

summary(alt_rewb_a)

alt_rewb_lr <- lmer(lr ~ altdeg_within + altdeg_between + age + female + Mauth +
                        parscr + pared + eng10 + maths10 + nonbrit30 +
                        (1 + altdeg_within | bcsid),
                      data = bcs_cc_42)

summary(alt_rewb_lr)

alt_rewb_r <- lmer(r ~ altdeg_within + altdeg_between + age + female + Mauth +
                       parscr + pared + eng10 + maths10 + nonbrit30 +
                       (1 | bcsid),
                     data = bcs_cc_42)

summary(alt_rewb_r)

#produce summary tables for each outcome

tab_model(alt_rewb_r,alt_rewb_a,alt_rewb_lr,
          show.ci = FALSE, 
          show.se = TRUE, 
          auto.label = FALSE, 
          string.se = "SE",
          p.style="stars",
          file="alt_rewb_cc.html",
          dv.labels = c("Racial prejudice", "Authoritarianism", "Left-Right"))

#####
#10. MODELS WITH IMPUTED DATA

#####THIS HAS ALREADY BEEN DONE IN THE SAVED FILE BELOW
#remove unimputed data

bcs_mi_42_l <- bcs_mi_42_l %>% filter(`.imp`!="0")

#code deg at 16 as 0 throughout

bcs_mi_42_l$deg[bcs_mi_42_l$age==16] <- 0

#demean time-varying variable, from package "parameters"

bcs_mi_42_l$imp_id <- paste0(bcs_mi_42_l$`.imp`,"_",bcs_mi_42_l$bcsid)

bcs_mi_42_l <- cbind(bcs_mi_42_l, demean(bcs_mi_42_l,
                                     select = "deg",
                                     group = "imp_id"))

bcs_mi_42_l$imp_id <- NULL

write_rds(bcs_mi_42_l, "bcs_mi_42_l.rds")

### Define a list that mitml will link to the multiply imputed data.

bcs_mi_42_l <- read_rds("bcs_mi_42_l.rds")

implist <-  as.mitml.list(split(bcs_mi_42_l, bcs_mi_42_l$`.imp`)) #imp is the variable that identifies the imputed dataset an observation belongs to

### Analyze the imputed datasets and pool the results. 
#Auth

a_form <- "a ~ deg_within + deg_between + age + female + Mauth +
                       parscr + pared + eng10 + maths10 + nonbrit30 +
                       (1 + deg_within | bcsid)"
imp_rewb <- with(implist, lmer(a_form,
                               REML = FALSE, 
                               control = lmerControl(optimizer ="Nelder_Mead")))
a_imp_est <- testEstimates(imp_rewb, df.com = NULL, extra.pars = T)
a_imp_est

a_imp_r2 <- tibble(R2m = rep(0,75),
                   R2c = rep(0,75))

for (i in seq_along(imp_rewb)) {
  a_imp_r2[i,] <- r.squaredGLMM(imp_rewb[[i]])
}

mean(a_imp_r2$R2m)
mean(a_imp_r2$R2c)

#Left-Right

lr_form <- "lr ~ deg_within + deg_between + age + female + Mauth +
                       parscr + pared + eng10 + maths10 + nonbrit30 +
                       (1 + deg_within | bcsid)"
imp_rewb <- with(implist, lmer(lr_form,
                               REML = FALSE, 
                               control = lmerControl(optimizer ="Nelder_Mead")))
lr_imp_est <- testEstimates(imp_rewb, df.com = NULL,extra.pars = T)
lr_imp_est

lr_imp_r2 <- tibble(R2m = rep(0,75),
                   R2c = rep(0,75))

for (i in seq_along(imp_rewb)) {
  lr_imp_r2[i,] <- r.squaredGLMM(imp_rewb[[i]])
}

mean(lr_imp_r2$R2m)
mean(lr_imp_r2$R2c)

#Racial prejudice

r_form <- "r ~ deg_within + deg_between + age + female + Mauth +
                       parscr + pared + eng10 + maths10 + nonbrit30 +
                       (1 | bcsid)"
imp_rewb <- with(implist, lmer(r_form,
                                 REML = FALSE, 
                                 control = lmerControl(optimizer ="Nelder_Mead")))
r_imp_est <- testEstimates(imp_rewb, df.com = NULL, extra.pars = T)
r_imp_est

r_imp_r2 <- tibble(R2m = rep(0,75),
                   R2c = rep(0,75))

for (i in seq_along(imp_rewb)) {
  r_imp_r2[i,] <- r.squaredGLMM(imp_rewb[[i]])
}

mean(r_imp_r2$R2m)
mean(r_imp_r2$R2c)

#Export MI results as table

tidy.mitml.testEstimates <- function(x, ...) {
  ret <- data.frame(
    term      = row.names(x$estimates),
    estimate  = x$estimates[, 1],
    std.error  = x$estimates[, 2],
    p.value = x$estimates[, 5],
    conf.low = confint(x)[,1],
    conf.high = confint(x)[,2],
    FMI = x$estimates[, 7])
  ret
}

glance.mitml.testEstimates <- function(x, ...) {
  data.frame(
    nimp = x$m,
    `σ2` = x$extra.pars[4],
    `τ00` = x$extra.pars[1],
    `τ11` = x$extra.pars[2],
    `r01` = x$extra.pars[3],
    ICC = x$extra.pars[5])
}

#output estimates and stars

modelsummary(list(r_imp_est,a_imp_est,lr_imp_est), estimate = "{estimate}{stars}",
             statistic = NULL, "rewb_mi.html")

r_imp_est
a_imp_est
lr_imp_est

nrow(implist$`1`)/4

#output SEs

modelsummary(list(r_imp_est,a_imp_est,lr_imp_est), estimate = "{std.error}",
             statistic = NULL, "rewb_mi_se.html")

###NOT PRESENTED
#FE with multiply imputed data

#Define a list that mitml will link to the multiply imputed data.

bcs_mi_42_l <- read_rds("bcs_mi_42_l.rds")

implist <-  as.mitml.list(split(bcs_mi_42_l, bcs_mi_42_l$`.imp`)) #imp is the variable that identifies the imputed dataset an observation belongs to

### Analyze the imputed datasets and pool the results. 
#Auth

imp_fe <- with(implist, felm(a ~ age + deg | bcsid))
a_imp_fe <- testEstimates(imp_fe, extra.pars = T)
a_imp_fe

#Left-Right

imp_fe <- with(implist, felm(lr ~ age + deg | bcsid))
lr_imp_fe <- testEstimates(imp_fe)
lr_imp_fe

#Racial prejudice

imp_fe <- with(implist, felm(r ~ age + deg | bcsid))
r_imp_fe <- testEstimates(imp_fe)
r_imp_fe

summary(r_imp_fe)

#Export MI results as table

tidy.mitml.testEstimates <- function(x, ...) {
  ret <- data.frame(
    term      = row.names(x$estimates),
    estimate  = x$estimates[, 1],
    std.error  = x$estimates[, 2],
    p.value = x$estimates[, 5],
    conf.low = confint(x)[,1],
    conf.high = confint(x)[,2],
    FMI = x$estimates[, 7])
  ret
}

#output estimates and stars

modelsummary(list(r_imp_fe,a_imp_fe,lr_imp_fe), estimate = "{estimate}{stars}",
             statistic = NULL, "fe_mi.html")

#output SEs

modelsummary(list(r_imp_fe,a_imp_fe,lr_imp_fe), estimate = "{std.error}",
             statistic = NULL, "fe_mi_se.html")

#apply entropy-balanced weights to imputed data
#Auth

a_form <- "a ~ deg_within + deg_between + age + female + Mauth +
                       parscr + pared + BD3RDAGE + BD3MATHS + nonbrit30 +
                       (1 + deg_within | bcsid)"
imp_rewb <- with(implist, lmer(a_form, weights = weights,
                               REML = FALSE, 
                               control = lmerControl(optimizer ="Nelder_Mead")))
a_imp_est <- testEstimates(imp_rewb, df.com = NULL, extra.pars = T)
a_imp_est
confint(a_imp_est)

#Left-Right

lr_form <- "lr ~ deg_within + deg_between + age + female + Mauth +
                       parscr + pared + BD3RDAGE + BD3MATHS + nonbrit30 +
                       (1 + deg_within | bcsid)"
imp_rewb <- with(implist, lmer(lr_form, weights = weights,
                               REML = FALSE, 
                               control = lmerControl(optimizer ="Nelder_Mead")))
lr_imp_est <- testEstimates(imp_rewb, df.com = NULL,extra.pars = T)
lr_imp_est

#Racial prejudice

r_form <- "r ~ deg_within + deg_between + age + female + Mauth +
                       parscr + pared + BD3RDAGE + BD3MATHS + nonbrit30 +
                       (1 + deg_within | bcsid)"
imp_rewb <- with(implist, lmer(r_form, weights = weights,
                               REML = FALSE, 
                               control = lmerControl(optimizer ="Nelder_Mead")))
r_imp_est <- testEstimates(imp_rewb, df.com = NULL, extra.pars = T)
r_imp_est

#####
#11. VISUALISING RESULTS
#Bespoke dot-whisker plot

lr.cc <- tidy(model_rewb_lr,conf.int = T,effects = "fixed")[-1]
lr.cc$Outcome <- "Left-Right"
lr.cc$Data <- "Complete Cases"
lr.cc$Model <- "Left-Right (CC)"

r.cc <- tidy(model_rewb_r,conf.int = T,effects = "fixed")[-1]
r.cc$Outcome <- "Racial Prejudice"
r.cc$Data <- "Complete Cases"
r.cc$Model <- "Racial Prejudice (CC)"

a.cc <- tidy(model_rewb_a,conf.int = T,effects = "fixed")[-1]
a.cc$Outcome <- "Authoritarianism"
a.cc$Data  <- "Complete Cases"
a.cc$Model <- "Authoritarianism (CC)"

tidy.mitml.testEstimates <- function(x, ...) {
  ret <- tibble(
    term      = row.names(x$estimates),
    estimate  = x$estimates[, 1],
    std.error  = x$estimates[, 2],
    statistic  = x$estimates[, 3],
    conf.low = confint(x)[,1],
    conf.high = confint(x)[,2])
  ret
}

lr.mi <- tidy(lr_imp_est)
lr.mi$Outcome <- "Left-Right"
lr.mi$Data <- "Multiply Imputed"
lr.mi$Model <- "Left-Right (MI)"

r.mi <- tidy(r_imp_est)
r.mi$Outcome <- "Racial Prejudice"
r.mi$Data <- "Multiply Imputed"
r.mi$Model <- "Racial Prejudice (MI)"

a.mi <- tidy(a_imp_est)
a.mi$Outcome <- "Authoritarianism"
a.mi$Data <- "Multiply Imputed"
a.mi$Model <- "Authoritarianism (MI)"

tidies <- rbind(r.cc,r.mi,a.cc,a.mi,lr.cc,lr.mi)

tidies <- tidies[grepl("deg",tidies$term),]

tidies$term <- gsub("deg_w","W",tidies$term)
tidies$term <- gsub("deg_b","B",tidies$term)

ggplot(tidies, aes(estimate, term, colour = Outcome, shape = Data)) +
  geom_point(alpha = 1, size = 3, position = position_dodge(.5)) +
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high, height =.2), position=position_dodge(.5)) +
  theme_bw() + xlab("Coefficient") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + scale_color_grey() +
  guides(colour = guide_legend(reverse=T), shape = guide_legend(reverse=T))

ggsave("results.png",type="cairo")

#Trajectory plot of FE estimates
#has to be lm object to work

model_fe_a <- lm(a ~ age + deg + bcsid, data = bcs_cc_42)

fea_out <- emmeans(model_fe_a, specs = "age", by = "deg", nuisance = "bcsid") %>%
  data.frame() %>% mutate(value = "Authoritarianism")

fea_out

model_fe_lr <- lm(lr ~ age + deg + bcsid, data = bcs_cc_42)

felr_out <- emmeans(model_fe_lr, specs = "age", by = "deg", nuisance = "bcsid") %>%
  data.frame() %>% mutate(value = "Left-Right")

felr_out

model_fe_r <- lm(r ~ age + deg + bcsid, data = bcs_cc_42)

fer_out <- emmeans(model_fe_r, specs = "age", by = "deg", nuisance = "bcsid") %>%
  data.frame() %>% mutate(value = "Racial prejudice")

fer_out

traj_fe <- rbind(fea_out[-5,],felr_out[-5,],fer_out[-4,])

traj_fe_adds <- traj_fe %>%
  dplyr::filter(age==16) %>%
  mutate(deg = 1)

traj_fe <- rbind(traj_fe,traj_fe_adds) %>% arrange(age,deg,value)

post_traj_plot <- traj_fe %>%
  ggplot(aes(x = age, y = emmean, ymin = lower.CL, ymax = upper.CL, group = factor(deg))) +
  geom_point(aes(shape=factor(deg)),size = 2) + geom_line(aes(linetype=factor(deg))) +
  geom_errorbar(width=.1) + facet_wrap(facets = vars(value)) +
  labs(x = "Age",
       y = "SD",
       group = "Graduate",
       shape = "Graduate",
       linetype = "Graduate")

post_traj_plot + theme_bw()

ggsave("fe_traj_plot.png",type="cairo",width = 18,height = 8, units = "cm")

#comparing REWB, TWFE and MI estimates

lr.cc <- tidy(model_rewb_lr,conf.int = T,effects = "fixed")[-1]
lr.cc$Outcome <- "Left-Right"
lr.cc$Model <- "REWB"

r.cc <- tidy(model_rewb_r,conf.int = T,effects = "fixed")[-1]
r.cc$Outcome <- "Racial Prejudice"
r.cc$Model <- "REWB"

a.cc <- tidy(model_rewb_a,conf.int = T,effects = "fixed")[-1]
a.cc$Outcome <- "Authoritarianism"
a.cc$Model <- "REWB"

lr.fe <- tidy(model_fe_lr,conf.int = T)[-5]
lr.fe$Outcome <- "Left-Right"
lr.fe$Model <- "TWFE"

r.fe <- tidy(model_fe_r,conf.int = T)[-5]
r.fe$Outcome <- "Racial Prejudice"
r.fe$Model <- "TWFE"

a.fe <- tidy(model_fe_a,conf.int = T)[-5]
a.fe$Outcome <- "Authoritarianism"
a.fe$Model <- "TWFE"

tidy.mitml.testEstimates <- function(x, ...) {
  ret <- tibble(
    term      = row.names(x$estimates),
    estimate  = x$estimates[, 1],
    std.error  = x$estimates[, 2],
    statistic  = x$estimates[, 3],
    conf.low = confint(x)[,1],
    conf.high = confint(x)[,2])
  ret
}

lr.mi <- tidy(lr_imp_est)
lr.mi$Outcome <- "Left-Right"
lr.mi$Model <- "MI REWB"

r.mi <- tidy(r_imp_est)
r.mi$Outcome <- "Racial Prejudice"
r.mi$Model <- "MI REWB"

a.mi <- tidy(a_imp_est)
a.mi$Outcome <- "Authoritarianism"
a.mi$Model <- "MI REWB"

tidies <- rbind(r.cc,r.fe,r.mi,a.cc,a.fe,a.mi,lr.cc,lr.fe,lr.mi)

tidies$term <- gsub("_within","",tidies$term)

tidies <- tidies[grepl("deg$",tidies$term),]

ggplot(tidies, aes(estimate, Outcome, shape = Model,colour = Model)) +
  geom_point(alpha = 1, size = 3, position = position_dodge(.4)) +
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high, height =.2), position=position_dodge(.4)) +
  theme_bw() + xlab("Coefficient") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + scale_color_grey() +
  guides(colour = guide_legend(reverse=T), shape = guide_legend(reverse=T))

ggsave("results_all.png",type="cairo")

#comparing CC and entropy-balanced estimates

lr.eb <- tidy(eb_rewb_lr,conf.int = T,effects = "fixed")[-1]
lr.eb$Outcome <- "Left-Right"
lr.eb$Data <- "Entropy Balanced"
lr.eb$Model <- "Left-Right (EB)"

r.eb <- tidy(eb_rewb_r,conf.int = T,effects = "fixed")[-1]
r.eb$Outcome <- "Racial Prejudice"
r.eb$Data <- "Entropy Balanced"
r.eb$Model <- "Racial Prejudice (EB)"

a.eb <- tidy(eb_rewb_a,conf.int = T,effects = "fixed")[-1]
a.eb$Outcome <- "Authoritarianism"
a.eb$Data  <- "Entropy Balanced"
a.eb$Model <- "Authoritarianism (EB)"

tidies <- rbind(r.cc,r.eb,a.cc,a.eb,lr.cc,lr.eb)

tidies <- tidies[grepl("deg",tidies$term),]

tidies$term <- gsub("deg_w","W",tidies$term)
tidies$term <- gsub("deg_b","B",tidies$term)

ggplot(tidies, aes(estimate, term, colour = Outcome, shape = Data)) +
  geom_point(alpha = 1, size = 3, position = position_dodge(.5)) +
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high, height =.2), position=position_dodge(.5)) +
  theme_bw() + xlab("Coefficient") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + scale_color_grey() +
  guides(colour = guide_legend(reverse=T), shape = guide_legend(reverse=T))

ggsave("results_ebal.png",type="cairo")