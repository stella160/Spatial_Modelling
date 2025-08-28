#importing libraries
install.packages("scales")
install.packages("Amelia")
library(haven)
library(tidyverse)
library(dplyr)
library(janitor)
library(Amelia)
library(sf)
library(corrplot)

##Importing the dataset
df<-read_sav("C:\\Users\\ADMIN\\Downloads\\2015 STEPS Data (2).sav")

st_drivers()
#exploring the data
shapefile<-read_sf("C:/Users/ADMIN/Downloads/Shapefile-20240313T093520Z-001/Shapefile/ke_county.shp")
plot(shapefile)


###
head(df)
tail(df)
## finding the dimension of the data
dim(df)
## data types of variables
sapply(df,class)
## Selecting variables of interest and renaming them
df1<-df %>%
  rename(id=PID,education=educlevel,Age=C3,Sex=C1,Height=M11,Weight=M12,Cholestrol=B8,Diabetes_status=H7b,County=X1,Obesity_status=H20h,Physical_inacativity=H20g,Residential_area=X2a,
         Age_range=agerange,Fasting_blood_sugar=B5) %>%
  select(id,education,Age,Sex,Height,Weight,Cholestrol,Diabetes_status,County,Obesity_status,Physical_inacativity,
         Residential_area,Age_range,Fasting_blood_sugar) %>%
  filter(!complete.cases(.)) %>%
  view()
##summary statistics
summary(df1)
table(df1$education)
table(df1$Sex)
table(df1$Diabetes_status)

table(df1$Residential_area)
table(df1$Age_range)
##checking for missing values
missmap(df1)
sum(is.na(df1))
##diabetes status, age,weight,cholestrol, fasting blood sugar and height seem to have many missing values
## Mode function for imputation
mode_func<-function(x){
  unique_x <- na.omit(x) # remove NA values
  unique_x_tab <- table(unique_x) # tabulate unique values
  mode_value <- names(unique_x_tab)[which.max(unique_x_tab)] # find the mode
  return(mode_value)
}
##dropping unwanted columns
df2<-df1 %>%
  select(-Diabetes_status,-Age)
view(df2)
##creating the BMI column
df2$BMI <- df2$Weight / ((df2$Height / 100)^2)
df2$BMI <- round(df2$BMI, 1)
view(df2)
sum(is.na(df2$BMI))
##0 indicates no diabetes, 1 indicates prediabetes and 2 indicates diabetes
summary(df2$Fasting_blood_sugar)
df2$Diabetes<-NA
df2$Diabetes[df2$Fasting_blood_sugar<5.6]=0
df2$Diabetes[df2$Fasting_blood_sugar>=5.6 &df2$Fasting_blood_sugar<=6.9]=1
df2$Diabetes[df2$Fasting_blood_sugar>=7.0]=2
table(df2$Diabetes)
##ommiting missing values
missmap(df2)
sum(is.na(df2$Cholestrol))
sum(is.na(df2$Weight))
sum(is.na(df2$Height))
summary(df2$Weight)
df3<-na.omit(df2)
dim(df3)
missmap(df3)
view(df3)
sapply(df3,class)
## recoding county from numbers to names
df_clean<-df3 %>%
  mutate(County_name=case_when(County==1 ~"Baringo",County==2~"Bomet",County==3~"Bungoma",County==4~"Busia",County==5~"Elgeyo Marakwet",County==6~"Embu",County==7~"Garissa",County==8~"Homabay",County==9~"Isiolo",County==10~"Kajiado",County==11~"Kakamega",County==12~"Kericho",County==13~"Kiambu",County==14~"Kilifi",County==15~"Kirinyaga",County==16~"Kisii",County==17~"Kisumu",
                               County==18~"Kitui",County==19~"Kwale",County==20~"Laikipia",County==21~"Lamu",County==22~"Machakos",County==23~"Makueni",County==24~"Mandera",County==25~"Marsabit",County==26~"Meru",County==27~"Migori",County==28~"Mombasa",County==29~"Muranga",County==30~"Nairobi",County==31~"Nakuru",County==32~"Nandi",County==33~"Narok",County==34~"Nyamira",
                               County==35~"Nyandarua",County==36~"Nyeri",County==37~"Samburu",County==38~"Siaya",County==39~"Taita Taveta",County==40~"Tana River",County==41~"Tharaka Nithi",County==42~"Trans Nzoia",County==43~"Turkana",County==44~"Uasin Gishu",County==45~"Vihiga",County==46~"Wajir",County==47~"West Pokot"))
view(df_clean) 
  
  ##EDA
##checking the distributions of continous variable

## weight
ggplot(df_clean,aes(x=Weight))+
         geom_histogram(bins=90)+
         labs(title="Histogram of Weight",x="weight",y="frequency")
##height
ggplot(df_clean,aes(x=Height))+
  geom_histogram(bins=100)+
  labs(title="Histogram of height",x="height",y="frequency")
##BMI
ggplot(df_clean,aes(x=BMI))+
  geom_histogram(bins=50)+
  labs(title="Histogram of BMI",x="BMI",y="frequency")
hist(df_clean$BMI)

###Checking for outliers
boxplot(df_clean$Weight)
max(df_clean$Weight)
boxplot(df_clean$Weight)
summary(df_clean$Weight)
df_clean<-df_clean %>%
  filter(Weight!=0.00)
df_clean<-df_clean %>%
  filter(Weight!=888.8)
##height
boxplot(df_clean$Height)
summary(df_clean$Height)
df_clean<-df_clean%>%
  filter(Height !=63.5 & Height !=270.0)
##BMI
boxplot(df_clean$BMI)
summary(df_clean$BMI)
df_clean<-df_clean %>%
  filter(BMI!=100.0)
##Cholestrol
boxplot(df_clean$Cholestrol)
summary(df_clean$Cholestrol)
df_clean<-df_clean %>%
  filter(Cholestrol!=777.000)
##education
ggplot(df_clean,aes(x=education))+
  geom_bar(stat="count")
##Diabetes status
ggplot(df_clean,aes(x=Diabetes))+
  geom_bar(stat="count")+
  labs(title="Bar chart of Diabetes status",xlab="Diabetes status",ylab="Frequency")
##checking for duplicate values
df_clean<-unique(df_clean)
view(df_clean)
###########################
##########################
##MERGING DATA WITH SHAPEFILE
plot(shapefile)
#creating a common column
df_clean<-df_clean %>%
  rename(county=County_name)
head(df_clean)
sapply(df_clean,class)
##merging
str(shapefile)
merged_data<-merge(df_clean,shapefile,by="county")
view(merged_data)


#Exploratory data Analysis
#Generalized linear mixed models
library(lme4)
library(gtsummary)
library(broom.mixed)
class(merged_data$Diabetes)
merged_data$Diabetes_status<-as.numeric(merged_data$Diabetes)
class(merged_data$Diabetes)
merged_data<-merged_data %>%
  select(Age_range,Sex,BMI,Weight_Category,Diabetes,Physical_inacativity,Cholestrol,
         Residential_area,Fasting_blood_sugar,Obesity,NAME_2,geometry) %>%
  View()
class(merged_data$Sex)
library(haven)
rlang::last_trace()
x_factor <- haven::as_factor('Sex')
x_character <- as.character(x_factor)

#Fitting Generalized linear mixed models
packageVersion("Matrix")
install.packages("Matrix", type = "source")
install.packages("lme4")
library(lme4)
library(Matrix)
view(merged_data)
variables_selected<-c("NAME_2","id","education","Sex",         
                      "Height","Weight","Cholestrol","County",              
                     "Obesity_status","Physical_inacativity","Residential_area","Age_range",           
                    "Fasting_blood_sugar","BMI","Diabetes","ID_0",            
                   "ISO","NAME_0","ID_1","NAME_1","ID_2","TYPE_2","ENGTYPE_2","NL_NAME_2","VARNAME_2","geometry")

  
merged_data$Diabetes<-as.factor(merged_data$Diabetes)
structured_model <- glmer(Diabetes ~ Residential_area + Age_range + Sex + BMI + Obesity_status + Cholestrol + 
                              + Physical_inacativity + (1 | NAME_2), 
                            family = 'binomial', data = merged_data)
structured_model
unstructured_model <- glmer(Diabetes ~ Residential_area + Age_range + Sex + BMI + Obesity_status + Cholestrol + 
                              + Physical_inacativity + (1 | NAME_2), 
                            family = 'binomial', data = merged_data)
unstructured_model

both_model <- glmer(Diabetes ~ Residential_area + Age_range + Sex + BMI + Obesity_status + Cholestrol + 
                      + Physical_inacativity + (1 | NAME_2), 
                    family = 'binomial', data = merged_data)
both_model

# Convert models to tbl_regression objects
# format results into data frame with global p-values
library(dplyr)
library(table1)
library(gtsummary)
tbl_model_structured<- structured_model %>%
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
tbl_model_structured

tbl_model_unstructured<- unstructured_model %>%
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

tbl_model_unstructured

tbl_model_both<- both_model %>%
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

tbl_model_both

# Add model names
tbl_model_structured <- modify_caption(tbl_model_structured, 
                                       caption = "Model: Model 1")
tbl_model_unstructured <- modify_caption(tbl_model_unstructured, 
                                         caption = "Model: Model 2")
tbl_model_both <- modify_caption(tbl_model_both, 
                                 caption = "Model: Model 3")
# Combine tables into a single gtsummary table
combined_tbl <- tbl_merge(
  tbls = list(tbl_model_structured, tbl_model_unstructured, tbl_model_both),
  tab_spanner = c("**Model 1**", "**Model 2**", "**Model 3**")
)
# Print the combined table
combined_tbl
##cross tabulation
library(tidyverse)
library(gt)
library(gtsummary)



fn_subtable <- function(data, main, sub){
  data %>% 
    dplyr::select({{main}},{{sub}}) %>% 
    gtsummary::tbl_summary(
      by = {{sub}}, 
      statistic = gtsummary::all_categorical()~ "{p}% ({n})",
      digits = list(dplyr::everything() ~ c(2, 0))) %>%
    gtsummary::add_p() 
  
}
fn_table3 <- function(data, main_var, sub_vars){
  
  main_var <- rlang::enexpr(main_var)
  sub_vars_expr <- rlang::enexpr(sub_vars)         # 1. Capture `list(...)` call as expression
  sub_vars_args <- rlang::call_args(sub_vars_expr) # 2. Pull out the arguments (they're now also exprs)
  sub_vars_fn   <- rlang::call_fn(sub_vars_expr)   # 3. Pull out the fn call
  # 4. Evaluate the fn with expr-ed arguments (this becomes `list( expr(agegp), expr(alcgp) )` )
  sub_vars_reconstructed <- rlang::exec(sub_vars_fn, !!!sub_vars_args)
  
  # --- sub_vars replaced with sub_vars_reconstructed from here onwards ---
  
  t0 <- data %>% 
    dplyr::select({{main_var}}) %>% 
    gtsummary::tbl_summary(statistic = gtsummary::all_categorical() ~ "{p}% ({n})",
                           digits = list(dplyr::everything() ~ c(2, 0))) %>%
    gtsummary::modify_header(label ~ "") %>% 
    gtsummary::bold_labels()
  
  sub_tables <- purrr::map(sub_vars_reconstructed, ~fn_subtable(data = data, main = main_var, sub = .x))
  
  tbls <-  c(list(t0), sub_tables) %>% 
    gtsummary::tbl_merge(tab_spanner = c("**Total**", paste0("**",sub_vars_reconstructed,"**"))) %>%
    gtsummary::as_gt() %>% 
    gt::tab_source_note(gt::md("*Cross tabulations*"))
  
  tbls
  
}

fn_table3(merge_shf1,Diabetes_status,list(Residential_area ,Age_range ,Sex ,BMI,Obesity_status ,Cholesterol ,
                                          Physical_activity))

##### table1
library(table1)
table1(~ Place_of_residence + Age_group + Gender + BMI_class + Obesity + Hdl_cholesterol + Harmful_alcohol_intake + 
         High_sugar_intake + Physical_inactivity + Tobacco_use + Bad_fat_intake | Diabetes, data=merged_df, overall=F, extra.col=list(`P-value`=pvalue),
       topclass="Rtable1-grid Rtable1-shade Rtable1-times")

#table1(~ age + sex + wt | treat, data=dat, topclass="Rtable1-grid Rtable1-shade Rtable1-times")
#table1(~ age + race + married + nodegree + re74 + re75 + re78 | treat,
#data=lalonde, overall=F, extra.col=list(`P-value`=pvalue))

oo<-options(repos="https://cran.r-project.org/")
install.packages("Matrix")
install.packages("lme4")
options(oo)

install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"),dep=TRUE)



