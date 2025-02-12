# Load necessary libraries
library(haven)  # For reading SAS files
library(dplyr)  # For data manipulation
library(survey) # For survey analysis

# Define paths to the data files
panel_path <- "D:/UAMS/Project/Data_sets/Full_Panels/"
med_path <- "D:/UAMS/Project/Data_sets/Medical_Conditions/"
full_path <- "D:/UAMS/Project/Data_sets/Full_Year/"
output_path <- "D:/UAMS/Project/Data_sets/Output_data/"

# Import Medical Expenditure Panel Survey (MEPS) Full Year Consolidated Data
f_2019 <- read_sas(paste0(full_path, "h216.sas7bdat"))
f_2018 <- read_sas(paste0(full_path, "h209.sas7bdat"))
f_2017 <- read_sas(paste0(full_path, "h201.sas7bdat"))
f_2016 <- read_sas(paste0(full_path, "h192.sas7bdat"))
f_2015 <- read_sas(paste0(full_path, "h181.sas7bdat"))
f_2014 <- read_sas(paste0(full_path, "h171.sas7bdat"))

# Import MEPS Medical Conditions Data
m_2019 <- read_sas(paste0(med_path, "H214.sas7bdat"))
m_2018 <- read_sas(paste0(med_path, "H207.sas7bdat"))
m_2017 <- read_sas(paste0(med_path, "H199.sas7bdat"))
m_2016 <- read_sas(paste0(med_path, "H190.sas7bdat"))
m_2015 <- read_sas(paste0(med_path, "H180.sas7bdat"))
m_2014 <- read_sas(paste0(med_path, "h170.sas7bdat"))

# Function to filter epilepsy cases and merge with full year data
process_epilepsy_data <- function(med_data, full_data, year) {
  epilepsy_data <- med_data %>%
    filter(ICD10CDX == "G40" | ICD9CODX == "345") %>%
    mutate(epi_flag = 1, year = year) %>%
    select(DUPERSID, epi_flag, year)
  
  merged_data <- inner_join(epilepsy_data, full_data, by = "DUPERSID")
  
  # Select and rename columns
  merged_data <- merged_data %>%
    select(DUPERSID, epi_flag, year, starts_with("AGE"), SEX, RACETHX, starts_with("REGION"), EDUCYR, starts_with("POVLEV"), 
           starts_with("INSCOV"), starts_with("MARRY"), HAVEUS42, PROVTY42_M18, EMPST42, RTHLTH42, MNHLTH42, VPCS42, VMCS42, 
           starts_with("OBTOTV"), starts_with("OPTOTV"), starts_with("OBVEXP"), starts_with("TOTEXP"), VARSTR, VARPSU, 
           starts_with("PERWT"), starts_with("SAQWT"))
  
  # Rename columns
  colnames(merged_data) <- gsub("\\d{2}X", "", colnames(merged_data))
  colnames(merged_data) <- gsub("\\d{2}", "", colnames(merged_data))
  
  return(merged_data)
}

# Process each year's data
epilepsy_2019m <- process_epilepsy_data(m_2019, f_2019, 2019)
epilepsy_2018m <- process_epilepsy_data(m_2018, f_2018, 2018)
epilepsy_2017m <- process_epilepsy_data(m_2017, f_2017, 2017)
epilepsy_2016m <- process_epilepsy_data(m_2016, f_2016, 2016)
epilepsy_2015m <- process_epilepsy_data(m_2015, f_2015, 2015)
epilepsy_2014m <- process_epilepsy_data(m_2014, f_2014, 2014)

# Combine all years' data
epilepsy14to19 <- bind_rows(epilepsy_2019m, epilepsy_2018m, epilepsy_2017m, epilepsy_2016m, epilepsy_2015m, epilepsy_2014m) %>%
  filter(AGE > 0 & PCS > 0 & MCS > 0) %>%
  arrange(year)

# Create additional categorical variables
epilepsy14to19 <- epilepsy14to19 %>%
  mutate(
    educat = case_when(
      EDUCYR <= 11 ~ 1,
      EDUCYR == 12 ~ 2,
      EDUCYR >= 13 & EDUCYR <= 16 ~ 3,
      EDUCYR >= 17 ~ 4
    ),
    marrycat = case_when(
      MARRY %in% c(1, 7) ~ 1,
      MARRY %in% c(2, 3, 4, 8, 9, 10) ~ 2,
      MARRY == 5 ~ 3
    ),
    povcat = case_when(
      POVLEV < 100 ~ 1,
      POVLEV >= 100 & POVLEV < 200 ~ 2,
      POVLEV >= 200 & POVLEV < 400 ~ 3,
      POVLEV >= 400 ~ 4
    ),
    EMPSTCAT = case_when(
      EMPST %in% c(1, 3) ~ 1,
      EMPST == 4 ~ 2,
      TRUE ~ NA_real_
    ),
    SAQWT1419 = SAQWT / 6
  )

# Convert categorical variables to character
epilepsy_fxd <- epilepsy14to19 %>%
  mutate(
    agecat_char = sprintf("%01d", agecat),
    sex_char = sprintf("%01d", SEX),
    RACETHX_char = sprintf("%01d", RACETHX),
    marrycat_char = sprintf("%01d", marrycat),
    educat_char = sprintf("%01d", educat),
    region_char = sprintf("%01d", REGION),
    povcat_char = sprintf("%01d", povcat),
    inscov_char = sprintf("%01d", INSCOV),
    EMPSTCAT_char = sprintf("%01d", EMPSTCAT),
    rthlth_char = sprintf("%01d", RTHLTH),
    mnhlth_char = sprintf("%01d", MNHLTH),
    HAVEUS_char = sprintf("%01d", HAVEUS),
    YEAR_char = sprintf("%04d", year)
  ) %>%
  select(-agecat, -SEX, -RACETHX, -marrycat, -educat, -REGION, -povcat, -INSCOV, -EMPSTCAT, -RTHLTH, -MNHLTH, -HAVEUS, -year)

# Descriptive statistics
svy_design <- svydesign(ids = ~VARPSU, strata = ~VARSTR, weights = ~SAQWT1419, data = epilepsy_fxd, nest = TRUE)

# Frequency tables
svytable(~inscov_char + sex_char + RACETHX_char + educat_char + region_char + povcat_char + EMPSTCAT_char + rthlth_char + mnhlth_char + HAVEUS_char + YEAR_char, design = svy_design)

# Descriptive statistics of continuous variables
svymean(~AGE + PCS + MCS, design = svy_design)
svyquantile(~AGE + PCS + MCS, design = svy_design, quantiles = c(0.25, 0.5, 0.75))

# Survey-weighted GLM for PCS
svyglm_pcs <- svyglm(PCS ~ inscov_char + sex_char + RACETHX_char + educat_char + region_char + povcat_char + EMPSTCAT_char + YEAR_char + AGE, 
                     design = svy_design, family = gaussian())

# Summarize the survey-weighted GLM results for PCS
summary(svyglm_pcs)

# Survey-weighted GLM for MCS
svyglm_mcs <- svyglm(MCS ~ inscov_char + sex_char + RACETHX_char + educat_char + region_char + povcat_char + EMPSTCAT_char + YEAR_char + AGE, 
                     design = svy_design, family = gaussian())

# Summarize the survey-weighted GLM results for MCS
summary(svyglm_mcs)

# Export data to CSV
write.csv(epilepsy14to19, file = paste0(output_path, "epilepsy15to19.csv"), row.names = FALSE)