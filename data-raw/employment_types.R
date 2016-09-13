employment_types <- data_frame(
  sector = c(
    "BuySell_A1_Mgmt_Bus", "BuySell_Accommodations",
    "BuySell_B1_Prof_Specialty",
    "BuySell_B2_Education",  "BuySell_B3_Health",
    "BuySell_B4_Technical_Unskilled",
    "BuySell_C1_Sales_Clerical_Professionals", "BuySell_C2_Sales_Service",
    "BuySell_C3_Clerical", "BuySell_C4_Sales_Clerical_Unskilled",
    "BuySell_Capital_Transfer_Receipts", "BuySell_Communications_and_Utilities",
    "BuySell_Construction", "BuySell_D1_Production_Specialists",
    "BuySell_D2_MaintConstRepair_Specialists",
    "BuySell_D3_ProtectTrans_Specialists",
    "BuySell_D4_Blue_Collar_Unskilled", "BuySell_Education_Reports_to_Sponsors",
    "BuySell_Energy", "BuySell_Entertainment_Services",
    "BuySell_Fire_Business_and_Professional_Services",
    "BuySell_Food_Services", "BuySell_Government_Administration",
    "BuySell_Government_Support_Receipts", "BuySell_Health_Services",
    "BuySell_Higher_Education", "BuySell_Internal_Services_Construction",
    "BuySell_Internal_Services_Education_k12",
    "BuySell_Internal_Services_Energy",
    "BuySell_Internal_Services_Government_Administration",
    "BuySell_Internal_Services_Information",
    "BuySell_Internal_Services_Manufacturing",
    "BuySell_Internal_Services_Resources",
    "BuySell_Internal_Services_Retail_Store",
    "BuySell_Internal_Services_Utilities",
    "BuySell_Internal_Services_Wholesale",
    "BuySell_Internal_ServicesTransport",
    "BuySell_Investing_Receipts",
    "BuySell_Personal_and_Other_Services_and_Amusements",
    "BuySell_Proprietor_Income_Receipts", "BuySell_Retail_Trade",
    "BuySell_Return_Investment_Receipts", "BuySell_Teaching_K12"
  ),
  naics = c(
    "55 - Management", "72 - Accomodation", "54 - Professional",
    "61 - Education", "62 - Health",

    # these don't match well
    "31- Technical Unskilled",
    "44 - C", "44 - C", "44 - C", "44 - C",

    "52 - Finance",

    "22 - Utilities",
    "23 - Construction",

    "33 - D1", "33 - D2", "33 - D3", "33 - D4",
    "61 - Education",
    "21 - Energy",
    "72 - Food Services",
    "54 - Professional Services",
    "92 - Public Administration",
    "92 - Public Administration",
    "62 - Health",
    "61 - Education",
    "23 - Construction",
    "61 - Education",
    "81 - Other Services",
    "21 - Energy",
    "92 - Public Administration",
    "51 - Information",
    "33 - Manufacturing",
    "81 - Other Services",
    "44 - Retail",
    "22 - Utilities",
    "42 - Wholesale",
    "48 - Transportation",
    "52 - Finance",
    "81 - Other Services",
    "55 - Management",
    "44 - Retail",
    "52 - Finance",
    "61 - Education"
  ),
  naics1 = substr(naics, 1, 1),
  naics_label = as.character(factor(
    naics1,
    labels = c("Energy/Utilities", "Technical/Manufacturing",
               "Retail/Transport", "Professional", "Health/Education",
               "Food", "Services", "Government")
  ))) %>%
  mutate(
    naics_label = ifelse(
      sector == "BuySell_Personal_and_Other_Services_and_Amusements", "Retail/Transport",
      ifelse(
        sector == "BuySell_Internal_Services_Resources", "Energy/Utilities",
        ifelse(
          sector == "BuySell_Internal_Services_Education_k12", "Health/Education", naics_label
        ) )),
    naics_label = ifelse(naics_label == "Food", "Retail/Transport", naics_label)
  )

devtools::use_data(employment_types, overwrite = TRUE)

# define consolidated employment types
emp_types <- data_frame(
  ACTIVITY = c(
    "CNST", "ENGY", "ENT", "FIRE", "GOV", "HIED", "HLTH", "HOSP",
    "INFO", "K12", "MFG", "RES", "RET", "SERV", "TRNS", "UTL", "WHSL"
  ),
  emp_type = c(
    "Const/Man/Transp", "Energy/Resources", "Retail",
    "Institutional", "Public Services", "Education", "Health", "Health",
    "Professional", "Education", "Const/Man/Transp", "Energy/Resources",
    "Retail", "Retail", "Const/Man/Transp",
    "Energy/Resources", "Const/Man/Transp"
  )
)

devtools::use_data(emp_types, overwrite = TRUE)
