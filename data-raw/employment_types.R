
employment_types <- data_frame(
  sector = names(table(employment$sector)),
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
    "54 - Professional Services",
    "72 - Food Services",
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
  naics1 = substr(naics, 1, 1)
)

devtools::use_data(employment_types)


