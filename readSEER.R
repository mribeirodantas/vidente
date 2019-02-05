# function to read SEER data from one or several ASCII data files
readSEER <- function(path, year_dx, state, cancer, site, fips) {
  if (missing(path)) {
    stop("It's impossible to read the files if you supply no path to files.")
  }
  ## read the file with the fixed width positions
  data.df_f <- NULL
  for (path_file in paths) {
    data.df <- read_fwf(path_file, 
                        fwf_positions(sas.df$start, sas.df$end, sas.df$col_name)
                        # , colClasses=c("character","integer","character","integer","character","integer","character",
                        #              "character","character","character"))
    )
    data.df_f <- rbind(data.df_f, data.df)
  } 
  # select only those rows that match the chosen site
  if (missing(site)) {
    print(paste(get(red("Note:"))," Cancer primary site not supplied, including all sites"))
  } else {
    data.df_f <- data.df_f[!(data.df_f$SITERWHO == siteLookUp(site)), ] 
  }
  # select only those rows that match the chosen year of diagnosis (or range of)
  if (missing(site)) {
    print(paste(get(red("Note:"))," Year of diagnosis not supplied, including all years"))
  } else {
    data.df_f <- data.df_f[data.df_f$YEAR_DX %in% year_dx, ] 
  }
}

# Convert primary site name to recode
siteLookUp <- function(site_name) {
  if(!exists("sites_recodes")) {
    sites_recodes <- readRDS("/home/mribeirodantas/Documentos/PhD data/Breast Cancer Analysis/vidente/sites_recodes.RData")
  }
  return (subset(sites_recodes, sites == site_name)$recodes)
}

# List cancers primary sites that are supported by this package
listPrimarySites <- function() {
  if(!exists("sites_recodes")) {
    sites_recodes <- readRDS("/home/mribeirodantas/Documentos/PhD data/Breast Cancer Analysis/vidente/sites_recodes.RData")
  }
  cat(red("# Oral Cavity and Pharynx"))
  print(paste(sites_recodes$sites[1:10], collapse = ", "))
  cat(red("# Digestive System"))
  print(paste(sites_recodes$sites[11:13], collapse = ", "))
  cat(red("# Colon Excluding rectum"))
  print(paste(sites_recodes$sites[14:22], collapse = ", "))
  cat(red("# Rectum and Rectosigmoid Junction"))
  print(paste(sites_recodes$sites[23:25], collapse = ", "))
  cat(red("# Liver and Intrahepatic Bile Duct"))
  print(paste(sites_recodes$sites[26:33], collapse = ", "))
  cat(red("# Respiratory System"))
  print(paste(sites_recodes$sites[34:38], collapse = ", "))
  cat(red("# Bones and Joints"))
  print(paste(sites_recodes$sites[39], collapse = ", "))
  cat(red("# Soft Tissue including Heart"))
  print(paste(sites_recodes$sites[40], collapse = ", "))
  cat(red("# Breast"))
  print(paste(sites_recodes$sites[41], collapse = ", "))
  cat(red("# Female Genital System"))
  print(paste(sites_recodes$sites[42:48], collapse = ", "))
  cat(red("# Male Genital System"))
  print(paste(sites_recodes$sites[49:52], collapse = ", "))
  cat(red("# Urinary System"))
  print(paste(sites_recodes$sites[53:56], collapse = ", "))
  cat(red("# Eye and Orbit"))
  print(paste(sites_recodes$sites[57], collapse = ", "))
  cat(red("# Brain and Other Nervous System"))
  print(paste(sites_recodes$sites[58:60], collapse = ", "))
  cat(red("# Endocrine System"))
  print(paste(sites_recodes$sites[61:62], collapse = ", "))
  cat(red("# Lymphoma Hodgkin"))
  print(paste(sites_recodes$sites[63:64], collapse = ", "))
  cat(red("# Lymphoma Non-Hodgkin"))
  print(paste(sites_recodes$sites[65:66], collapse = ", "))
  cat(red("# Myeloma"))
  print(paste(sites_recodes$sites[67], collapse = ", "))
  cat(red("# Lymphocyte Leukemia"))
  print(paste(sites_recodes$sites[68:71], collapse = ", "))
  cat(red("# Myeloid and Monocytic Leukemia"))
  print(paste(sites_recodes$sites[72:75], collapse = ", "))
  cat(red("# Other Leukemia"))
  print(paste(sites_recodes$sites[76:77], collapse = ", "))
  cat(red("# Mesothelioma"))
  print(paste(sites_recodes$sites[78], collapse = ", "))
  cat(red("# Kaposi Sarcoma"))
  print(paste(sites_recodes$sites[79], collapse = ", "))
  cat(red("# Miscellaneous and Invalid"))
  print(paste(sites_recodes$sites[80:81], collapse = ", "))
}

# Code to compile cancer primary site name and recode code for primarySiteLookUp function and listPrimarySite
# Data retrieved from https://seer.cancer.gov/siterecode/icdo3_dwhoheme/index.html
# It is the SITERWHO feature in SEER data
# Generate db with keys
# sites <- c(# Oral Cavity and Pharynx
#   "Lip", "Tongue", "Salivary Gland", "Floor of Mouth", "Gum and Other Mouth", "Nasopharynx", "Tonsil", "Oropharynx", "Hypopharynx", "Other Oral Cavity and Pharynx",
#   # Digestive System
#   "Esopohagus", "Stomach", "Small Intestine",
#   # Colon excluding rectum
#   "Cecum", "Appendix", "Ascending Colon", "Hepatic Flexure", "Transverse Colon", "Splenic Flexure", "Descending Colon", "Sigmoid Colon", "Large Intestine, NOS",
#   # Rectum and Rectosigmoid Junction
#   "Rectosigmoid Junction", "Rectum", "Anus, Anal Canal and Anorectum",
#   # Liver and Intrahepatic Bile Duct
#   "Liver", "Intrahepatic Bile Duct", "Gallbladder", "Other Biliary", "Pancreas", "Retroperitoneum", "Peritoneum, Omentum and Mesentery", "Other Digestive Organs",
#   # Respiratory System
#   "Nose, Nasal Cavity and Middle Ear", "Larynx", "Lung and Bronchus", "Pleura", "Trachea, Mediastinum and Other Respiratory Organs",
#   # Bones and Joints
#   "Bones and Joints",
#   # Soft Tissue including Heart
#   "Soft Tissue including Heart",
#   # Skin excluding Basal and Squamous
#   "Melanoma of the Skin", "Other Non-Epithelial Skin",
#   # Breast
#   "Breast",
#   # Female Genital System
#   "Cervix Uteri", "Corpus Uteri", "Uterus, NOS", "Ovary", "Vagina", "Vulva", "Other Female Genital Organs",
#   # Male Genital System
#   "Prostate", "Testis", "Penis", "Other Male Genital Organs",
#   # Urinary System
#   "Urinary Bladder", "Kidney and Renal Pelvis", "Ureter", "Other Urinary Organs",
#   # Eye and Orbit
#   "Eye and Orbit",
#   # Brain and Other Nervous System
#   "Brain", "Cranial Nerves Other Nervous System",
#   # Endocrine System
#   "Thyroid",  "Other Endocrine including Thymus",
#   # Lymphoma
#   # Hodgkin
#   "Hodgkin - Nodal", "Hodgkin - Extranodal",
#   # Non-Hodgkin
#   "NHL - Nodal", "NHL - Extranodal",
#   # Myeloma
#   "Myeloma",
#   # Leukemia
#   # Lymphocite Leukemia
#   "Acute Lymphocytic Leukemia", "Chronic Lymphocytic Leukemia", "Other Lymphocytic Leukemia",
#   # Myeloid and Monocytic Leukemia
#   "Acute Myeloid Leukemia", "Acute Monocytic Leukemia", "Chronic Myeloid Leukemia", "Other Myeloid/Monocytic Leukemia",
#   # Other Leukemia
#   "Other Acute Leukemia", "Aleukemic, subleukemic and NOS",
#   # Mesothelioma
#   "Mesothelioma",
#   # Kaposi Sarcoma
#   "Kaposi Sarcoma",
#   # Miscellaneous
#   "Miscellaneous",
#   # Invalid
#   "Invalid")
# recodes <- c(seq(20010, 20100, 10), seq(21010, 21030, 10), seq(21041, 21049), 21051, 21052, 21060, 21071, 21072, seq(21080, 21130, 10),
#              22010, 22020, 22030, 22050, 22060, 23000, 24000, 25010, 25020, 26000, seq(27010, 27070, 10), seq(28010, 28040, 10), seq(29010, 29040, 10),
#              30000, 31010, 31040, 32010, 32020, 33011, 33012, 33041, 33042, 34000, 35011, 35012, 35013, 35021, 35031, 35022, 35023, 35041, 35043,
#              36010, 36020, 37000, 99999)
# sites_recodes <- as.data.frame(cbind(sites, recodes), stringsAsFactors = FALSE)
# saveRDS(sites_recodes, "sites_recodes.RData")
