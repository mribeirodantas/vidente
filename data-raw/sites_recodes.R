# Code to generate table with cancer primary site name and recode code for
# the functions primarySiteLookUp and listPrimarySite.

sites <- c(
  # Oral Cavity and Pharynx
  'Lip', 'Tongue', 'Salivary Gland', 'Floor of Mouth', 'Gum and Other Mouth',
  'Nasopharynx', 'Tonsil', 'Oropharynx', 'Hypopharynx', 'Other Oral Cavity and
  Pharynx',
  # Digestive System
  'Esopohagus', 'Stomach', 'Small Intestine',
  # Colon excluding rectum
  'Cecum', 'Appendix', 'Ascending Colon', 'Hepatic Flexure',
  'Transverse Colon', 'Splenic Flexure', 'Descending Colon', 'Sigmoid Colon',
  'Large Intestine, NOS',
  # Rectum and Rectosigmoid Junction
  'Rectosigmoid Junction', 'Rectum', 'Anus, Anal Canal and Anorectum',
  # Liver and Intrahepatic Bile Duct
  'Liver', 'Intrahepatic Bile Duct', 'Gallbladder', 'Other Biliary',
  'Pancreas', 'Retroperitoneum', 'Peritoneum, Omentum and Mesentery',
  'Other Digestive Organs',
  # Respiratory System
  'Nose, Nasal Cavity and Middle Ear', 'Larynx', 'Lung and Bronchus', 'Pleura',
  'Trachea, Mediastinum and Other Respiratory Organs',
  # Bones and Joints
  'Bones and Joints',
  # Soft Tissue including Heart
  'Soft Tissue including Heart',
  # Skin excluding Basal and Squamous
  'Melanoma of the Skin', 'Other Non-Epithelial Skin',
  # Breast
  'Breast',
  # Female Genital System
  'Cervix Uteri', 'Corpus Uteri', 'Uterus, NOS', 'Ovary', 'Vagina', 'Vulva',
  'Other Female Genital Organs',
  # Male Genital System
  'Prostate', 'Testis', 'Penis', 'Other Male Genital Organs',
  # Urinary System
  'Urinary Bladder', 'Kidney and Renal Pelvis', 'Ureter',
  'Other Urinary Organs',
  # Eye and Orbit
  'Eye and Orbit',
  # Brain and Other Nervous System
  'Brain', 'Cranial Nerves Other Nervous System',
  # Endocrine System
  'Thyroid', 'Other Endocrine including Thymus',
  # Lymphoma
  # Hodgkin
  'Hodgkin - Nodal', 'Hodgkin - Extranodal',
  # Non-Hodgkin
  'NHL - Nodal', 'NHL - Extranodal',
  # Myeloma
  'Myeloma',
  # Leukemia
  # Lymphocite Leukemia
  'Acute Lymphocytic Leukemia', 'Chronic Lymphocytic Leukemia',
  'Other Lymphocytic Leukemia',
  # Myeloid and Monocytic Leukemia
  'Acute Myeloid Leukemia', 'Acute Monocytic Leukemia',
  'Chronic Myeloid Leukemia', 'Other Myeloid/Monocytic Leukemia',
  # Other Leukemia
  'Other Acute Leukemia', 'Aleukemic, subleukemic and NOS',
  # Mesothelioma
  'Mesothelioma',
  # Kaposi Sarcoma
  'Kaposi Sarcoma',
  # Miscellaneous
  'Miscellaneous',
  # Invalid
  'Invalid')

sites <- stringr::str_to_title(sites)
recodes <- c(seq(20010, 20100, 10), seq(21010, 21030, 10), seq(21041, 21049),
             21051, 21052, 21060, 21071, 21072, seq(21080, 21130, 10), 22010,
             22020, 22030, 22050, 22060, 23000, 24000, 25010, 25020, 26000,
             seq(27010, 27070, 10), seq(28010, 28040, 10),
             seq(29010, 29040, 10), 30000, 31010, 31040, 32010, 32020, 33011,
             33012, 33041, 33042, 34000, 35011, 35012, 35013, 35021, 35031,
             35022, 35023, 35041, 35043, 36010, 36020, 37000, 99999)

sites_recodes <- as.data.frame(cbind(sites, recodes), stringsAsFactor = FALSE)
setwd('/home/mribeirodantas/dev/vidente/R/')
save(sites_recodes, file = 'sites_recodes.rda')