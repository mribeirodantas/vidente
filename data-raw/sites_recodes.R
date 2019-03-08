# Code to generate tree with cancer primary site name And recode code for
# the functions primarySiteLookUp And listPrimarySites
library(data.tree)

sites <- Node$new("Primary sites (Site Recode ICD-O-3/WHO 2008 Definition)")

# 1. Oral Cavity And Pharynx
oral_cavity_pharynx <- sites$AddChild("Oral Cavity And Pharynx")
oral <- oral_cavity_pharynx$AddChild("Lip")
sites$`Oral Cavity And Pharynx`$Lip$recode <- 20010
# FindNode(node = sites, name = 'Lip')$recode
oral1 <- oral_cavity_pharynx$AddChild("Tongue")
sites$`Oral Cavity And Pharynx`$Tongue$recode <- 20020
oral2 <- oral_cavity_pharynx$AddChild("Salivary Gland")
sites$`Oral Cavity And Pharynx`$`Salivary Gland`$recode <- 20030
oral3 <- oral_cavity_pharynx$AddChild("Floor Of Mouth")
sites$`Oral Cavity And Pharynx`$`Floor Of Mouth`$recode <- 20040
oral4 <- oral_cavity_pharynx$AddChild("Gum And Other Mouth")
sites$`Oral Cavity And Pharynx`$`Gum And Other Mouth`$recode <- 20050
oral5 <- oral_cavity_pharynx$AddChild("Nasopharynx")
sites$`Oral Cavity And Pharynx`$Nasopharynx$recode <- 20060
oral6 <- oral_cavity_pharynx$AddChild("Tonsil")
sites$`Oral Cavity And Pharynx`$Tonsil$recode <- 20070
oral7 <- oral_cavity_pharynx$AddChild("Oropharynx")
sites$`Oral Cavity And Pharynx`$Oropharynx$recode <- 20080
oral8 <- oral_cavity_pharynx$AddChild("Hypopharynx")
sites$`Oral Cavity And Pharynx`$Hypopharynx$recode <- 20090
oral9 <- oral_cavity_pharynx$AddChild("Other Oral Cavity And Pharynx")
sites$`Oral Cavity And Pharynx`$`Other Oral Cavity And Pharynx`$recode <- 20100

# 2. Digestive System
digestive_system <- sites$AddChild("Digestive System")
digestive <- digestive_system$AddChild("Esophagus")
sites$`Digestive System`$Esophagus$recode <- 21010
# FindNode(node = sites, name = 'Esophagus')$recode
digestive <- digestive_system$AddChild("Stomach")
sites$`Digestive System`$Stomach$recode <- 21020
digestive <- digestive_system$AddChild("Small Intestine")
sites$`Digestive System`$`Small Intestine`$recode <- 21030

# 2.1 Colon And Rectum
colon_and_rectum <- digestive_system$AddChild("Colon And Rectum")

# 2.1.1 Colon excluding Rectum
colon_exc_rectum <- colon_and_rectum$AddChild("Colon Excluding Rectum")
colon <- colon_exc_rectum$AddChild("Cecum")
sites$`Digestive System`$`Colon And Rectum`$`Colon Excluding Rectum`$Cecum$recode <- 21041
colon1 <- colon_exc_rectum$AddChild("Appendix")
sites$`Digestive System`$`Colon And Rectum`$`Colon Excluding Rectum`$Appendix$recode <- 21042
colon2 <- colon_exc_rectum$AddChild("Ascending Colon")
sites$`Digestive System`$`Colon And Rectum`$`Colon Excluding Rectum`$`Ascending Colon`$recode <- 21043
colon3 <- colon_exc_rectum$AddChild("Hepatic Flexure")
sites$`Digestive System`$`Colon And Rectum`$`Colon Excluding Rectum`$`Hepatic Flexure`$recode <- 21044
colon4 <- colon_exc_rectum$AddChild("Transverse Colon")
sites$`Digestive System`$`Colon And Rectum`$`Colon Excluding Rectum`$`Transverse Colon`$recode <- 21045
colon5 <- colon_exc_rectum$AddChild("Splenic Flexure")
sites$`Digestive System`$`Colon And Rectum`$`Colon Excluding Rectum`$`Splenic Flexure`$recode <- 21046
colon6 <- colon_exc_rectum$AddChild("Descending Colon")
sites$`Digestive System`$`Colon And Rectum`$`Colon Excluding Rectum`$`Descending Colon`$recode <- 21047
colon7 <- colon_exc_rectum$AddChild("Sigmoid Colon")
sites$`Digestive System`$`Colon And Rectum`$`Colon Excluding Rectum`$`Sigmoid Colon`$recode <- 21048
colon8 <- colon_exc_rectum$AddChild("Large Intestine, NOS")
sites$`Digestive System`$`Colon And Rectum`$`Colon Excluding Rectum`$`Large Intestine, NOS`$recode <- 21049 

# 2.1.2 Rectum And Rectosigmoid Junction
rectum_and_rectosig_junc <- colon_and_rectum$AddChild("Rectum And Rectosigmoid Junction")
rec <- rectum_and_rectosig_junc$AddChild("Rectosigmoid Junction")
sites$`Digestive System`$`Colon And Rectum`$`Rectum And Rectosigmoid Junction`$`Rectosigmoid Junction`$recode <- 21051
rec1 <- rectum_and_rectosig_junc$AddChild("Rectum")
sites$`Digestive System`$`Colon And Rectum`$`Rectum And Rectosigmoid Junction`$`Rectum`$recode <- 21052
rec2 <- rectum_and_rectosig_junc$AddChild("Anus, Anal Canal And Anorectum")
sites$`Digestive System`$`Colon And Rectum`$`Rectum And Rectosigmoid Junction`$`Anus, Anal Canal And Anorectum`$recode <- 21060

# 2.2 Liver And Intrahepatic Bile Duct
liver_and_intra_bile_duct <- digestive_system$AddChild("Liver And Intrahepatic Bile Duct")
liver <- liver_and_intra_bile_duct$AddChild("Liver")
sites$`Digestive System`$`Liver And Intrahepatic Bile Duct`$Liver$recode <- 21071
liver1 <- liver_and_intra_bile_duct$AddChild("Intrahepatic Bile Duct")
sites$`Digestive System`$`Liver And Intrahepatic Bile Duct`$`Intrahepatic Bile Duct`$recode <- 21072
liver2 <- liver_and_intra_bile_duct$AddChild("Gallbladder")
sites$`Digestive System`$`Liver And Intrahepatic Bile Duct`$Gallbladder$recode <- 21080
liver3 <- liver_and_intra_bile_duct$AddChild("Other Biliary")
sites$`Digestive System`$`Liver And Intrahepatic Bile Duct`$`Other Biliary`$recode <- 21090
liver4 <- liver_and_intra_bile_duct$AddChild("Pancreas")
sites$`Digestive System`$`Liver And Intrahepatic Bile Duct`$Pancreas$recode <- 21100
liver5 <- liver_and_intra_bile_duct$AddChild("Retroperitoneum")
sites$`Digestive System`$`Liver And Intrahepatic Bile Duct`$Retroperitoneum$recode <- 21110 
liver6 <- liver_and_intra_bile_duct$AddChild("Peritoneum, Omentum And Mesentery")
sites$`Digestive System`$`Liver And Intrahepatic Bile Duct`$`Peritoneum, Omentum and Mesentery`$recode <- 21120
liver7 <- liver_and_intra_bile_duct$AddChild("Other Digestive Organs")
sites$`Digestive System`$`Liver And Intrahepatic Bile Duct`$`Other Digestive Organs`$recode <- 21130

# 3. Respiratory System
respiratory_system <- sites$AddChild("Respiratory System")
respiratory <- respiratory_system$AddChild("Nose, Nasal Cavity And Middle Ear")
sites$`Respiratory System`$`Nose, Nasal Cavity And Middle Ear`$recode <- 22010
respiratory1 <- respiratory_system$AddChild("Larynx")
sites$`Respiratory System`$Larynx$recode <- 22020
respiratory2 <- respiratory_system$AddChild("Lung And Bronchus")
sites$`Respiratory System`$`Lung And Bronchus`$recode <- 22030
respiratory3 <- respiratory_system$AddChild("Pleura")
sites$`Respiratory System`$Pleura$recode <- 22050
respiratory4 <- respiratory_system$AddChild("Trachea, Mediastinum And Other Respiratory Organs")
sites$`Respiratory System`$`Trachea, Mediastinum And Other Respiratory Organs`$recode <- 22060

# 4. Bones And Joints
bones_and_joints <- sites$AddChild("Bones And Joints")
sites$`Bones And Joints`$recode <- 23000

# 5. Soft Tissue including Heart
soft_tissue_inc_heart <- sites$AddChild("Soft Tissue including Heart")
sites$`Soft Tissue including Heart`$recode <- 24000

# 6. Skin excluding Basal And Squamous
skin_exc_basal_squa <- sites$AddChild("Skin excluding Basal And Squamous")
skin <- skin_exc_basal_squa$AddChild("Melanoma of the Skin")
sites$`Skin excluding Basal And Squamous`$`Melanoma of the Skin`$recode <- 25010
skin1 <- skin_exc_basal_squa$AddChild("Other Non-Epithelial Skin")
sites$`Skin excluding Basal And Squamous`$`Other Non-Epithelial Skin`$recode <- 25020

# 7. Breast
breast <- sites$AddChild("Breast")
sites$Breast$recode <- 26000

# 8. Female Genital System
female_gen_system <- sites$AddChild("Female Genital System")
female <- female_gen_system$AddChild("Cervix Uteri")
sites$`Female Genital System`$`Cervix Uteri`$recode <- 27010
female1 <- female_gen_system$AddChild("Ovary")
sites$`Female Genital System`$Ovary$recode <- 27040
female2 <- female_gen_system$AddChild("Vagina")
sites$`Female Genital System`$Vagina$recode <- 27050
female3 <- female_gen_system$AddChild("Vulva")
sites$`Female Genital System`$Vulva$recode <- 27060
female4 <- female_gen_system$AddChild("Other Female Genital Organs")
sites$`Female Genital System`$`Other Female Genital Organs`$recode <- 27070
# 8.1 Corpus And Uterus, NOS
corpus_and_uterus <- female_gen_system$AddChild("Corpus And Uterus, NOS")
corpus <- corpus_and_uterus$AddChild("Corpus Uteri")
sites$`Female Genital System`$`Corpus And Uterus, NOS`$`Corpus Uteri`$recode <- 27020
corpus1 <- corpus_and_uterus$AddChild("Uterus, NOS")
sites$`Female Genital System`$`Corpus And Uterus, NOS`$`Uterus, NOS`$recode <- 27030

# 9. Male Genital System
male_gen_system <- sites$AddChild("Male Genital System")
male <- male_gen_system$AddChild("Prostate")
sites$`Male Genital System`$Prostate$recode <- 28010
male1 <- male_gen_system$AddChild("Testis")
sites$`Male Genital System`$Testis$recode <- 28020
male2 <- male_gen_system$AddChild("Penis")
sites$`Male Genital System`$Penis$recode <- 28030
male3 <- male_gen_system$AddChild("Other Male Genital Organs")
sites$`Male Genital System`$`Other Male Genital Organs`$recode <- 28040

# 10. Urinary System
urinary_system <- sites$AddChild("Urinary System")
urinary <- urinary_system$AddChild("Urinary Bladder")
sites$`Urinary System`$`Urinary Bladder`$recode <- 29010
urinary1 <- urinary_system$AddChild("Kidney And Renal Pelvis")
sites$`Urinary System`$`Kidney And Renal Pelvis`$recode <- 29020
urinary2 <- urinary_system$AddChild("Ureter")
sites$`Urinary System`$Ureter$recode <- 29030
urinary3 <- urinary_system$AddChild("Other Urinary Organs")
sites$`Urinary System`$`Other Urinary Organs`$recode <- 29040

# 11. Eye And Orbit
eye_and_orbit <- sites$AddChild("Eye And Orbit")
sites$`Eye And Orbit`$recode <- 3000

# 12. Brain And Other Nervous System
brain_and_ons <- sites$AddChild("Brain And Other Nervous System")
brain <- brain_and_ons$AddChild("Brain")
sites$`Brain And Other Nervous System`$Brain$recode <- 31010
brain1 <- brain_and_ons$AddChild("Cranial Nerves Other Nervous System") 
sites$`Brain And Other Nervous System`$`Cranial Nerves Other Nervous System`$recode <- 31040

# 13. Endocrine System
endocrine_system <- sites$AddChild("Endocrine System")
endocrine <- endocrine_system$AddChild("Thyroid")
sites$`Endocrine System`$Thyroid$recode <- 32010
endocrine1 <- endocrine_system$AddChild("Other Endocrine including Thymus")
sites$`Endocrine System`$`Other Endocrine including Thymus`$recode <- 32020

# 14. Lymphoma
lymphoma <- sites$AddChild("Lymphoma")

# 14.1 Hodgkin Lymphoma
hodgkin_lymphoma <- lymphoma$AddChild("Hodgkin Lymphoma")
hodgkin <- hodgkin_lymphoma$AddChild("Hodgkin - Nodal")
sites$Lymphoma$`Hodgkin Lymphoma`$`Hodgkin - Nodal`$recode <- 33011
hodgkin2 <- hodgkin_lymphoma$AddChild("Hodgkin - Extranodal")
sites$Lymphoma$`Hodgkin Lymphoma`$`Hodgkin - Extranodal`$recode <- 33012

# 14.2 Non-Hodgkin Lymphoma
non_hodgkin_lymphoma <- lymphoma$AddChild("Non-Hodgkin Lymphoma")
non_hodgkin <- non_hodgkin_lymphoma$AddChild("NHL - Nodal")
sites$Lymphoma$`Non-Hodgkin Lymphoma`$`NHL - Nodal`$recode <- 33041
non_hodgkin2 <- non_hodgkin_lymphoma$AddChild("NHL - Extranodal")
sites$Lymphoma$`Non-Hodgkin Lymphoma`$`NHL - Extranodal`$recode <- 33042
# 15. Myeloma
myeloma <- sites$AddChild("Myeloma")
sites$Myeloma$recode <- 34000

# 16. Leukemia
leukemia <- sites$AddChild("Leukemia")

# 16.1 Lymphocytic Leukemia
lymph_leukemia <- leukemia$AddChild("Lymphocytic Leukemia")
lymph_leuke <- lymph_leukemia$AddChild("Acute Lymphocytic Leukemia")
sites$Leukemia$`Lymphocytic Leukemia`$`Acute Lymphocytic Leukemia`$recode <- 35011
lymph_leuke1 <- lymph_leukemia$AddChild("Chronic Lymphocytic Leukemia")
sites$Leukemia$`Lymphocytic Leukemia`$`Chronic Lymphocytic Leukemia`$recode <- 35012
lymph_leuke2 <- lymph_leukemia$AddChild("Other Lymphocytic Leukemia")
sites$Leukemia$`Lymphocytic Leukemia`$`Other Lymphocytic Leukemia`$recode <- 35013

# 16.2 Myeloid And Monocytic Leukemia
myeloid_leukemia <- leukemia$AddChild("Myeloid And Monocytic Leukemia")
myeloid <- myeloid_leukemia$AddChild("Acute Myeloid Leukemia")
sites$Leukemia$`Myeloid And Monocytic Leukemia`$`Acute Myeloid Leukemia`$recode <- 35021
myeloid1 <- myeloid_leukemia$AddChild("Acute Monocytic Leukemia")
sites$Leukemia$`Myeloid And Monocytic Leukemia`$`Acute Monocytic Leukemia`$recode <- 35031
myeloid2 <- myeloid_leukemia$AddChild("Chronic Myeloid Leukemia")
sites$Leukemia$`Myeloid And Monocytic Leukemia`$`Chronic Myeloid Leukemia`$recode <- 35022
myeloid3 <- myeloid_leukemia$AddChild("Other Myeloid/Monocytic Leukemia")
sites$Leukemia$`Myeloid And Monocytic Leukemia`$`Other Myeloid/Monocytic Leukemia`$recode <- 35023
  
# 16.3 Other Leukemia
other_leukemia <- leukemia$AddChild("Other Leukemia")
other1 <- other_leukemia$AddChild("Other Acute Leukemia")
sites$Leukemia$`Other Leukemia`$`Other Acute Leukemia`$recode <- 35041
other2 <- other_leukemia$AddChild("Aleukemic, Subleukemic And NOS")
sites$Leukemia$`Other Leukemia`$`Aleukemic, Subleukemic And NOS`$recode <- 35043
# 17. Mesothelioma
mesothelioma <- sites$AddChild("Mesothelioma")
sites$Mesothelioma$recode <- 36010

# 18. Kaposi Sarcoma
kaposi_sarcoma <- sites$AddChild("Kaposi Sarcoma")
sites$`Kaposi Sarcoma`$recode <- 36020

# 19. Miscellaneous
miscellaneous <- sites$AddChild("Miscellaneous")
sites$Miscellaneous$recode <- 37000

# 20. Invalid
invalid <- sites$AddChild("Invalid")
sites$Invalid$recode <- 99999

# print(sites, 'recode')

# setwd('/home/mribeirodantas/dev/vidente/R/')
# save(sites, file = 'sysdata.rda')
