preprocessSEER("/home/mribeirodantas/Documentos/PhD data/SEER 2018 Submission/SEER_1973_2015_TEXTDATA.d04102018/SEER_1973_2015_TEXTDATA/incidence/read.seer.research.nov17.sas")

#### read the file with the start+end positions----
paths = c("/home/mribeirodantas/Documentos/PhD data/SEER 2018 Submission/SEER_1973_2015_TEXTDATA.d04102018/SEER_1973_2015_TEXTDATA/incidence/yr1973_2015.seer9/BREAST.TXT",
          "/home/mribeirodantas/Documentos/PhD data/SEER 2018 Submission/SEER_1973_2015_TEXTDATA.d04102018/SEER_1973_2015_TEXTDATA/incidence/yr1992_2015.sj_la_rg_ak/BREAST.TXT",
          "/home/mribeirodantas/Documentos/PhD data/SEER 2018 Submission/SEER_1973_2015_TEXTDATA.d04102018/SEER_1973_2015_TEXTDATA/incidence/yr2000_2015.ca_ky_lo_nj_ga/BREAST.TXT",
          "/home/mribeirodantas/Documentos/PhD data/SEER 2018 Submission/SEER_1973_2015_TEXTDATA.d04102018/SEER_1973_2015_TEXTDATA/incidence/yr2005.lo_2nd_half/BREAST.TXT")

readSEER(paths, c(2007:2015), site="Breast")

