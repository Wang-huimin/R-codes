library(readxl)
library(dplyr)
library(tidyr)
library(eeptools)
library(reshape2)
library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### Read the Data ######

data_pelengkap_aktif <- read_excel("Raw Data/data pelengkap aktif.xls")
data_pelengkap_aktif$LULUS <- 0
data_pelengkap_lulus <- read_excel("Raw Data/data pelengkap lulus.xls")
data_pelengkap_lulus$LULUS <- 1
  
data_masih_aktif <- read_excel("Raw Data/data_msh_aktif.xlsx")
data_masih_aktif$LULUS <- 0
  
data_sdh_lulus <- read_excel("Raw Data/data_sdh lulus.xlsx")
data_sdh_lulus$BOBOT <- NULL
data_sdh_lulus$LULUS <- 1
  
dari_daa <- read.csv("Raw Data/Data FEB 2009-2019.csv")

data_income <- read_excel("Raw Data/mhs_data.xlsx") 


##### Data Cleaning #####

  ### Dari DAA

  dari_daa <- as_tibble(dari_daa)
  dari_daa <- dari_daa %>%
    separate(Jurusan.SMTA, into = c("SMA", "jurusan"),
             sep = " (?=[^ ]+$)")
  dari_daa$NIU <- as.character(dari_daa$NIU)

  ### Dari SIFE part 1 (Pelengkap)

  today <- Sys.Date()
  df_pelengkap <- rbind(data_pelengkap_lulus,data_pelengkap_aktif)
  df_pelengkap <- select(df_pelengkap, -c("KODE_PRODI","JALUR","PROP_ASAL","KOTA_ASAL"))
  df_pelengkap$Study_Duration <- ifelse(df_pelengkap$LULUS==1,
                                        df_pelengkap$TGL_LULUS-df_pelengkap$TGL_MASUK,
                                        as.POSIXct(today)-df_pelengkap$TGL_MASUK)
  df_pelengkap$KURIKULUM <- as.factor(df_pelengkap$KURIKULUM)
  df_pelengkap$NIU <- as.character(df_pelengkap$NIU)

  ### Dari SIFE part 2 (Nilai)

  names(data_masih_aktif) <- names(data_sdh_lulus)
  df_main <- rbind(data_sdh_lulus,data_masih_aktif)
  
  df_main <- df_main %>% 
    mutate(MAJOR = ifelse(KODE_PRODI==1,"Ilmu_Ekonomi Reg",
                          ifelse(KODE_PRODI==2,"Manajemen Reg",
                                 ifelse(KODE_PRODI==3,"Akuntansi Reg",
                                        ifelse(KODE_PRODI==31,"Ilmu_Ekonomi IUP",
                                               ifelse(KODE_PRODI==32,"Manajemen IUP",
                                                      ifelse(KODE_PRODI==33,"Akuntansi IUP",NA)))))))
  
  df_main <- df_main %>%
    separate(MAJOR, into = c("JURUSAN", "PROGRAM"),
             sep = " (?=[^ ]+$)")
  
  df_main <- df_main %>% 
    mutate(MK_QUANT = ifelse((grepl("Matematika",NAMA_MK)|grepl("Statistika",NAMA_MK)|
                                grepl("Ekonometrika",NAMA_MK)|grepl("Kuantitatif",NAMA_MK)|
                                grepl("Mathematics",NAMA_MK)|grepl("Statistics",NAMA_MK)|
                                grepl("Econometrics",NAMA_MK)|grepl("Quantitative",NAMA_MK))
                              ,1,0))
  
  df_main %>% 
    group_by(NILAI) %>% summarise(n=n())
  
  df_main <- df_main %>% 
    mutate(NILAI = ifelse((grepl("P",NILAI)|grepl("4",NILAI)|
                            grepl("3",NILAI)|grepl("2",NILAI)|
                             grepl("I",NILAI)|grepl("T",NILAI)|
                             grepl("W",NILAI)|grepl("6",NILAI)),NA,NILAI))
  
  grade_levels <- c("A+","A","A-","A/B","B+","B","B-","B/C","C+","C","C-","C/D","D+","D","E")
  grade_nums <- c(4, 4, 3.75, 3.5, 3.25, 3, 2.75, 2.5, 2.25, 2, 1.75, 1.5, 1.25, 1, 0)
  grade_list <- c(grade_levels=grade_nums)
  grade_list
  
  df_main$NILAI_NUM <- factor(df_main$NILAI, levels=grade_levels, labels=grade_nums)
  df_main$NILAI_NUM <- as.numeric(paste(df_main$NILAI_NUM))
  
  # Gather all the grades!
  
  df_main <- as.data.frame(df_main)
  df_GPA_agg_only <- df_main %>% 
    group_by(NIU) %>% summarize(GPA_Agg = mean(IPK))
  df_collapsed <- df_main %>% 
    group_by(NIU,MK_QUANT) %>% summarize(GPA_Class_mean = mean(NILAI_NUM,na.rm=T),
                                         GPA_Class_sd = sd(NILAI_NUM,na.rm=T),
                                         Batch = first(ANGKATAN),
                                         D_Graduated = first(LULUS),
                                         Major = first(JURUSAN),
                                         Program = first(PROGRAM))
  
  df_collapsed_wide <- dcast(setDT(df_collapsed), ... ~ MK_QUANT, 
                             value.var = c("GPA_Class_mean", "GPA_Class_sd"), drop=F) 
  df_collapsed_wide <- subset(df_collapsed_wide, GPA_Class_mean_0>0)
  names(df_collapsed_wide)[6:9] <- c("GPA_Qual_mean","GPA_Quant_mean","GPA_Qual_sd","GPA_Quant_sd")
  
  df_GPA_agg_only$NIU <- as.character(df_GPA_agg_only$NIU)
  df_collapsed_wide$NIU <- as.character(df_collapsed_wide$NIU)
  df_collapsed_wide <- merge(df_collapsed_wide,df_GPA_agg_only,by="NIU")
  df_collapsed_wide$NIU <- as.character(df_collapsed_wide$NIU) #need to re-do the above command due to change of type after the merge!

  ### Dari SIFE part 3 (Demografi)
  
  data_income$JENIS_KELAMIN <- as.factor(data_income$JENIS_KELAMIN)
  data_income$PEKERJAAN_AYAH <- as.factor(data_income$PEKERJAAN_AYAH)
  data_income$PEKERJAAN_IBU <- as.factor(data_income$PEKERJAAN_IBU)
  
  data_income <- data_income %>% 
    rename(
      Gender = JENIS_KELAMIN,
      Date_birth = TGL_LAHIR,
      Job_Father = PEKERJAAN_AYAH,
      Job_Mother = PEKERJAAN_IBU,
      Family_Income = PENGHASILAN_ORTU
    )
  
##### Merging One-to-One #####

df_merge0 <- merge(df_collapsed_wide, dari_daa, by="NIU")
df_merge1 <- merge(data_income, df_merge0, by="NIU")
df_merge <- merge(df_merge1, df_pelengkap, by="NIU")
  
df_merge <- df_merge %>% 
  mutate(jurusan = ifelse(jurusan!="IPA"&jurusan!='IPS',"Lainnya",jurusan))
df_merge[is.na(df_merge$jurusan),]$jurusan<- "Lainnya"
df_merge <- select(df_merge, -c("NAMA","LULUS","No.Peserta","NIM","Nama","Fakultas","Program.Studi",
                                "Angkatan","NPSN"))

df_merge <- df_merge %>% 
  rename(
    Admission_type = Jalur,
    HS_type = SMA,
    HS_program = jurusan,
    HS_name = Nama.SMTA,
    HS_regency = Kabupaten.SMTA,
    HS_province = Propinsi.SMTA,
    Date_start = TGL_MASUK,
    Date_graduate = TGL_LULUS,
    Thesis_spv = PEMBIMBING_SKRIPSI,
    Curriculum = KURIKULUM
  )

df_merge$Gender <- as.factor(df_merge$Gender)
df_merge$D_Graduated <- as.factor(df_merge$D_Graduated)
df_merge$HS_type <- as.factor(df_merge$HS_type)
df_merge$Batch <- as.factor(df_merge$Batch)
df_merge$HS_program <- as.factor(df_merge$HS_program)
df_merge$Major <- as.factor(df_merge$Major)
df_merge$Program <- as.factor(df_merge$Program)

df_merge$Age_graduation <- (ifelse(df_merge$D_Graduated==1,
                                  df_merge$Date_graduate-df_merge$Date_birth,NA))/365
df_merge$Age_startprog <- (df_merge$Date_start-df_merge$Date_birth)/365

df_merge$D_tested_admission <- as.factor(ifelse(df_merge$Admission_type=="SNMPTN" | 
                                        df_merge$Admission_type=="SNMPTN Undangan" |
                                        df_merge$Admission_type=="PBS", "No", "Yes"))

names(df_merge)

write.csv(df_merge, "Cleaned Data.csv")

summary(df_merge)
sapply(df_merge, class)
