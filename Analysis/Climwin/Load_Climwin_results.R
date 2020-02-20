### Load all the resulting climwin files in one central script
### Making sure all other scripts use the same files/winners


### Import climwin results ----------------------------------------------------------------------------------------

### HEQU
Hs <- readRDS("Results/Climwin/HEQU_s_month_result.rds")
Hg <- readRDS("Results/Climwin/HEQU_g_month_result.rds")
Hfp <- readRDS("Results/Climwin/HEQU_fp_month_result.rds")
Hfn <- readRDS("Results/Climwin/HEQU_fn_month_result.rds")

Hsr <- readRDS("Results/Climwin/HEQU_s_month_random.rds")
Hgr <- readRDS("Results/Climwin/HEQU_g_month_random.rds")
Hfpr <- readRDS("Results/Climwin/HEQU_fp_month_random.rds")
Hfnr <- readRDS("Results/Climwin/HEQU_fn_month_random.rds")


### CRFL
Cs <- readRDS("Results/Climwin/CRFL_s_month_result.rds")
Cg <- readRDS("Results/Climwin/CRFL_g_month_result.rds")
Cfp <- readRDS("Results/Climwin/CRFL_fp_month_result.rds")
Cfn <- readRDS("Results/Climwin/CRFL_fn_month_result.rds")

Csr <- readRDS("Results/Climwin/CRFL_s_month_random.rds")
Cgr <- readRDS("Results/Climwin/CRFL_g_month_random.rds")
Cfpr <- readRDS("Results/Climwin/CRFL_fp_month_random4.rds")
Cfnr <- readRDS("Results/Climwin/CRFL_fn_month_random.rds")

### OPIM
Os <- readRDS("Results/Climwin/OPIM_s_month_result.rds")
Og <- readRDS("Results/Climwin/OPIM_g_month_result.rds")
Ofp <- readRDS("Results/Climwin/OPIM_fp_month_result.rds")
Ofn <- readRDS("Results/Climwin/OPIM_fn_month_result.rds")

Osr <- readRDS("Results/Climwin/OPIM_s_month_random.rds")
Ogr <- readRDS("Results/Climwin/OPIM_g_month_random.rds")
Ofpr <- readRDS("Results/Climwin/OPIM_fp_month_random.rds")
Ofnr <- readRDS("Results/Climwin/OPIM_fn_month_random.rds")

### FRSP
Fs <- readRDS("Results/Climwin/FRSP_s_month_result.rds")
Fg <- readRDS("Results/Climwin/FRSP_g_month_result_5yrs.rds")
Ffp <- readRDS("Results/Climwin/FRSP_fp_month_result.rds")
Ffn <- readRDS("Results/Climwin/FRSP_fn_month_result.rds")

Fsr <- readRDS("Results/Climwin/FRSP_s_month_random.rds")
Fgr <- readRDS("Results/Climwin/FRSP_g_month_random_5yrs.rds")
Ffpr <- readRDS("Results/Climwin/FRSP_fp_month_random.rds")
Ffnr <- readRDS("Results/Climwin/FRSP_fn_month_random.rds")

### Set winners ---------------------------------------------------------------------------------------------------------------------------------------------
Hsurv <- 13
Hgrowth <- 2
HpFlwr <- 16
HnFlwr <-12 

Fsurv <- 14
Fgrowth <- 10
FpFlwr <- 14
FnFlwr <- 2

Csurv <- 16
Cgrowth <- 1
CpFlwr <- 4
CnFlwr <- 4

Osurv <- 3
Ogrowth <- 12
OpFlwr <- 2
OnFlwr <- 3
