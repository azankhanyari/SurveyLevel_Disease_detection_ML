library(funModeling)
library(dplyr)

head(df_18July_featureS_nhanes,10)

nhanes_factor

freq(df_18July_featureS_nhanes, "Status")

huq020_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'huq020', target = 'Status')

head(huq020_profil,15)

arrange(huq020_profil, -mean_target) %>% head(.)

#keep only those with atleast 1% presense in our data
huq020_high_rep=filter(huq020_profil, perc_rows>0.01) %>% .$huq020


huq051_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'huq051', target = 'Status')
head(huq051_profil)

ridreth3_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'ridreth3', target = 'Status')
head(ridreth3_profil)

dmqmiliz_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dmqmiliz', target = 'Status')
head(dmqmiliz_profil)

dmdborn4_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dmdborn4', target = 'Status')
head(dmdborn4_profil)

dmdcitzn_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dmdcitzn', target = 'Status')
head(dmdcitzn_profil)

dmdeduc2_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dmdeduc2', target = 'Status')
head(dmdeduc2_profil)

dmdmartl_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dmdmartl', target = 'Status')
head(dmdmartl_profil)

dmdhhsiz_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dmdhhsiz', target = 'Status')
head(dmdhhsiz_profil)

dmdfmsiz_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dmdfmsiz', target = 'Status')
head(dmdfmsiz_profil)

dmdhhsza_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dmdhhsza', target = 'Status')
head(dmdhhsza_profil)

dmdhhszb_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dmdhhszb', target = 'Status')
head(dmdhhszb_profil)

dmdhhszb_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dmdhhszb', target = 'Status')
head(dmdhhszb_profil)

dmdhhsze_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dmdhhsze', target = 'Status')
head(dmdhhsze_profil)

dmdhrgnd
dmdhrgnd_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dmdhrgnd', target = 'Status')
head(dmdhrgnd_profil)

dmdhredu
dmdhredu_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dmdhredu', target = 'Status')
head(dmdhredu_profil)

dmdhrmar
dmdhrmar_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dmdhrmar', target = 'Status')
head(dmdhrmar_profil)

indhhin2
indhhin2_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'indhhin2', target = 'Status')
head(indhhin2_profil)

dbq700
dbq700_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dbq700', target = 'Status')
head(dbq700_profil)

dbq197
dbq197_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dbq197', target = 'Status')
head(dbq197_profil)

dlq010
dlq010_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dlq010', target = 'Status')
head(dlq010_profil)

dlq020
dlq020_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dlq020', target = 'Status')
head(dlq020_profil)

dlq050
dlq050_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dlq050', target = 'Status')
head(dlq050_profil)

hiq011
hiq011_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'hiq011', target = 'Status')
head(hiq011_profil)

smd460
smd460_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'smd460', target = 'Status')
head(smd460_profil)

smd470
smd470_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'smd470', target = 'Status')
head(smd470_profil)

inq132
inq132_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'inq132', target = 'Status')
head(inq132_profil)

inq320
inq320_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'inq320', target = 'Status')
head(inq320_profil)

#keep only those with atleast 1% presense in our data
inq320_high_rep=filter(inq320_profil, perc_rows>0.01) %>% .$inq320

df_18July_featureS_nhanes$inq320_new=ifelse(df_18July_featureS_nhanes$inq320 %in% inq320_high_rep, df_18July_featureS_nhanes$inq320, "999")

df_18July_featureS_nhanes$inq320_new <- as.factor(df_18July_featureS_nhanes$inq320_new)

mcq010
mcq010_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'mcq010', target = 'Status')
head(mcq010_profil)

mcq053
mcq053_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'mcq053', target = 'Status')
head(mcq053_profil)

mcq080
mcq080_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'mcq080', target = 'Status')
head(mcq080_profil)

mcq203
mcq203_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'mcq203', target = 'Status')
head(mcq203_profil)

mcq365c
mcq365c_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'mcq365c', target = 'Status')
head(mcq365c_profil)

mcq220
mcq220_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'mcq220', target = 'Status')
head(mcq220_profil)

dpq030
dpq030_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dpq030', target = 'Status')
head(dpq030_profil)

dpq040
dpq040_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dpq040', target = 'Status')
head(dpq040_profil)

dpq050
dpq050_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'dpq050', target = 'Status')
head(dpq050_profil)


paq635
paq635_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'paq635', target = 'Status')
head(paq635_profil)

paq650
paq650_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'paq650', target = 'Status')
head(paq650_profil)

paq665
paq665_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'paq665', target = 'Status')
head(paq665_profil)

paq710
paq710_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'paq710', target = 'Status')
head(paq710_profil)

#reduce catg
df_18July_featureS_nhanes$paq710 <- ifelse(df_18July_featureS_nhanes$paq710 == 5,5,0)

paq715
paq715_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'paq715', target = 'Status')
head(paq715_profil)

df_18July_featureS_nhanes$paq715 <- ifelse(df_18July_featureS_nhanes$paq715 == 8,8,0)

slq030
slq030_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'slq030', target = 'Status')
head(slq030_profil)

slq120
slq120_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'slq120', target = 'Status')
head(slq120_profil)

smq040
smq040_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'smq040', target = 'Status')
head(smq040_profil)

whq225
whq225_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'whq225', target = 'Status')
head(whq225_profil)

whq030
whq030_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'whq030', target = 'Status')
head(whq030_profil)

bpq020
bpq020_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'bpq020', target = 'Status')
head(bpq020_profil)

bpq030
bpq030_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'bpq030', target = 'Status')
head(bpq030_profil)

bpq080
bpq080_profil <- categ_analysis(data = df_18July_featureS_nhanes, input = 'bpq080', target = 'Status')
head(bpq080_profil)

mcq300c <- categ_analysis(data = df_18July_featureS_nhanes, input = 'mcq300c', target = 'Status')
head(mcq300c)

df_18July_featureS_nhanes$mcq300c <- ifelse(df_18July_featureS_nhanes$mcq300c == 9,1,df_18July_featureS_nhanes$mcq300c)


df_final_favtrAnalysis <- df_18July_featureS_nhanes[,-c(92,90,93,107,116,123,131,139,146)]

#visual exam of dendo
dendo_features <- c('drxts080_avg','drxts040_avg','drxtchl_avg','drxtp183_avg','drxtm161_avg','drxttfat_avg',
                    'drxts180_avg','drxtvd_avg','drxtm221_avg','drxtp184_avg','drxtacar_avg','drxtvb12_avg',
                    'drxtniac_avg','drxtfa_avg','drxtret_avg','drxtmois_avg','drxtatoa_avg','drxtfibe_avg','drxtcopp_avg',
                    'drxtmagn_avg','drxtlyco_avg','drxtsodi_avg','drxtphos_avg','drxtcalc_avg','drxtzinc_avg','drxtkcal_avg',
                    'drxtcaff_avg','WaistCircumference','BMI','whd140','mcq080','pad645','pad660','pad675','bpq080','BPSystolic',
                    'mcq300c','alq120q','dmdmartl','sld012','ridageyr.y','IrregularPulse','dlq020','dlq050',
                    'paq710','slq120','mcq053','smq040','smq020','cbd071','indhhin2','dmdeduc2','dbd895','cbd111',
                    'bmxleg','HeartRate','ridreth3','dbd905','Status')

dendo_df <- df_18July_featureS_nhanes[,dendo_features]

