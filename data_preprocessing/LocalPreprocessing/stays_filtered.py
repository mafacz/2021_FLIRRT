## Clean the data / change errors 

import pandas as pd
import numpy as np
import dask as dd
import glob

############### 
## Load data ##

HiRID = False
print(f"HiRID = ",HiRID)

if HiRID:
    filepath_stay = glob.glob("C:\\Programming\\sources\\FLIRRT\\HiRID\\2025-06-05//stays//*parquet")
else:
    filepath_stay = glob.glob("C:\\Programming\\sources\\FLIRRT\\AmsterdamUMCDb\\2025-06-05//stays//*parquet")
dataframes_stay = [pd.read_parquet(file) for file in filepath_stay]
stays_df = pd.concat(dataframes_stay, ignore_index= True)

#rename columns to standardise format -> now it is called "patid" in all three dataframes, and correction of lenght to length
stays_df = stays_df.rename(columns={"patientid":"patid", "lenght_of_stay":"length_of_stay", "session_lenght":"session_length"}) 
#drop column "counter", since no additional information is gained.
stays_df = stays_df.drop(columns=["counter"])

############################################################################################################################
# Exclude patients that did not get CRRT (vm5025): 
if HiRID:
   stays_df = stays_df[~(stays_df["patid"]==76195)] #4 in HiRID is plasmapheresis
else:
    stays_df = stays_df[~(stays_df["patid"].isin([6177,22303]))] #6 in AmsterdamUMCDb is MPS
############################################################################################################################

###############
## Filtering ##

##Exclusion of patients with ICU death < 24h:
stays_df_LOS = stays_df[~((stays_df["length_of_stay"]<=(24*60*60)) & (stays_df["outcome_icu_death"]==True))] #LOS is in seconds

##Exclude patients with CRRT for less than 24h: 
stays_df_CRRTlength = stays_df_LOS[stays_df_LOS["session_length"] > (24*60)] #session length is in minutes

##Exclude patients with unknown 28-day outcome: 
stays_df_knownoutcome = stays_df_CRRTlength[(stays_df_CRRTlength["outcome_death_28d"].isin([0,1]))] #Unknown is -1 or -2 

##Exclude patients with unrealistic / missing weight: 
stays_df_withweight = stays_df_knownoutcome[stays_df_knownoutcome["weight_at_admission"]>20]

##correct patients where weight and height are mixed up:
mixed_up_height_weight = stays_df_withweight[(stays_df_withweight["height_at_admission"] < 100) & (stays_df_withweight["weight_at_admission"]>130)].copy()
mixed_up_height_weight["corrected_weight"]=mixed_up_height_weight["height_at_admission"]
mixed_up_height_weight["corrected_height"]=mixed_up_height_weight["weight_at_admission"]
mixed_up_height_weight=mixed_up_height_weight.drop(columns=["height_at_admission", "weight_at_admission"]).rename(columns={"corrected_weight":"weight_at_admission", "corrected_height":"height_at_admission"})
stays_df_withweight.loc[((stays_df_withweight["height_at_admission"] < 100) & (stays_df_withweight["weight_at_admission"]>130))] = mixed_up_height_weight

##delete remaining patients with wrong height:
stays_df_withheight = stays_df_withweight[stays_df_withweight["height_at_admission"]>100]
stays_df_filtered = stays_df_withheight

############## Add a row with the source of the data / add the suffix to the patid to make mischmasch impossible
if HiRID:
    stays_df_filtered["source"] = "HiRID"
    stays_df_filtered['patid'] =  stays_df_filtered['patid'].astype(str) + '_HiRID'  

else:
    stays_df_filtered["source"] = "AmsterdamUMCDb"
    stays_df_filtered["outcome_icu_death"] = stays_df_filtered["outcome_icu_death"].astype(bool) ##Change to True and False...
    stays_df_filtered['patid'] =  stays_df_filtered['patid'].astype(str) + '_Amsterdam'

##############
######
##

if HiRID:
    stays_df_filtered.to_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\HiRID_preprocessed\\stays_filtered_HiRID.csv", index=False)
else:
    stays_df_filtered.to_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\UMCDb_preprocessed\\stays_filtered_AmsterdamUMCDb.csv", index=False)

if HiRID:
    print("HiRID:")
else:
    print("AmsterdamUMCDb:")

print("Total cohort with CRRT:", len(stays_df))
print("Excluded because of ICU death < 24h:", len(stays_df)-len(stays_df_LOS))
print("Excluded because of CRRT for < 24h: ", len(stays_df_LOS)-len(stays_df_CRRTlength))
print("Excluded because of unknown 28-day outcome:", len(stays_df_CRRTlength) - len(stays_df_knownoutcome))
print("Excluded because of incorrect/missing weight:", len(stays_df_knownoutcome) - len(stays_df_withweight))
print("Excluded because of incorrect/missing height:", len(stays_df_withweight) - len(stays_df_withheight))
print("Total included cohort:", len(stays_df_filtered))
print("Total excluded patients:", len(stays_df) - len(stays_df_filtered))

##
######
##############






