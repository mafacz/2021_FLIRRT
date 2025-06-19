import pandas as pd
import numpy as np

HiRID = False
AmsterdamUMCDb = False
Total = True

if HiRID:
    suffix = "_HiRID"
    stays_filtered = pd.read_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\HiRID_preprocessed\\stays_filtered_HiRID.csv")
    regular_df_UFperkg = pd.read_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Final\\regular_UFperkg_HiRID.csv")
elif AmsterdamUMCDb: 
    suffix = "_AmsterdamUMCDb"
    stays_filtered = pd.read_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\UMCDb_preprocessed\\stays_filtered_AmsterdamUMCDb.csv")
    regular_df_UFperkg = pd.read_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Final\\regular_UFperkg_AmsterdamUMCdb.csv")
elif Total:
    suffix = "_Total"
    stays_filtered = pd.read_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Total_preprocessed\\stays_filtered_Total.csv")
    regular_df_UFperkg = pd.read_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Final\\regular_UFperkg_Total.csv")
    
## Create a df for fluid_overload with categories of 5, 7 and 10 % of kg body weight. 
fluidoverload = regular_df_UFperkg.groupby("patid")["fluidoverload"].mean().reset_index(name="fluidoverload")
fluidoverload["fluidoverload_5"] = np.where(fluidoverload["fluidoverload"] >= 5, 1, 0)
fluidoverload["fluidoverload_7"] = np.where(fluidoverload["fluidoverload"] >= 7, 1, 0)
fluidoverload["fluidoverload_10"] = np.where(fluidoverload["fluidoverload"] >= 10, 1, 0)

# SOFA score:
# Filter to first 24h and drop rows with missing SOFA # Get first SOFA score per patient
initial_window = regular_df_UFperkg[regular_df_UFperkg["session_length"] <= 24*60]
initial_window = initial_window.dropna(subset=["sofa_total_24h"])
SOFA_score = initial_window.groupby("patid")["sofa_total_24h"].first().reset_index(name="SOFA_score")

# Merge 
stays_fused = stays_filtered.merge(fluidoverload, on="patid", how="left") \
                            .merge(SOFA_score, on="patid", how="left")

stays_fused.to_csv(f"C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Final\\stays_fused{suffix}.csv", index=False)

print(suffix)
