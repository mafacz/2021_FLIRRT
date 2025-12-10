import pandas as pd
import numpy as np
import os
import json
import socket
import glob

HiRID = False
AmsterdamUMCDb = True
Total = True

# Load configuration
with open(os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', 'path.config'))) as f:
    config = json.load(f)

output_root = config[socket.gethostname()]["output_root"]

if HiRID:
    suffix = "_HiRID"
    stays_filtered = pd.read_csv(os.path.join(output_root, "stays_filtered_HiRID.csv"))
    regular_df_UFperkg = pd.read_csv(os.path.join(output_root, "Final", "regular_UFperkg_HiRID.csv"))
elif AmsterdamUMCDb: 
    suffix = "_AmsterdamUMCDb"
    stays_filtered = pd.read_csv(os.path.join(output_root, "stays_filtered_AmsterdamUMCDb.csv"))
    regular_df_UFperkg = pd.read_csv(os.path.join(output_root, "Final", "regular_UFperkg_AmsterdamUMCdb.csv"))
elif Total:
    suffix = "_Total"
    stays_filtered = pd.read_csv(os.path.join(output_root, "Total_preprocessed", "stays_filtered_Total.csv"))
    regular_df_UFperkg = pd.read_csv(os.path.join(output_root, "Final", "regular_UFperkg_Total.csv"))

print(stays_filtered.columns)
    
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

# Invasive Ventilation status:
# Filter to first 24h and get first Ventilation status per patient (in first 24h after CRRT start)
# If NaN, considered to not have invasive ventilation at start. 
initial_window = regular_df_UFperkg[regular_df_UFperkg["session_length"] <= 24*60]
# Get first non-null value per patient (if available)
inv_vent = (initial_window.groupby("patid")["dm_vent_inv_state"]
    .apply(lambda x: x.dropna().iloc[0] if x.dropna().any() else 0)
    .reset_index(name="invasive_ventilation"))

# Load diagnosis reviews (for HiRID patients, works for HiRID and Total datasets)
diagnosis_reviews = None
# Always try to load reviews, they will only match HiRID patients (with _HiRID suffix)
review_file_pattern = os.path.join(output_root, "DiagnosisData", "reviews_*.csv")
review_files = glob.glob(review_file_pattern)

if review_files:
    # Use the most recent file (based on filename timestamp)
    latest_review_file = sorted(review_files)[-1]
    print(f"Loading diagnosis reviews from: {latest_review_file}")
    
    # Load the reviews
    reviews_df = pd.read_csv(latest_review_file)
    
    # Create patid column from study_id (add _HiRID suffix to match stays_filtered format)
    reviews_df['patid'] = reviews_df['study_id'].astype(str) + '_HiRID'
    
    # Select only the organ failure columns we need
    diagnosis_reviews = reviews_df[['patid', 'has_aki', 'has_eskd', 'has_sepsis', 'has_acute_on_chronic']].copy()
    
    # Convert boolean columns to int (0/1) for consistency
    for col in ['has_aki', 'has_eskd', 'has_sepsis', 'has_acute_on_chronic']:
        diagnosis_reviews[col] = diagnosis_reviews[col].astype(int)
    
    print(f"Loaded {len(diagnosis_reviews)} diagnosis reviews (will match HiRID patients only)")
else:
    print(f"No diagnosis review files found at: {review_file_pattern}")

# Merge 
stays_fused = stays_filtered.merge(fluidoverload, on="patid", how="left") \
                            .merge(SOFA_score, on="patid", how="left") \
                            .merge(inv_vent, on="patid", how="left")

# Merge diagnosis reviews if available, otherwise create empty columns
if diagnosis_reviews is not None:
    # Left merge: HiRID patients with _HiRID suffix will match and get review data
    # Amsterdam patients with _AmsterdamUMCDb suffix will not match and get NaN
    stays_fused = stays_fused.merge(diagnosis_reviews, on="patid", how="left")
    print(f"Merged diagnosis reviews: {stays_fused[['has_aki', 'has_eskd', 'has_sepsis', 'has_acute_on_chronic']].notna().sum().to_dict()}")
else:
    # If no reviews available, create empty columns for all patients
    stays_fused['has_aki'] = np.nan
    stays_fused['has_eskd'] = np.nan
    stays_fused['has_sepsis'] = np.nan
    stays_fused['has_acute_on_chronic'] = np.nan
    print("No diagnosis reviews loaded - all organ failure columns set to NaN")

print(stays_fused.columns)

stays_fused.to_csv(os.path.join(output_root, "Final", f"stays_fused{suffix}.csv"), index=False)

print(suffix)




