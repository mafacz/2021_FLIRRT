
import pandas as pd
import numpy as np
from pathlib import Path

# Base directory
base_dir = Path("C:/Programming/FLIRRT/FLIRRT_preprocessing")

# Function to load, concatenate, and save
def combine_and_save_csv(
    hirid_path, amsterdam_path, output_path,
    hirid_suffix, amsterdam_suffix, total_suffix, 
    base_prefixes
):
    for base in base_prefixes:
        file_HiRID = pd.read_csv(hirid_path / f"{base}_{hirid_suffix}.csv")
        file_Amsterdam = pd.read_csv(amsterdam_path / f"{base}_{amsterdam_suffix}.csv")
        combined = pd.concat([file_HiRID, file_Amsterdam], ignore_index=True)
        combined.to_csv(output_path / f"{base}_{total_suffix}.csv", index=False)

# Paths
hirid_path = base_dir / "HiRID_preprocessed"
amsterdam_path = base_dir / "UMCDb_preprocessed"
output_path = base_dir / "Total_preprocessed"
output_path.mkdir(parents=True, exist_ok=True)  # Ensure the output folder exists

# STAYS
combine_and_save_csv(
    hirid_path, amsterdam_path, output_path,
    "HiRID", "AmsterdamUMCDb", "Total",
    ["stays_filtered"]
)

# CHANGES
combine_and_save_csv(
    hirid_path, amsterdam_path, output_path,
    "HiRID", "AmsterdamUMCDb", "Total",
    ["changes_preprocessed", "changes_preprocessed_48h", "changes_preprocessed_48h_NUF"]
)

# REGULAR
combine_and_save_csv(
    hirid_path, amsterdam_path, output_path,
    "HiRID", "AmsterdamUMCDb", "Total",
    ["regular_preprocessed", "regular_preprocessed_48h", "regular_preprocessed_48h_NUF"]
)

"""
###########################
###########################
## Load stay of HiRID ##
stays_df_filtered_HiRID = pd.read_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\HiRID_preprocessed\\stays_filtered_HiRID.csv")
## Load stay of AmsterdamUMCDb ##
stays_df_filtered_AmsterdamUMCDb = pd.read_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\UMCDb_preprocessed\\stays_filtered_AmsterdamUMCDb.csv")

## Combine both dataframes and save
stays_df_filtered_Total = pd.concat([stays_df_filtered_HiRID, stays_df_filtered_AmsterdamUMCDb], ignore_index=True)
stays_df_filtered_Total.to_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Total_preprocessed\\stays_filtered_Total.csv", index = False)


###########################
###########################
## Load changes of HiRID ##
changes_df_HiRID = pd.read_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\HiRID_preprocessed\\changes_preprocessed_HiRID.csv")
changes_df_48h_HiRID = pd.read_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\HiRID_preprocessed\\changes_preprocessed_48h_HiRID.csv")
changes_df_48h_NUF_HiRID = pd.read_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\HiRID_preprocessed\\changes_preprocessed_48h_NUF_HiRID.csv")
## Load changes of AmsterdamUMCDb ##
changes_df_AmsterdamUMCDb = pd.read_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\UMCDb_preprocessed\\changes_preprocessed_AmsterdamUMCDb.csv")
changes_df_48h_AmsterdamUMCDb = pd.read_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\UMCDb_preprocessed\\changes_preprocessed_48h_AmsterdamUMCDb.csv")
changes_df_48h_NUF_AmsterdamUMCDb= pd.read_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\UMCDb_preprocessed\\changes_preprocessed_48h_NUF_AmsterdamUMCDb.csv")

## Combine the dataframes and save
changes_df_Total = pd.concat([changes_df_HiRID, changes_df_AmsterdamUMCDb], ignore_index=True)
changes_df_Total.to_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Total_preprocessed\\changes_preprocessed_Total.csv", index=False)
changes_df_48h_Total = pd.concat([changes_df_48h_HiRID, changes_df_48h_AmsterdamUMCDb], ignore_index=True)
changes_df_48h_Total.to_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Total_preprocessed\\changes_preprocessed_48h_Total.csv", index=False)
changes_df_48h_NUF_Total = pd.concat([changes_df_48h_NUF_HiRID, changes_df_48h_NUF_AmsterdamUMCDb], ignore_index=True)
changes_df_48h_NUF_Total.to_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Total_preprocessed\\changes_preprocessed_48h_NUF_Total.csv", index=False)


###########################
###########################
## Load regular of HiRID ##
regular_df_HiRID = pd.read_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\HiRID_preprocessed\\regular_preprocessed_HiRID.csv")
regular_df_48h_HiRID = pd.read_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\HiRID_preprocessed\\regular_preprocessed_48h_HiRID.csv")
regular_df_48h_NUF_HiRID = pd.read_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\HiRID_preprocessed\\regular_preprocessed_48h_NUF_HiRID.csv")

## Load regular of AmsterdamUMCDb ##
regular_df_AmsterdamUMCDb = pd.read_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\UMCDb_preprocessed\\regular_preprocessed_AmsterdamUMCDb.csv")
regular_df_48h_AmsterdamUMCDb = pd.read_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\UMCDb_preprocessed\\regular_preprocessed_48h_AmsterdamUMCDb.csv")
regular_df_48h_NUF_AmsterdamUMCDb = pd.read_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\UMCDb_preprocessed\\regular_preprocessed_48h_NUF_AmsterdamUMCDb.csv")

## Combine the dataframes and save
regular_df_Total = pd.concat([regular_df_HiRID, regular_df_AmsterdamUMCDb], ignore_index=True)
regular_df_Total.to_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Total_preprocessed\\regular_preprocessed_Total.csv", index=False)
regular_df_48h_Total = pd.concat([regular_df_48h_HiRID, regular_df_48h_AmsterdamUMCDb], ignore_index=True)
regular_df_48h_Total.to_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Total_preprocessed\\regular_preprocessed_48h_Total.csv", index=False)
regular_df_48h_NUF_Total = pd.concat([regular_df_48h_NUF_HiRID, regular_df_48h_NUF_AmsterdamUMCDb], ignore_index=True)
regular_df_48h_NUF_Total.to_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Total_preprocessed\\regular_preprocessed_48h_NUF_Total.csv", index=False)


###########################
"""

