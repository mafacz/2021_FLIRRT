import os
import json
import socket
import pandas as pd
import numpy as np
from pathlib import Path

# Load configuration
with open(os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', 'path.config'))) as f:
    config = json.load(f)

data_root = config[socket.gethostname()]["output_root"]

# Base directory
base_dir = Path(data_root)

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
hirid_path = base_dir
amsterdam_path = base_dir
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
