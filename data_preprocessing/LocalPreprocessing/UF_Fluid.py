import pandas as pd
import numpy as np

#####################################################################################################################################################

def calculate_statistics(df, variable = "vm5010_idx", patid="patid", z_thresh=5, rolling_window=3, df_positive = True, df_neg_FB = False):
    """Calculates per-patient stats for a variable, once for all data, once for first 48h, and once for UF > 0 if df_positive = True (default)."""
    
    def compute_stats(df, suffix=""):
        sub_df = df.copy()
        zscore_colname = f"{variable}_zscore{suffix}"
        cleaned_colname = f"{variable}_cleaned{suffix}"
        rolling_colname = f"{variable}_rolling{suffix}"

        sub_df[zscore_colname] = (sub_df[variable] - sub_df[variable].mean()) / sub_df[variable].std()
        sub_df[cleaned_colname] = sub_df[variable].where(sub_df[zscore_colname].abs() <= z_thresh)
        sub_df[rolling_colname] = sub_df.groupby(patid)[cleaned_colname].transform(
            lambda x: x.rolling(window=rolling_window, min_periods=1).mean())

        mean_df = sub_df.groupby(patid)[cleaned_colname].mean().reset_index(name=f"mean_{variable}{suffix}")
        sd_df = sub_df.groupby(patid)[cleaned_colname].std().reset_index(name=f"standard_deviation_{variable}{suffix}")
        median_df = sub_df.groupby(patid)[rolling_colname].median().reset_index(name=f"median_{variable}{suffix}")
        min_df = sub_df.groupby(patid)[rolling_colname].min().reset_index(name=f"min_{variable}{suffix}")
        max_df = sub_df.groupby(patid)[rolling_colname].max().reset_index(name=f"max_{variable}{suffix}")
        Q3_df = sub_df.groupby(patid)[rolling_colname].quantile(0.75).reset_index(name=f"Q3_{variable}{suffix}")
        Q1_df = sub_df.groupby(patid)[rolling_colname].quantile(0.25).reset_index(name=f"Q1_{variable}{suffix}")
        
        IQR_df = pd.merge(Q3_df, Q1_df, on=patid, how="outer")
        IQR_df[f"IQR_{variable}{suffix}"] = IQR_df[f"Q3_{variable}{suffix}"] - IQR_df[f"Q1_{variable}{suffix}"]
        
        stats_df = mean_df \
            .merge(sd_df, on=patid, how="outer") \
            .merge(median_df, on=patid, how="outer") \
            .merge(min_df, on=patid, how="outer") \
            .merge(max_df, on=patid, how="outer") \
            .merge(IQR_df, on=patid, how="outer")

        return stats_df

    df_all = compute_stats(df, suffix="")
    df_first48h = compute_stats(df[df["session_length"]<=2880], suffix="_48h")
    result = df_all.merge(df_first48h, on=patid, how="outer")
    if df_positive == True:
        df_pos = compute_stats(df[df[variable] > 0], suffix = "_UFpos")
        result = result.merge(df_pos, on=patid, how="outer")
    if df_neg_FB == True:
        df_neg = compute_stats(df[df[variable] < 0], suffix = "_FBneg")
        result = result.merge(df_neg, on=patid, how="outer")
    return result

def creating_UF_bins(column_to_bin, newcolumname, df, low_bin_limit = 1.01, high_bin_limit = 1.75):
    """Creating bins using bin_limits as the margin, and a bin for 0 values."""
    
    # bin_limits can be changed as desired. 
    ### With the default bin_limits:
    #       0 is categorised as zero 
    #       < 1.01 [ml/kg/h] is categorised as low
    #       1.01 until 1.75 [ml/kg/h] is categorised as moderate
    #       > 1.75 [ml/kg/h] is categorised as high    
    condition = [df[column_to_bin] == 0, 
                 df[column_to_bin] < low_bin_limit, 
                 df[column_to_bin] <= high_bin_limit, 
                 df[column_to_bin] > high_bin_limit]
    category = ["zero",f"low (<{low_bin_limit})", f"moderate ({low_bin_limit}-{high_bin_limit})", f"high (>{high_bin_limit})"]
    df[newcolumname] = np.select(condition, category, default="NaN")
    return df

def creating_FB_bins(column_to_bin, newcolumname, df, neutral_bin_limit=20.833, negative_bin_limit=62.5):
    """Creating bins using bin_limit as the margin."""
    # bin_limit can be changed as desired. 
    # Default set to: ##negative FB bins: 500ml/d -> 20.833ml/h, 1500ml/d -> 62.5 ml/h
    # Neutral fluid balance displays patients with -500 until 500ml per day change of fluid balance... 

    ### If neutral_bin_limit is set to 100, then: 
    #       < -100 [ml/h] is categorised as negative
    #       -100 until 100 [ml/h] is categorised as neutral
    #       > 100 [ml/h] is categorised as positive    
    condition = [df[column_to_bin] > neutral_bin_limit, 
              df[column_to_bin] >= -neutral_bin_limit,
              df[column_to_bin] < -negative_bin_limit,
              df[column_to_bin] < -neutral_bin_limit]
    category = [f"positive (>{neutral_bin_limit})", f"neutral (-{neutral_bin_limit} - {neutral_bin_limit})", f"strong_negative (< -{negative_bin_limit})", f"light_negative (-{negative_bin_limit} - -{neutral_bin_limit})"]
    df[newcolumname] = np.select(condition, category, default="NaN")
    return df

def unlimited_UF_increase(x):
    """Groups patients binary into 1 = yes and 0 = no for patients where UF can be increased without need for decrease."""
    pause_null = x[x["NaN_converted_to_0"]==1]     
    if pause_null.empty:
        df = x.copy()
    else: 
        df = x[~x.index.isin(pause_null.index)].copy() #ignore all 0 that were added in for CRRT pauses that were NaN's initially 
        #Exclude drops of UF that were not due to physiological reasons.   
    
    if not (df["vm5010_idx"] > 0).any():
        return 0
    start_time = df.loc[df["vm5010_idx"]> 0, "session_length"].iloc[0]
    df = df[df["session_length"] <= start_time + 1440]
    
    df["UF_diff"] = df["vm5010_idx"].diff()
    if (df["UF_diff"].dropna() >= 0).all():
        return 1
    else: 
        return 0  

def calc_area_under_UF(patgroup, stays_filtered_df):
    """Calculates the area under the curve for UF rate [ml/kg/h] of the first 12h after first UF value > 0.
    For patients with no or one single entry of UF > 0, and patients with NUF for less than 12h, NaN is returned."""
    nonnull_df = patgroup[patgroup["vm5010_idx"] > 0].copy() #filter out CRRT pauses / null values, 
    if len(nonnull_df) <2:
        return np.nan
    start_time = nonnull_df["session_length"].iloc[0]
    
    df = patgroup[patgroup["session_length"] <= start_time + 720] 
    df = df.merge(stays_filtered_df.rename(columns={"session_length":"max_session_length"})[["patid", "max_session_length"]], on="patid", how="left")
    if df["max_session_length"].iloc[0] < (start_time + 720): #For patients not having at least 12h of UF after first time UF>0
        return np.nan
    
    new_row = df.iloc[-1].copy()
    new_row["session_length"] = start_time + 720
    df = pd.concat([df, new_row.to_frame().T], ignore_index=True)
    
    df["duration_h"] = df["session_length"].diff() / 60 
    df.loc[df.index[0], "duration_h"] = 0 #Change the NaN to null. 
    df["uf_volume"] = df["duration_h"] * df["vm5010_idx"]
    
    return df["uf_volume"].sum()

def calc_lab_increase(patgroup, variable, threshold, initial_window_min = 120):
    """Categorises patients into groups of increase of lab values: (2) Elevated from the start. (1) Elevated later. (0) Never elevated."""    
    if patgroup.empty or variable not in patgroup:
        return np.nan
    
    initial_window = patgroup[patgroup['session_length'] <= initial_window_min][variable].dropna()
    if initial_window.empty:
        reference_value = 0
    else: 
        reference_value = initial_window.iloc[0]
        
    if reference_value > threshold:
        return 2
    elif (patgroup[variable] >= threshold).any():
        return 1
    else:
        return 0
    
#####################################################################################################################################################

def process_dataset(dataset_name, stays_path, regular_path, changes_path, suffix):
    print(f"Processing {dataset_name} dataset...")
    stays_filtered_df = pd.read_csv(stays_path)
    regular_preprocessed_df = pd.read_csv(regular_path)
    changes_preprocessed_df = pd.read_csv(changes_path)

    # Merge with weight at admission
    changes_preprocessed_filtered_df = pd.merge(stays_filtered_df[["patid", "weight_at_admission"]],
                                               changes_preprocessed_df, on="patid", how="left")
    regular_preprocessed_filtered_df = pd.merge(stays_filtered_df[["patid", "weight_at_admission"]],
                                               regular_preprocessed_df, on="patid", how="left")

    # Fluid overload calculations
    changes_df_fluidoverload = changes_preprocessed_filtered_df.copy()
    changes_df_fluidoverload["fluidoverload"] = (changes_df_fluidoverload["fluid_balance_pre_crrt"] / 1000 /
                                                 changes_df_fluidoverload["weight_at_admission"]) * 100

    regular_df_fluidoverload = regular_preprocessed_filtered_df.copy()
    regular_df_fluidoverload["fluidoverload"] = (regular_df_fluidoverload["fluid_balance_pre_crrt"] / 1000 /
                                                 regular_df_fluidoverload["weight_at_admission"]) * 100

    # Calculate weight indexed UF rate and Noradrenaline rate
    changes_df_UFperkg = changes_df_fluidoverload.copy()
    changes_df_UFperkg["vm5010_idx"] = changes_df_UFperkg["vm5010"] / changes_df_UFperkg["weight_at_admission"]
    changes_df_UFperkg["vm2201_idx"] = changes_df_UFperkg["vm2201"] / changes_df_UFperkg["weight_at_admission"]
    changes_df_UFperkg = changes_df_UFperkg.drop(columns=["weight_at_admission"])

    regular_df_UFperkg = regular_df_fluidoverload.copy()
    regular_df_UFperkg["vm5010_idx"] = regular_df_UFperkg["vm5010"] / regular_df_UFperkg["weight_at_admission"]
    regular_df_UFperkg["vm2201_idx"] = regular_df_UFperkg["vm2201"] / regular_df_UFperkg["weight_at_admission"]
    regular_df_UFperkg = regular_df_UFperkg.drop(columns=["weight_at_admission"])
 
    # Calculate statistics and bins for UF: 
    UFnet_df = calculate_statistics(df=regular_df_UFperkg, variable="vm5010_idx")
    creating_UF_bins(column_to_bin="mean_vm5010_idx", newcolumname="mean_UFnet_bin", df=UFnet_df)
    creating_UF_bins(column_to_bin="mean_vm5010_idx_48h", newcolumname="mean_UFnet_bin_48h", df=UFnet_df)
    creating_UF_bins(column_to_bin="mean_vm5010_idx_UFpos", newcolumname="mean_UFnet_bin_UFpos", df=UFnet_df)
    creating_UF_bins(column_to_bin="Q3_vm5010_idx", newcolumname="Q3_UFnet_bin", df=UFnet_df)

    UFnet_df = UFnet_df.merge(regular_df_UFperkg.drop_duplicates(subset="patid")[["patid", "fluidoverload"]],
                              on="patid", how="outer")

    UF_AUC_df = changes_df_UFperkg.groupby("patid").apply(lambda x: calc_area_under_UF(x, stays_filtered_df)).reset_index(name="UF_AUC")
    UFnet_df = UFnet_df.merge(UF_AUC_df, on="patid", how="outer")

    UF_unlimited_increase_24h = changes_df_UFperkg.groupby("patid").apply(unlimited_UF_increase).reset_index(name="unlimited_UF_increase_24h")
    UFnet_df = UFnet_df.merge(UF_unlimited_increase_24h, on="patid", how="outer")

    #Calculate statistics and bins for FB: 
    fluidbalance_df = calculate_statistics(df=regular_df_UFperkg, z_thresh=1.96, variable="dm_balancerate_h", df_positive=False, df_neg_FB=True)
    creating_FB_bins(column_to_bin="mean_dm_balancerate_h", newcolumname="mean_FB_bin", df=fluidbalance_df)
    creating_FB_bins(column_to_bin="mean_dm_balancerate_h_48h", newcolumname="mean_FB_bin_48h", df=fluidbalance_df)
    creating_FB_bins(column_to_bin="Q1_dm_balancerate_h", newcolumname= "Q1_FB_bin", df=fluidbalance_df)
    creating_FB_bins(column_to_bin="mean_dm_balancerate_h_FBneg", newcolumname= "mean_FB_bin_FBneg", df=fluidbalance_df)

    fluid_and_ultrafiltration_df = pd.merge(UFnet_df, fluidbalance_df, on="patid", how="outer")

    ##Calculate statistics for Lactate and Noradrenaline: 
    Noradrenaline_df_reg = calculate_statistics(regular_df_UFperkg, variable="vm2201_idx", df_positive=False)
    Noradrenaline_df_reg = Noradrenaline_df_reg.merge(
        regular_df_UFperkg.groupby("patid").apply(lambda x: calc_lab_increase(x, variable="vm2201_idx", threshold=0.1))
        .reset_index(name="Noradrenalin_increase_0.1_mcgkgmin"), on="patid", how="outer")

    Lactate_df_reg = calculate_statistics(regular_df_UFperkg, variable="vm2105", df_positive=False)
    Lactate_df_reg = Lactate_df_reg.merge(
        regular_df_UFperkg.groupby("patid").apply(lambda x: calc_lab_increase(x, variable="vm2105", threshold=2))
        .reset_index(name="Lactate_increase_2"), on="patid", how="outer")
    Lactate_df_reg = Lactate_df_reg.merge(
        regular_df_UFperkg.groupby("patid").apply(lambda x: calc_lab_increase(x, variable="vm2105", threshold=4))
        .reset_index(name="Lactate_increase_4"), on="patid", how="outer")

    Lab_values_reg = pd.merge(Noradrenaline_df_reg, Lactate_df_reg, on="patid", how="outer")

    return {
        "fluid_and_ultrafiltration_df": fluid_and_ultrafiltration_df,
        "Lab_values_reg": Lab_values_reg,
        "regular_UFperkg" : regular_df_UFperkg,
        "changes_UFperkg" : changes_df_UFperkg
    }

# Paths for each dataset
datasets = {
    "HiRID": {
        "stays": "C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\HiRID_preprocessed\\stays_filtered_HiRID.csv",
        "regular": "C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\HiRID_preprocessed\\regular_preprocessed_HiRID.csv",
        "changes": "C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\HiRID_preprocessed\\changes_preprocessed_HiRID.csv",
        "suffix": "_HiRID"
    },
    "AmsterdamUMCDb": {
        "stays": "C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\UMCDb_preprocessed\\stays_filtered_AmsterdamUMCDb.csv",
        "regular": "C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\UMCDb_preprocessed\\regular_preprocessed_AmsterdamUMCDb.csv",
        "changes": "C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\UMCDb_preprocessed\\changes_preprocessed_AmsterdamUMCDb.csv",
        "suffix": "_AmsterdamUMCDb"
    }
}

results_fluid_and_UF = []
results_lab_values = []
results_regular_UFperkg = []
results_changes_UFperkg = []

for ds_name, paths in datasets.items():
    result = process_dataset(ds_name, paths["stays"], paths["regular"], paths["changes"], paths["suffix"])
    # Append dataset name as a new column to identify origin after merging
    result["fluid_and_ultrafiltration_df"]["dataset"] = ds_name
    result["Lab_values_reg"]["dataset"] = ds_name
    
    result["fluid_and_ultrafiltration_df"].to_csv(f"C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Final\\Fluid_and_Ultrafiltration_{ds_name}.csv", index=False)
    result["Lab_values_reg"].to_csv(f"C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Final\\Lab_values_reg_{ds_name}.csv", index=False)
    result["regular_UFperkg"].to_csv(f"C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Final\\regular_UFperkg_{ds_name}.csv", index=False)
    result["changes_UFperkg"].to_csv(f"C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Final\\changes_UFperkg_{ds_name}.csv", index=False)
    
    results_fluid_and_UF.append(result["fluid_and_ultrafiltration_df"])
    results_lab_values.append(result["Lab_values_reg"])
    results_regular_UFperkg.append(result["regular_UFperkg"])
    results_changes_UFperkg.append(result["changes_UFperkg"])
    
# Combine results
combined_fluid_and_UF = pd.concat(results_fluid_and_UF, ignore_index=True)
combined_lab_values = pd.concat(results_lab_values, ignore_index=True)
combined_regular_UFperkg = pd.concat(results_regular_UFperkg, ignore_index=True)
combined_changes_UFperkg = pd.concat(results_changes_UFperkg, ignore_index=True)

# Save combined CSVs
combined_fluid_and_UF.to_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Final\\Fluid_and_Ultrafiltration_Total.csv", index=False)
combined_lab_values.to_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Final\\Lab_values_reg_Total.csv", index=False)
combined_regular_UFperkg.to_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Final\\regular_UFperkg_Total.csv", index=False)
combined_changes_UFperkg.to_csv("C:\\Programming\\FLIRRT\\FLIRRT_preprocessing\\Final\\changes_UFperkg_Total.csv", index=False)

print("Processing complete. Combined CSV files saved.")

