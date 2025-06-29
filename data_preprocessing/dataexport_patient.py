import pandas as pd
import numpy as np
import os
import polars as pl

def merge_short_gaps(status_arr, short_gap_min, time_intervall, close_beginning=True):
    ''' Merge short gaps in the status array, the time intervall and  gap_lenght are in minutes'''
    in_gap=False
    gap_length=0
    session_counter=0

    for idx in range(len(status_arr)):
        cur_state=status_arr[idx]
        if in_gap and (cur_state==0.0 or np.isnan(cur_state)):
            gap_length+=time_intervall
        elif not in_gap and (cur_state==0.0 or np.isnan(cur_state)):
            in_gap=True
            in_gap_idx=idx
            gap_length=time_intervall
        elif in_gap and cur_state==1.0:
            in_gap=False
            session_counter+=1
            if gap_length<=short_gap_min:
                if close_beginning or in_gap_idx > 0:
                    status_arr[in_gap_idx:idx]=1.0
    
    return status_arr, session_counter

def record_values(idx, Session_start_time, session_lenght, df_crrt, patid, timepoint_label, fluid_balance_pre_crrt):
    values = [patid, Session_start_time, session_lenght, timepoint_label, idx]
    values.append(df_crrt.iloc[idx].AbsDatetime)
    values.append(df_crrt.iloc[idx].sofa_total_24h)
    #ventilation state
    values.append(df_crrt.iloc[idx].dm_vent_inv_state)
    values.append(df_crrt.iloc[idx].dm_vent_niv_state)
    #cardiovasc state
    values.append(df_crrt.iloc[idx].vm2001)
    values.append(df_crrt.iloc[idx].vm2002)
    values.append(df_crrt.iloc[idx].vm2105)
    values.append(df_crrt.iloc[idx].vm2201)
    #renal state
    values.append(df_crrt.iloc[idx].vm5010)
    values.append(df_crrt.iloc[idx].vm5025)
    #volume state
    values.append(df_crrt.iloc[idx].dm_balancerate_h)
    values.append(fluid_balance_pre_crrt)

    # Urine state: calculate total urine output over the next 24 hours (288 × 5min intervals)
    end_idx = min(idx + 288, len(df_crrt))
    urine_flow_h = df_crrt.iloc[idx:end_idx]["dm_urineflow_h"]  # ml/h, every 5 minutes
    urine_volume_ml_24h = urine_flow_h.sum() / 12  # Convert to total ml over 24h (5min = 1/12h)

    # Append flag: True if total urine < 200 ml (extreme oliguria)
    values.append(df_crrt.iloc[idx].dm_urineflow_h)
    values.append(urine_volume_ml_24h < 200)

    return values

def process_patient(pid, hirid=True):

    if hirid:
        PRESENCE_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_HiRID/dm_icu_presence/latest"
        STAY_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_HiRID/stay_info/latest"
        VENT_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_HiRID/dm_ventilation/latest"
        MERGED_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_HiRID/dm_merged/latest"
        CRRT_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_HiRID/dm_crrt/latest"
        VOLUME_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_HiRID/dm_volumestatus/latest"
        SCORE_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_HiRID/dm_scores/latest"
    else:
        PRESENCE_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_UMCDb/dm_icu_presence/latest"
        STAY_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_UMCDb/stay_info/latest"
        VENT_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_UMCDb/dm_ventilation/latest"
        MERGED_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_UMCDb/dm_merged/latest"
        CRRT_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_UMCDb/dm_crrt/latest"
        VOLUME_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_UMCDb/dm_volumestatus/latest"
        SCORE_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_UMCDb/dm_scores/latest"
    
    #load data
    stay_data = pl.scan_parquet(os.path.join(STAY_PATH, '*.parquet')).filter(pl.col("patientid") == pid).collect().to_pandas()
    # VENT
    df_vent_scan = pl.scan_parquet(os.path.join(VENT_PATH, '*.parquet')).filter(pl.col("PatientID") == pid)
    if "__index_level_0__" in df_vent_scan.schema:
        df_vent_scan = df_vent_scan.drop("__index_level_0__")
    df_vent = df_vent_scan

    # PRESENCE
    df_presence_scan = pl.scan_parquet(os.path.join(PRESENCE_PATH, '*.parquet')).filter(pl.col("PatientID") == pid)
    if "__index_level_0__" in df_presence_scan.schema:
        df_presence_scan = df_presence_scan.drop("__index_level_0__")
    df_presence = df_presence_scan

    # MERGED
    df_merged_scan = pl.scan_parquet(os.path.join(MERGED_PATH, '*.parquet')).filter(pl.col("PatientID") == pid)
    if "__index_level_0__" in df_merged_scan.schema:
        df_merged_scan = df_merged_scan.drop("__index_level_0__")
    df_merged = df_merged_scan.with_columns(pl.col("PatientID").cast(pl.Int64))

    # CRRT
    df_crrt_scan = pl.scan_parquet(os.path.join(CRRT_PATH, '*.parquet')).filter(pl.col("PatientID") == pid)
    if "__index_level_0__" in df_crrt_scan.schema:
        df_crrt_scan = df_crrt_scan.drop("__index_level_0__")
    df_crrt = df_crrt_scan

    # SCORE
    df_score_scan = pl.scan_parquet(os.path.join(SCORE_PATH, '*.parquet')).filter(pl.col("PatientID") == pid)
    if "__index_level_0__" in df_score_scan.schema:
        df_score_scan = df_score_scan.drop("__index_level_0__")
    df_score = df_score_scan.with_columns(pl.col("PatientID").cast(pl.Int64))

    # VOLUME
    df_volume_scan = pl.scan_parquet(os.path.join(VOLUME_PATH, '*.parquet')).filter(pl.col("PatientID") == pid)
    if "__index_level_0__" in df_volume_scan.schema:
        df_volume_scan = df_volume_scan.drop("__index_level_0__")
    df_volume = df_volume_scan.with_columns(pl.col("PatientID").cast(pl.Int64))

    df_crrt = df_presence.join(df_crrt, on=["PatientID", "AbsDatetime"], how='outer')
    df_crrt = df_crrt.join(df_vent, on=["PatientID", "AbsDatetime"], how='outer')
    df_crrt = df_crrt.join(df_volume, on=["PatientID", "AbsDatetime"], how='outer')
    df_crrt = df_crrt.join(df_score, on=["PatientID", "AbsDatetime"], how='outer')
    df_crrt = df_crrt.join(df_merged, on=["PatientID", "AbsDatetime"], how='outer').sort("AbsDatetime").collect().to_pandas()

    timeintervall = 5
    eval_steps_size_hours = 1

    #resamplee for 5min timegrid
    df_crrt = df_crrt.resample('5min', on='AbsDatetime').first().reset_index()
    df_crrt["dm_balancerate_cum"] = df_crrt["dm_balancerate_cum"].fillna(method="ffill", limit=6)                                            

    #forward/backward fill the variables you want
    ff_cols = ['vm2201', 'vm2105']
    bf_cols = ["dm_balancerate_h", "sofa_total_24h"]

    df_crrt.loc[:,ff_cols] = df_crrt.loc[:,ff_cols].fillna(method='ffill', limit=24)
    df_crrt.loc[:,bf_cols] = df_crrt.loc[:,bf_cols].fillna(method='bfill', limit=24)

    if hirid:
        df_crrt.loc[:,'vm5010'] = df_crrt.loc[:,'vm5010'].fillna(method='ffill', limit=4)
        df_crrt.loc[:,'vm5025'] = df_crrt.loc[:,'vm5025'].fillna(method='ffill')
    else:
        df_crrt.loc[:,'vm5010'] = df_crrt.loc[:,'vm5010'].fillna(method='ffill', limit=24)
        df_crrt.loc[:,'vm5025'] = df_crrt.loc[:,'vm5025'].fillna(method='ffill')

    if (len(stay_data) == 0):
        print("stay data empty")
        return pd.DataFrame(), pd.DataFrame(), pd.DataFrame()

    #check hard exclusion criteria (GC)
    if not stay_data.gc.iloc[0]:
        print("exclusion")
        return pd.DataFrame(), pd.DataFrame(), pd.DataFrame()

    #merge gaps in the crrt to generate crrt episodes (not only sessions), merging gaps smaller than 1 day (1440min)
    crrt_session, total_session_count = merge_short_gaps(df_crrt.dm_crrt.array, 1440, timeintervall, False)

    #generate patient information about the patient having the crrt
    patient_data = stay_data[["patientid", "icu_stay_nr", "lenght_of_stay","age_at_admission","gender","emergency_admission", "height_at_admission", "weight_at_admission", "adm_apache_group", "apache_score", "outcome_icu_death", "outcome_death_28d"]].iloc[0].values.tolist()

    #loop through first episode and generate information on regular basis and UF changes
    regular_timepoints = []
    change_timepoints = []
    current_UF = 0
    beforeFirstSession = True
    session_lenght = timeintervall
    Session_start_time = []
    fluid_balance_pre_crrt = 0
    counter = 0
    for idx in range(len(crrt_session)):
        cur_state=crrt_session[idx]
        if (cur_state==0.0 and not beforeFirstSession) | idx == len(crrt_session)-1:
            #first time point after new session
            change_timepoints.append(record_values(idx, Session_start_time, session_lenght, df_crrt, pid, "end", fluid_balance_pre_crrt))
            break
        if cur_state==1 and not beforeFirstSession:
            #timepoint in session
            if session_lenght % (eval_steps_size_hours*60)==0:
                #regular eval steps
                regular_timepoints.append(record_values(idx, Session_start_time, session_lenght, df_crrt, pid, session_lenght / 60, fluid_balance_pre_crrt))
            if df_crrt.iloc[idx].vm5010 != current_UF and not np.isnan(df_crrt.iloc[idx].vm5010):
                #eval step if uf changed
                change_timepoints.append(record_values(idx, Session_start_time, session_lenght, df_crrt, pid, "change", fluid_balance_pre_crrt))
                current_UF = df_crrt.iloc[idx].vm5010
            session_lenght += timeintervall
        elif cur_state==1.0 and beforeFirstSession:
            #calculate session information values
            fluid_balance_pre_crrt = df_crrt.iloc[idx]['dm_balancerate_cum']
            
            ###newly in first session
            #record values at start
            current_UF = df_crrt.iloc[idx].vm5010
            Session_start_time = df_crrt.iloc[idx].AbsDatetime
            regular_timepoints.append(record_values(idx-1, Session_start_time, 0, df_crrt, pid, -1, fluid_balance_pre_crrt))
            change_timepoints.append(record_values(idx, Session_start_time, session_lenght, df_crrt, pid, "start", fluid_balance_pre_crrt))

            beforeFirstSession = False

    patient_data.append(session_lenght)
    patient_data.append(counter)

    return [patient_data], regular_timepoints, change_timepoints