import pandas as pd
import numpy as np
import os
from pyspark.sql import functions as sf
import polars


SOURCE_PATH="/cluster/work/grlab/clinical/hirid2/pg_db_export"
PRESENCE_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_HiRID/dm_icu_presence/2021-12-04"
CRRT_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_HiRID/dm_crrt/2021-12-04"
STAY_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_HiRID/stay_info/2021-12-04"
VENT_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_HiRID/dm_ventilation/2021-02-12"
MERGED_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_HiRID/dm_merged/2022-05-02"

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

def record_values(idx, session_lenght, df_crrt, patid, timepoint_label):
    values = [patid, session_lenght, timepoint_label, idx]
    #ventilation state
    values.append(df_crrt.iloc[idx].dm_vent_inv_state)
    values.append(df_crrt.iloc[idx].dm_vent_niv_state)
    #cardiovasc state
    values.append(df_crrt.iloc[idx].vm2001)
    values.append(df_crrt.iloc[idx].vm2002)
    #renal state
    values.append(df_crrt.iloc[idx].vm5010)
    
    return values

def process_patient(pid, spark):
    timeintervall = 5
    eval_steps_size_hours = 6
    first_eval_timepoint_hours = 1

    #load data
    stay_data = spark.read.parquet(STAY_PATH).filter(sf.col("patientid") == pid).toPandas()
    df_vent = spark.read.parquet(os.path.join(VENT_PATH)).filter(sf.col("patientid") == pid)
    df_crrt = spark.read.parquet(os.path.join(CRRT_PATH)).filter(sf.col("patientid") == pid)
    df_presence = spark.read.parquet(os.path.join(PRESENCE_PATH)).filter(sf.col("patientid") == pid)
    df_merged = spark.read.parquet(os.path.join(MERGED_PATH)).filter(sf.col("patientid") == pid)

    df_crrt = df_presence.join(df_crrt, on=["AbsDatetime"], how='left')
    df_crrt = df_crrt.join(df_vent, on=["AbsDatetime"], how='left')
    df_crrt = df_crrt.join(df_merged, on=["AbsDatetime"], how='outer').sort("AbsDatetime").toPandas()

    #resamplee for 5min timegrid
    df_crrt = df_crrt.resample('5min', on='AbsDatetime').first().reset_index()

    #forwardfill the variables you want
    ff_cols = ['vm5010']
    df_crrt.loc[:,ff_cols] = df_crrt.loc[:,ff_cols].fillna(method='ffill', limit=3)

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
    patient_data = stay_data[["patientid", "icu_stay_nr", "lenght_of_stay","age_at_admission","gender","emergency_admission", "height_at_admission", "weight_at_admission", "adm_apache_group", "adm_codeid", "apache_score", "outcome_icu_death", "outcome_death_30d"]].iloc[0].values.tolist()

    #loop through first episode and generate information on regular basis and UF changes
    regular_timepoints = []
    change_timepoints = []
    current_UF = 0
    beforeFirstSession = True
    session_lenght = timeintervall
    counter = 0
    for idx in range(len(crrt_session)):
        cur_state=crrt_session[idx]
        if cur_state==0.0 and not beforeFirstSession:
            #first time point after new session
            change_timepoints.append(record_values(idx, session_lenght, df_crrt, pid, "end"))
            break
        if cur_state==1 and not beforeFirstSession:
            #timepoint in session
            if session_lenght==first_eval_timepoint_hours*60:
                #first eval step 
                regular_timepoints.append(record_values(idx, session_lenght, df_crrt, pid, session_lenght / 60))
            if session_lenght % (eval_steps_size_hours*60)==0:
                #regular eval steps
                regular_timepoints.append(record_values(idx, session_lenght, df_crrt, pid, session_lenght / 60))
            if df_crrt.iloc[idx].vm5010 != current_UF and not np.isnan(df_crrt.iloc[idx].vm5010):
                #eval step if uf changed
                change_timepoints.append(record_values(idx, session_lenght, df_crrt, pid, "change"))
                current_UF = df_crrt.iloc[idx].vm5010
            session_lenght += timeintervall
        elif cur_state==1.0 and beforeFirstSession:
            #newly in first session
            current_UF = df_crrt.iloc[idx].vm5010
            regular_timepoints.append(record_values(idx-1, 0, df_crrt, pid, -1))
            change_timepoints.append(record_values(idx, session_lenght, df_crrt, pid, "start"))
            beforeFirstSession = False

    patient_data.append(session_lenght)
    patient_data.append(counter)

    return [patient_data], regular_timepoints, change_timepoints