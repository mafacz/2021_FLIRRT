import pandas as pd
import numpy as np
import os
import sys
from pyspark.sql import functions as sf
from datetime import datetime
import argparse
import polars as pl

sys.path.append('/cluster/home/faltysm/git/base_ds/cluster')
from spark_common import get_spark_session
from dataexport_patient import process_patient

CRRT_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_HiRID/dm_crrt/2021-12-04"

'''wraper loops through a set of patients'''
def process_batch(startid, stopid):
    #open spark session
    spark = get_spark_session(4, 1024, 64)

    df_crrt_stays = pd.DataFrame()
    df_regular = pd.DataFrame()
    df_changes = pd.DataFrame()

    #get all patients with crrt and load data
    #this indirectly filters patients for the period of after mid 2013
    #before there was no recordings for v9599 which is usedm by dm_crrt to define presence for crrt
    pids = spark.read.parquet(os.path.join(CRRT_PATH)).filter("dm_crrt==1").filter(sf.col("patientid") >= startid).filter(sf.col("patientid") <= stopid).select("patientid").distinct().toPandas().patientid

    for pid in pids:
        print (pid)
    
        stays, regular, changes = process_patient(pid, spark)
        
        df_crrt_stays = df_crrt_stays.append(stays, ignore_index=True)
        df_regular = df_regular.append(regular, ignore_index=True)
        df_changes = df_changes.append(changes, ignore_index=True)

    df_crrt_stays.columns = ["patientid", "icu_stay_nr", "lenght_of_stay","age_at_admission","gender","emergency_admission", "height_at_admission", "weight_at_admission", "adm_apache_group", "adm_codeid", "apache_score", "outcome_icu_death", "outcome_death_30d", "session_lenght", "counter"]
    df_regular.columns = ["patid", "session_lenght", "timepoint_label", "idx", "dm_vent_inv_state", "dm_vent_niv_state", "vm2001", "vm2002", "vm5010"]
    df_changes.columns = ["patid", "session_lenght", "timepoint_label", "idx", "dm_vent_inv_state", "dm_vent_niv_state", "vm2001", "vm2002", "vm5010"]

    output_dir_results = os.path.join(OUTPUT_DIR, datetime.today().strftime('%Y-%m-%d'))  

    #make the three folders
    dir_stays = os.path.join(output_dir_results, "stays")
    dir_regular_measurements = os.path.join(output_dir_results, "regular_measurements")  
    dir_change_measurements = os.path.join(output_dir_results, "change_measurements")  

    if not os.path.exists(dir_stays): 
        os.makedirs(dir_stays)
    
    if not os.path.exists(dir_regular_measurements): 
        os.makedirs(dir_regular_measurements)

    if not os.path.exists(dir_change_measurements): 
        os.makedirs(dir_change_measurements)

    if not df_crrt_stays.empty:
        p_df = pl.from_pandas(df_changes)
        p_df.write_parquet(os.path.join(dir_stays, "flirrt_{}_{}.parquet".format(startid,stopid)))

    if not df_regular.empty:
        p_df = pl.from_pandas(df_changes)
        p_df.write_parquet(os.path.join(dir_regular_measurements, "flirrt_{}_{}.parquet".format(startid,stopid)))

    if not df_changes.empty:
        p_df = pl.from_pandas(df_changes)
        p_df.write_parquet(os.path.join(dir_change_measurements, "flirrt_{}_{}.parquet".format(startid,stopid)))
    
if __name__=="__main__": 
    parser=argparse.ArgumentParser()
    # CONSTANTS
    OUTPUT_DIR = '/cluster/work/grlab/clinical/hirid2/research/faltysm/2021_FLIRRT'
    LOG_DIR="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/logs"
    
    # Input paths
    parser.add_argument("--patientid_start", help="First patient id to be processed", type=int)
    parser.add_argument("--patientid_stop", help="Last patient id to be processed", type=int)
    parser.add_argument("--run_mode", default="INTERACTIVE", help="Should job be run in batch or interactive mode")
    parser.add_argument("--output_path", default=OUTPUT_DIR, help="Path to store results")

    args=parser.parse_args()
    assert(args.run_mode in ["CLUSTER", "INTERACTIVE"]) 

    if args.run_mode=="CLUSTER":
        sys.stdout=open(os.path.join(LOG_DIR,"{}_CRRT_{}_{}.stdout".format(datetime.today().strftime('%Y-%m-%d'), args.patientid_start,args.patientid_stop)),'w')
        sys.stderr=open(os.path.join(LOG_DIR,"{}_CRRT_{}_{}.stderr".format(datetime.today().strftime('%Y-%m-%d'), args.patientid_start,args.patientid_stop)),'w')

    process_batch(args.patientid_start,args.patientid_stop)

    print ("success")