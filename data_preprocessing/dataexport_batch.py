import pandas as pd
import numpy as np
import os
import sys
from datetime import datetime
import argparse
import polars as pl

from dataexport_patient import process_patient

CRRT_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_HiRID/dm_crrt/latest"

def str2bool(v):
    if isinstance(v, bool):
        return v
    if v.lower() in ('yes', 'true', 't', 'y', '1'):
        return True
    elif v.lower() in ('no', 'false', 'f', 'n', '0'):
        return False
    else:
        raise argparse.ArgumentTypeError('Boolean value expected.')

'''wraper loops through a set of patients'''
def process_batch(startid, stopid, hirid=True):
    df_crrt_stays = pd.DataFrame()
    df_regular = pd.DataFrame()
    df_changes = pd.DataFrame()

    #get all patients with crrt and load data
    #this indirectly filters patients for the period of after mid 2013
    #before there was no recordings for v9599 which is usedm by dm_crrt to define presence for crrt
    if hirid:
        CRRT_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_HiRID/dm_crrt/latest"
    else:
        CRRT_PATH="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/DataFrame/private_UMCDb/dm_crrt/latest"

    pids = pl.scan_parquet(os.path.join(CRRT_PATH, '*.parquet')).filter(pl.col("dm_crrt")>0).filter(pl.col("PatientID") >= startid).filter(pl.col("PatientID") <= stopid).select("PatientID").unique().collect().to_pandas().PatientID

    output_dir_results = os.path.join(OUTPUT_DIR, datetime.today().strftime('%Y-%m-%d')) 

    for pid in pids:
        print (pid)
    
        stays, regular, changes = process_patient(pid, hirid)
        
        df_crrt_stays = df_crrt_stays.append(stays, ignore_index=True)
        df_regular = df_regular.append(regular, ignore_index=True)
        df_changes = df_changes.append(changes, ignore_index=True)

    df_crrt_stays.columns = ["patientid", "icu_stay_nr", "lenght_of_stay","age_at_admission","gender","emergency_admission", "height_at_admission", "weight_at_admission", "adm_apache_group", "apache_score", "outcome_icu_death", "outcome_death_28d", "session_lenght", "counter"]
    df_regular.columns = ["patid", "Session_start_time", "session_lenght", "timepoint_label", "idx", "AbsDatetime", "sofa_total_24h", "dm_vent_inv_state", "dm_vent_niv_state", "vm2001", "vm2002", "vm2105", "vm2201", "vm5010", "vm5025", "dm_balancerate_h", "fluid_balance_pre_crrt"]
    df_changes.columns = ["patid", "Session_start_time", "session_lenght", "timepoint_label", "idx", "AbsDatetime", "sofa_total_24h", "dm_vent_inv_state", "dm_vent_niv_state", "vm2001", "vm2002", "vm2105", "vm2201", "vm5010", "vm5025", "dm_balancerate_h", "fluid_balance_pre_crrt"]

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
        p_df = pl.from_pandas(df_crrt_stays)
        p_df.write_parquet(os.path.join(dir_stays, "flirrt_{}_{}.parquet".format(startid,stopid)))

    if not df_regular.empty:
        p_df = pl.from_pandas(df_regular)
        p_df.write_parquet(os.path.join(dir_regular_measurements, "flirrt_{}_{}.parquet".format(startid,stopid)))

    if not df_changes.empty:
        p_df = pl.from_pandas(df_changes)
        p_df.write_parquet(os.path.join(dir_change_measurements, "flirrt_{}_{}.parquet".format(startid,stopid)))



if __name__=="__main__": 
    parser=argparse.ArgumentParser()
    # CONSTANTS
    LOG_DIR="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/logs"
    OUTPUT_DIR = '/cluster/work/grlab/clinical/hirid2/research/faltysm/2021_FLIRRT/HiRID'
    
    # Input paths
    parser.add_argument("--patientid_start", help="First patient id to be processed", type=int)
    parser.add_argument("--patientid_stop", help="Last patient id to be processed", type=int)
    parser.add_argument("--run_mode", default="INTERACTIVE", help="Should job be run in batch or interactive mode")
    parser.add_argument('--database_hirid', type=str2bool, default=True, help='True if hirid, False if Amsterdam')

    args=parser.parse_args()
    assert(args.run_mode in ["CLUSTER", "INTERACTIVE"]) 

    if args.database_hirid:
        print ("HiRID")
        OUTPUT_DIR = '/cluster/work/grlab/clinical/hirid2/research/faltysm/2021_FLIRRT/HiRID'
    else:
        print ("AmsterdamUMCDb")
        OUTPUT_DIR = '/cluster/work/grlab/clinical/hirid2/research/faltysm/2021_FLIRRT/AmsterdamUMCDb'

    if args.run_mode=="CLUSTER":
        sys.stdout=open(os.path.join(LOG_DIR,"{}_CRRT_{}_{}.stdout".format(datetime.today().strftime('%Y-%m-%d'), args.patientid_start,args.patientid_stop)),'w')
        sys.stderr=open(os.path.join(LOG_DIR,"{}_CRRT_{}_{}.stderr".format(datetime.today().strftime('%Y-%m-%d'), args.patientid_start,args.patientid_stop)),'w')

    process_batch(args.patientid_start,args.patientid_stop, args.database_hirid)

    print ("success")