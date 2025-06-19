import subprocess
import os
import os.path
import sys
import argparse

step_size = 6000
i = 0
for batch in range(60000,114000, step_size):
    i = i + 1
    LOG_DIR="/cluster/work/grlab/clinical/hirid2/research/faltysm/ICU_pipe/logs"
    job_name="flirrt_{}".format(batch)
    mem_in_mbytes = 8000
    n_cpu_cores = 4
    n_compute_hours = 24

    compute_script_path="/cluster/home/faltysm/git/2021_FLIRRT/data_preprocessing/dataexport_batch.py"

    log_result_file=os.path.join(LOG_DIR, "{}_RESULT.txt".format(job_name))
    log_error_file=os.path.join(LOG_DIR, "{}_ERROR.txt".format(job_name))

    cmd_line=" ".join(["sbatch", "--mem-per-cpu {}".format(mem_in_mbytes), 
                                "-n", "{}".format(n_cpu_cores),
                                "--time", "{}:00:00".format(n_compute_hours),
                                "--mail-type FAIL",
                                "--partition=gpu",  
                                "--job-name","{}".format(job_name), "-o", log_result_file, "-e", log_error_file, "--wrap",
                                '\"python3', compute_script_path, "--run_mode CLUSTER",
                                "--patientid_start {}".format(batch), 
                                "--patientid_stop {}".format(batch+step_size-1),
                                "--database_hirid true", '\"'])
     
    print (cmd_line)
    subprocess.call([cmd_line], shell=True)

print ("number of jobs:" + str(i))