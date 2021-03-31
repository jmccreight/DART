import copy
import datetime
import f90nml
import pathlib
import pickle
import shlex
import shutil
import subprocess
import time
import yaml

from .gregorian import gregorian
from .setup_experiment_tools import replace_in_file, get_top_level_dir_from_config
from .create_nysm_snow_daily_obs_seq import create_nysm_snow_daily_obs_seq

def setup_nysm_snow_daily(
    config,
    config_file: None
):

    print('Preparing NYSM snow daily observations.')

    # Setup the namelist and establish the "outputdir". The create_nysm_snow_daily_obs_seq
    # actually creates the obs sequences.

    nysm_daily_config = config['observation_preparation']['NYSM_snow_daily']
    input_dir = nysm_daily_config['input_dir']
    output_dir = nysm_daily_config['output_dir']
    # Output directory: make if DNE
    output_dir.mkdir(exist_ok=False, parents=True)

    # converter: identity or regular obs converter?
    # Check that the desired obs converter is in the dart build
    exp_dir = config['experiment']['experiment_dir']
    dart_build_dir = config['dart']['build_dir']
    dart_compile = pickle.load(open(exp_dir / dart_build_dir / 'DartCompile.pkl', 'rb'))

    if nysm_daily_config['identity_obs']:
#       ocp = dart_compile.models__wrf_hydro__work.exes['create_identity_streamflow_obs']
        print("AT THIS TIME IDENTITY OBSERVATION IS NOT SUPPORTED FOR NYSM SNOW")
    else:
        ocp = dart_compile.observations__obs_converters__NYSM__work.exes['convert_NYSM_snow']

    obs_conv_prog = ocp
    _ = shutil.copy(obs_conv_prog, output_dir / obs_conv_prog.name)

    # input.nml: patch.
    converter_nml = str(obs_conv_prog.name) + '_nml'
    obs_conv_patches = nysm_daily_config['input_nml_patches'][converter_nml]
    input_nml = f90nml.read(obs_conv_prog.parent / 'input.nml')
    internal_patches = ['input_files', 'location_file']
    special_patches = ['gages_file_list']
    for kk in nysm_daily_config['input_nml_patches'].keys():
        if kk in internal_patches + special_patches:
            if kk in internal_patches:
                warnings.warn("NYSM observation converter namelist patch is applied internally: " + kk)
            pass
        input_nml[kk].update(nysm_daily_config['input_nml_patches'][kk])

    # input.nml gage_file_list: Allow a file or a list: link or construct file, set in input.nml. 
    wanted_gages = nysm_daily_config['wanted_gages']
    if type(wanted_gages) in [str, pathlib.PosixPath]:
        wanted_gages = pathlib.PosixPath(wanted_gages)
        (output_dir / wanted_gages.name).symlink_to(wanted_gages)
        input_nml[convert_nml]['gages_list_file'] = wanted_gages.name
    elif type(wanted_gages) is list:
        default_filename = 'wanted_gages_list.txt'
        input_nml[converter_nml]['gages_list_file'] = default_filename
        with open(output_dir / default_filename, 'w') as opened_file:
            for gg in wanted_gages:
                _ = opened_file.write(str(gg) + '\n')
    else:
        raise ValueError("wanted_gages must be either string or list type.")

    #input.nml input_files: create a list of files in the start and end range.
    in_start_time = datetime.datetime.strptime(str(nysm_daily_config['start_date']), '%Y-%m-%d')
    in_end_time = datetime.datetime.strptime(str(nysm_daily_config['end_date']), '%Y-%m-%d')
    all_input_files = sorted(input_dir.glob("*.nc"))
    input_files_requested = []
    for ff in all_input_files:
        file_time = datetime.datetime.strptime(ff.name.split('.')[0], '%Y%m%d')
        # For end_time, add in a day and use a stricly less than... 
        if file_time >= in_start_time and file_time < (in_end_time + datetime.timedelta(days=1)):
            input_files_requested.append(ff)

    default_filename = 'list_of_obs_files.txt'
    input_nml[converter_nml]['input_files'] = default_filename
    with open(output_dir / default_filename, 'w') as opened_file:
        for ff in input_files_requested:
            _ = opened_file.write(str(ff) + '\n')

    # Now we are done editing it, write the input.nml back out.
    input_nml.write(output_dir / 'input.nml')

    # Symlink the config file into the output_dir so the default yaml file name
    # can be used by create_nysm_daily_obs_seq.
    if config_file is None:
        config_file = sorted(exp_dir.glob('original.*.yaml'))[0]
    (output_dir / 'config_file.yaml').symlink_to(config_file)

    # Stage the file that does the batch processing.
    this_file = pathlib.Path(__file__)
    batcher_base = 'create_nysm_snow_daily_obs_seq.py'
    (output_dir / batcher_base).symlink_to(this_file.parent / batcher_base)

    # Setup the scheduled script.
    orig_submit_script = this_file.parent / 'submission_scripts/submit_nysm_daily_obs_converter.sh'
    this_submit_script = output_dir / 'submit_nysm_daily_obs_converter.sh'
    shutil.copy(orig_submit_script, this_submit_script)

    # Set the PBS directives (cheyenne)

    # PBS options from config
    # Short-hand
    nysm_sched = config['observation_preparation']['NYSM_snow_daily']['scheduler']

    if nysm_sched is not None and nysm_sched != 'None':
    
        # The easy ones.
        replace_in_file(this_submit_script, 'JOB_NAME_TEMPLATE', nysm_sched['job_name'])
        replace_in_file(this_submit_script, 'ACCOUNT_TEMPLATE', nysm_sched['account'])
        replace_in_file(this_submit_script, 'EMAIL_WHO_TEMPLATE', nysm_sched['email_who'])
        replace_in_file(this_submit_script, 'EMAIL_WHEN_TEMPLATE', nysm_sched['email_when'])
        replace_in_file(this_submit_script, 'QUEUE_TEMPLATE', nysm_sched['queue'])

        # Wall time
        nysm_walltime = nysm_sched['walltime']
        if len(nysm_walltime.split(':')) == 2:
            nysm_walltime = nysm_walltime + ':00'
        nysm_walltime = 'walltime=' + nysm_walltime
        replace_in_file(this_submit_script, 'WALLTIME_TEMPLATE', nysm_walltime)

        # Select statement
        # Right now, only single node processing
        select_stmt = 'select=1:ncpus={ncpus}:mpiprocs={mpiprocs}'.format(
            **{
                'ncpus': nysm_sched['ncpus'],
                'mpiprocs': nysm_sched['mpiprocs']
            }
        )
        replace_in_file(this_submit_script, 'PBS_SELECT_TEMPLATE', select_stmt)

        wait_file = output_dir / '.this_submit_script_not_complete'
        replace_in_file(this_submit_script, 'WAIT_FILE_TEMPLATE', str(wait_file))

        proc = subprocess.Popen(
            shlex.split('touch ' + wait_file.name),
            cwd=output_dir
        )
        proc.wait()

        proc = subprocess.Popen(
            shlex.split('qsub ' + this_submit_script.name),
            cwd=output_dir
        )
        proc.wait()

        print('Job submitted. \nWait file: ' + str(wait_file) + ' ...')
        while wait_file.exists():
            msg = 'Last check for wait file {twirl}: ' + \
                  datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
            for twirl in ['|', '/', '-', '\\', '|', '/', '-', '\\']:
                print(msg.format(**{'twirl':twirl}), end='\r')
                time.sleep(10/8)

    else:

        result = create_nysm_snow_daily_obs_seq(config)

    # Link the obs_seq files to the "all_obs_dir" for the experiment.    
    all_obs_dir = pathlib.PosixPath(config['observation_preparation']['all_obs_dir'])
    all_obs_seq = output_dir.glob('obs_seq.*')
    for oo in all_obs_seq:
        (all_obs_dir / oo.name).symlink_to(oo)
        
        
    return 0
