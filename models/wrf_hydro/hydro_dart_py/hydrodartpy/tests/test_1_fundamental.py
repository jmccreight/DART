import os
import pathlib
import pytest
import hydrodartpy
import hydrodartpy.core.setup_experiment_tools as hdp_tools
import xarray as xr
import yaml

def calc_file_stats(file):
    ds = xr.open_dataset(file)
    stats_dict = {}
    for vv in ds.data_vars:
        # dtype check?
        if ds[vv].dtype.char in ['S']:
            continue
        stats_dict[vv] = {
            'mean': ds[vv].mean().values.tolist(),
            'std': ds[vv].std().values.tolist()}
    return stats_dict


def test_setup_experiment(config_file):
    setup_return_code = hydrodartpy.setup_experiment(config_file=config_file)
    assert setup_return_code == 0


def test_run_experiment(config_dict):
    run_dir = pathlib.Path(config_dict['experiment']['run_dir'])
    os.chdir(run_dir)
    run_return_code = hydrodartpy.run_filter_experiment(run_dir)
    assert run_return_code == 0


def test_experiment_results(config_dict, answer_file):
    run_dir = config_dict['experiment']['run_dir']

    with open(run_dir / answer_file) as in_file:
        answer = yaml.load(in_file, Loader=yaml.FullLoader)

    check_files = list(answer.keys())

    # generate answer outputs - this requires the file paths
    # relative to run_dir to be the left-most items in the file.
    if False:
        # Note that this depends very much on the dependencies
        # of wrfhydropy among other factors.
        answer = {
            ff: calc_file_stats(run_dir / ff) for
            ff in check_files }
        with open(answer_file, 'w') as out_file:
            _ = yaml.dump(answer, out_file)
        answer_orig = answer
        assert False, "Wrote answer file, avoiding tautology with this error"
        # end generate answer outputs

    candidates = {
        str(ff): calc_file_stats(run_dir / ff) for
        ff in check_files }

    assert candidates == answer
