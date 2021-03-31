#----------------------------------------------------------------
# This script is to modify the wrf_hydro_ens_sim object to have 
# 1- hourly LSM and Hydro RESTART files
# 2- Get an emsemble of DOMIAN directories 
# 3- Get an ensemble of FORCING directories 
# 4- Set up the final ensemble based on ensemble of DOMAINS * ensemble of FORCING
#    so if I have 8 different domains and 10 forcing ensemble I can build 80 members
#---------------------------------------------------------------------------------------
import pathlib

# Get the number of domain members
pattern = "member_*"
domainPath = config['wrf_hydro']['domain_src'] / 'NWM/DOMAIN'
domain_ens_size = 1 # len(sorted(domainPath.glob(pattern)))
# print('domain_ens_size = ' + str(domain_ens_size))

# Get the number of forcing members
forcingPath=config['wrf_hydro']['domain_src'] / 'FORCING'
forcing_ens_size = 3 # len(sorted(forcingPath.glob(pattern)))
# print('forcing_ens_size = ' + str(forcing_ens_size))

#------------------------------------------------------------------------------------
# Change restart frequency to hourly in hydro namelist
#------------------------------------------------------------------------------------
att_tuple = ('base_hydro_namelist', 'hydro_nlist', 'rst_dt')
# The values can be a scalar (uniform across the ensemble) or a list of length N (ensemble size).
values = 60
wrf_hydro_ens_sim.set_member_diffs(att_tuple, values)
wrf_hydro_ens_sim.member_diffs # wont report any values uniform across the ensemble
# but this will:
[mm.base_hydro_namelist['hydro_nlist']['rst_dt'] for mm in wrf_hydro_ens_sim.members]

#----------------------------------------------------------------------------------------
# Change restart frequency to hourly in hrldas namelist
#---------------------------------------------------------------------------------------
att_tuple = ('base_hrldas_namelist', 'noahlsm_offline', 'restart_frequency_hours')
values = 1
wrf_hydro_ens_sim.set_member_diffs(att_tuple, values)
[mm.base_hrldas_namelist['noahlsm_offline']['restart_frequency_hours'] for mm in wrf_hydro_ens_sim.members]

#---------------------------------------------------------------------------------------
# Change the indir to path of the ensemble forcings in hrldas namelist
#---------------------------------------------------------------------------------------
att_tuple = ('base_hrldas_namelist', 'noahlsm_offline', 'indir')
forcing_ens_files = sorted(
    [str(d) for d in sorted(forcingPath.glob(pattern))[0:forcing_ens_size]])  # * domain_ens_size)
wrf_hydro_ens_sim.set_member_diffs(att_tuple, forcing_ens_files)

#---------------------------------------------------------------------------------------
# Change the path to the soil_properties file that has different snow parameters
#---------------------------------------------------------------------------------------
att_tuple = ('base_hrldas_namelist', 'noahlsm_offline', 'spatial_filename')
domain_ens = list(domainPath.glob(pattern))
soil_ens_files = [
    str(d) + '/soil_properties.nc' for d in sorted(domain_ens)[0:forcing_ens_size]]
# * forcing_ens_size
wrf_hydro_ens_sim.set_member_diffs(att_tuple, soil_ens_files)

