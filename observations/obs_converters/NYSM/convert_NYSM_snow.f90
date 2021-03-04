! DART software - Copyright UCAR. This open source software is provided
! by UCAR, "as is", without charge, subject to all terms of use at
! http://www.image.ucar.edu/DAReS/DART/DART_download

program convert_NYSM_snow

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   convert_NYSM_snow - program that reads a netCDF observation file from
!                        New York Mesonnet Sites (NYSM) and writes a DART
!                          obs_seq file using the DART library routines.
!
!     created Oct. 2020 Arezoo Rafieei Nasab, NCAR/RAL/HAP
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

use         types_mod, only : r8, missing_r8

use      location_mod, only : VERTISHEIGHT

use     utilities_mod, only : initialize_utilities, finalize_utilities, &
                              find_textfile_dims, error_handler, E_ERR, E_MSG, &
                              find_namelist_in_file, check_namelist_read, &
                              open_file, close_file

use  time_manager_mod, only : time_type, set_calendar_type, GREGORIAN, set_date, &
                              increment_time, get_time, print_time, print_date

use  obs_sequence_mod, only : obs_sequence_type, obs_type, read_obs_seq, &
                              static_init_obs_sequence, init_obs, write_obs_seq, &
                              init_obs_sequence, get_num_obs, &
                              set_copy_meta_data, set_qc_meta_data, &
                              get_num_copies, get_num_qc

use      obs_kind_mod, only : SNOW_THICKNESS

use obs_utilities_mod, only : add_obs_to_seq, create_3d_obs

use netcdf_utilities_mod, only : nc_check,  nc_open_file_readonly, nc_close_file, &
                                 nc_get_variable, nc_get_attribute_from_variable, &
                                 nc_get_dimension_size, nc_get_variable_size

implicit none

! version controlled file description for error handling, do not edit
character(len=*), parameter :: source   = 'convert_NYSM_snow.f90'
character(len=*), parameter :: revision = ''
character(len=*), parameter :: revdate  = ''
character(len=*), parameter :: routine  = 'convert_NYSM_snow:'

character(len=512) :: string1, string2, string3 ! strings for messages

! input file has data quality control fields, whether to use or ignore them.

integer, parameter :: num_copies = 1,   &   ! number of copies in sequence
                      num_qc     = 1        ! number of QC entries

integer :: existing_num_copies, existing_num_qc

! dimensions from NYSM standard data
integer, parameter :: IDLength = 4 ! length of the station name ! NOT USED RIGHT NOW
integer            :: station_num  ! number of stations in a given file
integer            :: time_num     ! time dimension

! variables from NYSM standard data
character(len=IDLength),  allocatable :: station_strings(:) ! NOT USED RIGHT NOW
integer,                  allocatable :: time_5M(:)
real(r8),                 allocatable :: lat(:), lon(:), elev(:), &
                                         snow_depth(:,:)

!-----------------------------------------------------------------------------------------
! extra internal variables
character(len=256) :: input_file
integer :: ifile, iunit, nfiles, ncid, io, num_new_obs
integer :: i, n, m
logical :: file_exist, first_obs
real(r8) :: oerr, qc, FillValue
integer  :: osec, oday

type(obs_sequence_type) :: obs_seq
type(obs_type)          :: obs, prev_obs
type(time_type)         :: time_obs, prev_time, comp_day0

!-----------------------------------------------------------------------------------------
! namelist variables
character(len=256) :: input_files     = 'inputs.txt'
character(len=256) :: output_file     = 'obs_seq.out'
character(len=256) :: gages_list_file = ''
real(r8)           :: obs_fraction_for_error = 0.01_r8
real(r8)           :: obs_min_err_std = 0.5_r8
integer            :: verbose = 0

namelist / convert_NYSM_snow_nml / &
     input_files, output_file, &
     obs_fraction_for_error, obs_min_err_std, &
     verbose, gages_list_file

!-----------------------------------------------------------------------------------------
! start of executable code
!-----------------------------------------------------------------------------------------

call initialize_utilities(routine)

! Read the DART namelist
call find_namelist_in_file('input.nml', 'convert_NYSM_snow_nml', iunit)
read(iunit, nml = convert_NYSM_snow_nml, iostat = io)
call check_namelist_read(iunit, io, 'convert_NYSM_snow_nml')

! print the content of the namelist for clarification
print*, 'list of input files is in : "'//trim(input_files)//'"'
print*, 'output_file               : "'//trim(output_file)//'"'
print*, 'obs_fraction_for_error    : ', obs_fraction_for_error
print*, 'verbose                   : ', verbose
print*, 'gages_list_file           : "'//trim(gages_list_file)//'"'

!*****************************************************************************************
!  Do some initilizations
!  Do a one time check of the obs_seq file and create it if does not exist
!  or append to it if does exist
!*****************************************************************************************
! put the reference date into DART format
call set_calendar_type(GREGORIAN)
comp_day0 = set_date(1970, 1, 1, 0, 0, 0)

first_obs = .true.

!  either read existing obs_seq or create a new one
call static_init_obs_sequence()
call init_obs(obs,      num_copies, num_qc)
call init_obs(prev_obs, num_copies, num_qc)

! get the number of files that need to be processed
! specified in the input_files variable in the namelist
call find_textfile_dims(input_files, nfiles)

! estimate the maximum number of observations that is going to be written into the obs_seq file
num_new_obs = estimate_total_obs_count(input_files, nfiles)

inquire(file=output_file, exist=file_exist)

if ( file_exist ) then ! existing file found, append to it

   write(string1,*) "found existing obs_seq file, appending to ", trim(output_file)
   write(string2,*) "adding up to a maximum of ", num_new_obs, " new observations"
   call error_handler(E_MSG, routine, string1, &
                      source, revision, revdate, text2=string2)

   call read_obs_seq(output_file, 0, 0, num_new_obs, obs_seq)

   ! check to see if existing file is compatible
   existing_num_copies = get_num_copies(obs_seq)
   existing_num_qc     = get_num_qc(obs_seq)

   if (existing_num_copies /= NUM_COPIES .or.  existing_num_qc /= NUM_QC) then
      write(string1,*)'incompatible existing observation sequence file'
      write(string2,'(A,i4,A,i4)')'expected ',NUM_COPIES, &
                           ' obs copies got ',existing_num_copies
      write(string3,'(A,i4,A,i4)')'expected ',NUM_QC, &
                           ' QC  copies got ',existing_num_qc
      call error_handler(E_ERR, routine, string1, &
                  source, revision, revdate, text2=string2, text3=string3)
   endif

else ! create a new one ...

   call init_obs_sequence(obs_seq, NUM_COPIES, NUM_QC, num_new_obs)

   do i=1,NUM_COPIES ! kinda silly ... only 1 type of observation
      call set_copy_meta_data(obs_seq, i, 'observation')
   enddo
   do i=1,NUM_QC ! kinda silly ... only 1 type of qc
      call set_qc_meta_data(obs_seq, i, 'Data QC')
   enddo

   write(string1,*) "no existing obs_seq file, creating ", trim(output_file)
   write(string2,*) "with up to a maximum of ", num_new_obs, " observations"
   call error_handler(E_MSG, routine, string1, &
                      source, revision, revdate, text2=string2)

endif

!*****************************************************************************************
!   Loop through the NYSM files listed as input file (input_files specified in the input.nml)
!  and adding the data to obs_seq.out
!*****************************************************************************************

! open the text file that has the list of the files to be processed
iunit = open_file(input_files,form='formatted',action='read')

FILELOOP : do ifile=1,nfiles

   read(iunit,'(A)', iostat=io) input_file
   if (io /= 0 ) then
     write(string1,*) 'Unable to read input file from "'//trim(input_files)//'"'
!     write(string2,*) 'file ',ifile
     call error_handler(E_ERR,'convert_NYSM_snow',string1, &
                source, revision, revdate, text2=string2)
   endif

   ncid = nc_open_file_readonly(input_file, routine)

   station_num =  nc_get_dimension_size(ncid, 'station')
   time_num    =  nc_get_dimension_size(ncid, 'time_5M')

   write(string1,*)'Reading NYSM standard obs file "',trim(input_file)//'"'
   write(string2,*)'number of stations    in the obs file is ',station_num
   write(string3,*)'number of time stamps in the obs file is ',time_num

   call error_handler(E_MSG, routine, string1, text2=string2, text3=string3)

   allocate(station_strings(station_num))
   allocate(            lat(station_num))
   allocate(            lon(station_num))
   allocate(           elev(station_num))

   allocate(time_5M(time_num))

   ! 'ncdump' reports the varibles in 'C-style' ordering, with the fastest-varying
   ! dimension on the right - this is opposite to the Fortran declaration where
   ! the fastest-varying dimension is on the left. 'row-major' vs. 'column-major'
   !
   ! dimensions:
   !    station = UNLIMITED ; // (126 currently)
   !    station_length = 4 ;
   !    time_5M = 288 ;
   ! float snow_depth(station, time_5M) ;
   !    snow_depth:units = "m" ;
   !    snow_depth:long_name = "snow depth" ;
   !    snow_depth:_FillValue = -996.f ;

   allocate(snow_depth(time_num,station_num))

   ! read in the data arrays
   call nc_get_variable(ncid,    'time_5M', time_5M)
   call nc_get_variable(ncid,        'lat', lat )
   call nc_get_variable(ncid,        'lon', lon )
   call nc_get_variable(ncid,       'elev', elev )
   call nc_get_variable(ncid, 'snow_depth', snow_depth) ! height above station in meters
   call nc_get_attribute_from_variable(ncid, 'snow_depth', '_FillValue', FillValue)
   call nc_close_file(ncid)

   ! convert [-180,180] to [0,360]
   where (lon < 0.0_r8) lon = lon + 360.0_r8

   ! Now looping through the stations

   TIMELOOP: do m = 1, time_num

      ! compute time of observation by adding seconds to base time
      ! extract time of observation into oday, osec needed by create_3d_obs()

      time_obs = increment_time(comp_day0, time_5M(m))
      call get_time(time_obs, osec, oday)

      ! debug, actually ... just make sure we have this correct
      call print_date(time_obs,'observation date is ')
      call print_time(time_obs,'observation time is ')
      write(*,*)''  ! just some whitespace for clarity

      STATIONLOOP: do n = 1, station_num
!         write(*,*)n, snow_depth(m,n)

         if (snow_depth(m,n) == FillValue) cycle STATIONLOOP

         ! oerr is the observation error standard deviation in this application.
         ! The observation error variance encoded in the observation file
         ! will be oerr*oerr
         oerr = max(snow_depth(m,n)*obs_fraction_for_error, obs_min_err_std)
         qc   = 0.0_r8


         call create_3d_obs(lat(n), lon(n), elev(n), VERTISHEIGHT, &
                            snow_depth(m,n), SNOW_THICKNESS, & ! Tim has (m,n) why?!
                            oerr, oday, osec, qc, obs)

         call add_obs_to_seq(obs_seq, obs, time_obs, prev_obs, prev_time, first_obs)

      enddo STATIONLOOP
   enddo TIMELOOP

   deallocate(lat, lon, elev)
   deallocate(time_5M)
   deallocate(snow_depth)

enddo FILELOOP

! If we added any obs to the sequence, write it now.
if ( get_num_obs(obs_seq) > 0 )  then
   call write_obs_seq(obs_seq, output_file)
   write(string1,*)'Writing ',get_num_obs(obs_seq),' observations to file.'
   call error_handler(E_MSG, routine, string1)
else
   write(string1,*)'ZERO observations to add.'
   call error_handler(E_ERR, routine, string1)
endif

call close_file(iunit)

! end of main program
call finalize_utilities()

contains

! -------------------------------- Functions Used above ----------------------------------
! provide a estimate of the number of total observation based on
! the number of NYSM files, number of gages in the first file
! and number of time steps in the first file

function estimate_total_obs_count(file_list,nfiles) result (num_obs)

character(len=*), intent(in) :: file_list
integer,          intent(in) :: nfiles
integer                      :: num_obs

character(len=*), parameter :: routine = 'estimate_total_obs_count'
integer :: iunit, io, ncid, station_num, time_num
character(len=256) :: input_file

iunit = open_file(file_list,form='formatted',action='read')
read(iunit,'(A)', iostat=io) input_file
if (io /= 0 ) then
  write(string1,*) 'Unable to read input file from "'//trim(file_list)//'"'
  call error_handler(E_ERR,routine,string1,source,revision,revdate)
endif
call close_file(iunit)

! Need to know about how many observations are in each file.
! hoping first file is representative
ncid        = nc_open_file_readonly(input_file)
station_num = nc_get_dimension_size(ncid, 'station')
time_num    = nc_get_dimension_size(ncid, 'time_5M')
call nc_close_file(ncid)

! We need to know how many observations there may be.
! Specifying too many is not really a problem.
! I am adding 50%

num_obs = 1.5_r8 * station_num * time_num * nfiles

end function estimate_total_obs_count

end program

