! Data Assimilation Research Testbed -- DART
! Copyright 2004, 2005, Data Assimilation Initiative, University Corporation for Atmospheric Research
! Licensed under the GPL -- www.gpl.org/licenses/gpl.html

module obs_def_dew_point_mod

! <next five lines automatically updated by CVS, do not edit>
! $Source$
! $Revision$
! $Date$
! $Author$
! $Name$

use        types_mod, only : r8, missing_r8, t_kelvin, es_alpha, es_beta, es_gamma, &
                             gas_constant_v, gas_constant, L_over_Rv
use    utilities_mod, only : register_module, error_handler, E_ERR, E_MSG
use     location_mod, only : location_type, set_location, get_location , write_location, &
                             read_location
use  assim_model_mod, only : interpolate
use     obs_kind_mod, only : KIND_PS, KIND_T, KIND_QV, KIND_P, KIND_T2, KIND_Q2

implicit none
private

public :: get_expected_dew_point

! CVS Generated file description for error handling, do not edit
character(len=128) :: &
source   = "$Source$", &
revision = "$Revision$", &
revdate  = "$Date$"

logical, save :: module_initialized = .false.

contains

!----------------------------------------------------------------------



  subroutine initialize_module
!----------------------------------------------------------------------------
! subroutine initialize_module

call register_module(source, revision, revdate)
module_initialized = .true.

end subroutine initialize_module



subroutine get_expected_dew_point(state_vector, location, key, td, istatus)

real(r8),            intent(in)  :: state_vector(:)
type(location_type), intent(in)  :: location
integer,             intent(in)  :: key
real(r8),            intent(out) :: td
integer,             intent(out) :: istatus

integer  :: ipres, iqv, it
real(r8) :: t, qv, t_c, es, qvs, p, INVTD, rd_over_rv, rd_over_rv1

character(len=129) :: errstring

if ( .not. module_initialized ) call initialize_module

if(key == 1) then
   ipres = KIND_P
   iqv = KIND_QV
   it = KIND_T
elseif(key == 2) then
   ipres = KIND_PS
   iqv = KIND_Q2
   it = KIND_T2
else
   write(errstring,*)'key has to be 1 (upper levels) or 2 (2-meter), got ',key
   call error_handler(E_ERR,'get_expected_dew_point', errstring, &
        source, revision, revdate)
endif

call interpolate(state_vector, location, ipres, p, istatus)
if (istatus /= 0) then
   td = missing_r8
   return
endif
call interpolate(state_vector, location, iqv, qv, istatus)
if (istatus /= 0) then
   td = missing_r8
   return
endif
call interpolate(state_vector, location, it, t, istatus)
if (istatus /= 0) then
   td = missing_r8
   return
endif

rd_over_rv = gas_constant / gas_constant_v
rd_over_rv1 = 1.0_r8 - rd_over_rv

t_c = t - t_kelvin

!------------------------------------------------------------------------------
!  Calculate saturation vapour pressure:
!------------------------------------------------------------------------------

es = es_alpha * exp( es_beta * t_c / ( t_c + es_gamma ) )

!------------------------------------------------------------------------------
!  Calculate saturation specific humidity:
!------------------------------------------------------------------------------

qvs = rd_over_rv * es / ( p - rd_over_rv1 * es )

INVTD = 1.0_r8/t  - LOG (qv / qvs) / L_over_Rv

td = 1.0_r8 / INVTD

end subroutine get_expected_dew_point

!----------------------------------------------------------------------------

end module obs_def_dew_point_mod
