!! Copyright (C)  2020 N. Tancogne-Dejean
!!
!! This program is free software; you can redistribute it and/or modify
!! it under the terms of the GNU General Public License as published by
!! the Free Software Foundation; either version 2, or (at your option)
!! any later version.
!!
!! This program is distributed in the hope that it will be useful,
!! but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!! GNU General Public License for more details.
!!
!! You should have received a copy of the GNU General Public License
!! along with this program; if not, write to the Free Software
!! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
!! 02110-1301, USA.
!!

#include "global.h"

module propagator_beeman_oct_m
  use global_oct_m
  use messages_oct_m
  use profiling_oct_m
  use propagator_abst_oct_m
  use simulation_clock_oct_m
  use system_abst_oct_m

  implicit none

  private
  public ::                            &
    propagator_beeman_t

  type, extends(propagator_abst_t) :: propagator_beeman_t
    private
  end type propagator_beeman_t

  interface propagator_beeman_t
    procedure propagator_beeman_init
  end interface propagator_beeman_t

contains

  ! ---------------------------------------------------------
  type(propagator_beeman_t) function propagator_beeman_init(time, dt, predictor_corrector) result(this)
    FLOAT,   intent(in)    :: time
    FLOAT,   intent(in)    :: dt
    logical, intent(in)    :: predictor_corrector

    PUSH_SUB(propagator_beeman_init)

    this%predictor_corrector = predictor_corrector

    if(predictor_corrector) then

      call this%list%add_node(STORE_CURRENT_STATUS)
      call this%list%add_node(BEEMAN_PREDICT_POS)
      call this%list%add_node(START_SCF_LOOP)
      call this%list%add_node(SYNC_DT)
      call this%list%add_node(UPDATE_INTERACTIONS)
      call this%list%add_node(VERLET_COMPUTE_ACC)
      call this%list%add_node(BEEMAN_CORRECT_POS)
      call this%list%add_node(BEEMAN_CORRECT_VEL)
      call this%list%add_node(END_SCF_LOOP)
      call this%list%add_node(FINISHED)

      this%max_scf_count = 2 !From Wikipedia
      this%scf_tol = CNST(1e-6) !At the moment arbitrary
 
    else

      call this%list%add_node(BEEMAN_PREDICT_POS)
      call this%list%add_node(SYNC_DT)
      call this%list%add_node(UPDATE_INTERACTIONS)
      call this%list%add_node(VERLET_COMPUTE_ACC)
      call this%list%add_node(BEEMAN_PREDICT_VEL)
      call this%list%add_node(FINISHED)

    end if

    this%internal_time = time
    this%dt = dt

    this%clock = simulation_clock_t(this%dt)

    POP_SUB(propagator_beeman_init)
  end function propagator_beeman_init

end module propagator_beeman_oct_m


!! Local Variables:
!! mode: f90
!! coding: utf-8
!! End:
