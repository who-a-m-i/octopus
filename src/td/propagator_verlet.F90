!! Copyright (C)  2019 N. Tancogne-Dejean
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

module propagator_verlet_oct_m
  use clock_oct_m
  use global_oct_m
  use gauge_field_oct_m
  use messages_oct_m
  use profiling_oct_m
  use propagator_abst_oct_m
  use system_abst_oct_m

  implicit none

  private
  public ::                            &
    propagator_verlet_t

  type, extends(propagator_abst_t) :: propagator_verlet_t
    private
  end type propagator_verlet_t

  interface propagator_verlet_t
    procedure propagator_verlet_init
  end interface propagator_verlet_t

contains

  ! ---------------------------------------------------------
  function propagator_verlet_init(dt) result(this)
    FLOAT,                     intent(in) :: dt
    type(propagator_verlet_t), pointer    :: this

    PUSH_SUB(propagator_verlet_init)

    SAFE_ALLOCATE(this)

    call this%add(VERLET_UPDATE_POS)
    call this%add(UPDATE_INTERACTIONS)
    call this%add(VERLET_COMPUTE_ACC)
    call this%add(VERLET_COMPUTE_VEL)
    call this%add(FINISHED)

    ! Verlet has only one algorithmic step
    this%algo_steps = 1

    this%dt = dt

    POP_SUB(propagator_verlet_init)
  end function propagator_verlet_init

end module propagator_verlet_oct_m


!! Local Variables:
!! mode: f90
!! coding: utf-8
!! End:
