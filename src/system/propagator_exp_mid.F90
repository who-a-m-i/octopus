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

module propagator_exp_mid_oct_m
  use clock_oct_m
  use global_oct_m
  use messages_oct_m
  use namespace_oct_m
  use profiling_oct_m
  use propagator_abst_oct_m

  implicit none

  private
  public ::                            &
    propagator_exp_mid_t

  type, extends(propagator_abst_t) :: propagator_exp_mid_t
    private
  end type propagator_exp_mid_t

  interface propagator_exp_mid_t
    procedure propagator_exp_mid_init
  end interface propagator_exp_mid_t

contains

  ! ---------------------------------------------------------
  function propagator_exp_mid_init(namespace) result(this)
    type(namespace_t),         intent(in) :: namespace
    type(propagator_exp_mid_t), pointer    :: this

    PUSH_SUB(propagator_exp_mid_init)

    SAFE_ALLOCATE(this)

    call this%add(EXPMID_PREDICT_DT_2)
    call this%add(UPDATE_INTERACTIONS)
    call this%add(UPDATE_HAMILTONIAN)
    call this%add(EXPMID_CORRECT_DT)
    call this%add(FINISHED)

    ! Verlet has only one algorithmic step
    this%algo_steps = 2

    call this%parse_td_variables(namespace)

    POP_SUB(propagator_exp_mid_init)
  end function propagator_exp_mid_init

end module propagator_exp_mid_oct_m


!! Local Variables:
!! mode: f90
!! coding: utf-8
!! End:
