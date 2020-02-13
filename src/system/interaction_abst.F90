!! Copyright (C) 2020 M. Oliveira
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

module interaction_abst_oct_m
  use global_oct_m
  use messages_oct_m
  use profiling_oct_m
  use simulation_clock_oct_m

  implicit none

  private
  public ::               &
    interaction_abst_t

  !> The only purpose of the following class is to act as a surrogate and
  !> avoid circular dependencies between the interactions and the systems.
  type, abstract :: interaction_abst_t
    private

    integer, public :: n_system_observables
    integer, public :: n_partner_observables
    integer, allocatable, public :: system_observables(:)
    integer, allocatable, public :: partner_observables(:)

    type(simulation_clock_t), public :: clock
    
  contains
  end type interaction_abst_t

  contains
   
end module interaction_abst_oct_m

!! Local Variables:
!! mode: f90
!! coding: utf-8
!! End:
