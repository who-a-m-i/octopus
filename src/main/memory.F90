!! Copyright (C) 2009 X. Andrade
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

module memory_oct_m
  use debug_oct_m
  use global_oct_m
  use mesh_oct_m
  use messages_oct_m
  use states_elec_oct_m
  use system_oct_m
  use unit_system_oct_m
  
  implicit none

  private
  public :: memory_run

contains

  ! ---------------------------------------------------------
  subroutine memory_run(sys)
    type(system_t),      intent(inout) :: sys

    real(8) :: mesh_global, mesh_local, wfns

    PUSH_SUB(memory_run)

    mesh_global = mesh_global_memory(sys%gr%mesh)
    mesh_local  = mesh_local_memory(sys%gr%mesh)

    call message_g%write('Mesh')
    call message_g%new_line()

    call message_g%write('  global  :')
    call message_g%write(mesh_global, units = unit_megabytes, fmt = '(f10.1)')
    call message_g%new_line()

    call message_g%write('  local   :')
    call message_g%write(mesh_local, units = unit_megabytes, fmt = '(f10.1)')
    call message_g%new_line()

    call message_g%write('  total   :')
    call message_g%write(mesh_global + mesh_local, units = unit_megabytes, fmt = '(f10.1)')
    call message_g%new_line()

    call message_g%info()

    wfns = states_elec_wfns_memory(sys%st, sys%gr%mesh)

    call message_g%write('States')
    call message_g%new_line()

    call message_g%write('  real    :')
    call message_g%write(wfns, units = unit_megabytes, fmt = '(f10.1)')
    call message_g%write(' (par_kpoints + par_states + par_domains)')
    call message_g%new_line()    

    call message_g%write('  complex :')
    call message_g%write(2.0_8*wfns, units = unit_megabytes, fmt = '(f10.1)')
    call message_g%write(' (par_kpoints + par_states + par_domains)')
    call message_g%new_line()

    call message_g%info()

    POP_SUB(memory_run)

  end subroutine memory_run

end module memory_oct_m

!! Local Variables:
!! mode: f90
!! coding: utf-8
!! End:
