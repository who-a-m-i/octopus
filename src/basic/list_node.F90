!! Copyright (C) 2019 M. Oliveira
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

module list_node_oct_m
  use global_oct_m
  use messages_oct_m
  use profiling_oct_m
  implicit none

  private
  public :: list_node_t, &
            list_node

  type :: list_node_t
    private
    class(*),          pointer :: value => null()
    type(list_node_t), pointer :: next_node => null()
  contains
    procedure :: get
    procedure :: next
    procedure :: set_next
  end type list_node_t

  interface list_node
    procedure constructor
  end interface list_node

contains

  function constructor(value, next)
    class(list_node_t), pointer :: constructor
    class(*),           target  :: value
    class(list_node_t), pointer :: next

    PUSH_SUB(constructor)

    ! No safe_allocate macro here, as its counterpart in linked_list.F90
    ! causes an internal compiler error with GCC 6.4.0
    allocate(constructor)
    constructor%next_node => next
    constructor%value => value

    POP_SUB(constructor)
  end function constructor

  function next(this)
    class(list_node_t) :: this
    class(list_node_t), pointer :: next

    next => this%next_node

  end function next

  subroutine set_next(this, next_node)
    class(list_node_t) :: this
    class(list_node_t), pointer :: next_node

    this%next_node => next_node

  end subroutine set_next

  function get(this)
    class(list_node_t) :: this
    class(*), pointer :: get

    get => this%value

  end function get

end module list_node_oct_m
