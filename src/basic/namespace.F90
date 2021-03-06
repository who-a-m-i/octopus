!! Copyright (C) 2019 M. Oliveira, S. Ohlmann
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

module namespace_oct_m
  use global_oct_m
  implicit none

  private
  public :: namespace_t

  integer, parameter :: MAX_NAMESPACE_LEN = 128

  type :: namespace_t
    private
    character(len=MAX_NAMESPACE_LEN) :: name
  contains
    procedure :: get => namespace_get
    procedure :: len => namespace_len
  end type namespace_t

  interface namespace_t
    procedure namespace_init
  end interface namespace_t

contains

  ! ---------------------------------------------------------
  type(namespace_t) function namespace_init(name)
    character(len=*), intent(in) :: name

    if (len(name) <= MAX_NAMESPACE_LEN) then
      namespace_init%name = name
    else
      write(stderr,'(a)') '*** Fatal Error (description follows)'
      write(stderr,'(a,i4,a)') 'Namespaces are limited to ', MAX_NAMESPACE_LEN, ' characters'
    end if

  end function namespace_init

  function namespace_get(this) result(name)
    class(namespace_t), intent(in) :: this
    character(len=MAX_NAMESPACE_LEN) :: name

    name = this%name

  end function namespace_get

  pure function namespace_len(this)
    class(namespace_t), intent(in) :: this
    integer :: namespace_len

    namespace_len = len(this%name)

  end function namespace_len

end module namespace_oct_m

!! Local Variables:
!! mode: f90
!! coding: utf-8
!! End:
