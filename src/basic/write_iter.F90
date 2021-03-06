!! Copyright (C) 2002-2006 M. Marques, A. Castro, A. Rubio, G. Bertsch, M. Oliveira
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

module write_iter_oct_m

  implicit none

  !> Define which routines can be seen from the outside.
  private

  public ::                  &
    write_iter_init,         &
    write_iter_clear,        &
    write_iter_flush,        &
    write_iter_end,          &
    write_iter_start,        &
    write_iter_set,          &
    write_iter_string,       &
    write_iter_header_start, &
    write_iter_header,       &
    write_iter_nl,           &
    write_iter_double,       &
    write_iter_int

  ! ---------------------------------------------------------
  !> write_iter functions
  interface
    !> Initializes the C object with the parameters
    subroutine write_iter_init(out,  iter, factor, file)
      use iso_c_binding
      implicit none
      type(c_ptr),      intent(inout) :: out    !< Write C object
      integer,          intent(in)    :: iter   !< Iteration number
      FLOAT,            intent(in)    :: factor !< Time interval
      character(len=*), intent(in)    :: file   !< The name of the file
    end subroutine write_iter_init

    subroutine write_iter_clear(out)
      use iso_c_binding
      implicit none
      type(c_ptr), intent(inout) :: out !< Write C object
    end subroutine write_iter_clear

    subroutine write_iter_flush(out)
      use iso_c_binding
      implicit none
      type(c_ptr), intent(inout) :: out !< Write C object
    end subroutine write_iter_flush

    subroutine write_iter_end(out)
      use iso_c_binding
      implicit none
      type(c_ptr), intent(inout) :: out !< Write C object
    end subroutine write_iter_end

    !> Writes to the corresponding file and
    !! adds one to the iteration.
    !! Must be called after write_iter_init()
    subroutine write_iter_start(out)
      use iso_c_binding
      implicit none
      type(c_ptr), intent(inout) :: out !< Write C object
    end subroutine write_iter_start

    !> Sets the iteration number to the C object
    subroutine write_iter_set(out,  iter)
      use iso_c_binding
      implicit none
      type(c_ptr),      intent(inout) :: out  !< Write C object
      integer,          intent(in)    :: iter !< Iteration number
    end subroutine write_iter_set

    subroutine write_iter_string(out, string)
      use iso_c_binding
      implicit none
      type(c_ptr),      intent(inout) :: out    !< Write C object
      character(len=*), intent(in)    :: string
    end subroutine write_iter_string

    subroutine write_iter_header_start(out)
      use iso_c_binding
      implicit none
      type(c_ptr), intent(inout) :: out !< Write C object
    end subroutine write_iter_header_start

    subroutine write_iter_header(out, string)
      use iso_c_binding
      implicit none
      type(c_ptr),      intent(inout) :: out    !< Write C object
      character(len=*), intent(in)    :: string
    end subroutine write_iter_header

    subroutine write_iter_nl(out)
      use iso_c_binding
      implicit none
      type(c_ptr), intent(inout) :: out !< Write C object
    end subroutine write_iter_nl
  end interface

  interface write_iter_double
    subroutine write_iter_double_1(out, d, n)
      use iso_c_binding
      implicit none
      type(c_ptr), intent(inout) :: out !< Write C object
      integer,     intent(in)    :: n
      real(8),     intent(in)    :: d
    end subroutine write_iter_double_1

    subroutine write_iter_double_n(out, d, n)
      use iso_c_binding
      implicit none
      type(c_ptr), intent(inout) :: out !< Write C object
      integer,     intent(in)    :: n
      real(8),     intent(in)    :: d(n)
    end subroutine write_iter_double_n

    subroutine write_iter_float_1(out, d, n)
      use iso_c_binding
      implicit none
      type(c_ptr), intent(inout) :: out !< Write C object
      integer,     intent(in)    :: n
      real(4),     intent(in)    :: d
    end subroutine write_iter_float_1

    subroutine write_iter_float_n(out, d, n)
      use iso_c_binding
      implicit none
      type(c_ptr), intent(inout) :: out !< Write C object
      integer,     intent(in)    :: n
      real(4),     intent(in)    :: d(n)
    end subroutine write_iter_float_n
  end interface write_iter_double

  interface write_iter_int
    subroutine write_iter_int_1(out, i, n)
      use iso_c_binding
      implicit none
      type(c_ptr), intent(inout) :: out !< Write C object
      integer,     intent(in)    :: n
      integer,     intent(in)    :: i
    end subroutine write_iter_int_1

    subroutine write_iter_int_n(out, i, n)
      use iso_c_binding
      implicit none
      type(c_ptr), intent(inout) :: out !< Write C object
      integer,     intent(in)    :: n
      integer,     intent(in)    :: i(n)
    end subroutine write_iter_int_n
  end interface write_iter_int

end module write_iter_oct_m

!! Local Variables:
!! mode: f90
!! coding: utf-8
!! End:
