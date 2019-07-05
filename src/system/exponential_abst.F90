!! Copyright (C) 2019 N. Tancogne-Dejean
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
!! along with st program; if not, write to the Free Software
!! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
!! 02110-1301, USA.
!!
#include "global.h"

module exponential_abst_oct_m
  use batch_oct_m
  use derivatives_oct_m
  use global_oct_m
  use loct_oct_m
  use messages_oct_m
  use types_oct_m
  use varinfo_oct_m

  implicit none

  private

  public ::                           &
    exponential_abst_t

  type, abstract :: exponential_abst_t
    private
    integer, public :: exp_method  !< which method is used to apply the exponential
    FLOAT, public   :: lanczos_tol !< tolerance for the Lanczos method
    integer, public :: exp_order   !< order to which the propagator is expanded
    integer, public :: arnoldi_gs  !< Orthogonalization scheme used for Arnoldi

  contains

    procedure, non_overridable      :: copy_to => exponential_copy
    procedure  :: end => exponential_end
    procedure(exponential_apply), deferred :: apply_batch
  end type exponential_abst_t

  integer, public, parameter ::  &
    EXP_LANCZOS            = 2,  &
    EXP_TAYLOR             = 3,  &
    EXP_CHEBYSHEV          = 4

  abstract interface
    subroutine exponential_apply(te, der, psib, ik, deltat, psib2, deltat2)
      import exponential_abst_t
      import derivatives_t
      import batch_t
      class(exponential_abst_t),       intent(inout) :: te
      type(derivatives_t),             intent(inout) :: der
      integer,                         intent(in)    :: ik
      type(batch_t), target,           intent(inout) :: psib
      FLOAT,                           intent(in)    :: deltat
      type(batch_t), target, optional, intent(inout) :: psib2
      FLOAT, optional,                 intent(in)    :: deltat2
    end subroutine exponential_apply
  end interface 


contains

   ! ---------------------------------------------------------
  subroutine exponential_end(te)
    class(exponential_abst_t), intent(inout) :: te

    PUSH_SUB(exponential_end)

    POP_SUB(exponential_end)
  end subroutine exponential_end

  ! ---------------------------------------------------------
  subroutine exponential_copy(tei, teo)
    class(exponential_abst_t), intent(in)    :: tei
    class(exponential_abst_t), intent(inout) :: teo

    PUSH_SUB(exponential_copy)

    teo%exp_method  = tei%exp_method
    teo%lanczos_tol = tei%lanczos_tol
    teo%exp_order   = tei%exp_order
    teo%arnoldi_gs  = tei%arnoldi_gs

    POP_SUB(exponential_copy)
  end subroutine exponential_copy


end module exponential_abst_oct_m


!! Local Variables:
!! mode: f90
!! coding: utf-8
!! End:
