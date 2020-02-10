!! Copyright (C) 2019 N. Tancogne-Dejean
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

module system_abst_oct_m
  use global_oct_m
  use interaction_abst_oct_m
  use messages_oct_m
  use namespace_oct_m
  use linked_list_oct_m
  use profiling_oct_m
  use propagator_abst_oct_m

  implicit none

  private
  public ::               &
    system_abst_t

  integer, public, parameter ::         &
    FINISHED                     =  0,  &
    VERLET_UPDATE_POS            =  1,  &
    VERLET_COMPUTE_ACC           =  2,  &
    VERLET_COMPUTE_VEL           =  3,  &
    VERLET_SYNC_DT               =  4,  &
    UPDATE_INTERACTIONS          =  5,  &
    START_SCF_LOOP               =  6,  &
    END_SCF_LOOP                 =  7,  &
    STORE_CURRENT_STATUS         =  8,  &
    BEEMAN_PREDICT_POS           =  9,  &
    BEEMAN_PREDICT_VEL           = 10,  &
    BEEMAN_CORRECT_POS           = 11,  &
    BEEMAN_CORRECT_VEL           = 12

  integer, public, parameter ::        &
    TOTAL_CURRENT                = 1,  &
    FORCE                        = 2

  type, abstract :: system_abst_t
    private
    type(namespace_t),   public :: namespace
  contains
    procedure                                                 :: system_dt
    procedure(system_add_interaction_partner),       deferred :: add_interaction_partner
    procedure(system_has_interaction),               deferred :: has_interaction
    procedure(system_do_td_op),                      deferred :: do_td_operation
    procedure(system_update_interaction_as_partner), deferred :: update_interaction_as_partner
    procedure(system_update_interactions),           deferred :: update_interactions
    procedure(system_set_propagator),                deferred :: set_propagator
    procedure(system_write_td_info),                 deferred :: write_td_info
    procedure(system_is_tolerance_reached),          deferred :: is_tolerance_reached
    procedure(system_store_current_status),          deferred :: store_current_status
  end type system_abst_t

  abstract interface
    subroutine system_add_interaction_partner(this, partner)
      import system_abst_t
      class(system_abst_t),     intent(inout) :: this
      class(system_abst_t),     intent(in)    :: partner
    end subroutine system_add_interaction_partner

    logical function system_has_interaction(this, interaction)
      import system_abst_t
      import interaction_abst_t
      class(system_abst_t),      intent(in) :: this
      class(interaction_abst_t), intent(in) :: interaction
    end function system_has_interaction

    subroutine system_do_td_op(this, operation)
      import system_abst_t
      class(system_abst_t), intent(inout) :: this
      integer             , intent(in)    :: operation
    end subroutine system_do_td_op

    subroutine system_update_interaction_as_partner(this, interaction)
      import system_abst_t
      import interaction_abst_t
      class(system_abst_t),      intent(in)    :: this
      class(interaction_abst_t), intent(inout) :: interaction
    end subroutine system_update_interaction_as_partner

    subroutine system_update_interactions(this)
      import system_abst_t
      class(system_abst_t),      intent(inout) :: this
    end subroutine system_update_interactions

    subroutine system_set_propagator(this, prop)
      import system_abst_t
      import propagator_abst_t
      class(system_abst_t),             intent(inout) :: this
      class(propagator_abst_t), target, intent(in)    :: prop
    end subroutine system_set_propagator

    subroutine system_write_td_info(this)
      import system_abst_t
      class(system_abst_t), intent(in) :: this
    end subroutine system_write_td_info

    logical function system_is_tolerance_reached(this, tol)
      import system_abst_t
      class(system_abst_t), intent(in) :: this
      FLOAT,                intent(in) :: tol
    end function system_is_tolerance_reached

    subroutine system_store_current_status(this)
      import system_abst_t
      class(system_abst_t), intent(inout) :: this
    end subroutine system_store_current_status

  end interface

contains

  subroutine system_dt(this, prop)
    class(system_abst_t),     intent(inout) :: this
    class(propagator_abst_t), intent(inout) :: prop

    integer :: tdop

    PUSH_SUB(system_dt)

    tdop = prop%get_td_operation()
    select case(tdop)
    case(FINISHED)
      if (debug%info .and. .not. prop%step_is_done()) then
        message(1) = "Debug: Propagation step finished for " + trim(this%namespace%get())
        call messages_info(1)
      end if
      call prop%finished()
      !DO OUTPUT HERE AND BROADCAST NEEDED QUANTITIES
      !ONLY IF WE ARE NOT YET FINISHED

    case(UPDATE_INTERACTIONS)
      if (debug%info) then
        message(1) = "Debug: Propagation step - Updating interactions for " + trim(this%namespace%get())
        call messages_info(1)
      end if

      call this%update_interactions()

      call prop%list%next()

    case(START_SCF_LOOP)
      ASSERT(prop%predictor_corrector)
      if (debug%info) then
        write(message(1), '(a,i3,a)') "Debug: SCF iter ", prop%scf_count, " for " + trim(this%namespace%get())
        call messages_info(1)
      end if

      call prop%save_scf_start()

    case(END_SCF_LOOP)
      !Here we first check if we did the maximum number of steps.
      !Otherwise, we need check the tolerance 
      if(prop%scf_count == prop%max_scf_count) then
        if (debug%info) then
          message(1) = "Debug: Max SCF Iter reached for " + trim(this%namespace%get())
          call messages_info(1)
        end if
        call prop%list%next()
      else
        !We reset the pointer to the begining of the scf loop
        if(this%is_tolerance_reached(prop%scf_tol)) then
          if (debug%info) then
            message(1) = "Debug: SCF tolerance reached for " + trim(this%namespace%get())
            call messages_info(1)
          end if
          call prop%list%next()
        else
          call prop%reset_scf_loop()
          if (debug%info) then
            write(message(1), '(a,i3,a)') "Debug: SCF iter ", prop%scf_count, " for " + trim(this%namespace%get())
           call messages_info(1)
         end if 
        end if
      end if

    case(STORE_CURRENT_STATUS)
 
      if (debug%info) then
        message(1) = "Debug: Storing the current status for " + trim(this%namespace%get())
        call messages_info(1)
      end if


      call this%store_current_status()
      call prop%list%next()

    case default
      call this%do_td_operation(tdop)
    end select

    POP_SUB(system_dt)
  end subroutine system_dt

end module system_abst_oct_m

!! Local Variables:
!! mode: f90
!! coding: utf-8
!! End:
