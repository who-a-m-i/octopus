!! Copyright (C) 2002-2006 M. Marques, A. Castro, A. Rubio, G. Bertsch
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

module grid_oct_m
  use cube_oct_m
  use curvilinear_oct_m
  use derivatives_oct_m
  use double_grid_oct_m
  use geometry_oct_m
  use global_oct_m
  use mesh_oct_m
  use mesh_init_oct_m
  use messages_oct_m
  use mpi_oct_m
  use multicomm_oct_m
  use multigrid_oct_m
  use namespace_oct_m
  use nl_operator_oct_m
  use parser_oct_m
  use profiling_oct_m
  use space_oct_m
  use simul_box_oct_m
  use stencil_oct_m
  use stencil_cube_oct_m
  use unit_oct_m
  use unit_system_oct_m

  implicit none

  private
  public ::                &
    grid_t,                &
    grid_init_stage_0,     &
    grid_init_stage_1,     &
    grid_init_stage_2,     &
    grid_end,              &
    grid_write_info,       &
    grid_create_multigrid

  type grid_t
    ! Components are public by default
    type(simul_box_t)           :: sb
    type(mesh_t)                :: mesh
    type(multigrid_level_t)     :: fine
    type(derivatives_t)         :: der
    type(curvilinear_t)         :: cv
    type(multigrid_t), pointer  :: mgrid
    type(multigrid_t), pointer  :: mgrid_prec  ! the multigrid object for the preconditioner
    type(double_grid_t)         :: dgrid
    logical                     :: have_fine_mesh
    type(stencil_t)             :: stencil
  end type grid_t


contains

  !-------------------------------------------------------------------
  !>
  !! "Zero-th" stage of grid initialization. It initializes the simulation box.
  subroutine grid_init_stage_0(gr, namespace, geo, space)
    type(grid_t),          intent(inout) :: gr
    type(namespace_t),     intent(in)    :: namespace
    type(geometry_t),      intent(inout) :: geo
    type(space_t),         intent(in)    :: space

    PUSH_SUB(grid_init_stage_0)

    call simul_box_init(gr%sb, namespace, geo, space)
      
    POP_SUB(grid_init_stage_0)
  end subroutine grid_init_stage_0


  !-------------------------------------------------------------------
  subroutine grid_init_stage_1(gr, namespace, geo)
    type(grid_t),      intent(inout) :: gr
    type(namespace_t), intent(in)    :: namespace
    type(geometry_t),  intent(in)    :: geo

    type(stencil_t) :: cube
    integer :: enlarge(1:MAX_DIM)
    type(block_t) :: blk
    integer :: idir
    FLOAT :: def_h, def_rsize
    FLOAT :: grid_spacing(1:MAX_DIM)

    PUSH_SUB(grid_init_stage_1)

    !%Variable UseFineMesh
    !%Type logical
    !%Default no
    !%Section Mesh
    !%Description
    !% If enabled, <tt>Octopus</tt> will use a finer mesh for the calculation
    !% of the forces or other sensitive quantities.
    !% Experimental, and incompatible with domain-parallelization.
    !%End
    if (gr%sb%dim == 3) then 
      call parse_variable(namespace, 'UseFineMesh', .false., gr%have_fine_mesh)
    else
      gr%have_fine_mesh = .false.
    end if

    if(gr%have_fine_mesh) call messages_experimental("UseFineMesh")

    call geometry_grid_defaults(geo, def_h, def_rsize)
    
    ! initialize to -1
    grid_spacing = -M_ONE

    !%Variable Spacing
    !%Type float
    !%Section Mesh
    !%Description
    !% The spacing between the points in the mesh. This controls the
    !% quality of the discretization: smaller spacing gives more
    !% precise results but increased computational cost.
    !%
    !% When using curvilinear coordinates, this is a canonical spacing
    !% that will be changed locally by the transformation. In periodic
    !% directions, your spacing may be slightly different than what
    !% you request here, since the box size must be an integer
    !% multiple of the spacing.
    !%
    !% The default value is defined by the species if only default pseudopotentials are used
    !% or by the image resolution if <tt>BoxShape = box_image</tt>. Otherwise, there is
    !% no default.
    !%
    !% It is possible to have a different spacing in each one of the Cartesian directions
    !% if we define <tt>Spacing</tt> as block of the form
    !%
    !% <tt>%Spacing
    !% <br>&nbsp;&nbsp;spacing_x | spacing_y | spacing_z
    !% <br>%</tt>
    !%End

    if(parse_block(namespace, 'Spacing', blk) == 0) then
      if(parse_block_cols(blk,0) < gr%sb%dim) call messages_input_error('Spacing')
      do idir = 1, gr%sb%dim
        call parse_block_float(blk, 0, idir - 1, grid_spacing(idir), units_inp%length)
        if(def_h > M_ZERO) call messages_check_def(grid_spacing(idir), .true., def_h, 'Spacing', units_out%length)
      end do
      call parse_block_end(blk)
    else
      call parse_variable(namespace, 'Spacing', -M_ONE, grid_spacing(1), units_inp%length)
      grid_spacing(1:gr%sb%dim) = grid_spacing(1)
      if(def_h > M_ZERO) call messages_check_def(grid_spacing(1), .true., def_h, 'Spacing', units_out%length)
    end if

#if defined(HAVE_GDLIB)
    if(gr%sb%box_shape == BOX_IMAGE) then 
      do idir = 1, gr%sb%dim
        ! default grid_spacing is determined from lsize and the size of the image
        if(grid_spacing(idir) < M_ZERO) then
          grid_spacing(idir) = M_TWO*gr%sb%lsize(idir)/real(gr%sb%image_size(idir), REAL_PRECISION)
        end if
      end do
    end if
#endif

    if (any(grid_spacing(1:gr%sb%dim) < M_EPSILON)) then
      if (def_h > M_ZERO .and. def_h < huge(def_h)) then
        call geometry_grid_defaults_info(geo)
        do idir = 1, gr%sb%dim
          grid_spacing(idir) = def_h
          write(message(1), '(a,i1,3a,f6.3)') "Info: Using default spacing(", idir, &
            ") [", trim(units_abbrev(units_out%length)), "] = ",                        &
            units_from_atomic(units_out%length, grid_spacing(idir))
          call messages_info(1)
        end do
        ! Note: the default automatically matches the 'recommended' value compared by messages_check_def above.
      else
        message(1) = 'Either:'
        message(2) = "   *) variable 'Spacing' is not defined and"
        message(3) = "      I can't find a suitable default"
        message(4) = "   *) your input for 'Spacing' is negative or zero"
        call messages_fatal(4)
      end if
    end if

    !%Variable PeriodicBoundaryMask
    !%Type block
    !%Section Mesh
    !%Description
    !% (Experimental) Defines a mask for which periodic boundaries are replaced by zero boundary conditions.
    !%End
    if(parse_block(namespace, 'PeriodicBoundaryMask', blk) < 0) then
      gr%mesh%masked_periodic_boundaries = .false.
    else
      gr%mesh%masked_periodic_boundaries = .true.
      call parse_block_string(blk, 0, 0, gr%mesh%periodic_boundary_mask)
      call messages_experimental('PeriodicBoundaryMask')
    end if

    ! initialize curvilinear coordinates
    call curvilinear_init(gr%cv, namespace, gr%sb, geo, grid_spacing)

    ! initialize derivatives
    call derivatives_nullify(gr%der)
    call derivatives_init(gr%der, namespace, gr%sb, gr%cv%method /= CURV_METHOD_UNIFORM)

    call double_grid_init(gr%dgrid, namespace, gr%sb)

    enlarge = 0
    enlarge(1:gr%sb%dim) = 2
    enlarge = max(enlarge, double_grid_enlarge(gr%dgrid))
    enlarge = max(enlarge, gr%der%n_ghost)

    ! now we generate the mesh and the derivatives
    call mesh_init_stage_1(gr%mesh, gr%sb, gr%cv, grid_spacing, enlarge)

    ! the stencil used to generate the grid is a union of a cube (for
    ! multigrid) and the Laplacian.
    call stencil_cube_get_lapl(cube, gr%sb%dim, order = 2)
    call stencil_union(gr%sb%dim, cube, gr%der%lapl%stencil, gr%stencil)
    call stencil_end(cube)

    call mesh_init_stage_2(gr%mesh, gr%sb, geo, gr%cv, gr%stencil, namespace)

    POP_SUB(grid_init_stage_1)

  end subroutine grid_init_stage_1


  !-------------------------------------------------------------------
  subroutine grid_init_stage_2(gr, namespace, mc, geo)
    type(grid_t), target, intent(inout) :: gr
    type(namespace_t),    intent(in)    :: namespace
    type(multicomm_t),    intent(in)    :: mc
    type(geometry_t),     intent(in)    :: geo

    PUSH_SUB(grid_init_stage_2)

    call mesh_init_stage_3(gr%mesh, namespace, gr%stencil, mc)

    call nl_operator_global_init(namespace)
    if(gr%have_fine_mesh) then
      message(1) = "Info: coarse mesh"
      call messages_info(1)
    end if
    call derivatives_build(gr%der, namespace, gr%mesh)

    ! initialize a finer mesh to hold the density, for this we use the
    ! multigrid routines
    
    if(gr%have_fine_mesh) then

      if(gr%mesh%parallel_in_domains) then
        message(1) = 'UseFineMesh does not work with domain parallelization.'
        call messages_fatal(1)
      end if

      SAFE_ALLOCATE(gr%fine%mesh)
      SAFE_ALLOCATE(gr%fine%der)
      
      call multigrid_mesh_double(geo, gr%cv, gr%mesh, gr%fine%mesh, gr%stencil, namespace)

      call derivatives_nullify(gr%fine%der)      
      call derivatives_init(gr%fine%der, namespace, gr%mesh%sb, gr%cv%method /= CURV_METHOD_UNIFORM)
      
      call mesh_init_stage_3(gr%fine%mesh, namespace, gr%stencil, mc)
      
      call multigrid_get_transfer_tables(gr%fine%tt, gr%fine%mesh, gr%mesh)
      
      message(1) = "Info: fine mesh"
      call messages_info(1)
      call derivatives_build(gr%fine%der, namespace, gr%fine%mesh)

      gr%fine%der%coarser => gr%der
      gr%der%finer =>  gr%fine%der
      gr%fine%der%to_coarser => gr%fine%tt
      gr%der%to_finer => gr%fine%tt

    else
      gr%fine%mesh => gr%mesh
      gr%fine%der => gr%der
    end if

    ! multigrids are not initialized by default
    nullify(gr%mgrid)

    ! print info concerning the grid
    call grid_write_info(gr, geo, stdout)

    POP_SUB(grid_init_stage_2)
  end subroutine grid_init_stage_2


  !-------------------------------------------------------------------
  subroutine grid_end(gr)
    type(grid_t), intent(inout) :: gr

    PUSH_SUB(grid_end)

    call nl_operator_global_end()

    if(gr%have_fine_mesh) then
      call derivatives_end(gr%fine%der)
      call mesh_end(gr%fine%mesh)
      SAFE_DEALLOCATE_P(gr%fine%mesh)
      SAFE_DEALLOCATE_P(gr%fine%der)
      SAFE_DEALLOCATE_P(gr%fine%tt%to_coarse)
      SAFE_DEALLOCATE_P(gr%fine%tt%to_fine1)
      SAFE_DEALLOCATE_P(gr%fine%tt%to_fine2)
      SAFE_DEALLOCATE_P(gr%fine%tt%to_fine4)
      SAFE_DEALLOCATE_P(gr%fine%tt%to_fine8)
      SAFE_DEALLOCATE_P(gr%fine%tt%fine_i)
    end if

    call double_grid_end(gr%dgrid)

    call derivatives_end(gr%der)
    call curvilinear_end(gr%cv)
    call mesh_end(gr%mesh)

    if(associated(gr%mgrid)) then
      call multigrid_end(gr%mgrid)
      SAFE_DEALLOCATE_P(gr%mgrid)
    end if

    call stencil_end(gr%stencil)

    POP_SUB(grid_end)
  end subroutine grid_end


  !-------------------------------------------------------------------
  subroutine grid_write_info(gr, geo, iunit)
    type(grid_t),     intent(in) :: gr
    type(geometry_t), intent(in) :: geo
    integer,          intent(in) :: iunit

    PUSH_SUB(grid_write_info)

    if(.not.mpi_grp_is_root(mpi_world)) then
      if(debug%info) call messages_debug_newlines(6)
      POP_SUB(grid_write_info)
      return
    end if

    call messages_print_stress(iunit, "Grid")
    call simul_box_write_info(gr%sb, geo, iunit)

    if(gr%have_fine_mesh) then
      message(1) = "Wave-functions mesh:"
      call messages_info(1, iunit)
      call mesh_write_info(gr%mesh, iunit)
      message(1) = "Density mesh:"
    else
      message(1) = "Main mesh:"
    end if
    call messages_info(1, iunit)
    call mesh_write_info(gr%fine%mesh, iunit)

    if (gr%mesh%use_curvilinear) then
      call curvilinear_write_info(gr%cv, iunit)
    end if
    
    call messages_print_stress(iunit)

    POP_SUB(grid_write_info)
  end subroutine grid_write_info


  !-------------------------------------------------------------------
  subroutine grid_create_multigrid(gr, namespace, geo, mc)
    type(grid_t),      intent(inout) :: gr
    type(namespace_t), intent(in)    :: namespace
    type(geometry_t),  intent(in)    :: geo
    type(multicomm_t), intent(in)    :: mc

    PUSH_SUB(grid_create_multigrid)

    SAFE_ALLOCATE(gr%mgrid)
    call multigrid_init(gr%mgrid, namespace, geo, gr%cv, gr%mesh, gr%der, gr%stencil, mc)

    POP_SUB(grid_create_multigrid)
  end subroutine grid_create_multigrid

end module grid_oct_m

!! Local Variables:
!! mode: f90
!! coding: utf-8
!! End:
