! ***********************************************************************
!
!   Copyright (C) 2010  Bill Paxton
!
!   this file is part of mesa.
!
!   mesa is free software; you can redistribute it and/or modify
!   it under the terms of the gnu general library public license as published
!   by the free software foundation; either version 2 of the license, or
!   (at your option) any later version.
!
!   mesa is distributed in the hope that it will be useful, 
!   but without any warranty; without even the implied warranty of
!   merchantability or fitness for a particular purpose.  see the
!   gnu library general public license for more details.
!
!   you should have received a copy of the gnu library general public license
!   along with this software; if not, write to the free software
!   foundation, inc., 59 temple place, suite 330, boston, ma 02111-1307 usa
!
! ***********************************************************************
 
      module run_star_extras

      use star_lib
      use star_def
      use const_def
      use math_lib
      
      implicit none

      include "test_suite_extras_def.inc"
      
      ! declarations for xtra_coeff_os
      real(dp) :: &
         xtra_coef_os_full_on, &
         xtra_coef_os_full_off, &
         xtra_coef_os_above_nonburn, &
         xtra_coef_os_below_nonburn, &
         xtra_coef_os_above_burn_h, &
         xtra_coef_os_below_burn_h, &
         xtra_coef_os_above_burn_he, &
         xtra_coef_os_below_burn_he, &
         xtra_coef_os_above_burn_z, &
         xtra_coef_os_below_burn_z, &
         xtra_dist_os_above_nonburn, &
         xtra_dist_os_below_nonburn, &
         xtra_dist_os_above_burn_h, &
         xtra_dist_os_below_burn_h, &
         xtra_dist_os_above_burn_he, &
         xtra_dist_os_below_burn_he, &
         xtra_dist_os_above_burn_z, &
         xtra_dist_os_below_burn_z      
      namelist /xtra_coeff_os/ &
         xtra_coef_os_full_on, &
         xtra_coef_os_full_off, &
         xtra_coef_os_above_nonburn, &
         xtra_coef_os_below_nonburn, &
         xtra_coef_os_above_burn_h, &
         xtra_coef_os_below_burn_h, &
         xtra_coef_os_above_burn_he, &
         xtra_coef_os_below_burn_he, &
         xtra_coef_os_above_burn_z, &
         xtra_coef_os_below_burn_z, &
         xtra_dist_os_above_nonburn, &
         xtra_dist_os_below_nonburn, &
         xtra_dist_os_above_burn_h, &
         xtra_dist_os_below_burn_h, &
         xtra_dist_os_above_burn_he, &
         xtra_dist_os_below_burn_he, &
         xtra_dist_os_above_burn_z, &
         xtra_dist_os_below_burn_z
      ! end of declarations for xtra_coeff_os
      
      ! these routines are called by the standard run_star check_model

               ! Constants
      !a) Physical Constants
      REAL(dp) ::  avogad = 6.02214179D23        ! avogadro number
      REAL(dp) ::  speed_of_light   = 3.0D8      ! speed of light
      REAL(dp) ::  h_bar     = 1.055D-34
      REAL(dp) ::  G         = 6.67D-11          ! Gravitational Constant
      REAL(dp) ::  m_p       = 1.6726219D-27     ! Mass of proton
      REAL(dp) ::  M_sun     = 1.989D30          ! Mass of the sun
      REAL(dp) ::  R_sun     = 696340000         ! Radius of the sun
      REAL(dp) ::  light_year= 9.4607D15         ! L ight Year
      REAL(dp) ::  Gev_c2
      REAL(dp) ::  parsec    = 3.0857D16         ! Parsec

      REAL(dp) ::  At_h1    = 1.0            ! Atomic number of h1
      REAL(dp) ::  At_he4   = 2.0            ! Atomic number of he4_cap_rate
      REAL(dp) ::  At_c12   = 6.0            ! Atomic number of c12
      REAL(dp) ::  At_n14   = 7.0            ! Atomic number of n14
      REAL(dp) ::  At_o16   = 8.0            ! Atomic number of o16
      REAL(dp) ::  At_ne20  = 10.0           ! Atomic number of ne20_cap_rate
      REAL(dp) ::  At_mg24  = 12.0           ! Atomic number of mg24

      ! b) Dark Matter related parameters
      REAL(dp) ::  Total_Capture_Rate
      REAL(dp) ::  arg_luminosity
      REAL(dp) ::  extra_luminosity
      REAL(dp) ::  average_extra_luminosity
      REAL(dp) ::  M_wimp
      REAL(dp) ::  rho_wimp
      REAL(dp) ::  sigma_SI
      REAL(dp) ::  sigma_SD
      REAL(dp) ::  Extra_Luminosity_WIMP

      ! c) Star parameters
      REAL(dp) :: v_esca      ! escape velocity
      REAL(dp) :: v_ave_x     !
      REAL(dp) :: v_star      !

      ! d) Other constants
      Real(dp) :: part1_h1
      Real(dp) :: part1_he4
      Real(dp) :: part1_c12
      Real(dp) :: part1_n14
      Real(dp) :: part1_o16
      Real(dp) :: part1_ne20
      Real(dp) :: part1_mg24

      Real(dp) :: part2_h1
      Real(dp) :: part2_he4
      Real(dp) :: part2_c12
      Real(dp) :: part2_n14
      Real(dp) :: part2_o16
      Real(dp) :: part2_ne20
      Real(dp) :: part2_mg24

      REAL(dp) :: part3_h1
      REAL(dp) :: part3_he4
      REAL(dp) :: part3_c12
      REAL(dp) :: part3_n14
      REAL(dp) :: part3_o16
      REAL(dp) :: part3_ne20
      REAL(dp) :: part3_mg24

      Real(dp) :: part4_h1
      Real(dp) :: part4_he4
      Real(dp) :: part4_c12
      Real(dp) :: part4_n14
      Real(dp) :: part4_o16
      Real(dp) :: part4_ne20
      Real(dp) :: part4_mg24

      REAL(dp) ::  m_n_h1
      REAL(dp) ::  m_n_he4
      REAL(dp) ::  m_n_c12
      REAL(dp) ::  m_n_n14
      REAL(dp) ::  m_n_o16
      REAL(dp) ::  m_n_ne20
      REAL(dp) ::  m_n_mg24

      REAL(dp) :: capture_rate_h1
      REAL(dp) :: capture_rate_he4
      REAL(dp) :: capture_rate_c12
      REAL(dp) :: capture_rate_n14
      REAL(dp) :: capture_rate_o16
      REAL(dp) :: capture_rate_ne20
      REAL(dp) :: capture_rate_mg24
      
      REAL(dp) :: E_0_he4
      REAL(dp) :: E_0_c12
      REAL(dp) :: E_0_n14
      REAL(dp) :: E_0_o16
      REAL(dp) :: E_0_ne20
      REAL(dp) :: E_0_mg24

      REAL(dp) :: R_0_he4
      REAL(dp) :: R_0_c12
      REAL(dp) :: R_0_n14
      REAL(dp) :: R_0_o16
      REAL(dp) :: R_0_ne20
      REAL(dp) :: R_0_mg24

      REAL(dp) :: u_max_h1
      REAL(dp) :: u_max_he4
      REAL(dp) :: u_max_c12
      REAL(dp) :: u_max_n14
      REAL(dp) :: u_max_o16
      REAL(dp) :: u_max_ne20
      REAL(dp) :: u_max_mg24

      REAL(dp) :: mu_posi_h1
      REAL(dp) :: mu_posi_he4
      REAL(dp) :: mu_posi_c12
      REAL(dp) :: mu_posi_n14
      REAL(dp) :: mu_posi_o16
      REAL(dp) :: mu_posi_ne20
      REAL(dp) :: mu_posi_mg24

      REAL(dp) :: mu_nega_h1
      REAL(dp) :: mu_nega_he4
      REAL(dp) :: mu_nega_c12
      REAL(dp) :: mu_nega_n14
      REAL(dp) :: mu_nega_o16
      REAL(dp) :: mu_nega_ne20
      REAL(dp) :: mu_nega_mg24

      REAL(dp) :: mu_h1
      REAL(dp) :: mu_he4
      REAL(dp) :: mu_c12
      REAL(dp) :: mu_n14
      REAL(dp) :: mu_o16
      REAL(dp) :: mu_ne20
      REAL(dp) :: mu_mg24

      integer  :: n_Monte_Carlo_h1
      integer  :: n_Monte_Carlo_he4
      integer  :: n_Monte_Carlo_c12
      integer  :: n_Monte_Carlo_n14
      integer  :: n_Monte_Carlo_o16
      integer  :: n_Monte_Carlo_ne20
      integer  :: n_Monte_Carlo_mg24

      common avogad, speed_of_light, h_bar, G, m_p, M_sun, R_sun, light_year, Gev_c2, parsec
      common part1_h1, part1_he4, part1_c12, part1_n14, part1_o16, part1_ne20, part1_mg24
      common part2_h1, part2_he4, part2_c12, part2_n14, part2_o16, part2_ne20, part2_mg24
      common part3_h1, part3_he4, part3_c12, part3_n14, part3_o16, part3_ne20, part3_mg24
      common part4_h1, part4_he4, part4_c12, part4_n14, part4_o16, part4_ne20, part4_mg24
      common mu_h1, mu_he4, mu_c12, mu_n14, mu_o16, mu_ne20, mu_mg24
      common m_n_h1, m_n_he4, m_n_c12, m_n_n14, m_n_o16, m_n_ne20, m_n_mg24
      common At_h1, At_he4, At_c12, At_n14, At_o16, At_ne20, At_mg24
      common capture_rate_h1, capture_rate_he4, capture_rate_c12, capture_rate_n14, capture_rate_o16, capture_rate_ne20, capture_rate_mg24
      common E_0_he4, E_0_c12, E_0_n14, E_0_o16, E_0_ne20, E_0_mg24
      common R_0_he4, R_0_c12, R_0_n14, R_0_o16, R_0_ne20, R_0_mg24
      common u_max_h1, u_max_he4, u_max_c12, u_max_n14, u_max_o16, u_max_ne20, u_max_mg24
      common mu_posi_h1, mu_posi_he4, mu_posi_c12, mu_posi_n14, mu_posi_o16, mu_posi_ne20, mu_posi_mg24
      common mu_nega_h1, mu_nega_he4, mu_nega_c12, mu_nega_n14, mu_nega_o16, mu_nega_ne20, mu_nega_mg24
      common v_esca, v_ave_x, v_star
      common Total_Capture_Rate, arg_luminosity, extra_luminosity, average_extra_luminosity, M_wimp, rho_wimp, sigma_SI, sigma_SD, Extra_Luminosity_WIMP
      common n_Monte_Carlo_h1, n_Monte_Carlo_he4, n_Monte_Carlo_c12, n_Monte_Carlo_n14, n_Monte_Carlo_o16, n_Monte_Carlo_ne20, n_Monte_Carlo_mg24


      contains

      include "test_suite_extras.inc"
      
      subroutine extras_controls(id, ierr)
         integer, intent(in) :: id
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         
         if (s% use_other_mesh_delta_coeff_factor) then ! setup for xtra_coeff_os
            call read_inlist_xtra_coeff_os(ierr)
            if (ierr /= 0) return
            s% other_mesh_delta_coeff_factor => other_mesh_delta_coeff_factor
         end if
         
         s% other_energy => WIMPs_other_energy

         s% extras_startup => extras_startup
         s% extras_check_model => extras_check_model
         s% extras_finish_step => extras_finish_step
         s% extras_after_evolve => extras_after_evolve
         s% how_many_extra_history_columns => how_many_extra_history_columns
         s% data_for_extra_history_columns => data_for_extra_history_columns
         s% how_many_extra_profile_columns => how_many_extra_profile_columns
         s% data_for_extra_profile_columns => data_for_extra_profile_columns  
      end subroutine extras_controls


  !****     code for xtra_coeff_os
      
      subroutine read_inlist_xtra_coeff_os(ierr)
         use utils_lib
         integer, intent(out) :: ierr
         character (len=256) :: filename, message
         integer :: unit
         
         filename = 'inlist_xtra_coeff_os'
         
         write(*,*) 'read_inlist_xtra_coeff_os'
         
         ! set defaults
         xtra_coef_os_full_on = 1d-4
         xtra_coef_os_full_off = 0.1d0
         xtra_coef_os_above_nonburn = 1d0
         xtra_coef_os_below_nonburn = 1d0
         xtra_coef_os_above_burn_h = 1d0
         xtra_coef_os_below_burn_h = 1d0
         xtra_coef_os_above_burn_he = 1d0
         xtra_coef_os_below_burn_he = 1d0
         xtra_coef_os_above_burn_z = 1d0
         xtra_coef_os_below_burn_z = 1d0
         xtra_dist_os_above_nonburn = 0.2d0
         xtra_dist_os_below_nonburn = 0.2d0   
         xtra_dist_os_above_burn_h = 0.2d0
         xtra_dist_os_below_burn_h = 0.2d0   
         xtra_dist_os_above_burn_he = 0.2d0
         xtra_dist_os_below_burn_he = 0.2d0   
         xtra_dist_os_above_burn_z = 0.2d0
         xtra_dist_os_below_burn_z = 0.2d0

         open(newunit=unit, file=trim(filename), action='read', delim='quote', iostat=ierr)
         if (ierr /= 0) then
            write(*, *) 'Failed to open control namelist file ', trim(filename)
         else
            read(unit, nml=xtra_coeff_os, iostat=ierr)  
            close(unit)
            if (ierr /= 0) then
               write(*, *) 'Failed while trying to read control namelist file ', trim(filename)
               write(*, '(a)') &
                  'The following runtime error message might help you find the problem'
               write(*, *) 
               open(newunit=unit, file=trim(filename), action='read', delim='quote', status='old', iostat=ierr)
               read(unit, nml=xtra_coeff_os)
               close(unit)
            end if  
         end if

      end subroutine read_inlist_xtra_coeff_os


      subroutine other_mesh_delta_coeff_factor(id, eps_h, eps_he, eps_z, ierr)
         use const_def
         use math_lib
         use chem_def
         integer, intent(in) :: id
         real(dp), intent(in), dimension(:) :: eps_h, eps_he, eps_z
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         real(dp) :: he_cntr, full_off, full_on, alfa_os
         integer :: k, kk, nz, max_eps_loc
         real(dp) :: xtra_coef, xtra_dist, coef, Hp, r_extra, max_eps, eps
         logical :: in_convective_region
         logical, parameter :: dbg = .false.
         
         include 'formats'
         
         !write(*,*) 'enter other_mesh_delta_coeff_factor'
         ierr = 0
         if (xtra_coef_os_above_nonburn == 1d0 .and. &
             xtra_coef_os_below_nonburn == 1d0 .and. &
             xtra_coef_os_above_burn_h == 1d0 .and. &
             xtra_coef_os_below_burn_h == 1d0 .and. &
             xtra_coef_os_above_burn_he == 1d0 .and. &
             xtra_coef_os_below_burn_he == 1d0 .and. &
             xtra_coef_os_above_burn_z == 1d0 .and. &
             xtra_coef_os_below_burn_z == 1d0) return
             
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return

         nz = s% nz
         he_cntr = s% xa(s% net_iso(ihe4),nz)
         full_off = xtra_coef_os_full_off
         full_on = xtra_coef_os_full_on
         if (he_cntr >= full_off) then
            alfa_os = 0
         else if (he_cntr <= full_on) then
            alfa_os = 1
         else
            alfa_os = (full_off - he_cntr)/(full_off - full_on)
         end if
         !write(*,1) 'alfa_os', alfa_os
         if (alfa_os == 0) return

         ! first go from surface to center doing below convective boundaries
         in_convective_region = (s% mixing_type(1) == convective_mixing)
         k = 2
         max_eps = -1d99
         max_eps_loc = -1
         do while (k <= nz)
            eps = eps_h(k) + eps_he(k) + eps_z(k)
            if (in_convective_region) then
               if (s% mixing_type(k) == convective_mixing) then
                  if (eps > max_eps) then
                     max_eps = eps
                     max_eps_loc = k
                  end if
               else
                  in_convective_region = .false.
                  if (max_eps < 1d0) then
                     xtra_coef = xtra_coef_os_below_nonburn
                     xtra_dist = xtra_dist_os_below_nonburn
                  else if (eps_h(max_eps_loc) > 0.5d0*max_eps) then
                     xtra_coef = xtra_coef_os_below_burn_h
                     xtra_dist = xtra_dist_os_below_burn_h
                  else if (eps_he(max_eps_loc) > 0.5d0*max_eps) then
                     xtra_coef = xtra_coef_os_below_burn_he
                     xtra_dist = xtra_dist_os_below_burn_he
                  else
                     xtra_coef = xtra_coef_os_below_burn_z
                     xtra_dist = xtra_dist_os_below_burn_z
                  end if
                  xtra_coef = xtra_coef*alfa_os + (1-alfa_os)
                  if (xtra_coef > 0 .and. xtra_coef /= 1) then
                     coef = xtra_coef
                     do
                        if (s% mixing_type(k) /= overshoot_mixing) exit
                        if (coef < s% mesh_delta_coeff_factor(k)) then
                           s% mesh_delta_coeff_factor(k) = coef
                           !write(*,2) 'below mesh_delta_coeff_factor(k)', &
                           !   k, s% mesh_delta_coeff_factor(k)
                        end if
                        if (k == nz) exit
                        k = k+1
                     end do
                     if (xtra_dist > 0) then
                        Hp = s% P(k)/(s% rho(k)*s% grav(k))
                        r_extra = max(0d0, s% r(k) - xtra_dist*Hp)
                        if (dbg) write(*,2) 'extra below overshoot region', &
                           k, s% r(k)/Rsun, Hp/Rsun, r_extra/Rsun
                        do
                           if (s% r(k) < r_extra) exit
                           if (coef < s% mesh_delta_coeff_factor(k)) then
                              s% mesh_delta_coeff_factor(k) = coef
                              !write(*,2) 'extra below mesh_delta_coeff_factor(k)', &
                              !   k, s% mesh_delta_coeff_factor(k)
                           end if
                           if (k == nz) exit
                           k = k+1
                        end do
                     end if
                  end if
                  if (dbg) write(*,2) 'done with extra below overshoot region', k
                  if (dbg) write(*,*)
               end if
            else if (s% mixing_type(k) == convective_mixing) then
               in_convective_region = .true.
               max_eps = eps
               max_eps_loc = k
            end if
            k = k+1
         end do

         ! now go from center to surface doing above convective boundaries
         in_convective_region = (s% mixing_type(nz) == convective_mixing)
         k = nz-1
         max_eps = -1d99
         max_eps_loc = -1
         do while (k >= 1)
            eps = eps_h(k) + eps_he(k) + eps_z(k)
            if (in_convective_region) then
               if (s% mixing_type(k) == convective_mixing) then
                  if (eps > max_eps) then
                     max_eps = eps
                     max_eps_loc = k
                  end if
               else
                  in_convective_region = .false.
                  if (max_eps < 1d0) then
                     xtra_coef = xtra_coef_os_above_nonburn
                     xtra_dist = xtra_dist_os_above_nonburn
                  else if (eps_h(max_eps_loc) > 0.5d0*max_eps) then
                     xtra_coef = xtra_coef_os_above_burn_h
                     xtra_dist = xtra_dist_os_above_burn_h
                  else if (eps_he(max_eps_loc) > 0.5d0*max_eps) then
                     xtra_coef = xtra_coef_os_above_burn_he
                     xtra_dist = xtra_dist_os_above_burn_he
                  else
                     xtra_coef = xtra_coef_os_above_burn_z
                     xtra_dist = xtra_dist_os_above_burn_z
                  end if
                  xtra_coef = xtra_coef*alfa_os + (1-alfa_os)
                  if (dbg) write(*,2) 'xtra_coeff to surf', s% model_number, xtra_coef

                  if (xtra_coef > 0 .and. xtra_coef /= 1) then
                     coef = xtra_coef
                     do
                        if (s% mixing_type(k) /= overshoot_mixing) exit
                        if (coef < s% mesh_delta_coeff_factor(k)) then
                           s% mesh_delta_coeff_factor(k) = coef
                           !write(*,2) 'above mesh_delta_coeff_factor(k)', &
                           !   k, s% mesh_delta_coeff_factor(k)
                        end if
                        if (k == 1) exit
                        k = k-1
                     end do
                     if (xtra_dist > 0) then
                        Hp = s% P(k)/(s% rho(k)*s% grav(k))
                        r_extra = min(s% r(1), s% r(k) + xtra_dist*Hp)
                        if (dbg) write(*,2) 'extra above overshoot region', &
                           k, s% r(k)/Rsun, Hp/Rsun, r_extra/Rsun
                        do
                           if (s% r(k) > r_extra) exit
                           if (coef < s% mesh_delta_coeff_factor(k)) then
                              s% mesh_delta_coeff_factor(k) = coef
                              !write(*,2) 'extra above mesh_delta_coeff_factor(k)', &
                              !   k, s% mesh_delta_coeff_factor(k)
                           end if
                           if (k == 1) exit
                           k = k-1
                        end do
                     end if
                  end if
                  if (dbg) write(*,2) 'done with extra above overshoot region', k
                  if (dbg) write(*,*)
               end if
            else if (s% mixing_type(k) == convective_mixing) then
               in_convective_region = .true.
               max_eps = eps
               max_eps_loc = k
            end if
            k = k-1
         end do

      end subroutine other_mesh_delta_coeff_factor

  !****     end of code for xtra_coeff_os
        
      
      subroutine extras_startup(id, restart, ierr)
         integer, intent(in) :: id
         logical, intent(in) :: restart
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         call test_suite_startup(s, restart, ierr)
      end subroutine extras_startup
      
      
      subroutine extras_after_evolve(id, ierr)
         integer, intent(in) :: id
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         real(dp) :: dt
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         call test_suite_after_evolve(s, ierr)
      end subroutine extras_after_evolve
      

      ! returns either keep_going, retry, or terminate.
      integer function extras_check_model(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         extras_check_model = keep_going         
      end function extras_check_model


      integer function how_many_extra_history_columns(id)
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         how_many_extra_history_columns = 8
      end function how_many_extra_history_columns
      

      subroutine data_for_extra_history_columns(id, n, names, vals, ierr)
         use math_lib, only: safe_log10
         use chem_def, only: ih1, ihe3, ihe4, ic12, in14, io16, ine20, img24
   
         integer, intent(in) :: id, n
         character (len=maxlen_history_column_name) :: names(n)
         real(dp) :: vals(n)
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         real(dp), parameter :: frac = 0.90
         integer :: i, k
         real(dp) :: edot, edot_partial
   
         ! Calculating some parameters that are constants during the run!
         Gev_c2      = 1.78D-27
   
         m_n_h1      = 0.9387597 * Gev_c2
         m_n_he4     = 3.7284    * Gev_c2
         m_n_c12     = 11.269585 * Gev_c2
         m_n_n14     = 13.047    * Gev_c2
         m_n_o16     = 15.011    * Gev_c2
         m_n_ne20    = 18.93     * Gev_c2
         m_n_mg24    = 22.805    * Gev_c2
   
         ! Dark matter Properties
         M_wimp       = 1D2 * Gev_c2
         rho_wimp     = 1D-2 * 8D-3  * (M_sun / parsec**3)
         sigma_SI     = 1D-48
         sigma_SD     = 1D-42
         v_esca       = 600D3 !sqrt((2*G*(s% star_mass))/(10**(s% log_surface_radius)))
         v_ave_x      = 270D3
         v_star       = 220D3
   
         mu_h1   = M_wimp / m_n_h1
         mu_he4  = M_wimp / m_n_he4
         mu_c12  = M_wimp / m_n_c12
         mu_n14  = M_wimp / m_n_n14
         mu_o16  = M_wimp / m_n_o16
         mu_ne20 = M_wimp / m_n_ne20
         mu_mg24 = M_wimp / m_n_mg24
   
         mu_nega_h1   = (mu_h1   - 1) / 2
         mu_nega_he4  = (mu_he4  - 1) / 2
         mu_nega_c12  = (mu_c12  - 1) / 2
         mu_nega_n14  = (mu_n14  - 1) / 2
         mu_nega_o16  = (mu_o16  - 1) / 2
         mu_nega_ne20 = (mu_ne20 - 1) / 2
         mu_nega_mg24 = (mu_mg24 - 1) / 2
   
         mu_posi_h1   = (mu_h1   - 1) / 2
         mu_posi_he4  = (mu_he4  - 1) / 2
         mu_posi_c12  = (mu_c12  - 1) / 2
         mu_posi_n14  = (mu_n14  - 1) / 2
         mu_posi_o16  = (mu_o16  - 1) / 2
         mu_posi_ne20 = (mu_ne20 - 1) / 2
         mu_posi_mg24 = (mu_mg24 - 1) / 2
   
         u_max_h1   = (v_esca * sqrt(mu_h1))   / mu_nega_h1
         u_max_he4  = (v_esca * sqrt(mu_he4))  / mu_nega_he4
         u_max_c12  = (v_esca * sqrt(mu_c12))  / mu_nega_c12
         u_max_n14  = (v_esca * sqrt(mu_n14))  / mu_nega_n14
         u_max_o16  = (v_esca * sqrt(mu_o16))  / mu_nega_o16
         u_max_ne20 = (v_esca * sqrt(mu_ne20)) / mu_nega_ne20
         u_max_mg24 = (v_esca * sqrt(mu_mg24)) / mu_nega_mg24
   
         R_0_he4 = ((0.91*((m_n_he4/Gev_c2)**0.3333)+0.3)*1e-15)**2
         E_0_he4 = (3*(h_bar**2)) / (2*(m_n_he4)*R_0_he4)
         R_0_c12 = ((0.91*((m_n_c12/Gev_c2)**0.3333)+0.3)*1e-15)**2
         E_0_c12 = (3*(h_bar**2)) / (2*(m_n_c12)*R_0_c12)
         R_0_n14 = ((0.91*((m_n_n14/Gev_c2)**0.3333)+0.3)*1e-15)**2
         E_0_n14 = (3*(h_bar**2)) / (2*(m_n_n14)*R_0_n14)
         R_0_o16 = ((0.91*((m_n_o16/Gev_c2)**0.3333)+0.3)*1e-15)**2
         E_0_o16 = (3*(h_bar**2)) / (2*(m_n_o16)*R_0_o16)
         R_0_ne20 = ((0.91*((m_n_ne20/Gev_c2)**0.3333)+0.3)*1e-15)**2
         E_0_ne20 = (3*(h_bar**2)) / (2*(m_n_ne20)*R_0_ne20)
         R_0_mg24 = ((0.91*((m_n_mg24/Gev_c2)**0.3333)+0.3)*1e-15)**2
         E_0_mg24 = (3*(h_bar**2)) / (2*(m_n_mg24)*R_0_mg24)
        
         ! To calculate Capture rateby Hydrogen atoms, we used equation 11 of the paper: MNRAS 503, 458–471 (2021)_DOI: 10.1093/mnras/stab256, Title: "Capture rate of weakly interacting massive particles (WIMPs) in binary star systems". We calculated each bracket in equation 11 seperately and then multiplayed them.
   
         ! Calculating Brackets of equation 11
         ! Bracket 1 - h1
         part1_h1 = (4 * sqrt(6*pi) * rho_wimp * exp((-3*(v_star**2)) / (2*(v_ave_x**2))) ) &
         / (M_wimp*v_ave_x*v_star)
   
         ! Bracket 2 - h1
         part2_h1 = 1D-48 + 1D-42
   
         ! Bracket 3 - h1
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
   
         edot = dot_product(s% dm(1:s% nz), s% eps_nuc(1:s% nz))
   
         edot_partial = 0
         do i = s% nz, 1, -1
         edot_partial = edot_partial + s% dm(i) * s% eps_nuc(i)
         if (edot_partial .ge. (frac * edot)) exit
         end do
         
         part3_h1 = 0
         do i = s% nz, 2, -1
         part3_h1 = part3_h1 + ((avogad * (s% rho(i)) &
         *(s% xa(s% net_iso(ih1),i)) &
         / (At_h1))) * ((s% r(i))**2) * ((s% r(i-1)) - (s% r(i)))
         end do
         part3_h1 = part3_h1 + part3_h1 + ((avogad * (s% rho(1)) &
         *(s% xa(s% net_iso(ih1),1)) &
         / (At_h1))) * ((s% r(1))**2) * ((s% r(1)) - (s% r(2)))
   
         ! Bracket  4 h1
         n_Monte_Carlo_h1 = 1000
         call Monte_Carlo_integration_Hydrogen(n_Monte_Carlo_h1,u_max_h1)
   
         capture_rate_h1 = part1_h1 * part2_h1 * part3_h1 * part4_h1
         !PRINT*, 'capture_rate_h1=', capture_rate_h1
   
      
         ! Calculating Brackets of equation 12 for he4
         ! Bracket 1 - he4
         part1_he4 = (8 * sqrt(6*pi) * rho_wimp * E_0_he4 * (mu_posi_he4**2) * exp((-3*(v_star**2)) / (2*(v_ave_x**2))) ) &
         / ((M_wimp**2)*v_ave_x*v_star*mu_he4)
   
         ! Bracket 2 - he4
         part2_he4 = sigma_SI * (At_he4**2) * ((M_wimp*m_n_he4)/(M_wimp+m_n_he4))**2 * ((M_wimp+m_p)/(M_wimp*m_p))**2
   
         ! Bracket 3 - he4 
         part3_he4 = 0
         do i = s% nz, 2, -1
         part3_he4 = part3_he4 + ((avogad * (s% rho(i)) &
         *(s% xa(s% net_iso(ihe4),i)) &
         / (At_he4))) * ((s% r(i))**2) * ((s% r(i-1)) - (s% r(i)))
         end do
         part3_he4 = part3_he4 + ((avogad * (s% rho(1)) &
         *(s% xa(s% net_iso(ihe4),1)) &
         / (At_he4))) * ((s% r(1))**2) * ((s% r(1)) - (s% r(2)))
   
         ! Bracket 4 - he4
         n_Monte_Carlo_he4 = 1000
         call Monte_Carlo_integration_he4(n_Monte_Carlo_he4,u_max_he4)
   
         capture_rate_he4 = part1_he4 * part2_he4 * part3_he4 * part4_he4
         !PRINT*, 'capture_rate_he4=', capture_rate_he4
        
   
         ! Calculating Brackets of equation 12 for c12
         ! Bracket 1 - c12
         part1_c12 = (8 * sqrt(6*pi) * rho_wimp * E_0_c12 * (mu_posi_c12**2) * exp((-3*(v_star**2)) / (2*(v_ave_x**2))) ) &
         / ((M_wimp**2)*v_ave_x*v_star*mu_c12)
   
         ! Bracket 2 - c12
         part2_c12 = sigma_SI * (At_c12**2) * ((M_wimp*m_n_c12)/(M_wimp+m_n_c12))**2 * ((M_wimp+m_p)/(M_wimp*m_p))**2
   
         ! Bracket 3 - c12
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
   
         edot = dot_product(s% dm(1:s% nz), s% eps_nuc(1:s% nz))
   
         edot_partial = 0
         do i = s% nz, 1, -1
         edot_partial = edot_partial + s% dm(i) * s% eps_nuc(i)
         if (edot_partial .ge. (frac * edot)) exit
         end do
         
         part3_c12 = 0
         do i = s% nz, 2, -1
         part3_c12 = part3_c12 + ((avogad * (s% rho(i)) &
         *(s% xa(s% net_iso(ic12),i)) &
         / (At_c12))) * ((s% r(i))**2) * ((s% r(i-1)) - (s% r(i)))
         end do
         part3_c12 = part3_c12 + ((avogad * (s% rho(1)) &
         *(s% xa(s% net_iso(ic12),1)) &
         / (At_c12))) * ((s% r(1))**2) * ((s% r(1)) - (s% r(2)))
   
         ! Bracket 4 - c12
         n_Monte_Carlo_c12 = 1000
         call Monte_Carlo_integration_c12(n_Monte_Carlo_c12,u_max_c12)
   
         capture_rate_c12 = part1_c12 * part2_c12 * part3_c12 * part4_c12
         !PRINT*, 'capture_rate_c12=', capture_rate_c12
   
         ! Calculating Brackets of equation 12 for n14
         ! Bracket 1 - n14
         part1_n14 = (8 * sqrt(6*pi) * rho_wimp * E_0_n14 * (mu_posi_n14**2) * exp((-3*(v_star**2)) / (2*(v_ave_x**2))) ) &
         / ((M_wimp**2)*v_ave_x*v_star*mu_n14)
   
         ! Bracket 2 - n14
         part2_n14 = sigma_SI * (At_n14**2) * ((M_wimp*m_n_n14)/(M_wimp+m_n_n14))**2 * ((M_wimp+m_p)/(M_wimp*m_p))**2
   
         ! part_3_n14      
         part3_n14 = 0
         do i = s% nz, 2, -1
         part3_n14 = part3_n14 + ((avogad * (s% rho(i)) &
         *(s% xa(s% net_iso(in14),i)) &
         / (At_n14))) * ((s% r(i))**2) * ((s% r(i-1)) - (s% r(i)))
         end do
         part3_n14 = part3_n14 + ((avogad * (s% rho(1)) &
         *(s% xa(s% net_iso(in14),1)) &
         / (At_n14))) * ((s% r(1))**2) * ((s% r(1)) - (s% r(2)))
   
         ! Bracket 4 - n14
         n_Monte_Carlo_n14 = 1000
         call Monte_Carlo_integration_n14(n_Monte_Carlo_n14,u_max_n14)
   
         capture_rate_n14 = part1_n14 * part2_n14 * part3_n14 * part4_n14
         !PRINT*, 'capture_rate_n14=', capture_rate_n14
   
         ! Calculating Brackets of equation 12 for o16
         ! Bracket 1 - o16
         part1_o16 = (8 * sqrt(6*pi) * rho_wimp * E_0_o16 * (mu_posi_o16**2) * exp((-3*(v_star**2)) / (2*(v_ave_x**2))) ) &
         / ((M_wimp**2)*v_ave_x*v_star*mu_o16)
   
         ! Bracket 2 - o16
         part2_o16 = sigma_SI * (At_o16**2) * ((M_wimp*m_n_o16)/(M_wimp+m_n_o16))**2 * ((M_wimp+m_p)/(M_wimp*m_p))**2
   
         ! part_3_o16      
         part3_o16 = 0
         do i = s% nz, 2, -1
         part3_o16 = part3_o16 + ((avogad * (s% rho(i)) &
         *(s% xa(s% net_iso(io16),i)) &
         / (At_o16))) * ((s% r(i))**2) * ((s% r(i-1)) - (s% r(i)))
         end do
         part3_o16 = part3_o16 + ((avogad * (s% rho(1)) &
         *(s% xa(s% net_iso(io16),1)) &
         / (At_o16))) * ((s% r(1))**2) * ((s% r(1)) - (s% r(2)))
   
         ! Bracket 4 - o16
         n_Monte_Carlo_o16 = 1000
         call Monte_Carlo_integration_o16(n_Monte_Carlo_o16,u_max_o16)
   
         capture_rate_o16 = part1_o16 * part2_o16 * part3_o16 * part4_o16
         !PRINT*, 'capture_rate_o16=', capture_rate_o16
   
         ! Calculating Brackets of equation 12 for ne20
         ! Bracket 1 - ne20
         part1_ne20 = (8 * sqrt(6*pi) * rho_wimp * E_0_ne20 * (mu_posi_ne20**2) * exp((-3*(v_star**2)) / (2*(v_ave_x**2))) ) &
         / ((M_wimp**2)*v_ave_x*v_star*mu_ne20)
   
         ! Bracket 2 - ne20
         part2_ne20 = sigma_SI * (At_ne20**2) * ((M_wimp*m_n_ne20)/(M_wimp+m_n_ne20))**2 * ((M_wimp+m_p)/(M_wimp*m_p))**2
         
         ! part_3_ne20      
         part3_ne20 = 0
         do i = s% nz, 2, -1
         part3_ne20 = part3_ne20 + ((avogad * (s% rho(i)) &
         *(s% xa(s% net_iso(ine20),i)) &
         / (At_ne20))) * ((s% r(i))**2) * ((s% r(i-1)) - (s% r(i)))
         end do
         part3_ne20 = part3_ne20 + ((avogad * (s% rho(1)) &
         *(s% xa(s% net_iso(ine20),1)) &
         / (At_ne20))) * ((s% r(1))**2) * ((s% r(1)) - (s% r(2)))
   
         ! Bracket 4 - ne20
         n_Monte_Carlo_ne20 = 1000
         call Monte_Carlo_integration_ne20(n_Monte_Carlo_ne20,u_max_ne20)
   
         capture_rate_ne20 = part1_ne20 * part2_ne20 * part3_ne20 * part4_ne20
         !PRINT*, 'capture_rate_ne20=', capture_rate_ne20
       
         ! Calculating Brackets of equation 12 for mg24
         ! Bracket 1 - mg24
         part1_mg24 = (8 * sqrt(6*pi) * rho_wimp * E_0_mg24 * (mu_posi_mg24**2) * exp((-3*(v_star**2)) / (2*(v_ave_x**2))) ) &
         / ((M_wimp**2)*v_ave_x*v_star*mu_mg24)
   
         ! Bracket 2 - mg24
         part2_mg24 = sigma_SI * (At_mg24**2) * ((M_wimp*m_n_mg24)/(M_wimp+m_n_mg24))**2 * ((M_wimp+m_p)/(M_wimp*m_p))**2
   
         ! part_3_mg24      
         part3_mg24 = 0
         do i = s% nz, 2, -1
         part3_mg24 = part3_mg24 + ((avogad * (s% rho(i)) &
         *(s% xa(s% net_iso(img24),i)) &
         / (At_mg24))) * ((s% r(i))**2) * ((s% r(i-1)) - (s% r(i)))
         end do
         part3_mg24 = part3_mg24 + ((avogad * (s% rho(1)) &
         *(s% xa(s% net_iso(img24),1)) &
         / (At_mg24))) * ((s% r(1))**2) * ((s% r(1)) - (s% r(2)))
   
         ! Bracket 4 - mg24
         n_Monte_Carlo_mg24 = 1000
         call Monte_Carlo_integration_mg24(n_Monte_Carlo_mg24,u_max_mg24)
   
   
         capture_rate_mg24 = part1_mg24 * part2_mg24 * part3_mg24 * part4_mg24
         !PRINT*, 'capture_rate_mg24=', capture_rate_mg24
         

         Total_Capture_Rate = capture_rate_h1 + capture_rate_he4 + capture_rate_c12 + capture_rate_n14 &
         + capture_rate_o16 + capture_rate_ne20 + capture_rate_mg24
   
         !PRINT*, 'Total_Capture_Rate=', Total_Capture_Rate
   
         !*** Extra Luminosity throuth WIMPs annihilation
         ! By deviding annihilated-energy to (s% time_step) Energy will be converted to luminosity.
         Extra_Luminosity_WIMP = ((Total_Capture_Rate * M_wimp * (speed_of_light**2)) / ((s% star_mass)*M_sun))* 1D4 * (s% time_step) * (365 * 24 * 3600)!/ (3D9)!(s% time_step)
         !PRINT*, 'Extra_Luminosity_WIMP=', Extra_Luminosity_WIMP
        
         !*********
        
         ! column Capture-rate by h1
         names(1) = "CR_h1"
         vals(1)  =  capture_rate_h1
         !PRINT*, 'capture_rate_h1=', vals(1)
         
         ! column Capture-rate by he4
         names(2) = "CR_he4"
         vals(2)  = capture_rate_he4
         !PRINT*, 'capture_rate_he4', vals(2)
         
         ! column Capture-rate by c12
         names(3) = "CR_c12"
         vals(3)  = capture_rate_c12
         !PRINT*, 'capture_rate_c12=', vals(3)
         
         ! column Capture-rate by n14
         names(4) = "CR_n14"
         vals(4)  = capture_rate_n14
         !PRINT*, 'capture_rate_c12=', vals(4) 
         
         ! column Capture-rate by o16
         names(5) = "CR_o16"
         vals(5)  = capture_rate_o16
         !PRINT*, 'capture_rate_c12=', vals(5)  
         
         ! column Capture-rate by ne20
         names(6) = "CR_ne20"
         vals(6)  = capture_rate_ne20
         !PRINT*, 'capture_rate_c12=', vals(6)  
         
         ! column Capture-rate by mg24
         names(7) = "CR_mg24"
         vals(7)  = capture_rate_mg24
         !PRINT*, 'capture_rate_c12=', vals(7) 
   
         ! Extra Luminosity through WIMP annihilation
         names(8) = "L_WIMP"
         vals(8)  = Extra_Luminosity_WIMP
         !PRINT*, 'Extra_Luminosity_WIMP=', vals(8) 
   
         ierr = 0
         end subroutine data_for_extra_history_columns

      
      integer function how_many_extra_profile_columns(id)
         use star_def, only: star_info
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         how_many_extra_profile_columns = 0
      end function how_many_extra_profile_columns
      
      
      subroutine data_for_extra_profile_columns(id, n, nz, names, vals, ierr)
         use star_def, only: star_info, maxlen_profile_column_name
         use const_def, only: dp
         integer, intent(in) :: id, n, nz
         character (len=maxlen_profile_column_name) :: names(n)
         real(dp) :: vals(nz,n)
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         integer :: k
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
      end subroutine data_for_extra_profile_columns
      

      ! returns either keep_going, retry, or terminate.
      integer function extras_finish_step(id)
         use chem_def
         integer, intent(in) :: id
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         extras_finish_step = keep_going
      end function extras_finish_step
      
      integer function how_many_extra_history_header_items(id)
      integer, intent(in) :: id
      integer :: ierr
      type (star_info), pointer :: s
      ierr = 0
      call star_ptr(id, s, ierr)
      if (ierr /= 0) return
      how_many_extra_history_header_items = 8
   end function how_many_extra_history_header_items

   subroutine WIMPs_other_energy(id, ierr)
      integer, intent(in) :: id
      integer, intent(out) :: ierr
      type (star_info), pointer :: s
      integer :: k
      ierr = 0
      call star_ptr(id, s, ierr)
      if (ierr /= 0) return
     
      !average_extra_luminosity = Extra_Luminosity_WIMP / (s% nz)

      do k = 1, int( (s% nz) / 7.0 )
         s% extra_heat(k) = 7.0 * (Extra_Luminosity_WIMP) !/ ((s% time_step)*(365*24*3600))
      end do

      !PRINT*, 'extra_heat          =', s% extra_heat(5)

    end subroutine WIMPs_other_energy


    subroutine Monte_Carlo_integration_Hydrogen(n_Monte_Carlo_h1,u_max_h1)
      implicit none
      integer, intent(in) :: n_Monte_Carlo_h1
      real(dp), intent(in) :: u_max_h1
      real(dp) :: u, integral_err
      real(dp) :: f
      integer :: i

      part4_h1 = 0.0
      f= 0.0d0

      do i=1,n_Monte_Carlo_h1
         call random_number(u)
         u=u*u_max_h1  ! random_number only returns uniformly distributed from [0.0, 1.0]
         f = f+integrand_h1(u)
      end do

      f=f/n_Monte_Carlo_h1
      part4_h1 = (u_max_h1-0.0)*f
      !write (*,*) "# MC integration_h1 = ",part4_h1
    end subroutine Monte_Carlo_integration_Hydrogen

    function integrand_h1(u) result (value)
      implicit none
      real(dp) :: u
      real(dp) :: value

      if (u .lt. 0.00001) then
         u = 0.00001
      end if

      value = exp((-3*u**2)/(2*v_ave_x**2)) * sinh((3*u*v_star)/(v_ave_x**2)) * (v_esca **2 - ((mu_nega_h1**2)*(u**2))/(mu_h1))
    end function integrand_h1

    subroutine Monte_Carlo_integration_he4(n_Monte_Carlo_he4,u_max_he4)
      implicit none
      integer, intent(in) :: n_Monte_Carlo_he4
      real(dp), intent(in) :: u_max_he4
      real(dp) :: u, integral_err
      real(dp) :: f
      integer :: i

      part4_he4 = 0.0
      f= 0.0d0

      do i=1,n_Monte_Carlo_he4
         call random_number(u)
         u=u*u_max_he4
         f = f+integrand_he4(u)
      end do

      f=f/n_Monte_Carlo_he4
      part4_he4 = (u_max_he4-0.0)*f
      !write (*,*) "# MC integration_he4 = ",part4_he4
    end subroutine Monte_Carlo_integration_he4

    function integrand_he4(u) result (value)
      implicit none
      real(dp) :: u
      real(dp) :: value

      if (u .lt. 0.00001) then
         u = 0.00001
      end if

      value = exp((-3*u**2)/(2*v_ave_x**2)) * sinh((3*u*v_star)/(v_ave_x**2)) &
      * (exp((-M_wimp*(u**2))/(2*E_0_he4)) &
      - exp((-M_wimp*(u**2)*mu_he4)/(2*E_0_he4*(mu_posi_he4**2))) &
      * exp(((-M_wimp*(v_esca**2)*mu_he4)/(2*E_0_he4*(mu_nega_he4**2)))*(1-(mu_he4)/(mu_posi_he4**2)))   )

    end function integrand_he4

    subroutine Monte_Carlo_integration_c12(n_Monte_Carlo_c12,u_max_c12)
      implicit none
      integer, intent(in) :: n_Monte_Carlo_c12
      real(dp), intent(in) :: u_max_c12
      real(dp) :: u, integral_err
      real(dp) :: f
      integer :: i

      part4_c12 = 0.0
      f= 0.0d0

      do i=1,n_Monte_Carlo_c12
         call random_number(u)
         u=u*u_max_c12
         f = f+integrand_c12(u)
      end do

      f=f/n_Monte_Carlo_c12
      part4_c12 = (u_max_c12-0.0)*f
      !write (*,*) "# MC integration_c12 = ",part4_c12
    end subroutine Monte_Carlo_integration_c12

    function integrand_c12(u) result (value)
      implicit none
      real(dp) :: u
      real(dp) :: value

      if (u .lt. 0.00001) then
         u = 0.00001
      end if

      value = exp((-3*u**2)/(2*v_ave_x**2)) * sinh((3*u*v_star)/(v_ave_x**2)) &
      * (exp((-M_wimp*(u**2))/(2*E_0_c12)) &
      - exp((-M_wimp*(u**2)*mu_c12)/(2*E_0_c12*(mu_posi_c12**2))) &
      * exp(((-M_wimp*(v_esca**2)*mu_c12)/(2*E_0_c12*(mu_nega_c12**2)))*(1-(mu_c12)/(mu_posi_c12**2)))   )

    end function integrand_c12

    subroutine Monte_Carlo_integration_n14(n_Monte_Carlo_n14,u_max_n14)
      implicit none
      integer, intent(in) :: n_Monte_Carlo_n14
      real(dp), intent(in) :: u_max_n14
      real(dp) :: u, integral_err
      real(dp) :: f
      integer :: i

      part4_n14 = 0.0
      f= 0.0d0

      do i=1,n_Monte_Carlo_n14
         call random_number(u)
         u=u*u_max_n14
         f = f+integrand_n14(u)
      end do

      f=f/n_Monte_Carlo_n14
      part4_n14 = (u_max_n14-0.0)*f
      !write (*,*) "# MC integration_n14 = ",part4_n14
    end subroutine Monte_Carlo_integration_n14

    function integrand_n14(u) result (value)
      implicit none
      real(dp) :: u
      real(dp) :: value

      if (u .lt. 0.00001) then
         u = 0.00001
      end if

      value = exp((-3*u**2)/(2*v_ave_x**2)) * sinh((3*u*v_star)/(v_ave_x**2)) &
      * (exp((-M_wimp*(u**2))/(2*E_0_n14)) &
      - exp((-M_wimp*(u**2)*mu_n14)/(2*E_0_n14*(mu_posi_n14**2))) &
      * exp(((-M_wimp*(v_esca**2)*mu_n14)/(2*E_0_n14*(mu_nega_n14**2)))*(1-(mu_n14)/(mu_posi_n14**2)))   )

    end function integrand_n14

    subroutine Monte_Carlo_integration_o16(n_Monte_Carlo_o16,u_max_o16)
      implicit none
      integer, intent(in) :: n_Monte_Carlo_o16
      real(dp), intent(in) :: u_max_o16
      real(dp) :: u, integral_err
      real(dp) :: f
      integer :: i

      part4_n14 = 0.0
      f= 0.0d0

      do i=1,n_Monte_Carlo_o16
         call random_number(u)
         u=u*u_max_o16
         f = f+integrand_o16(u)
      end do

      f=f/n_Monte_Carlo_o16
      part4_o16 = (u_max_o16-0.0)*f
      !write (*,*) "# MC integration_o16 = ",part4_o16
    end subroutine Monte_Carlo_integration_o16

    function integrand_o16(u) result (value)
      implicit none
      real(dp) :: u
      real(dp) :: value

      if (u .lt. 0.00001) then
         u = 0.00001
      end if

      value = exp((-3*u**2)/(2*v_ave_x**2)) * sinh((3*u*v_star)/(v_ave_x**2)) &
      * (exp((-M_wimp*(u**2))/(2*E_0_o16)) &
      - exp((-M_wimp*(u**2)*mu_o16)/(2*E_0_o16*(mu_posi_o16**2))) &
      * exp(((-M_wimp*(v_esca**2)*mu_o16)/(2*E_0_o16*(mu_nega_o16**2)))*(1-(mu_o16)/(mu_posi_o16**2)))   )

    end function integrand_o16

    subroutine Monte_Carlo_integration_ne20(n_Monte_Carlo_ne20,u_max_ne20)
      implicit none
      integer, intent(in) :: n_Monte_Carlo_ne20
      real(dp), intent(in) :: u_max_ne20
      real(dp) :: u, integral_err
      real(dp) :: f
      integer :: i

      part4_n14 = 0.0
      f= 0.0d0

      do i=1,n_Monte_Carlo_ne20
         call random_number(u)
         u=u*u_max_ne20
         f = f+integrand_ne20(u)
      end do

      f=f/n_Monte_Carlo_ne20
      part4_ne20 = (u_max_ne20-0.0)*f
      !write (*,*) "# MC integration_ne20 = ",part4_ne20
    end subroutine Monte_Carlo_integration_ne20

    function integrand_ne20(u) result (value)
      implicit none
      real(dp) :: u
      real(dp) :: value

      if (u .lt. 0.00001) then
         u = 0.00001
      end if

      value = exp((-3*u**2)/(2*v_ave_x**2)) * sinh((3*u*v_star)/(v_ave_x**2)) &
      * (exp((-M_wimp*(u**2))/(2*E_0_ne20)) &
      - exp((-M_wimp*(u**2)*mu_ne20)/(2*E_0_ne20*(mu_posi_ne20**2))) &
      * exp(((-M_wimp*(v_esca**2)*mu_ne20)/(2*E_0_ne20*(mu_nega_ne20**2)))*(1-(mu_ne20)/(mu_posi_ne20**2)))   )

    end function integrand_ne20

    subroutine Monte_Carlo_integration_mg24(n_Monte_Carlo_mg24,u_max_mg24)
      implicit none
      integer, intent(in) :: n_Monte_Carlo_mg24
      real(dp), intent(in) :: u_max_mg24
      real(dp) :: u, integral_err
      real(dp) :: f
      integer :: i

      part4_n14 = 0.0
      f= 0.0d0

      do i=1,n_Monte_Carlo_mg24
         call random_number(u)
         u=u*u_max_mg24
         f = f+integrand_mg24(u)
      end do

      f=f/n_Monte_Carlo_mg24
      part4_mg24 = (u_max_mg24-0.0)*f
      !write (*,*) "# MC integration_mg24 = ",part4_mg24
    end subroutine Monte_Carlo_integration_mg24

    function integrand_mg24(u) result (value)
      implicit none
      real(dp) :: u
      real(dp) :: value

      if (u .lt. 0.00001) then
         u = 0.00001
      end if

      value = exp((-3*u**2)/(2*v_ave_x**2)) * sinh((3*u*v_star)/(v_ave_x**2)) &
      * (exp((-M_wimp*(u**2))/(2*E_0_mg24)) &
      - exp((-M_wimp*(u**2)*mu_mg24)/(2*E_0_mg24*(mu_posi_mg24**2))) &
      * exp(((-M_wimp*(v_esca**2)*mu_mg24)/(2*E_0_mg24*(mu_nega_mg24**2)))*(1-(mu_mg24)/(mu_posi_mg24**2)))   )

     
    end function integrand_mg24


      end module run_star_extras
      
