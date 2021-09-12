# Dark-Matter-effect-on-stars-in-Globular-Clusters-version 2.0

1) We used the MESA-r15140 stellar evolutionary code in ubuntu 20.04 to consider dark matter effects on stars' evolution. 
2) To change the WIMP parameters (mass, density, scattering cross-section and etc), just edit the src/run_star_extras.f90 file.
3) To change the mass of the star, change the value of the "initial_mass" variable in inlist_start, inlist_to_start_he_core_flash, inlist_to_end_core_he_burn, inlist_to_wd, inlist_to_end_agb and inlist_to_end_core_h_burn files.
4) To run the code, it is recommended to edit the run_the_star.sh bash file and then run it through the terminal.
