Explanation of tracer code in WRF

The following text describes the files included with this README that need to be updated in the WRF library for implementing passive tracers. Once the updated files are in place, you will need to recompile WRF for the changes to be available. Once you have completed compilation, the namelist.input file should include tracer_opt = 2 in the &dynamics options list for tracers to be included in your WRF runs. 

One file needs to be modified in dyn_em directory:

SUBROUTINE solve_em.F
is called every time step and is modified to set the tracers
	• Added tracer module and routine to set the tracers for non-WRFCHEM at line 66
	• Calls tracer routine (set_tracer_CRH) at lines 3504-3525
-----------------------------------------

In directory phys, three new files are added/replaced:

1) Updated Makefile to include the two subroutines below in the WRF compilation

SUBROUTINE module_input_tracer.F.
module_input_tracer.F has two key routines that are needed to set the tracers:
set_tracer_CRH: 
	• Initializes tracers for parent domain and all nested domains at the initial model time step
	• Resets tracers within 3 grid points of the model boundary of the parent domain at each model time step

tropopause: 
	• Computes the altitude of the lapse-rate tropopause following the WMO definition

For the attached version of the tracer code, there are 7 (out of a maximum of 8 allowed) tracers that are set: Troposphere, Boundary Layer, Mid-troposphere, Upper-troposphere, Stratosphere, Stratospheric Middleworld, & Stratospheric Overworld. These tracers are given in the order they are output in WRF and named tr17_1, tr17_2, etc., and they are defined as follows and set to 0 otherwise: 
	• Troposphere: =1 from surface to the tropopause
	• Boundary Layer: =1 from surface to the altitude of the planetary boundary layer
	• Mid-troposphere: =1 for grid points above PBL and below 2 km below the tropopause
	• Upper-troposphere: =1 for grid points within 2 km of the tropopause
	• Stratosphere: =1 for all grid points above the level of the tropopause
	• Stratospheric Middleworld: =1 for all grid points above the level of the tropopause and at or below the 380 K potential temperature surface
	• Stratospheric Overworld: =1 for all grid points above the 380 K potential temperature surface and the tropopause

One additional note: In order to supply a continuous feed of boundary layer air, the BL tracer is set =1 at each model time step for the entire domain.

————————————————————————————————————————————————————————————————————————————————————————————
Errors compiling:

“Error in opening the compiled module file.  Check INCLUDE paths.”
-Means the .mod file was not found

Edits made to:
module_initialize_real.F
solve_em.F
start_em.F
module_input_tracer.F
chem_driver.F
