********************************************************************************
* Project: China's Sponge City Programme and rain-induced road congestion
* File:    08_supplementary_figures7_8_precipitation_topography.do
* Purpose: Draw Supplementary Figs. 7-8: precipitation and drainage density;
*          topographic characteristics and nighttime light intensity
* Author:  Yanghan Lin et al.
********************************************************************************

version 17.0
clear all
set more off

********************************************************************************
* 0. User paths
********************************************************************************

* Run 00_data_preparation.do before this script.
global PROJECT_ROOT "C:/Users/Administrator/Documents/Sponge City and road congestion paper"
global SUBMISSION    "${PROJECT_ROOT}/submission_code"
global PROCESSED     "${SUBMISSION}/data/processed"
global OUTPUT        "${SUBMISSION}/outputs"
global LOGS          "${SUBMISSION}/logs"

capture mkdir "${OUTPUT}"
capture mkdir "${LOGS}"

capture log close _all
log using "${LOGS}/08_supplementary_figures7_8_precipitation_topography.log", replace text

graph set window fontface "Helvetica"
set scheme s1color

********************************************************************************
* 1. Fig. 7: load annual precipitation and drainage infrastructure variables
********************************************************************************

use "${PROCESSED}/yearly_panel_prepared.dta", clear

* Retain city-year observations, as in the original annual-panel scatter plots.
keep city_code year precp pipe_density_urban rain_pipe build_up_area
gen double precp_100mm = precp / 100
gen double rainpipe_density = rain_pipe / build_up_area ///
    if build_up_area > 0 & !missing(build_up_area)

label variable precp_100mm      "Annual precipitation (100 mm)"
label variable pipe_density_urban "Drainage pipeline density (km/km2)"
label variable rainpipe_density "Stormwater pipeline density (km/km2)"

tempfile fig7_data
save `fig7_data', replace

********************************************************************************
* 2. Fig. 7, panel a: annual precipitation and drainage pipeline density
********************************************************************************

use `fig7_data', clear
drop if missing(precp_100mm) | missing(pipe_density_urban)

reg pipe_density_urban precp_100mm
local r2_drainage : display %4.3f e(r2)
scalar p_drainage = 2 * ttail(e(df_r), abs(_b[precp_100mm] / _se[precp_100mm]))
local p_drainage_text "P < 0.001"
if p_drainage >= 0.001 {
    local p_drainage_value : display %4.3f p_drainage
    local p_drainage_text "P = `p_drainage_value'"
}

predict double yhat_drainage, xb
predict double se_drainage, stdp
gen double ci_lo_drainage = yhat_drainage - invttail(e(df_r), 0.025) * se_drainage
gen double ci_hi_drainage = yhat_drainage + invttail(e(df_r), 0.025) * se_drainage
sort precp_100mm

twoway ///
    (rarea ci_hi_drainage ci_lo_drainage precp_100mm, sort color(red%18) lcolor(none)) ///
    (line yhat_drainage precp_100mm, sort lcolor(red) lwidth(medthin)) ///
    (scatter pipe_density_urban precp_100mm, msymbol(O) msize(small) mcolor(navy%55)), ///
    title("a", position(11) ring(0) size(medium)) ///
    xtitle("Annual precipitation (100 mm)") ///
    ytitle("Drainage pipeline density (km/km{sup:2})") ///
    note("R-squared = `r2_drainage'; `p_drainage_text'", size(vsmall)) ///
    legend(off) ///
    graphregion(color(white)) plotregion(color(white)) ///
    name(fig7a_drainage, replace)

graph save "${OUTPUT}/supplementary_figure7a_precipitation_drainage.gph", replace
graph export "${OUTPUT}/supplementary_figure7a_precipitation_drainage.png", ///
    as(png) replace width(1800)

********************************************************************************
* 3. Fig. 7, panel b: annual precipitation and stormwater pipeline density
********************************************************************************

use `fig7_data', clear
drop if missing(precp_100mm) | missing(rainpipe_density)

reg rainpipe_density precp_100mm
local r2_stormwater : display %4.3f e(r2)
scalar p_stormwater = 2 * ttail(e(df_r), abs(_b[precp_100mm] / _se[precp_100mm]))
local p_stormwater_text "P < 0.001"
if p_stormwater >= 0.001 {
    local p_stormwater_value : display %4.3f p_stormwater
    local p_stormwater_text "P = `p_stormwater_value'"
}

predict double yhat_stormwater, xb
predict double se_stormwater, stdp
gen double ci_lo_stormwater = yhat_stormwater - invttail(e(df_r), 0.025) * se_stormwater
gen double ci_hi_stormwater = yhat_stormwater + invttail(e(df_r), 0.025) * se_stormwater
sort precp_100mm

twoway ///
    (rarea ci_hi_stormwater ci_lo_stormwater precp_100mm, sort color(red%18) lcolor(none)) ///
    (line yhat_stormwater precp_100mm, sort lcolor(red) lwidth(medthin)) ///
    (scatter rainpipe_density precp_100mm, msymbol(O) msize(small) mcolor(navy%55)), ///
    title("b", position(11) ring(0) size(medium)) ///
    xtitle("Annual precipitation (100 mm)") ///
    ytitle("Stormwater pipeline density (km/km{sup:2})") ///
    note("R-squared = `r2_stormwater'; `p_stormwater_text'", size(vsmall)) ///
    legend(off) ///
    graphregion(color(white)) plotregion(color(white)) ///
    name(fig7b_stormwater, replace)

graph save "${OUTPUT}/supplementary_figure7b_precipitation_stormwater.gph", replace
graph export "${OUTPUT}/supplementary_figure7b_precipitation_stormwater.png", ///
    as(png) replace width(1800)

********************************************************************************
* 4. Combine Fig. 7 panels
********************************************************************************

graph combine fig7a_drainage fig7b_stormwater, ///
    cols(2) imargin(tiny) graphregion(color(white))

graph save "${OUTPUT}/supplementary_figure7_precipitation_drainage.gph", replace
graph export "${OUTPUT}/supplementary_figure7_precipitation_drainage.png", ///
    as(png) replace width(3000)
graph export "${OUTPUT}/supplementary_figure7_precipitation_drainage.pdf", ///
    as(pdf) replace

********************************************************************************
* 5. Fig. 8: load city-level baseline variables
********************************************************************************

use "${PROCESSED}/yearly_panel_prepared.dta", clear

keep city_code city light_2014 slope_2014 altitude_2014
duplicates drop city_code, force
drop if missing(light_2014) | missing(slope_2014) | missing(altitude_2014)

gen double altitude_100m = altitude_2014 / 100
label variable light_2014   "Nighttime light intensity"
label variable slope_2014   "Slope"
label variable altitude_100m "Elevation"

tempfile fig8_data
save `fig8_data', replace

********************************************************************************
* 6. Fig. 8, panel a: slope and nighttime light intensity
********************************************************************************

use `fig8_data', clear

reg light_2014 slope_2014
local r2_slope : display %4.3f e(r2)
scalar p_slope = 2 * ttail(e(df_r), abs(_b[slope_2014] / _se[slope_2014]))
local p_slope_text "P < 0.001"
if p_slope >= 0.001 {
    local p_slope_value : display %4.3f p_slope
    local p_slope_text "P = `p_slope_value'"
}

predict double yhat_slope, xb
predict double se_slope, stdp
gen double ci_lo_slope = yhat_slope - invttail(e(df_r), 0.025) * se_slope
gen double ci_hi_slope = yhat_slope + invttail(e(df_r), 0.025) * se_slope
sort slope_2014

twoway ///
    (rarea ci_hi_slope ci_lo_slope slope_2014, sort color(red%18) lcolor(none)) ///
    (line yhat_slope slope_2014, sort lcolor(red) lwidth(medthin)) ///
    (scatter light_2014 slope_2014, msymbol(O) msize(small) mcolor(navy%55)), ///
    title("a", position(11) ring(0) size(medium)) ///
    xtitle("Slope (degrees)") ///
    ytitle("Nighttime light intensity") ///
    note("R-squared = `r2_slope'; `p_slope_text'", size(vsmall)) ///
    legend(off) ///
    graphregion(color(white)) plotregion(color(white)) ///
    name(fig8a_slope, replace)

graph save "${OUTPUT}/supplementary_figure8a_slope_light.gph", replace
graph export "${OUTPUT}/supplementary_figure8a_slope_light.png", ///
    as(png) replace width(1800)

********************************************************************************
* 7. Fig. 8, panel b: elevation and nighttime light intensity
********************************************************************************

use `fig8_data', clear

reg light_2014 altitude_100m
local r2_altitude : display %4.3f e(r2)
scalar p_altitude = 2 * ttail(e(df_r), abs(_b[altitude_100m] / _se[altitude_100m]))
local p_altitude_text "P < 0.001"
if p_altitude >= 0.001 {
    local p_altitude_value : display %4.3f p_altitude
    local p_altitude_text "P = `p_altitude_value'"
}

predict double yhat_altitude, xb
predict double se_altitude, stdp
gen double ci_lo_altitude = yhat_altitude - invttail(e(df_r), 0.025) * se_altitude
gen double ci_hi_altitude = yhat_altitude + invttail(e(df_r), 0.025) * se_altitude
sort altitude_100m

twoway ///
    (rarea ci_hi_altitude ci_lo_altitude altitude_100m, sort color(red%18) lcolor(none)) ///
    (line yhat_altitude altitude_100m, sort lcolor(red) lwidth(medthin)) ///
    (scatter light_2014 altitude_100m, msymbol(O) msize(small) mcolor(navy%55)), ///
    title("b", position(11) ring(0) size(medium)) ///
    xtitle("Elevation (100 m)") ///
    ytitle("Nighttime light intensity") ///
    note("R-squared = `r2_altitude'; `p_altitude_text'", size(vsmall)) ///
    legend(off) ///
    graphregion(color(white)) plotregion(color(white)) ///
    name(fig8b_altitude, replace)

graph save "${OUTPUT}/supplementary_figure8b_elevation_light.gph", replace
graph export "${OUTPUT}/supplementary_figure8b_elevation_light.png", ///
    as(png) replace width(1800)

********************************************************************************
* 8. Combine Fig. 8 panels
********************************************************************************

graph combine fig8a_slope fig8b_altitude, ///
    cols(2) imargin(tiny) graphregion(color(white))

graph save "${OUTPUT}/supplementary_figure8_topography_light.gph", replace
graph export "${OUTPUT}/supplementary_figure8_topography_light.png", ///
    as(png) replace width(3000)
graph export "${OUTPUT}/supplementary_figure8_topography_light.pdf", ///
    as(pdf) replace

log close
