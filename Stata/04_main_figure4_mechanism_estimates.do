********************************************************************************
* Project: China's Sponge City Programme and rain-induced road congestion
* File:    04_main_figure4_mechanism_estimates.do
* Purpose: Estimate and export the annual mechanism results used in Fig. 4a-l
* Author:  Yanghan Lin et al.
********************************************************************************

version 17.0
clear all
set more off

********************************************************************************
* 0. User paths and dependencies
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
log using "${LOGS}/04_main_figure4_mechanism_estimates.log", replace text

capture which reghdfe
if _rc {
    display as error "The user-written Stata command reghdfe is required."
    display as error "Install it before running this script: ssc install reghdfe, replace"
    exit 111
}

********************************************************************************
* 1. Load annual panel and construct variables used in Fig. 4
********************************************************************************

use "${PROCESSED}/yearly_panel_prepared.dta", clear

* Fig. 4 uses the full annual panel. Procurement panels contain 95 cities over
* 2015-2024, giving N = 950. The local-pilot exclusion is used in robustness
* checks rather than in this mechanism figure.

capture confirm variable policy
if _rc {
    display as error "Variable policy was not found in the annual panel."
    exit 111
}

capture confirm variable distance
if _rc {
    gen distance = year - pilot_time
}

foreach var in ln_light ln_pop ln_slope ln_altitude {
    capture confirm variable `var'
    if _rc {
        if "`var'" == "ln_light" {
            gen ln_light = ln(light_2014)
        }
        if "`var'" == "ln_pop" {
            gen ln_pop = ln(pop_2014)
        }
        if "`var'" == "ln_slope" {
            gen ln_slope = ln(slope_2014)
        }
        if "`var'" == "ln_altitude" {
            gen ln_altitude = ln(altitude_2014)
        }
    }
}

capture confirm variable imp_surface_ratio
if _rc {
    gen imp_surface_ratio = (imp_surface / area) * 100
}

capture confirm variable ln_green_invest
if _rc {
    gen ln_green_invest = ln(green_invest * 10000 / build_up_area)
}

capture confirm variable ln_pipe_invest
if _rc {
    gen ln_pipe_invest = ln(pipe_invest * 10000 / build_up_area)
}

capture confirm variable ln_pipe_density
if _rc {
    gen ln_pipe_density = ln(pipe_density_urban)
}

capture confirm variable ln_rain_pipe_density
if _rc {
    gen ln_rain_pipe_density = ln(rain_pipe / build_up_area)
}

foreach var in ln_proc_source_lid ln_proc_gray_drain ln_proc_keynode_om ///
    ln_proc_smart_monitor ln_proc_bluegreen ln_proc_plan_design ///
    imp_surface_ratio green_ratio_urban ln_green_invest ///
    ln_pipe_density ln_pipe_invest ln_rain_pipe_density {
    capture confirm variable `var'
    if _rc {
        display as error "Required Fig. 4 variable `var' was not found."
        exit 111
    }
}

foreach var in pre_5 pre_4 pre_3 pre_2 pre_1 current las_1 las_2 las_3 las_4 las_5 {
    capture confirm variable `var'
    if _rc {
        gen byte `var' = 0
    }
    replace `var' = 0
}

replace pre_5  = (distance <= -5) if !missing(distance)
replace pre_4  = (distance == -4) if !missing(distance)
replace pre_3  = (distance == -3) if !missing(distance)
replace pre_2  = (distance == -2) if !missing(distance)
replace pre_1  = (distance == -1) if !missing(distance)
replace current = (distance == 0) if !missing(distance)
replace las_1  = (distance == 1) if !missing(distance)
replace las_2  = (distance == 2) if !missing(distance)
replace las_3  = (distance == 3) if !missing(distance)
replace las_4  = (distance == 4) if !missing(distance)
replace las_5  = (distance >= 5) if !missing(distance)

local controls c.ln_light#i.year c.ln_pop#i.year ///
    c.ln_slope#i.year c.ln_altitude#i.year

********************************************************************************
* 2. Helper program for ATT and event-study exports
********************************************************************************

capture program drop estimate_fig4_panel
program define estimate_fig4_panel
    syntax, PANEL(string) OUTCOME(name) TITLE(string) SHORT(string) ///
        EVENTHANDLE(name) ATTHANDLE(name) CONTROLS(string)

    quietly reghdfe `outcome' policy `controls', ///
        absorb(city_code year) vce(cluster city_code)

    scalar b_att = _b[policy]
    scalar se_att = _se[policy]
    scalar p_att = 2 * ttail(e(df_r), abs(b_att / se_att))
    local stars ""
    if p_att < 0.01 {
        local stars "***"
    }
    else if p_att < 0.05 {
        local stars "**"
    }
    else if p_att < 0.10 {
        local stars "*"
    }

    post `atthandle' ("`panel'") ("`outcome'") ("`title'") ("`short'") ///
        (b_att) (se_att) (p_att) ("`stars'") (e(N))

    quietly reghdfe `outcome' pre_5 pre_4 pre_3 pre_2 current ///
        las_1 las_2 las_3 las_4 las_5 `controls', ///
        absorb(city_code year) vce(cluster city_code)

    forvalues k = -5/5 {
        if `k' == -5 {
            local v "pre_5"
        }
        else if `k' == -4 {
            local v "pre_4"
        }
        else if `k' == -3 {
            local v "pre_3"
        }
        else if `k' == -2 {
            local v "pre_2"
        }
        else if `k' == -1 {
            local v "reference"
        }
        else if `k' == 0 {
            local v "current"
        }
        else {
            local v "las_`k'"
        }

        if "`v'" == "reference" {
            post `eventhandle' ("`panel'") ("`outcome'") ("`title'") ///
                ("`short'") (`k') (0) (.) (0) (0)
        }
        else {
            capture scalar b_ev = _b[`v']
            capture scalar se_ev = _se[`v']
            if _rc {
                post `eventhandle' ("`panel'") ("`outcome'") ("`title'") ///
                    ("`short'") (`k') (.) (.) (.) (.)
            }
            else {
                scalar lo_ev = b_ev - invnormal(0.975) * se_ev
                scalar hi_ev = b_ev + invnormal(0.975) * se_ev
                post `eventhandle' ("`panel'") ("`outcome'") ("`title'") ///
                    ("`short'") (`k') (b_ev) (se_ev) (lo_ev) (hi_ev)
            }
        }
    }
end

********************************************************************************
* 3. Estimate all Fig. 4 panels
********************************************************************************

tempfile event_results att_results
tempname event_handle att_handle

postfile `event_handle' str4 panel str40 outcome str80 title str12 short ///
    int k double beta se ci_lo ci_hi using `event_results', replace

postfile `att_handle' str4 panel str40 outcome str80 title str12 short ///
    double beta se p_value str4 stars double observations using `att_results', replace

estimate_fig4_panel, panel("a") outcome(ln_proc_source_lid) ///
    title("Source-control LID procurement") short("LIDP") ///
    eventhandle(`event_handle') atthandle(`att_handle') controls("`controls'")

estimate_fig4_panel, panel("b") outcome(ln_proc_gray_drain) ///
    title("Gray drainage procurement") short("GDP") ///
    eventhandle(`event_handle') atthandle(`att_handle') controls("`controls'")

estimate_fig4_panel, panel("c") outcome(ln_proc_keynode_om) ///
    title("Key-node O&M procurement") short("KOM") ///
    eventhandle(`event_handle') atthandle(`att_handle') controls("`controls'")

estimate_fig4_panel, panel("d") outcome(ln_proc_smart_monitor) ///
    title("Smart monitoring procurement") short("SMP") ///
    eventhandle(`event_handle') atthandle(`att_handle') controls("`controls'")

estimate_fig4_panel, panel("e") outcome(ln_proc_bluegreen) ///
    title("Blue-green restoration procurement") short("BGP") ///
    eventhandle(`event_handle') atthandle(`att_handle') controls("`controls'")

estimate_fig4_panel, panel("f") outcome(ln_proc_plan_design) ///
    title("Planning and design service procurement") short("PDP") ///
    eventhandle(`event_handle') atthandle(`att_handle') controls("`controls'")

estimate_fig4_panel, panel("g") outcome(imp_surface_ratio) ///
    title("Impervious surface ratio") short("ISR") ///
    eventhandle(`event_handle') atthandle(`att_handle') controls("`controls'")

estimate_fig4_panel, panel("h") outcome(green_ratio_urban) ///
    title("Urban vegetation coverage") short("NDVI") ///
    eventhandle(`event_handle') atthandle(`att_handle') controls("`controls'")

estimate_fig4_panel, panel("i") outcome(ln_green_invest) ///
    title("Urban greening investment") short("UGI") ///
    eventhandle(`event_handle') atthandle(`att_handle') controls("`controls'")

estimate_fig4_panel, panel("j") outcome(ln_pipe_density) ///
    title("Drainage pipeline density") short("DPD") ///
    eventhandle(`event_handle') atthandle(`att_handle') controls("`controls'")

estimate_fig4_panel, panel("k") outcome(ln_pipe_invest) ///
    title("Drainage pipeline investment") short("DPI") ///
    eventhandle(`event_handle') atthandle(`att_handle') controls("`controls'")

estimate_fig4_panel, panel("l") outcome(ln_rain_pipe_density) ///
    title("Stormwater pipeline density") short("SPD") ///
    eventhandle(`event_handle') atthandle(`att_handle') controls("`controls'")

postclose `event_handle'
postclose `att_handle'

preserve
    use `event_results', clear
    order panel outcome title short k beta se ci_lo ci_hi
    sort panel k
    export delimited using "${OUTPUT}/fig4_mechanism_event_study.csv", replace
restore

preserve
    use `att_results', clear
    order panel outcome title short beta se p_value stars observations
    sort panel
    export delimited using "${OUTPUT}/fig4_mechanism_att_summary.csv", replace
restore

log close
