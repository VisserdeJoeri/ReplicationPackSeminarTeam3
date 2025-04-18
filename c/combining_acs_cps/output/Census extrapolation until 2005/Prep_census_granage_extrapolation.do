////////// Granage extrapolation ///////////

clear

// Laad de 2000 census data
use "C:/Users/maud5/Documents/GitHub/paper-nra-mortality/int/2000_clean.dta"

keep wbho sex age perwt year educ edclass

// Beperk de data tot leeftijden tussen 25 en 70
keep if inrange(age, 25, 70)

// Verkrijg de som per alle cellen
collapse (sum) perwt, by(sex wbho age edclass`edtype' year)

tempfile acs
save `acs', replace

/* Verkrijg de totalen per ras en geslacht */
collapse (sum) perwt, by(sex age edclass`edtype' year)

gen wbho = 0
append using `acs'
save `acs', replace

/* Verkrijg het totaal per geslacht */
collapse (sum) perwt, by(wbho age edclass`edtype' year)
gen sex = 0
append using `acs'

// Hernoem de kolom 'perwt' naar 'inst'
ren perwt inst
save `acs', replace

// Voeg een nieuw bestand toe
append using "C:/Users/maud5/Documents/GitHub/paper-nra-mortality/int/inst_popsgranage.dta"

// Voeg rijen toe voor de jaren 2001-2005, waar inst leeg is zodat we de extrapolatie kunnen gebruiken
levelsof wbho, local(wbho_levels)
levelsof sex, local(sex_levels)
levelsof edclass`edtype', local(edclass_levels)
levelsof age, local(age_levels)

local years = "2001 2002 2003 2004 2005"

foreach wbho in `wbho_levels' {
    foreach sex in `sex_levels' {
        foreach edclass in `edclass_levels' {
            foreach age in `age_levels' {
                foreach year in `years' {

                    set obs `=_N + 1'
                    replace wbho = `wbho' in `=_N'
                    replace sex = `sex' in `=_N'
                    replace edclass = `edclass' in `=_N'
                    replace age = `age' in `=_N'
                    replace year = `year' in `=_N'
                    replace inst = . in `=_N'  
                }
            }
        }
    }
}

// Zet de lege 'inst' waarden om naar 0
replace inst = 0 if inst == .

// Maak tijdelijke variabelen voor de extrapolatie
foreach year in 2000 2006 {
    gen inst_`year'_tmp = inst if year == `year'
    bys wbho sex edclass age: egen inst_`year' = mean(inst_`year'_tmp)
}

// Sorteer de tabel
sort wbho sex edclass age year

// Doe de extrapolatie voor 2001-2005
bys wbho sex edclass age: replace inst = (year - 2000) * (inst_2006 - inst_2000) / (2006 - 2000) + inst_2000 if inrange(year,2001,2005)

// Verwijder tijdelijke variabelen
drop *tmp

// Sla het bestand op
keep if inrange(year,1992,2018)
save "C:/Users/maud5/Documents/GitHub/paper-nra-mortality/int/extrapolated_inst_pops_2000_2005_granage.dta", replace


