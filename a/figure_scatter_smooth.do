import delimited "$mdata/mort/nchs/appended_rank_mort.csv", clear

/* collapse to 2-year bins from 2000 to 2021 */
egen year_bin = cut(year), at(2000(2)2022)
collapse (mean) *mortrate ed_rank_sex tpop_rolling, by(race sex age edclass year_bin)
ren year_bin year

save $tmp/mort_clean_nchs_smooth, replace

/*********************************************/
/* plot mortality over time against ed rank  */
/*********************************************/
capture program drop mortality_ed_fig
capture program define mortality_ed_fig
{
    syntax, sex(int) age(int) cause(string) race(string) [ylabel(passthru) name(string) title(string) presentation ]

    if "`presentation'" != "" {
      local paneloff = "paneloff"
      local labsize = "medium"
    }

    if mi("`name'") {
        local name cdmort`cause'_`sex'_`racet`race''_`age'
    }

    preserve

    qui keep if age == `age'

    if "`cause'" == "t" & mi("`ylabel'") {
        qui sum `cause'mortrate if edclass == 1 & race == 2 & sex == 1
        local ymax = floor(`r(max)' / 1000) * 1000 + 1000
        local yhalf = `ymax' / 4
        local ylabel ylabel(0(`yhalf')`ymax')
    }

    /* position text labels above and to the right of each 2020 point */
    forval ed = 1/4 {
      qui sum `cause'mortrate if edclass == `ed' & race == `race' & sex == `sex' & year == 2020
      local text_y`ed' = `r(mean)' + 200

      qui sum ed_rank_sex if edclass == `ed' & race == `race' & sex == `sex' & year == 2020
      local text_x`ed' = `r(mean)' - 1
    }

    /* for blacks: position labels at 2000 point */
    if `race' == 2 {
      forval ed = 1/4 { 
        qui sum `cause'mortrate if edclass == `ed' & race == `race' & sex == `sex' & year == 2000
        local text_y`ed' = `r(mean)' + 200

        qui sum ed_rank_sex if edclass == `ed' & race == `race' & sex == `sex' & year == 2000
        local text_x`ed' = `r(mean)' - 1
      }
    }

    /* manual label tweaks if needed */
    if `age' == 50 & `sex' == 1 & `race' == 1 & "`cause'" == "t" {
      local text_x1 = 7
      local text_y1 = 1750
      local text_x4 = 89
    }
    if `age' == 50 & `sex' == 2 & `race' == 2 & "`cause'" == "t" {
      local text_x1 = 2
      local text_y1 = 1350
      local text_x3 = 55
    }
    if `age' == 50 & `sex' == 1 & `race' == 2 & "`cause'" == "t" {
      local text_x1 = 10
      local text_y1 = 1825
      local text_x2 = 27
      local text_y2 = 1300
      local text_x3 = 48
      local text_y3 = 500
    }

    qui keep if sex == `sex' & race == `race'

    local t0 "All Genders" 
    local t1 "Male"
    local t2 "Female"

    if "`presentation'" != "" {
      local t1 "Men"
      local t2 "Women"
    }

    local age_plus_4 = `age' + 4
    local tt Total Mortality
    local dt "Deaths of Despair"
    local Pt Poisoning 
    local St Suicide
    local Lt Alcoholic Liver
    local Ht Heart Disease
    local ht Heart Disease
    local Ct Cancer
    local lungCt Lung Cancer
    local ct Cancer
    local lungct Lung Cancer
    local CDrate Cerebrovascular 
    local CLrate Chronic Resp.
    local racet1 White
    local racet2 Black
    local racet3 Hisp
    local racet4 Other
    local panel1 "Panel A: "
    local panel2 "Panel B: "
    local panel3 "Panel C: "
    local panel4 "Panel D: "
    if "`paneloff'" != "" {
      local panel1 ""
      local panel2 ""
      local panel3 ""
      local panel4 "" 
    }

    if mi("`title'") {
      local title title(`"``cause't' for Age `age'-`age_plus_4'"')
    }
    local panel_num = (3 - `sex') + (`race'-1) * 2
    if `race' != 0 & `sex' != 0 local panel `panel`panel_num''

    /* build legend for 2-year bins from 2000 to 2020 */
    local legend
    local order 
    forval y = 2000(2)2020 {
      local yp2 = `y' + 1
      local index = (`y' - 2000) / 2 + 5
      local order `order' `index'
      local legend `legend' lab(`index' `y'-`yp2')
    }

    local legend legend(`legend' pos(2) ring(0) order(`order') rowgap(0.4) region(lcolor(gs8)) cols(1) size(`legendsize'))

    local lcolor 100 100 100
    local subtitle "`panel' `racet`race'', `t`sex''" 
    if "`presentation'" != "" {
      local subtitle "Mortality Among `racet`race'' `t`sex''"
    }

    sort year
    line `cause'mortrate    ed_rank_sex if edclass == 1, lcolor("`lcolor'") || ///
    line `cause'mortrate    ed_rank_sex if edclass == 2, lcolor("`lcolor'") || ///
    line `cause'mortrate    ed_rank_sex if edclass == 3, lcolor("`lcolor'") || ///
    line `cause'mortrate    ed_rank_sex if edclass == 4, lcolor("`lcolor'") || ///
    scatter `cause'mortrate ed_rank_sex if year == 2000, msize(vlarge) mcolor("0 0 0") msymbol(Th) || ///
    scatter `cause'mortrate ed_rank_sex if year == 2002, msize(medium) mcolor("60 0 0") msymbol(+) || ///
    scatter `cause'mortrate ed_rank_sex if year == 2004, msize(medium) mcolor("120 0 0") msymbol(+) || ///
    scatter `cause'mortrate ed_rank_sex if year == 2006, msize(medium) mcolor("180 0 0") msymbol(+) || ///
    scatter `cause'mortrate ed_rank_sex if year == 2008, msize(medium) mcolor("240 0 0") msymbol(+) || ///
    scatter `cause'mortrate ed_rank_sex if year == 2010, msize(medium) mcolor("255 40 0") msymbol(+) || ///
    scatter `cause'mortrate ed_rank_sex if year == 2012, msize(medium) mcolor("255 100 0") msymbol(+) || ///
    scatter `cause'mortrate ed_rank_sex if year == 2014, msize(medium) mcolor("255 180 0") msymbol(+) || ///
    scatter `cause'mortrate ed_rank_sex if year == 2016, msize(medium) mcolor("255 220 0") msymbol(+) || ///
    scatter `cause'mortrate ed_rank_sex if year == 2018, msize(medium) mcolor("255 240 0") msymbol(s) || ///
    scatter `cause'mortrate ed_rank_sex if year == 2020, msize(vlarge) mcolor("255 255 0") mlcolor(black) msymbol(s) ///
    xtitle("Mean Education Percentile", size(medlarge)) ytitle("Deaths per 100,000", size(medlarge)) subtitle("`subtitle'", size(large) ring(0) position(12)) ///
    xlabel(0(25)100, labsize(large))  `ylabel' `ytick' `title' ///
    name(`name', replace) graphregion(color(white)) ylabel(,labsize(large)) `legend' ///
    text(`text_y1' `text_x1' "No High School", placement(e) size(medlarge)) ///
    text(`text_y2' `text_x2' "High School", placement(e) size(medlarge)) ///
    text(`text_y3' `text_x3' "Some College", placement(e) size(medlarge)) ///
    text(`text_y4' `text_x4' "B.A.+", placement(e) size(medlarge)) xtitle(,size(large)) ytitle(,size(large)) title(,size(vlarge))

    restore
}
end

local racet1 white
local racet2 black
local racet3 hisp
local racet4 other

use $tmp/mort_clean_nchs_smooth, clear

/* generate presentation and regular graphs */
global causelist t 
global agelist 50(5)50

foreach cause in $causelist {
  foreach age of numlist $agelist {
    foreach race in 1 2 {
      foreach sex in 1 2 {
        mortality_ed_fig, sex(`sex') age(`age') cause(`cause') race(`race') name(s`sex'_r`race') title(" ") presentation
        graphout presentation-scatter-smooth-`cause'-`age'-`sex'-`race', pdf
      }
    }
  }
}

foreach cause in $causelist {
  foreach age of numlist $agelist {
    foreach race in 1 2 {
      foreach sex in 1 2 {
        mortality_ed_fig, sex(`sex') age(`age') cause(`cause') race(`race') name(s`sex'_r`race') title(" ")
        graphout scatter-smooth-`cause'-`age'-`sex'-`race', pdf
      }
    }
  }
}
