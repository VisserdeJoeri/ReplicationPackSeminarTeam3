# ACS Tutorial

Goedemorgen!

Naast het efficiënt bouwen van appended_mort en de matlab bounds, ga je, als je voor het echie wil runnen, er al snel tegenaan lopen dat we de ACS correctie nog op de CPS aggregated data moeten toepassen. Omdat ik de komende dagen minder beschikbaar ben, heb ik vannacht alvast mijn kennis over ACS op een rijtje gezet. Zo kun je er direct mee verder.

**Waarom corrigeren?**

Ik heb voor de jaren rond 2015 getest hoeveel procent van een (year, age, race, gender, educ) groep institutionalized was. Bij sommige groepen was dit vaak minder dan 1% (wit, oud, hoogopgeleid), bij andere groepen bijna 30% (zwart, jong, laagopgeleid). Omdat sterfte onder institutionalized wel terugkomt in de sterftecijfers, is corrigeren hiervoor onvermijdelijk.

**Hoe corrigeer je?**

Door bij elke groep van het bestand cps_processed_data een correctiegetal bij de variabele tpop_2 (de geschatte populatie van die groep) op te tellen. Dat getal is het aantal institutionalized personen voor die groep. Omdat de cps_processed_data 22 jaar beslaat (2000-2021), moeten wij ook voor 22 jaar een getal vinden die we kunnen optellen als ACS correctie.

**Wat is er nu gebeurd en hoe heb ik dat gedaan?**

Ik heb voor de periode 2006-2018 voor elke groep een ACS correctiegetal (gegeven met inst) gevonden. Die vind je in het mapje output. inst_pops.dta kun je direct gebruiken om voor 2006-2018 nu al de CPS data te corrigeren\*.

\* Geschreven met beunhaas code (m.n. bij prep_institutionalized). Ik heb liever dat er iemand nog door de code gaat en alles checkt.

Hoe heb ik dit gedaan?

Dit werkt best wel goed in Stata. Ik raad je ook sterk aan om de code te downloaden en in Stata aan de slag te gaan.

1.  Zorg er altijd voor dat je datapaden in nra_mortality.do goed staan. Ter refererentie: die van mij (MACOS) ziet er zo uit:

    global out /Users/Stata/out

    global tmp /Users/Stata/temp

    global mdata /Users/Stata/data

    global mcode /Users/Stata

2.  Download per jaar een csv.hus en csv.pus bestand van een de census website. De link daarvoor vind je in datanotes.md van Novosad. Vervolgens zet je dit in een jaarmapje op de juiste plek in je databestand. Voorbeeld van waar ik heb het gezet:

    /Users/Stata/data/raw/acs/2018/csv_hus.zip

    Staan de zip files op de juiste plek, dan kun je aan de slag met het uitlezen van de zipbestanden. Het belangrijkste daarvoor is dat de do files *make_nra_mortality, make_mortality_data en cepr_acs_master.do* aangesproken worden, en dat daarin alle code die je niet gebruikt, uitgecomment is. Mijn versies staan in de Github

    Gaat dit goed, dan levert je dat een cepr_acs_20XY.dta (+- 2,5 GB) bestand op, waarmee je verder kunt rekenen. Die vind ik allemaal hier:

    /Users/Stata/data/int/acs_prepped/cepr_acs_20XY.dta

    Het kost ongeveer een kwartier om een jaar uit te lezen. Ik heb voor 2006-2018 alle jaren uitgelezen, maar ik kan altijd aanbevelen om er zelf eentje te proberen. Daarna is het makkelijker om te begrijpen wat er moet gebeuren.

3.  Vervolgens ga je rekenen. Hiervoor worden de do files *make_nra_mortality, make_mortality_data en prep_institutionalized.do* gebruikt. Met name bij *prep_institutionalized.do* heb ik veel moeten aanpassen: normaal komen hier de cps files, census files en acs bestanden samen in één groot bestand. Samen met ChatGPT heb ik op een beunhaas manier ervoor gezorgd dat alleen de ACS uitgespit wordt. Ik weet niet helemaal zeker of dit goed is, maar volgens mij wel. Uiteraard staat deze code ook in GitHub.

    In mijn prep_institutionalized gaat alles van 2006 tot en met 2018: daarom heb ik ook die jaren al ready.

**Wat moet er nog gebeuren?**

Probleem 1: ACS correctiepunten vinden voor 2000-2005

Optie A. Voor 2006 gebruikt Novosad een lineaire extrapolatie van de 1990 en 2000 Censusdata. De code hiervoor is al ready. Het enige wat je hoeft te doen, is de juiste zipbestanden vinden en die inpluggen met de juiste do-files. Ik ben hier nog niet mee aan de slag gegaan, maar dit zou te doen moeten lijken.

Optie B: Als A niet lukt, kun je altijd nog een reverse time series doen: terugvoorspellen hoe iets geweest zou moeten zijn (zie ook volgende probleem)

Probleem 2: ACS correctiepunten vinden voor 2019-2021

Optie A: De zipbestanden van 2019-2021 downloaden, en inlezen op dezelfde manier als we nu 2006-2018 doen. Het nadeel is dat je hier in *cepr_acs_master.do* dingen voor moet gaan aanpassen, en met name een nieuwe do-file *cepr_acs_read_2019.do* moet programmeren. Misschien wijkt die veel af van de 2018 versie, misschien bijna niet.

Optie B: Een timeseries maken van de ACS 2006-2018 data en vervolgens forecasten voor 2019-2021. Hiervoor is inst_popsgranage een heel interessant bestand: daarin zijn de groepen niet in leeftijdsgroepen van 5 jaar, maar per jaar. Dat maakt het makkelijker om een timerseries te bouwen.

Ik heb oprecht geen voorkeur voor A of B (al is A natuurlijk wel preciezer), en ze kunnen tegelijkertijd door meerdere mensen uitgewerkt worden.

**Finalise**

Als het gelukt is om de correctiepunten voor 2000-2021 te vinden, kunnen we door en dat als basis gebruiken voor de rest van ons onderzoek!
