# Seminar_covid_mortality

* Calculates analytical and numerical bounds on conditional expectation functions from public mortality data, from Mortality Change Among Less Educated Americans Before
and During COVID-19

* Replication code and data for the seminar group 3

# Novosad vs Seminar Group

The code is similar - and often directly copied from - Novosad. Please visit their GitHub repository here:
https://github.com/devdatalab/paper-nra-mortality/tree/master

Custom-written programs, that do not interact witht the Novosad code, are stored in the c map.

# Replication Code and Data

To regenerate the tables and figures from the paper, take the
following steps:

1. Download the replication data package from this [Google Drive Folder](https://drive.google.com/drive/folders/1dz1cHoA6XTNUQ41MDlPdEWMcrwAubFAt?usp=sharing). For those places where our Google Drive capacity was limited, you should download the data from the direct sources and add them to the folder.
   
2. Clone this repo.

3. Open the do file make_nra_mortality.do, and set the globals `out`,
   `mdata`, and `tmp`.  
   * `$out` is the target folder for all outputs, such as tables
   and graphs. 
   * `$mdata` is the folder where you unzipped and saved the
     replication data package.
   * intermediate files will be placed in both `$tmp` and `$mdata/int`.

4. Open `matlab/set_matlab_paths.m` and set `base_path` to the same path as `$mdata`.

5. Open `a/graph_intuitive.py` and set `output_path` to `$mdata/out` in line 10.

**NOTE:** The code probably won't work if you have spaces in the pathnames. Blame Stata Corp, not Van Der Zande et. al. (2025)

6. Run the do file `make_nra_mortality.do`.  This will run through all the
   other do files to regenerate all of the results in `$out/`.

P.S. Please note that this approach will most likely only partially work, since you have to manually add data and adjust path names accordingly. Please contact us if you need any help.

