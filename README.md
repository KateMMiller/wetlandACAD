# wetlandACAD
This R package was developed to analyze freshwater wetland data collected in Acadia National Park by the 
Northeast Temperate Network (NETN). 

The R package can be installed using `pak::pkg_install('doi-nps/wetlandACAD')`

Previous archived versions of this R package can be found at <a href="www.github.com/katemmiller/wetlandACAD">www.github.com/katemmiller/wetlandACAD</a>

The following functions are used to compile water level data collected in shallow wells to monitor growing season 
water level in 8 sentinel sites on Mount Desert Island, ME. Example scripts that compiled previous year's data 
can be found in https://irma.nps.gov/DataStore/Reference/Edit/2239342.
<ul>
<li>bind_HOBO_data: (step 1) imports and row binds HOBO files by searching for 4-letter wetland codes in 
specified file containing exported csvs from HOBOconnect. </li>
<li>compile_WL_data: (step 2) converts well pressure data into water level relative to wetland surface. </li>
<li>get_NADP_precip: downloads hourly precipitation data from the National Atmospheric Deposition Program website 
for the McFarland Air and Research Station (stationID: ME98) to relate hourly water level to precipitation</li>
<li>calc_WL_stats: Calculates growing season water level statistics by year.</li>
<li>plot_hydro_site_year: Plots hourly water level and precipitation for a given site.</li>
</ul>

The following functions are used to compile and analyze vegetation data collected using Rapid Assessments. 
The sumVegMMI requires protected species to correctly calculate the MMI for each site. 
<ul>
<li>importRAM: imports database tables from the NETN RAM backend Microsoft database and compiles views of the
data used in the wetland data package. Note that protected species by default are not imported/exported in this 
function and require special permissions to access.  </li>
<li>sumSpeciesList: Generates a species list for each site, and only including protected species if imported
by the importRAM function and include_protected = T is specified in the function.</li>
<li>sumVegMMI: Calculates the Vegetation Multimetric Indicator for each site. Note that this must be 
calculated with protected species included in the import.</li>
</ul>
