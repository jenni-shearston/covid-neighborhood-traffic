--------------------------------------------------------------------------------
Documentation for NHGIS crosswalk files...
    from 2010 blocks...
    to 2020 blocks...
    with GEOID identifiers
--------------------------------------------------------------------------------
 
Contents
    - Data Summary
    - Notes
    - Citation and Use
 
Additional documentation on NHGIS crosswalks is available at:
    https://www.nhgis.org/user-resources/geographic-crosswalks


--------------------------------------------------------------------------------
Data Summary
--------------------------------------------------------------------------------
 
File name:  nhgis_blk2010_blk2020_ge{_state FIPS code*}.csv

	* A state code suffix indicates the extent covered for a file that is limited to a single state. Such files may contain some source zones from neighboring states in cases where the Census Bureau adjusted state boundary lines between censuses. Files with no state code suffix in their names cover the entire U.S. and Puerto Rico.

Content:
	- The top row is a header row
	- Each subsequent row represents an intersection between a 2010 block and 2020 block
	- The first 2 fields, GEOID10 and GEOID20, contain standard census block identifiers
		- A block GEOID is a concatenation of:
			- State FIPS code: 2 digits
			- County FIPS code: 3 digits
			- Census tract code: 6 digits
			- Census block code: 4 digits
	- The third field, WEIGHT, contains interpolation weights to allocate portions of 2010 block counts to 2020 blocks
	- The fourth field, PAREA, contains the portion of each 2010 block's land* area lying in each 2020 block.
		* If a 2010 block's area is entirely water, then this value is based on the block's total area including water
		- NHGIS uses these values to compute lower and upper bounds on block-based estimates: for any record with a value greater than 0 and less than 1, we assume that either all or none of the source block's characteristics could be located in the corresponding target block.


--------------------------------------------------------------------------------
Notes
--------------------------------------------------------------------------------

The interpolation weights are based on "target-density weighting" (TDW) as described at https://www.nhgis.org/documentation/time-series/2000-blocks-to-2010-geog#simpler-model. In short, TDW assumes that characteristics in each source zone have a distribution proportional to the densities of another characteristic among intersecting target zones. For example, if a 2010 block intersects two 2020 blocks, one of which was 10 times as dense as the other in 2020, then TDW assumes that same 10:1 ratio holds within the 2010 block in 2010.

The weights are based specifically on the densities of 2020 block population and housing units _summed together_. Weights based on this summed density are more generally applicable--suitable for interpolating either population or housing characteristics--compared to weights based on either density alone.

As explained on the page linked above, NHGIS uses this same TDW model for the vast majority of cases in the 2000-2010 block crosswalks. The more advanced hybrid model described in other sections on that page applies only to "blocks of interest" where a 2000 block intersected multiple higher-level 2010 reporting areas. The advanced model does improve on simple TDW, but the improvement is small on average, and the advanced model is relatively difficult to implement, so we use only the TDW model for all records in the 2010-2020 block crosswalk.


--------------------------------------------------------------------------------
Citation and Use
--------------------------------------------------------------------------------
 
Use of NHGIS crosswalks is subject to the same conditions as for all NHGIS data. See https://www.nhgis.org/citation-and-use-nhgis-data.

