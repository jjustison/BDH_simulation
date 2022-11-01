After running param_setups.R: this folder will be populated with the following folders:

Simulations that use the same hybrid rate proportions of 1/3,1/3,1/3.
Then each dataset varies the genetic distance dependence and incomplete sampling, respectively.
* all_equal_hybs_gen_dist
* all_equal_hybs_sampling_frac

Simulations that simulate across the hybrid rate simplex under various conditions
* simplex - simulates complete phylogenies across the whole simplex 
* simplex_double_time - doubles the timeout time to 20 seconds
* simplex_gen_dist_2 - delta of 2
* simplex_gen_dist_4 - delta of 4
* simplex_sampling_frac_0.75 - sampling fraction of 0.75



Folders will contain an R workspace object titled "parameters.Rdata" that records the workspace just prior to simulation. In the workspace should have all the parameters used in simulation and the seed set to the value just prior to simulation.
Additionally, folders will contain folders themselves, each with the simulated data inside. Each subfolder represents one specific parameterization, either variying the hybrid rate proportions or the sampling fraction/delta
Contained within are the following files:
* parameters.Rdata - R workspace object with all specific parameters for a given parameter setting, including the seed
* phydata.csv - contains information for the each replicate for that parameterization (i.e., class membership, number of tips, etc.)
* phys.RDS - contains an R object with all the phylogenetic networks. Use 'readRDS()' to read in a list with the phylogenetic networks
* sim_data.fst - contains information regarding the simulation replicates, including their completion status
* double_time_analysis.R - Looks at the numbers of failed simulations for both the complete and complete double-time analyses