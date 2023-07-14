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


Running the pop_sims.R file will generate data in the two following folders:
* pop_sims - simulate the average population size when 's' varies from -0.1 to 0.1
* pop_sims_double_time - simulate the average population size when 's' varies from -0.1 to -0.01. Simulation run time is doubled. We don't do this when 's' is positive because most simulation will timeout anyway and it is a waste of computation time.

These folders contain the average lineage thru time data for a given hybrid diversification rate 's'. 
There will be two file types:
* pops_x.csv - these will store the average number of lineages thru time
* pops_cond_x.csv - these will store the average number of lineages thru time when conditioned on survival

The 'x' here corresponds to the simulation condition and the 's' value used. The table in 's_vals.csv' denotes which value for 'x' corresponds to a given value of 's' (mapping 'x' -> 's')