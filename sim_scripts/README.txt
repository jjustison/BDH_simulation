Here you will find all the R scripts used to simulate, analyze, and create figures

Simulation & Analysis Files:
* param_setups.R - This is the core simulation script that is ran to generate each dataset. This is where each specific simulation condition is defined and simulations. It calls common_pars.R and sim_runs.R to generate datasets. It then calls net_analysis.R or net_analysis_rec_only to collect data on each simulated phylogeny 
* common_pars.R - This script loads all the parameters that are common to all simulations
* sim_runs.R - This is the script that actually performs the simulations once all needed parameters are loaded
* net_analysis.R - This collects the core data from each simulated phylogeny, Class membership, number of tips, number of reticulations, etc. This is performed for the complete and reconstructed (extant-only) phylogenies 
* net_analysis_rec_only.R - This collects the core data from each simulated phylogeny, Class membership, number of tips, number of reticulations, etc. This is performed only for reconstructed (extant-only) phylogenies 

R packages required: SiPhyNetwork, R.utils, ape, fst

All simulation data gets saved in a folder named "data" in the parent directory. More details about dataset files in the "data" README.txt


Analysis & Plotting Files
* Summarize_data.R - This script summarizes all the datasets and computes averages/proportions of the various aspects of the phylogenies
* plotting.R - creates the plots used in figures

R packages required: tidyr, dplyr, ggtern, reshape2, rcartocolor, cowplot, gridGraphics, xtable, ggplot2

Plots get saved in the "Figures" folder
