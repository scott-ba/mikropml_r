raw_data/baxter.% :
  wget https://
  unzip 0.3.zip
  mv raw_data-0.3 raw_data
  rm 0.3.zip

SEEDS = $(shell seq 1 1 10)
L2_RDS = $(patsubst %,processed_data/l2_genus_%.Rds,$(SEEDS))

# use placeholder % and add $* to command to specify seed a runtime.
# to call, use make -j 4 processed_data/l2_genus_pooled_performance.tsv
# using four parrallel processors.
# add l2_genus scrip as dependancy, to make dry.

$(L2_GENUS_RDS) : code/run_split.R code/genus_process.R\
																code/l2_genus.R\
																raw_data/baxter.metadata.tsv\
																raw_data/baxter.cons.taxonomy\
																raw_data/baxter.subsample.shared
	./code/run_split.R $@ code/l2_genus.R
																
	
processed_data/l2_genus_pooled_%.tsv : code/combine_models.R $(L2_RDS)
		$^
		

		
RF_GENUS_FIT_RDS = $patsubst %.processed_data/rf_genus_fit.Rds,$(SEEDS))

$(RF_GENUS_FIT_RDS)  :  code/run_split.R code/genus_process.R\
												code/rf_genus_fit.R\
												raw_data/baxter.metadata.tsv\
																raw_data/baxter.metadata.tsv\
																raw_data/baxter.cons.taxonomy\
																raw_data/baxter.subsample.shared
		./code/run_split.R $@ code/rf_genus_fit.R\														
 
 processed_data/rf_genus_fit_performance.tsv : code/combine_models.R\
												$(RF_GENUS_FIT_RDS)
$^



$(L2_GENUS_RDS) : code/run_split.R code/genus_process.R\
																code/l2_genus.R\
																raw_data/baxter.metadata.tsv\
																raw_data/baxter.cons.taxonomy\
																raw_data/baxter.subsample.shared
	./code/run_split.R $@ code/l2_genus.R
																
	
processed_data/l2_genus_pooled_%.tsv : code/combine_models.R $(L2_RDS)
		$^
		
		