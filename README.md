# Rawson-spatial-covid
# orderly

This is an [`orderly`](https://github.com/vimc/orderly) project, holding the analysis for our manuscript

> "The impact of health inequity on regional variation of COVID-19 infection in England".

## Running

A sequence of tasks needs to be run with a set of parameters to generate the final results.  This is sketched out in the `Master_script.R` script, though this is provided only as a form of documentation. In practice these were run over several days on a HPC.


1. Run the `01_Prepare_Data` task to prepare the raw data for fitting.
2. Run the `02_model_fit_A` task to fit the model.
3. Run the `03_model_summaries_A` task to produce model summaries. 
4. Run the `04_nb_CI_output` task to prepare credible intervals for plotting.
5. Run the `05_manuscript_figures` task to produce figures seen in the manuscript.


## Requirements


You will need a recent [orderly](https://www.vaccineimpact.org/orderly/) which can be installed with

```r
drat:::add("vimc")
install.packages("orderly")
```

You will also need to follow the specific install instructions for [rstan](https://mc-stan.org/users/interfaces/rstan).

## License

MIT © Imperial College of Science, Technology and Medicine
