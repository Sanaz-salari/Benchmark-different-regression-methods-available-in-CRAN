datasets <- c(
  "ceo_pay", "AIG_daily", "diamonds", "energy", "beijin_pm2.5", "ami_charges", "ny_bridges",
  "wine_quality", "named_uci_data", "Parkinsons", "sleep", "garments", "weight", "boston", "cholestrol", "graduate", "iceCream",
  "Saratoga_house_prices", "cigarettes", "forest_fires", "QSAR_fish", "NBA_Betting_Odds", "Predictive_maintenance", "QSAR_aquatic", "Fuel_economy",
  "Seoul_Bike_Sharing_Demand", "Amazon_book_dataset", "Movie_profit_dataset", "PVC", "Cost_of_living_2018", "Real_Estate_Cost_Evaluation",
  "Commercial_properties"
)

urls <- c(
  "https://dasl.datadescription.com/download/data/3662", 
  "https://dasl.datadescription.com/download/data/3046",
  "https://dasl.datadescription.com/download/data/3162",
  "https://archive.ics.uci.edu/ml/machine-learning-databases/00374/energydata_complete.csv",
  "https://archive.ics.uci.edu/ml/machine-learning-databases/00381/PRSA_data_2010.1.1-2014.12.31.csv",
  "https://dasl.datadescription.com/download/data/3263",
  "https://dasl.datadescription.com/download/data/3364", 
  "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", 
  "https://archive.ics.uci.edu/ml/machine-learning-databases/00471/Data_for_UCI_named.csv",
  "https://www.openml.org/data/get_csv/1798093/phpFBxu1w",
  "https://www.openml.org/data/get_csv/52979/rmftsa_sleepdata.arff",
  "https://archive.ics.uci.edu/ml/machine-learning-databases/00597/garments_worker_productivity.csv",
  "https://www.openml.org/data/get_csv/52738/bodyfat.arff",
  "https://www.openml.org/data/get_csv/52643/boston.arff",
  "https://www.openml.org/data/get_csv/3641/dataset_2190_cholesterol.arff",
  "https://www.sheffield.ac.uk/polopoly_fs/1.937198!/file/Graduate_R.csv",
  "https://www.sheffield.ac.uk/polopoly_fs/1.937201!/file/Ice_cream_R.csv", 
  "https://dasl.datadescription.com/download/data/3437", 
  "https://dasl.datadescription.com/download/data/3113",
  "https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv",
  "https://archive.ics.uci.edu/ml/machine-learning-databases/00504/qsar_fish_toxicity.csv",
  "http://users.stat.ufl.edu/~winner/data/nbaodds201415.csv",
  "http://archive.ics.uci.edu/ml/machine-learning-databases/00601/ai4i2020.csv",
  "https://archive.ics.uci.edu/ml/machine-learning-databases/00505/qsar_aquatic_toxicity.csv",
  "https://dasl.datadescription.com/download/data/3225", 
  "https://archive.ics.uci.edu/ml/machine-learning-databases/00560/SeoulBikeData.csv", 
  "https://dasl.datadescription.com/download/data/3052",
  "https://dasl.datadescription.com/download/data/3349",
  "https://dasl.datadescription.com/download/data/3415",
  "https://dasl.datadescription.com/download/data/3613",
  "https://dasl.datadescription.com/download/data/3423", 
  "https://dasl.datadescription.com/download/data/3122" 
)

for(i in seq_along(datasets)) {
  dat <- data.table::fread(urls[i], data.table = F)
  perf_thrshld <- 1000
  if(nrow(dat) > perf_thrshld) {
    subset_indx <- sample(nrow(dat), perf_thrshld)
    dat <- dat[subset_indx, ]
  }
  assign(datasets[i], dat)
}

rm(list = c("dat", "datasets", "i", "perf_thrshld", "subset_indx", "urls"))

save.image(file = "regression2_datasets.RData")
