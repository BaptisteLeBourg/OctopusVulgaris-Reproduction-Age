# Octopus maturity and age

This repository provides the data and script used in the paper for "Biology of a re-emerging population of octopus from north-eastern Atlantic" by Le Bourg et al. (2025).

It contains three tables: 

Data-Biometry.csv provides biometry data for octopus sampled in the Concarneau fish market. The columns are:
- Date: Date of the sampling.
- YearAndMonth: Year and month during which the sampling occurred.
- Sex: Sex of the individual for which the biometry has been assessed.
- Weight: Weight (kg) of the individual for which the biometry has been assessed.
- WeightClass: Weight class the individual belongs to (30—less than 1 kg; 20— 1 to 2 kg; 10— 2 to 3 kg; 11—more than 3 kg).
- VentralMantleLength: Ventral mantle length (cm) of the individual for which the biometry has been assessed.

Data-Landings.csv provides total landings of octopuses in the Concarneau fish market during the sampling period. The columns are:
- Month: Month during which the landings occurred.
- Year: Year during which the landings occurred.
- YearAndMonth: Combination of the two previous columns.
- WeightClass: Weight class of the octopuses (30—less than 1 kg; 20— 1 to 2 kg; 10— 2 to 3 kg; 11—more than 3 kg).
- Landings: Total weight (kg) of landed octopuses.

Data-Maturity-Age.csv provides data on maturity and age in octopuses sampled in the Concarneau fish market or not. The columns are:
- Individual: Number of the sampled individual.
- Date: Date of the sampling.
- Month: Month during which the sampling occurred.
- Year: Year during which the sampling occurred.
- YearAndMonth: Combination of the two previous columns.
- Season: Season during which the sampling occurred (winter for January, February and March; spring for April, May and June; summer for July, August and September; autumn for October, November and December).
- Sex: Sex of the individual for which maturity and age have been assessed.
- Maturity: Maturity stage of the sample individual.
- Weight: Weight (kg) of the sampled individual.
- WeightClass: Weight class the individual belongs to (30—less than 1 kg; 20— 1 to 2 kg; 10— 2 to 3 kg; 11—more than 3 kg).
- VentralMantleLength: Ventral mantle length (cm) of the sampled individual.
- AgeReading1: First reading of the number of increments on the inner lateral wall surface of the upper beak.
- AgeReading2: Second reading of the number of increments on the inner lateral wall surface of the upper beak.
- AgeReading3: Third reading of the number of increments on the inner lateral wall surface of the upper beak.
- MeanAge: Mean of the two age readings with the lowest coefficient of variation (CV). 
- StandardDeviation: Standard deviation of the two age readings with the lowest coefficient of variation (CV). 
- CV: Coefficient of variation of the two age readings with the lowest coefficient of variation. 
- HatchingDate: Hatching date of the sampled individual (Date - MeanAge).
- HatchingMonth: Hatching month of the sampled individual.
- HatchingYear: Hatching year of the sampled individual.
- HatchingSeason: Hatching season of the sampled individual (winter for January, February and March; spring for April, May and June; summer for July, August and September; autumn for October, November and December).
- SampledInFishMarket: Specify whether the individual was sampled in the fish market or directly provided by fishermen.

Six R scripts are provided:

GrowthRegions.R is the script comparing the growth of octopuses from Southern Brittany with other regions.

GrowthSeasonsLength.R is the script assessing the relationship between the number of increments and the ventral mantle length in octopuses.

GrowthSeasonsWeight.R is the script assessing the relationship between the number of increments and the weight in octopuses.

Hatching.R is a script to make a plot displaying the hatching frequencies for each month and each year. This plot does not appear in the paper but shows that octopuses hatch throughout the year.

MaturityMonths.R is the script assessing the reproduction period.

MaturityOgives.R is the script assessing the weight, length and age at maturity with maturity ogives.
