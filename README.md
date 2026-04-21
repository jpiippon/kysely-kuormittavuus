# Kyselyn kuormittavuus -analyysiputki

Tämä projekti tuottaa tiivistetyt taulut ja kuviot kyselyn lapsiperheille suunnatuista hoivakuormitusarvioista ikäjaksoittain.

## Raakadatan sijainti

Raaka CSV -aineisto odotetaan polussa:

- `data/raw/forms_responses.csv`

Jos tiedostoa ei löydy, käytetään varatiedostoa:

- `data/raw/form_responses.csv.csv`

## Miten ajetaan

1. Suorita analyysi putkena:
   - `Rscript scripts/run_pipeline.R`
2. Varmista, että kansiot `output/tables` ja `output/figures` ovat luotu ja tiedostot syntyvät.

## Tuotokset

Projektin ajon jälkeen syntyy:

- `output/tables/age_interval_summary.csv` (ikäjaksoittaiset yhteenvetotunnusluvut)
- `output/figures/01_burden_trajectory_median_iqr.png` (mediaani + IQR)
- `output/figures/02_burden_variation_spaghetti.png` (yksilölliset arviot + mediaani + IQR)

