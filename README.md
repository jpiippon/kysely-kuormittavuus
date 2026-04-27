# Kyselyn kuormittavuus

Tässä harrasteprojektissa analysoidaan "Lapsiarjen kuormittavuus" -kyselyn vastauksia.

## Miten analyysi tehdään

Analyysi ajetaan yhdellä komennolla:

```powershell
Rscript .\scripts\run_pipeline.R
```

Putki lukee lomakevastaukset `data/raw/`-kansiosta, muokkaa ne analyysimuotoon ja tuottaa lopuksi taulukot sekä kuviot `output/`-kansioon.

## Rakenne

- `scripts/00_helpers.R` sisältää apufunktioita ja yhteiset asetukset
- `scripts/01_load_clean.R` lukee ja puhdistaa lomakevastaukset
- `scripts/02_reshape_long.R` muotoilee aineiston pitkään muotoon
- `scripts/03_figures.R` sisältää kuvioiden piirto- ja tallennusfunktiot
- `scripts/run_pipeline.R` on pääajuri

## Tuotokset

Pipeline tuottaa ainakin seuraavat kuviot `output/figures/`-kansioon:

- `pääkuva.png`
- `lämpökartta.png`
- `isä_vs_äiti.png`
- `ensimmäinen_vs_monesko.png`
- `histogrammi.png`
- `taustakysymykset.png`
- `sisarus_vs_ei_sisarusta.png`
- `suurin_vs_pienin_pistemaara.png`

## Tekotapa

Koko koodi on tehty VS Codessa Codexin avulla, enkä kirjoittanut itse yhtään koodiriviä.
