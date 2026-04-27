# Lapsiarjen kuormittavuus

Tässä harrasteprojektissa analysoidaan "Lapsiarjen kuormittavuus" -kyselyn vastauksia.

> Raakadataa ei julkaista tässä repossa. Kyselyvastaukset on kerätty anonyymisti, ja julkiset kuvat esittävät tulokset koontitasolla. Tulokset ovat kuvailevia eivätkä yleistettävissä väestötasolle.

## Ajo

Analyysi ajetaan yhdellä komennolla:

```powershell
Rscript .\scripts\run_pipeline.R
```

Putki lukee lomakevastaukset `data/raw/`-kansiosta, muokkaa ne analyysimuotoon ja tuottaa taulukot sekä kuviot `output/`-kansioon.

PDF-karuselli voidaan muodostaa erillisellä komennolla:

```powershell
Rscript .\scripts\make_linkedin_pdf.R
```

## Rakenne

- `scripts/00_helpers.R` sisältää apufunktioita ja yhteiset asetukset
- `scripts/01_load_clean.R` lukee ja puhdistaa lomakevastaukset
- `scripts/02_reshape_long.R` muotoilee aineiston pitkään muotoon
- `scripts/03_figures.R` sisältää kuvioiden piirto- ja tallennusfunktiot
- `scripts/run_pipeline.R` on pääajuri
- `scripts/make_linkedin_pdf.R` kokoaa lopullisista PNG-kuvista PDF-karusellin

## Tuotokset

Pipeline tuottaa ainakin seuraavat kuviot `output/figures/`-kansioon:

- `01_paakuva.png`
- `herkkyys_paakuva.png`
- `02_heatmap.png`
- `03_aidit_vs_isat.png`
- `04_monesko_lapsi.png`
- `05_taustaprofiili.png`
- `histogrammi.png`
- `sisarus_vs_ei_sisarusta.png`
- `suurin_vs_pienin_pistemaara.png`

PDF-karuselli tallentuu kansioon `output/linkedin/` nimellä `linkedin_karuselli_lapsiarki.pdf`.

## Tekotapa

Analyysiputki ja kuvat on tuotettu VS Codessa Codexin avulla. Oma roolini oli kysymyksenasettelu, analyysin ohjaus, tulosten tarkistus ja visualisointien iterointi.
