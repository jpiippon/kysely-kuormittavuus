# Data dictionary

Main data file: `data/raw/forms_responses.csv`

Key columns:
- `Timestamp`: response timestamp
- `Synnytitkö tarkasteltavan lapsen/lapset?`: Kyllä / En
- `Mitä lasta tämä vastaus koskee?`: Ensimmäistä lastani / Toista tai myöhempää lastani / Lapsiani yleisesti
- `Syntyikö arvioimillesi lapsille(lapselle) sisaruksia(sisarus) ennen 3v ikää? Tämä voi sekoittaa pakkaa..`: Kyllä / Ei / missing

Burden columns in the correct chronological order:
1. `Syntymästä 3 viikon ikään`
2. `3 viikon iästä 3 kuukauden ikään`
3. `3–6 kk `
4. `6–12 kk`
5. `1–1,5 vuotta (12-18kk)`
6. `1,5–2 vuotta (18-24kk)`
7. `2–2,5 vuotta (24-30kk)`
8. `2,5–3 vuotta (30kk-36kk)`

Burden scale:
- 0 = ei lainkaan kuormittavaa
- 10 = äärimmäisen raskasta