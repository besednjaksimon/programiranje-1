from functools import lru_cache

###############################################################################
# Napisite funkcijo [najdaljse_narascajoce_podazporedje], ki sprejme seznam in
# poisce najdaljse (ne strogo) narascajoce podzaporedje stevil v seznamu.
#
# Na primer: V seznamu [2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9] je najdaljse naj vrne
# rezultat [2, 3, 4, 4, 6, 7, 8, 9].
###############################################################################

s = [2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9]


def najdaljse_narascajoce_podzaporedje(sez):

    @lru_cache(maxsize=None)
    def najdaljse(spodnja_meja, i):
        if i >= len(sez):
            return []
        elif sez[i] < spodnja_meja:
            return najdaljse(spodnja_meja, i+1)
        else:
            vzamemo = [sez[i]] + najdaljse(sez[i], i+1)
            ne_vzamemo = najdaljse(spodnja_meja, i+1)
            if len(vzamemo) > len(ne_vzamemo):
                return vzamemo
            else:
                return ne_vzamemo
    if len(sez) == 0:
        return []
    else:
        return najdaljse(sez[0], 0)

###############################################################################
# Nepreviden študent je pustil robotka z umetno inteligenco nenadzorovanega.
# Robotek želi pobegniti iz laboratorija, ki ga ima v pomnilniku
# predstavljenega kot matriko števil:
#   - ničla predstavlja prosto pot
#   - enica predstavlja izhod iz laboratorija
#   - katerikoli drugi znak označuje oviro, na katero robotek ne more zaplejati

# Robotek se lahko premika le gor, dol, levo in desno, ter ima omejeno količino
# goriva. Napišite funkcijo [pobeg], ki sprejme matriko, ki predstavlja sobo,
# začetno pozicijo in pa število korakov, ki jih robotek lahko naredi z
# gorivom, in izračuna ali lahko robotek pobegne. Soba ima vedno vsaj eno
# polje.
#
# Na primer za laboratorij:
# [[0, 1, 0, 0, 2],
#  [0, 2, 2, 0, 0],
#  [0, 0, 2, 2, 0],
#  [2, 0, 0, 2, 0],
#  [0, 2, 2, 0, 0],
#  [0, 0, 0, 2, 2]]
#
# robotek iz pozicije (3, 1) pobegne čim ima vsaj 5 korakov, iz pozicije (5, 0)
# pa v nobenem primeru ne more, saj je zagrajen.
###############################################################################

soba = [[0, 1, 0, 0, 2],
        [0, 2, 2, 0, 0],
        [0, 0, 2, 2, 0],
        [2, 0, 0, 2, 0],
        [0, 2, 2, 0, 0],
        [0, 0, 0, 2, 2]]


def pobeg(soba, pozicija, koraki):
    max_vrsta = len(soba)
    max_stolpec = len(soba[0])

    @lru_cache(maxsize=None)
    def pobegni(i, j, koraki):
        # Robotek pade iz sobe:
        if not (0 <= i < max_vrsta) or not (0 <= j < max_stolpec):
            return False
        # Robotek pobegne:
        elif soba[i][j] == 1:
            return True
        # Robotek nadaljuje s pobegom:
        elif soba[i][j] == 0 and koraki > 0:
            return any([pobegni(i+1, j, koraki-1),
                        pobegni(i-1, j, koraki-1),
                        pobegni(i, j+1, koraki-1),
                        pobegni(i, j-1, koraki-1)])
        # Robotku zmanjka korakov ali pa zadene oviro:
        else:
            return False
    return pobegni(pozicija[0], pozicija[1], koraki)
