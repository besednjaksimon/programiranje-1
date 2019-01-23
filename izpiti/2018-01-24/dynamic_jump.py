from functools import lru_cache

#  ==========================================================================
#  NALOGA 4.1
#
#  Lisjaček krade jabolka v sadovnjaku, kjer so jablane razporejene v vrstah.
#  Vsaka jablana ima različno število jabolk, ki jih lisjaček lahko doseže.
#  Okrade lahko zgolj N jablan preden ga prežene lastnik sadovnjaka.
#
#  Ker je lisjaček še mlad in nevešč kraje jabolk, v vsaki vrsti obira
#  jablane po vrsti, ko pa se odloči da se bo lotil naslednje vrste se ne
#  vrača več nazaj. V primeru, da se znajde na koncu zadnje vrste svojo
#  avanturo zaključi, ne glede na to ali ima na voljo še kakšno obiranje.
#
#  Napišite algoritem, ki izračuna največje število jabolk, ki jih lisjaček
#  lahko ukrade. Sadovnjak predstavimo z matriko, kjer vrste predstavljajo
#  vrste sadovnjaka in vrednosti v matriki število jabolk na jablani.
#  V smislu matrike se lahko lisjaček premika zgolj v desno ali pa na začetek
#  nove vrstice, kjer za vsak premik porabi en korak.
#
#  Primer: | 1 2 0 5 |      N = 6       ------>  17
#          | 0 4 1 1 |
#          | 8 0 4 2 |
#
#  Za vse točke mora biti funkcija učinkovita (ne eksponentna časovna
#  zahtevnost).
#
#  ==========================================================================

test_matrix1 = [[1, 2, 0, 5], [0, 4, 1, 1], [8, 0, 4, 2]]
test_matrix2 = [[2, 4, 1, 1], [3, 2, 0, 5], [8, 0, 7, 2]]


def max_apples(matrix, max_steps):
    n = len(matrix)
    m = len(matrix[0])

    @lru_cache(maxsize=None)
    def position(i, j, steps_left):
        if steps_left > 0:
            if j >= m:
                return position(i+1, 0, steps_left-1)
            elif i >= n:
                return 0
            else:
                return matrix[i][j] + max(position(i, j+1, steps_left-1),
                                          position(i+1, 0, steps_left-1))
        else:
            return 0
    return position(0, 0, max_steps)
