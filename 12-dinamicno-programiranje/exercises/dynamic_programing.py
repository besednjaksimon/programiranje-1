from functools import lru_cache

# (*----------------------------------------------------------------------------*]
# Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
# samo za eno polje navzdol ali za eno polje na desno in na koncu mora prispeti
# v desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
# različne (ne-negativne) mase. Miška bi se rada kar se da nažrla, zato jo
# zanima, katero pot naj ubere.

# Funkcija [max_cheese cheese_matrix], ki dobi matriko [cheese_matrix] z masami
# sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
# optimalni poti.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# # max_cheese test_matrix;;
# - : int = 13


def max_cheese(a):

    def vmesni_max(a, i, j):
        if i == len(a) and j == len(a[0]):
            return a[i][j]
        elif i+1 > len(a):
            return 0
        elif j+1 > len(a[0]):
            return 0
        else:
            return a[i][j] + max(vmesni_max(a, i+1, j), vmesni_max(a, i, j+1))

    return vmesni_max(a, 0, 0)

a = [[1, 2, 0], [2, 4, 5], [7, 0, 1]]

# Rešujemo problem sestavljanja alternirajoče obarvanih stolpov. Imamo štiri
# različne tipe gradnikov, dva modra in dva rdeča. Modri gradniki so višin 2 in
# 3, rdeči pa višin 1 in 2.

# Funkcija [alternating_towers] za podano višino vrne število različnih stolpov
# dane višine, ki jih lahko zgradimo z našimi gradniki, kjer se barva gradnikov
# v stolpu izmenjuje (rdeč na modrem, moder na rdečem itd.). Začnemo z
# gradnikom poljubne barve.

# Namig: Uporabi medsebojno rekurzivni pomožni funkciji z ukazom [and].
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# # alternating_towers 10;;
# - : int = 35


def alternating_towers(h):

    def blue(h1):
        if h1 < 0:
            return 0
        elif h1 == 0:
            return 1
        else:
            return red(h1-1) + red(h1-2)

    def red(h2):
        if h2 < 0:
            return 0
        elif h2 == 0:
            return 1
        else:
            return blue(h2-2) + blue(h2-3)

    if h < 0:
        return 0
    elif h == 0:
        return 1
    else:
        return blue(h) + red(h)

# Na nagradni igri ste zadeli kupon, ki vam omogoča, da v Mercatorju kupite
# poljubne izdelke, katerih skupna masa ne presega [max_w] kilogramov. Napišite
# funkcijo [best_value articles max_w], ki poišče največjo skupno ceno, ki jo
# lahko odnesemo iz trgovine, kjer lahko vsak izdelek vzamemo večkrat, nato pa
# še funkcijo [best_value_uniques articles max_w], kjer lahko vsak izdelek
# vzamemo kvečjemu enkrat.

# Namig: Modul [Array] ponuja funkcije kot so [map], [fold_left], [copy] in
# podobno, kot alternativa uporabi zank.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# # best_value articles 1.;;
# - : float = 10.95
# # best_value_unique articles 1.;;
# - : float = 7.66

articles = [("yoghurt", 0.39, 0.18),
            ("milk", 0.89, 1.03),
            ("coffee", 2.19, 0.2),
            ("butter", 1.49, 0.25),
            ("yeast", 0.22, 0.042),
            ("eggs", 2.39, 0.69),
            ("sausage", 3.76, 0.50),
            ("bread", 2.99, 1.0),
            ("Nutella", 4.99, 0.75),
            ("juice", 1.15, 2.0)]


def best_value(articles, max_w):
    @lru_cache(maxsize=None)
    def best_val(w):
        options = []

        for item in articles:
            (_, price, weight) = item
            if w - weight < 0:
                pass
            else:
                option = best_val(w-weight) + price
                options.append(option)
        if options:
            return max(options)
        else:
            return 0

    return best_val(max_w)


def best_value_unique(articles, max_w):
    @lru_cache(maxsize=None)
    def best_val(w, taken):
        options = []

        for i, item in enumerate(articles):
            (_, price, weight) = item
            if w - weight < 0 or taken[i] == "1":
                pass
            else:
                new_taken = taken[:i] + "1" + taken[i+1:]
                option = best_val(w-weight, new_taken) + price
                options.append(option)
        if options:
            return max(options)
        else:
            return 0

    return best_val(max_w, "0"*len(articles))
