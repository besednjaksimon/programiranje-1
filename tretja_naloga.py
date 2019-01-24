from functools import lru_cache

# 3. naloga

test1 = [2, 4, 1, 2, 1, 3, 1, 1, 5]
test2 = [4, 1, 8, 2, 11, 1, 1, 1, 1, 1]
test3 = [10 for i in range(50)]


def zabica(mocvara):
    dolzina = len(mocvara)

    @lru_cache(maxsize=None)
    def skok(polozaj, energija):
        if polozaj >= dolzina:
            return 0
        moznosti = []
        for j in range(1, energija+1):
            if polozaj+j >= dolzina:
                return 1
            else:
                moznost = skok(polozaj+j, energija-j+mocvara[polozaj+j]) + 1
                moznosti.append(moznost)
        if moznosti:
            return min(moznosti)
        else:
            return 0
    return skok(0, mocvara[0])
        