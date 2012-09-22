from math import log


def _mul_vec(vecA, vecB):
    size = len(vecA)

    if size != len(vecB):
        raise ArgumentException("Invalid vector size!")

    result = [0.0] * size

    for i in range(size):
        result[i] = vecA[i] * vecB[i]

    return result


def _compute_mi(classNum, p, pClasses):
    for i in range(classNum):
        for j in range(i + 1, classNum):
            pClasses[i][j] = p[i] / (p[i] + p[j])
            pClasses[j][i] = 1.0 - pClasses[i][j]

    return pClasses


def _norm(p):
    size = len(p)
    sumP = sum(p)

    for i in range(size):
        p[i] = p[i] / sumP

    return p


def _compute_p(classNum, tempSum, n, pClasses, p):
    for i in range(classNum):
        p[i] = p[i] * tempSum[i] / sum(_mul_vec(n[i], pClasses[i]))

    return _norm(p)


def _compute_conv(classNum, n, r, pClasses):
    conv = 0.0
    for j in range(classNum):
        for i in range(j):
            mr = r[i][j]
            ir = 1.0 - mr
            npClasses = pClasses[i][j]
            inpClasses = 1.0 - npClasses
            conv = conv + n[i][j] * (mr * log(mr / npClasses) +
                      ir * log(ir / inpClasses))
    
    return conv


def pwc(r, n, maxIt):
    # Funkcja wyznaczająca wektor prawdopodobieństw klas.
    # Wyznaczanie odbywa się metodą iteracyjną, przybliżającą
    # prawdopodobieństwa warunkowe par klas do prawdopodobieństw wejściowych.
    #   n - macierz liczebności przypadków uczących klasyfikatory parowe.
    #   r - prawdopodobieństwa wejściowe.
    #   maxIt - maksymalna liczba iteracji.

    classNum = len(r)
    
    p = [0.0] * classNum
    tempSum = [0.0] * classNum

    pClasses = [[0.0 for i in range(classNum)] for j in range(classNum)]

    iterations = 0

    for c in range(classNum):
        tempSum[c] = sum(_mul_vec(n[c], r[c]))
        p[c] = 2.0 * sum(r[c]) / (classNum * (classNum - 1))

    p = _norm(p)

    pClasses = _compute_mi(classNum, p, pClasses)

    conv_after = _compute_conv(classNum, n, r, pClasses)
    conv_before = conv_after + 1.0

    while (conv_before - conv_after) >= 0.0:
        iterations = iterations + 1
        conv_before = conv_after

        p = _compute_p(classNum, tempSum, n, pClasses, p)
        pClasses = _compute_mi(classNum, p, pClasses)

        conv_after = _compute_conv(classNum, n, r, pClasses)

        if iterations >= maxIt:
            break

    return (p, pClasses, iterations)
