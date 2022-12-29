def swaprows(mat, i: int, j: int):
    tmp = mat[i]
    mat[i] = mat[j]
    mat[j] = tmp
    return mat

A = [[1, 2], [3, 4]]
print(swaprows(A, 0, 1))
print(A)
