def find_inverses(p: int) -> list[int]:
    print(f"Finding inverses of {p}")
    inverses = []
    for elem in range(1, p):
        for possible_inverse in range(1, p):
            if (elem * possible_inverse) % p == 1:
                inverses.append(possible_inverse)
    assert len(inverses) == (p-1)
    return inverses

print(find_inverses(7))
print(find_inverses(11))


