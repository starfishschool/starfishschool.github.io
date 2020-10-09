def divide_by_number(num):
    return 1.0 / num


def multiply_by_inverted_number(num1, num2):
    return num1 * divide_by_number(num2)


def two_dim_loop(val):
    for i in range(val):
        for j in range(val):
            multiply_by_inverted_number(i, j)


two_dim_loop(3)
