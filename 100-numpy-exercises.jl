# Create null vector of size 10
zeros(10)
falses(10)

# Create null vector of size 10 and set 5th element to 1

Z = zeros(10); Z[5] = 1

# Create vector with values ranging from 10 - 99
Z = collect(10:99)

# Create 3 x 3 matrix with values ranging from 0 to 8

reshape(sample(0:8, replace = true, 9), (3, 3))

# Find indices of non-zero elements from [1, 2, 0, 0, 4, 0]

find(x -> x != 0, [1, 2, 0, 0, 4, 0])
find([1, 2, 0, 0, 4, 0])

# Create 3 x 3 identity matrix

eye(3)
eye(Int, 3)

# Create a 5 x 5 matrix with 1, 2, 3, 4 just below the diagonal

A = zeros(Int, 5, 5)

for j in 1:4
    A[j+1, j] = j
end

diagm(1:4, -1)

# Create a 10 x 10 x 10 array with random values

[(i, j, k) for i in rand(10), j in rand(10), k in rand(10)]
rand(10, 10, 10)

# Create a 8 x 8 matrix and fill it with a checkerboard pattern

C = zeros(Int, 8, 8)