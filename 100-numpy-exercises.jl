
srand(20130810)

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

C[1:2:end, 2:2:end] = 1
C[2:2:end, 1:2:end] = 1

[(i+j)%2 for i in 1:8, j in 1:8]

# Create a 10 x 10 array with random values and find the maximum and minimum values

R = rand(10, 10)
maximum(R), minimum(R)
extrema(R)

# Create the checkerboard using the repmat function

repmat([0 1;1 0], 4, 4)

# Normalize a 5 x 5 random matrix

R = reshape(sample(1:10, 25, replace = true), 5, 5)
R_min, R_max = extrema(R)

R = (R .- R_min)./(R_max - R_min)

# Multiply 5 x 3 matrix by 3 x 2 matrix

A = reshape(sample(1:10, 15, replace = true), 5, 3)
B = reshape(sample(1:10, 6, replace = true), 3, 2)
A * B

# Calculate 10 x 10 matrix with row values between 0 to 9

reshape(sample(0:9, 100, replace = true), 10, 10)
[y for x in 1:10, y in 0:9]

# Create a vector of size 1000 with values ranging from 0 to 1, both excluded

rand(1000)
linspace(0, 1, 1002)[2:end-1]

# Create a random vector of size 100 and sort it

sort!(rand(100))

# Consider two random matrices A and B and check if they are equal

A = rand(10, 10)
B = rand(5, 5)

isequal(A, B)
A == B

# Create a random vector of size 1000 and find its mean

mean(rand(1000))

# Consider a 1000 x 2 random matrix representing cartesian coordinates, convert them to polar coordinates

A = reshape(sample(1:10, 2000, replace = true), 1000, 2)

X = A[:, 1], Y = A[:, 2]
R = sqrt.(X .* X + Y .* Y)
T = atan2(Y, X)

# Create a random vector of size 100 and replace the maximum value by 0

V = rand(100)

V[indmax(V)] = 0

# Create a structured array with x and y coordinates covering the [0, 1] x [0, 1] area

[(x, y) for x in linspace(0, 1, 100),
            y in linspace(0, 1, 100)]

# Print the maximum and minimum value for each Julia scalar type

for dtype in [Int8, Int16, Int32, Int64, Int128]
    println("max = ", typemax(dtype), ", min = ", typemin(dtype))
end

# Consider a random vector with shape (100, 2) representing coordinates. Find point-by-point distances

Z = rand(100, 2)

X = Z[:, 1], Y =  Z[:, 2]

sqrtm((X .- X.')^2 + (Y .- Y.')^2)

# Generate 2D gaussian like array

sigma, mu = 1.0, 0.0
[exp(-(x-mu).^2/(2.0*sigma^2) - (y - mu).^2/(2.0*sigma^2)) for x in linspace(-1, 1, 100),
 y in linspace(-1, 1, 100)]

#  Consider the vector [1, 2, 3, 4, 5]. Create a vector with 0's interleaved between each value

V = [1, 2, 3, 4, 5]
W = Int[]

for (i, j) in zip(V, zeros(Int, 5))
    push!(W, i, j)
end

# Find the nearest value from any given value in an array

V = rand(100)

function find_nearest(V::Vector{T}, v::T) where T <: Real

    min_index = indmin(abs.(V - v))

    return V[min_index]

end

find_nearest(V, 0.2)

