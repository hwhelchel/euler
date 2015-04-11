--A Pythagorean triplet is a set of three natural numbers,
-- a < b < c, for which,

--a^2 + b^2 = c^2
--For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

--There exists exactly one Pythagorean triplet for
--which a + b + c = 1000.
--Find the product abc.

pythagoreanTriplet :: Int -> [Int]
pythagoreanTriplet x = take 1 [ a * b * c | c <- [1..], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c == x]

-- pythagoreanTriplet 1000
