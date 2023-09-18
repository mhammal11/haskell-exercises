{--
Michael Hammal
--}

{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad (void)
import Test.QuickCheck
import Test.Hspec
import Data.List


-- TODO square each number in the list
-- Apply the square function to every element in the list through map
square :: [Int] -> [Int]
square xs = map (^2) xs

-- TODO returns the sum of the lengths of the individual strings in the list
-- Using foldr along with function composition of the length function to calculate the length of each string
-- and the addition function to add all the different lengths
lengths :: [String] -> Int
lengths ss = foldr ((+) . length) 0 ss

-- TODO the output list includes three consecutive copies of every element in the input list
-- Using foldr along with function composition of the replicate function to replicate each element 3 times
-- and the concatenation function to produce one list with all the elements
triplicate :: [a] -> [a]
triplicate as = foldr ((++) . (replicate 3)) [] as

-- TODO returns True only if all the numbers in the input list are even
-- Using foldr along with function composition of the even function to determine if a number is even
-- and the && function to combine the boolean results.
allEven :: [Int] -> Bool
allEven ns = foldr ((&&) . even) True ns

-- TODO multiplies all the elements in the input list
-- Using foldr along with the multiplication function to multiply every element in the list
mul :: [Int] -> Int
mul ns = foldr (*) 1 ns

-- TODO adds all the elements in the input list modulo 10
-- Using foldr along with the addition function to add every element in the list and then 
-- using the modulo function to compute mod 10.
checkSum :: [Int] -> Int
checkSum ns = foldr (+) 0 ns `mod` 10

-- TODO returns the largest element in the input list
-- Using foldr along with the max function to compute the largest element in the list by
-- checking the max between every two adjacent elements 
largest :: [Int] -> Int
largest ns = foldr max 0 ns

-----------------------------------------------------------------------------------------------------------------------
-- Sample Property for `mul`
prop_mul_concat :: [Int] -> [Int] -> Bool
prop_mul_concat xs ys = mul (xs++ys) == mul xs * mul ys

--- Add your properties here bellow the sample. Describe why you think this property holds based on your implementation.
-- Simple concatenation equality property. Squaring a list first or concatenating first has no effect on the result.
prop_square_concat :: [Int] -> [Int] -> Bool
prop_square_concat xs ys = square (xs++ys) == square xs ++ square ys

-- Resulting list from the square function should not contain any negative numbers since squaring a number always
-- returns a positive result.
prop_square_no_negative :: [Int] -> Bool
prop_square_no_negative xs = null ([x | x <- square xs, x < 0])

-- Simple concatenation equality property. Calculating the sum of the lengths of each element in one list first and  
-- then adding the sums or concatenating first has no effect on the result.
prop_lengths_concat :: [String] -> [String] -> Bool
prop_lengths_concat xs ys = lengths (xs++ys) == lengths xs + lengths ys

-- Property to check if the foldr implementation gives the same result as a map implementation 
prop_lengths_map :: [String] -> Bool
prop_lengths_map xs = lengths xs == sum (map length xs)

-- Property to check if the foldr implementation gives the same result as the intercalate function
prop_lengths_intercalate :: [String] -> Bool
prop_lengths_intercalate xs = lengths xs == length (intercalate "" xs)

-- Simple concatenation equality property. Triplicating a list first or concatenating first has no effect on the result.
prop_triplicate_concat :: Eq a => [a] -> [a] -> Bool
prop_triplicate_concat xs ys = triplicate (xs++ys) == triplicate xs ++ triplicate ys

-- Property to check if the foldr implementation gives the same result as a concatMap implementation
prop_triplicate_concatMap :: Eq a => [a] -> Bool
prop_triplicate_concatMap xs = triplicate xs == concatMap (replicate 3) xs

-- Property to check if the foldr implementation with replicate gives the same result as a customn function
prop_triplicate_custom :: Eq a => [a] -> Bool
prop_triplicate_custom xs = triplicate xs == foldr (\x y -> x : x : x : y) [] xs

-- Property to check if the foldr implementation gives the same result as list comprehension
prop_triplicate_list_comprehension :: Eq a => [a] -> Bool
prop_triplicate_list_comprehension xs = triplicate xs == [y | x <- xs, y <- [x,x,x]]

-- Simple concatenation equality property. Checking if all elements are even in each list first and then using the &&
-- function to combine the results or concatenating the lists first has no effect on the result.
prop_allEven_concat :: [Int] -> [Int] -> Bool
prop_allEven_concat xs ys = allEven (xs++ys) == (allEven xs && allEven ys)

-- Property to check if the foldr implementation gives the same result as the all function
prop_allEven_all :: [Int] -> Bool
prop_allEven_all xs = allEven xs == all even xs

-- Property to check if the foldr implementation gives the same result as the product function
prop_mul_product :: [Int] -> Bool
prop_mul_product xs = mul xs == product xs

-- Property to check if the foldr implementation gives the same result as a recursive implementation
prop_mul_recusrive :: [Int] -> Bool
prop_mul_recusrive xs = mul xs == recProd xs
  where recProd [] = 1
        recProd (x : xs) = x * recProd xs

-- Simple concatenation equality property. Checking if the sum modulo 10 in each list first and adding the results and
-- computing modulo 10 of that or concatenating the lists first has no effect on the result.
prop_checkSum_conat :: [Int] -> [Int] -> Bool
prop_checkSum_conat xs ys = checkSum (xs++ys) == mod (checkSum xs + checkSum ys) 10

-- Propert to check if the foldr implementation gives the same result as the sum function
prop_checkSum_function :: [Int] -> Bool
prop_checkSum_function xs = checkSum xs == sum xs `mod` 10

-- Simple concatenation equality property. Computing the largest element in each list first and computing the largest
-- of the two largest elements from each list or concatenating the lists first has no effect on the result.
prop_largest_concat :: [Int] -> [Int] -> Bool
prop_largest_concat xs ys = largest (xs++ys) == max (largest xs) (largest ys)

-- Propert to check if the foldr implementation gives the same result as the maximum function with no negative numbers
prop_largest_maximum :: [Int] -> Bool
prop_largest_maximum xs = largest xs == if null xs || null [x | x <- xs, x >= 0] then 0 else maximum xs

-- End --
return []

allProps = void $quickCheckAll


--- Your implementation suppose to pass the following sample tests
main:: IO()
main =
  hspec $ describe "UnitTests for A1 Implementatoins:" $ do
    specify "Square Unit Test:" $
      square [1,2,3] `shouldBe` [1,4,9]
    specify "Length Unit Test:" $
      lengths ["abc", "12", "wxyz", "ABCDEFG"] `shouldBe` 16
    specify "Triplicate Unit Test:" $
      triplicate [1,2,3] `shouldBe` [1,1,1,2,2,2,3,3,3]
    specify "AllEven Unit Test:" $ do
      allEven [2,4,6] `shouldBe` True
      allEven [2,3,6] `shouldBe` False
    specify "Mul Unit Test:" $
      mul [1,2,3,4,5] `shouldBe` 120
    specify "CheckSum Unit Test:" $
      checkSum [1,2,3,4,5,6] `shouldBe` 1
    specify "Largest Unit Test:" $
      largest [1,2,9,3,4] `shouldBe` 9
    specify "QuickCheck the properties" $ property allProps