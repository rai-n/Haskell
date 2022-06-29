module Ratings where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Data.List 
import qualified Data.List as List

type Customer = String
type Product = String
type Score = Int

-- a single rating of a product by a customer
type Rating = (Customer, Product, Score)

-- a list of all customers who submitted a rating, without duplicates
customers :: [Rating] -> [Customer]
customers = Set.elems . Set.fromList . map(\ (a, b, c) -> (a)) 
     
-- a list of all products that have been rated, without duplicates
products :: [Rating] -> [Product]
products = Set.elems . Set.fromList . map(\ (a, b, c) -> (b)) 

-- customers and the number of products they have rated
numScores :: [Rating] -> [(Customer, Int)]
numScores ws = [(head g, length g) | g <- group (sort xs)]
    where xs = map(\ (a, b, c) -> (a)) ws 

-- a list of the customers who give the same score in all their ratings
-- consistent :: [Rating] -> [Customer]
    
similar :: Ord a => [(a, b)] -> [(a, [b])]
similar = Map.toList . Map.fromListWith (++) . map pairup 
  where
    pairup :: (a, b) -> (a, [b])
    pairup (a, b) = (a, [b])

allTrue :: Eq a => [a] -> Bool
allTrue (x:xs) = all (x ==) xs

consistent :: [Rating] -> [Customer]
consistent = Map.keys
  . Map.filter allTrue
  . Map.fromListWith (++)
  . map pairup
  where
    pairup :: Rating -> (Customer, [Int])
    pairup (a, _, c) = (a, [c])
 
average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)

-- the average score of each product
averageScore :: [Rating] -> [(Product, Double)]
averageScore = map (\(p, xs) -> (p, average xs)) . similar . map(\ (a, b, c) -> (b, fromIntegral c)) 

-- the products that have each been rated by every customer
popularHelper :: [Rating] -> [(Product, Int)]
popularHelper = map (\(p, xs) -> (p, length xs)) . similar . map(\ (a, b, c) -> (b, a))
  
popular :: [Rating] -> [Product]
popular xs = map fst 
  . filter (\(_,b) -> b == len)
  . popularHelper $ xs
  where 
    len = (length . customers $ xs)

 