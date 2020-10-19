import System.IO
import qualified Data.Map as Map
import Data.List (sortOn, sortBy)

remain :: Int -> Int -> Bool
remain divider num
    | r == 0 = False
    | otherwise = True
    where r = rem num divider

--getNumPrime :: [Int] -> Int -> Int
--getNumPrime [] acc = acc
--getNumPrime (hd:tl) acc = getNumPrime (filter (remain hd) tl) (acc + 1)
--hw1 :: Int -> Int
--hw1 num 
--        | num >= 2 = getNumPrime [2..num] 0
--        | otherwise = 0

-- 1
getNumPrime :: Int -> Int
getNumPrime num
    | num >= 2 = removeDivisible [2..num] 0
    | otherwise = 0
    where removeDivisible (hd:tl) acc  
                | tl == [] = acc + 1
                | otherwise = removeDivisible (filter (remain hd) tl) (acc + 1)

isPrime num
        | num >= 2 = findDivisor num 2
        | otherwise = 0
        where findDivisor num' divisor  | divisor * divisor > num' = 1
                                        | (rem num' divisor) == 0 = 0
                                        | otherwise = findDivisor num' (divisor + 1)
-- 2
getNthPrime :: Int -> Int
getNthPrime cnt = findNthPrime 2 cnt
    where findNthPrime num cnt' | cnt'== 0 = (num - 1)
                                | otherwise = findNthPrime (num + 1) (cnt' - (isPrime num))

--3
getNearestFibonacci :: Int -> Int
getNearestFibonacci num
                | num == 0 = 1
                | otherwise = findNearestFibonacci num 1 2
                where findNearestFibonacci num' fprev fcur
                        | diff > 0 = findNearestFibonacci num' fcur (fprev + fcur)
                        | otherwise = fprev
                        where diff = abs (num' - fprev) - abs (num' - fcur)


--4
getMinNdividers :: Int -> Int
getMinNdividers num = findMindNdividers nums 1 1
    where nums = primeFactorization num
          findMindNdividers (hd:tl) nth acc | tl == [] = (acc*((getNthPrime nth)^(hd - 1)))
                                            | otherwise = findMindNdividers tl (nth + 1) (acc*((getNthPrime nth)^(hd - 1)))

--5
primeFactorization :: Int -> [Int]
primeFactorization num = factorization 1 num []
                        where factorization nth cur primes
                                | cur == 1 = primes
                                | (rem cur nthPrime) == 0 
                                    = factorization nth (div cur nthPrime) ([nthPrime] ++ primes)
                                | otherwise = factorization (nth + 1) cur primes
                                where nthPrime = getNthPrime nth

factorial 1 acc = acc
factorial num acc = factorial (num - 1) (acc * num)

sumDigits 0 acc = acc
sumDigits num acc = sumDigits (div num 10) (acc + (rem num 10))

--6
sumDigitsFac :: Int -> Int
sumDigitsFac num = (factorial num 1) `sumDigits` 0


changeB num a b | a + b > num = []
                | ((a^2) + (b^2)) == (c^2) = [(a, b, c)] ++ (changeB num a (b + 1))
                | otherwise = changeB num a (b + 1)
                where c = (num - a - b)
changeA num a | a > num = []
              | otherwise = (changeB num a 1) ++ (changeA num (a+1))
--7
getNpytha num = changeA num 1


--8
sumDigitsBin num = getSumBinDigits num 0
    where getSumBinDigits num' acc  | num' == 0 = acc
                                    | otherwise = getSumBinDigits (div num' 2) (acc + (rem num' 2))


countWords :: (Ord k, Num a) => [k] -> Map.Map k a -> Map.Map k a
countWords [] wdsMap = wdsMap
countWords (hd:tl) wdsMap = countWords tl (Map.insertWith (+) hd 1 wdsMap)


--9
readTextFile = do
    hPutStr stdout "Enter a Text File Path : "
    filePath <- hGetLine stdin
    handle <- openFile filePath ReadMode
    contents <- hGetContents handle     -- contents = char list
    --print contents
    let wds = words contents
    let wdsMap = countWords wds (Map.fromList [])
    --let sortedWds = sortOn snd (Map.toList wdsMap)
    let sortedWds = sortBy (\(_, a) (_, b) -> compare (-a) (-b)) (Map.toList wdsMap) 
    --print sortedWds
    print (take 10 sortedWds)
    hClose handle