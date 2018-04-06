
> module Main where
>
> import Data.Array
> import System.IO
> -- import IPFInputOutput
> import Examples

> import IPFNum 
> import IPFProc

> import Intervals.IntervalType
> 
> 
> numIt = 10 -- number of iterations
> numTestMx = 10
> numTestMg = 10
> dimensions = [(2,2),(2,5),(2,10),(5,5),(5,10),(10,10)]
>

> -- combines inputs from all files with fitting dimensions
> testAll :: IO()
> testAll = foldl (>>) (return()) [readWriteIPF dim indMx indMg |
>                      dim   <- dimensions,
>                      indMx <- [1.. numTestMx],
>                      indMg <- [1.. numTestMg]
>                             ]

> dummytest :: IO()
> dummytest =  do s <- readFile "IPFTestInput/2by2/marginals/1.txt"
>                 writeFile "dummy.txt" s
>                 let x = readDoubleMarginals s
>                 putStrLn (show x)
>                 return()
>
> machArray :: (Array (Int,Int) Double)
> machArray = array ((1,1),(2,1)) [((1,1), 40),((2,1), 50)]
>
> dummytest2 :: IO()
> dummytest2 =  do s <- readFile "dummytest3.txt"
>                  writeFile "dummy2.txt" s
>                  let x = readDoubleMatrix s
>                  putStrLn (show x)
>                  return()
> 
> {- gets dimensions and a number for a matrix and a pair of marginals, reads the files from IPFTestInput/[n]by[m]/[matricies/marginals]/[i/j].txt,
>    performs IPF on them after converting the numbers to intervals. Then writes the result with all steps in a file to 
>    IPFTestResults/[n]by[m]/Matrix[i]Marginals[j].txt
>  -}
>
> readWriteIPF :: (Int,Int) -> Int -> Int -> IO()
> readWriteIPF (n,m) i j = do mx <- readFile $ "IPFTestInput/" ++ show(n) ++ "by" ++ show(m) ++ "/matricies/" ++
>                                                       show(i) ++ ".txt"
>                             mg <- readFile $ "IPFTestInput/" ++ show(n) ++ "by" ++ show(m) ++ "/marginals/" ++
>                                                       show(j) ++ ".txt"
>                             writeIPF path (readDoubleMatrixToInterval mx) (readDoubleMarginalsToInterval mg)
>                               where path = "IPFTestResults/" ++
>                                             show(n) ++ "by" ++ show(m) ++ "/Matrix" ++ show(i) ++
>                                            "Marginals" ++ show(j) ++ ".txt"
>
>
> readMarginals :: IPFNum a => String -> ([a],[a])
> readMarginals s = (read x, read (tail y))
>                   where (x,y) = break (=='%') s
>
> readDoubleMarginals :: String -> ([Double],[Double])
> readDoubleMarginals = readMarginals
>
> readDoubleMarginalsToInterval :: String -> ([Interval],[Interval])
> readDoubleMarginalsToInterval s = (map double2Interval mcol, map double2Interval mrow)
>                                     where (mcol, mrow) = readDoubleMarginals s
>

> readMatrix :: (Read a, Signed a, Eq a, Num a, Show a, Fractional a) => String -> (Array (Int,Int) a)
> readMatrix = twoDimListToArray.read
>
> readDoubleMatrix :: String -> (Array (Int,Int) Double)
> readDoubleMatrix = readMatrix
>
> readDoubleMatrixToInterval :: String -> (Array (Int, Int) Interval)
> readDoubleMatrixToInterval = (fmap double2Interval).readMatrix
>


> twoDimListToArray :: [[a]] -> (Array (Int,Int) a)
> twoDimListToArray l = array (bounds ) (zip (range bounds) (concat l))
>                               where  bounds = ((1,1),(length l,length (head l)))
>                           
>
> {- gets a string describing the input data,
>    an array and two lists-the marginals, runs IPF and saves the result
>    conditions: sum list1 = sum list2, arraydim1 =length list 1, arraydim2 = length list 2
>    all entries are positive, errors will be reported in output
>  -}
>
>
> writeIPF :: IPFNum a => FilePath -> (Array (Int,Int) a) -> ([a],[a]) -> IO ()
> writeIPF path mx (mcol, mrow)
>              | (uj - lj + 1) /= length mcol || (ui - li + 1) /= length mrow =
>                                  writeFile path "Marginal lengths do not match array bounds."
>              | sumsDontMatch (simpleSum mcol) (simpleSum mrow) =
>                                  writeFile path "Illegal marginal entries."            
>              | not (isAllPositive (fuseArr mx mcol mrow))  =
>                                  writeFile path "Some marginal or matrix entry is not positive."
>              | otherwise =
>                                  writeFile path (ipfString numIt (fuseArr mx mcol mrow))
>               where ((li,lj),(ui,uj)) = bounds mx
>				    
>                     simpleSum l = foldl maybeplus (Just 0) (map toDouble l)
> 
>                     maybeplus Nothing  _        = Nothing
>                     maybeplus (Just a) Nothing  = Nothing 
>                     maybeplus (Just a) (Just b) = Just (a + b)						
> 
>                     sumsDontMatch Nothing  x        = True
>                     sumsDontMatch (Just x) Nothing  = True
>                     sumsDontMatch (Just a) (Just b) = a /= b  
>                       
> 
>                      
>
> isAllPositive :: (Signed a, Num a) => (Array (Int,Int) a) -> Bool
> isAllPositive  mx = foldl (&&) True (map isPositive entries)
>                      where entries = [mx!(a,b) | a <- range (li, ui), b <- range (lj, uj)]
>                            ((li,lj),(ui,uj)) = bounds mx
>         
>
> {- gets an array and two marginals and (if inputs are valid) runs ipfHistory to -}
>                  
>
> saveIPF :: IPFNum a => (Array (Int,Int) a) -> ([a],[a]) -> Maybe [(Array (Int,Int) a)]
> saveIPF mx (mcol, mrow)
>              | (uj - lj + 1) /= length mcol || (ui - li + 1) /= length mrow = Nothing
>              | sumsDontMatch (simpleSum mcol) (simpleSum mrow) = Nothing           
>              | not (isAllPositive (fuseArr mx mcol mrow))  = Nothing 
>              | otherwise = Just (ipfHistory numIt (fuseArr mx mcol mrow))
>               where ((li,lj),(ui,uj)) = bounds mx
>				    
>                     simpleSum l = foldl maybeplus (Just 0) (map toDouble l)
> 
>                     maybeplus Nothing  _        = Nothing
>                     maybeplus (Just a) Nothing  = Nothing 
>                     maybeplus (Just a) (Just b) = Just (a + b)						
> 
>                     sumsDontMatch Nothing  x        = True
>                     sumsDontMatch (Just x) Nothing  = True
>                     sumsDontMatch (Just a) (Just b) = a /= b  
>
>
>
>
>
 
