
> module Main where
>
> import Data.Array
> import System.IO
> -- import IPFInputOutput
> import Examples
> 
> import IPFGraphResultsTypeOne
> import IPFGraphResultsTypeTwo
> import IPFVisualData
> import IPFNum 
> import IPFProc

> import Intervals.IntervalType
>
> 
> 
> numIt = 10 -- number of iterations, in the end here should be a 10
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
>
> array2by1 :: (Array (Int,Int) Double)
> array2by1 = array ((1,1),(2,1)) [((1,1), 40),((2,1), 50)]
>
> mx3by3 ::(Array (Int,Int) Interval)
> mx3by3 = array ((1,1),(3,3)) [((1,1), (IV 40 40)),((1,2), (IV 50 50)),((1,3), (IV 13 13)),
>                               ((2,1), (IV 10 10)),((2,2), (IV 40 40)),((2,3), (IV 60 60)),
>                               ((3,1), (IV 40 40)),((3,2), (IV 6 6)),((3,3), (IV 24 24))]
>
> {-result of running 3 iterations of IPF on machArray3by3-}
> 
> ipf3result3by3ArrayIV :: Maybe [Array (Int,Int) Interval]
> ipf3result3by3ArrayIV =
>    Just [array ((0,0),(2,2))
>      [((0,0),(IV 176.99999999999997 177.00000000000003)),((0,1),(IV 45.0 45.0)),  ((0,2),(IV 132.0 132.0)),
>       ((1,0),(IV 120.0 120.0)),
>                       ((1,1),(IV 102.0 102.0)),((1,2),(IV 55.0 55.0)),
>       ((2,0),(IV 57.0 57.0)),
>                       ((2,1),(IV 10.0 10.0)),  ((2,2),(IV 6.0 6.0))],
>          array ((0,0),(2,2))
>      [((0,0),(IV 176.99999999999997 177.00000000000003)),((0,1),(IV 45.0 45.0)),((0,2),(IV 132.0 132.0)),
>       ((1,0),(IV 120.0 120.0)),
>            ((1,1),(IV 30.73688832779857 30.736888327798763)),((1,2),(IV 89.26311167220106 89.2631116722016)),
>       ((2,0),(IV 57.0 57.0)),
>            ((2,1),(IV 13.470478847047836 13.470478847047927)),((2,2),(IV 43.52952115295198 43.52952115295225))],
>          array ((0,0),(2,2))
>      [((0,0),(IV 176.99999999999997 177.00000000000003)),((0,1),(IV 45.0 45.0)),((0,2),(IV 132.0 132.0)),
>       ((1,0),(IV 120.0 120.0)),
>            ((1,1),(IV 31.283225980832736 31.283225980833695)),((1,2),(IV 88.71677401916544 88.71677401916811)),
>       ((2,0),(IV 57.0 57.0)),
>           ((2,1),(IV 13.716406985011602 13.716406985012034)),((2,2),(IV 43.283593014987524 43.283593014988845))]
>          ]
> 
> col1it1l :: Double
> col1it1l = (30.73688832779857 + 13.470478847047836 - 45)/45
>
> col1it2l :: Double
> col1it2l = (31.283225980832736 + 13.716406985011602 -45)/45
>
> col1it1u :: Double
> col1it1u = (30.736888327798763 + 13.470478847047927- 45)/45
>
> col1it2u :: Double
> col1it2u = ( 31.283225980833695 + 13.716406985012034- 45)/45
> 
> mg3by3 :: ([Double],[Double])
> mg3by3 = ([140,230,31],[77,120,204])
> 
>
>
> {- drawIPFTypeOne draws the results of the IPF procedure for one input combination
> (dimension, marginalnumber, matrixnumber). -}
>
> readDrawIPFTypeOne :: (Int,Int) -> Int -> Int -> IO()
> readDrawIPFTypeOne (n,m) i j = do mx <- getMatrix (n,m) i
>                                   mg <- getMarginals (n,m) j 
>                                   drawGraphs $ saveIPF mx mg
>                                      where drawGraphs :: Maybe [(Array (Int,Int) Interval)] -> IO()
>                                            drawGraphs Nothing = errorTexFileOne
>                                            drawGraphs (Just a)  = buildGraphicOne $ map ivLengthByNumberSize a
>
>
> readDrawIPFTypeTwo :: (Int,Int) -> Int -> Int -> IO()
> readDrawIPFTypeTwo (n,m) i j = do mx <- getMatrix (n,m) i
>                                   mg <- getMarginals (n,m) j 
>                                   drawGraphs $ saveIPF mx mg
>                                      where drawGraphs :: Maybe [(Array (Int,Int) Interval)] -> IO()
>                                            drawGraphs Nothing = errorTexFileTwo
>                                            drawGraphs (Just a)  = buildGraphicTwo $ map ivBoundDeviations a
>
>
> drawGraphsDirect :: Maybe [(Array (Int,Int) Interval)] -> IO()
> drawGraphsDirect Nothing = errorTexFileTwo
> drawGraphsDirect (Just a)  = buildGraphicTwo $ map ivBoundDeviations a
> 
> {- Mattis Test
> readTestIPFTypeTwo ::(Int,Int) -> Int -> Int -> IO()
> readTestIPFTypeTwo (n,m) i j = do mx <- getMatrix (n,m) i
>                                   mg <- getMarginals (n,m) j 
>                                    printDevis $ saveIPF mx mg
>                                        where printDevis :: Maybe [(Array (Int,Int) Interval)] -> IO()
>                                              printDevis Nothing  = putStrLn "Error"
>                                              printDevis (Just a) = putStrLn $
>                                                                  show (map ivBoundDeviations (tail a))
> -}
> {-probiere ohne file einlesen mit direkter Eingabe
> -}
> 
> muttitest :: (Array (Int,Int) Interval) -> IO()
> muttitest arr  =  drawGraphs $ saveIPF (innerArr arr) ((fstLine arr), (fstCol arr))
>                       where drawGraphs :: Maybe [(Array (Int,Int) Interval)] -> IO()
>                             drawGraphs Nothing = errorTexFileTwo
>                             drawGraphs (Just a)  = buildGraphicTwo $ map ivBoundDeviations a
> 
>
> 
> {- gets dimensions and a number for a matrix and a pair of marginals, reads the files from
> IPFTestInput/[n]by[m]/[matricies/marginals]/[i/j].txt,
>    performs IPF on them after converting the numbers to intervals. Then writes the result
> with all steps in a file to 
>    IPFTestResults/[n]by[m]/Matrix[i]Marginals[j].txt
>  -}
>
> readWriteIPF :: (Int,Int) -> Int -> Int -> IO()
> readWriteIPF (n,m) i j = do mx <- getMatrix (n,m) i
>                             mg <- getMarginals (n,m) j
>                             writeIPF path mx mg
>                               where path = "IPFTestResults/" ++
>                                             show(n) ++ "by" ++ show(m) ++ "/Matrix" ++ show(i) ++
>                                            "Marginals" ++ show(j) ++ ".txt"
>
> {-getMarginals takes the dimensions and a number of a fitting marginal file and produces a
> two list of  marginals(Doubles interpreted as intervals -}
>
> getMarginals :: (Int,Int) ->  Int -> IO (([Interval],[Interval]))
> getMarginals (n,m) j = do strData <- readFile path 
>                           return(readDoubleMarginalsToInterval strData)
>                               where path = "IPFTestInput/" ++ show(n) ++ "by" ++ show(m) ++
>                                            "/marginals/" ++ show(j) ++ ".txt"
>
> {-getMatrix takes the dimensions and a number of a fitting marginal file and produces a
> a matrix of Intervals -}
>
> getMatrix :: (Int,Int) ->  Int -> IO ((Array (Int, Int) Interval))
> getMatrix (n,m) i = do strData <- readFile path
>                        return(readDoubleMatrixToInterval strData)
>                               where path = "IPFTestInput/" ++ show(n) ++ "by" ++ show(m) ++
>                                            "/matricies/" ++ show(i) ++ ".txt"
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
 
