> module IPFVisualData where
>
> import Data.Array
>
> import IPFProc
>
> import Intervals.IntervalType
> import Intervals.IntervalOps
>
> {- takes a sample matrix (with marginals, which are then dropped) and outputs all pairs of 
>    average interval values with interval size to be displayed in a nice graphic -}
>
> ivLengthByNumberSize :: (Array (Int,Int) Interval) -> [(Double, Double)]
> ivLengthByNumberSize mx = [sizeAndLength $ mx'!(i,j) | (i,j) <- range $ bounds mx']
>                           where mx'          = innerArr mx
>                                 sizeAndLength i = (cen i, len i)
>
> {- Given an array (a_ij)_0_0^n^m ivBoundDeviations calculates a list [(dl_j,du_j)] of length m that sums up the
>  columnwise the entries (a_ij)_i=1..n then diminishes by m_j and divides by m_j. -}
> 
>
> ivBoundDeviations :: (Array (Int, Int) Interval) -> [(Double,Double)]
> ivBoundDeviations mx = map (deviation (0,0)) $ zip (fstLine mx) (getColumns (innerArr mx))
>	                       where --deviation :: (Double,Double) -> (Interval, [Interval]) -> (Double, Double)
>                                deviation (ls, rs) (i,[])     = ((ls - (cen i))/(cen i), (rs - (cen i))/(cen i))
>                                deviation (ls, rs) (i,(x:xs)) = deviation (ls + (lb x), rs + (rb x)) (i,xs)
> -- für unten stehendes machArrayIV sollte in der ersten Spalte berechnet werden:
> -- (102+10-45)/45 = 1.48888 und (55+6-132)/132 = -0.537878.. was auch rauskommt
>
> 
> 
> machArray3by3 :: (Array (Int,Int) Int)
> machArray3by3 = array ((1,1),(3,3)) [((1,1), 40),((1,2), 13),((1,3), 23),
>                                      ((2,1), 23),((2,2), 25),((2,3), 79),
>                                      ((3,1), 17),((3,2), 54),((3,3), 40)]
>
> machArrayIV :: (Array (Int,Int) Interval)
> machArrayIV = array ((0,0),(2,2)) [((0,0),(IV 177 177)),((0,1),(IV 45 45)),  ((0,2),(IV 132 132)),
>                                    ((1,0),(IV 120 120)),((1,1),(IV 102 102)),((1,2),(IV 55 55)),
>                                    ((2,0),(IV 57 57)),  ((2,1),(IV 10 10)),  ((2,2),(IV 6 6)) ]
> mach2ArrayIV :: (Array (Int,Int) Interval)
> mach2ArrayIV = array ((0,0),(4,1)) [((0,0),(IV 145 145)),((0,1),(IV 145 145)),
>                                    ((1,0),(IV 20 20)),((1,1),(IV 102 102)),
>                                    ((2,0),(IV 55 55)),((2,1),(IV 132 132)),
>                                    ((3,0),(IV 57 57)),((3,1),(IV 10 10)),
>                                    ((4,0),(IV 23 23)),((4,1),(IV 6 6)) ]
>

>
> getColumns :: (Array (Int, Int) a) -> [[a]]
> getColumns mx = [[mx!(i,j)| i <- [li..ui]]| j <- [lj..uj]]
>                  where ((li, lj),(ui,uj)) = bounds mx

