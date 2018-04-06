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
>
> ivBoundDeviations :: (Array (Int, Int) Interval) -> [(Double,Double)]
> ivBoundDeviations mx = map (deviation (0,0)) $ zip (fstLine mx) (getColumns (innerArr mx))
>	                       where --deviation :: (Double,Double) -> (Interval, [Interval]) -> (Double, Double)
>                                deviation (ls, rs) (i,[])     = ((ls - (cen i))/(cen i), (rs - (cen i))/(cen i))
>                                deviation (ls, rs) (i,(x:xs)) = deviation (ls + (lb x), rs + (rb x)) (i,xs)
>                                                                
>
>
>
> getColumns :: (Array (Int, Int) a) -> [[a]]
> getColumns mx = [[mx!(i,j)| i <- [li..ui]]| j <- [lj..uj]]
>                  where ((li, lj),(ui,uj)) = bounds mx

