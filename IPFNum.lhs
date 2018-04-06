> module IPFNum where
>
> import GHC.Float

> import Intervals.IntervalType
> import Intervals.IntervalArithmetic
> import Intervals.IntervalProp
>
>
> class (Show a, Num a, Read a, Signed a) => IPFNum a where
>  
> {- WARNING: this function assumes that (in the case of intervals) the second input is purely positive -}
>
> 		divide :: a -> a -> a
> 		toDouble :: a -> Maybe Double
> 
> instance IPFNum Double where
>              divide = (/)
>              toDouble f = Just f 
>               
>
> instance IPFNum Float where
>              divide = (/) 
>              toDouble f = Just (float2Double f)
>
>               
> 
> instance IPFNum Interval where
> 	      divide = divPlus
> 	      toDouble i | (lb i) == (rb i) = Just (lb i)
>                    | otherwise = Nothing
> 
>
> class Signed a where
>  isPositive :: a -> Bool
>  isNegative :: a -> Bool
>  isUnsigned :: a -> Bool
>  isUnsigned x = not $ isPositive x ||  isNegative x
> 
> instance Signed Int where
>         isPositive i = i > 0
>         isNegative i = i < 0
>
> instance Signed Double where
>         isPositive i = i > 0
>         isNegative i = i < 0
>
> instance Signed Float where
>         isPositive i = i > 0
>         isNegative i = i < 0 
>
> instance Signed Interval where
>         isPositive = isIntP
>         isNegative = isIntN
> 