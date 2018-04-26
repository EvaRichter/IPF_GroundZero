> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE GADTs #-}
> module MultiDimIPF where
> -- this is iterative proportional fitting for more than just two dimensions (any dimension, as long as you manage to get 
> -- an element of TypeNat for that number
>
> import Data.Array
> import IPFNum
> 
>
> -- First, a few Simple types to act like we can do dependent typing
> -- good old Nat
> data Nat = Z | S Nat
>
> -- natural numbers up to n (with DataKinds)
> data Fin :: Nat -> * where
>            F0 :: Fin (S n)
>            FS :: Fin n -> Fin (S n)
> 
> -- just a type that depends on a natural but has only one element per number, this is to wrap the number of dimensions
> -- into a type, so that we can handle different numbers in different ways, using pattern matching
> data TypeNat :: Nat -> * where
>                  T0 :: TypeNat Z
>                  TS :: TypeNat n -> TypeNat (S n)
>
> -- unfortunately we need some kind of interface to actually get a type from a number, but since that is 
> -- fundamentally impossible in haskell to do for a general number, here are just some we might use:
> typeZero = T0
> typeOne = TS typeZero
> typeTwo = TS typeOne
> typeThree = TS typeTwo
> typeFour = TS typeThree
> typeFive = TS typeFour
> typeSix = TS typeFive
> typeSeven = TS typeSix
> typeEight = TS typeSeven
> typeNine = TS typeEight
> typeTen = TS typeNine
>
> -- a little Function to create a list of all elements in (F n) uses n disguised as a type in TypeNat n
> allFin :: (TypeNat n) -> [Fin n]
> allFin T0     = []
> allFin (TS i) = F0 : (map FS (allFin i))
> 
> -- a Vector with fixed length n:
> data Vect :: Nat -> * -> * where
>            Nil  :: Vect Z a
>            (:#) :: a -> Vect n a -> Vect (S n) a
>
> -- instances for Eq, Ord and Ix to use these Vectors as array indicies, all implemented the same way they are for tuples in haskell
> instance (Eq a) => Eq (Vect n a) where
>             Nil    == Nil    = True
>             (x:#u) == (y:#v) = (x==y) && (u == v) 
>
> instance (Ord a) => Ord (Vect n a) where
>              Nil    <= Nil    = True
>              (x:#u) <= (y:#v) | x > y     = False
>                               | x < y     = True
>                               | otherwise = (u <= v)
>
> instance (Ix a) => Ix (Vect n a) where
>                     range (Nil, Nil)      = [Nil]
>                     range ((x:#u),(y:#v)) = concat [ map (z:#) (range (u,v))| z <- range (x,y)]
>                     
>                     inRange (Nil, Nil) Nil          = True
>                     inRange ((x:#u), (y:#v)) (z:#w) = (inRange (x,y) z) && (inRange (u,v) w)
>
>                     index (Nil, Nil) Nil          = 0
>                     index ((x:#u), (y:#v)) (z:#w) = (index (x,y) z)*(rangeSize (u,v)) + (index (u,v) w)
>
>                     rangeSize (Nil, Nil)      = 1
>                     rangeSize ((x:#u),(y:#v)) = (rangeSize (x,y)) * (rangeSize (u,v))
> 
>
> -- some very handy operations on these Vectors:
> 
> -- get the k-th entry (k given by an onject of Fin n, so it will always be a valid coordinate!)
> vGet :: (Fin n) -> (Vect n a) -> a
> vGet F0     (x:#_) = x
> vGet (FS i) (_:#u) = vGet i u 
> 
> -- set the k-th entry to a different value 
> vSet :: (Fin n) -> a -> (Vect n a) -> (Vect n a)
> vSet F0     y (_:#u) = (y:#u)
> vSet (FS i) y (x:#u) = (x:#(vSet i y u))  
> 
> 
> -- drop the k-th entry of the vector so you are left with a vector of smaller size
> vDrop :: (Fin (S n)) -> (Vect (S n) a) -> (Vect n a)
> vDrop F0 (_:#u)            = u    
> vDrop (FS i) (x:#u@(_:#_)) = (x:#(vDrop i u))
>
> -- insert a new value into the vector to create a vector of bigger size.
> vInsert :: (Fin (S n)) -> a -> (Vect n a) -> (Vect (S n) a)
> vInsert F0 y Nil        = (y:#Nil)
> vInsert (FS i) y (x:#u) = (x:#(vInsert i y u)) 
>
> -- nice Properties: for all k in (Fin n), l in (Fin (S n)), x,y in a, u in Vect n a, v in Vect (S n) a 
> -- vGet k (vSet k x u)      = x
> -- vSet k y (vSet k x u)    = vSet k y u
> -- vDrop l (vSet l x v)     = vDrop k u
> -- vGet l (vInsert l x u)   = x 
> -- vSet l y (vInsert l x u) = vInsert l y u
> -- vDrop l (vInsert l x u)  = u
>
>
> -- a usefule function to work with bounds: 
> pairmap :: (a -> b) -> (a,a) -> (b,b)
> pairmap f (x,y) = (f x, f y) 
> 
> -- analogue to adaptrow and adaptcol functions in twoDim IPF: insteadof summing over 1-dimensional 
> -- subpsaces we sum over n-1 dimensional subspcaes (Hypersurfaces) and adapt entries to the marginal value 
> adaptSurf :: IPFNum a => ((Array (Vect n Int) a),a) -> (Array (Vect n Int) a)
> adaptSurf (mx, mrg) = fmap (\x -> divide (x*mrg) (sum (elems mx))) mx
>
>
> -- split the big array into its hypersyrfaces along the k-th dimension, to be adapted with adaptSurf
> splitAlongDim :: (Fin (S n)) -> (Array (Vect (S n) Int) a) -> [Array (Vect n Int) a] 
> splitAlongDim d mx = [listArray newBounds [mx!u | u <- range $ pairmap (vSet d i) (bounds mx)] | i <- range dimBounds]
>                                      where dimBounds = pairmap (vGet d) (bounds mx)  -- bounds for selected Dimension 
>                                            newBounds = pairmap (vDrop d) (bounds mx) -- bounds for the hypersurface 
> 
> -- reconstruct the big array from its hypersurfaces along the k-th dimension. Since the old bounds in that direction are lost,
> -- the new ones will just start from 0 and go to however many surfaces there are (minus 1 since westarted at 0)
> joinAlongDim :: (Fin (S n)) -> [Array (Vect n Int) a] -> (Array (Vect (S n) Int) a)
> joinAlongDim d mxs = array fullBounds $ concat [map (addIndex i) (assocs (mxs!!i)) | i <- [0..(length mxs - 1)]]
>                                                where fullBounds       = (vInsert d 0 lb, vInsert d (length mxs - 1) ub)
>                                                      (lb,ub)          = bounds (mxs!!0)
>                                                      addIndex i (v, x)= (vInsert d i v, x) 
>
> -- to tidy up the code, here a tpye synonymforwhat we areusuall yworking with: a big matrix together with a list of marginals for every dimension
> type IPFData n a = ((Array (Vect n Int) a),(Vect n [a]))
>
>
> -- adapts the Dataset in one dimension, basically just doing a split followed by an adaptSurf all the slices and then a join
> -- also makes sure all themarginals are where they should be
> adaptIn1Dim :: IPFNum a => (Fin (S n)) -> (IPFData (S n) a) -> (IPFData (S n) a)
> adaptIn1Dim d (mx, mrgs) = ((joinAlongDim d).(map adaptSurf) $ (zip (splitAlongDim d mx) (vGet d mrgs)),mrgs)
> 
> -- adapts in every possible direction. This procedure is definitely dependent on the number of dimensions though,
> -- so we need TypeNat at this point, to know what all thedifferent dimensions are (given by allFin)
> oneIPF :: IPFNum a => (TypeNat n) -> (IPFData n a) -> (IPFData n a)
> oneIPF T0       dataset = dataset
> oneIPF t@(TS _) dataset = foldr adaptIn1Dim dataset (allFin t)
>
> -- multiple iterations or useful interface are still to do




