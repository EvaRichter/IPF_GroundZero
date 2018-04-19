> {-# LANGUAGE OverloadedStrings #-}
> --the string "foo" is interpreted as fromString "foo", which is a method of every type of class IsString

> module IPFGraphResults where
>
> 


> -- import Text.LaTeX.Base.Syntax
> -- import Text.LaTeX.Base.Render
> -- import Data.Text (unpack,lines)
> -- import Data.Monoid (mconcat,mempty)



> {- 

> --import Main2018_04_09
> import Data.Array
> import Control.Monad
> import System.Environment
> import Numeric.IEEE
> --import System.IO
> import IPFInputOutput
> import Examples
> import GHC.Float
> import Data.List


> import Intervals.IntervalType
> import Intervals.IntervalArithmetic
> import Intervals.IntervalOps
> import Intervals.IntervalProp
> -}
>
> import Data.List
>
> import Text.LaTeX
> import Text.LaTeX.Packages.Inputenc
> import Text.LaTeX.Packages.TikZ 
> import Text.LaTeX.Base.Commands
> import Text.LaTeX.Base.Writer



> buildGraphicOne :: [[(Double,Double)]] -> IO()
> buildGraphicOne list = (execLaTeXT
>                      (thePreamble >> document (graphicTypeOne list) ))
>                      >>= renderFile "graphicIPF1.tex"
>
> buildGraphicTwo :: [[(Double,Double)]] -> IO()
> buildGraphicTwo list = (execLaTeXT
>                      (thePreamble >> document (graphicTypeTwo list) ))
>                      >>= renderFile "graphicIPF2.tex"
> 

> errorTexFile :: IO ()
> errorTexFile = (execLaTeXT (thePreamble >> document (maketitle >> "There was nothing to draw") ))
>                      >>= renderFile "graphicIPF2.tex"

> thePreamble :: LaTeXT IO ()
> thePreamble = do
>    {documentclass [] article;
>     usepackage [utf8] inputenc;
>     usepackage [] tikz;
>     author "Eva Richter";
>     title "Graphik 1 von IPF"
>    }


> list1 :: [(Double,Double)]
> list1 = [(2,3),(3,7),(4,9)]
>
> {- scaling of results works with lenScale and higScale for width and centre of intervals -}
> 
>
> lenScale = 500000
>
> higScale :: Double
> higScale = 0.005
>
> minScaling :: Double -> Double
> minScaling x = max 0.1 x*lenScale
>
> logScaling :: Double -> Double
> logScaling x = max 0.05 (log(x*1000000000000)/30)
>
> doubleLogScaling :: Double -> Double
> doubleLogScaling x = log(log(x+1)+1)*lenScale
> 
> graphicTypeOneSingleIt :: Int -> [(Double,Double)] -> (Double -> Double) -> TikZ
> graphicTypeOneSingleIt itNum itData scaling =
>       scope [TColor $ BasicColor Blue, TWidth (Pt 1)] $ 
>       foldl (->>)(scope [TColor $ BasicColor Black] $ draw (Start (pointAtXY x 0) ->- pointAtXY x 10)) -- y-axis
>            $ fmap draw  [Start (pointAtXY (x - (scaling len)) (hig * higScale))
>                             ->- pointAtXY (x + (scaling len)) (hig * higScale) | (hig,len) <- itData]
>                     where x = fromIntegral itNum
>
>
> 
> graphicTypeOne :: [[(Double,Double)]] -> LaTeXT IO ()
> graphicTypeOne list = do
>   maketitle
>   "The graphic below depicts the length ofintervels depending on size of the average value classified by number of iterations (length is on a logarithmic scale!)."
>   newline  
>   center $ tikzpicture $  
>     foldl (->>) frame [graphicTypeOneSingleIt itNum itData logScaling | (itNum, itData) <- zip [1..] list]
>
> {- das ist das Original NICHT Löschen!!
> graphicTypeTwo :: [[(Double, Double)]] -> LaTeXT IO() 
> graphicTypeTwo list = do
>  maketitle
>  "insert description"
>  newline
>  center $ tikzpicture $
>    foldl (->>) frame [graphicTypeTwoSingleRow rowColour (unzip rowData) | (rowColour, rowData) <- zip allcolours (transpose list)]
> -}

> graphicTypeTwo :: [[(Double, Double)]] -> LaTeXT IO() 
> graphicTypeTwo list = do
>  maketitle
>  "neuer Versuch"
>  newline
>  center $ tikzpicture $
>    foldl (->>) frame [graphicTypeTwoSingleRow rowColour (unzip rowData) | (rowColour, rowData) <- zip allcolours (transpose list)]
>

> graphicTypeTwoSingleRow :: TikZColor -> ([Double], [Double]) -> TikZ
> graphicTypeTwoSingleRow color ((u:uppers), (l:lowers)) = scope [TColor color, TWidth (Pt 1)]
>                      (draw (bpath (pointAtXY 0 u) (mapM_ (line.mkPoint) (zip [1..] uppers)))
>                       ->> draw (bpath  (pointAtXY 0 l) (mapM_ (line.mkPoint) (zip [1..] lowers))))
>
> mkPoint :: (Int, Double) -> TPoint
> mkPoint (a,b) = pointAtXY (fromIntegral a) $ if (abs (b*20000)) > 10 then 10*signum(b) else (b*20000) 
> 
> orange :: TikZColor
> orange = RGBColor 255 128 0  
>
> gray :: TikZColor
> gray = RGBColor 102 51 0
>
> violet :: TikZColor
> violet = RGBColor 102 0 102
> 
> brown :: TikZColor
> brown = RGBColor 128 128 128 
> 
> allcolours :: [TikZColor]
> allcolours = [BasicColor Red, BasicColor Green, BasicColor Blue, BasicColor Yellow,
>               BasicColor Magenta, BasicColor Cyan, orange, gray, violet, brown]
>
> frame :: TikZ
> frame = draw (Rectangle (Start $ pointAtXY 0 0) (pointAtXY 12 11))
> 
> theOldBody :: LaTeXT IO ()
> theOldBody = do
>  maketitle
>  "The picture below was generated using the TikZ DSL of "
>  hatex
>  "."
>  center $ tikzpicture $ draw $
>   Cycle $ Start (pointAtXY 0 0) ->- pointAtXY 1 0 ->- pointAtXY 0 1
>  "And some more pictures."
>  center $ tikzpicture $
>      draw  (Rectangle (Start $ pointAtXY 0   0  ) (pointAtXY 1 1))
>       ->> fill  (Circle    (Start $ pointAtXY 1.5 0.5)  0.5)
>       ->> shade (Ellipse   (Start $ pointAtXY 3   0.5 ) 1 0.5)
>  center $ tikzpicture $ 
>       draw (Start (pointAtXY   0    1) ->- pointAtXY  0     (-1))
>       ->> draw (Start (pointAtXY (-0.2) 0) ->- pointAtXY (3*pi)   0 )
>       ->> scope [TColor $ BasicColor Blue, TWidth (Pt 1)] (draw $ bpath (pointAtXY 0 0) $
>        mapM_ line [ pointAtXY x (sin x) | x <- [0,0.05 .. 3*pi] ]
>                                                            )
>  "Nochmal Text"
>  center $ tikzpicture $ draw $
>   Cycle $ Start (pointAtXY 1 1) ->- pointAtXY 2 1 ->- pointAtXY 1 2
>  "And some more other pictures."

>
> theFirstEx :: LaTeXT IO ()
> theFirstEx = do
>  center $ tikzpicture $ 
>       draw (Start (pointAtXY  (-1) (-1)) ->- pointAtXY  2 1)
>       ->> draw (Start (pointAtXY (-0.2) 0) ->- pointAtXY (3*pi)   0 )
>       ->> scope [TColor $ BasicColor Red, TWidth (Pt 1)] (draw $ bpath (pointAtXY 0 0) $
>        mapM_ line [ pointAtXY x (2*x) | x <- [0,0.05 .. 3*pi] ]
>                                                            )
> 
> {-                                                          
>  center $ tikzpicture $ draw $
>      Cycle $ Start (pointAtXY 0 0) ->- pointAtXY 1 0 ->- pointAtXY 0 1
> -- ->- pointAtXY 1 1
>      "We also show the graph of the "
>       emph "sine"
>      " function."
> -}



