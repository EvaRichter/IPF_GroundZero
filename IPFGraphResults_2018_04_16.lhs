> {-# LANGUAGE OverloadedStrings #-}
> --the string "foo" is interpreted as fromString "foo", which is a method of every type of class IsString

> module IPFGraphResults_2018_04_16 where
>
> import Data.List
>
> import Text.LaTeX
> import Text.LaTeX.Packages.Inputenc
> import Text.LaTeX.Packages.TikZ 
> import Text.LaTeX.Base.Commands
> import Text.LaTeX.Base.Writer


> {- We produce three types of graphics, i.e. three different Latexfiles-} 

> buildGraphicOne :: [[(Double,Double)]] -> IO()
> buildGraphicOne list = (execLaTeXT
>                      (thePreambleOne >> document (graphicTypeOne list) ))
>                      >>= renderFile "graphicIPF1.tex"
>
> -- production of preamble with title and remarks
> 
> thePreambleOne :: LaTeXT IO ()
> thePreambleOne = do
>    {documentclass [] article;
>     usepackage [utf8] inputenc;
>     usepackage [] tikz;
>     author "Eva Richter";
>     title "Graphik 1 von IPF"
>    }
> -- the second part of the document is an error message if the saveIPF does not yield a correct array
> 
> errorTexFileOne :: IO ()
> errorTexFileOne = (execLaTeXT (thePreambleOne >> document (maketitle >> "There was nothing to draw") ))
>                      >>= renderFile "graphicIPF1.tex"


> {- for every iteration a graphic is build that shows the width of all interval entries in the array -}
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
> {-the graphics (tizkz)of all iterations are appended to one tikz-}
> 
> graphicTypeOne :: [[(Double,Double)]] -> LaTeXT IO ()
> graphicTypeOne list = do
>   maketitle
>   "The graphic below depicts the length of intervels depending on size of the average"
>   "value classified by number of iterations (length is on a logarithmic scale!)."
>   newline  
>   center $ tikzpicture $  
>     foldl (->>) frame [graphicTypeOneSingleIt itNum itData logScaling | (itNum, itData) <- zip [1..] list]
>
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
> list1 :: [(Double,Double)]
> list1 = [(2,3),(3,7),(4,9)]
>
> {-*****************************Graphic Type 2*************************************************-}
> 
>
> buildGraphicTwo :: [[(Double,Double)]] -> IO()
> buildGraphicTwo list = (execLaTeXT
>                      (thePreambleTwo >> document (graphicTypeTwo list) ))
>                      >>= renderFile "graphicIPF2.tex"
> 
> 
> -- production of preamble with title and remarks
> 
> thePreambleTwo :: LaTeXT IO ()
> thePreambleTwo = do
>    {documentclass [] article;
>     usepackage [utf8] inputenc;
>     usepackage [] tikz;
>     author "Eva Richter";
>     title "Graphik 2 von IPF"
>    }
> {--
> the second part of the document contains only an error message if the input for the local function
> drawGraphs in readDrawIPFTypeTwo from main-module  is Nothing, i.e. saveIPF does not yield a correct array
> -}
> 
> errorTexFileTwo :: IO ()
> errorTexFileTwo = (execLaTeXT (thePreambleTwo >> document (maketitle >> "There was nothing to draw") ))
>                      >>= renderFile "graphicIPF2.tex"
>
> graphicTypeTwo :: [[(Double, Double)]] -> LaTeXT IO() 
> graphicTypeTwo list = do
>  maketitle
>  "The graphic below shows the changes in relative error of the column sum. Each column is represented by one color, the x axis shows the number of iterations.The y-axis has a logarithmic scale"
>  newline
>  center $ tikzpicture $
>    foldl (->>) yLogAxis [graphicTypeTwoSingleRow rowColour (unzip rowData)
>                        | (rowColour, rowData) <- zip allcolours (transpose list)]
>

> graphicTypeTwoSingleRow :: TikZColor -> ([Double], [Double]) -> TikZ
> graphicTypeTwoSingleRow color ((u:uppers), (l:lowers)) = scope [TColor color, TWidth (Pt 1)]
>                      (draw (bpath (pointAtXY 0 u) (mapM_ (line.mkPointLog) (zip [1..] uppers)))
>                       ->> draw (bpath  (pointAtXY 0 l) (mapM_ (line.mkPointLog) (zip [1..] lowers))))
>
> mkPoint :: (Int, Double) -> TPoint
> mkPoint (a,b) = pointAtXY (fromIntegral a) $ if (abs (b*20000)) > 10 then 10*signum(b) else (b*20000)
>
> mkPointLog :: (Int, Double) -> TPoint
> mkPointLog (a,b) = pointAtXY (fromIntegral a) y where
>                      y = signum b * 20 * (1/ (max (-(log (abs b))) 1))
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
> yLogAxis :: TikZ
> yLogAxis =  foldl (->>)  (draw (Start (pointAtXY 0 (-8)) ->- pointAtXY 0 8 ))
>                    [draw (Start (pointAtXY (-0.05) y) ->- pointAtXY  0.05 y )| y <- [(-7),(-6).. 7]]
>             ->> (foldl (->>)  (draw (Start (pointAtXY 0 0) ->- pointAtXY 10 0 ))
>                  [draw (Start (pointAtXY x (-0.05) ) ->- pointAtXY x  0.05 )| x <- [1,2..8]])
>
> 
>
> 
>



