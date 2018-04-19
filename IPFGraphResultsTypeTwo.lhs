> module IPFGraphResultsTypeTwo where
>
> import Data.List
>
> import Text.LaTeX
> import Text.LaTeX.Packages.Inputenc
> import Text.LaTeX.Packages.TikZ 
> import Text.LaTeX.Base.Commands
> import Text.LaTeX.Base.Writer
> import Text.LaTeX.Base.Render


> {- The functions in this module produce a LaTeX File that will compile to a document(article)
> consisting of an introduction and a tikzPicture. The graphic shows the width of the intervals
> ,i.e. the entries in an IPF matrix depending on the number of iterations.
> The interface to the outside is the function buildGraphicOne.
> -}
>
> 
>
> buildGraphicTwo :: [[(Double,Double)]] -> IO()
> buildGraphicTwo list = (execLaTeXT
>                      (thePreambleTwo >> document (graphicTypeTwo list) ))
>                      >>= renderFile "graphicIPF2.tex"
> 
> --constants--
>
> -- mkPoint :: (Int, Double) -> TPoint
> -- mkPoint (a,b) = pointAtXY (fromIntegral a) $ if (abs (b*20000)) > 10 then 10*signum(b) else (b*20000)
>
> mkPointLog :: (Int, Double) -> TPoint
> mkPointLog (a,b) = pointAtXY (fromIntegral a) y where
>                      y = signum b * 20 * (1/ (max (-(log (abs b))) 1))
>
> calcax :: Double -> Double
> calcax x = -(abs x)/ (log 10)
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
> -- frame :: TikZ
> -- frame = draw (Rectangle (Start $ pointAtXY 0 0) (pointAtXY 12 11))
>
> {-draw x and y axis, x shows number of iteration, y has a logarithmic scale-}
>
> yLogAxis :: TikZ
> yLogAxis =  axdescrip 
>             ->>  foldl (->>)  (draw (Start (pointAtXY 0 (-8)) ->- pointAtXY 0 8 ))
>                    [draw (Start (pointAtXY (-0.05) y) ->- pointAtXY  0.05 y )|
>                                                                                 y <- [(-7),(-6).. 7]]
>             ->> (foldl (->>)  (draw (Start (pointAtXY 0 0) ->- pointAtXY 10 0 ))
>                  [draw (Start (pointAtXY x (-0.05) ) ->- pointAtXY x  0.05 )| x <- [1,2..8]])
>
> axdescrip :: TikZ
> axdescrip = foldl (->>)  (draw (Start (pointAtXY 0 (-8)) ->- pointAtXY 0 8 ))
>                          [draw (Node (Start (pointAtXY (-0.9) y))
>                                 ( rendertex (calcax y)) )| y <- [(-7),(-6).. 7]]
>             
>
> -- production of preamble with title and remarks
> 
> thePreambleTwo :: LaTeXT IO ()
> thePreambleTwo = do
>    {documentclass [] article;
>     usepackage [utf8] inputenc;
>     usepackage [] tikz;
>     author(fromString ("Eva Richter"));
>     title (fromString ("Relative deviations of columnsum from marginals depending on iterations"))
>    }
> {--
> the second part of the document contains only an error message if the input for the local function
> drawGraphs in readDrawIPFTypeTwo from main-module  is Nothing, i.e. saveIPF does not yield a correct array
> -}
> 
> errorTexFileTwo :: IO ()
> errorTexFileTwo = (execLaTeXT (thePreambleTwo >> document (maketitle >> fromString ("There was nothing to draw")) ))
>                      >>= renderFile "graphicIPF2.tex"
>
> graphicTypeTwo :: [[(Double, Double)]] -> LaTeXT IO() 
> graphicTypeTwo list = do
>  maketitle
>  fromString ("The graphic below shows the changes in relative error of the column sum.")
>  fromString ("Each column is represented by one color, the x axis shows the number of iterations.The y-axis has a logarithmic scale")
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

> 
>
