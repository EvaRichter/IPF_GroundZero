> module IPFGraphResultsTypeOne where
>
> import Data.List
>
> import Text.LaTeX
> import Text.LaTeX.Packages.Inputenc
> import Text.LaTeX.Packages.TikZ 
> import Text.LaTeX.Base.Commands
> import Text.LaTeX.Base.Writer


>
>
> {- The functions in this module produce a LaTeX File that will compile to a document(article)
> consisting of an introduction and a tikzPicture. The graphic shows the width of the intervals
> ,i.e. the entries in an IPF matrix depending on the number of iterations.
> The interface to the outside is the function buildGraphicOne.
> -}
> 
> savePath = "IPFGraphResults/graphicIPF1.tex"
>
> {- the function to actually write th file, is called from Main -} 
>
> buildGraphicOne :: [[(Double,Double)]] -> IO()
> buildGraphicOne list = (execLaTeXT
>                      (thePreambleOne >> document (graphicTypeOne list) ))
>                      >>= renderFile savePath
>
> {-constants to scale the graphic appropriately-}
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
> frame :: TikZ
> frame = draw (Rectangle (Start $ pointAtXY 0 0) (pointAtXY 12 11))
> 
>

>
> -- production of preamble with title and remarks
> 
> thePreambleOne :: LaTeXT IO ()
> thePreambleOne = do
>    {documentclass [] article;
>     usepackage [utf8] inputenc;
>     usepackage [] tikz;
>     author(fromString ("Eva Richter"));
>     title (fromString ("Graphik 1 von IPF"))
>    }
> -- the second part of the document is an error message if the saveIPF does not yield a correct array
>  
> errorTexFileOne :: IO ()
> errorTexFileOne = (execLaTeXT
>                   (thePreambleOne >> document (maketitle >> fromString ("There was nothing to draw"))
>                    ))
>                    >>= renderFile savePath


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
>   fromString ("The graphic below depicts the length of intervels depending on size of the average")
>   fromString ("value classified by number of iterations (length is on a logarithmic scale!).")
>   newline  
>   center $ tikzpicture $  
>     foldl (->>) frame [graphicTypeOneSingleIt itNum itData logScaling | (itNum, itData) <- zip [1..] list]
>
>
> {- scaling of results works with lenScale and higScale for width and centre of intervals -}
> 
> 
