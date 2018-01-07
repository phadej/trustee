module Trustee.Txt (
    Txt,
    mkTxt,
    emptyTxt,
    txtToShowS,
    txtToString,
    txtLength,
    txtLeftPad,
    Color (..),
    colored,
    ) where

import System.Console.ANSI
       (setSGRCode, ConsoleLayer (..), ColorIntensity (..), SGR (..), Color(..))

data Txt = Txt !Color !Int String

instance Show Txt where
    showsPrec d (Txt c _ s)
        = showString (setSGRCode [SetColor Foreground Dull c])
        . showsPrec d s
        . showString (setSGRCode [Reset])

emptyTxt :: Txt
emptyTxt = Txt Black 0 ""

mkTxt :: Color -> String -> Txt
mkTxt c s = Txt c (length s) s

txtToShowS :: Txt -> ShowS
txtToShowS (Txt Black _ s) = showString s
txtToShowS (Txt c     _ s)
    = showString (setSGRCode [SetColor Foreground Dull c])
    . showString s
    . showString (setSGRCode [Reset])

txtToString :: Txt -> String
txtToString s = txtToShowS s ""

txtLength :: Txt -> Int
txtLength (Txt _ n _ ) = n

txtLeftPad :: Txt -> Int -> Txt
txtLeftPad (Txt c n s) m = mkTxt c $ s ++ replicate (m - n) ' '

colored :: Color -> String -> String
colored c s = txtToString (mkTxt c s)
