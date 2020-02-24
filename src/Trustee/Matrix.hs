module Trustee.Matrix (cmdMatrix) where

import Control.Concurrent.STM (STM, orElse)
import Data.Function (on)

import Distribution.Types.PkgconfigVersion (rpmvercmp)

import Trustee.GHC
import Trustee.CabalBuild
import Trustee.Monad
import Trustee.Options
import Trustee.Table
import Trustee.Txt

import Peura
import Urakka

cmdMatrix
    :: GlobalOpts -> Verify -> [Path Absolute]
    -> M ( STM String
         , Urakka () ())
cmdMatrix opts verify dirs' = do
    let ghcs = reverse $ ghcsInRange (goGhcVersions opts)
    xssU <- for dirs $ urakkaCabalRow verify ghcs mempty

    let stms :: [[STM CabalResult]]
        stms = fmap fst xssU

        stms' :: STM [[CabalResult]]
        stms' = traverse (traverse (`orElse` return CabalResultPending)) stms

        xssU' :: Urakka () [[CabalResult]]
        xssU' = traverse snd xssU

    u <- urakka xssU' $ \xss -> do
        putStrs [ renderTable $ makeTable ghcs xss ]

        -- TODO: some errors
        putStrs $ take 2 $ mapMaybe isFail $ concat xss

    return (renderTable . makeTable ghcs <$> stms' , u)
  where
    dirs = sortBy (flip rpmvercmp `on` toUTF8BS . toUnrootedFilePath . takeFileName) dirs'

    makeTable :: [GHCVer] -> [[CabalResult]] -> [[Txt]]
    makeTable ghcs xss
        = (emptyTxt : map (mkTxt Black . prettyShow. toVersion) ghcs)
        : zipWith mkRow dirs xss

    mkRow :: Path Absolute -> [CabalResult] -> [Txt]
    mkRow p xs
        = mkTxt Black (toUnrootedFilePath $ takeFileName p)
        : map mkCell xs

    mkCell :: CabalResult -> Txt
    mkCell CabalResultPending    = mkTxt Black   "..."
    mkCell CabalResultOk         = mkTxt Green   "OK"
    mkCell CabalResultDryFail {} = mkTxt Blue    "NO-IP"
    mkCell CabalResultDepFail {} = mkTxt Magenta "DEP"
    mkCell CabalResultFail {}    = mkTxt Red     "FAIL"

    -- mkCell CabalResultTestFail   = mkTxt Yellow  "TEST"

    isFail (CabalResultFail _ o e) = Just (fromUTF8BS o ++ "\n" ++ fromUTF8BS e)
    isFail _                       = Nothing
