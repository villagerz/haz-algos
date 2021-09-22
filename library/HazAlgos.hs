{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
-- no-cse allows CmdArgs to ignore duplicate arguments
-- | An example module.
module HazAlgos where

import qualified Algz.RankSuffixes      as Rank
import           Prelude.Unicode        ((∘), (≤), (⊥))
import           System.Console.CmdArgs ((&=))
import qualified System.Console.CmdArgs as Cli


data Compress = Bwt
    { file :: String
    }
    | Ranktails
    { val :: String
    }
    deriving (Show, Cli.Data, Cli.Typeable)

bwt = Bwt {file = Cli.def
                 &= Cli.typFile
                 &= Cli.help "File to run BWT on"
               }
ranktails = Ranktails { val = "some string" &= Cli.args }



main :: IO ()
main = do
  compress ←  Cli.cmdArgs ( Cli.modes [bwt, ranktails] &= (Cli.program "haz-algos"))
  result ← preprocc compress
  print result


preprocc ∷ Compress → IO String
preprocc (Ranktails v) =  return (show (Rank.ranktails v))
preprocc (Bwt f)       = return "not implemented yet"
