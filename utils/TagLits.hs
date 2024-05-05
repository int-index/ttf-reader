{-# LANGUAGE LambdaCase, BlockArguments #-}

module TagLits where

import Data.Char
import Data.List

tagName :: String -> String
tagName = map \case
  '/' -> '_'
  ' ' -> ' '
  c | isAlphaNum c -> c
    | otherwise    -> error $ "tagName: unknown tag char: " ++ show c

tagCase :: String -> [String] -> String
tagCase namePrefix tags =
    unlines ("case tag of" : map mk_match tags)
  where
    mk_match tag = "  " ++ tagLit tag ++ " -> Just " ++ namePrefix ++ tagName tag

tagLit :: String -> String
tagLit [c1, c2, c3, c4] = "0x" ++ showHexChar c1 (showHexChar c2 (showHexChar c3 (showHexChar c4 "")))
tagLit _ = error "tagLit: not a 4-letter tag"

showHexChar :: Char -> ShowS
showHexChar c = (\s -> intToDigit d1 : intToDigit d2 : s)
  where (d1, d2) = quotRem (ord c) 16

main :: IO ()
main = do
  putStrLn $
    tagCase "TableTag_" $ nub $
      [
        -- Required tables
        "cmap",
        "head",
        "hhea",
        "hmtx",
        "maxp",
        "name",
        "OS/2",
        "post",

        -- Tables related to TrueType
        "cvt ",
        "fpgm",
        "glyf",
        "loca",
        "prep",
        "gasp",

        -- Tables related to CFF outlines
        "CFF ",
        "CFF2",
        "VORG",

        -- Tables related to SVG outlines
        "SVG ",

        -- Tables related to bitmap glyphs
        "EBDT",
        "EBLC",
        "EBSC",
        "CBDT",
        "CBLC",
        "sbix",

        -- Advanced typographic tables
        "BASE",
        "GDEF",
        "GPOS",
        "GSUB",
        "JSTF",
        "MATH",

        -- Tables used for OpenType font variations
        "avar",
        "cvar",
        "fvar",
        "gvar",
        "HVAR",
        "MVAR",
        "STAT",
        "VVAR",

        -- Tables related to color fonts
        "COLR",
        "CPAL",
        "CBDT",
        "CBLC",
        "sbix",
        "SVG ",

        -- Other OpenType tables
        "DSIG",
        "hdmx",
        "kern",
        "LTSH",
        "MERG",
        "meta",
        "STAT",
        "PCLT",
        "VDMX",
        "vhea",
        "vmtx"
      ]
  putStrLn ("kernTag = " <> tagLit "kern")
