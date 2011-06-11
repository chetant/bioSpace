{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Fields.Users where

import Control.Applicative((<$>), (<*>))
import Data.Text(Text, pack, unpack)

import BioSpace

zipMulFlds ds fs mvs = zip ds (zip fs mvs)

userAccessField = multipleFields (areq boolField)

multipleFields form datas fldsets mvals = foldr (<*>) z xs
    where xs = map (\x -> (:) . ((,) $ fst x) <$> mkform x) r
          z = (:[]) . ((,) $ fst l) <$> (mkform l)
          mkform = uncurry form . snd
          l = last fs
          r = init fs
          fs = zipMulFlds datas fldsets mvals
          
