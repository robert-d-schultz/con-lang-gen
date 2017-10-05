{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Out.Grammar
( writeGrammar
) where

import ClassyPrelude

import Data.Grammar

-- Parse grammar system
writeGrammar :: Grammar -> Text
writeGrammar (Grammar si ob ci vi ah ns ot nt tm ic wm pp qi) = "\nParameters:" ++ out where
  out = "\n\t*" ++ intercalate "\n\t*" [ tshow si
                                  , tshow ob
                                  , tshow ci
                                  , tshow vi
                                  , tshow ah
                                  , tshow ns
                                  , tshow ot
                                  , tshow nt
                                  , tshow tm
                                  , tshow ic
                                  , tshow wm
                                  , tshow pp
                                  , tshow qi
                                  ]
