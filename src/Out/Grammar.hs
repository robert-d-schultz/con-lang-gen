module Out.Grammar
( writeGrammar
) where

import ClassyPrelude

import Data.Grammar

-- Write out grammar system
writeGrammar :: Grammar -> Text
writeGrammar (Grammar si ob ci vi ah ns ot nt tm ic wm pp qi) = "\n<br>\nParameters:" ++ out where
  out = "\n<ul>\n\t<li>" ++ intercalate "</li>\n\t<li>" [ tshow si
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
                                          ] ++ "</li>\n</ul>"
