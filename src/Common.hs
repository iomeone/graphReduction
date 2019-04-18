module Common where

joinBy sep list = drop (length sep) $ concat $ map (\item -> sep ++ item) list
