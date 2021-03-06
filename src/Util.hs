module Util(filterMap, separate, separateMap) where

maybecons :: Maybe t -> [t] -> [t]
maybecons Nothing l = l
maybecons (Just e) l = e : l

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap _ [] = []
filterMap f (a:as) = maybecons (f a) $ filterMap f as

separate :: (a -> Bool) -> [a] -> ([a], [a])
separate f list =
  foldr (\el (l, r) -> if (f el) then (el : l, r) else (l, el : r)) ([], []) list

separateMap :: (a -> Either b c) -> [a] -> ([b], [c])
separateMap f list =
  foldr (\el (l, r) -> case (f el) of
    Left b -> (b : l, r)
    Right c -> (l, c : r)) ([], []) list
