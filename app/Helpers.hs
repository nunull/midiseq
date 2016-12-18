module Helpers where

maybe' :: (a -> b) -> Maybe a -> b -> b
maybe' f m d = maybe d f m

mapFst :: (a -> b) -> (a,c) -> (b,c)
mapFst f (a,b) = (f a, b)

mapSnd :: (a -> b) -> (c,a) -> (c,b)
mapSnd f (a,b) = (a, f b)
