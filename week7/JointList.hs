{-# OPTIONS_GHC -Wall #-}
module JointList where

import Data.Monoid

data JointList m a = Empty
                   | Single m a
                   | Append m (JointList m a) (JointList m a)
                     deriving (Eq, Show)

(+++) :: Monoid m => JointList m a -> JointList m a -> JointList m a
(+++) a b = Append (tag a <> tag b) a b

tag :: Monoid m => JointList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- instance Monoid m => Monoid (JointList m a) where
--     mempty = Empty
--     mappend = (+++)
