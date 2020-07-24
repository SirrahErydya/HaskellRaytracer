 
{-# LANGUAGE ForeignFunctionInterface #-}

module Geometry where

import Foreign.C.Types
import Linear.Vector
import Linear.V

boring_test :: Int -> Int
boring_test 0 = 0
boring_test n = n + boring_test (n-1)


cast_initial_rays :: (Float, Float, Float) -> Float -> Float -> Float -> Float -> Float -> (Float, Float, Float)
cast_initial_rays (x, y, z) theta rho d w h = (to_vector cx cy cz) - (to_vector qx qy 0)
	where 
		cx = d * tan (theta/2)
		cy = d * tan (rho/2)
		cz = d
		qx = 2*cx/(w-1)
		qy = 2*cy/(h-1)

