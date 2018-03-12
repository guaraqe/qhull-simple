{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Simple bindings to part of Qhull (<http://qhull.org>).
--
-- Currently, Qhull's @Qt@ option is always used, and @Qx@ is added in
-- dimension 5 and above.  This was done simply because it's what
-- Octave does. I hope to expose more Qhull options in the future.
--
-- The call to Qhull does use @'unsafePerformIO'@, but this /should/
-- be OK since @Qt@ and @Qx@ mode should give behavior fully
-- determined by the input point set. See also \"on referential
-- transparency\" below.
--
-- /On thread safety:/ Qhull is horrendously thread-/unsafe/, with
-- lots of global state. Thus, calls to it are serialized in a small C
-- wrapper. No speedup can therefore be had by running Qhull
-- computations in parallell, but it should be safe to do so.
--
-- /On referential transparency:/ As far as I can tell, Qhull
-- computations are fully determined by their input when using the
-- @Qt@ and/or the @Qx@ options, as is done by these bindings. I'm
-- somewhat more suspicious of the /error messages/ given by Qhull if
-- it fails to compute a convex hull. There is /a lot/ of information
-- in there, and looking at the Qhull code, it wouldn't surprise me if
-- it sometimes included the current date, time and weather. Please
-- notify me if you see a case where the same input data produces
-- different error messages!

module Numeric.Qhull
       ( Error(Error, errorCode, errorMessage)
       , qhull
       , qhull'
       , qhull''
       , delaunay
       , delaunay'
       , delaunay'' ) where

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.ForeignPtr
import System.IO.Unsafe
import qualified Data.Vector.Storable as SV
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV

data Error = Error { errorCode :: Int,
                     errorMessage :: String }

-- | @'qhull' d points@ assumes that @points@ is a row-major
-- representation of an @m@ by @d@ matrix, where row @i@ constitutes
-- the coordinates of the @i@'th point for which to compute the convex
-- hull. If @d@ does not divide the length of @points@, then it is
-- padded with zeros so that that becomes the case. This causes
-- @points@ to be copied, and may be detrimental to performance. See
-- also @'qhull''@ and @'qhull'''@ below.
--
-- The resulting list contains vectors of length @d@. These are the indices of
-- the input points that make up the vertices of the /simplicial/ faces of the
-- convex hull of the input points. If the computation fails, the @'Error'@
-- returned contains a detailed error message as reported by Qhull.
--
-- 'qhull' and 'qhull'' are convenience functions that ultimately call 'qhull',
-- so the latter should be used if your data is already in a suitable form.
--
-- Currently, Qhull is run with the @Qt@ option in dimension 4 and below, and
-- @Qt Qx@ above.
qhull :: (GV.Vector v Double) => Int -> v Double -> Either [SV.Vector Int] Error
qhull d points
    | GV.null points = Left []
    | otherwise = unsafePerformIO action
    where
      r = GV.length points `rem` d
      points' = if r == 0
                then points
                else points GV.++ GV.replicate (d - r) 0
      m = GV.length points' `quot` d
      d' = fromIntegral d
      m' = fromIntegral m
      command = if d <= 4
                then "qhull Qt Qz" -- Tirar qz?
                else "qhull Qt Qx Qz"
      action = withCString command $ \pCommand ->
               SV.unsafeWith (GV.convert points') $ \pPoints ->
               alloca $ \ppRes ->
               alloca $ \pN ->
               alloca $ \ppErrMsg ->
               c_qhull_wrapper d' m' pPoints pCommand pN ppRes ppErrMsg >>= \ret ->
               if ret == 0
               then peek pN >>= \n ->
                    peek ppRes >>= newForeignPtr finalizerFree >>= \fpRes ->
                    let
                        n' = fromIntegral n
                        res = map (\i -> SV.map fromIntegral (SV.unsafeFromForeignPtr fpRes i d)) [0,d..d*(n'-1)]
                    in
                    return (Left res)
               else peek ppErrMsg >>= \pErrMsg ->
                    peekCString pErrMsg >>= \errMsg ->
                    free pErrMsg >>
                    return (Right (Error (fromIntegral ret) errMsg))


delaunay :: Int -> SV.Vector Double -> Either Error [SV.Vector Int]
delaunay d points
    | GV.length points < d*(d + 2) = Right []
    | GV.any isNaN points = Right []
    | GV.length points `rem` d /= 0 =
        Left (Error 1 "Number of points and dimension do not match.")
    | otherwise = unsafePerformIO action
    where
      m = GV.length points `div` d
      d' = fromIntegral d
      m' = fromIntegral m
      command = if d <= 3
                  then "qhull d Qbb Qt" -- tirar Qz, juntar Qbb?
                  else "qhull d Qbb Qt Qx"
      action = withCString command $ \pCommand ->
               SV.unsafeWith points $ \pPoints ->
               alloca3 $ qhullWrapper d' m' pPoints pCommand

-- | A version of 'qhull' where the points are input as a vector of
-- vectors. In @'qhull'' points@, every element in @points@ is assumed to have
-- the same length as the /first/ element. The @points@ 'GV.Vector' will be
-- concatenated together to form the same input as for 'qhull'.
qhull' :: (GV.Vector v Double) => V.Vector (v Double) -> Either [SV.Vector Int] Error
qhull' points
    | V.null points = Left []
    | otherwise     = qhull (GV.length (V.head points)) (GV.concat (V.toList points))

delaunay' :: V.Vector (SV.Vector Double) -> Either Error [SV.Vector Int]
delaunay' points
    | V.null points = Right []
    | otherwise     = delaunay (GV.length (V.head points)) (GV.concat (V.toList points))


-- | A list-based version of 'qhull'' for convenience. The conventions
-- are the same as for 'qhull''.
qhull'' :: [[Double]] -> Either [SV.Vector Int] Error
qhull'' points = qhull' (V.fromList (map SV.fromList points))

delaunay'' :: [[Double]] -> Either Error [SV.Vector Int]
delaunay'' points = delaunay' (V.fromList (map SV.fromList points))


foreign import ccall "qhull_wrapper.h qhull_wrapper"
  c_qhull_wrapper :: CInt            -- ^ spatial dimension
                  -> CInt            -- ^ number of vertices
                  -> Ptr Double      -- ^ pointer to row-major matrix of coordinates
                  -> CString         -- ^ qhull command to be called
                  -> Ptr CInt        -- ^ pointer to output number of simplexes
                  -> Ptr (Ptr CInt)  -- ^ pointer to output simplexes
                  -> Ptr (Ptr CChar) -- ^ pointer to error message
                  -> IO CInt         -- ^ exit status



qhullWrapper d' m' pPoints pCommand pN ppRes ppErrMsg =
  do ret <- c_qhull_wrapper d' m' pPoints pCommand pN ppRes ppErrMsg
     if ret == 0
               then
                 do n <- toInt <$> peek pN
                    outPtr <- peek ppRes
                    outFPtr <- newForeignPtr finalizerFree outPtr
                    let outDim = toInt d' + 1
                        res = parseOutput outDim n outFPtr
                    return (Right res)
               else
                 do pErrMsg <- peek ppErrMsg
                    errMsg <- peekCString pErrMsg
                    free pErrMsg
                    return (Left (Error (fromIntegral ret) errMsg))

----------------------------------------------------------------------

toCInt :: Int -> CInt
toCInt = fromIntegral

toInt :: CInt -> Int
toInt = fromIntegral

range :: Int -> Int -> [Int]
range d n = map (*d) [0 .. n-1]

parseOutput :: Int -> Int -> ForeignPtr CInt -> [SV.Vector Int]
parseOutput d n p = map (\i -> SV.map toInt (SV.unsafeFromForeignPtr p i d))
                              (range d n)

alloca3 :: (Storable a, Storable b, Storable c)
        => (Ptr a -> Ptr b -> Ptr c -> IO d) -> IO d
alloca3 f = alloca $ \x
         -> alloca $ \y
         -> alloca $ \z
         -> f x y z
