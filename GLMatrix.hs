-- | Modified from / based on:
--   https://github.com/kig/tomtegebra/blob/master/Tomtegebra/Matrix.hs
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE PatternSynonyms #-}
module GLMatrix (
    translationMatrix, frustumMatrix,
    identityMatrix, toGLFormat, withMatrix,
    matrixMulVec, matrix4x4To3x3, matrix3x3To4x4,
    invertMatrix4x4ON, scalingMatrix,
    rotationMatrix, lookAtMatrixG, orthoMatrix,
    perspectiveMatrix, addVec,
    setMatrix4x4Uniform,
    Matrix4x4, Matrix3x3, Vector4, Vector3
) where

import Data.List (transpose)
import Foreign (Ptr)
import Foreign.C (withCString)
import Foreign.Marshal.Array (withArray)
import Graphics.GL
    (GLfloat, GLuint, glGetUniformLocation,
     glUniformMatrix4fv, pattern GL_FALSE)

-- | 4x4 Matrix in the OpenGL orientation:
--   translation column is the last 4 elements.
type Matrix4x4 = [[GLfloat]]
-- | 3x3 Matrix in the OpenGL orientation.
type Matrix3x3 = [[GLfloat]]
-- | Four element GLfloat vector.
type Vector4 = [GLfloat]
-- | Three element GLfloat vector.
type Vector3 = [GLfloat]

instance Num Matrix4x4 where
    a * b =
        map (\row -> map (dotVec row) at) b
        where at = transpose a
    a + b = applyToIndices2 a b (+)
    abs = map (map abs)
    fromInteger i =
        [
        [fromInteger i, 0, 0, 0],
        [0, fromInteger i, 0, 0],
        [0, 0, fromInteger i, 0],
        [0, 0, 0, fromInteger i]
        ]
    signum = map (map signum)

setMatrix4x4Uniform :: GLuint -> Matrix4x4 -> String -> IO ()
setMatrix4x4Uniform shader matrix var = do
    loc <- withCString var $ glGetUniformLocation shader
    withMatrix matrix (glUniformMatrix4fv loc 1 (fromIntegral GL_FALSE))

withMatrix :: Matrix4x4 -> (Ptr GLfloat -> IO a) -> IO a
withMatrix = withArray . toGLFormat

applyToIndices2 :: [[a]] -> [[b]] -> (a -> b -> c) -> [[c]]
applyToIndices2 (a:as) (b:bs) f =
    applyToIndices a b f : applyToIndices2 as bs f
applyToIndices2 _ _ _ = []

applyToIndices :: [a] -> [b] -> (a -> b -> c) -> [c]
applyToIndices (a:as) (b:bs) f =
    f a b : applyToIndices as bs f
applyToIndices _ _ _ = []

toGLFormat :: Matrix4x4 -> [GLfloat]
toGLFormat = concat
{-# INLINE toGLFormat #-}

-- | The 'Matrix4x4' identity matrix.
identityMatrix :: Matrix4x4
identityMatrix =
    [
        [1,0,0,0],
        [0,1,0,0],
        [0,0,1,0],
        [0,0,0,1]
    ]
{-# INLINE identityMatrix #-}

-- | Multiplies a vector by a matrix.
matrixMulVec :: Matrix4x4 -> Vector4 -> Vector4
matrixMulVec m v = map (dotVec v) (transpose m)
{-# INLINE matrixMulVec #-}

-- | Returns the upper-left 3x3 matrix of a 4x4 matrix.
matrix4x4To3x3 :: Matrix4x4 -> Matrix3x3
matrix4x4To3x3 m = take 3 $ map vec4To3 m

-- | Pads the 3x3 matrix to a 4x4 matrix with a 1 in 
--   bottom right corner and 0 elsewhere.
matrix3x3To4x4 :: Matrix3x3 -> Matrix4x4
matrix3x3To4x4 [x,y,z] = [x ++ [0], y ++ [0], z ++ [0], [0,0,0,1]]
matrix3x3To4x4 m = m
{-# INLINE matrix3x3To4x4 #-}

-- | Inverts a 4x4 orthonormal matrix with the special case trick.
invertMatrix4x4ON :: Matrix4x4 -> Matrix4x4
invertMatrix4x4ON m = -- orthonormal matrix inverse
    let [a,b,c] = transpose $ matrix4x4To3x3 m
        [_,_,_,t4] = m
        t = vec4To3 t4
    in [
        vec3To4 a 0, vec3To4 b 0, vec3To4 c 0,
        [dotVec a t, dotVec b t, dotVec c t, t4 !! 3]
       ]

-- | Creates the translation matrix that translates points by the given vector.
translationMatrix :: Vector3 -> Matrix4x4
translationMatrix [x,y,z] =
    [[1,0,0,0],
     [0,1,0,0],
     [0,0,1,0],
     [x,y,z,1]]
translationMatrix _ = identityMatrix
{-# INLINE translationMatrix #-}

-- | Creates the scaling matrix that scales points by the factors given by the
--   vector components.
scalingMatrix :: Vector3 -> Matrix4x4
scalingMatrix [x,y,z] =
   [[x,0,0,0],
    [0,y,0,0],
    [0,0,z,0],
    [0,0,0,1]]
scalingMatrix _ = identityMatrix
{-# INLINE scalingMatrix #-}

rotationMatrix :: GLfloat -> Vector3 -> Matrix4x4
rotationMatrix angle axis =
    let [x,y,z] = normalizeVec axis
        c = cos angle
        s = sin angle
        c1 = 1-c
    in [
      [x*x*c1+c, y*x*c1+z*s, z*x*c1-y*s, 0],
      [x*y*c1-z*s, y*y*c1+c, y*z*c1+x*s, 0],
      [x*z*c1+y*s, y*z*c1-x*s, z*z*c1+c, 0],
      [0,0,0,1]
       ]
{-# INLINE rotationMatrix #-}

-- | Creates a lookAt matrix from three vectors: the eye position, the point the
--   eye is looking at and the up vector of the eye.
lookAtMatrixG :: Vector3 -> Vector3 -> Vector3 -> Matrix4x4
lookAtMatrixG eye center up =
    let z = directionVec eye center
        x = normalizeVec $ crossVec3 up z
        y = normalizeVec $ crossVec3 z x
    in matrix3x3To4x4 (transpose [x,y,z]) *
        translationMatrix (negateVec eye)
{-# INLINE lookAtMatrixG #-}

-- | Creates a frustumMatrix from the given
--   left, right, bottom, top, znear and zfar
--   values for the view frustum.
frustumMatrix ::
    GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Matrix4x4
frustumMatrix left right bottom top znear zfar =
    let x = 2*znear/(right-left)
        y = 2*znear/(top-bottom)
        a = (right+left)/(right-left)
        b = (top+bottom)/(top-bottom)
        c = -(zfar+znear)/(zfar-znear)
        d = -2*zfar*znear/(zfar-znear)
    in
       [[x, 0, 0, 0],
        [0, y, 0, 0],
        [a, b, c, -1],
        [0, 0, d, 0]]
{-# INLINE frustumMatrix #-}

orthoMatrix ::
    GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Matrix4x4
orthoMatrix l r b t n f =
    let ai = 2/(r-l)
        bi = 2/(t-b)
        ci = -2/(f-n)
        di = -(r+l)/(r-l)
        ei = -(t+b)/(t-b)
        fi = -(f+n)/(f-n)
    in
    [[ai, 0, 0, 0],
     [0, bi, 0, 0],
     [0, 0, ci, 0],
     [di, ei, fi, 1]]
{-# INLINE orthoMatrix #-}

-- | Creates a perspective projection matrix for the given field-of-view,
--   screen aspect ratio, znear and zfar.
perspectiveMatrix :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Matrix4x4
perspectiveMatrix fovy aspect znear zfar =
    let ymax = znear * tan (fovy * pi / 360.0)
        ymin = -ymax
        xmin = ymin * aspect
        xmax = ymax * aspect
    in frustumMatrix xmin xmax ymin ymax znear zfar
{-# INLINE perspectiveMatrix #-}

-- | Normalizes a vector to a unit vector.
normalizeVec :: [GLfloat] -> [GLfloat]
normalizeVec v = scaleVec (recip $ lengthVec v) v
{-# INLINE normalizeVec #-}

-- | Scales a vector by a scalar.
scaleVec :: GLfloat -> [GLfloat] -> [GLfloat]
scaleVec s = map (s*)
{-# INLINE scaleVec #-}

-- | Computes the length of a vector.
lengthVec :: [GLfloat] -> GLfloat
lengthVec v = sqrt.sum $ map square v
{-# INLINE lengthVec #-}

-- | Inner product of two vectors.
innerVec :: [GLfloat] -> [GLfloat] -> [GLfloat]
innerVec = zipWith (*)
{-# INLINE innerVec #-}

-- | Adds two vectors together.
addVec :: [GLfloat] -> [GLfloat] -> [GLfloat]
addVec = zipWith (+)
{-# INLINE addVec #-}

-- | Subtracts a vector from another.
subVec :: [GLfloat] -> [GLfloat] -> [GLfloat]
subVec = zipWith (-)
{-# INLINE subVec #-}

-- | Negates a vector.
negateVec :: [GLfloat] -> [GLfloat]
negateVec = map negate
{-# INLINE negateVec #-}


-- | Computes the direction unit vector between two vectors.
directionVec :: [GLfloat] -> [GLfloat] -> [GLfloat]
directionVec u v = normalizeVec (subVec u v)
{-# INLINE directionVec #-}

-- | Vector dot product.
dotVec :: [GLfloat] -> [GLfloat] -> GLfloat
dotVec a b = sum $ innerVec a b
{-# INLINE dotVec #-}

-- | Cross product of two 3-vectors.
crossVec3 :: [GLfloat] -> [GLfloat] -> [GLfloat]
crossVec3 [u0,u1,u2] [v0,v1,v2] = [u1*v2-u2*v1, u2*v0-u0*v2, u0*v1-u1*v0]
crossVec3 _ _ = [0,0,1]
{-# INLINE crossVec3 #-}

-- | Converts a 4-vector into a 3-vector by dropping the fourth element.
vec4To3 :: Vector4 -> Vector3
vec4To3 = take 3
{-# INLINE vec4To3 #-}

-- | Converts a 3-vector into a 4-vector by appending the given value to it.
vec3To4 :: Vector3 -> GLfloat -> Vector4
vec3To4 v i = v ++ [i]
{-# INLINE vec3To4 #-}

-- | Multiplies a GLfloat by itself.
square :: GLfloat -> GLfloat
square x = x * x
{-# INLINE square #-}
