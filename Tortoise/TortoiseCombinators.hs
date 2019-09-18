module TortoiseCombinators
       ( andThen 
       , loop 
       , invisibly 
       , retrace 
       , overlay 
       ) where

import Tortoise

-- See Tests.hs or the assignment spec for specifications for each
-- of these combinators.

andThen :: Instructions -> Instructions -> Instructions
andThen Stop i = i
andThen i Stop = i
andThen (Move d i1) i2 = Move d (andThen i1 i2)
andThen (Turn a i1) i2 = Turn a (andThen i1 i2)
andThen (SetStyle ls i1) i2 = SetStyle ls (andThen i1 i2)
andThen (SetColour c i1) i2 = SetColour c (andThen i1 i2)
andThen (PenDown i1) i2 = PenDown (andThen i1 i2)
andThen (PenUp i1) i2 = PenUp (andThen i1 i2)

loop :: Int -> Instructions -> Instructions
loop n i 
    | n <= 0    =  Stop
    | otherwise = andThen i (loop (n-1) i)

invisibly :: Instructions -> Instructions
invisibly Stop = Stop
invisibly i = addUp (andThen (getInstructions i) (getPen i))

addUp :: Instructions -> Instructions
addUp i = PenUp i

getInstructions :: Instructions -> Instructions
getInstructions Stop =  PenDown Stop
getInstructions (Move d i) = Move d (getInstructions i)
getInstructions (Turn a i) = Turn a (getInstructions i)
getInstructions (SetStyle ls i) = SetStyle ls (getInstructions i)
getInstructions (SetColour c i) = SetColour c (getInstructions i)
getInstructions (PenDown i) = getInstructions i  
getInstructions (PenUp i) = getInstructions i

getPen :: Instructions -> Instructions
getPen Stop =  Stop
getPen (Move d i) = getPen i
getPen (Turn a i) = getPen i
getPen (SetStyle ls i) = getPen i
getPen (SetColour c i) = getPen i
getPen (PenDown i) = PenDown (getPen i)  
getPen (PenUp i) = PenUp (getPen i)


retrace :: Instructions -> Instructions
retrace Stop = Stop
retrace i = subPens (subColors (subStyles rl sl) cl) pl 
            where rl = reverseIns i Stop
                  sl = realStyles i
                  cl = realColors i
                  pl = realPens i



reverseIns :: Instructions -> Instructions -> Instructions
reverseIns Stop i = i
reverseIns (Move d i) i2 = reverseIns i (Move (-d) i2)
reverseIns (Turn a i) i2 = reverseIns i (Turn (-a) i2)
reverseIns (SetStyle ls i) i2 = reverseIns i (SetStyle ls i2)
reverseIns (SetColour c i) i2 = reverseIns i (SetColour c i2)
reverseIns (PenDown i) i2 = reverseIns i (PenDown i2)
reverseIns (PenUp i) i2 = reverseIns i (PenUp i2)

subStyles :: Instructions -> Instructions -> Instructions
subStyles Stop i = i
subStyles (Move d i1) i2 = Move d (subStyles i1 i2)
subStyles (Turn a i1) i2 = Turn a (subStyles i1 i2)
subStyles (SetStyle ls1 i1) (SetStyle ls2 i2) = SetStyle ls2 (subStyles i1 i2)
subStyles (SetColour c i1) i2 = SetColour c (subStyles i1 i2)
subStyles (PenDown i1) i2 = PenDown (subStyles i1 i2)
subStyles (PenUp i1) i2 = PenUp (subStyles i1 i2)

subColors :: Instructions -> Instructions -> Instructions
subColors Stop i = i
subColors (Move d i1) i2 = Move d (subColors i1 i2)
subColors (Turn a i1) i2 = Turn a (subColors i1 i2)
subColors (SetStyle ls i1) i2 = SetStyle ls (subColors i1 i2)
subColors (SetColour c1 i1) (SetColour c2 i2) = SetColour c2 (subColors i1 i2)
subColors (PenDown i1) i2 = PenDown (subColors i1 i2)
subColors (PenUp i1) i2 = PenUp (subColors i1 i2)

subPens :: Instructions -> Instructions -> Instructions
subPens Stop i = i
subPens (Move d i1) i2 = Move d (subPens i1 i2)
subPens (Turn a i1) i2 = Turn a (subPens i1 i2)
subPens (SetStyle ls i1) i2 = SetStyle ls (subPens i1 i2)
subPens (SetColour c i1) i2 = SetColour c (subPens i1 i2)
subPens (PenDown i1) (PenDown i2) = PenDown (subPens i1 i2)
subPens (PenDown i1) (PenUp i2) = PenUp (subPens i1 i2)
subPens (PenUp i1) (PenDown i2) = PenDown (subPens i1 i2)
subPens (PenUp i1) (PenUp i2) = PenUp (subPens i1 i2)

realStyles :: Instructions ->Instructions
realStyles i = backStyles (getStyles i (SetStyle (Solid 1) Stop))

realColors :: Instructions ->Instructions
realColors i = backColors (getColors i (SetColour white Stop))

realPens :: Instructions -> Instructions
realPens i = backPens (getPens i (PenDown Stop))

backStyles :: Instructions -> Instructions
backStyles Stop = Stop
backStyles (SetStyle l i) = i

backColors :: Instructions -> Instructions
backColors Stop = Stop
backColors (SetColour c i) = i

backPens :: Instructions -> Instructions
backPens Stop = Stop
backPens (PenDown i) = i
backPens (PenUp i) = i


getStyles :: Instructions -> Instructions ->Instructions
getStyles Stop i = i
getStyles (Move d i) i2 = getStyles i i2
getStyles (Turn a i) i2 = getStyles i i2
getStyles (SetStyle ls i) i2 = getStyles i (SetStyle ls i2)
getStyles (SetColour c i) i2 = getStyles i i2
getStyles (PenDown i) i2 = getStyles i i2
getStyles (PenUp i) i2 = getStyles i i2

getColors :: Instructions -> Instructions -> Instructions
getColors Stop i = i
getColors (Move d i) i2 = getColors i i2
getColors (Turn a i) i2 = getColors i i2
getColors (SetStyle ls i) i2 = getColors i i2
getColors (SetColour c i) i2 = getColors i (SetColour c i2)
getColors (PenDown i) i2 = getColors i i2
getColors (PenUp i) i2 = getColors i i2

getPens :: Instructions -> Instructions -> Instructions
getPens Stop i = i
getPens (Move d i) i2 = getPens i i2
getPens (Turn a i) i2 = getPens i i2
getPens (SetStyle ls i) i2 = getPens i i2
getPens (SetColour c i) i2 = getPens i i2
getPens (PenDown i) i2 = getPens i (PenDown i2)  
getPens (PenUp i) i2 = getPens i (PenUp i2)


overlay :: [Instructions] -> Instructions
overlay [] = Stop
overlay (i:is) = andThen (andThen i (invisibly (retrace i)) (overlay is))  




-- invisibly (PenUp (PenDown Stop)) = andThen (invisibly (PenDown Stop)) (PenUp Stop)
--                                  = andThen (andThen invisibly Stop PenDown Stop) (PenUp Stop)
--                                  = andThen (andThen Stop (PenDown Stop)) (PenUp Stop)
--                                  = andThen (andThen PenDown Stop) (PenUp Stop)
--                                  = andThen PenDown Stop PenUp Stop
--                                  = PenDown andThen Stop (PenUp Stop)
--                                  = PenDown (PenUp Stop)

-- retrace (Move 50 (SetStyle ls (SetColour c (Turn 30 PenDown (PenUp Stop)))))
        -- = SetColour (Colour {redC = 10, greenC = 255, blueC = 10, alphaC = 255})