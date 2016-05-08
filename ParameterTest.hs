module ParameterTest where

import TestCommon
import Andromeda



test = do
    print "Parameter test"

    let p1 = temperaturePar
    let p2 = temperaturePar
    let p3 = pressurePar




    if (p1 == p2) 
        then print "Equality success"
        else print "Equality fail"

    if (p1 == p3)
        then print "Inequality fail"
        else print "Inequality success"


