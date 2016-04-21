{-# LANGUAGE OverloadedStrings #-}

module HardwareDescriptorsTest where

import Andromeda

import TestCommon


boosters :: Hdl ()
boosters = do
    untag $ sensor aaa_t_25 "zone1-t" temperature
    untag $ sensor aaa_p_02 "zone1-p" pressure
    untag $ sensor aaa_p_02 "zone2-p" pressure









test = do
    print "HardwareDescriptorsTest:"
