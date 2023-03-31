# Function: two_digit_pad
# Project: Acquisition and Analysis of Crowd-Sourced Traffic Data at Varying Spatial Scales
# Script Authors: Sebastian T. Rowland and Jenni A. Shearston 
# Updated: 03/02/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 1: Function to Add a Two-digit Pad to Specified Values

####**************
#### N: Notes #### 
####**************

# This function adds a two digit pad to the left of specified values.

####************************************************************
#### 1: Function to Add a Two-digit Pad to Specified Values #### 
####************************************************************

# BEGIN FUNCTION 

two_digit_pad <- function(x) {
  stringr::str_pad(x, 2, 'left', '0')
}

# END FUNCTION



