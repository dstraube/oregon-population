######################################################################
## Copyright (C) 2016, Dave Straube, http://davestraube.com
##     
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA
######################################################################

# Pretty print routines - generic, money, and percentage versions.

pp <- function(val, digits = 2) {
    format(ifelse ( digits > 0, round(val, digits = digits), as.integer(val) ),
           nsmall = ifelse ( digits > 0, digits, 0 ),
           scientific = FALSE,
           big.mark = ',')
}
money <- function(val, digits = 2) { paste('$', pp(val, digits), sep = '') }
pct <- function(val, digits = 2) { paste(pp(val, digits), '%', sep = '') }