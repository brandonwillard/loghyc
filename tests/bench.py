#! /usr/bin/env python
## adderall - miniKanren in Hy
## Copyright (C) 2014  Gergely Nagy <algernon@madhouse-project.org>
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

import timeit
import hy
import sys

SETUP_FORMAT="""import hy
from {0} import {1}"""

def benchmark(module, function, times = 25):
    print ("{0}/{1} * {2}:".format(module, function, times)),
    sys.__stdout__.flush()
    print (timeit.timeit("{0}()".format(function),
                         setup = SETUP_FORMAT.format(module, function),
                         number = times))


if __name__ == '__main__':
    benchmark("tests.extra.zebra_bench", "zebra_benchmark")
