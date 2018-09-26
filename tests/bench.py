#! /usr/bin/env python
## adderall - miniKanren in Hy
## Copyright (C) 2014  Gergely Nagy <algernon@madhouse-project.org>
##
## This library is free software: you can redistribute it and/or
## modify it under the terms of the GNU Lesser General Public License
## as published by the Free Software Foundation, either version 3 of
## the License, or (at your option) any later version.
##
## This library is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
## Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public
## License along with this program. If not, see <http://www.gnu.org/licenses/>.

import hy
import sys
import timeit
from nose.tools import nottest


SETUP_FORMAT = """import hy
from {0} import {1}"""


@nottest
def benchmark(module, function, times=10):
    print("{0}/{1} * {2}:".format(module, function, times)),
    sys.__stdout__.flush()
    result = timeit.timeit("{0}()".format(function),
                           setup=SETUP_FORMAT.format(module, function),
                           number=times)
    print("{0:.5f}s ({1:.5f}s average)".format(result, result / times))


def test_zebra_benchmark():
    benchmark("tests.extra.zebra_bench", "zebra_benchmark")


def test_cheetah_benchmark():
    benchmark("tests.extra.cheetah_bench", "cheetah_benchmark", 100)
