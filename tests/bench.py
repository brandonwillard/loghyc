#! /usr/bin/env python

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
