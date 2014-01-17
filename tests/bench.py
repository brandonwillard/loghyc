#! /usr/bin/env python

import benchmark
import hy
from tests.extra.zebra_bench import BenchMark_Zebra

if __name__ == '__main__':
    benchmark.main(format="markdown", numberFormat="%.4g")
