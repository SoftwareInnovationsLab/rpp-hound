import argparse
import hashlib
import logging
import matplotlib
import matplotlib.pyplot as plt
import numpy
import operator
import os

from mpl_toolkits.axes_grid1 import make_axes_locatable
from pathlib import Path

font = {'family' : 'normal',
        'size'   : 14}

matplotlib.rc('font', **font)


def plot_coverages(args, max_time=120):
    try:
        print(args)
        afl_fuzzing_dir = args.afl
        ff_fuzzing_dir = args.ff
        program = args.program
    except ValueError as e:
        print(e)
        return

    with open(os.path.join(afl_fuzzing_dir, f'{program}-rp-cov.csv'), 'r') as f:
        f.readline()
        afl_coverage = [x.strip().split(',') for x in f.readlines()]
    with open(os.path.join(ff_fuzzing_dir, f'{program}-rp-cov.csv'), 'r') as f:
        f.readline()
        ff_coverage = [x.strip().split(',') for x in f.readlines()]

    afl_coverage = [(float(ts), float(cov)) for (ts, cov) in afl_coverage]
    ff_coverage = [(float(ts), float(cov)) for (ts, cov) in ff_coverage]

    if afl_coverage[-1][0] < max_time:
        afl_coverage.append((max_time, afl_coverage[-1][1]))
    if ff_coverage[-1][0] < max_time:
        ff_coverage.append((max_time, ff_coverage[-1][1]))

    afl_coverage = sorted(set(afl_coverage))
    ff_coverage = sorted(set(ff_coverage))

    while True:
        original_len = len(afl_coverage)
        for i in range(len(afl_coverage)-1):
            if afl_coverage[i+1][0] - afl_coverage[i][0] > 2:
                afl_coverage.insert(i+1, (afl_coverage[i][0]+1, afl_coverage[i][1]))
                break
        if len(afl_coverage) == original_len:
            break

    while True:
        original_len = len(ff_coverage)
        for i in range(len(ff_coverage)-1):
            if ff_coverage[i+1][0] - ff_coverage[i][0] > 2:
                ff_coverage.insert(i+1, (ff_coverage[i][0]+1, ff_coverage[i][1]))
                break
        if len(ff_coverage) == original_len:
            break

    bar = 10

    _, axMain = plt.subplots(1)
    axMain.set_xlim((-1, max_time))
    #axMain.set_title(f'{program.split("-")[0]} fuzzing coverages')
    axMain.set_ylabel('rare-path coverage (%)')
    axMain.plot(*zip(*afl_coverage), color='black', linestyle='dashed', label='AFL++')
    axMain.plot(*zip(*ff_coverage), color='black', linestyle='dotted', label='FairFuzz')
    box = axMain.get_position()
    axMain.set_position([box.x0, box.y0, box.width*0.8, box.height])
    axMain.legend(loc='center left', bbox_to_anchor=(1, 0.5))

    axMain.set_yscale('linear')
    axMain.set_ylim((bar, 105))
    #axMain.spines['bottom'].set_visible(False)
    axMain.xaxis.set_ticks_position('bottom')
    #axMain.xaxis.set_visible(False)

    divider = make_axes_locatable(axMain)
    axLin = divider.append_axes('bottom', size=1.1, pad=0.25, sharex=axMain)
    axLin.set_yscale('log')
    axLin.set_ylim((1e-10, bar))
    axLin.plot(*zip(*afl_coverage), color='black', linestyle='dashed', label='AFL++')
    axLin.plot(*zip(*ff_coverage), color='black', linestyle='dotted', label='FairFuzz')

    #axLin.spines['top'].set_visible(False)
    axLin.xaxis.set_ticks_position('bottom')
    axLin.set_xlabel('time (s)')
    plt.setp(axLin.get_yticklabels(), visible=True)
    #plt.show()
    plt.tight_layout()
    plt.savefig(f'{program}-rp-coverages.png', bbox_inches='tight')


if __name__ == "__main__":
    argument_parser = argparse.ArgumentParser('Rare-path coverage graphing')
    argument_parser.add_argument('program', help='the program being fuzzed')
    argument_parser.add_argument('afl', help='afl++ fuzzing directory')
    argument_parser.add_argument('ff', help='fairfuzz fuzzing directory')
    argument_parser.add_argument(
        '--log', help='path to log file', default='rp-cov.log')
    argument_parser.add_argument(
        '--debug', help='log debug output', default=False, action='store_true')
    args = argument_parser.parse_args()

    plot_coverages(args)
