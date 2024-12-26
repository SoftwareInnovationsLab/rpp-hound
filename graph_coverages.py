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


def plot_coverage(program, coverage_dir, max_time=60):
    with open(os.path.join(coverage_dir, f'{program}-rp-cov.csv'), 'r') as f:
        f.readline()
        rp_coverage = [x.strip().split(',') for x in f.readlines()]

    with open(os.path.join(coverage_dir, f'{program}-line-cov.csv'), 'r') as f:
        f.readline()
        line_coverage = [x.strip().split(',') for x in f.readlines()]

    with open(os.path.join(coverage_dir, f'{program}-branch-cov.csv'), 'r') as f:
        f.readline()
        branch_coverage = [x.strip().split(',') for x in f.readlines()]

    rp_coverage = [(float(ts), float(cov)) for (ts, cov) in rp_coverage]
    line_coverage = [(float(ts), float(cov)) for (ts, cov) in line_coverage]
    branch_coverage = [(float(ts), float(cov)) for (ts, cov) in branch_coverage]
    if rp_coverage[-1][0] < max_time:
        rp_coverage.append((max_time, rp_coverage[-1][1]))
    if line_coverage[-1][0] < max_time:
        line_coverage.append((max_time, line_coverage[-1][1]))
    if branch_coverage[-1][0] < max_time:
        branch_coverage.append((max_time, branch_coverage[-1][1]))

    #foo = min([float(ts) for (ts, cov) in rp_coverage[1:] if float(cov) > 0])
    #line_coverage = [(ts + foo, cov) for (ts, cov) in line_coverage]
    #branch_coverage = [(ts + foo, cov) for (ts, cov) in branch_coverage]
    #rp_coverage = [(ts + foo, cov) for (ts, cov) in rp_coverage]

    rp_coverage = sorted(set(rp_coverage))
    line_coverage = sorted(set(line_coverage))
    branch_coverage = sorted(set(branch_coverage))

    while True:
        original_len = len(rp_coverage)
        for i in range(len(rp_coverage)-1):
            if rp_coverage[i+1][0] - rp_coverage[i][0] > 2:
                rp_coverage.insert(i+1, (rp_coverage[i][0]+1, rp_coverage[i][1]))
                break
        if len(rp_coverage) == original_len:
            break

    while True:
        original_len = len(line_coverage)
        for i in range(len(line_coverage)-1):
            if line_coverage[i+1][0] - line_coverage[i][0] > 2:
                line_coverage.insert(i+1, (line_coverage[i][0]+1, line_coverage[i][1]))
                break
        if len(line_coverage) == original_len:
            break

    while True:
        original_len = len(branch_coverage)
        for i in range(len(branch_coverage)-1):
            if branch_coverage[i+1][0] - branch_coverage[i][0] > 2:
                branch_coverage.insert(i+1, (branch_coverage[i][0]+1, branch_coverage[i][1]))
                break
        if len(branch_coverage) == original_len:
            break

    bar = 10

    _, axMain = plt.subplots(1)
    axMain.set_xlim((-1, max_time))
    #axMain.set_title(f'{program.split("-")[0]} fuzzing coverages')
    axMain.set_ylabel('coverage (%)')
    axMain.plot(*zip(*rp_coverage), color='black', linestyle='dotted', label='rare-path')
    axMain.plot(*zip(*line_coverage), color='black', linestyle='solid', label='line')
    axMain.plot(*zip(*branch_coverage), color='black', linestyle='dashed', label='branch')
    box = axMain.get_position()
    axMain.set_position([box.x0, box.y0, box.width*0.8, box.height])
    axMain.legend(loc='center left', bbox_to_anchor=(1, 0.5))

    axMain.set_yscale('linear')
    axMain.set_ylim((bar, 105))
    #axMain.spines['bottom'].set_visible(False)
    axMain.xaxis.set_ticks_position('bottom')
    plt.yticks(numpy.arange(bar, 105, 10))
    #axMain.xaxis.set_visible(False)

    #line_coverage = [(ts - foo, cov) for (ts, cov) in line_coverage]
    #branch_coverage = [(ts - foo, cov) for (ts, cov) in branch_coverage]
    #rp_coverage = [(ts - foo, cov) for (ts, cov) in rp_coverage]

    divider = make_axes_locatable(axMain)
    axLin = divider.append_axes('bottom', size=1.1, pad=0.25, sharex=axMain)
    axLin.set_yscale('log')
    #axLin.set_ylim((min([cov for (_, cov) in rp_coverage if cov > 0]), bar))
    axLin.set_ylim((1e-10, bar))
    axLin.plot(*zip(*rp_coverage), color='black', linestyle='dotted', label='rare-path')
    axLin.plot(*zip(*line_coverage), color='black', linestyle='solid', label='line')
    axLin.plot(*zip(*branch_coverage), color='black', linestyle='dashed', label='branch')

    #axLin.spines['top'].set_visible(False)
    axLin.xaxis.set_ticks_position('bottom')
    axLin.set_xlabel('time (s)')
    plt.setp(axLin.get_yticklabels(), visible=True)
    #plt.show()
    plt.tight_layout()
    plt.savefig(f'{program}-coverages.png', bbox_inches='tight')


def plot_coverage_change(fault_analysis_dir):
    coverage_changes = []
    i = 1
    for filename in sorted(os.listdir(fault_analysis_dir)):
        full_path = os.path.join(fault_analysis_dir, filename)
        with open(full_path, 'r') as f:
            f.readline()
            x = f.readline().strip().split(',')
            rp, line, branch = x
            coverage_changes.append((float(rp), float(line), float(branch)))
            rp, line, branch = f.readline().strip().split(',')
            print(f'{i} & {rp} & {line} & {branch} \\\\\n\\hline')
        i += 1

    indecies = [i for i in range(1, len(coverage_changes)+1)]

    ax = plt.subplot(111)
    w = 0.25
    ax.bar([i-w for i in indecies], [rp for (rp, _, _) in coverage_changes],
           width=w, color='g', align='center', label='rare-path')
    ax.bar(indecies, [l for (_, l, _) in coverage_changes],
           width=w, color='r', align='center', label='line')
    ax.bar([i+w for i in indecies], [b for (_, _, b) in coverage_changes],
           width=w, color='b', align='center', label='branch')
    ax.set_ylim(top=100, auto=True)
    ax.set_xlabel('Bug')
    ax.set_xlim((0, max(indecies)+1))
    plt.xticks(indecies)
    ax.set_ylabel('Δ coverage')
    ax.set_yscale('log')
    box = ax.get_position()
    ax.set_position([box.x0, box.y0, box.width*0.8, box.height])
    ax.legend(loc='center left', bbox_to_anchor=(1, 0.5))
    #plt.show()
    plt.savefig('fault_analysis.png')


if __name__ == "__main__":
    argument_parser = argparse.ArgumentParser('Rare-path coverage calculator')
    argument_parser.add_argument('program', help='the program being fuzzed')
    argument_parser.add_argument('fuzzing_dir', help='fuzzing directory')
    argument_parser.add_argument(
        '--log', help='path to log file', default='rp-cov.log')
    argument_parser.add_argument(
        '--debug', help='log debug output', default=False, action='store_true')
    argument_parser.add_argument('--fault', default=False, action='store_true')
    args = argument_parser.parse_args()

    if args.fault:
        plot_coverage_change('fault_analysis')
    else:
        plot_coverage(args.program, args.fuzzing_dir)
