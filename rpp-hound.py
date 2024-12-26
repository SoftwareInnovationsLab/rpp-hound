#!/usr/bin/python3

import argparse
import os
import resource
import subprocess
import time

joern_path = os.path.join(os.environ['HOME'], 'bin/joern/joern-cli/joern')

def sizeof_fmt(num, suffix="B"):
    for unit in ("Ki", "Mi", "Gi", "Ti", "Pi", "Ei", "Zi"):
        if abs(num) < 1024.0:
            return f"{num:3.1f}{unit}{suffix}"
        num /= 1024.0
    return f"{num:.1f}Yi{suffix}"

def print_rusage(rusage, start, end):
    rusage = resource.getrusage(resource.RUSAGE_CHILDREN)
    print(f'Memory: {sizeof_fmt(rusage.ru_maxrss)}')
    print(f'Total time: {end - start} seconds')

def run_analysis(args):
    cmd = f'{joern_path} --script analysis.scala ' \
          f'--param cpgFile={args.cpg} ' \
          f'--param verbose={args.verbose} ' \
          f'--param outFile={args.outfile} ' \
          f'--param shouldFold={args.no_folding} ' \
          f'--param onlyReachable={args.no_reachability} ' \
          f'--param stdOut={args.stdout} ' \
          f'--command exec'
    start = time.time()
    proc = subprocess.Popen(cmd.split(' '), env={'LD_LIBRARY_PATH': '/usr/local/lib'})
    proc.wait()
    end = time.time()
    rusage = resource.getrusage(resource.RUSAGE_CHILDREN)
    print_rusage(rusage, start, end)

def extract_paths(args):
    with open('/dev/null', 'wb') as null:
        cmd = f'swipl --table-space={args.table}G --stack-limit={args.stack}G -l {args.facts}'
        if args.method:
            find_cmd = f'find_rare_paths({args.n},"{args.method}",Total).'
        else:
            find_cmd = f'find_rare_paths({args.n},Total).'
        swipl_cmd = '\n'.join([
            f'assert(output_dir("{args.out_dir}/")).',
            f'assert(depth_limit({args.depth})).',
            '[coverage].' if args.coverage else '[cfg].',
            find_cmd,
            'halt.\n'
        ])
        start = time.time()
        proc = subprocess.Popen(cmd.split(' '), stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        output = proc.communicate(input=swipl_cmd.encode())
        print(f'Process stdout:\n{output[0].decode()}\n')
        print(f'Process stderr:\n{output[1].decode()}\n')
        proc.wait()
        end = time.time()
        rusage = resource.getrusage(resource.RUSAGE_CHILDREN)
        print_rusage(rusage, start, end)
    print('done')

if __name__ =='__main__':
    parser = argparse.ArgumentParser('analysis', formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    subparsers = parser.add_subparsers(required=True, help='sub-command help')

    rt_parser = subparsers.add_parser('rt', help='relational translator', formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    rt_parser.add_argument('cpg', help='path to cpg file')
    rt_parser.add_argument('--outfile', default='facts.pl', help='path to output file')
    rt_parser.add_argument('--stdout', action='store_true', default=False, help='write facts to standard out')
    rt_parser.add_argument('-v', '--verbose', default=False, action='store_true', help='write verbose output')
    rt_parser.add_argument('--no-folding', help='turn off folding optimization', default=True, action='store_false')
    rt_parser.add_argument('--no-reachability', help='turns off user-input reachability optimization', default=True, action='store_false')
    rt_parser.set_defaults(func=run_analysis)

    paths_parser = subparsers.add_parser('paths', help='identify rare paths', formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    paths_parser.add_argument('facts', help='path to facts file')
    paths_parser.add_argument('-d', '--depth', help='path depth limit', default=50)
    paths_parser.add_argument('-n', help='number of rare paths to calculate', default=10)
    paths_parser.add_argument('-m', '--method', help='starting method', type=str, default=None)
    paths_parser.add_argument('-o', '--out-dir', help='path to output directory', default='paths')
    paths_parser.add_argument('--table', help='size of SWIPL table (in GB)', default=50)
    paths_parser.add_argument('--stack', help='size of SWIPL stack (in GB)', default=4)
    paths_parser.add_argument('--coverage', help='output paths in coverage format', default=True, action='store_false')
    paths_parser.set_defaults(func=extract_paths)

    args = parser.parse_args()
    args.func(args)
