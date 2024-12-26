import argparse
import hashlib
import logging
import math
import numpy
import operator
import os

from pathlib import Path


class RPCoverageCalculator:
    paths = []
    total_weight = 0.0
    covered_weight = 0.0
    n_covered_paths = 0

    rp_coverage = [(0, 0.0)]
    branch_coverage = [(0, 0.0)]
    line_coverage = [(0, 0.0)]
    fault = (0.0, 0.0, 0.0, 0.0, 0.0)

    def __init__(self, args):
        try:
            paths_dir = args.paths_dir
            self.fuzzing_dir = args.fuzzing_dir
            self.program = args.program
            self.rp_only = args.rp_only
            self.path_fault = args.path_fault
            logging.basicConfig(
                level=logging.ERROR, #logging.DEBUG if args.debug else logging.INFO,
                format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
                handlers=[
                    logging.StreamHandler(),
                    logging.FileHandler(args.log)
                ]
            )
            self.logger = logging.getLogger('rp-cov')
        except AttributeError as e:
            print(f'Invalid args: {e}')
            return

        self._injest_program_paths(paths_dir)

        fuzzing_out_dir = os.path.join(self.fuzzing_dir, f'{self.program}_out/default')
        self._get_start_time(fuzzing_out_dir)

        traces_dir = os.path.join(self.fuzzing_dir, 'traces')
        self._calculate_rp_coverage(traces_dir)

        afl_coverage_dir = os.path.join(self.fuzzing_dir, f'{self.program}_out/cov')
        self._calculate_afl_coverage(afl_coverage_dir)

        rp_at_fault = -1 
        rp_change = -1
        line_at_fault = -1 
        line_change = -1
        branch_at_fault = -1
        branch_change = -1

        tolerance = 0.001

        while line_at_fault == -1 and branch_at_fault == -1:
            if rp_at_fault == -1:
                for i in range(1, len(self.rp_coverage)):
                    x = self.rp_coverage[i]
                    if abs(x[0] - self.fault[0]) < tolerance:
                        rp_at_fault = x[1]
                        prev = self.rp_coverage[i-1]
                        cov_change = x[1] - prev[1]
                        time_change = x[0] - prev[0]
                        rp_change = cov_change
                        break
            if line_at_fault == -1:
                for i in range(1, len(self.line_coverage)):
                    x = self.line_coverage[i]
                    if abs(x[0] - self.fault[0]) < tolerance:
                        line_at_fault = x[1]
                        prev = self.line_coverage[i-1]
                        cov_change = x[1] - prev[1]
                        time_change = x[0] - prev[0]
                        line_change = cov_change
                        break
            if branch_at_fault == -1:
                for i in range(1, len(self.branch_coverage)):
                    x = self.branch_coverage[i]
                    if abs(x[0] - self.fault[0]) < tolerance:
                        branch_at_fault = x[1]
                        prev = self.branch_coverage[i-1]
                        cov_change = x[1] - prev[1]
                        time_change = x[0] - prev[0]
                        branch_change = cov_change
                        break
            if rp_at_fault != 1 and self.rp_only:
                break
            tolerance += 0.001
            if tolerance > 0.5:
                break

        if line_at_fault == -1 and self.line_coverage[-1][0] < self.fault[0]:
            line_at_fault = self.line_coverage[-1][1]
            line_change = self.line_coverage[-1][1] - self.line_coverage[-2][1] 
        if branch_at_fault == -1 and self.branch_coverage[-1][0] < self.fault[0]:
            branch_at_fault = self.branch_coverage[-1][1]
            branch_change = self.branch_coverage[-1][1] - self.branch_coverage[-2][1] 

        self.coverage_stats = []
        self.coverage_stats.append((rp_at_fault, rp_change))
        self.coverage_stats.append((line_at_fault, line_change))
        self.coverage_stats.append((branch_at_fault, branch_change))

        self.dump_coverages(self.fuzzing_dir, latex=args.latex)

    def _injest_program_paths(self, paths_dir):
        for filename in os.listdir(paths_dir):
            file_path = os.path.join(paths_dir, filename)

            if not os.path.isfile(file_path): 
                # shouldn't happen...
                self.logger.debug(f'skipped injesting path file: {file_path}')
                continue

            with open(file_path, 'r') as f:
                prob = f.readline().strip()
                try:
                    prob = float(prob)
                except ValueError as e:
                    self.logger.error(f'invalid probability in {filename}: {prob}')
                    continue
                # The weight for each path is 1/p
                weight = 1 / prob
                path = [t.rstrip() for t in f.readlines()]

            # Track the path if it has not already been seen; this ensures
            # that all paths are unique.
            if (path, False, weight, filename) not in self.paths:
                self.logger.debug(f'New path ({filename}): weight={weight}')
                self.paths.append((path, False, weight, filename))
                self.total_weight += weight

        # sort paths by descending weight
        self.paths = sorted(self.paths, key=operator.itemgetter(2), reverse=True)
        self.logger.info(f'injested {len(self.paths)} paths; total weight={self.total_weight}')

    def _get_start_time(self, fuzzing_out_dir):
        with open(os.path.join(fuzzing_out_dir, 'fuzzer_stats')) as f:
            for line in f:
                if line.startswith('start_time'):
                    self.start_time = int(line.split(': ')[1].strip())
                    return
        self.logger.error('no start_time in fuzzer_stats')

    def md5(self, fname):
        hash_md5 = hashlib.md5()
        with open(fname, "rb") as f:
            for chunk in iter(lambda: f.read(4096), b""):
                hash_md5.update(chunk)
        return hash_md5.hexdigest()

    def _calculate_rp_coverage(self, traces_dir):
        rp_coverage = []
        seen_traces = []

        trace_files = sorted(Path(traces_dir).iterdir(), key=os.path.getmtime)
        for trace_file in trace_files:
            seen_fault = False

            mtime = os.path.getmtime(trace_file)
            if mtime < self.start_time:
                continue

            with open(trace_file, 'r') as f:
                trace = [line.rstrip() for line in f]
            if not trace:
                continue

            # No need to check a trace that has already been seen
            hash = self.md5(trace_file)
            if hash in seen_traces:
                self.logger.debug(f'ignoring duplicate: {trace_file}')
                continue
            seen_traces.append(hash)

            if 'FAULT' == trace[-1]:
                seen_fault = True
                trace = trace[:-1]
            #if 'FAULT2' == trace[-1] and 'FAULT' == trace[-2]:
            #    seen_fault = True
            #    trace = trace[:-2]
            #elif 'FAULT2' == trace[-1]:
            #    seen_fault = self.path_fault
            #    trace = trace[:-1]
            #elif 'FAULT' == trace[-1]:
            #    seen_fault = not self.path_fault
            #    trace = trace[:-1]

            for i in range(len(self.paths)):
                path, seen, weight, path_file = self.paths[i]
                if seen:
                    self.logger.debug(f'already seen path: {path_file}')
                    continue

                if not self._is_same_path(path, trace):
                    continue
                #print(f'matched {path_file}')

                self.n_covered_paths += 1
                self.paths[i] = (path, True, weight, path_file)
                self.covered_weight += weight
                self.rp_cov = self.covered_weight / self.total_weight

                mtime = os.path.getmtime(trace_file) - self.start_time
                rp_coverage.append((mtime, self.rp_cov * 100))

                if seen_fault:
                    self.logger.info(f'FAULT: {mtime}')
                    self.fault = (mtime, weight, self.rp_cov, 0.0, 0.0)
                    self.rp_coverage += rp_coverage
                    seen_fault = False
                    return
        self.rp_coverage += rp_coverage

    def _calculate_afl_coverage(self, coverage_dir):
        if self.rp_only:
            return
        fault_time, fault_weight, rp_cov, _, _ = self.fault
        if fault_time == 0.0:
            self.logger.error(f'No fault found')

        with open(os.path.join(coverage_dir, 'afl-cov.log'), 'r') as f:
            log_content = f.readlines()

        line_coverage = []
        branch_coverage = []

        timestamps = []
        branches = [0]
        lines = [0]
        timeline = []
        first = True

        for i in range(len(log_content)):
            l = log_content[i]
            if l.startswith('[+] AFL test case:'):
                if 'lines' not in log_content[i+1]:
                    continue
                #line_cov = float(log_content[i+1].split(' ')[5].strip('%'))
                lines = log_content[i+1].split(' of ')
                lines_covered = int(lines[0].split('(')[1]) - 11
                total_lines = int(lines[1].split(' ')[0]) - 13 # ignore fault detection code

                # if the fault was seen, subtract two lines from covered
                if self.fault[0] != 0.0:
                    lines_covered -= 2

                line_cov = lines_covered / total_lines * 100
                #branch_cov = float(log_content[i+3].split(' ')[5].strip('%'))
                branches = log_content[i+3].split(' of ')
                branches_covered = int(branches[0].split('(')[1])
                total_branches = int(branches[1].split(' ')[0]) - 1 # ignore fault detection code
                if self.fault[0] != 0.0:
                    branches_covered -= 1

                branch_cov = branches_covered / total_branches * 100
                queue_file = os.path.join(coverage_dir.replace('cov', 'default/queue'), l.split(' ')[4])
                timestamp = os.path.getmtime(queue_file) - self.start_time
                t = float(l.split(' ')[4].split(',')[2].split(':')[1]) / 1000
                if timestamp < 0 and abs(timestamp) < 0.1:
                    timestamp = 0.001
                elif timestamp < 0 or timestamp-t > 1:
                    # sometimes we see weird timestamps with test cases...
                    # ignoring it isn't the best solution, but it does prevent
                    # us from skipping real timestamps.
                    continue
                if not line_coverage or line_coverage[-1][1] < line_cov:
                    line_coverage.append((timestamp, line_cov))
                if not branch_coverage or branch_coverage[-1][1] < branch_cov:
                    branch_coverage.append((timestamp, branch_cov))

        self.line_coverage += line_coverage
        self.branch_coverage += branch_coverage


    def dump_coverages(self, fuzzing_dir, latex=False):
        filename = os.path.join(fuzzing_dir, f'{self.program}')
        with open(f'{filename}-fault-coverage.csv', 'w') as f:
            f.write(f'{self.program},{self.fault[0]},{self.fault[1]}')
            for cov, change in self.coverage_stats:
                f.write(f',{cov},{change}')
            f.write('\n')

        with open(f'{filename}-rp-cov.csv', 'w') as f:
            f.write('time,coverage\n')
            for ts, cov in self.rp_coverage:
                f.write(f'{ts},{cov}\n')
        with open(f'{filename}-line-cov.csv', 'w') as f:
            f.write('time,coverage\n')
            for ts, cov in self.line_coverage:
                f.write(f'{ts},{cov}\n')
        with open(f'{filename}-branch-cov.csv', 'w') as f:
            f.write('time,coverage\n')
            for ts, cov in self.branch_coverage:
                f.write(f'{ts},{cov}\n')

        if latex:
            tabular = ''
            if (1 / self.fault[1]) < 1e-5:
                tabular += '\\rowcolor{gray!20} '
            tabular += f'{self.program} '
            for cov, _ in self.coverage_stats:
                tabular += f' & {cov}'
            tabular += ' \\\\\n\\hline'
            print(tabular)
            
    def _extract_loop_bodies(self, loop_line, path):
        loop_bodies = []
        loop_body = []
        in_loop = False
        for node in path:
            _, line, tf = node.split(',')
            if not in_loop and line == loop_line and (tf == 'Enter' or tf == 'T'):
                # starting new iteration of the loop
                in_loop = True
                continue
            elif line == loop_line and (tf == 'End' or tf == 'Enter'):
                # finish iteration of the loop or continue
                in_loop = False
                #if loop_body:
                loop_bodies.append(loop_body)
                loop_body = []
                continue
            elif tf == 'Return':
                # exited the loop via return statement
                loop_bodies.append(loop_body)
                break
            elif line == loop_line and (tf == 'Exit' or tf == 'F'):
                # exited loop
                #if loop_body:
                loop_bodies.append(loop_body)
                break
            elif in_loop:
                loop_body.append(f'_,{line},{tf}')
            else:
                self.logger.debug(f'unexpected node: {node}')
        if in_loop: #and loop_body:
            loop_bodies.append(loop_body)
        return loop_bodies

    def _is_same_node(self, node1, node2):
        self.logger.debug(f'{node1} = {node2} ?')
        _, line1, tf1 = node1.split(',')
        _, line2, tf2 = node2.split(',')

        if tf1 == 'Return' and tf2 == 'Return':
            return True
        if line1 != line2:
            return False
        if tf1 == 'T' and tf2 == 'Enter':
            return True
        if tf1 == 'F' and tf2 == 'Exit':
            return True
        if tf1 == 'Enter' and tf2 == 'T':
            return True
        if tf1 == 'Exit' and tf2 == 'F':
            return True
        return tf1 == tf2

    def _is_same_path(self, path, trace):
        if len(trace) < len(path):
            return False

        end = ''
        in_loop = False
        for i in range(len(path)):
            if len(trace) < len(path):
                return False

            # If the current node in the trace is in the body of a loop, continue
            # until the loop has terminated.
            if in_loop:
                if path[i] != end and 'Return' not in path[i]:
                    self.logger.debug(f'skipping {path[i]}')
                    continue
                self.logger.debug(f'exited loop: {path[i]}')
                in_loop = False
                continue
            try:
                path_node = path[i]
                trace_node = trace[i]
            except IndexError:
                self.logger.error(f'Invalid index: {i}')
            try:
                path_file, path_line, _ = path_node.split(',')
                trace_file, trace_line, trace_tf = trace_node.split(',')
            except Exception as e:
                self.logger.error(f'{e}: {path_node}, {trace_node}')
                continue

            if trace_tf == 'Enter' and self._is_same_node(path_node, trace_node):
                trace_bodies = self._extract_loop_bodies(trace_line, trace[i:])
                loop_bodies = self._extract_loop_bodies(path_line, path[i:])

                # Get all of the nodes that occur after the loop exits
                trace_post_loop = []
                for j in range(i, len(trace)):
                    if 'Return' not in trace[j] and trace[j].replace(trace_file, path_file) != ','.join([path_file, path_line, 'Exit']):
                        continue
                    trace_post_loop += trace[j:]
                    break

                for loop_body in trace_bodies:
                    if loop_body in loop_bodies:
                        # Entered a loop; continue until we see the end of the
                        # loop in the path
                        in_loop = True
                        end = ','.join([path_file, path_line, 'F'])

                        # Update the trace to remove all loop iterations except
                        # for the one that matches the path. Append all of the
                        # trace nodes that occur after the loop exits.
                        trace = trace[:i] + [trace_node] + \
                            loop_body + trace_post_loop
                        break

                if not in_loop:
                    return False

            if not self._is_same_node(path_node, trace_node):
                self.logger.debug('not a match')
                return False
        return True


if __name__ == "__main__":
    argument_parser = argparse.ArgumentParser('Rare-path coverage calculator')
    argument_parser.add_argument('paths_dir', help='rare path files directory')
    argument_parser.add_argument('program', help='the program being fuzzed')
    argument_parser.add_argument('fuzzing_dir', help='fuzzing directory')
    argument_parser.add_argument(
        '--log', help='path to log file', default='rp-cov.log')
    argument_parser.add_argument(
        '--debug', help='log debug output', default=False, action='store_true')
    argument_parser.add_argument('--latex', default=False, action='store_true')
    argument_parser.add_argument('--rp-only', default=False, action='store_true')
    argument_parser.add_argument('--path-fault', default=False, action='store_true')

    args = argument_parser.parse_args()

    rp_cov = RPCoverageCalculator(args)
