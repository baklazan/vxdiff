import subprocess

produced_files = 0

def next_filename():
    global produced_files
    result = str(produced_files)
    produced_files += 1
    return result

def extract_file(commit, in_filename, out_filename):
    with open(out_filename, 'w') as outfile:
        subprocess.run(['git', 'cat-file', '-p', '{}:{}'.format(commit, in_filename)], stdout=outfile)

hashes = subprocess.run(['git', '--no-pager', 'log', '--pretty=format:%H'], capture_output=True).stdout.decode().split()

for new_hash, old_hash in zip(hashes[:-1], hashes[1:]):
    lines = subprocess.run(['git', 'diff', '--diff-filter=MR', '--name-status', old_hash, new_hash], capture_output=True).stdout.decode().split('\n')
    for line in lines:
        tokens = line.split()
        if len(tokens) == 0:
            continue
        if tokens[0][0] == 'M':
            out_basename = next_filename()
            extract_file(old_hash, tokens[1], out_basename + ".left")
            extract_file(new_hash, tokens[1], out_basename + ".right")
        elif tokens[0][0] == 'R':
            out_basename = next_filename()
            extract_file(old_hash, tokens[1], out_basename + ".left")
            extract_file(new_hash, tokens[2], out_basename + ".right")
        else:
            print("Unknown file type: {}".format(tokens[0]))
