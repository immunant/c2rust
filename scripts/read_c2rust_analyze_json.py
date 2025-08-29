from collections import defaultdict
import json
from pprint import pprint
import sys

all_perms = defaultdict(dict)

for line in sys.stdin:
    if not line.startswith('JSON:'):
        continue
    json_str = line[len('JSON:'):]
    j = json.loads(json_str)

    func_name = j['func']
    var_name = j['var']
    perms = j['perms']
    all_perms[func_name][var_name] = perms

pprint(all_perms)
