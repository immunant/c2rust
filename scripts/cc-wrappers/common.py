import hashlib
import json
import os
import subprocess
import sys

def run(build_type):
    build_commands_dir = os.environ.get("BUILD_COMMANDS_DIRECTORY",
                                        "/tmp/build_commands")
    if not os.path.exists(build_commands_dir):
        os.mkdir(build_commands_dir)

    command = os.path.basename(sys.argv[0])
    build_info = {
        "type": build_type,
        "directory": os.getcwd(),
        "arguments": [command] + sys.argv[1:],
    }
    build_json = json.dumps(build_info, indent=4)

    # Hash the contents of the JSON file and use that as the file name
    # This is safe for concurrency, and guarantees that each unique
    # compilation gets an output file
    hm = hashlib.sha256()
    hm.update(build_json.encode("utf-8"))
    build_file_name = "%s.json" % hm.hexdigest()
    build_file = os.path.join(build_commands_dir, build_file_name)
    with open(build_file, 'w') as f:
        f.write(build_json)

    script_dir = os.path.dirname(os.path.realpath(__file__))
    b_arg = ["-B" + script_dir] if build_type == "cc" else []
    return subprocess.call([command] + b_arg + sys.argv[1:])
