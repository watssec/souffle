import argparse
import os
import re
import shutil
import subprocess
import sys
import tempfile


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--input_dir")
    parser.add_argument("--output_dir")
    parser.add_argument("--cxx")
    parser.add_argument("--souffle")
    parser.add_argument("--driver")
    parser.add_argument("--include")
    parser.add_argument("--namespace", action="store_true")
    parser.set_defaults(namespace=False)
    parser.add_argument("dls", nargs="+")

    args = parser.parse_args()
    cxx = args.cxx
    souffle = args.souffle
    driver = args.driver
    include = args.include
    dls = args.dls
    output_dir = args.output_dir

    def cpp_file(dl):
        return re.sub(r"dl$", "cpp", dl)

    def object_file(dl):
        return re.sub(r"dl$", "o", dl)

    os.chdir(args.input_dir)

    if not os.path.isdir(output_dir):
        if os.path.exists(output_dir):
            os.remove(output_dir)
        os.mkdir(output_dir)

    for f in [driver] + dls:
        shutil.copy(f, output_dir)

    os.chdir(output_dir)
    redirects = dict(stdout=sys.stdout, stderr=sys.stderr)
    ns_index = 0
    for dl in dls:
        ns_index += 1
        ns_args = ["-N", "ns" + str(ns_index)] if args.namespace else []
        subprocess.run([souffle] + ns_args + ["-g", cpp_file(dl), dl], **redirects)
    cxx_args = [
        cxx,
        "-I",
        include,
        "--std=c++17",
        "-D__EMBEDDED_SOUFFLE__=1",
    ]
    for dl in dls:
        subprocess.run(
            cxx_args
            + [
                "-c",
                cpp_file(dl),
                "-o",
                object_file(dl),
            ],
            **redirects
        )
    os.execvp(cxx, cxx_args + [driver] + [object_file(dl) for dl in dls])


if __name__ == "__main__":
    main()
