# interp.py

import sys

from libinterp import Execute, Interp

if __name__ == "__main__":
    if len(sys.argv) is 1:
        Interp()

    elif len(sys.argv) is 2:
        Execute(sys.argv[1])

    else:
        print("Usage: interp.py          進入交互模式")
        print("Usage: interp.py main.l   執行代碼文件")