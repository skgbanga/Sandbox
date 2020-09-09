import sys


def print_help():
    print('Usage: ./tool -f1 -f2 -f3 -o1 xyz -o2 abc -o3 dhj x y z')


if __name__ == "__main__":
    FLAGS = ("f1", "f2", "f3")
    flag1 = False
    flag2 = False
    flag3 = False

    OPTIONS = ("o1", "o2", "o3")
    option1 = None
    option2 = None
    option3 = None

    args = sys.argv
    argc = len(args)

    if argc == 1:
        print_help()
        sys.exit(1)

    for arg in args:
        if arg in ("-h", "--help"):
            print_help()
            sys.exit(1)

    for arg in args:
        if arg in ("-f1", "--f1"):
            flag1 = True
        if arg in ("-f2", "--f2"):
            flag2 = True
        if arg in ("-f3", "--f3"):
            flag3 = True

    if not (flag1 and flag2 and flag3):  # all three flags should be present
        print_help()
        sys.exit(1)

    for idx, arg in enumerate(args):
        if arg in ("-o1", "--o1"):
            if idx == argc - 1:
                raise ValueError("option o1 doesn't have an argument")
            option1 = args[idx + 1]
        if arg in ("-o2", "--o2"):
            if idx == argc - 1:
                raise ValueError("option o2 doesn't have an argument")
            option2 = args[idx + 1]
        if arg in ("-o3", "--o3"):
            if idx == argc - 1:
                raise ValueError("option o3 doesn't have an argument")
            option3 = args[idx + 1]

    if option1 is None or option2 is None or option3 is None:
        print_help()
        sys.exit(1)

    remaining = []
    for arg in reversed(args):
        if arg.startswith('-'):
            if arg in ('-o1', '--o1', '-o2', '--o2', '-o3', '--o3'):
                remaining.pop()
            break
        else:
            remaining.append(arg)
    remaining.reverse()

    print('Flags', flag1, flag2, flag3)
    print('Options', option1, option2, option3)
    print('Remaining', remaining)
