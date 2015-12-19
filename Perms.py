#!/usr/bin/python

import stat

perms_digit = {'0': {'0': False, '1': stat.S_ISVTX, '2': stat.S_ISGID, '3': stat.S_ISVTX | stat.S_ISGID, '4': stat.S_ISUID, '5': stat.S_ISVTX | stat.S_ISUID, '6':stat.S_ISGID | stat.S_ISUID , '7': stat.S_ISVTX | stat.S_ISGID | stat.S_ISUID,}, '1': {'0': False, '1': stat.S_IXUSR, '2': stat.S_IWUSR, '3': stat.S_IWUSR | stat.S_IXUSR,  '4': stat.S_IRUSR, '5': stat.S_IRUSR | stat.S_IXUSR, '6': stat.S_IRUSR | stat.S_IWUSR, '7': stat.S_IRUSR | stat.S_IWUSR | stat.S_IXUSR,}, '2': {'0': False, '1': stat.S_IXGRP, '2': stat.S_IWGRP, '3': stat.S_IWGRP | stat.S_IXGRP, '4': stat.S_IRGRP, '5': stat.S_IRGRP | stat.S_IXGRP, '6': stat.S_IRGRP | stat.S_IWGRP, '7': stat.S_IRGRP | stat.S_IWGRP | stat.S_IXGRP,}, '3': {'0': False, '1': stat.S_IXOTH, '2': stat.S_IWOTH, '3': stat.S_IWOTH | stat.S_IXOTH, '4': stat.S_IROTH, '5': stat.S_IROTH | stat.S_IXOTH, '6': stat.S_IROTH | stat.S_IWOTH, '7': stat.S_IROTH | stat.S_IWOTH | stat.S_IXOTH,},}

perms_char = {'0': {'r': stat.S_IRUSR, '-':  False,}, '1': {'w': stat.S_IWUSR, '-': False,}, '2': {'x': stat.S_IXUSR, 'S': stat.S_ISUID, 's': stat.S_ISUID | stat.S_IXUSR, '-': False,}, '3': {'r': stat.S_IRGRP, '-': False,}, '4': {'w': stat.S_IWGRP, '-': False,}, '5': {'-': False, 'x': stat.S_IXGRP, 'S': stat.S_ISGID, 's': stat.S_ISGID | stat.S_IXGRP,}, '6': {'r': stat.S_IROTH, '-': False,}, '7': {'w': stat.S_IWOTH, '-': False,}, '8': {'x': stat.S_IXOTH, 'T': stat.S_ISVTX, 't': stat.S_ISVTX | stat.S_IXOTH, '-': False,},} 

def convertPerms(perm):
    from re import search
    idx = 0
    perms_spec = False

    if search(r'^[\-rwxTtsS]{9,10}$', perm):
        letters = list(perm)
        for i in letters:
            if perms_spec:
                if perms_char[str(idx)][i]:
                    perms_spec = perms_spec | perms_char[str(idx)][i]
            else:
                perms_spec = perms_char[str(idx)][i]
            idx += 1
    elif search(r'^[0-7]{4}$', perm):
        digits = list(perm)
        for i in digits:
            if perms_spec:
                if perms_digit[str(idx)][i]:
                    perms_spec = perms_spec | perms_digit[str(idx)][i]
            else:
                perms_spec = perms_digit[str(idx)][i]
            idx += 1
    else:
        raise ValueError(perm)
    return perms_spec

if __name__ == '__main__':
    import sys
    print convertPerms(sys.argv[1])
