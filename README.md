### Overview

`coop` allows you to pair-debug programs without having to share a screen and a keyboard.

Simply run `coop <program> <arg>...` to start a server running `<program> <arg>`
interactively, and attach additional clients with `coop`. Clients can come and go at any time.

Optionally specify an endpoint and port with `--endpoint` and `--port`. See `coop --help` for
more info.

### Building

    stack build

### Installing (local user)

    stack install

### Sample session

(You'll have to use your imagination to untangle the order these commands were input)

##### Server/Client 1

    $ coop --endpoint 10.0.0.14 --port 5678 gdb a.out
    Spawning 'gdb a.out'
    Subscribing to input on 'tcp://10.0.0.14:5678'
    Publishing input and output on 'tcp://10.0.0.14:5679'
    Press enter when client(s) are ready.

    ...
    Reading symbols from a.out...done.
    (gdb) [fjdt]: b main
    Breakpoint 1 at 0x40050e: file foo.c, line 4.
    (gdb) [fjdt]: r
    Starting program: /home/mitchell/a.out
    Breakpoint 1, main () at foo.c:4
    4	    printf("Hello, world!\n");
    (gdb) n
    5	    int a = 5;
    (gdb) n
    6	    a += 1;
    (gdb) n
    7	    a += 2;
    (gdb) n
    8	    a += 4;
    (gdb) [fjdt]: n
    9	    printf("a = %d\n", a);
    (gdb) [fjdt]: n
    10	    return 0;
    (gdb) [fjdt]: n
    11	}

##### Client 2

    $ coop --endpoint 10.0.0.14 --port 5678
    Publishing input on 'tcp://10.0.0.14:5678'
    Subscribing to output on 'tcp://10.0.0.14:5679'
    ...
    Reading symbols from a.out...done.
    (gdb) b main
    Breakpoint 1 at 0x40050e: file foo.c, line 4.
    (gdb) r
    Starting program: /home/mitchell/a.out 
    Breakpoint 1, main () at foo.c:4
    4	    printf("Hello, world!\n");
    (gdb) [gxoc]: n
    5	    int a = 5;
    (gdb) [gxoc]: n
    6	    a += 1;
    (gdb) [gxoc]: n
    7	    a += 2;
    (gdb) [gxoc]: n
    8	    a += 4;
    (gdb) n
    9	    printf("a = %d\n", a);
    (gdb) n
    10	    return 0;
    (gdb) n
    11	}

###### foo.c

```c
#include <stdio.h>

int main() {
    printf("Hello, world!\n");
    int a = 5;
    a += 1;
    a += 2;
    a += 4;
    printf("a = %d\n", a);
    return 0;
}
```
