### Overview

`coop-server` and `coop-client` allow you to pair-debug programs without having to share a screen and a keyboard.

### Building

    stack build

### Installing

    stack install

### Usage

Start a `coop-server`:

    $ coop-server gdb a.out
    Spawning 'gdb a.out'
    Subscribing to input on 'tcp://127.0.0.1:14448'
    Publishing input and output on 'tcp://127.0.0.1:14449'
    Press enter when client(s) are ready.

And any number of clients:

    $ coop-client
    Publishing input on 'tcp://127.0.0.1:14448'
    Subscribing to output on 'tcp://127.0.0.1:14449'

Now, all of `gdb a.out`'s output on stdout and stderr will be sent to every client.
Input from all clients will also be sent to each other, preceded by a 4-byte randomly
generated identifier. Clients can connect and disconnect at any time.

### Sample session

(You'll have to use your imagination to untangle the order these commands were input)

##### foo.c

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

##### Server

    $ coop-server
    Spawning 'gdb a.out'...
    Subscribing to input on 'tcp://127.0.0.1:14448'
    Publishing input and output on 'tcp://127.0.0.1:14449'
    Press enter when client(s) are ready.

##### Client 1

    $ coop-client
    Publishing input on 'tcp://127.0.0.1:14448'
    Subscribing to output on 'tcp://127.0.0.1:14449'
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

    $ coop-client
    Publishing input on 'tcp://127.0.0.1:14448'
    Subscribing to output on 'tcp://127.0.0.1:14449'
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
