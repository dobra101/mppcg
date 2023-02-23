MACHINE Swap

VARIABLES  xx,yy

INVARIANT  xx : INT & yy : INT

INITIALISATION xx := 0 || yy:= 1

OPERATIONS
    swap = xx := yy || yy := xx;
    swap2 = xx,yy := yy,xx;
    swap3 = xx,yy := yy,xx || xx := yy;
    out <-- getXX = BEGIN out := xx END;
    out <-- getYY = BEGIN out := yy END

END