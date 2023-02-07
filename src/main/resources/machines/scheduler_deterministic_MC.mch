MACHINE scheduler_deterministic_MC

SETS
	PID  = {process1,process2,process3}

VARIABLES active, ready, waiting

DEFINITIONS
    ASSERT_LTL == "G([new]=> X e(del))";        /* This formula is true */

INVARIANT  active : POW(PID) & ready : POW(PID) & waiting: POW(PID) & /* the types */
           /* and now the rest of the invariant */
           active <: PID &
           ready <: PID   &
           waiting <: PID &
           (ready /\ waiting) = {} &
           active /\ (ready \/ waiting) = {} &
           card(active) <= 1
              
INITIALISATION  
	active := {}; ready := {}; waiting := {}
	
OPERATIONS

new(pp) =
	SELECT
		pp : PID  &
		pp /: active &
		pp /: (ready \/ waiting) 
	THEN
		waiting := (waiting \/ { pp })
	END

END