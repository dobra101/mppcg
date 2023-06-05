MACHINE Lift
VARIABLES level
INVARIANT level >= 0 & level <= 100
INITIALISATION level := 0
OPERATIONS
	inc = PRE level<100 THEN level := level + 1 END;
    dec = PRE level>0 THEN level := level - 1 END
END