procedure SubTypes is

   type MY_INT_TYPE is range -10_000..20_000;
   My_Int : MY_INT_TYPE;

   subtype SUB_INT is INTEGER range 12..144;
   Thing       : SUB_INT;
   Count       : INTEGER;
   Stuff       : INTEGER range 12..144;

   START       : constant := 4;                       -- START is 4
   STOP        : constant := START + 13;              -- STOP is 17
   Example     : SUB_INT range 4*START..2*STOP + 7;   -- 16..41
   Example2    : MY_INT_TYPE range START..2*STOP + 7; -- 4..41

begin

   Thing := 6 * 3;                        -- Thing is 18
   Count := Thing + 17;                   -- Count is 35
   Stuff := Count - Thing;                -- Stuff is 17

   My_Int := 6 * 3;                       -- My_Int is 18
   My_Int := MY_INT_TYPE(Thing + 17);     -- My_Int is 35
   My_Int := MY_INT_TYPE(Thing) + 17;     -- My_Int is 35

   My_Int := MY_INT_TYPE(Thing) - 10;     -- My_Int is 8
   My_Int := MY_INT_TYPE(Thing - 10);     -- Run Time Error

   My_Int := 35;
   Thing  := SUB_INT(My_Int - 17);        -- Thing is 18
   Thing  := SUB_INT(My_Int - 27);        -- Run Time Error

   Example := Thing + SUB_INT(My_Int);

end SubTypes;




-- Result of execution

-- Exception never handled: constraint_error
-- Value 8 out of range 12..144.

