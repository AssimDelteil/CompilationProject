procedure BoolVars is

   Correct  : BOOLEAN;
   Maybe    : BOOLEAN;
   Probably : BOOLEAN;

begin

   Correct := TRUE;                        -- TRUE
   Maybe := FALSE;                         -- FALSE
   Probably := Correct or Maybe;           -- TRUE
   Probably := Correct and Maybe;          -- FALSE
   Probably := Correct xor Maybe;          -- TRUE
   Probably := Correct and not Maybe;      -- TRUE
   if Maybe < Correct then
      Put("FALSE is of less value than TRUE in a BOOLEAN variable");
      New_Line;
   end if;

   Put(Correct, 8);
   Put(Maybe, 8);
   New_Line;

end BoolVars;



