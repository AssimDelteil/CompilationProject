procedure InitEx is

   Index_1, Index_2, Index_3    : INTEGER;
   This_Is_A_Long_Variable_Name : INTEGER;

   DOZEN  : constant INTEGER := 12;
   GROSS  : constant INTEGER := 12 * DOZEN;
   BIG_NO : constant         := 32_24_7;           -- This is 32247
   TWO    : constant         := BIG_NO - 3_22_45;  -- This is 2

begin

   Index_1 := GROSS;                    -- Index_1 is 144
   Index_2 := BIG_NO - TWO;             -- Index_2 is 32245
   Index_3 := TWO * GROSS;              -- Index_3 is 288
   This_Is_A_Long_Variable_Name := DOZEN * DOZEN - GROSS;
   Put("Index_1 = "); Put(Index_1); New_Line;
   Put("Index_2 = "); Put(Index_2); New_Line;
   Put("Index_3 = "); Put(Index_3); New_Line;
   Put("This_Is_A_Long_Variable_Name =");
   Put(This_Is_A_Long_Variable_Name); New_Line;

   Index_1 := 123E2;                    -- 12300
   Index_2 := 1_23e2;                   -- 12300
   Index_3 := 12_3e+2;                  -- 12300
   Index_1 := 2#10111#;                 -- Binary number
   Index_2 := 8#377#;                   -- Octal number
   Index_3 := 16#1FF#e1;                -- Hexadecimal number
   Index_1 := 12#A4#;                   -- Base 12 number

end InitEx;




-- Result of execution

-- Index_1 =         144
-- Index_2 =       32245
-- Index_3 =         288
-- This_Is_A_Long_Variable_Name =          0

