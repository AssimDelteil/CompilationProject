procedure Funct is

   Twelve : INTEGER := 12;
   Sum    : INTEGER;
                            -- This is a function specification
   function Square(Val : INTEGER) return INTEGER;

                            -- This is a function body
   function Square(Val : INTEGER) return INTEGER is
   begin
      return Val * Val;
   end Square;

   function Sum_Of_Numbers(Val1, Val2 : INTEGER) return INTEGER is
   begin
      return Val1 + Val2;
   end Sum_Of_Numbers;

begin
   Put("The square of 12 is");
   Put(Square(Twelve), 4);
   New_line;
   Sum := Sum_Of_Numbers(Twelve, 12);
   Put("The sum of 12 and 12 is");
   Put(Sum, 4);
   New_Line;
end Funct;




-- Result of execution

-- The square of 12 is 144
-- The sum of 12 and 12 is  24

