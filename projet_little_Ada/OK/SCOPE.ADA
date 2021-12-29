procedure Scope is

   Count : INTEGER;

   procedure Level1 is
      Index : INTEGER;

      procedure Level2 is
         Count : INTEGER;
      begin
         null;
      end Level2;

      procedure Level2_Prime is
         Data : INTEGER;
      begin
         null;
      end Level2_Prime;

   begin
      null;
   end Level1;

   procedure Other_Level1 is
   begin
      null;
   end Other_Level1;

begin
   null;
end Scope;




-- Result of execution

--   (No output from this program.)

