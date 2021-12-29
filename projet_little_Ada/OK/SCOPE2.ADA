procedure Scope2 is

   Count, Index : INTEGER;

   procedure Level1 is
      Index, Count : INTEGER;

      procedure Level2 is
         Count : INTEGER;
      begin
         Count :=                             -- Count from line 10
                  Scope2.Count;               -- Count from line 4
      end Level2;

      procedure Level2_Prime is
         Data, Index, Count : INTEGER;
         Outer_Index : INTEGER renames Scope2.Level1.Index;
      begin

         Count := Index                        -- Count from line 17
                        + Scope2.Level1.Count; -- Count from line 7

         Index :=                              -- Index from line 17
                  Scope2.Level1.Index +        -- Index from line 7
                   Scope2.Index;               -- Index from line 4

         Index :=                              -- Index from line 17
                  Outer_Index +                -- Index from line 7
                   Scope2.Index;               -- Index from line 4

      end Level2_Prime;

   begin
      null;
   end Level1;

   procedure Other_Level1 is
   begin
      Count := Index;                         -- Both from line 4
   end Other_Level1;

begin
   Count := Index;                            -- Both from line 4
end Scope2;




-- Result of execution

-- (No output from this program)

