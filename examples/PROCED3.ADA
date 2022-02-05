procedure Proced3 is

   Dogs : INTEGER;
   Cats : INTEGER;
   Animals : INTEGER;
                             -- This is a procedure specification
   procedure Total_Number_Of_Animals(Variety1 : in     INTEGER;
                                     Variety2 : in     INTEGER;
                                     Total    :    out INTEGER);

                             -- This is a procedure body
   procedure Total_Number_Of_Animals(Variety1 : in     INTEGER;
                                     Variety2 : in     INTEGER;
                                     Total    :    out INTEGER) is
   begin
      Total := Variety1 + Variety2;
   end Total_Number_Of_Animals;

begin
   Dogs := 3;
   Cats := 4;
   Total_Number_Of_Animals(Dogs, Cats, Animals);
   Put("The total number of animals is");
   Put(Animals, 3);
   New_Line;
end Proced3;




-- Result of execution

-- The total number of animals is  7

