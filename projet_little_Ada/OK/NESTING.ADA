procedure Nesting is

   procedure Triple is

         procedure Second_Layer is

               procedure Bottom_Layer is
               begin
                  Put_Line("This is the Bottom Layer talking.");
               end Bottom_Layer;

         begin
            Put_Line("This is the Second Layer talking.");
            Bottom_Layer;
            Put_Line("We are back up to the Second Layer.");
         end Second_Layer;

   begin
      Put_Line("This is procedure Triple talking to you.");
      Second_Layer;
      Put_Line("We are back up to the procedure named Triple.");
   end Triple;

begin
   Put_Line("Start the triple nesting here.");
   Triple;
   Put_Line("Finished, and back to the top level.");
end Nesting;




-- Result of execution

-- Start the triple nesting here.
-- This is procedure Triple talking to you.
-- This is the Second Layer talking.
-- This is the Bottom Layer talking.
-- We are back up to the Second Layer.
-- We are back up to the procedure named Triple.
-- Finished, and back to the top level.

