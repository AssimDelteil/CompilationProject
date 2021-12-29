procedure nocaps is

begin
   for index in 1..7 loop  -- this contains two simple if statements
      put("index is ");
      put(index, 3);
      if index < 4 then
         put(" so is less than 4");
      end if;
      if index > 5 then
         put(" so is more than 5");
      end if;
      new_line;
   end loop;
   new_line;

   for index in 13..17 loop  -- this contains an else clause
      put("index is");
      put(index, 3);
      if index < 15 then
         put_line(" and is less than 15.");
      else
         put_line(" and is 15 or greater.");
      end if;
   end loop;
   new_line;

   for index in 13..17 loop  -- this introduces the elsif statement
      put("index is");
      put(index, 3);
      if index < 15 then
         put_line(" and is less than 15.");
      elsif index = 15 then
         put_line(" and is 15.");
      elsif index = 16 then
         put_line(" and is 16.");
      else
         put_line(" and is greater than 16.");
      end if;
   end loop;
   new_line;

-- this final group of statements contains a loop with a nested if
--   statement, and a loop within the the else part of the nested
--   if statement.

   for index in 13..17 loop
      put("index is");
      put(index, 3);
      if index < 16 then
         if index > 14 then
            put(" and is less than 16 and greater than 14.");
         else
            put(" and is less than or equal to 14.");
         end if;
      else
         put(" and is 16 or greater.");
         for new_index in 222..224 loop
           put(" stutter");
         end loop;
      end if;
      new_line;
   end loop;

end nocaps;
