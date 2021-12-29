procedure WrongCase2 is
   X : Integer := 0;
begin
   case X is 
      when 
	0 |
	1 |
	2 |
	3 |
	=> null;
      when others => null;
   end case;
end;
