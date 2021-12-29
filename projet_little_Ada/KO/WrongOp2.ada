procedure WrongOp2 is
begin
   loop exit when 1 <= 2 and 3 < 1 or 2 = 2; end loop;
end;
