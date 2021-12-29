procedure WrongCall is
   procedure Child is
   begin
      null;
   end;
   
begin
   Child();
end;
