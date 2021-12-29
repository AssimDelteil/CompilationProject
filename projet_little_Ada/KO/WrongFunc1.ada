procedure WrongFunc1 is
   function Failed_Identity(X : in out Integer) is
   begin
      return X;
   end;
       
begin
   null;
end;
