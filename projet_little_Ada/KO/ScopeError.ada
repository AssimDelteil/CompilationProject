procedure ScopeError is
   procedure G is
      X : Integer;
   begin
      null;
   end;
begin
   X := 0;
end;
