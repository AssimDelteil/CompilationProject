procedure ScopeError3 is
   procedure G is
      X : Integer;
   begin
      null;
   end;
   procedure H is 
   begin
      ScopeError3.G.X := 0;
   end;
begin
   null;
end;
