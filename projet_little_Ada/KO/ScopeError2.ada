procedure ScopeError2 is
   procedure G is
      X : Integer;
   begin
      null;
   end;
begin
   ScopeError2.G.X := 0;
end;
