procedure ScopeFunc is
   function F(X : Integer) return Integer is
   begin
      return X;
   end;
      
begin
   X := F(2);
end;
