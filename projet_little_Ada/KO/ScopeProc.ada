procedure ScopeProc is
   procedure Sub is
      
      procedure F(X : Integer) is
      begin
	 null;
      end F;
      
   begin
      null;
   end Sub;
      
begin
   Sub.F(2);
end;
