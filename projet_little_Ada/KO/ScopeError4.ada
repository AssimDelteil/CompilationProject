procedure ScopeError4 is
   procedure G is
      X : Integer;
      
      procedure H is 
	 X : Integer;
      begin
	 X := 0;
      end H;
      
      Y : Integer renames ScopeError4.G.H.X;
   begin
      null;
   end G;
   
begin
   null;
end ScopeError4;
