procedure Main is 
   X : Integer;
   
   procedure Child1 is 
      X : Integer := 1;
   begin
      null;
   end Child1;
   
   procedure Child2 is
      X : Integer := 2;
      
      procedure Child2_1 is 
        X : Integer := 21;
        Y : Integer renames Main.Child2.X;
      begin
	 Main.X := X + Main.Child2.X + Y;
      end Child2_1;

   begin
      Child2_1;
   end Child2;
      
begin
   Child2;
   Put(X);
end;
