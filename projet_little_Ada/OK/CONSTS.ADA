procedure Consts is 
   X : constant := 1234567;
   Y : constant := 1_234_567;
   Z : constant := 1_2_3_4_5_6_7;
   U : constant := 12_3.4_56;
   V : constant := 12e-2_345;
   W : constant := 1.234_5E1_2;
   A : constant := 10#1_234_567#;
   B : constant := 2#1001.0110#E-2;
   C : constant := 12#A02.b5#e-3;
   D : constant := 16#Dead_Beef#;
   
begin
   Put("ab""\b");
   Put("une chaine");
   Put("une autre ""chaine""");
end Consts;
