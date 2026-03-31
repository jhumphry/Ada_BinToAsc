-- Storage_Array_To_Hex_String
-- This is a simple conversion routine for test or example routines

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with System.Storage_Elements;

function Storage_Array_To_Hex_String(X : System.Storage_Elements.Storage_Array)
                                 return String is
   use System.Storage_Elements;

   Hex_Digits : constant String := "0123456789ABCDEF";
   Output : String(1 .. X'Length * 3);
   Byte : Storage_Element;

begin

   -- This compile-time check is useful for GNAT, but in GNATprove it currently
   -- just generates a warning that it can not yet be proved correct.
   pragma Warnings (GNATprove, Off, "Compile_Time_Error");
   pragma Compile_Time_Error ((Character'Size /= Storage_Element'Size),
                              "Character and Storage_Element types are different sizes!");
   pragma Warnings (GNATprove, On, "Compile_Time_Error");

   for I in 1.. X'Length loop
      Byte := X(Storage_Offset(I) + X'First - 1);
      Output((I-1)*3 + 1 .. (I-1)*3 + 3) :=
        (Hex_Digits(Integer(Byte / 16) + 1),
         Hex_Digits(Integer(Byte and 15) + 1),
         ',');
   end loop;

   return Output(Output'First..Output'Last - 1);

end Storage_Array_To_Hex_String;
