-- String_To_Storage_Array
-- This is a simple conversion routine to help implement test vectors

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with System.Storage_Elements;

function String_To_Storage_Array(X : String)
                                 return System.Storage_Elements.Storage_Array is
   use System.Storage_Elements;
begin

   -- This compile-time check is useful for GNAT, but in GNATprove it currently
   -- just generates a warning that it can not yet be proved correct.
   pragma Warnings (GNATprove, Off, "Compile_Time_Error");
   pragma Compile_Time_Error ((Character'Size /= Storage_Element'Size),
                              "Character and Storage_Element types are different sizes!");
   pragma Warnings (GNATprove, On, "Compile_Time_Error");

   return R : Storage_Array(Storage_Offset(X'First) .. Storage_Offset(X'Last)) do
      for I in X'Range loop
         R(Storage_Offset(I)) := Character'Pos(X(I)) - Character'Pos(Character'First);
      end loop;
   end return;

end String_To_Storage_Array;
