-- Storage_Array_To_String
-- This is a simple conversion routine to help implement test vectors

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with System.Storage_Elements;

function Storage_Array_To_String(X : System.Storage_Elements.Storage_Array)
                                 return String is
   use System.Storage_Elements;
begin

   -- This compile-time check is useful for GNAT, but in GNATprove it currently
   -- just generates a warning that it can not yet be proved correct.
   pragma Warnings (GNATprove, Off, "Compile_Time_Error");
   pragma Compile_Time_Error ((Character'Size /= Storage_Element'Size),
                              "Character and Storage_Element types are different sizes!");
   pragma Warnings (GNATprove, On, "Compile_Time_Error");

   return R : String(Integer(X'First) .. Integer(X'Last)) do
      for I in X'Range loop
         R(Integer(I)) := Character'Val(Storage_Element'Pos(X(I)));
      end loop;
   end return;

end Storage_Array_To_String;
