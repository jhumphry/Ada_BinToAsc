-- BinToAsc_Suite
-- Unit tests for BinToAsc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with Ada.Strings, Ada.Strings.Bounded;

with AUnit.Test_Suites;

package BinToAsc_Suite is

   package Test_Vector_Strings is
     new Ada.Strings.Bounded.Generic_Bounded_Length(Max => 12);
   use all type Test_Vector_Strings.Bounded_String;

   function TBS (Source : in String;
                 Drop : in Ada.Strings.Truncation := Ada.Strings.Error)
                 return Test_Vector_Strings.Bounded_String
                 renames Test_Vector_Strings.To_Bounded_String;

   subtype Test_Vector_String is Test_Vector_Strings.Bounded_String;

   type Test_Vector is
      record
         Unencoded : Test_Vector_String;
         Encoded : Test_Vector_String;
      end record;

   type Test_Vector_Array is array (Natural range <>) of Test_Vector;

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end BinToAsc_Suite;
