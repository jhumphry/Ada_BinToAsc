-- BinToAsc_Suite.Misc_Tests
-- Unit tests for BinToAsc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package BinToAsc_Suite.Misc_Tests is

   type Misc_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out Misc_Test);

   function Name (T : Misc_Test) return Test_String;

   procedure Set_Up (T : in out Misc_Test);

   procedure Check_Valid_Alphabet (T : in out Test_Cases.Test_Case'Class);

   procedure Check_Make_Reverse_Alphabet (T : in out Test_Cases.Test_Case'Class);

end BinToAsc_Suite.Misc_Tests;
