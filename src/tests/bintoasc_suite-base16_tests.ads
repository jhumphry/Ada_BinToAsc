-- BinToAsc_Suite.Base16_Tests
-- Unit tests for BinToAsc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package BinToAsc_Suite.Base16_Tests is

   Base16_Test_Vectors : Test_Vector_Array := ((TBS(""), TBS("")),
                                               (TBS("f"), TBS("66")),
                                               (TBS("fo"), TBS("666F")),
                                               (TBS("foo"), TBS("666F6F")),
                                               (TBS("foob"), TBS("666F6F62")),
                                               (TBS("fooba"), TBS("666F6F6261")),
                                               (TBS("foobar"), TBS("666F6F626172")));

   type Base16_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out Base16_Test);

   function Name (T : Base16_Test) return Test_String;

   procedure Set_Up (T : in out Base16_Test);

   procedure Check_Test_Vectors (T : in out Test_Cases.Test_Case'Class);
   procedure Check_Test_Vectors_Incremental (T : in out Test_Cases.Test_Case'Class);
   procedure Check_Test_Vectors_By_Char (T : in out Test_Cases.Test_Case'Class);

end BinToAsc_Suite.Base16_Tests;
