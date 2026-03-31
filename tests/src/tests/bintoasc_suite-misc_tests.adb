-- BinToAsc_Suite.Misc_Tests
-- Unit tests for BinToAsc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with AUnit.Assertions;

with RFC4648;
with BinToAsc.Testable;

package body BinToAsc_Suite.Misc_Tests is

   use AUnit.Assertions;

   use RFC4648;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T: in out Misc_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Check_Valid_Alphabet'Access,
                        "Check the validation of alphabets for use with codecs works");
      Register_Routine (T, Check_Make_Reverse_Alphabet'Access,
                        "Check building the reverse look-up table for alphabets");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   function Name (T : Misc_Test) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("Tests of misc supporting routines");
   end Name;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out Misc_Test) is
   begin
      null;
   end Set_Up;

   --------------------------
   -- Check_Valid_Alphabet --
   --------------------------

   procedure Check_Valid_Alphabet (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      use RFC4648.BToA;

      Valid : constant Alphabet_16 := "0123456789ABCDEF";
      Duplicate : constant Alphabet_16 := "0123456787ABCDEF";
      Case_Wise : constant Alphabet_16 := "ABCDEFGHabcdefgh";
   begin
      Assert(Valid_Alphabet(Valid, True),
             "Valid_Alphabet does not accept " & String(Valid) & " as a valid "&
               "case-sensitive alphabet");

      Assert(Valid_Alphabet(Valid, False),
             "Valid_Alphabet does not accept " & String(Valid) & " as a valid "&
               "case-insensitive alphabet");

      Assert(not Valid_Alphabet(Duplicate, True),
             "Valid_Alphabet accepts " & String(Duplicate) & " as a valid "&
               "despite duplicate value");

      Assert(Valid_Alphabet(Case_Wise, True),
             "Valid_Alphabet does not accept " & String(Case_Wise) & " as a " &
               "valid case-sensitive alphabet");

      Assert(not Valid_Alphabet(Case_Wise, False),
             "Valid_Alphabet accepts " & String(Case_Wise) & " as a valid "&
               "despite duplicate values when case is not considered");
   end Check_Valid_Alphabet;

    --------------------------------
   -- Check_Make_Reverse_Alphabet --
   ---------------------------------

   procedure Check_Make_Reverse_Alphabet (T : in out Test_Cases.Test_Case'Class) is

      package Testable is new BToA.Testable;

   begin

      Testable.Check_Make_Reverse_Alphabet(T);

   end Check_Make_Reverse_Alphabet;

end BinToAsc_Suite.Misc_Tests;
