-- BinToAsc_Suite.Misc_Tests
-- Unit tests for BinToAsc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with Ada.Characters.Handling;

with AUnit.Assertions;

package body BinToAsc.Testable is

   use Ada.Characters.Handling;
   use AUnit.Assertions;

   ---------------------------------
   -- Check_Make_Reverse_Alphabet --
   ---------------------------------

   procedure Check_Make_Reverse_Alphabet (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reversed : Reverse_Alphabet_Lookup;

      Valid : constant Alphabet_16 := "0123456789ABCDEF";
      Case_Wise : constant Alphabet_16 := "ABCDEFGHijklmnop";

   begin

      Reversed := Make_Reverse_Alphabet(Valid, True);

      Assert((for all I in Valid'Range =>
                Reversed(Valid(I)) = Bin(I)),
             "Make_Reverse_Alphabet does not create an appropriate reverse " &
               "lookup table for " & String(Valid));
      Assert((for all I in Reversed'Range =>
                (Reversed(I) = Invalid_Character_Input or else
                     Valid(Integer(Reversed(I))) = I)),
             "Make_Reverse_Alphabet does not create an appropriate reverse " &
               "lookup table for " & String(Valid));

      Reversed := Make_Reverse_Alphabet(Valid, False);

      Assert((for all I in Valid'Range =>
                Reversed(To_Upper(Valid(I))) = Bin(I) and
                Reversed(To_Lower(Valid(I))) = Bin(I)),
             "Make_Reverse_Alphabet does not create an appropriate reverse " &
               "lookup table for case-insensitive " & String(Valid));
      Assert((for all I in Reversed'Range =>
                (Reversed(I) = Invalid_Character_Input or else
                     To_Lower(Valid(Integer(Reversed(I)))) = To_Lower(I))),
             "Make_Reverse_Alphabet does not create an appropriate reverse " &
               "lookup table for case-insensitive " & String(Valid));

      Reversed := Make_Reverse_Alphabet(Case_Wise, True);

      Assert((for all I in Case_Wise'Range =>
                Reversed(Case_Wise(I)) = Bin(I)),
             "Make_Reverse_Alphabet does not create an appropriate reverse " &
               "lookup table for " & String(Case_Wise));
      Assert((for all I in Reversed'Range =>
                (Reversed(I) = Invalid_Character_Input or else
                     Case_Wise(Integer(Reversed(I))) = I)),
             "Make_Reverse_Alphabet does not create an appropriate reverse " &
               "lookup table for " & String(Case_Wise));

   end Check_Make_Reverse_Alphabet;

end BinToAsc.Testable;
