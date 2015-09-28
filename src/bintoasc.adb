-- BinToAsc
-- Binary data to ASCII codecs

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with Ada.Characters.Handling;

package body BinToAsc is

   use Ada.Characters.Handling;

   function Valid_Alphabet (A : in Alphabet;
                            Case_Sensitive : in Boolean) return Boolean is
   begin
      for I in A'First + 1 ..A'Last loop
         for J in A'First .. I - 1 loop
            if A(I) = A(J) or
              (not Case_Sensitive and To_Lower(A(I)) = To_Lower(A(J)))
            then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Valid_Alphabet;

   function Make_Reverse_Alphabet (A : in Alphabet;
                              Case_Sensitive : Boolean)
                              return Reverse_Alphabet_Lookup is
   begin
      -- The precondition on Valid_Alphabet ensures that the input A does not
      -- contain any duplicate characters.
      return R : Reverse_Alphabet_Lookup do
         R := (others => 255);
         for I in A'Range loop
            if Case_Sensitive then
               R(A(I)) := Bin(I mod Bin'Modulus);
            else
               R(To_Lower(A(I))) := Bin(I mod Bin'Modulus);
            end if;
         end loop;
      end return;
   end Make_Reverse_Alphabet;

end BinToAsc;
