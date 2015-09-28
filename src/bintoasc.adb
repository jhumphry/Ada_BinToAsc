-- BinToAsc
-- Binary data to ASCII codecs

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with Ada.Characters.Handling;

package body BinToAsc is

   use Ada.Characters.Handling;

   function Make_Reverse_Alphabet (A : in Alphabet;
                              Case_Sensitive : Boolean)
                              return Reverse_Alphabet_Lookup is
   begin
      return R : Reverse_Alphabet_Lookup do
         R := (others => 255);
         for I in A'Range loop
            if Case_Sensitive then
               if R(A(I)) /= 255 then
                  raise Program_Error with "Duplicate letter '" & A(I) &
                    "' in alphabet.";
               end if;
               R(A(I)) := Bin(I);
            else
               if R(To_Lower(A(I))) /= 255 then
                  raise Program_Error with "Duplicate letter '" & To_Lower(A(I)) &
                    "' in alphabet (case insensitive).";
               end if;
               R(To_Lower(A(I))) := Bin(I);
            end if;
         end loop;
      end return;
   end Make_Reverse_Alphabet;

end BinToAsc;
