-- BinToAsc
-- Binary data to ASCII codecs

-- Copyright (c) 2015 - 2016, James Humphry - see LICENSE file for details

with Ada.Characters.Handling;

package body BinToAsc
with SPARK_Mode => On is

   use Ada.Characters.Handling;

   function To_String (Input : in Bin_Array) return String is
      C : Codec;
      Buffer_Length : constant Integer
        := (Input'Length / Input_Group_Size(C) + 1)
          * Output_Group_Size(C);
      Buffer : String(1 .. Buffer_Length);
      Result_Length : Integer;
      Tail_Length : Integer;
   begin
      Reset(C);
      Process(C => C,
              Input => Input,
              Output => Buffer,
              Output_Length => Result_Length);
      if C.State /= Ready then
         raise Program_Error with "Could not convert data";
      end if;
      Complete(C => C,
                Output => Buffer(Result_Length + 1 .. Buffer'Last),
                Output_Length => Tail_Length);
      if C.State /= Completed then
         raise Program_Error with "Could not convert data";
      end if;
      return Buffer(1 .. Result_Length + Tail_Length);
   end To_String;

   function To_Bin (Input : in String) return Bin_Array is
      C : Codec;
      Buffer_Length : constant Bin_Array_Index
        := Bin_Array_Index((Input'Length / Input_Group_Size(C) + 1)
                           * Output_Group_Size(C));
      Buffer : Bin_Array(1 .. Buffer_Length);
      Result_Length : Bin_Array_Index;
      Tail_Length : Bin_Array_Index;
   begin
      Reset(C);
      Process(C => C,
              Input => Input,
              Output => Buffer,
              Output_Length => Result_Length);
      if C.State /= Ready then
         raise Invalid_Data_Encoding;
      end if;
      Complete(C => C,
                Output => Buffer(Result_Length + 1 .. Buffer'Last),
                Output_Length => Tail_Length);
      if C.State /= Completed then
         raise Invalid_Data_Encoding;
      end if;
      return Buffer(1 .. Result_Length + Tail_Length);
   end To_Bin;

   function Valid_Alphabet (A : in Alphabet;
                            Case_Sensitive : in Boolean) return Boolean is
   begin
      for I in A'First + 1 ..A'Last loop
         pragma Loop_Invariant (I > A'First and I <= A'Last);
         for J in A'First .. I - 1 loop
            pragma Loop_Invariant (J >= A'First);
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
         R := (others => Invalid_Character_Input);
         for I in A'Range loop
            if Case_Sensitive then
               R(A(I)) := I;
            else
               R(To_Lower(A(I))) := I;
               R(To_Upper(A(I))) := I;
            end if;
         end loop;
      end return;
   end Make_Reverse_Alphabet;

end BinToAsc;
