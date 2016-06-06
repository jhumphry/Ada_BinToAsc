-- BinToAsc
-- Binary data to ASCII codecs

-- Copyright (c) 2015 - 2016, James Humphry - see LICENSE file for details

package body BinToAsc_Proof
with SPARK_Mode => On is

   use Ada.Characters.Handling;

   function To_String (Input : in Bin_Array) return String is
      C : Codec;
      Buffer_Length : constant Integer
        := (Input'Length / Input_Group_Size + 1)
          * Output_Group_Size + Output_Group_Size;
      -- Extra Output_Group_Size makes overflow checks easier to prove, and in
      -- practice will be a trivial amount of storage
      Buffer : String(1 .. Buffer_Length);
      Result_Length : Integer;
      Tail_Length : Integer;
   begin
      Reset(C);
      pragma Assert (C.State = Ready);
      pragma Assert (Empty(C));
      Process(C => C,
              Input => Input,
              Output => Buffer,
              Output_Length => Result_Length);
      if C.State = Ready then
         Complete(C => C,
                  Output => Buffer(Result_Length + 1 .. Buffer'Last),
                  Output_Length => Tail_Length);
         if C.State = Completed then
            return Buffer(1 .. Result_Length + Tail_Length);
         end if;
      end if;
      return Buffer(1..0);
   end To_String;

   function To_Bin (Input : in String) return Bin_Array is
      C : Codec;
      Buffer_Length : constant Bin_Array_Index
        := Bin_Array_Index((Input'Length / Input_Group_Size + 1)
                           * Output_Group_Size + Output_Group_Size);
      -- Extra Output_Group_Size makes overflow checks easier to prove, and in
      -- practice will be a trivial amount of storage
      Buffer : Bin_Array(1 .. Buffer_Length);
      Result_Length : Bin_Array_Index;
      Tail_Length : Bin_Array_Index;
   begin
      Reset(C);
      pragma Assert (C.State = Ready);
      pragma Assert (Empty(C));
      Process(C => C,
              Input => Input,
              Output => Buffer,
              Output_Length => Result_Length);
      if C.State = Ready then
         Complete(C => C,
                  Output => Buffer(Result_Length + 1 .. Buffer'Last),
                  Output_Length => Tail_Length);
         if C.State = Completed then
            return Buffer(1 .. Result_Length + Tail_Length);
         end if;
      end if;
      return Buffer(1..0);
   end To_Bin;

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

end BinToAsc_Proof;
