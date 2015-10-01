-- BinToAsc.Base64
-- Binary data to ASCII codecs - Base64 codec as in RFC4648

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

generic
   Alphabet : Alphabet_64;
   Padding : Character;
package BinToAsc.Base64 is

   type Base64_To_String is new Codec_To_String with private;

   procedure Reset (C : out Base64_To_String);

   function Input_Group_Size (C : in Base64_To_String) return Positive is (3);

   function Output_Group_Size (C : in Base64_To_String) return Positive is (4);

   procedure Process (C : in out Base64_To_String;
                      Input : in Bin;
                      Output : out String;
                      Output_Length : out Natural)
     with Post => (Output_Length = 0 or Output_Length = 4);

   procedure Process (C : in out Base64_To_String;
                      Input : in Bin_Array;
                      Output : out String;
                      Output_Length : out Natural)
     with Post => (Output_Length / 4 = Input'Length / 3 or
                     Output_Length / 4 = Input'Length / 3 + 1);

   procedure Complete (C : in out Base64_To_String;
                        Output : out String;
                        Output_Length : out Natural)
     with Post => (Output_Length = 0 or Output_Length = 4);

   function To_String (Input : in Bin_Array) return String;

   type Base64_To_Bin is new Codec_To_Bin with private;

   procedure Reset (C : out Base64_To_Bin);

   function Input_Group_Size (C : in Base64_To_Bin) return Positive is (4);

   function Output_Group_Size (C : in Base64_To_Bin) return Positive is (3);

   procedure Process (C : in out Base64_To_Bin;
                      Input : in Character;
                      Output : out Bin_Array;
                      Output_Length : out Bin_Array_Index)
     with Post => (Output_Length >= 0 and Output_Length <= 3);

   procedure Process (C : in out Base64_To_Bin;
                      Input : in String;
                      Output : out Bin_Array;
                      Output_Length : out Bin_Array_Index)
     with Post => ((Output_Length / 3 >= Input'Length / 4 - 1 and
                       Output_Length / 3 <= Input'Length / 4 + 1) or
                         C.State = Failed);

   -- Re: the postcondition. If the input is a given number four character
   -- groups but with the last containing padding, the output may be less than
   -- that number of three character groups. As usual, if the codec contained
   -- some partially decoded data, the number of output groups can be one more
   -- than otherwise expected.

   procedure Completed (C : in out Base64_To_Bin;
                        Output : out Bin_Array;
                        Output_Length : out Bin_Array_Index)
     with Post => (Output_Length = 0);

   function To_Bin (Input : in String) return Bin_Array;

private

   type Base64_Bin_Index is range 0..2;
   type Base64_Bin_Buffer is array (Base64_Bin_Index) of Bin;

   type Base64_To_String is new Codec_To_String with
      record
         Next_Index : Base64_Bin_Index := 0;
         Buffer : Base64_Bin_Buffer := (others => 0);
      end record;

   type Base64_Reverse_Index is range 0..3;
   type Base64_Reverse_Buffer is array (Base64_Reverse_Index) of Bin;

   type Base64_To_Bin is new Codec_To_Bin with
      record
         Next_Index : Base64_Reverse_Index := 0;
         Buffer : Base64_Reverse_Buffer := (others => 0);
         Padding_Length : Bin_Array_Index := 0;
      end record;

end BinToAsc.Base64;
