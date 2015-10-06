-- BinToAsc.Base85
-- Various binary data to ASCII codecs known as Base85, ASCII85 etc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

generic
   Alphabet : Alphabet_85;
package BinToAsc.Base85 is

   type Base85_To_String is new Codec_To_String with private;

   procedure Reset (C : out Base85_To_String);

   function Input_Group_Size (C : in Base85_To_String) return Positive is (4);

   function Output_Group_Size (C : in Base85_To_String) return Positive is (5);

   procedure Process (C : in out Base85_To_String;
                      Input : in Bin;
                      Output : out String;
                      Output_Length : out Natural)
     with Post => (Output_Length = 0 or Output_Length = 5);

   procedure Process (C : in out Base85_To_String;
                      Input : in Bin_Array;
                      Output : out String;
                      Output_Length : out Natural)
     with Post => (Output_Length / 5 = Input'Length / 4 or
                     Output_Length / 5 = Input'Length / 4 + 1);

   procedure Complete (C : in out Base85_To_String;
                        Output : out String;
                        Output_Length : out Natural)
     with Post => (Output_Length <= 5);

   function To_String (Input : in Bin_Array) return String;

   type Base85_To_Bin is new Codec_To_Bin with private;

   procedure Reset (C : out Base85_To_Bin);

   function Input_Group_Size (C : in Base85_To_Bin) return Positive is (5);

   function Output_Group_Size (C : in Base85_To_Bin) return Positive is (4);

   procedure Process (C : in out Base85_To_Bin;
                      Input : in Character;
                      Output : out Bin_Array;
                      Output_Length : out Bin_Array_Index)
     with Post => (Output_Length = 0 or Output_Length = 4);

   procedure Process (C : in out Base85_To_Bin;
                      Input : in String;
                      Output : out Bin_Array;
                      Output_Length : out Bin_Array_Index)
     with Post => ((Output_Length / 4 >= Input'Length / 5 - 1 and
                       Output_Length / 4 <= Input'Length / 5 + 1) or
                         C.State = Failed);

   procedure Completed (C : in out Base85_To_Bin;
                        Output : out Bin_Array;
                        Output_Length : out Bin_Array_Index)
     with Post => (Output_Length < 4);

   function To_Bin (Input : in String) return Bin_Array;

private

   type Base85_Bin_Index is range 0..3;
   type Base85_Bin_Buffer is array (Base85_Bin_Index) of Bin;

   type Base85_To_String is new Codec_To_String with
      record
         Next_Index : Base85_Bin_Index := 0;
         Buffer : Base85_Bin_Buffer := (others => 0);
      end record;

   type Base85_Reverse_Index is range 0..4;
   type Base85_Reverse_Buffer is array (Base85_Reverse_Index) of Bin;

   type Base85_To_Bin is new Codec_To_Bin with
      record
         Next_Index : Base85_Reverse_Index := 0;
         Buffer : Base85_Reverse_Buffer := (others => 0);
      end record;

end BinToAsc.Base85;
