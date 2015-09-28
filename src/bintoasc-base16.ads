-- BinToAsc.Base16
-- Binary data to ASCII codecs - Base16 codec as in RFC4648

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

generic
   Alphabet : Alphabet_16 := "0123456789ABCDEF";
   Case_Sensitive : Boolean := True;
package BinToAsc.Base16 is

   type Base16_To_String is new Codec_To_String with null record;

   procedure Reset (C : out Base16_To_String);

   function Expansion_Numerator (C : in Base16_To_String)
                                 return Positive is (2);

   function Expansion_Denominator (C : in Base16_To_String)
                                   return Positive is (1);

   function Maximum_Padding (C : in Base16_To_String)
                             return Natural is (0);

   procedure Process (C : in out Base16_To_String;
                      Input : in Bin;
                      Output : out String;
                      Output_Length : out Natural)
     with Post'Class => Output_Length = 2;

   procedure Process (C : in out Base16_To_String;
                      Input : in Bin_Array;
                      Output : out String;
                      Output_Length : out Natural)
     with Post'Class => Output_Length = 2 * Input'Length;

   procedure Completed (C : in out Base16_To_String;
                        Output : out String;
                        Output_Length : out Natural)
     with Post'Class => Output_Length = 0;

   type Base16_To_Bin is new Codec_To_Bin with private;

   procedure Reset (C : out Base16_To_Bin);

   function Compression_Numerator (C : in Base16_To_Bin)
                                 return Positive is (1);

   function Compression_Denominator (C : in Base16_To_Bin)
                                   return Positive is (2);

   function Maximum_Padding (C : in Base16_To_Bin)
                             return Natural is (1);

   procedure Process (C : in out Base16_To_Bin;
                      Input : in Character;
                      Output : out Bin_Array;
                      Output_Length : out Bin_Array_Index)
     with Post'Class => Output_Length = 0 or Output_Length = 1;

   procedure Process (C : in out Base16_To_Bin;
                      Input : in String;
                      Output : out Bin_Array;
                      Output_Length : out Bin_Array_Index)
     with Post'Class => Output_Length >= Input'Length / 2 and
     Output_Length <= Input'Length / 2 + 1;

   procedure Completed (C : in out Base16_To_Bin;
                        Output : out Bin_Array;
                        Output_Length : out Bin_Array_Index)
     with Post'Class => Output_Length = 0;

private

   subtype Half_Bin is Bin range 0..15;

   type Base16_To_Bin is new Codec_To_Bin with
      record
         Loaded : Boolean := False;
         Load : Half_Bin := 0;
      end record;

end BinToAsc.Base16;
