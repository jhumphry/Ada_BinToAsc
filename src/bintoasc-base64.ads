-- BinToAsc.Base64
-- Binary data to ASCII codecs - Base64 codec as in RFC4648

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

generic
   Alphabet : Alphabet_64 := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
   Padding : Character := '=';
package BinToAsc.Base64 is

   type Base64_To_String is new Codec_To_String with private;

   procedure Reset (C : out Base64_To_String);

   function Expansion_Numerator (C : in Base64_To_String)
                                 return Positive is (4);

   function Expansion_Denominator (C : in Base64_To_String)
                                   return Positive is (3);

   function Maximum_Padding (C : in Base64_To_String)
                             return Natural is (3);

   procedure Process (C : in out Base64_To_String;
                      Input : in Bin;
                      Output : out String;
                      Output_Length : out Natural)
     with Post'Class => (Output_Length = 0 or Output_Length = 4);

   procedure Process (C : in out Base64_To_String;
                      Input : in Bin_Array;
                      Output : out String;
                      Output_Length : out Natural)
     with Post'Class => (Output_Length / Expansion_Numerator(C) =
                             Input'Length / Expansion_Denominator(C) or
                        Output_Length / Expansion_Numerator(C) =
                             Input'Length / Expansion_Denominator(C) + 1);

   procedure Completed (C : in out Base64_To_String;
                        Output : out String;
                        Output_Length : out Natural)
     with Post'Class => (Output_Length = 0 or Output_Length = 4);

--     type Base64_To_Bin is new Codec_To_Bin with private;
--
--     procedure Reset (C : out Base64_To_Bin);
--
--     function Compression_Numerator (C : in Base64_To_Bin)
--                                   return Positive is (1);
--
--     function Compression_Denominator (C : in Base64_To_Bin)
--                                     return Positive is (2);
--
--     function Maximum_Padding (C : in Base64_To_Bin)
--                               return Natural is (1);
--
--     procedure Process (C : in out Base64_To_Bin;
--                        Input : in Character;
--                        Output : out Bin_Array;
--                        Output_Length : out Bin_Array_Index)
--       with Post'Class => Output_Length = 0 or Output_Length = 1;
--
--     procedure Process (C : in out Base64_To_Bin;
--                        Input : in String;
--                        Output : out Bin_Array;
--                        Output_Length : out Bin_Array_Index)
--       with Post'Class => Output_Length >= Input'Length / 2 and
--       Output_Length <= Input'Length / 2 + 1;
--
--     procedure Completed (C : in out Base64_To_Bin;
--                          Output : out Bin_Array;
--                          Output_Length : out Bin_Array_Index)
--       with Post'Class => Output_Length = 0;

private

   type Base64_Index is range 0..2;
   type Base64_Buffer is array (Base64_Index) of Bin;

   type Base64_To_String is new Codec_To_String with
      record
         Next_Index : Base64_Index := 0;
         Buffer : Base64_Buffer := (others => 0);
      end record;

end BinToAsc.Base64;
