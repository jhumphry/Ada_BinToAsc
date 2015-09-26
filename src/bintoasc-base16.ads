-- BinToAsc.Base16
-- Binary data to ASCII codecs - Base16 codec as in RFC4648

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

generic
   Alphabet : Alphabet_16 := "0123456789ABCDEF";
   Case_Sensitive : Boolean := True;
package BinToAsc.Base16 is

   pragma Unreferenced(Case_Sensitive);

   type Base16_To_String is new Codec_To_String with null record;

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

end BinToAsc.Base16;
